/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <modules/iswa/rendering/kameleonplane.h>

#include <modules/iswa/rendering/iswabasegroup.h>
#include <modules/iswa/rendering/iswakameleongroup.h>
#include <modules/iswa/util/dataprocessorkameleon.h>
#include <modules/iswa/util/iswamanager.h>
#include <openspace/json.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <fstream>

namespace {
    using json = nlohmann::json;
    constexpr std::string_view _loggerCat = "KameleonPlane";

    constexpr openspace::properties::Property::PropertyInfo FieldLineSeedsInfo = {
        "FieldlineSeedsIndexFile",
        "Fieldline Seedpoints",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo ResolutionInfo = {
        "Resolution",
        "Resolution%",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SliceInfo = {
        "Slice",
        "Slice",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::User
    };
} // namespace

namespace openspace {

KameleonPlane::KameleonPlane(const ghoul::Dictionary& dictionary)
    : DataCygnet(dictionary)
    , _fieldlines(FieldLineSeedsInfo)
    , _resolution(ResolutionInfo, 100.f, 10.f, 200.f)
    , _slice(SliceInfo, 0.f, 0.f, 1.f)
{
    addProperty(_resolution);
    addProperty(_slice);
    addProperty(_fieldlines);

    if (dictionary.hasValue<std::string>("kwPath")) {
        _kwPath = dictionary.value<std::string>("kwPath");
    }

    if (dictionary.hasValue<std::string>("fieldlineSeedsIndexFile")) {
        _fieldlineIndexFile = dictionary.value<std::string>("fieldlineSeedsIndexFile");
    }

    std::string axis;
    if (dictionary.hasValue<std::string>("axisCut")) {
        axis = dictionary.value<std::string>("axisCut");
    }

    if (axis == "x") {
        _cut = Cut::X;
    }
    else if (axis == "y") {
        _cut = Cut::Y;
    }
    else {
        _cut = Cut::Z;
    }

    _origOffset = _data.offset;

    _scale = _data.scale[_cut];
    _data.scale[_cut] = 0;
    _data.offset[_cut] = 0;

    _slice = (_data.offset[_cut] -_data.gridMin[_cut]) / _scale;

    setDimensions();
}

KameleonPlane::~KameleonPlane() {}

void KameleonPlane::deinitializeGL() {
    IswaCygnet::deinitialize();
    _fieldlines = std::set<std::string>();
}

void KameleonPlane::initializeGL() {
    if (!_shader) {
        _shader = global::renderEngine->buildRenderProgram(
            "DataPlaneProgram",
            absPath("${MODULE_ISWA}/shaders/dataplane_vs.glsl"),
            absPath("${MODULE_ISWA}/shaders/dataplane_fs.glsl")
        );
    }

    if (!_data.groupName.empty()) {
        initializeGroup();
    }

    initializeTime();
    createGeometry();

    readFieldlinePaths(absPath(_fieldlineIndexFile).string());

    if (_group) {
        _dataProcessor = _group->dataProcessor();
        subscribeToGroup();
    }
    else {
        _dataProcessor = std::make_shared<DataProcessorKameleon>();

        //If autofiler is on, background values property should be hidden
        _autoFilter.onChange([this]() {
            // If autofiler is selected, use _dataProcessor to set backgroundValues
            // and unregister backgroundvalues property.
            if (_autoFilter) {
                _backgroundValues = _dataProcessor->filterValues();
                _backgroundValues.setVisibility(properties::Property::Visibility::Hidden);
                //_backgroundValues.setVisible(false);
            // else if autofilter is turned off, register backgroundValues
            }
            else {
                _backgroundValues.setVisibility(properties::Property::Visibility::Always);
                //_backgroundValues.setVisible(true);
            }
        });
    }

    fillOptions(_kwPath);

    readTransferFunctions(_transferFunctionsFile);

    // Set Property Callbacks of DataCygnet (must be called after fillOptions)
    setPropertyCallbacks();

    // Set Property callback specific to KameleonPlane
    _resolution.onChange([this]() {
        for (size_t i = 0; i < _textures.size(); i++) {
            _textures[i] = nullptr;
        }

        updateTextureResource();
        setDimensions();

    });

    _slice.onChange([this]() { updateTextureResource(); });

    _fieldlines.onChange([this]() { updateFieldlineSeeds(); });

    std::dynamic_pointer_cast<DataProcessorKameleon>(_dataProcessor)->setDimensions(
        _dimensions
    );
    _dataProcessor->addDataValues(_kwPath, _dataOptions);
    // if this datacygnet has added new values then reload texture
    // for the whole group, including this datacygnet, and return after.
    if (_group) {
        _group->updateGroup();
    }
    updateTextureResource();
}

bool KameleonPlane::createGeometry() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer

    // ============================
    //         GEOMETRY (quad)
    // ============================
    // GLfloat x,y, z;
    float s = _data.spatialScale.x;
    const GLfloat x = s * _data.scale.x / 2.f;
    const GLfloat y = s * _data.scale.y / 2.f;
    const GLfloat z = s * _data.scale.z / 2.f;
    const GLfloat w = _data.spatialScale.w;

    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //      x      y     z     w     s     t
        -x, -y,             -z,  w, 0, 1,
         x,  y,              z,  w, 1, 0,
        -x,  ((x>0)?y:-y),   z,  w, 0, 0,
        -x, -y,             -z,  w, 0, 1,
         x,  ((x>0)?-y:y),  -z,  w, 1, 1,
         x,  y,              z,  w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, nullptr);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );

    return true;
}

bool KameleonPlane::destroyGeometry() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    return true;
}

void KameleonPlane::renderGeometry() const {
    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
}

std::vector<float*> KameleonPlane::textureData() {
    DataProcessorKameleon* p = dynamic_cast<DataProcessorKameleon*>(_dataProcessor.get());
    p->setSlice(_slice);
    return p->processData(_kwPath, _dataOptions, _dimensions);
}

bool KameleonPlane::updateTextureResource() {
    _data.offset[_cut] = _slice * _scale + _data.gridMin[_cut];
    // _textureDirty = true;
    updateTexture();
    return true;
}

void KameleonPlane::setUniforms() {
    setTextureUniforms();
    _shader->setUniform("backgroundValues", _backgroundValues.value());
    _shader->setUniform("transparency", _alpha.value());
}

void KameleonPlane::updateFieldlineSeeds() {
    std::set<std::string> selectedOptions = _fieldlines;
    std::vector<std::string> opts = _fieldlines.options();

    // seedPath == map<int selectionValue, tuple<string name, string path, bool active>>
    using K = int;
    using V = std::tuple<std::string, std::string, bool>;
    for (std::pair<const K, V>& seedPath : _fieldlineState) {
        // if this option was turned off
        std::string o = opts[seedPath.first];
        const auto it = std::find(selectedOptions.begin(), selectedOptions.end(), o);
        if (it == selectedOptions.end() && std::get<2>(seedPath.second)) {
            SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(
                std::get<0>(seedPath.second)
            );
            if (!n) {
                return;
            }

            LDEBUG("Removed fieldlines: " + std::get<0>(seedPath.second));
            global::scriptEngine->queueScript(
                "openspace.removeSceneGraphNode('" + std::get<0>(seedPath.second) + "')",
                scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                scripting::ScriptEngine::ShouldSendToRemote::Yes
            );
            std::get<2>(seedPath.second) = false;
        // if this option was turned on
        }
        else if (it != selectedOptions.end() && !std::get<2>(seedPath.second)) {
            SceneGraphNode* n = global::renderEngine->scene()->sceneGraphNode(
                std::get<0>(seedPath.second)
            );
            if (n) {
                return;
            }

            LDEBUG("Created fieldlines: " + std::get<0>(seedPath.second));
            IswaManager::ref().createFieldline(
                std::get<0>(seedPath.second),
                _kwPath,
                std::get<1>(seedPath.second)
            );
            std::get<2>(seedPath.second) = true;
        }
    }
}

void KameleonPlane::readFieldlinePaths(const std::string& indexFile) {
    LINFO(std::format("Reading seed points paths from file '{}'", indexFile));
    if (_group) {
        dynamic_cast<IswaKameleonGroup*>(_group)->setFieldlineInfo(
            indexFile,
            _kwPath
        );
        return;
    }

    // Read the index file from disk
    std::ifstream seedFile(indexFile);
    if (!seedFile.good()) {
        LERROR(std::format("Could not open seed points file '{}'", indexFile));
    }
    else {
        try {
            //Parse and add each fieldline as an selection
            json fieldlines = json::parse(seedFile);
            int i = 0;
            const std::string& fullName = identifier();
            std::string partName = fullName.substr(0,fullName.find_last_of("-"));
            for (json::iterator it = fieldlines.begin(); it != fieldlines.end(); it++) {
                _fieldlines.addOption(it.key());
                _fieldlineState[i] = std::make_tuple<std::string, std::string, bool>(
                    partName + "/" + it.key(),
                    it.value(),
                    false
                );
                i++;
            }
        } catch (const std::exception& e) {
            LERROR(
                "Error when reading json file with paths to seedpoints: " +
                std::string(e.what())
            );
        }
   }
}

void KameleonPlane::subscribeToGroup() {
    // Subscribe to DataCygnet events
    DataCygnet::subscribeToGroup();

    //Add additional Events specific to KameleonPlane
    ghoul::Event<ghoul::Dictionary>& groupEvent = _group->groupEvent();
    groupEvent.subscribe(
        identifier(),
        "resolutionChanged",
        [this](ghoul::Dictionary dict) {
            LDEBUG(identifier() + " Event resolutionChanged");
            if (dict.hasKey("resolution") && dict.hasValue<double>("resolution")) {
                _resolution = static_cast<float>(dict.value<double>("resolution"));
            }
        }
    );

    groupEvent.subscribe(
        identifier(),
        "cdfChanged",
        [this](ghoul::Dictionary dict) {
            LDEBUG(identifier() + " Event cdfChanged");
            if (dict.hasKey("path") && dict.hasValue<std::string>("path")) {
                const std::string& path = dict.value<std::string>("path");
                changeKwPath(path);
            }
            updateTexture();
        }
    );
}

void KameleonPlane::setDimensions() {
    // the cdf files has an offset of 0.5 in normali resolution.
    // with lower resolution the offset increases.
    _data.offset = _origOffset - 0.5f * (100.f / _resolution);
    _dimensions = glm::size3_t(_data.scale * (_resolution / 100.f));
    _dimensions[_cut] = 1;

    if (_cut == Cut::X) {
        _textureDimensions = glm::size3_t(_dimensions.y, _dimensions.z, 1);
    }
    else if (_cut == Cut::Y) {
        _textureDimensions = glm::size3_t(_dimensions.x, _dimensions.z, 1);
    }
    else {
        _textureDimensions = glm::size3_t(_dimensions.x, _dimensions.y, 1);
    }
}

void KameleonPlane::changeKwPath(std::string kwPath) {
    _kwPath = std::move(kwPath);
}

}// namespace openspace
