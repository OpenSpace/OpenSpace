/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/base/rendering/renderablemodel.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/rendering/renderengine.h>
#include <modules/base/rendering/modelgeometry.h>
#include <openspace/engine/configurationmanager.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/scene/scenegraphnode.h>

#include <openspace/util/time.h>
#include <ghoul/misc/invariants.h>

#include <openspace/engine/openspaceengine.h>

#define _USE_MATH_DEFINES
#include <math.h>

namespace { 
    const std::string _loggerCat     = "RenderableModel";

    const char* KeyGeometry = "Geometry";
    const char* KeyTexture = "Textures.Color";


    const char* keyBody        = "Body";
    const char* keyStart       = "StartTime";
    const char* keyEnd         = "EndTime";
    const char* keyFading      = "Shading.Fadeable";
    
    const char* keyModelTransform = "Rotation.ModelTransform";
} // namespace

namespace openspace {

documentation::Documentation RenderableModel::Documentation() {
    using namespace documentation;
    return {
        "RenderableModel",
        "base_renderable_model",
        {
            {
                KeyGeometry,
                new ReferencingVerifier("base_geometry_model"),
                "This specifies the model that is rendered by the Renderable.",
                Optional::No
            },
            {
                KeyTexture,
                new StringVerifier,
                "A color texture that can be applied to the model specified bny the "
                "Geometry.",
                Optional::Yes
            }
        }
    };
}

RenderableModel::RenderableModel(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorTexturePath("colorTexture", "Color Texture")
    , _performFade("performFading", "Perform Fading", false)
    , _fading("fading", "Fade", 0)
    , _debugModelRotation("modelrotation", "Model Rotation", glm::vec3(0.f), glm::vec3(0.f), glm::vec3(360.f))
    , _programObject(nullptr)
    , _texture(nullptr)
    , _geometry(nullptr)
    , _alpha(1.f)
    , _performShading("performShading", "Perform Shading", true)
    , _frameCount(0)
{
    ghoul_precondition(
        dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyName),
        "Name was not passed to RenderableModel"
    );

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableModel"
    );


    if (dictionary.hasKey(KeyGeometry)) {
        std::string name = dictionary.value<std::string>(SceneGraphNode::KeyName);
        ghoul::Dictionary dict = dictionary.value<ghoul::Dictionary>(KeyGeometry);
        dict.setValue(SceneGraphNode::KeyName, name);
        _geometry = modelgeometry::ModelGeometry::createFromDictionary(dict);
    }

    if (dictionary.hasKey(KeyTexture)) {
        _colorTexturePath = absPath(dictionary.value<std::string>(KeyTexture));
    }

    addPropertySubOwner(_geometry.get());

    addProperty(_colorTexturePath);
    _colorTexturePath.onChange(std::bind(&RenderableModel::loadTexture, this));

    addProperty(_debugModelRotation);

    if (dictionary.hasKeyAndValue<glm::dmat3>(keyModelTransform)) {
        dictionary.getValue(keyModelTransform, _modelTransform);
    }
    else {
        _modelTransform = glm::dmat3(1.f);
    }
    addProperty(_performShading);

    if (dictionary.hasKeyAndValue<bool>(keyFading)) {
        bool fading;
        dictionary.getValue(keyFading, fading);
        _performFade = fading;
    }
    addProperty(_performFade);
}

bool RenderableModel::isReady() const {
    bool ready = true;
    ready &= (_programObject != nullptr);
    ready &= (_texture != nullptr);
    return ready;
}

bool RenderableModel::initialize() {
    bool completeSuccess = true;
    if (_programObject == nullptr) {
        // NH shader

        RenderEngine& renderEngine = OsEng.renderEngine();
        _programObject = renderEngine.buildRenderProgram("ModelProgram",
            "${MODULE_BASE}/shaders/model_vs.glsl",
            "${MODULE_BASE}/shaders/model_fs.glsl");

        if (!_programObject)
            return false;
    }

    loadTexture();

    completeSuccess &= (_texture != nullptr);
    completeSuccess &= _geometry->initialize(this); 
    //completeSuccess &= !_source.empty();
    //completeSuccess &= !_destination.empty();

    return completeSuccess;
}

bool RenderableModel::deinitialize() {
    if (_geometry) {
        _geometry->deinitialize();
        _geometry = nullptr;
    }
    _texture = nullptr;


    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    return true;
}

void RenderableModel::render(const RenderData& data) {
    _programObject->activate();
    
    // Fading
    if (_performFade && _fading > 0.f) {
        _fading = _fading - 0.01f;
    }
    else if (!_performFade && _fading < 1.f) {
        _fading = _fading + 0.01f;

    }


    // debug rotation controlled from GUI
    glm::mat4 unitMat4(1);
    glm::vec3 debugEulerRot = glm::radians(_debugModelRotation.value());
    glm::mat4 rotX = glm::rotate(unitMat4, debugEulerRot.x, glm::vec3(1, 0, 0));
    glm::mat4 rotY = glm::rotate(unitMat4, debugEulerRot.y, glm::vec3(0, 1, 0));
    glm::mat4 rotZ = glm::rotate(unitMat4, debugEulerRot.z, glm::vec3(0, 0, 1));
    glm::dmat4 debugModelRotation = rotX * rotY * rotZ;

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::dmat4(glm::scale(glm::dmat4(_modelTransform), glm::dvec3(data.modelTransform.scale)));
        debugModelRotation; // debug model rotation controlled from GUI
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

    glm::vec3 directionToSun = glm::normalize(_sunPos - data.modelTransform.translation);
    glm::vec3 directionToSunViewSpace = glm::mat3(data.camera.combinedViewMatrix()) * directionToSun;

    _programObject->setUniform("transparency", _alpha);
    _programObject->setUniform("directionToSunViewSpace", directionToSunViewSpace);
    _programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _programObject->setUniform("projectionTransform", data.camera.projectionMatrix());
    _programObject->setUniform("performShading", _performShading);
    _programObject->setUniform("fading", _fading);

    _geometry->setUniforms(*_programObject);

    // Bind texture
    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _texture->bind();
    _programObject->setUniform("texture1", unit);

    _geometry->render();

    // disable shader
    _programObject->deactivate();
}

void RenderableModel::update(const UpdateData& data) {
    if (_programObject->isDirty()) {
        _programObject->rebuildFromFile();
    }
//    double _time = data.time;

    //if (_isGhost){
    //    futureTime = openspace::ImageSequencer::ref().getNextCaptureTime();
    //    double remaining = openspace::ImageSequencer::ref().getNextCaptureTime() - data.time;
    //    double interval = openspace::ImageSequencer::ref().getIntervalLength();
    //    double t = 1.f - remaining / openspace::ImageSequencer::ref().getIntervalLength();
    //    if (interval > 60) {
    //        if (t < 0.8)
    //            _fading = static_cast<float>(t);
    //        else if (t >= 0.95)
    //            _fading = _fading - 0.5f;
    //    }
    //    else
    //        _fading = 0.f;
    //    _time = futureTime;
    //}

    _sunPos = OsEng.renderEngine().scene()->sceneGraphNode("Sun")->worldPosition();
}

void RenderableModel::loadTexture() {
    _texture = nullptr;
    if (_colorTexturePath.value() != "") {
        _texture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorTexturePath));
        if (_texture) {
            LDEBUG("Loaded texture from '" << absPath(_colorTexturePath) << "'");
            _texture->uploadTexture();
            _texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        }
    }
}

}  // namespace openspace
