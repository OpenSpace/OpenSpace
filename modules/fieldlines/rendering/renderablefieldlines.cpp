/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/fieldlines/rendering/renderablefieldlines.h>

#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/programobject.h>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "RenderableFieldlines";

    constexpr const float DefaultFieldlineStepSize = 0.5f;
    const glm::vec4 DefaultFieldlineColor = glm::vec4(1.f, 0.f, 0.f, 1.f);

    constexpr const char* KeyVectorField = "VectorField";
    constexpr const char* KeyVectorFieldType = "Type";
    constexpr const char* KeyVectorFieldFile = "File";
    constexpr const char* KeyVectorFieldVolumeModel = "Model";
    constexpr const char* KeyVectorFieldVolumeVariable = "Variables";

    constexpr const char* KeyFieldlines = "Fieldlines";
    constexpr const char* KeyFieldlinesStepSize = "Stepsize";
    constexpr const char* KeyFieldlinesClassification = "Classification";
    constexpr const char* KeyFieldlinesColor = "Color";

    constexpr const char* KeySeedPoints = "SeedPoints";
    constexpr const char* KeySeedPointsType = "Type";
    constexpr const char* KeySeedPointsFile = "File";
    constexpr const char* KeySeedPointsTable = "SeedPoints";

    constexpr const char* SeedPointsSourceFile = "File";
    constexpr const char* SeedPointsSourceTable = "Table";

    constexpr const char* VectorFieldTypeVolumeKameleon = "VolumeKameleon";

    constexpr const char* VectorFieldKameleonModelBATSRUS = "BATSRUS";

    constexpr const char* VectorFieldKameleonVariableLorentz = "Lorentz";

    constexpr const int SeedPointSourceFile = 0;
    constexpr const int SeedPointSourceTable = 1;

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
        KeyFieldlinesStepSize,
        //"StepSize",
        "Fieldline Step Size",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo Classification = {
        KeyFieldlinesClassification,
        "Fieldline Classification",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo FieldlineColorInfo = {
        "FieldlineColor",
        "Fieldline Color",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo SeedPointSourceInfo = {
        "Source",
        "SeedPoint Source",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo SeedPointFileInfo = {
        "SourceFile",
        "SeedPoint File",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace {

RenderableFieldlines::RenderableFieldlines(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _stepSize(StepSizeInfo, DefaultFieldlineStepSize, 0.f, 10.f)
    , _classification(Classification, true)
    , _fieldlineColor(
        FieldlineColorInfo,
        DefaultFieldlineColor,
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _seedPointSource(SeedPointSourceInfo)
    , _seedPointSourceFile(SeedPointFileInfo)
{
    std::string identifier = dictionary.value<std::string>(SceneGraphNode::KeyIdentifier);
    setIdentifier(identifier);

    bool success = dictionary.getValue(KeyVectorField, _vectorFieldInfo);
    if (!success) {
        LERROR(fmt::format("Renderable does not contain a key for '{}'", KeyVectorField));
    }

    success = dictionary.getValue(KeyFieldlines, _fieldlineInfo);
    if (!success) {
        LERROR(fmt::format("Renderable does not contain a key for '{}'", KeyFieldlines));
    }

    success = dictionary.getValue(KeySeedPoints, _seedPointsInfo);
    if (!success) {
        LERROR(fmt::format("Renderable does not contain a key for '{}", KeySeedPoints));
    }

    // @TODO a non-magic number perhaps ---abock
    setBoundingSphere(250.f*6371000.f);

    _seedPointSource.addOption(SeedPointSourceFile, "File");
    _seedPointSource.addOption(SeedPointSourceTable, "Lua Table");

    initializeDefaultPropertyValues();

    // @TODO hook up visibility changes ---abock

    auto dirtyFieldlines = [this]() { this->_fieldLinesAreDirty = true; };
    auto dirtySeedpoints = [this]() { this->_seedPointsAreDirty = true; };

    _stepSize.onChange(dirtyFieldlines);
    addProperty(_stepSize);

    addProperty(_classification);

    _fieldlineColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_fieldlineColor);

    _seedPointSource.onChange(dirtySeedpoints);
    addProperty(_seedPointSource);

    _seedPointSourceFile.onChange(dirtySeedpoints);
    addProperty(_seedPointSourceFile);
}

void RenderableFieldlines::initializeDefaultPropertyValues() {
    _fieldlineInfo.getValue(KeyFieldlinesStepSize, _stepSize);
    _fieldlineInfo.getValue(KeyFieldlinesClassification, _classification);
    _fieldlineInfo.getValue(KeyFieldlinesColor, _fieldlineColor);

    if (_seedPointsInfo.hasKeyAndValue<std::string>(KeySeedPointsType)) {
        std::string sourceType = _seedPointsInfo.value<std::string>(KeySeedPointsType);

        if (sourceType == SeedPointsSourceFile) {
            _seedPointSource = SeedPointSourceFile;

            if (_seedPointsInfo.hasKeyAndValue<std::string>(KeySeedPointsFile)) {
                std::string seedPointSourceFile = _seedPointsInfo.value<std::string>(
                    KeySeedPointsFile
                );
                _seedPointSourceFile = absPath(seedPointSourceFile);
            }
        }
        else if (sourceType == SeedPointsSourceTable) {
            _seedPointSource = SeedPointSourceTable;
        }
    }
}

bool RenderableFieldlines::isReady() const {
    const bool programReady = _program != nullptr;
    const bool vectorFieldReady = !_vectorFieldInfo.empty();
    const bool fieldlineReady = !_fieldlineInfo.empty();
    const bool seedPointsReady = !_seedPointsInfo.empty();
    return programReady && vectorFieldReady && fieldlineReady && seedPointsReady;
}

void RenderableFieldlines::initializeGL() {
    if (_vectorFieldInfo.empty() || _fieldlineInfo.empty() || _seedPointsInfo.empty()) {
        throw ghoul::RuntimeError("Error initializing");
    }

    _program = global::renderEngine.buildRenderProgram(
        "Fieldline",
        absPath("${MODULE_FIELDLINES}/shaders/fieldline_vs.glsl"),
        absPath("${MODULE_FIELDLINES}/shaders/fieldline_fs.glsl"),
        absPath("${MODULE_FIELDLINES}/shaders/fieldline_gs.glsl")
    );
}

void RenderableFieldlines::deinitializeGL() {
    glDeleteVertexArrays(1, &_fieldlineVAO);
    _fieldlineVAO = 0;
    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    if (_program) {
        global::renderEngine.removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableFieldlines::render(const RenderData& data, RendererTasks&) {
    _program->activate();
    _program->setUniform("modelViewProjection", data.camera.viewProjectionMatrix());
    _program->setUniform("modelTransform", glm::mat4(1.0));
    _program->setUniform(
        "cameraViewDir",
        glm::vec3(data.camera.viewDirectionWorldSpace())
    );
    glDisable(GL_CULL_FACE);
    setPscUniforms(*_program, data.camera, data.position);

    _program->setUniform("classification", _classification);
    if (!_classification) {
        _program->setUniform("fieldLineColor", _fieldlineColor);
    }

    glBindVertexArray(_fieldlineVAO);
    glMultiDrawArrays(
        GL_LINE_STRIP_ADJACENCY,
        &_lineStart[0],
        &_lineCount[0],
        static_cast<GLsizei>(_lineStart.size())
    );
    glBindVertexArray(0);
    glEnable(GL_CULL_FACE);
    _program->deactivate();
}

void RenderableFieldlines::update(const UpdateData&) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
    }

    if (_seedPointsAreDirty) {
        loadSeedPoints();
        _seedPointsAreDirty = false;
        _fieldLinesAreDirty = true;
    }

    if (_fieldLinesAreDirty) {
        const std::vector<Line>& fieldlines = generateFieldlines();

        if (fieldlines.empty()) {
            return;
        }

        int prevEnd = 0;
        std::vector<LinePoint> vertexData;
        // Arrange data for glMultiDrawArrays
        for (size_t j = 0; j < fieldlines.size(); ++j) {
            _lineStart.push_back(prevEnd);
            _lineCount.push_back(static_cast<int>(fieldlines[j].size()));
            prevEnd = prevEnd + static_cast<int>(fieldlines[j].size());
            vertexData.insert(
                vertexData.end(),
                fieldlines[j].begin(),
                fieldlines[j].end()
            );
        }
        LDEBUG(fmt::format("Number of vertices: {}", vertexData.size()));

        if (_fieldlineVAO == 0) {
            glGenVertexArrays(1, &_fieldlineVAO);
        }
        glBindVertexArray(_fieldlineVAO);

        if (_vertexPositionBuffer == 0) {
            glGenBuffers(1, &_vertexPositionBuffer);
        }
        glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

        glBufferData(
            GL_ARRAY_BUFFER,
            vertexData.size() * sizeof(LinePoint),
            &vertexData.front(),
            GL_STATIC_DRAW
        );

        GLuint vertexLocation = 0;
        glEnableVertexAttribArray(vertexLocation);
        glVertexAttribPointer(
            vertexLocation,
            3,
            GL_FLOAT,
            GL_FALSE,
            sizeof(LinePoint),
            nullptr
        );

        GLuint colorLocation = 1;
        glEnableVertexAttribArray(colorLocation);
        glVertexAttribPointer(
            colorLocation,
            4,
            GL_FLOAT,
            GL_FALSE,
            sizeof(LinePoint),
            reinterpret_cast<void*>(sizeof(glm::vec3))
        );

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);

        _fieldLinesAreDirty = false;
    }
}

void RenderableFieldlines::loadSeedPoints() {
    _seedPoints.clear();
    switch (_seedPointSource.value()) {
        case SeedPointSourceFile:
            loadSeedPointsFromFile();
            break;
        case SeedPointSourceTable:
            loadSeedPointsFromTable();
            break;
        default:
            throw ghoul::MissingCaseException();
    }
}

void RenderableFieldlines::loadSeedPointsFromFile() {
    LINFO(fmt::format("Reading seed points from '{}'", _seedPointSourceFile.value()));

    std::ifstream seedFile(_seedPointSourceFile);
    if (!seedFile.good())
        LERROR(fmt::format(
            "Could not open seed points file '{}'", _seedPointSourceFile.value()
        ));
    else {
        std::string line;
        while (std::getline(seedFile, line)) {
            std::stringstream s(line);

            glm::vec3 point;
            s >> point.x;
            s >> point.y;
            s >> point.z;
            _seedPoints.push_back(std::move(point));
        }
    }
}

void RenderableFieldlines::loadSeedPointsFromTable() {
    // @TODO needs testing ---abock
    LINFO("Loading provided list of seed points");
    ghoul::Dictionary seedpointsDictionary;
    _seedPointsInfo.getValue(KeySeedPointsTable, seedpointsDictionary);
    glm::vec3 seedPos;
    for (const std::string& index : seedpointsDictionary.keys()) {
        _fieldlineInfo.getValue(std::string(KeySeedPointsTable) + "." + index, seedPos);
        _seedPoints.push_back(seedPos);
    }
}

std::vector<RenderableFieldlines::Line> RenderableFieldlines::generateFieldlines() {
    std::string type;
    bool success = _vectorFieldInfo.getValue(KeyVectorFieldType, type);
    if (!success) {
        LERROR(fmt::format(
            "{} does not contain a '{}' key", KeyVectorField, KeyVectorFieldType
        ));
        return {};
    }

    if (type == VectorFieldTypeVolumeKameleon) {
        return generateFieldlinesVolumeKameleon();
    }
    else {
        LERROR(fmt::format(
            "{}.{} does not name a valid type", KeyVectorField, KeyVectorFieldType
        ));
        return {};
    }
}

std::vector<RenderableFieldlines::Line>
RenderableFieldlines::generateFieldlinesVolumeKameleon()
{
    std::string model;
    bool success = _vectorFieldInfo.getValue(KeyVectorFieldVolumeModel, model);
    if (!success) {
        LERROR(fmt::format("{} does not name a model", KeyVectorField));
        return {};
    }

    std::string fileName;
    success = _vectorFieldInfo.getValue(KeyVectorFieldFile, fileName);
    if (!success) {
        LERROR(fmt::format("{} does not name a file", KeyVectorField));
        return {};
    }
    fileName = absPath(fileName);

    //KameleonWrapper::Model modelType;
    if (model != VectorFieldKameleonModelBATSRUS) {
        //modelType = KameleonWrapper::Model::BATSRUS;
    //else {
        LERROR(fmt::format(
            "{}.{} model '{}' not supported",
            KeyVectorField, KeyVectorFieldVolumeModel, model
        ));
        return {};
    }

    const std::string v1 = std::string(KeyVectorFieldVolumeVariable) + ".1";
    const std::string v2 = std::string(KeyVectorFieldVolumeVariable) + ".2";
    const std::string v3 = std::string(KeyVectorFieldVolumeVariable) + ".3";

    const bool threeVariables = _vectorFieldInfo.hasKeyAndValue<std::string>(v1) &&
                          _vectorFieldInfo.hasKeyAndValue<std::string>(v2) &&
                          _vectorFieldInfo.hasKeyAndValue<std::string>(v3);

    const bool lorentzForce = _vectorFieldInfo.hasKeyAndValue<std::string>(v1) &&
          (_vectorFieldInfo.value<std::string>(v1) == VectorFieldKameleonVariableLorentz);

    if (!threeVariables && !lorentzForce) {
        LERROR(fmt::format("'{}' does not name variables", KeyVectorField));
        return {};
    }

    if (threeVariables) {
        std::string xVariable = _vectorFieldInfo.value<std::string>(v1);
        std::string yVariable = _vectorFieldInfo.value<std::string>(v2);
        std::string zVariable = _vectorFieldInfo.value<std::string>(v3);

        KameleonWrapper kw(fileName);
        return kw.classifiedFieldLines(
            xVariable,
            yVariable,
            zVariable,
            _seedPoints,
            _stepSize
        );
    }

    if (lorentzForce) {
        KameleonWrapper kw(fileName);
        return kw.lorentzTrajectories(_seedPoints, _fieldlineColor, _stepSize);
    }

    ghoul_assert(false, "Should not reach this");
    return {};
}

} // namespace openspace
