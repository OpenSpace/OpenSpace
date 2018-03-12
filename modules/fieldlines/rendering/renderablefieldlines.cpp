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
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/programobject.h>

#include <fstream>

namespace {

    std::string _loggerCat = "RenderableFieldlines";

    constexpr const float defaultFieldlineStepSize = 0.5f;
    const glm::vec4 defaultFieldlineColor = glm::vec4(1.f, 0.f, 0.f, 1.f);

    constexpr const char* keyVectorField = "VectorField";
    constexpr const char* keyVectorFieldType = "Type";
    constexpr const char* keyVectorFieldFile = "File";
    constexpr const char* keyVectorFieldVolumeModel = "Model";
    constexpr const char* keyVectorFieldVolumeVariable = "Variables";

    constexpr const char* keyFieldlines = "Fieldlines";
    constexpr const char* keyFieldlinesStepSize = "Stepsize";
    constexpr const char* keyFieldlinesClassification = "Classification";
    constexpr const char* keyFieldlinesColor = "Color";

    constexpr const char* keySeedPoints = "SeedPoints";
    constexpr const char* keySeedPointsType = "Type";
    constexpr const char* keySeedPointsFile = "File";
    constexpr const char* keySeedPointsTable = "SeedPoints";

    constexpr const char* seedPointsSourceFile = "File";
    constexpr const char* seedPointsSourceTable = "Table";

    constexpr const char* vectorFieldTypeVolumeKameleon = "VolumeKameleon";

    constexpr const char* vectorFieldKameleonModelBATSRUS = "BATSRUS";

    constexpr const char* vectorFieldKameleonVariableLorentz = "Lorentz";

    constexpr const int SeedPointSourceFile = 0;
    constexpr const int SeedPointSourceTable = 1;

    static const openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Fieldline Step Size",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo Classification = {
        "Classification",
        "Fieldline Classification",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo FieldlineColorInfo = {
        "FieldlineColor",
        "Fieldline Color",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo SeedPointSourceInfo = {
        "Source",
        "SeedPoint Source",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo SeedPointFileInfo = {
        "SourceFile",
        "SeedPoint File",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace {

RenderableFieldlines::RenderableFieldlines(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _stepSize(StepSizeInfo, defaultFieldlineStepSize, 0.f, 10.f)
    , _classification(Classification, true)
    , _fieldlineColor(
        FieldlineColorInfo,
        defaultFieldlineColor,
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _seedPointSource(SeedPointSourceInfo)
    , _seedPointSourceFile(SeedPointFileInfo)
    , _program(nullptr)
    , _seedPointsAreDirty(true)
    , _fieldLinesAreDirty(true)
    , _fieldlineVAO(0)
    , _vertexPositionBuffer(0)
{
    ghoul_assert(
        dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyName),
        "Renderable does not have a name"
    );

    std::string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);
    setName(name);

    _loggerCat = "RenderableFieldlines [" + name + "]";

    bool success = dictionary.getValue(keyVectorField, _vectorFieldInfo);
    if (!success) {
        LERROR(fmt::format("Renderable does not contain a key for '{}'", keyVectorField));
    }

    success = dictionary.getValue(keyFieldlines, _fieldlineInfo);
    if (!success) {
        LERROR(fmt::format("Renderable does not contain a key for '{}'", keyFieldlines));
    }

    success = dictionary.getValue(keySeedPoints, _seedPointsInfo);
    if (!success) {
        LERROR(fmt::format("Renderable does not contain a key for '{}", keySeedPoints));
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

    // OsEng.gui()._property.registerProperty(&_enabled);
    // OsEng.gui()._property.registerProperty(&_stepSize);
    // OsEng.gui()._property.registerProperty(&_classification);
    // OsEng.gui()._property.registerProperty(&_fieldlineColor);
    // OsEng.gui()._property.registerProperty(&_seedPointSource);
    // OsEng.gui()._property.registerProperty(&_seedPointSourceFile);
}

void RenderableFieldlines::initializeDefaultPropertyValues() {
    bool success;

    // Step size
    float stepSize;
    success = _fieldlineInfo.getValue(keyFieldlinesStepSize, stepSize);
    if (success) {
        _stepSize = stepSize;
    }

    // Classification
    bool classification;
    success = _fieldlineInfo.getValue(keyFieldlinesClassification, classification);
    if (success) {
        _classification = classification;
    }

    // Fieldline Color
    glm::vec4 color;
    success = _fieldlineInfo.getValue(keyFieldlinesColor, color);
    if (success) {
        _fieldlineColor = color;
    }

    // Seedpoints Type
    std::string sourceType;
    success = _seedPointsInfo.getValue(keySeedPointsType, sourceType);
    if (success) {
        if (sourceType == seedPointsSourceFile) {
            _seedPointSource = SeedPointSourceFile;

            std::string seedPointSourceFile;
            success = _seedPointsInfo.getValue(keySeedPointsFile, seedPointSourceFile);
            if (success)
                _seedPointSourceFile = absPath(seedPointSourceFile);
        }
        else if (sourceType == seedPointsSourceTable)
            _seedPointSource = SeedPointSourceTable;
    }
}

bool RenderableFieldlines::isReady() const {
    bool programReady = _program != nullptr;
    bool vectorFieldReady = !_vectorFieldInfo.empty();
    bool fieldlineReady = !_fieldlineInfo.empty();
    bool seedPointsReady = !_seedPointsInfo.empty();
    return programReady && vectorFieldReady && fieldlineReady && seedPointsReady;
}

void RenderableFieldlines::initializeGL() {
    if (_vectorFieldInfo.empty() || _fieldlineInfo.empty() || _seedPointsInfo.empty()) {
        throw ghoul::RuntimeError("Error initializing");
    }

    _program = OsEng.renderEngine().buildRenderProgram(
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

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_program) {
        renderEngine.removeRenderProgram(_program);
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
    if (!_classification)
        _program->setUniform("fieldLineColor", _fieldlineColor);

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
        std::vector<Line> fieldlines = generateFieldlines();

        if (fieldlines.empty()) {
            return ;
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
            reinterpret_cast<void*>(0)
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
            "Could not open seed points file '{}'",
            _seedPointSourceFile.value()
        ));
    else {
        std::string line;
        glm::vec3 point;
        while (std::getline(seedFile, line)) {
            std::stringstream s(line);
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
    _seedPointsInfo.getValue(keySeedPointsTable, seedpointsDictionary);
    glm::vec3 seedPos;
    for (const std::string& index : seedpointsDictionary.keys()) {
        _fieldlineInfo.getValue(std::string(keySeedPointsTable) + "." + index, seedPos);
        _seedPoints.push_back(seedPos);
    }
}

std::vector<RenderableFieldlines::Line> RenderableFieldlines::generateFieldlines() {
    std::string type;
    bool success = _vectorFieldInfo.getValue(keyVectorFieldType, type);
    if (!success) {
        LERROR(fmt::format(
            "{} does not contain a '{}' key",
            keyVectorField,
            keyVectorFieldType
        ));
        return {};
    }

    if (type == vectorFieldTypeVolumeKameleon) {
        return generateFieldlinesVolumeKameleon();
    }
    else {
        LERROR(fmt::format(
            "{}.{} does not name a valid type",
            keyVectorField,
            keyVectorFieldType
        ));
        return {};
    }
}

std::vector<RenderableFieldlines::Line>
RenderableFieldlines::generateFieldlinesVolumeKameleon()
{
    std::string model;
    bool success = _vectorFieldInfo.getValue(keyVectorFieldVolumeModel, model);
    if (!success) {
        LERROR(fmt::format("{} does not name a model", keyVectorField));
        return {};
    }

    std::string fileName;
    success = _vectorFieldInfo.getValue(keyVectorFieldFile, fileName);
    if (!success) {
        LERROR(fmt::format("{} does not name a file", keyVectorField));
        return {};
    }
    fileName = absPath(fileName);

    //KameleonWrapper::Model modelType;
    if (model != vectorFieldKameleonModelBATSRUS) {
        //modelType = KameleonWrapper::Model::BATSRUS;
    //else {
        LERROR(fmt::format(
            "{}.{} model '{}' not supported",
            keyVectorField,
            keyVectorFieldVolumeModel,
            model
        ));
        return {};
    }

    std::string v1 = std::string(keyVectorFieldVolumeVariable) + ".1";
    std::string v2 = std::string(keyVectorFieldVolumeVariable) + ".2";
    std::string v3 = std::string(keyVectorFieldVolumeVariable) + ".3";

    bool threeVariables =
        _vectorFieldInfo.hasKeyAndValue<std::string>(v1) &&
        _vectorFieldInfo.hasKeyAndValue<std::string>(v2) &&
        _vectorFieldInfo.hasKeyAndValue<std::string>(v3);

    bool lorentzForce =
      _vectorFieldInfo.hasKeyAndValue<std::string>(v1) &&
      (_vectorFieldInfo.value<std::string>(v1) == vectorFieldKameleonVariableLorentz);

    if (!threeVariables && !lorentzForce) {
        LERROR(fmt::format("'{}' does not name variables", keyVectorField));
        return {};
    }

    if (threeVariables) {
        std::string xVariable, yVariable, zVariable;
        _vectorFieldInfo.getValue(v1, xVariable);
        _vectorFieldInfo.getValue(v2, yVariable);
        _vectorFieldInfo.getValue(v3, zVariable);

        KameleonWrapper kw(fileName);
        return kw.getClassifiedFieldLines(
            xVariable,
            yVariable,
            zVariable,
            _seedPoints,
            _stepSize
        );
    }

    if (lorentzForce) {
        KameleonWrapper kw(fileName);
        return kw.getLorentzTrajectories(_seedPoints, _fieldlineColor, _stepSize);
    }

    ghoul_assert(false, "Should not reach this");
    return {};
}

} // namespace openspace
