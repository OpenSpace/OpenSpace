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

#include <modules/fieldlines/rendering/renderablefieldlines.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/time.h>

#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/programobject.h>

#include <fstream>

// ADDED BY CARLBAUM to
// Bypass wrapper
#include "ccmc/Kameleon.h"
#include "ccmc/Tracer.h"
#include "ccmc/Point3f.h"
// #include <ccmc/cxform.h>

namespace {
    std::string _loggerCat = "RenderableFieldlines";

    const float defaultFieldlineStepSize = 0.5f;
    const glm::vec4 defaultFieldlineColor = glm::vec4(1.f, 0.f, 0.f, 1.f);

    const char* keyVectorField = "VectorField";
    const char* keyVectorFieldType = "Type";
    const char* keyVectorFieldFile = "File";
    const char* keyVectorFieldVolumeModel = "Model";
    const char* keyVectorFieldVolumeVariable = "Variables";
    const char* keyVectorFieldTimeDependent = "TimeDependent";

    const char* keyFieldlines = "Fieldlines";
    const char* keyFieldlinesStepSize = "Stepsize";
    const char* keyFieldlinesClassification = "Classification";
    const char* keyFieldlinesColor = "Color";

    const char* keySeedPoints = "SeedPoints";
    const char* keySeedPointsType = "Type";
    const char* keySeedPointsFile = "File";
    const char* keySeedPointsTable = "SeedPoints";

    const char* seedPointsSourceFile = "File";
    const char* seedPointsSourceTable = "Table";

    const char* vectorFieldTypeVolumeKameleon = "VolumeKameleon";

    const char* vectorFieldKameleonModelBATSRUS = "BATSRUS";
    const char* vectorFieldKameleonModelENLIL = "ENLIL";

    const char* vectorFieldKameleonVariableLorentz = "Lorentz";

    const int SeedPointSourceFile = 0;
    const int SeedPointSourceTable = 1;

    const char* GSM = "GSM";
    const char* J2000 = "J2000";
}
const float R_E_TO_METER = 6371000.f; // Earth radius
const float R_S_TO_METER = 695700000.f; // Sun radius
const float A_U_TO_METER = 149597871000.f; // Astronomical Units
const float A_U_TO_KM = 149597871.f; // Astronomical Units
const float DEG_TO_RAD = 3.14159265359f / 180.f;

namespace openspace {

RenderableFieldlines::RenderableFieldlines(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _stepSize("stepSize", "Fieldline Step Size", defaultFieldlineStepSize, 0.f, 10.f)
    , _classification("classification", "Fieldline Classification", true)
    , _isTimeDependent("isTimeDependent", "Is Time Dependent", false)
    , _fieldlineColor(
        "fieldlineColor",
        "Fieldline Color",
        defaultFieldlineColor,
        glm::vec4(0.f),
        glm::vec4(1.f)
      )
    , _seedPointSource("source", "SeedPoint Source")
    , _seedPointSourceFile("sourceFile", "SeedPoint File")
    , _program(nullptr)
    , _seedPointsAreDirty(true)
    , _fieldLinesAreDirty(true)
    , _isWithinTimeInterval(true)
    , _fieldlineVAO(0)
    , _vertexPositionBuffer(0)
{
    ghoul_assert(
        dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyName),
        "Renderable does not have a name"
    );

    std::string name;
    dictionary.getValue(SceneGraphNode::KeyName, name);
    setName(name); //TODO redundant?

    _loggerCat = "RenderableFieldlines [" + name + "]";

    bool success = dictionary.getValue(keyVectorField, _vectorFieldInfo);
    if (!success) {
        LERROR("Renderable does not contain a key for '" <<
            keyVectorField << "'");
    }

    success = dictionary.getValue(keyFieldlines, _fieldlineInfo);
    if (!success) {
        LERROR("Renderable does not contain a key for '" <<
            keyFieldlines << "'");
    }

    success = dictionary.getValue(keySeedPoints, _seedPointsInfo);
    if (!success) {
        LERROR("Renderable does not contain a key for '" <<
            keySeedPoints << "'");
    }

    // @TODO a non-magic number perhaps ---abock
    const float EARTH_RADIUS = 6371000.f;
    setBoundingSphere(250.f*EARTH_RADIUS);

    _seedPointSource.addOption(SeedPointSourceFile, "File");
    _seedPointSource.addOption(SeedPointSourceTable, "Lua Table");

    initializeDefaultPropertyValues();

    // @TODO hook up visibility changes ---abock

    auto dirtyFieldlines = [this]() { this->_fieldLinesAreDirty = true; };
    auto dirtySeedpoints = [this]() { this->_seedPointsAreDirty = true; };

    _stepSize.onChange(dirtyFieldlines);
    addProperty(_stepSize);

    addProperty(_classification);

    addProperty(_isTimeDependent);

    _fieldlineColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_fieldlineColor);

    _seedPointSource.onChange(dirtySeedpoints);
    addProperty(_seedPointSource);

    _seedPointSourceFile.onChange(dirtySeedpoints);
    addProperty(_seedPointSourceFile);


    _osTime = &Time::ref();

    // OsEng.gui()._property.registerProperty(&_enabled);
    // OsEng.gui()._property.registerProperty(&_stepSize);
    // OsEng.gui()._property.registerProperty(&_classification);
    // OsEng.gui()._property.registerProperty(&_isTimeDependent);
    // OsEng.gui()._property.registerProperty(&_fieldlineColor);
    // OsEng.gui()._property.registerProperty(&_seedPointSource);
    // OsEng.gui()._property.registerProperty(&_seedPointSourceFile);
}

void RenderableFieldlines::initializeDefaultPropertyValues() {
    bool success;

    // Step size
    float stepSize;
    success = _fieldlineInfo.getValue(keyFieldlinesStepSize, stepSize);
    if (success)
        _stepSize = stepSize;

    // Classification
    bool classification;
    success = _fieldlineInfo.getValue(keyFieldlinesClassification, classification);
    if (success)
        _classification = classification;

    // Fieldline Color
    glm::vec4 color;
    success = _fieldlineInfo.getValue(keyFieldlinesColor, color);
    if (success)
        _fieldlineColor = color;

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

bool RenderableFieldlines::initialize() {
    _updateCoordinates = false;
    if (_vectorFieldInfo.empty() || _fieldlineInfo.empty() || _seedPointsInfo.empty()) {
        return false;
    }

    _program = OsEng.renderEngine().buildRenderProgram(
        "Fieldline",
        "${MODULE_FIELDLINES}/shaders/fieldline_flow_direction_vs.glsl",
        "${MODULE_FIELDLINES}/shaders/fieldline_flow_direction_fs.glsl"
        //"${MODULE_FIELDLINES}/shaders/fieldline_vs.glsl",
        //"${MODULE_FIELDLINES}/shaders/fieldline_fs.glsl",
        //"${MODULE_FIELDLINES}/shaders/fieldline_gs.glsl"
    );

    if (!_program)
        return false;

    return true;
}

bool RenderableFieldlines::deinitialize() {
    glDeleteVertexArrays(1, &_fieldlineVAO);
    _fieldlineVAO = 0;
    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_program) {
        renderEngine.removeRenderProgram(_program);
        _program = nullptr;
    }

    return true;
}

void RenderableFieldlines::render(const RenderData& data) {
    if (_isWithinTimeInterval) {
        _program->activate();

        glm::dmat4 rotationTransform = glm::dmat4(data.modelTransform.rotation);
        glm::mat4 scaleTransform = glm::mat4(1.0);
        glm::dmat4 modelTransform =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
            rotationTransform *
            glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale))) *
            glm::dmat4(scaleTransform);
        glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;

        // Set uniforms for shaders
        _program->setUniform("modelViewProjection",
                data.camera.projectionMatrix() * glm::mat4(modelViewTransform));

        //_program->setUniform("modelViewProjection", data.camera.viewProjectionMatrix());
        //_program->setUniform("modelTransform", glm::mat4(1.0));
        int testTime = static_cast<int>(OsEng.runTime()*100)/5;
        _program->setUniform("time", testTime);
        //_program->setUniform("cameraViewDir", glm::vec3(data.camera.viewDirectionWorldSpace()));
        glDisable(GL_CULL_FACE);
        //setPscUniforms(*_program, data.camera, data.position);

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
}

void RenderableFieldlines::update(const UpdateData&) {
    if (_program->isDirty())
        _program->rebuildFromFile();

    if (_seedPointsAreDirty) {
        loadSeedPoints();
        _seedPointsAreDirty = false;
        _fieldLinesAreDirty = true;
    }

    _currentTime = _osTime->j2000Seconds();

    if (_fieldLinesAreDirty) {
        std::vector<Line> fieldlines = generateFieldlines();

        if (fieldlines.empty())
            return ;

        int prevEnd = 0;

          // DEBUG -- DRAW COORDINATE VECTORS
        // {
        //     // float vectorScale = 1.f;
        //     // float vectorScale = R_E_TO_METER;
        //     // float vectorScale = R_S_TO_METER;
        //     float vectorScale = A_U_TO_METER;
        //     //std::vector<Line> coordinateAxes;
        //     Line xAxis, yAxis, zAxis;

        //     // ccmc::Kameleon* kameleon = new ccmc::Kameleon;
        //     // ccmc::Position o_kam{0.0f, 0.0f, 0.0f};
        //     // ccmc::Position x_kam{vectorScale, 0.0f, 0.0f};
        //     // ccmc::Position y_kam{0.0f, vectorScale, 0.0f};
        //     // ccmc::Position z_kam{0.0f, 0.0f, vectorScale};
        //     // ccmc::Position o_kam_o;
        //     // ccmc::Position x_kam_o;
        //     // ccmc::Position y_kam_o;
        //     // ccmc::Position z_kam_o;
        //     // double orig_in[3] = {0.1, 0.1, 0.1};
        //     // double x_in[3] = {1.0 * vectorScale, 0.0, 0.0};
        //     // double y_in[3] = {0.0, 1.0 * vectorScale, 0.0};
        //     // double z_in[3] = {0.0, 0.0, 1.0 * vectorScale};
        //     // double orig_out[3];
        //     // double x_out[3];
        //     // double y_out[3];
        //     // double z_out[3];

        //     // int o = kameleon->_cxform(GSM, J2000, _currentTime, &o_kam, &o_kam_o);
        //     // int a = kameleon->_cxform(GSM, J2000, _currentTime, &x_kam, &x_kam_o);
        //     // int b = kameleon->_cxform(GSM, J2000, _currentTime, &y_kam, &y_kam_o);
        //     // int c = kameleon->_cxform(GSM, J2000, _currentTime, &z_kam, &z_kam_o);
        //     // int a = cxform(GSM, J2000, _currentTime, x_in, x_out);
        //     // int b = cxform(GSM, J2000, _currentTime, y_in, y_out);
        //     // int c = cxform(GSM, J2000, _currentTime, z_in, z_out);
        //     // glm::vec3 x_glm = glm::vec3(x_out[0], x_out[1], x_out[2]) * vectorScale;
        //     // glm::vec3 y_glm = glm::vec3(y_out[0], y_out[1], y_out[2]) * vectorScale;
        //     // glm::vec3 z_glm = glm::vec3(z_out[0], z_out[1], z_out[2]) * vectorScale;
        //     // glm::vec3 orig_glm = glm::vec3(orig_out[0], orig_out[1], orig_out[2]) /* vectorScale*/;

        //     // LinePoint origin = LinePoint(orig_glm, glm::vec4(1.0f, 1.0f, 1.0f, 1.0f));
        //     // LinePoint x      = LinePoint(x_glm, glm::vec4(1.0f, 0.0f, 0.0f, 1.0f));
        //     // LinePoint y      = LinePoint(y_glm, glm::vec4(0.0f, 1.0f, 0.0f, 1.0f));
        //     // LinePoint z      = LinePoint(z_glm, glm::vec4(0.0f, 0.0f, 1.0f, 1.0f));
        //     // LinePoint origin = LinePoint(glm::vec3(0.1f, 0.1f, 0.1f),               glm::vec4(1.0f, 1.0f, 1.0f, 1.0f));
        //     // LinePoint x      = LinePoint(glm::vec3(1.0f * vectorScale, 0.0f, 0.0f), glm::vec4(1.0f, 0.0f, 0.0f, 1.0f));
        //     // LinePoint y      = LinePoint(glm::vec3(0.0f, 1.0f * vectorScale, 0.0f), glm::vec4(0.0f, 1.0f, 0.0f, 1.0f));
        //     // LinePoint z      = LinePoint(glm::vec3(0.0f, 0.0f, 1.0f * vectorScale), glm::vec4(0.0f, 0.0f, 1.0f, 1.0f));
        //     // LinePoint ox = LinePoint(glm::vec3(o_kam_o.c0 + 0.01f, o_kam_o.c1 + 0.01f, o_kam_o.c2 + 0.01f), glm::vec4(1.0f, 0.0f, 0.0f, 1.0f))    ;
        //     // LinePoint oy = LinePoint(glm::vec3(o_kam_o.c0 + 0.01f, o_kam_o.c1 + 0.01f, o_kam_o.c2 + 0.01f), glm::vec4(0.0f, 1.0f, 0.0f, 1.0f));
        //     // LinePoint oz = LinePoint(glm::vec3(o_kam_o.c0 + 0.01f, o_kam_o.c1 + 0.01f, o_kam_o.c2 + 0.01f), glm::vec4(0.0f, 0.0f, 1.0f, 1.0f));
        //     // LinePoint x  = LinePoint(glm::vec3(x_kam_o.c0 + 0.01f, x_kam_o.c1 + 0.01f, x_kam_o.c2 + 0.01f), glm::vec4(1.0f, 0.0f, 0.0f, 1.0f));
        //     // LinePoint y  = LinePoint(glm::vec3(y_kam_o.c0 + 0.01f, y_kam_o.c1 + 0.01f, y_kam_o.c2 + 0.01f), glm::vec4(0.0f, 1.0f, 0.0f, 1.0f));
        //     // LinePoint z  = LinePoint(glm::vec3(z_kam_o.c0 + 0.01f, z_kam_o.c1 + 0.01f, z_kam_o.c2 + 0.01f), glm::vec4(0.0f, 0.0f, 1.0f, 1.0f));

        //     LinePoint ox = LinePoint(glm::vec3(0.f,0.f,0.f), glm::vec4(1.0f, 0.0f, 0.0f, 1.0f));
        //     LinePoint oy = LinePoint(glm::vec3(0.f,0.f,0.f), glm::vec4(0.0f, 1.0f, 0.0f, 1.0f));
        //     LinePoint oz = LinePoint(glm::vec3(0.f,0.f,0.f), glm::vec4(0.0f, 0.0f, 1.0f, 1.0f));
        //     LinePoint x  = LinePoint(glm::vec3(vectorScale,0.f,0.f), glm::vec4(1.0f, 0.0f, 0.0f, 1.0f));
        //     LinePoint y  = LinePoint(glm::vec3(0.f,vectorScale,0.f), glm::vec4(0.0f, 1.0f, 0.0f, 1.0f));
        //     LinePoint z  = LinePoint(glm::vec3(0.f,0.f,vectorScale), glm::vec4(0.0f, 0.0f, 1.0f, 1.0f));

        //     xAxis.push_back(ox); //For some reason these are needed twice.. TODO: Look into why!
        //     xAxis.push_back(ox);
        //     xAxis.push_back(x);
        //     xAxis.push_back(x);

        //     yAxis.push_back(oy);
        //     yAxis.push_back(oy);
        //     yAxis.push_back(y);
        //     yAxis.push_back(y);

        //     zAxis.push_back(oz);
        //     zAxis.push_back(oz);
        //     zAxis.push_back(z);
        //     zAxis.push_back(z);

        //     fieldlines.push_back(xAxis);
        //     fieldlines.push_back(yAxis);
        //     fieldlines.push_back(zAxis);
        //     // for (int j = 0; j < coordinateAxes.size(); ++j) {
        //     //     _lineStart.push_back(prevEnd);
        //     //     _lineCount.push_back(2);
        //     //     prevEnd += 2;
        //     //     _vertexData.insert(_vertexData.end(), coordinateAxes[j].begin(), coordinateAxes[j].end());
        //     // }

        //     //delete kameleon;
        // }
        // Arrange data for glMultiDrawArrays
        for (int j = 0; j < fieldlines.size(); ++j) {
            _lineStart.push_back(prevEnd);
            _lineCount.push_back(static_cast<int>(fieldlines[j].size()));
            prevEnd = prevEnd + static_cast<int>(fieldlines[j].size());
            _vertexData.insert(_vertexData.end(), fieldlines[j].begin(), fieldlines[j].end());
        }

        LDEBUG("Number of vertices : " << _vertexData.size());

        if (_fieldlineVAO == 0)
            glGenVertexArrays(1, &_fieldlineVAO);
        glBindVertexArray(_fieldlineVAO);

        if (_vertexPositionBuffer == 0)
            glGenBuffers(1, &_vertexPositionBuffer);
        glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

        // Transfer from CPU to GPU (I think)
        glBufferData(GL_ARRAY_BUFFER, _vertexData.size()*sizeof(LinePoint), &_vertexData.front(), GL_STATIC_DRAW);

        GLuint vertexLocation = 0;
        glEnableVertexAttribArray(vertexLocation);
        glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, sizeof(LinePoint), reinterpret_cast<void*>(0));

        GLuint colorLocation = 1;
        glEnableVertexAttribArray(colorLocation);
        glVertexAttribPointer(colorLocation, 4, GL_FLOAT, GL_FALSE, sizeof(LinePoint), (void*)(sizeof(glm::vec3)));

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);

        _fieldLinesAreDirty = false;
        // _updateCoordinates = true;
        // _timeDelay = _currentTime;
    } /*else if (_updateCoordinates && _currentTime - _timeDelay > 20.0){
        // Convert to J2000 coordinates and glm::vec3 format
        //_vertexData.clear();
        ccmc::Kameleon* kameleon = new ccmc::Kameleon;
        for (int j = 0; j < _modelCoordinates.size(); ++j) {
            _vertexData[j].position = convertGsmToJ2000(_modelCoordinates[j], kameleon);
        }
        delete kameleon;


        if (_fieldlineVAO == 0) {
            glGenVertexArrays(1, &_fieldlineVAO);
        }
        glBindVertexArray(_fieldlineVAO);

        if (_vertexPositionBuffer == 0) {
            glGenBuffers(1, &_vertexPositionBuffer);
        }
        glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

        glBufferData(GL_ARRAY_BUFFER, _vertexData.size()*sizeof(LinePoint), &_vertexData.front(), GL_STATIC_DRAW);

        GLuint vertexLocation = 0;
        glEnableVertexAttribArray(vertexLocation);
        glVertexAttribPointer(vertexLocation, 3, GL_FLOAT, GL_FALSE, sizeof(LinePoint), reinterpret_cast<void*>(0));

        GLuint colorLocation = 1;
        glEnableVertexAttribArray(colorLocation);
        glVertexAttribPointer(colorLocation, 4, GL_FLOAT, GL_FALSE, sizeof(LinePoint), (void*)(sizeof(glm::vec3)));

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);
        _timeDelay = _currentTime;
    }*/

    if (_isTimeDependent) {
        if (_currentTime > _startTime && _currentTime < _endTime) {
            _isWithinTimeInterval = true;
        } else {
            _isWithinTimeInterval = false;
        }
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
    LINFO("Reading seed points from file '" << _seedPointSourceFile.value() << "'");

    std::ifstream seedFile(_seedPointSourceFile);
    if (!seedFile.good()) {
        LERROR("Could not open seed points file '" << _seedPointSourceFile.value() << "'");
    } else {
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
        LERROR(keyVectorField << " does not contain a '" <<
            keyVectorFieldType << "' key");
        return {};
    }

    if (type == vectorFieldTypeVolumeKameleon) {
        return generateFieldlinesVolumeKameleonBypassWrapper();
    } else {
        LERROR(keyVectorField << "." << keyVectorFieldType <<
            " does not name a valid type");
        return {};
    }
}

std::vector<RenderableFieldlines::Line>
RenderableFieldlines::generateFieldlinesVolumeKameleon()
{
    std::string model;
    bool success = _vectorFieldInfo.getValue(keyVectorFieldVolumeModel, model);
    if (!success) {
        LERROR(keyVectorField << " does not name a model");
        return {};
    }

    bool isTimeDependent;
    success = _vectorFieldInfo.getValue(keyVectorFieldTimeDependent, isTimeDependent);
    if (success) {
        _isTimeDependent = isTimeDependent;
    }

    std::string fileName;
    success = _vectorFieldInfo.getValue(keyVectorFieldFile, fileName);
    if (!success) {
        LERROR(keyVectorField << " does not name a file");
        return {};
    }
    fileName = absPath(fileName);

    //KameleonWrapper::Model modelType;
    if (model != vectorFieldKameleonModelBATSRUS &&
            model != vectorFieldKameleonModelENLIL) {
    //     LDEBUG(keyVectorField << "." << keyVectorFieldVolumeModel << " model '" << 
    //         model << "' is found");
    //     //modelType = KameleonWrapper::Model::BATSRUS;
    // } else {
        LERROR(keyVectorField << "." << keyVectorFieldVolumeModel << " model '" << 
            model << "' not supported");
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
        LERROR(keyVectorField << " does not name variables");
        return {};
    }

    if (threeVariables) {
        std::string xVariable, yVariable, zVariable;
        _vectorFieldInfo.getValue(v1, xVariable);
        _vectorFieldInfo.getValue(v2, yVariable);
        _vectorFieldInfo.getValue(v3, zVariable);

        KameleonWrapper kw(fileName);
        if (_isTimeDependent) {
            _osTime = &Time::ref();
            _currentTime = _osTime->j2000Seconds();
            std::string tempString = kw.getStartTimeString();
            _startTime = _osTime->convertTime(tempString.substr(0, tempString.length()-2)) + kw.getElapsedTime();
            _endTime = _startTime + 240.0; // TODO this should NOT be a hardcoded value.. should be taken from meta data
        }
        return kw.getClassifiedFieldLines(xVariable, yVariable, zVariable, _seedPoints, _stepSize);
    }

    if (lorentzForce) {
        KameleonWrapper kw(fileName);
        return kw.getLorentzTrajectories(_seedPoints, _fieldlineColor, _stepSize);
    }

    ghoul_assert(false, "Should not reach this");
    return {};
}


// LinePoint convertPoint3fToLinePoint(ccmc::Point3f& p) {
//     return LinePoint(glm::vec3(p.component1*R_E_TO_METER, p.component3*R_E_TO_METER, p.component2*R_E_TO_METER),glm::vec4(.5f,.2f,.8f,.6f));
// }

LinePoint convertGsmPoint3fToJ2000GlmVec3(ccmc::Point3f& p) {
    return LinePoint(glm::vec3( p.component1*R_E_TO_METER,
                                p.component3*R_E_TO_METER,
                                p.component2*R_E_TO_METER),
                                glm::vec4(0.0f,0.2f,1.f,.6f));
}

LinePoint convertHnmPoint3fToJ2000GlmVec3(ccmc::Point3f& p) {
    p.component1 *= A_U_TO_METER *.1f;
    // // ccmc::Point3f temp = p.component1 * A_U_TO_METER;
    // //p = p.getCartesian();
    // float x = p.component1 * cos(DEG_TO_RAD * p.component3) * cos(DEG_TO_RAD * p.component2);
    // float y = p.component1 * cos(DEG_TO_RAD * p.component3) * sin(DEG_TO_RAD * p.component2);
    // float z = p.component1 * sin(DEG_TO_RAD * p.component3);
    // return LinePoint(glm::vec3(x, z, y), glm::vec4(.5f,.2f,.8f,.6f));//*R_E_TO_METER, p.component2*R_E_TO_METER, p.component3*R_E_TO_METER),glm::vec4(.5f,.2f,.8f,.6f));
    return LinePoint(glm::vec3(p.component1, p.component2, p.component3), glm::vec4(.0f,1.0f,0.f,1.0f));//*R_E_TO_METER, p.component2*R_E_TO_METER, p.component3*R_E_TO_METER),glm::vec4(.5f,.2f,.8f,.6f));
}

LinePoint convertHnmToCartesian(const ccmc::Point3f& p) {
    float radius_meters = A_U_TO_METER *  p.component1;
    float lat_rad       =   DEG_TO_RAD * (90.f - p.component2);
    float lon_rad       =   DEG_TO_RAD *  p.component3;
    float x = radius_meters * sin(lat_rad) * cos(lon_rad);
    float y = radius_meters * sin(lat_rad) * sin(lon_rad);
    float z = radius_meters * cos(lat_rad);
    // float y = p.component1 * cos(DEG_TO_RAD * p.component3) * sin(DEG_TO_RAD * p.component2);
    // float z = p.component1 * sin(DEG_TO_RAD * p.component3);
    // return LinePoint(glm::vec3(x, z, y), glm::vec4(.5f,.2f,.8f,.6f));//*R_E_TO_METER, p.component2*R_E_TO_METER, p.component3*R_E_TO_METER),glm::vec4(.5f,.2f,.8f,.6f));
    return LinePoint(glm::vec3(x,y,z), glm::vec4(1.0f, 1.0f,1.0f,1.0f));//*R_E_TO_METER, p.component2*R_E_TO_METER, p.component3*R_E_TO_METER),glm::vec4(.5f,.2f,.8f,.6f));
}

LinePoint convertHnmToCartesian(const ccmc::Point3f& p, const glm::vec4& color) {
    float radius_meters = A_U_TO_METER *  p.component1;
    float lat_rad       =   DEG_TO_RAD *  p.component2;
    float lon_rad       =   DEG_TO_RAD *-(p.component3-90.f);
    float x = radius_meters * sin(lat_rad) * cos(lon_rad);
    float y = radius_meters * sin(lat_rad) * sin(lon_rad);
    float z = radius_meters * cos(lat_rad);
    // float y = p.component1 * cos(DEG_TO_RAD * p.component3) * sin(DEG_TO_RAD * p.component2);
    // float z = p.component1 * sin(DEG_TO_RAD * p.component3);
    // return LinePoint(glm::vec3(x, z, y), glm::vec4(.5f,.2f,.8f,.6f));//*R_E_TO_METER, p.component2*R_E_TO_METER, p.component3*R_E_TO_METER),glm::vec4(.5f,.2f,.8f,.6f));
    return LinePoint(glm::vec3(x,y,z), color);//*R_E_TO_METER, p.component2*R_E_TO_METER, p.component3*R_E_TO_METER),glm::vec4(.5f,.2f,.8f,.6f));
}

glm::vec3 RenderableFieldlines::convertGsmToJ2000(ccmc::Position& p, ccmc::Kameleon* kameleon) {
    ccmc::Position pJ2000; // position
    // int a = kameleon->_cxform(GSM, J2000, _currentTime, &p, &pJ2000);
    // TODO ASSERTION HERE? Check that a == 1 ?
    // return static_cast<glm::vec3>(pJ2000); // Wont work I think, so try the row below instead
    // return glm::vec3{pJ2000.c0, pJ2000.c1, pJ2000.c2} * R_E_TO_METER;
    return glm::vec3{p.c0, p.c1, p.c2} * R_E_TO_METER; // TODO: swap c1 & c2 ? x,z,y?
}

glm::vec3 RenderableFieldlines::convertHnmToJ2000(ccmc::Position& p, ccmc::Kameleon* kameleon) {
    // float radius_meters = R_S_TO_METER *  p.c0;
    // float lat_rad       =   DEG_TO_RAD *  p.c1;
    // float lon_rad       =   DEG_TO_RAD *-(p.c2-90.f);
    // float x = radius_meters * sin(lat_rad) * cos(lon_rad);
    // float y = radius_meters * sin(lat_rad) * sin(lon_rad);
    // float z = radius_meters * cos(lat_rad);
    // return glm::vec3{x, y, z};
    // return glm::vec3{p.c0 * A_U_TO_METER, p.c1, p.c2};

    // p.c0 = radius    (MEASURED IN AU?)
    // p.c1 = latitude  (MEASURED IN DEGREES? from -90 to +90)
    // p.c2 = longitude (MEASURED IN DEGREES! from 0 to 360)
    // float r         = R_S_TO_METER * p.c0;
    float r         = A_U_TO_METER * p.c0;
    float lat_rad   = DEG_TO_RAD   * p.c1;
    float lon_rad   = DEG_TO_RAD   * p.c2;
    float r_cosLat  = r * cos(lat_rad);
    return glm::vec3(r_cosLat * cos(lon_rad), r_cosLat * sin(lon_rad), r * sin(lat_rad));
}

std::vector<RenderableFieldlines::Line>
RenderableFieldlines::generateFieldlinesVolumeKameleonBypassWrapper() {

    std::string fileName;

    bool success = _vectorFieldInfo.getValue(keyVectorFieldFile, fileName);
    if (!success) {
        LERROR(keyVectorField << " does not name a file");
        return {};
    }
    fileName = absPath(fileName);

    ccmc::Kameleon* kameleon = new ccmc::Kameleon;
    long status = kameleon->open(fileName);
    if (status == ccmc::FileReader::OK) {
        kameleon->loadVariable("b");
        // kameleon->loadVariable("br");
        // kameleon->loadVariable("btheta");
        // kameleon->loadVariable("bphi");
        bool isEnlil = kameleon->getModelName() == "enlil";
        ccmc::Tracer tracer(kameleon);
        tracer.setMaxIterations(5000);
        tracer.setInnerBoundary(.1f);
        std::vector<Line> fieldlines;

        for (glm::vec3 seedPoint : _seedPoints) {
            // ccmc::Fieldline ccmcFieldline = tracer.bidirectionalTrace("b", 1.f, 0.f, 0.f); //TODO convert positions to glm::vec3
            ccmc::Fieldline ccmcFieldline = tracer.bidirectionalTrace("b", seedPoint.x, seedPoint.y, seedPoint.z); //TODO convert positions to glm::vec3
            std::vector<ccmc::Point3f> ccmcLinePoints = ccmcFieldline.getPositions();

            Line tempLine;
            // std::for_each (ccmcLinePoints.begin(), ccmcLinePoints.end(),
            //         [&tempLine](ccmc::Point3f p) {
            //                 tempLine.push_back(convertPoint3fToLinePoint(isSphericalCoordinates ? p.getCartesian() : p));
            //         });
            // tempLine.insert(tempLine.begin(), ccmcLinePoints.begin(), ccmcLinePoints.end());
            float minX, minY, minZ;
            minX = minY = minZ = FLT_MAX;
            float maxX, maxY, maxZ;
            maxX = maxY = maxZ = FLT_MIN;
            int counter = 0;
            int size = ccmcLinePoints.size();
            float delta = 1.0f / static_cast<float>(size);
            for (auto it = ccmcLinePoints.begin(); it != ccmcLinePoints.end(); ++it) {
                // ++counter;
                ccmc::Position pModel{it->component1, it->component2, it->component3}; // position in GSM
                _modelCoordinates.push_back(pModel);
                if (pModel.c0 > maxX) maxX = pModel.c0;
                if (pModel.c1 > maxY) maxY = pModel.c1;
                if (pModel.c2 > maxZ) maxZ = pModel.c2;
                if (pModel.c0 < minX) minX = pModel.c0;
                if (pModel.c1 < minY) minY = pModel.c1;
                if (pModel.c2 < minZ) minZ = pModel.c2;
                tempLine.push_back( LinePoint(
                        isEnlil ? convertHnmToJ2000(pModel, kameleon) : convertGsmToJ2000(pModel, kameleon),
                        glm::vec4(1.0f,1.0f-delta*counter,1.0f,1.0f)));
                // tempLine.push_back( LinePoint(it->component1, it->component2, it->component3),
                //         glm::vec4(1.0f,1.0f-delta*counter,1.0f,1.0f));
                        // glm::vec4(1.0f,1.0f/static_cast<float>(counter),1.0f,1.0f)));
                ++counter;
            }
            fieldlines.push_back(tempLine);
        }
        if (isEnlil) {
            Line tempLine;
            Line tempLineX;
            Line tempLineY;
            Line tempLineZ;
            // tempLine.push_back(LinePoint(glm::vec3( 0.0f, 0.0f, 0.0f), glm::vec4(.2f,.8f,.5f,.6f)));
            // tempLine.push_back(LinePoint(glm::vec3( 1.0f, 0.0f, 0.0f), glm::vec4(.5f,.2f,.8f,.6f)));
            // tempLine.push_back(convertGsmToJ2000(ccmc::Point3f( 0.00000001f, 0.00000001f, 0.00000001f), glm::vec4(0.0f, 1.0f, 0.0f, 1.0f)));
            // tempLine.push_back(convertGsmToJ2000(ccmc::Point3f( 1.0f, 0.0f, 0.0f), glm::vec4(1.0f, 0.0f, 0.0f, 1.0f)));
            ccmc::Position orig{0.0f,0.0f,0.0f};
            ccmc::Position earth{1.0f,-7.0f,0.0f};
            ccmc::Position x{1.0f,0.0f,0.0f};
            ccmc::Position y{1.0f,90.0f,0.0f};
            ccmc::Position z{1.0f,0.0f,90.0f};

            tempLine.push_back(LinePoint(convertHnmToJ2000(orig, kameleon),glm::vec4(1.0f,1.0f,0.0f,1.0f)));
            tempLine.push_back(LinePoint(convertHnmToJ2000(orig, kameleon),glm::vec4(1.0f,1.0f,0.0f,1.0f)));
            tempLine.push_back(LinePoint(convertHnmToJ2000(earth, kameleon),glm::vec4(0.0f,1.0f,1.0f,1.0f)));
            tempLine.push_back(LinePoint(convertHnmToJ2000(earth, kameleon),glm::vec4(0.0f,1.0f,1.0f,1.0f)));

            tempLineX.push_back(LinePoint(convertHnmToJ2000(orig, kameleon),glm::vec4(1.0f,0.0f,0.0f,1.0f)));
            tempLineX.push_back(LinePoint(convertHnmToJ2000(orig, kameleon),glm::vec4(1.0f,0.0f,0.0f,1.0f)));
            tempLineX.push_back(LinePoint(convertHnmToJ2000(x, kameleon),glm::vec4(1.0f,0.0f,0.0f,1.0f)));
            tempLineX.push_back(LinePoint(convertHnmToJ2000(x, kameleon),glm::vec4(1.0f,0.0f,0.0f,1.0f)));

            tempLineY.push_back(LinePoint(convertHnmToJ2000(orig, kameleon),glm::vec4(0.0f,1.0f,0.0f,1.0f)));
            tempLineY.push_back(LinePoint(convertHnmToJ2000(orig, kameleon),glm::vec4(0.0f,1.0f,0.0f,1.0f)));
            tempLineY.push_back(LinePoint(convertHnmToJ2000(y, kameleon),glm::vec4(0.0f,1.0f,0.0f,1.0f)));
            tempLineY.push_back(LinePoint(convertHnmToJ2000(y, kameleon),glm::vec4(0.0f,1.0f,0.0f,1.0f)));

            tempLineZ.push_back(LinePoint(convertHnmToJ2000(orig, kameleon),glm::vec4(0.0f,0.0f,1.0f,1.0f)));
            tempLineZ.push_back(LinePoint(convertHnmToJ2000(orig, kameleon),glm::vec4(0.0f,0.0f,1.0f,1.0f)));
            tempLineZ.push_back(LinePoint(convertHnmToJ2000(z, kameleon),glm::vec4(0.0f,0.0f,1.0f,1.0f)));
            tempLineZ.push_back(LinePoint(convertHnmToJ2000(z, kameleon),glm::vec4(0.0f,0.0f,1.0f,1.0f)));
            fieldlines.push_back(tempLine);
            fieldlines.push_back(tempLineX);
            fieldlines.push_back(tempLineY);
            fieldlines.push_back(tempLineZ);
        }
        delete kameleon;

        return fieldlines;
    }

    delete kameleon;
    return {};
}


} // namespace openspace
