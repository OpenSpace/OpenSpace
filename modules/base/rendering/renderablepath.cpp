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

#include <modules/base/rendering/renderablepath.h>
#include <openspace/util/time.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/opengl/programobject.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <fstream>

/* TODO for this class:
*  In order to add geometry shader (for pretty-draw),
*  need to pack each consecutive point pair into a vec2
*  in order to draw quad between them.
*/

namespace {
    const std::string _loggerCat = "RenderableTrail";
    //constants
    const char* keyName = "Name";
    const char* keyBody = "Body";
    const char* keyObserver = "Observer";
    const char* keyFrame = "Frame";
    const char* keyPathModule = "ModulePath";
    const char* keyColor = "RGB";
    const char* keyTimeSteps = "TimeSteps";
    const char* keyPointSteps = "PointSteps";
    const char* keyDrawLine = "DrawLine";
    const char* keRenderDistanceInterval = "RenderDistanceInterval";
}

namespace openspace {

RenderablePath::RenderablePath(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lineWidth("lineWidth", "Line Width", 2.f, 1.f, 20.f)
    , _drawLine("drawline", "Draw Line", false)
    , _programObject(nullptr)
    , _successfullDictionaryFetch(true)
    , _vaoID(0)
    , _vBufferID(0)
    , _needsSweep(true)
    , _start(0.0)
    , _stop(0.0)
{
    _successfullDictionaryFetch &= dictionary.getValue(keyBody, _target);
    _successfullDictionaryFetch &= dictionary.getValue(keyObserver, _observer);
    _successfullDictionaryFetch &= dictionary.getValue(keyFrame, _frame);
    _successfullDictionaryFetch &= dictionary.getValue(keyTimeSteps, _increment);

    float fPointSteps; // Dictionary can not pick out ints...
    if (!dictionary.getValue(keyPointSteps, fPointSteps))
        fPointSteps = 4;
    _pointSteps = fPointSteps;

    glm::vec3 color(0.f);
    if (dictionary.hasKeyAndValue<glm::vec3>(keyColor))
        dictionary.getValue(keyColor, color);
    _lineColor = color;

    bool drawLine = false;
    if (dictionary.hasKeyAndValue<bool>(keyDrawLine))
        dictionary.getValue(keyDrawLine, drawLine);
    _drawLine = drawLine;
    addProperty(_drawLine);
    addProperty(_lineWidth);
    _distanceFade = 1.0;
}

bool RenderablePath::initialize() {
    if (!_successfullDictionaryFetch) {
        LERROR("The following keys need to be set in the Dictionary. Cannot initialize!");
        LERROR(keyBody << ": " << _target);
        LERROR(keyObserver << ": " << _observer);
        LERROR(keyFrame << ": " << _frame);
        return false;
    }

    bool completeSuccess = true;


    RenderEngine& renderEngine = OsEng.renderEngine();
    _programObject = renderEngine.buildRenderProgram("PathProgram",
        "${MODULE_BASE}/shaders/path_vs.glsl",
        "${MODULE_BASE}/shaders/path_fs.glsl");
    if (!_programObject)
        return false;

    bool intervalSet = hasTimeInterval();
    if (intervalSet) {
        completeSuccess &= getInterval(_start, _stop);
    }

    return completeSuccess;
}

bool RenderablePath::deinitialize() {
    glDeleteVertexArrays(1, &_vaoID);
    _vaoID = 0;

    glDeleteBuffers(1, &_vBufferID);
    _vBufferID = 0;

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }

    return true;
}

bool RenderablePath::isReady() const {
    return (_programObject != nullptr) && _successfullDictionaryFetch;
}

void RenderablePath::render(const RenderData& data) {
    double time = openspace::Time::ref().j2000Seconds();
    if (_start > time || _stop < time)
        return;


    int nPointsToDraw = _vertexArray.size();// (time - _start) / (_stop - _start) * (_vertexArray.size()) + 1 + 0.5;

    _programObject->activate();

    // Calculate variables to be used as uniform variables in shader
    glm::dvec3 bodyPosition = data.modelTransform.translation;

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform =
        glm::translate(glm::dmat4(1.0), bodyPosition); // Translation
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransform;
    
    _programObject->setUniform("modelViewTransform", glm::mat4(modelViewTransform));
    _programObject->setUniform("projectionTransform", data.camera.projectionMatrix());
    _programObject->setUniform("pointSteps", _pointSteps);
    _programObject->setUniform("color", _lineColor);

    if (_drawLine) {
        glLineWidth(_lineWidth);
        glBindVertexArray(_vaoID);
        glDrawArrays(GL_LINE_STRIP, 0, static_cast<GLsizei>(nPointsToDraw));
        glBindVertexArray(0);
        glLineWidth(1.f);
    }

    glEnable(GL_PROGRAM_POINT_SIZE);

    glBindVertexArray(_vaoID);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(nPointsToDraw));
    glBindVertexArray(0);

    glDisable(GL_PROGRAM_POINT_SIZE);

    _programObject->deactivate();
}

void RenderablePath::update(const UpdateData& data) {
    if (data.isTimeJump)
        _needsSweep = true;

    if (_needsSweep) {
        calculatePath(_observer);
        sendToGPU();
        _needsSweep = false;
    }

    if (_programObject->isDirty())
        _programObject->rebuildFromFile();
}

void RenderablePath::calculatePath(std::string observer) {
    double interval = (_stop - _start);
    int segments = static_cast<int>(interval /_increment);

    if (segments == 0)
        return;

    double lightTime;
//    bool correctPosition = true;

    double currentTime = _start;
    _vertexArray.resize(segments);

    //psc pscPos;
    //float r, g, b;
    //float g = _lineColor[1];
    //float b = _lineColor[2];
    glm::vec3 position;
    for (int i = 0; i < segments; i++) {
        position =
        glm::vec3(SpiceManager::ref().targetPosition(_target, observer, _frame, {}, currentTime, lightTime));
        
        // Convert from 100 m to m
        position *= 10e2;
        
        //pscPos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
        //pscPos[3] += 3;
            
        //if (!correctPosition) {
        //    r = 1.f;
        //    g = b = 0.5f;
        //}
        //else if ((i % 8) == 0) {
        //    r = _lineColor[0];
        //    g = _lineColor[1];
        //    b = _lineColor[2];
        //}
        //else {
        //    r = g = b = 0.6f;
        //}
        //add position
        _vertexArray[i] = { position[0], position[1], position[2], 1.0f };
        //add color for position
        //_vertexArray[i + 1] = { r, g, b, a };
        currentTime += _increment;
    }
    _lastPosition = glm::vec4(position, 1.0f);

    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glBufferSubData(GL_ARRAY_BUFFER, 0, _vertexArray.size() *  sizeof(VertexInfo), &_vertexArray[0]);
}

void RenderablePath::sendToGPU() {
    glGenVertexArrays(1, &_vaoID);
    glGenBuffers(1, &_vBufferID);

    glBindVertexArray(_vaoID);
    
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferID);
    glBufferData(GL_ARRAY_BUFFER, _vertexArray.size() * sizeof(VertexInfo), NULL, GL_STREAM_DRAW); // orphaning the buffer, sending NULL data.
    glBufferSubData(GL_ARRAY_BUFFER, 0, _vertexArray.size() * sizeof(VertexInfo), &_vertexArray[0]);

    
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 0, 0);
    //glEnableVertexAttribArray(1);
    //glVertexAttribPointer(1, 4, GL_FLOAT, GL_FALSE, sizeof(VertexInfo) * 2, (void*)(sizeof(VertexInfo)));

    glBindVertexArray(0);
}

} // namespace openspace
