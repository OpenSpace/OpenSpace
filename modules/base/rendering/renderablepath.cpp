/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
    const std::string keyName = "Name";
    const std::string keyBody = "Body";
    const std::string keyObserver = "Observer";
    const std::string keyFrame = "Frame";
    const std::string keyPathModule = "ModulePath";
    const std::string keyColor = "RGB";
    const std::string keyTimeSteps = "TimeSteps";
    const std::string keyDrawLine = "DrawLine";

}

namespace openspace {

RenderablePath::RenderablePath(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _lineFade("lineFade", "Line Fade", 0.75f, 0.f, 5.f)
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

    glm::vec3 color(0.f);
    if (dictionary.hasKeyAndValue<glm::vec3>(keyColor))
        dictionary.getValue(keyColor, color);
    _lineColor = color;

    bool drawLine = false;
    if (dictionary.hasKeyAndValue<bool>(keyDrawLine))
        dictionary.getValue(keyDrawLine, drawLine);
    _drawLine = drawLine;
    addProperty(_drawLine);
    addProperty(_lineFade);
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
        getInterval(_start, _stop);
        std::string start = SpiceManager::ref().dateFromEphemerisTime(_start);
        std::string stop = SpiceManager::ref().dateFromEphemerisTime(_stop);
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
    double time = openspace::Time::ref().currentTime();
    if (_start > time || _stop < time)
        return;

    //const psc& position = data.camera.position();
    //const psc& origin = openspace::OpenSpaceEngine::ref().interactionHandler()->focusNode()->worldPosition();
    //const PowerScaledScalar& pssl = (position - origin).length();
    //double properLength = static_cast<double>(pssl.lengthf());
    //const PowerScaledScalar corrected = PowerScaledScalar::CreatePSS(properLength);
    //float exp = corrected[1];
    //
    //if (exp > 11)
    //    return;
    
    _programObject->activate();
    psc currentPosition = data.position;
    glm::mat4 camrot = glm::mat4(data.camera.viewRotationMatrix());
    glm::mat4 transform = glm::mat4(1);

    // setup the data to the shader
    _programObject->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _programObject->setUniform("ModelTransform", transform);
    _programObject->setUniform("color", _lineColor);
    _programObject->setUniform("lastPosition", _lastPosition);
    setPscUniforms(*_programObject.get(), data.camera, data.position);

    if (_drawLine) {
        glLineWidth(_lineWidth);
        glBindVertexArray(_vaoID);
        glDrawArrays(GL_LINE_STRIP, 0, static_cast<GLsizei>(_vertexArray.size()));
        glBindVertexArray(0);
        glLineWidth(1.f);
    }
    
    //float pointSize = std::min((11-exp),5.f);
    //glPointSize(5);
    glEnable(GL_PROGRAM_POINT_SIZE);
    
    //GLfloat distanceParams[] = { 1.0f, 5.0f, 10.f }; //a, b, c
    //GLfloat fadeThreshold[] = { 0.1f };
    //
    //glPointParameterf(GL_POINT_SIZE_MAX, 5.0f);
    //glPointParameterf(GL_POINT_SIZE_MIN, 1.0f);
    //glPointParameterfv(GL_POINT_DISTANCE_ATTENUATION, distanceParams);
    ////=> size = clamp(size*sqrt(1/(a+b*d+c*d^2)));
    //glPointParameterfv(GL_POINT_FADE_THRESHOLD_SIZE, fadeThreshold);

    glBindVertexArray(_vaoID);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_vertexArray.size()));
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

    psc pscPos;
    //float r, g, b;
    //float g = _lineColor[1];
    //float b = _lineColor[2];
    for (int i = 0; i < segments; i++) {
        glm::dvec3 p =
        SpiceManager::ref().targetPosition(_target, observer, _frame, {}, currentTime, lightTime);
        pscPos = PowerScaledCoordinate::CreatePowerScaledCoordinate(p.x, p.y, p.z);
        pscPos[3] += 3;
            
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
        _vertexArray[i] = { pscPos[0], pscPos[1], pscPos[2], pscPos[3] };
        //add color for position
        //_vertexArray[i + 1] = { r, g, b, a };
        currentTime += _increment;
    }
    _lastPosition = pscPos.dvec4();

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
