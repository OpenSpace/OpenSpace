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

#include <modules/base/rendering/renderabletrailnew.h>

#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/timerange.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/interaction/interactionhandler.h>

#include <ghoul/opengl/programobject.h>

#include <limits>
#include <stdint.h>

namespace {
    const std::string _loggerCat = "RenderableTrailNew";
    
    // Spice
    const std::string keyBody                   = "Body";
    const std::string keyFrame                  = "Frame";
    const std::string keyObserver               = "Observer";
    // Rendering properties
    const std::string keyLineColor              = "LineColor";
    const std::string keyPointColor             = "PointColor";
    const std::string keyLineFade               = "LineFade";
    const std::string keyLineWidth              = "LineWidth";
    const std::string keyRenderPart             = "RenderPart";
    const std::string keyShowTimeStamps         = "ShowTimeStamps";
    const std::string keyRenderFullTrail        = "RenderFullTrail";
    // Time interval
    const std::string keyTimeRange              = "TimeRange";
    const std::string keySampleDeltaTime        = "SampleDeltaTime";
    const std::string keySubSamples             = "SubSamples";
    // Static Constants
    static const glm::vec3 DEFAULT_COLOR        = glm::vec3(1.0f);
    static const float DEFAULT_LINE_FADE        = 0.5f;
    static const float DEFAULT_LINE_WIDTH       = 2.0f;
    static const int DEFAULT_POINT_STEPS        = 1;
    static const int DEFAULT_RENDER_PART        = 1;
    static const bool DEFAULT_SHOW_TIME_STAMPS  = false;
    static const bool DEFAULT_RENDER_FULL_TRAIL = false;
}

namespace openspace {

RenderableTrailNew::RenderableTrailNew(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    // Properties
    , _lineColor("lineColor", "Line Color", DEFAULT_COLOR, glm::vec3(0), glm::vec3(1))
    , _pointColor("pointColor", "Point Color", DEFAULT_COLOR, glm::vec3(0), glm::vec3(1))
    , _lineFade("lineFade", "Line Fade", DEFAULT_LINE_FADE, 0, 1)
    , _lineWidth("lineWidth", "Line Width", DEFAULT_LINE_WIDTH, 1, 10)
    , _renderPart("renderPart", "Render Part", DEFAULT_RENDER_PART, 0, DEFAULT_RENDER_PART)
    , _showTimeStamps("showTimeStamps", "Show TimeStamps", DEFAULT_SHOW_TIME_STAMPS)
    , _renderFullTrail("renderFullTrail", "Render Full Trail", DEFAULT_RENDER_FULL_TRAIL)
    // OpenGL
    , _vaoGlobalID(0)
    , _vBufferGlobalID(0)
    , _vaoLocalID(0)
    , _vBufferLocalID(0)
    // Other
    , _programObject(nullptr)
    , _successfullDictionaryFetch(true)
    , _currentTimeClamped(0)
    , _subSamples(0)
{
    ghoul::Dictionary timeRangeDict;

    // Values that needs to be set
    _successfullDictionaryFetch &= dictionary.getValue(keyBody, _body);
    _successfullDictionaryFetch &= dictionary.getValue(keyObserver, _observer);
    _successfullDictionaryFetch &= dictionary.getValue(keyFrame, _frame);
    _successfullDictionaryFetch &= dictionary.getValue(keySampleDeltaTime, _sampleDeltaTime);
    _successfullDictionaryFetch &= dictionary.getValue(keyTimeRange, timeRangeDict);
    _successfullDictionaryFetch &= TimeRange::initializeFromDictionary(
        timeRangeDict, _timeRange);

    // Validate
    _successfullDictionaryFetch &= _sampleDeltaTime > 0;

    // Initialize optional values
    glm::vec3 lineColor = glm::vec3(DEFAULT_COLOR);
    glm::vec3 pointColor = glm::vec3(DEFAULT_COLOR);
    float lineFade = DEFAULT_LINE_FADE;
    float lineWidth = DEFAULT_LINE_WIDTH;
    float pointSteps = DEFAULT_POINT_STEPS;
    double renderPart = DEFAULT_RENDER_PART;
    bool showTimeStamps = DEFAULT_SHOW_TIME_STAMPS;
    bool renderFullTrail = DEFAULT_RENDER_FULL_TRAIL;

    // Fetch from dictionary
    dictionary.getValue(keyLineColor, lineColor);
    dictionary.getValue(keyPointColor, pointColor);
    dictionary.getValue(keyLineFade, lineFade);
    dictionary.getValue(keyLineWidth, lineWidth);
    dictionary.getValue(keyRenderPart, renderPart);
    dictionary.getValue(keyShowTimeStamps, showTimeStamps);
    dictionary.getValue(keyRenderFullTrail, renderFullTrail);
    float fSubSamples; // ghoul can not read ints from dictionaries...
    if (dictionary.getValue(keySubSamples, fSubSamples))
        _subSamples = fSubSamples;

    // Set property values
    _lineColor = lineColor;
    _pointColor = pointColor;
    _lineFade = lineFade;
    _lineWidth = lineWidth;
    _renderPart = renderPart;
    _showTimeStamps = showTimeStamps;
    _renderFullTrail = renderFullTrail;

    // Add all properties and set view options
    addProperty(_lineColor);
    addProperty(_pointColor);
    addProperty(_lineFade);
    addProperty(_lineWidth);
    addProperty(_renderPart);
    addProperty(_showTimeStamps);
    addProperty(_renderFullTrail);

    _lineColor.setViewOption(properties::Property::ViewOptions::Color);
    _pointColor.setViewOption(properties::Property::ViewOptions::Color);
}

bool RenderableTrailNew::initialize() {
    if (!_successfullDictionaryFetch) {
        LERROR("The following keys need to be set in the Dictionary. Cannot initialize!");
        LERROR(keyBody      << ": " << _body);
        LERROR(keyObserver  << ": " << _observer);
        LERROR(keyFrame     << ": " << _frame);
        LERROR(keyTimeRange);
        return false;
    }
    if (_subSamples < 0)
        LERROR("SubSamples must not be less than 0: " << _subSamples);

    RenderEngine& renderEngine = OsEng.renderEngine();
    _programObject = renderEngine.buildRenderProgram("RenderableTrailNewProgram",
        "${MODULE_BASE}/shaders/renderabletrailnew_vs.glsl",
        "${MODULE_BASE}/shaders/renderabletrailnew_fs.glsl");

    if (!_programObject)
        return false;

    sweepTimeRange();
    initializeGlobalOpenGLPathData();
    initializeLocalOpenGLPathData();

    return true;
}

void RenderableTrailNew::initializeGlobalOpenGLPathData() {
    glGenVertexArrays(1, &_vaoGlobalID);
    glGenBuffers(1, &_vBufferGlobalID);

    glBindVertexArray(_vaoGlobalID);
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferGlobalID);
    // No need to update the trail several times, no need for stream draw.
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexPositionArray.size() * sizeof(glm::vec3),
        &_vertexPositionArray[0],
        GL_STATIC_DRAW);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
    glBindVertexArray(0);
}

void RenderableTrailNew::initializeLocalOpenGLPathData() {
    glGenVertexArrays(1, &_vaoLocalID);
    glGenBuffers(1, &_vBufferLocalID);

    glBindVertexArray(_vaoLocalID);
    
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferLocalID);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

    glBindVertexArray(0);
}

void RenderableTrailNew::deInitializeGlobalOpenGLPathData() {
    glDeleteVertexArrays(1, &_vaoGlobalID);
    glDeleteBuffers(1, &_vBufferGlobalID);
}

void RenderableTrailNew::deInitializeLocalOpenGLPathData() {
    glDeleteVertexArrays(1, &_vaoLocalID);
    glDeleteBuffers(1, &_vBufferLocalID);
}

bool RenderableTrailNew::deinitialize() {
    deInitializeGlobalOpenGLPathData();
    deInitializeLocalOpenGLPathData();

    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_programObject) {
        renderEngine.removeRenderProgram(_programObject);
        _programObject = nullptr;
    }
    
    return true;
}

void RenderableTrailNew::sweepTimeRange() {
    double lightTime = 0.0;
    double subDeltaTime = _sampleDeltaTime / (1 + _subSamples);
    glm::dvec3 bodyPosition;
    // Loop through all points from time range start to end
    for (double t = _timeRange.start; t < _timeRange.end; t += subDeltaTime) {
        try {
            bodyPosition = SpiceManager::ref().targetPosition(
                _body, _observer, _frame, {}, t, lightTime);
        }
        catch (const SpiceManager::SpiceException& e) {
            LERROR(e.what());
            break;
        }
        // Convert from km used by SPICE to meters used by OpenSpace
        bodyPosition *= 1000;
        _vertexPositionArray.push_back(glm::vec3(bodyPosition));
    }
    // Last point
    bodyPosition = SpiceManager::ref().targetPosition(
        _body, _observer, _frame, {}, _timeRange.end, lightTime);
    _vertexPositionArray.push_back(glm::vec3(bodyPosition));
}

bool RenderableTrailNew::isReady() const {
    return (_programObject != nullptr) && _successfullDictionaryFetch;
}

void RenderableTrailNew::render(const RenderData& data) {
    _programObject->activate();
    if (_renderFullTrail.value() == true) {
        preRender(_vertexPositionArray.size());
        preRenderSubPathGlobally(data);
        // Easy but not beautiful solution to render all vertices with max alpha
        _programObject->setUniform(
            "vertexIDPadding", static_cast<int>(_vertexPositionArray.size()));

        renderLines(_vaoGlobalID, _vertexPositionArray.size() - 1);
        if (_showTimeStamps) {
            renderPoints(_vaoGlobalID, _vertexPositionArray.size() - 1);
        }
    }
    else { // Only render the trail up to the point of the object body
        int nVerticesToDraw = glm::ceil(_vertexPositionArray.size() *
            (_currentTimeClamped - _timeRange.start) / (_timeRange.end - _timeRange.start));

        nVerticesToDraw = glm::min(
            nVerticesToDraw, static_cast<int>(_vertexPositionArray.size())) - 1;
        if (nVerticesToDraw > 1) {
            preRender(nVerticesToDraw);
            // Perform rendering of the bulk of the trail in single floating point precision
            preRenderSubPathGlobally(data);
            // The last vertex is drawn with higher precision after this
            // Hence we subtract one vertex from the ones to draw globally
            int nVerticesToDrawGlobally = nVerticesToDraw - 1;
            renderLines(_vaoGlobalID, nVerticesToDrawGlobally);
            if (_showTimeStamps) {
                renderPoints(_vaoGlobalID, nVerticesToDrawGlobally);
            }

            // The last line segment is rendered relative to body to achieve high precision
            preRenderSubPathLocally(data, nVerticesToDraw);
            renderLines(_vaoLocalID, 2);
            if (_showTimeStamps) {
                renderPoints(_vaoLocalID, 2);
            }
        }
        else if (_currentTimeClamped > _timeRange.start) {
            preRenderSubPathLocally(data, 2);
            renderLines(_vaoLocalID, 2);
        }
    }
    
    
    _programObject->deactivate();
}

void RenderableTrailNew::preRender(int totalNumVerticesToDraw) {
    // Upload uniforms that are the same for global and local rendering to the program
    _programObject->setUniform("lineFade", _lineFade.value());
    _programObject->setUniform("subSamples", _subSamples);
    _programObject->setUniform("maxNumVertices",
        static_cast<int>(_renderPart.value() * _vertexPositionArray.size()));
    _programObject->setUniform("numVertices", totalNumVerticesToDraw);
}

void RenderableTrailNew::preRenderSubPathGlobally(const RenderData& data) {
    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * _modelTransform;
    glm::mat4 modelViewProjectionTransform =
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform);

    // Upload uniforms that are specific to global rendering to the shader program
    _programObject->setUniform(
        "modelViewProjectionTransform", modelViewProjectionTransform);
    _programObject->setUniform("vertexIDPadding", 0);
}

void RenderableTrailNew::preRenderSubPathLocally(
    const RenderData& data, int totalNumVerticesToDraw) {
    glm::dvec3 v0; // Vertex that connects to the global part of the trail
    glm::dvec3 v1; // last vertex of the trail is in the position of the body

    v0 = _vertexPositionArray[totalNumVerticesToDraw - 2];
    v1 = _clampedBodyPosition;

    // Define positions relative to body (v1) which gives the high precision
    glm::vec3 vertexData[2] = {glm::vec3(v0 - v1), glm::vec3(0)};

    // Translation translates from the position of body so vertices should
    // be defined relative to body (hence the added v1 in translation part of model matrix)
    glm::dmat4 localTranslation = glm::translate(glm::dmat4(1.0), v1);
    glm::dmat4 modelTransformLocal = _modelTransform * localTranslation;
    glm::dmat4 modelViewTransform = data.camera.combinedViewMatrix() * modelTransformLocal;
    glm::mat4 modelViewProjectionTransform =
        data.camera.projectionMatrix() * glm::mat4(modelViewTransform);

    // Upload the new MVP matrix to the shader program
    _programObject->setUniform(
        "modelViewProjectionTransform", modelViewProjectionTransform);
    _programObject->setUniform("vertexIDPadding", totalNumVerticesToDraw - 2);

    // Update the attribute data on the GPU
    glBindBuffer(GL_ARRAY_BUFFER, _vBufferLocalID);
    glBufferData(
        GL_ARRAY_BUFFER,
        2 * sizeof(glm::vec3), // Only two vertices for this part of the trail
        vertexData, // Two vertices
        GL_DYNAMIC_DRAW); // This part of the path is rendered dynamically
    
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);
}

void RenderableTrailNew::renderLines(GLuint vao, int numberOfVertices) {
    glLineWidth(_lineWidth);

    _programObject->setUniform("color", _lineColor.value());

    glBindVertexArray(vao);
    glDrawArrays(GL_LINE_STRIP, 0, static_cast<GLsizei>(numberOfVertices));
    glBindVertexArray(0);

    glLineWidth(1.f);
}

void RenderableTrailNew::renderPoints(GLuint vao, int numberOfVertices) {
    glEnable(GL_PROGRAM_POINT_SIZE);

    _programObject->setUniform("color", _pointColor.value());
    _programObject->setUniform("pointSize", static_cast<float>(_lineWidth * 3));

    glBindVertexArray(vao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(numberOfVertices));
    glBindVertexArray(0);

    glDisable(GL_PROGRAM_POINT_SIZE);
}

void RenderableTrailNew::update(const UpdateData& data) {
    _currentTimeClamped = glm::clamp(data.time, _timeRange.start, _timeRange.end);
    _modelTransform =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    // Fetch the body position using SPICE
    double lightTime = 0.0;
    try {
        _clampedBodyPosition = SpiceManager::ref().targetPosition(
            _body, _observer, _frame, {}, _currentTimeClamped, lightTime);
    }
    catch (const SpiceManager::SpiceException& e) {
        try {
            _clampedBodyPosition = SpiceManager::ref().targetPosition(
                _body, _observer, _frame, {}, _currentTimeClamped, _timeRange.end);
        }
        catch (const SpiceManager::SpiceException& e) {
            return;
        }
    }
    // Convert from km used by SPICE to meters used by OpenSpace
    _clampedBodyPosition *= 1000;
}

} // namespace openspace
