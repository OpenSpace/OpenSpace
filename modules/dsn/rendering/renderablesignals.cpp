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

#include <modules/dsn/rendering/renderablesignals.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/translation.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <openspace/util/spicemanager.h>
#include <openspace/interaction/navigationhandler.h>


namespace {
    constexpr const char* ProgramName = "SignalsProgram";
    constexpr const char* KeyTranslation = "Translation";
    constexpr const char* _loggerCat = "RenderableSignals";

    constexpr const std::array<const char*, 3> UniformNames = {
        "modelViewStation","modelViewSpacecraft", "projectionTransform"
    };

    constexpr openspace::properties::Property::PropertyInfo MadridColorInfo = {
        "MadridColor",
        "MadridColor",
        "This value determines the RGB main color for the lines "
        "of communication to and from Madrid."
    };

    constexpr openspace::properties::Property::PropertyInfo GoldstoneColorInfo = {
        "GoldstoneColor",
        "GoldstoneColor",
        "This value determines the RGB main color for the lines "
        "of communication to and from Goldstone."
    };
    constexpr openspace::properties::Property::PropertyInfo CanberraColorInfo = {
        "CanberraColor",
        "CanberraColor",
        "This value determines the RGB main color for the lines "
        "of communication to and from Canberra."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the communication package. "
    };

} // namespace

namespace openspace {

documentation::Documentation RenderableSignals::Documentation() {
    using namespace documentation;
    return {
        "Renderable Signals",
        "dsn_renderable_renderablesignals",
        {
            {
                KeyTranslation,
                new ReferencingVerifier("core_transform_translation"),
                Optional::No,
                "This object is used to compute locations along the path. Any "
                "Translation object can be used here."
            },
            {
                MadridColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                MadridColorInfo.description
            },
            {
                GoldstoneColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                GoldstoneColorInfo.description
            },
            {
                CanberraColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                GoldstoneColorInfo.description
            },
            {
                LineWidthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LineWidthInfo.description
            }
        }
    };
}

RenderableSignals::RenderableSignals(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _madridLineColor(MadridColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _goldstoneLineColor(GoldstoneColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _canberraLineColor(CanberraColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 20.f)
{
    _translation = Translation::createFromDictionary(
        dictionary.value<ghoul::Dictionary>(KeyTranslation)
    );
    addPropertySubOwner(_translation.get());

    if (dictionary.hasKeyAndValue<glm::vec3>(MadridColorInfo.identifier)) {
        _madridLineColor = dictionary.value<glm::vec3>(MadridColorInfo.identifier);
    }
    else {
        _madridLineColor = glm::vec3(1.f, 0.f, 0.f);
    }
    addProperty(_madridLineColor);

    if (dictionary.hasKeyAndValue<glm::vec3>(CanberraColorInfo.identifier)) {
        _canberraLineColor = dictionary.value<glm::vec3>(CanberraColorInfo.identifier);
    }
    else {
        _canberraLineColor = glm::vec3(0.f, 1.f, 0.f);
    }
    addProperty(_canberraLineColor);

    if (dictionary.hasKeyAndValue<glm::vec3>(GoldstoneColorInfo.identifier)) {
        _goldstoneLineColor = dictionary.value<glm::vec3>(GoldstoneColorInfo.identifier);
    }
    else {
        _goldstoneLineColor = glm::vec3(0.f, 0.f, 1.f);
    }
    addProperty(_goldstoneLineColor);

    if (dictionary.hasKeyAndValue<double>(LineWidthInfo.identifier)) {
        _lineWidth = static_cast<float>(dictionary.value<double>(
            LineWidthInfo.identifier
        ));
    }
    addProperty(_lineWidth);

    std::unique_ptr<ghoul::Dictionary> dictionaryPtr = std::make_unique<ghoul::Dictionary>(dictionary);
    extractData(dictionaryPtr);
}

void RenderableSignals::initializeGL() {
    _programObject = BaseModule::ProgramObjectManager.request(
        ProgramName,
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine.buildRenderProgram(
                ProgramName,
                absPath("${MODULE_DSN}/shaders/renderablesignals_vs.glsl"),
                absPath("${MODULE_DSN}/shaders/renderablesignals_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_programObject, _uniformCache, UniformNames);

    setRenderBin(Renderable::RenderBin::Overlay);

    // We don't need an index buffer, so we keep it at the default value of 0
    glGenVertexArrays(1, &_lineRenderInformation._vaoID);
    glGenBuffers(1, &_lineRenderInformation._vBufferID);
}

void RenderableSignals::deinitializeGL() {

    glDeleteVertexArrays(1, &_lineRenderInformation._vaoID);
    glDeleteBuffers(1, &_lineRenderInformation._vBufferID);

    BaseModule::ProgramObjectManager.release(
        ProgramName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine.removeRenderProgram(p);
        }
    );
    _programObject = nullptr;
}

bool RenderableSignals::isReady() const {
    return _programObject != nullptr;
}

// Unbind buffers and arrays
inline void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableSignals::render(const RenderData& data, RendererTasks&) {
    _programObject->activate();

    //The stations are statically translated with respect to Earth
    glm::dmat4 modelTransformStation = global::renderEngine.scene()->sceneGraphNode("Earth")->modelTransform();

    _programObject->setUniform(_uniformCache.modelViewStation,
        data.camera.combinedViewMatrix() * modelTransformStation);

    _programObject->setUniform(_uniformCache.modelViewSpacecraft,
        data.camera.combinedViewMatrix()  * _lineRenderInformation._localTransformSpacecraft);

    _programObject->setUniform(_uniformCache.projection, data.camera.projectionMatrix());

    const bool usingFramebufferRenderer =
        global::renderEngine.rendererImplementation() ==
        RenderEngine::RendererImplementation::Framebuffer;

    if (usingFramebufferRenderer) {
        glDepthMask(false);
        //glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }
    glLineWidth(_lineWidth);

    glBindVertexArray(_lineRenderInformation._vaoID);

    glDrawArrays(
        GL_LINES,
        _lineRenderInformation.first,
        _lineRenderInformation.count
    );

    //unbind vertex array and buffers
    unbindGL();

    if (usingFramebufferRenderer) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }

    _programObject->deactivate();
}



void RenderableSignals::update(const UpdateData& data) {

    double currentTime = data.time.j2000Seconds();
    //Todo: change this magic number. 86400 equals 24hrs in seconds
    double endTime = 86400;

    //Bool if the current time is within the timeframe for the currently loaded data
    const bool isTimeInFileInterval = (currentTime >= SignalManager::_dsnData.sequenceStartTime) &&
        (currentTime < SignalManager::_dsnData.sequenceStartTime + endTime);

    //Reload data if it is not relevant anymore
    if (!isTimeInFileInterval) {
        SignalManager::_dsnData.isLoaded = false;

        int activeFileIndex = findFileIndexForCurrentTime(currentTime, SignalManager::_fileStartTimes);
        //parse data for that file
        if (!SignalManager::_dsnData.isLoaded)
        {
            SignalManager::jsonParser(activeFileIndex);

        }
        else
            return;
    }

    // Make space for the vertex renderinformation
    _vertexArray.clear();

    //update focusnode information used to calculate spacecraft positions
    _focusNode = global::navigationHandler.focusNode();
    _lineRenderInformation._localTransformSpacecraft = glm::translate(glm::dmat4(1.0), _focusNode->worldPosition());

    //Todo; keep track of active index for signalvector, or swap for loop for binary search
    for (int i = 0; i < SignalManager::_dsnData.signals.size(); i++) {

        SignalManager::Signal currentSignal = SignalManager::_dsnData.signals[i];
        if (isSignalActive(currentTime, currentSignal.startTime, currentSignal.endTime))
            pushSignalDataToVertexArray(currentSignal);
    };


    // ... and upload them to the GPU
    glBindVertexArray(_lineRenderInformation._vaoID);
    glBindBuffer(GL_ARRAY_BUFFER, _lineRenderInformation._vBufferID);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexArray.size() * sizeof(float),
        _vertexArray.data(),
        GL_STATIC_DRAW
    );

    // position attributes
    glVertexAttribPointer(_locVer, _sizePosVal, GL_FLOAT, GL_FALSE, sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout), (void*)0);
    glEnableVertexAttribArray(_locVer);
    // color attributes
    glVertexAttribPointer(_locCol, _sizeColorVal, GL_FLOAT, GL_FALSE, sizeof(ColorVBOLayout) + sizeof(PositionVBOLayout), (void*)(sizeof(PositionVBOLayout)));
    glEnableVertexAttribArray(_locCol);

    // Directly render the entire step
    _lineRenderInformation.first = 0;
    _lineRenderInformation.count = static_cast<GLsizei>(_vertexArray.size() / (_sizePosVal + _sizeColorVal));

    //unbind vertexArray
    unbindGL();
}


int RenderableSignals::findFileIndexForCurrentTime(double time, std::vector<double> vec) {
    // upper_bound has O(log n) for sorted vectors, more efficient than for loop
    auto iter = std::upper_bound(vec.begin(), vec.end(), time);

    int fileIndex = -1;
    //check what index we got 
    if (iter != vec.end()) {
        if (iter != vec.begin()) {
            fileIndex = static_cast<int>(
                std::distance(vec.begin(), iter)
                ) - 1;
        }
        else {
            fileIndex = 0;
        }
    }
    else {
        fileIndex = static_cast<int>(vec.size()) - 1;
    }

    return fileIndex;
}

bool RenderableSignals::isSignalActive(double currentTime, std::string signalStartTime, std::string signalEndTime) {
    double startTimeInSeconds = SpiceManager::ref().ephemerisTimeFromDate(signalStartTime);
    double endTimeInSeconds = SpiceManager::ref().ephemerisTimeFromDate(signalEndTime);

    if (startTimeInSeconds <= currentTime && endTimeInSeconds >= currentTime)
        return true;

    return false;
}

void RenderableSignals::extractData(std::unique_ptr<ghoul::Dictionary> &dictionary) {

    if (!SignalManager::extractMandatoryInfoFromDictionary(_identifier, dictionary)) {
        LERROR(fmt::format("{}: Did not manage to extract data.", _identifier));
    }
    else {
        LDEBUG(fmt::format("{}: Successfully read data.", _identifier));
    }
}


void RenderableSignals::pushSignalDataToVertexArray(SignalManager::Signal signal) {

    ColorVBOLayout color = getSiteColor(signal.dishName);
    glm::vec3 posStation = getPositionForGeocentricSceneGraphNode(signal.dishName.c_str());
    glm::vec3 posSpacecraft = getSuitablePrecisionPositionForSceneGraphNode(signal.spacecraft.c_str());

    //fill the render array
    _vertexArray.push_back(posStation.x);
    _vertexArray.push_back(posStation.y);
    _vertexArray.push_back(posStation.z);

    _vertexArray.push_back(color.r);
    _vertexArray.push_back(color.g);
    _vertexArray.push_back(color.b);
    _vertexArray.push_back(color.a);

    _vertexArray.push_back(posSpacecraft.x);
    _vertexArray.push_back(posSpacecraft.y);
    _vertexArray.push_back(posSpacecraft.z);

    _vertexArray.push_back(color.r);
    _vertexArray.push_back(color.g);
    _vertexArray.push_back(color.b);
    _vertexArray.push_back(color.a);

}


/* Since our station dishes have a static translation from Earth, we
* can get their local translation. The reason to handle it differently
* compared to the spacecrafts is to keep an exact render position
* for the station line ends even when the focusNode is Earth. */
glm::dvec3 RenderableSignals::getCoordinatePosFromFocusNode(SceneGraphNode* node) {

    glm::dvec3 nodePos = node->worldPosition();
    glm::dvec3 focusNodePos = _focusNode->worldPosition();

    glm::dvec3 diff = glm::vec3(nodePos.x - focusNodePos.x, nodePos.y - focusNodePos.y,
        nodePos.z - focusNodePos.z);

    return diff;
}

glm::dvec3 RenderableSignals::getEstimatedCoordinatePosFromFocusNode(glm::vec3 pos) {

    glm::dvec3 earthPos = global::renderEngine.scene()->sceneGraphNode("Earth")->worldPosition();
    glm::dvec3 focusNodePos = _focusNode->worldPosition();

    glm::dmat4 translationMatrixEarth = glm::translate(glm::dmat4(1.0), glm::dvec3(earthPos));

    glm::dvec4 newPos = { pos, 1.0 };
    glm::dvec4 nodePos = _rotEquatorialSphere * translationMatrixEarth  *newPos;

    glm::dvec3 diff = glm::vec3(nodePos.x - focusNodePos.x, nodePos.y - focusNodePos.y,
        nodePos.z - focusNodePos.z);

    return diff;
}

glm::vec3 RenderableSignals::getSuitablePrecisionPositionForSceneGraphNode(std::string id) {

    glm::vec3 position;

    if (global::renderEngine.scene()->sceneGraphNode(id)) {
        SceneGraphNode* spacecraftNode = global::renderEngine.scene()->sceneGraphNode(id);
        position = getCoordinatePosFromFocusNode(spacecraftNode);
    }
    else {
        //If no scenegraphnode with proper id was found, estimate the position of the spacecraft by RA/DEC/RANGE 
        position = convertRaDecRangeToCartesian();
    }

    return position;
}


glm::vec3 RenderableSignals::getPositionForGeocentricSceneGraphNode(const char* id) {

    glm::dvec3 position;

    if (global::renderEngine.scene()->sceneGraphNode(id)) {
        position = global::renderEngine.scene()->sceneGraphNode(id)->position();
    }
    else {
        LERROR(fmt::format("No position data for the station dish {}, drawing line from center of Earth", id));
        position = glm::vec3(0, 0, 0);
    }

    return position;
}


glm::dvec3 RenderableSignals::convertRaDecRangeToCartesian() {
    //Todo: stream data from file
    //Dummy data for voyager 1
    double ra = 257.777029167736; //2018-246
    double dec = 12.2537708651048; // 2018-246
    double range = 2.14044781771236e+13;

    // Convert RA and DEC from degrees to radians 
    ra = glm::radians(ra);
    dec = glm::radians(dec);

    //Save array in vector 
    glm::dvec3 raDecPos = SpiceManager::getPositionFromRaDecRange(ra,dec,range);

    //Get the RA / DEC values in world coordinates with respect to the current focus node
    raDecPos = getEstimatedCoordinatePosFromFocusNode(raDecPos);

    return raDecPos;
}


RenderableSignals::ColorVBOLayout RenderableSignals::getSiteColor(std::string dishidentifier) {
    
    glm::vec3 color(0.0f,0.0f,0.0f);
    RenderableSignals::ColorVBOLayout colorVbo;
    SiteEnum site;

    try {
        site = StationToSiteConversion.at(dishidentifier);
    }
    catch (const std::exception& e) {
        LERROR(fmt::format("Station {} has no site location.", dishidentifier));
    }

    switch (site) {
        case 0: 
            color = _goldstoneLineColor; 
            break;
        case 1: 
            color = _madridLineColor;
            break;
        case 2: 
            color = _canberraLineColor;
            break;
    }

    colorVbo.r = color.r;
    colorVbo.g = color.g;
    colorVbo.b = color.b;

    //have different alpha for the 70m antennas
    if (dishidentifier == "DSS14" || dishidentifier == "DSS63" || dishidentifier == "DSS43")
    {
        colorVbo.a = 1.0;
    }
    else {
        colorVbo.a = 0.6f;
    }

    return colorVbo;
}

} // namespace openspace
