/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
 //including our own h file
#include <modules/fieldlinessequence/rendering/renderablestreamnodes.h>

//includes from fieldlinessequence, might not need all of them
#include <modules/fieldlinessequence/fieldlinessequencemodule.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
//test debugging tools more then logmanager
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/visualstudiooutputlog.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>
#include <thread>
#include <openspace/json.h>

//this is a call to use the nlohmann json file
using json = nlohmann::json;

namespace {
    //log category
    constexpr const char* _loggerCat = "renderableStreamNodes";

    //gl variables for shaders, probably needed some of them atleast
    constexpr const GLuint VaPosition = 0; // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaColor = 1; // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaFiltering = 2; // MUST CORRESPOND TO THE SHADER PROGRAM


    // ----- KEYS POSSIBLE IN MODFILE. EXPECTED DATA TYPE OF VALUE IN [BRACKETS]  ----- //
    // ---------------------------- MANDATORY MODFILE KEYS ---------------------------- //
   // [STRING] "cdf", "json" or "osfls"
    constexpr const char* KeyInputFileType = "InputFileType";
    // [STRING] should be path to folder containing the input files
    constexpr const char* KeySourceFolder = "SourceFolder";

    // ---------------------- MANDATORY INPUT TYPE SPECIFIC KEYS ---------------------- //
    // [STRING] Currently supports: "batsrus", "enlil" & "pfss"
    constexpr const char* KeyJsonSimulationModel = "SimulationModel";

    // ----------------------- OPTIONAL INPUT TYPE SPECIFIC KEYS ---------------------- //
   // [STRING ARRAY]
    constexpr const char* KeyCdfExtraVariables = "ExtraVariables";
    // [STRING]
    constexpr const char* KeyCdfTracingVariable = "TracingVariable";
    // [STRING]
    constexpr const char* KeyJsonScalingFactor = "ScaleToMeters";
    // [BOOLEAN] If value False => Load in initializing step and store in RAM
    constexpr const char* KeyOslfsLoadAtRuntime = "LoadAtRuntime";

    //[INT] Line Width should have a range
    constexpr const char* KeyLineWidth = "LineWidth";

    //[INT] Threshold Radius should have a range
    constexpr const char* KeyThresholdRadius = "ThresholdRadius";

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    constexpr const char* ValueInputFileTypeCdf = "cdf";
    constexpr const char* ValueInputFileTypeJson = "json";
    constexpr const char* ValueInputFileTypeOsfls = "osfls";


    //properties::PropertyOwner _pStreamGroup;
    constexpr openspace::properties::Property::PropertyInfo ColorModeInfo = {
        "colorMode",
        "Color Mode",
        "Color lines uniformly or using color tables based on specific values on nodes,"
        "for examples flux values."
    };
    // Size of simulated flow particles
    constexpr openspace::properties::Property::PropertyInfo StreamColorInfo = {
        "color",
        "Color",
        "Color of particles."
    };
    constexpr openspace::properties::Property::PropertyInfo StreamsenabledInfo = {
       "streamsEnabled",
       "Stream Direction",
       "Toggles the rendering of moving particles along the lines. Can, for example, "
       "illustrate magnetic flow."
    };
    constexpr openspace::properties::Property::PropertyInfo NodeSizeInfo = {
       "nodeSize",
       "Size of nodes",
       "Change the size of the nodes"
    };
    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
       "lineWidth",
       "Line Width",
       "This value specifies the line width of the field lines if the "
       "selected rendering method includes lines."
    };
    constexpr openspace::properties::Property::PropertyInfo ThresholdRadiusInfo = {
       "thresholdRadius",
       "Threshold Radius",
       "This value specifies the threshold that will be changed with the radius."
    };
    constexpr openspace::properties::Property::PropertyInfo FilteringInfo = {
        "filtering",
        "FilteringLower",
        "Use filtering to show nodes within a given range."
    };
    constexpr openspace::properties::Property::PropertyInfo FilteringUpperInfo = {
        "filtering2",
        "FilteringUpper",
        "Use filtering to show nodes within a given range."
    };

    enum class SourceFileType : int {
        Json = 0,
        Cdf,
        Osfls,
        Invalid
    };

    float stringToFloat(const std::string input, const float backupValue = 0.f) {
        float tmp;
        try {
            tmp = std::stof(input);
        }
        catch (const std::invalid_argument& ia) {
            LWARNING(fmt::format(
                "Invalid argument: {}. '{}' is NOT a valid number", ia.what(), input
                ));
            return backupValue;
        }
        return tmp;
    }
    double stringToDouble(const std::string input, const float backupValue = 0.f) {
        double tmp;

        try {
            tmp = std::stod(input);
        }
        catch (const std::invalid_argument& ia) {
            LWARNING(fmt::format(
                "Invalid argument: {}. '{}' is NOT a valid number", ia.what(), input
                ));
            return backupValue;
        }
        return tmp;
    }
    //changed everything from dvec3 to vec3
    glm::vec3 sphericalToCartesianCoord(glm::vec3 position) {
        glm::vec3 cartesianPosition = glm::vec3();
        //LDEBUG("spherical R:" + std::to_string(position.x));

        //ρsinφcosθ 
        cartesianPosition.x = position.x * sin(position.z) * cos(position.y);
        //ρsinφsinθ
        cartesianPosition.y = position.x * sin(position.z) * sin(position.y);
        //ρcosφ
        cartesianPosition.z = position.x * cos(position.z);

        //LDEBUG("cartesian position x: " + std::to_string(cartesianPosition.x));
        //cartesian position x : 0.002175
        return cartesianPosition;
    }
} //namespace

namespace openspace {
    using namespace properties;
    RenderableStreamNodes::RenderableStreamNodes(const ghoul::Dictionary& dictionary)

        : Renderable(dictionary)
        , _pColorGroup({ "Color" })
        , _pColorMode(ColorModeInfo, OptionProperty::DisplayType::Radio)
        , _pStreamColor(StreamColorInfo,
            glm::vec4(0.96f, 0.88f, 0.8f, 0.5f),
            glm::vec4(0.f),
            glm::vec4(1.f))
        , _pStreamsEnabled(StreamsenabledInfo, true)
        , _pStreamGroup({ "Streams" })
        , _pNodeSize(NodeSizeInfo, 2.f, 1.f, 20.f)
        , _pLineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
        //, _pThresholdRadius(ThresholdRadiusInfo, -2.f, -5.f, 5.f)
        //, _pThresholdRadius(ThresholdRadiusInfo, 100000000000.f, -500000000000.f, 400000000000.f)
        , _pThresholdRadius(ThresholdRadiusInfo, 0.f, -10.f, 10.f)

        , _pFiltering(FilteringInfo, 100000.f, 10000000.f, 1000000000000.f)
        , _pFilteringUpper(FilteringUpperInfo, 600000000000.f, 1000000.f, 1000000000000.f)
       
        
    {
        _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
    }

    void RenderableStreamNodes::initializeGL() {
        // EXTRACT MANDATORY INFORMATION FROM DICTIONARY
        //std::string filepath = "C:/Users/chrad171//openspace/OpenSpace/sync/http/bastille_day_streamnodes/1/datawithoutprettyprint_newmethod.json";
       
        //LDEBUG("testar json");
        //log(ghoul::logging::LogLevel::Debug, _loggerCat, "testar json");
        SourceFileType sourceFileType = SourceFileType::Invalid;
        if (!extractMandatoryInfoFromDictionary(sourceFileType)) {
            return;
        }

        // EXTRACT OPTIONAL INFORMATION FROM DICTIONARY
        std::string outputFolderPath;
        //extractOptionalInfoFromDictionary(outputFolderPath);

        // EXTRACT SOURCE FILE TYPE SPECIFIC INFOMRATION FROM DICTIONARY & GET STATES FROM
        // SOURCE
        if (!loadJsonStatesIntoRAM(outputFolderPath)) {
            return;
        }
        // dictionary is no longer needed as everything is extracted
        _dictionary.reset();


        // No need to store source paths in memory if they are already in RAM!
        //if (!_loadingStatesDynamically) {
        //    _sourceFiles.clear();
        //}
        _nStates = 274;
        setupProperties();
       
        extractTriggerTimesFromFileNames();
        computeSequenceEndTime();
        LDEBUG("filepath i init: " + std::to_string(_sourceFiles.size()));
        if (!_loadingStatesDynamically) {
            LoadfilesintoRam();
        }
        std::string filepath = _sourceFiles[0];
       
        //std::string filepath = "C:/Users/emiho502/desktop/OpenSpace/sync/http/bastille_day_streamnodes/1/datawithoutprettyprint_newmethod.json";
        //std::vector<std::string> vec = LoadJsonfile(filepath);
        // Setup shader program
        computeSequenceEndTime();
        _shaderProgram = global::renderEngine.buildRenderProgram(
            "Streamnodes",
            absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/streamnodes_vs.glsl"),
            absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/streamnodes_fs.glsl")
            );

        _uniformCache.streamColor = _shaderProgram->uniformLocation("streamColor");
        _uniformCache.usingParticles = _shaderProgram->uniformLocation("usingParticles");
        _uniformCache.nodeSize = _shaderProgram->uniformLocation("nodeSize");
        _uniformCache.thresholdRadius = _shaderProgram->uniformLocation("thresholdRadius");

        glGenVertexArrays(1, &_vertexArrayObject);
        glGenBuffers(1, &_vertexPositionBuffer);
        glGenBuffers(1, &_vertexColorBuffer);
        glGenBuffers(1, &_vertexFilteringBuffer);

        // Probably not needed, seems to be needed for additive blending
        //setRenderBin(Renderable::RenderBin::Overlay);
    }

    bool RenderableStreamNodes::LoadfilesintoRam() {
        size_t filesnumbers = 270;
        for (size_t j = 0; j < _nStates; ++j) {
            
            std::ifstream streamdata(_sourceFiles[j]);
            if (!streamdata.is_open())
            {
                LDEBUG("did not read the data.json file");
                return false;
            }
            json jsonobj = json::parse(streamdata);
           
            const char* sNode = "node0";
            const char* sStream = "stream0";
            const char* sData = "data";

            const json& jTmp = *(jsonobj.begin()); // First node in the file
            const char* sTime = "time";
            //double testtime = jTmp[sTime];
            std::string testtime = jsonobj["time"];
          
            size_t lineStartIdx = 0;
            //Loop through all the nodes
            const int numberofStreams = 383;
            constexpr const float AuToMeter = 149597870700.f;  // Astronomical Units
          
            _vertexPositions.clear();
            _lineCount.clear();
            _lineStart.clear();
            _vertexRadius.clear();
            _vertexColor.clear();
            int counter = 0;

            const size_t nPoints = 1;
            for (int i = 0; i < numberofStreams; ++i) {
                //i += 20;
               /* if (i > 37 && i < 154) {
                    i = 154;
                }
                if (i > 154 && i < 210) {
                    i = 210;
                }
                if (i > 210) {
                    break;
                }
                */

                for (json::iterator lineIter = jsonobj["stream" + std::to_string(i)].begin();
                    lineIter != jsonobj["stream" + std::to_string(i)].end(); ++lineIter) {
                 
                    std::string r = (*lineIter)["R"].get<std::string>();
                    std::string phi = (*lineIter)["Phi"].get<std::string>();
                    std::string theta = (*lineIter)["Theta"].get<std::string>();
                    std::string flux = (*lineIter)["Flux"].get<std::string>();

                  
                    //--------FLOAT
                    float rValue = stringToFloat(r);
                    float phiValue = stringToFloat(phi);
                    float thetaValue = stringToFloat(theta);
                    float fluxValue = stringToFloat(flux);
                    float ninetyDeToRad = 1.57079633f * 2;
                    const float pi = 3.14159265359f;
                  
                    float rTimesFluxValue = rValue * rValue * fluxValue;
                    rValue = rValue * AuToMeter;

                    //if(thetaValue > 1.4 && thetaValue < 1.6){
                    //if(rTimesFluxValue > 0)
                    glm::vec3 sphericalcoordinates =
                        glm::vec3(rValue, phiValue, thetaValue);




                    glm::vec3 position = sphericalToCartesianCoord(sphericalcoordinates);
                    //KOLLA OM DEN KONVERTATION FROM DEGREE
                    //Look in to convertion
                    //Roterar åt fel håll counter clockwise

                    //position.x = position.x * AuToMeter;
                    //position.y = position.y * AuToMeter;
                    //position.z = position.z * AuToMeter;
                    _vertexPositions.push_back(
                        position);
                    ++counter;
              
                    _lineCount.push_back(static_cast<GLsizei>(nPoints));
                    _lineStart.push_back(static_cast<GLsizei>(lineStartIdx));
                    lineStartIdx += nPoints;

                    _vertexColor.push_back(rTimesFluxValue);
                    _vertexRadius.push_back(rValue);

                    int skipcounter = 0;
                    int nodeskipn = 10;
                    
                    while (skipcounter < nodeskipn && lineIter != jsonobj["stream" + std::to_string(i)].end() - 1) {
                        ++lineIter;
                        ++skipcounter;
                    }
                   // }
                }
            }

            LDEBUG("vertPos size:" + std::to_string(_vertexPositions.size()));
            LDEBUG("counter for how many times we push back" + std::to_string(counter));

            _statesPos.push_back(_vertexPositions);
            _statesColor.push_back(_vertexColor);
            _statesRadius.push_back(_vertexRadius);
            LDEBUG("_states size: " + std::to_string(_statesPos.size()));
        }

        return true;
    }
    /**
     * Extracts the general information (from the lua modfile) that is mandatory for the class
     * to function; such as the file type and the location of the source files.
     * Returns false if it fails to extract mandatory information!
     */
    bool RenderableStreamNodes::extractMandatoryInfoFromDictionary(
        SourceFileType& sourceFileType)
    {
        _dictionary->getValue(SceneGraphNode::KeyIdentifier, _identifier);

        // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
        std::string inputFileTypeString;
        if (!_dictionary->getValue(KeyInputFileType, inputFileTypeString)) {
            LERROR(fmt::format("{}: The field {} is missing", _identifier, KeyInputFileType));
        }
        else {
            // Verify that the input type is corrects
            if (inputFileTypeString == ValueInputFileTypeJson) {
                sourceFileType = SourceFileType::Json;
            }
            else {
                LERROR(fmt::format(
                    "{}: {} is not a recognized {}",
                    _identifier, inputFileTypeString, KeyInputFileType
                    ));
                sourceFileType = SourceFileType::Invalid;
                return false;
            }
        }

        std::string sourceFolderPath;
        if (!_dictionary->getValue(KeySourceFolder, sourceFolderPath)) {
            LERROR(fmt::format("{}: The field {} is missing", _identifier, KeySourceFolder));
            return false;
        }

        // Ensure that the source folder exists and then extract
        // the files with the same extension as <inputFileTypeString>
        ghoul::filesystem::Directory sourceFolder(sourceFolderPath);
        if (FileSys.directoryExists(sourceFolder)) {
            // Extract all file paths from the provided folder
            _sourceFiles = sourceFolder.readFiles(
                ghoul::filesystem::Directory::Recursive::No,
                ghoul::filesystem::Directory::Sort::Yes
                );

            // Ensure that there are available and valid source files left
            if (_sourceFiles.empty()) {
                LERROR(fmt::format(
                    "{}: {} contains no {} files",
                    _identifier, sourceFolderPath, inputFileTypeString
                    ));
                return false;
            }
        }
        else {
            LERROR(fmt::format(
                "{}: FieldlinesSequence {} is not a valid directory",
                _identifier,
                sourceFolderPath
                ));
            return false;
        }
        return true;
    }

    //void RenderableStreamNodes::extractOptionalInfoFromDictionary(
      //  std::string& outputFolderPath)
    //{
    // ------------------- EXTRACT OPTIONAL VALUES FROM DICTIONARY ------------------- //
       // bool streamsEnabled;
        //if (_dictionary->getValue(KeyStreamsEnabled, streamsEnabledValue)) {
            //_pStreamsEnabled = streamsEnabledValue;
        //}
    //}

    bool RenderableStreamNodes::extractJsonInfoFromDictionary(fls::Model& model) {
        std::string modelStr;
        if (_dictionary->getValue(KeyJsonSimulationModel, modelStr)) {
            std::transform(
                modelStr.begin(),
                modelStr.end(),
                modelStr.begin(),
                [](char c) { return static_cast<char>(::tolower(c)); }
            );
            model = fls::stringToModel(modelStr);
        }
        else {
            LERROR(fmt::format(
                "{}: Must specify '{}'", _identifier, KeyJsonSimulationModel
                ));
            return false;
        }
        float lineWidthValue;
        if (_dictionary->getValue(KeyLineWidth, lineWidthValue)) {
            _pLineWidth = lineWidthValue;
        }
        float thresholdRadiusValue;
        if (_dictionary->getValue(KeyThresholdRadius, thresholdRadiusValue)) {
            _pThresholdRadius = thresholdRadiusValue;
        }
        float scaleFactor;
        if (_dictionary->getValue(KeyJsonScalingFactor, scaleFactor)) {
            _scalingFactor = scaleFactor;
        }
        else {
            LWARNING(fmt::format(
                "{}: Does not provide scalingFactor. Assumes coordinates are in meters",
                _identifier
                ));
        }
        return true;
    }

    void RenderableStreamNodes::setupProperties() {

        // -------------- Add non-grouped properties (enablers and buttons) -------------- //
        addProperty(_pStreamsEnabled);
        addProperty(_pLineWidth);
        addProperty(_pFiltering);
        addProperty(_pFilteringUpper);
        // ----------------------------- Add Property Groups ----------------------------- //
        addPropertySubOwner(_pStreamGroup);
        addPropertySubOwner(_pColorGroup);
        // ------------------------- Add Properties to the groups ------------------------ //
        _pColorGroup.addProperty(_pColorMode);
        _pColorGroup.addProperty(_pStreamColor);
        _pStreamGroup.addProperty(_pNodeSize);
        _pStreamGroup.addProperty(_pThresholdRadius);

        // --------------------- Add Options to OptionProperties --------------------- //
        _pColorMode.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");
        _pColorMode.addOption(static_cast<int>(ColorMethod::ByFluxValue), "By Flux Value");
    }

    void RenderableStreamNodes::deinitializeGL() {
        glDeleteVertexArrays(1, &_vertexArrayObject);
        _vertexArrayObject = 0;

        glDeleteBuffers(1, &_vertexPositionBuffer);
        _vertexPositionBuffer = 0;

        glDeleteBuffers(1, &_vertexColorBuffer);
        _vertexColorBuffer = 0;

        glDeleteBuffers(1, &_vertexFilteringBuffer);
        _vertexFilteringBuffer = 0;

        if (_shaderProgram) {
            global::renderEngine.removeRenderProgram(_shaderProgram.get());
            _shaderProgram = nullptr;
        }

        // Stall main thread until thread that's loading states is done!
        bool printedWarning = false;
        while (_isLoadingStateFromDisk) {
            if (!printedWarning) {
                LWARNING("Trying to destroy class when an active thread is still using it");
                printedWarning = true;
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(5));
        }
    }

    bool RenderableStreamNodes::isReady() const {
        return _shaderProgram != nullptr;
    }

    // Extract J2000 time from file names
    // Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.osfls'
    void RenderableStreamNodes::extractTriggerTimesFromFileNames() {
        // number of  characters in filename (excluding '.json')
        constexpr const int FilenameSize = 23;
        // size(".json")
        constexpr const int ExtSize = 5;

        for (const std::string& filePath : _sourceFiles) {
            LDEBUG("filepath " + filePath);
            const size_t strLength = filePath.size();
            // Extract the filename from the path (without extension)
            std::string timeString = filePath.substr(
                strLength - FilenameSize - ExtSize,
                FilenameSize - 1
                );
            // Ensure the separators are correct
            timeString.replace(4, 1, "-");
            timeString.replace(7, 1, "-");
            timeString.replace(13, 1, ":");
            timeString.replace(16, 1, ":");
            timeString.replace(19, 1, ".");
            const double triggerTime = Time::convertTime(timeString);
            LDEBUG("timestring " + timeString);
            _startTimes.push_back(triggerTime);
            
        }

    }
    void RenderableStreamNodes::updateActiveTriggerTimeIndex(double currentTime) {
        auto iter = std::upper_bound(_startTimes.begin(), _startTimes.end(), currentTime);
        if (iter != _startTimes.end()) {
            if (iter != _startTimes.begin()) {
                _activeTriggerTimeIndex = static_cast<int>(
                    std::distance(_startTimes.begin(), iter)
                    ) - 1;
            }
            else {
                _activeTriggerTimeIndex = 0;
            }
        }
        else {
            _activeTriggerTimeIndex = static_cast<int>(_nStates) - 1;
        }
    }
    void RenderableStreamNodes::render(const RenderData& data, RendererTasks&) {
        if (_activeTriggerTimeIndex != -1) {
            _shaderProgram->activate();

            // Calculate Model View MatrixProjection
            const glm::dmat4 rotMat = glm::dmat4(data.modelTransform.rotation);
            const glm::dmat4 modelMat =
                glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
                rotMat *
                glm::dmat4(glm::scale(glm::dmat4(1), glm::dvec3(data.modelTransform.scale)));
            const glm::dmat4 modelViewMat = data.camera.combinedViewMatrix() * modelMat;

            _shaderProgram->setUniform("modelViewProjection",
                data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewMat));

        // Flow/Particles
        _shaderProgram->setUniform(_uniformCache.streamColor, _pStreamColor);
        _shaderProgram->setUniform(_uniformCache.usingParticles, _pStreamsEnabled);
        _shaderProgram->setUniform(_uniformCache.nodeSize, 1);
        _shaderProgram->setUniform(_uniformCache.thresholdRadius, _pThresholdRadius);
        _shaderProgram->setUniform("colorMode", _pColorMode);
        _shaderProgram->setUniform("filterRadius", _pFiltering);
        _shaderProgram->setUniform("filterUpper", _pFilteringUpper);

        //if (_pColorMode == static_cast<int>(ColorMethod::ByFluxValue)) {
           //ghoul::opengl::TextureUnit textureUnit;
            //textureUnit.activate();
            //_transferFunction->bind(); // Calls update internally
            //_shaderProgram->setUniform("colorTable", textureUnit);
            //_shaderProgram->setUniform("colorTableRange",
            //    _colorTableRanges[_pColorQuantity]);
        //}

        const std::vector<glm::vec3>& vertPos = _vertexPositions;
        glBindVertexArray(_vertexArrayObject);
        glLineWidth(_pLineWidth);
        //glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
        
        /*glMultiDrawArrays(
            GL_LINE_STRIP, //_drawingOutputType,
            _lineStart.data(),
            _lineCount.data(),
            static_cast<GLsizei>(_lineStart.size())
        );*/

         glPointSize(_pNodeSize);
        
            GLint temp = 0;
            glDrawArrays(
                GL_PATCHES,
                temp,
                static_cast<GLsizei>(_lineCount.size())
            );



            glBindVertexArray(0);
            _shaderProgram->deactivate();

        }
    }
    inline void unbindGL() {
        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);
    }

    void RenderableStreamNodes::computeSequenceEndTime() {
        if (_nStates > 1) {
            const double lastTriggerTime = _startTimes[_nStates - 1];
            const double sequenceDuration = lastTriggerTime - _startTimes[0];
            const double averageStateDuration = sequenceDuration /
                (static_cast<double>(_nStates) - 1.0);
            _sequenceEndTime = lastTriggerTime + averageStateDuration;
            //_sequenceEndTime = lastTriggerTime;
        }
        else {
            // If there's just one state it should never disappear!
            _sequenceEndTime = DBL_MAX;
        }
    }
    void RenderableStreamNodes::update(const UpdateData& data) {
        if (_shaderProgram->isDirty()) {
            _shaderProgram->rebuildFromFile();
        }
        //Everything below is for updating depending on time.
        
        const double currentTime = data.time.j2000Seconds();
         const bool isInInterval = (currentTime >= _startTimes[0]) &&
             (currentTime < _sequenceEndTime);
        //const bool isInInterval = true;
        if (isInInterval) {
            const size_t nextIdx = _activeTriggerTimeIndex + 1;
            if (
                // true => Previous frame was not within the sequence interval
                //_activeTriggerTimeIndex < 0 ||
                // true => We stepped back to a time represented by another state
                currentTime < _startTimes[_activeTriggerTimeIndex] ||
                // true => We stepped forward to a time represented by another state
                (nextIdx < _nStates && currentTime >= _startTimes[nextIdx]))
            {
                updateActiveTriggerTimeIndex(currentTime);
                //LDEBUG("Vi borde uppdatera1");

                // _mustLoadNewStateFromDisk = true;


                _needsUpdate = true;
                _activeStateIndex = _activeTriggerTimeIndex;

            } // else {we're still in same state as previous frame (no changes needed)}
        }
            else {
                //not in interval => set everything to false
            //LDEBUG("not in interval");
                _activeTriggerTimeIndex = -1;
                _needsUpdate = false;
            }

        
            if (_needsUpdate) {
                //LDEBUG("needsupdate");
                if(_loadingStatesDynamically){
                if (!_isLoadingStateFromDisk) {
                    _isLoadingStateFromDisk = true;
                    LDEBUG("triggertime: " + std::to_string(_activeTriggerTimeIndex));
                std::string filePath = _sourceFiles[_activeTriggerTimeIndex];
               // auto vec = LoadJsonfile(filePath);
                std::thread readBinaryThread([this, f = std::move(filePath)]{
                     auto vec = LoadJsonfile(f);
                    });
                   readBinaryThread.detach();
                }
        
        _needsUpdate = false;
        _newStateIsReady = false;
        if(_vertexPositions.size() > 5800){
        updatePositionBuffer();
        updateVertexColorBuffer();
        updateVertexFilteringBuffer();
        }
            }
                //needs fix, right now it stops cuz it cant find the states.
                else if(!_statesPos[_activeTriggerTimeIndex].empty()){
                    _vertexPositions = _statesPos[_activeTriggerTimeIndex];
                    _vertexColor = _statesColor[_activeTriggerTimeIndex];
                    _vertexRadius = _statesRadius[_activeTriggerTimeIndex];
                    _needsUpdate = false;
                    _newStateIsReady = false;
                    updatePositionBuffer();
                    updateVertexColorBuffer();
                    updateVertexFilteringBuffer();
                }
            }
        
    }

    std::vector<std::string> RenderableStreamNodes::LoadJsonfile(std::string filepath) {
       
        //'YYYY-MM-DDTHH-MM-SS-XXX.osfls'
        //C:\Users\Chrad171\openspace\
        
        //std::ifstream streamdata("C:/Users/emiho502/desktop/OpenSpace/sync/http/bastille_day_streamnodes/1/datawithoutprettyprint_newmethod.json");
        //std::ifstream streamdata("C:/Users/chrad171//openspace/OpenSpace/sync/http/bastille_day_streamnodes/1/datawithoutprettyprint_newmethod.json");
        std::ifstream streamdata(filepath);
        //std::ifstream streamdata("C:/Users/chris/Documents/openspace/Openspace_ourbranch/OpenSpace/sync/http/bastille_day_streamnodes/2/datawithoutprettyprint_newmethod.json");
        if (!streamdata.is_open())
        {
            LDEBUG("did not read the data.json file");
        }
        json jsonobj = json::parse(streamdata);
        //json jsonobj;
        //streamdata >> jsonobj;
        
        
        
        //printDebug(jsonobj["stream0"]);
        //LDEBUG(jsonobj["stream0"]);
       // std::ofstream o("C:/Users/chris/Documents/openspace/Openspace_ourbranch/OpenSpace/sync/http/bastille_day_streamnodes/1/newdata2.json");
        //o << jsonobj << std::endl;
        LDEBUG("vi callade på loadJSONfile med  " + filepath);
        const char* sNode = "node0";
        const char* sStream = "stream0";
        const char* sData = "data";

        const json& jTmp = *(jsonobj.begin()); // First node in the file
        const char* sTime = "time";
        //double testtime = jTmp[sTime];
        std::string testtime = jsonobj["time"];
        //double testtime = Time::now();
        //const json::value_type& variableNameVec = jTmp[sStream][sNode][sData];
        //const size_t nVariables = variableNameVec.size();

        size_t lineStartIdx = 0;
        //Loop through all the nodes
        const int numberofStreams = 383;
        constexpr const float AuToMeter = 149597870700.f;  // Astronomical Units
        //constexpr const float ReToMeter = 6371000.f;       // Earth radius
        //constexpr const float RsToMeter = 695700000.f;     // Sun radius
        //const int coordToMeters = 1;
        //we have to have coordToMeters * our coord. 
         _vertexPositions.clear();
         _lineCount.clear();
         _lineStart.clear();
         _vertexRadius.clear();
         _vertexColor.clear();
        int counter = 0;
        
        const size_t nPoints = 1;
        for (int i = 0; i < numberofStreams; ++i) {
            //i += 20;
           /* if (i > 37 && i < 154) {
                i = 154;
            }
            if (i > 154 && i < 210) {
                i = 210;
            }
            if (i > 210) {
                break;
            }
            */
            
            for (json::iterator lineIter = jsonobj["stream" + std::to_string(i)].begin();
                lineIter != jsonobj["stream" + std::to_string(i)].end(); ++lineIter) {
              //  for (size_t k = 0; k < 1999; ++k) {
               //     json::iterator lineIter = jsonobj["stream" + std::to_string(i)][std::to_string(k)].begin();
                
                //lineIter += 20;
                //const size_t Nodesamount = 
                //LDEBUG("testar debuggen");
                //log(ghoul::logging::LogLevel::Debug, _loggerCat, lineIter.key());
                //LDEBUG("stream" + std::to_string(i));
                //LDEBUG("Phi value: " + (*lineIter)["Phi"].get<std::string>());
                //LDEBUG("Theta value: " + (*lineIter)["Theta"].get<std::string>());
                //LDEBUG("R value: " + (*lineIter)["R"].get<std::string>());
                //LDEBUG("Flux value: " + (*lineIter)["Flux"].get<std::string>());

                 //probably needs some work with types, not loading in strings. 
                std::string r = (*lineIter)["R"].get<std::string>();
                std::string phi = (*lineIter)["Phi"].get<std::string>();
                std::string theta = (*lineIter)["Theta"].get<std::string>();
                std::string flux = (*lineIter)["Flux"].get<std::string>();

                //LDEBUG("testar koordinater: " + r + "phi" + phi + "theta: " + theta);
                //LDEBUG("flux: " + r);
                //------DOUBLE 
                /*
                double rvalue = stringToDouble(r);
                double phivalue = stringToDouble(phi);
                double thetavalue = stringToDouble(theta);
                const double pi = 3.14159265359;
                phivalue = phivalue * (180 / pi);
                thetavalue = thetavalue * (180 / pi);
                rvalue = rvalue * AuToMeter;
                */
                //lineIter += 20;
                //--------FLOAT
                float rValue = stringToFloat(r);
                float phiValue = stringToFloat(phi);
                float thetaValue = stringToFloat(theta);
                float fluxValue = stringToFloat(flux);
                float ninetyDeToRad = 1.57079633f * 2;
                const float pi = 3.14159265359f;
                //phiValue = phiValue * (180.f / pi);
                //thetaValue = thetaValue + ninetyDeToRad; //(180.f / pi);
                //phiValue = phiValue + ninetyDeToRad;
                float rTimesFluxValue = rValue * rValue * fluxValue;
                rValue = rValue * AuToMeter;
                
                if(thetaValue < 1.6 && thetaValue > 1.4){
                //if(rTimesFluxValue > 0)
                glm::vec3 sphericalcoordinates =
                    glm::vec3(rValue, phiValue, thetaValue);

                


                //glm::dvec3 sphericalcoordinates =
                //    glm::dvec3(stringToDouble((*lineIter)["R"].get<std::string>()),
                  //      stringToDouble((*lineIter)["Phi"].get<std::string>()),
                   //     stringToDouble((*lineIter)["Theta"].get<std::string>()));

                //precision issue, right now rounding up at around 7th decimal. Probably 
                //around conversion with string to Double.
                //LDEBUG("R value after string to Float: " + std::to_string(stringToDouble
                //((*lineIter)["R"].get<std::string>())));
                //sphericalcoordinates.x = sphericalcoordinates.x * AuToMeter;
                glm::vec3 position = sphericalToCartesianCoord(sphericalcoordinates);
                //KOLLA OM DEN KONVERTATION FROM DEGREE
                //Look in to convertion
                //Roterar åt fel håll counter clockwise

                //position.x = position.x * AuToMeter;
                //position.y = position.y * AuToMeter;
                //position.z = position.z * AuToMeter;
                    _vertexPositions.push_back(
                        position);
                ++counter;
                //   coordToMeters * glm::vec3(
                  //     stringToFloat((*lineIter)["Phi"].get<std::string>(), 0.0f),
                   //    ,

                    //   )
                   //);

                _lineCount.push_back(static_cast<GLsizei>(nPoints));
                _lineStart.push_back(static_cast<GLsizei>(lineStartIdx));
                lineStartIdx += nPoints;

                _vertexColor.push_back(rTimesFluxValue);
                _vertexRadius.push_back(rValue);

                //skipping nodes
                int skipcounter = 0;
                int nodeskipn = 10;
                while (skipcounter < nodeskipn && lineIter != jsonobj["stream" + std::to_string(i)].end() - 1) {
                    ++lineIter;
                    ++skipcounter;
                }
                }
            }
        }

        LDEBUG("vertPos size:" + std::to_string(_vertexPositions.size()));
        LDEBUG("counter for how many times we push back" + std::to_string(counter));

        //log(ghoul::logging::LogLevel::Debug, _loggerCat, lineIter.value());

   // }

   // for (auto& el : jsonobj.items())
   // {
   //     LDEBUG(el.key());
   // }

        LDEBUG("Time:" + testtime);
        //openspace::printDebug("testar json"):
        //for 
        //LWARNING(fmt::format("Testar json", data));

        //LWARNING(fmt::format("Testar json"));
        _isLoadingStateFromDisk = false;
        //streamdata.close();
        return std::vector<std::string>();
    }

    void RenderableStreamNodes::updatePositionBuffer() {
        glBindVertexArray(_vertexArrayObject);
        glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

        const std::vector<glm::vec3>& vertPos = _vertexPositions;

        glBufferData(
            GL_ARRAY_BUFFER,
            vertPos.size() * sizeof(glm::vec3),
            vertPos.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(VaPosition);
        glVertexAttribPointer(VaPosition, 3, GL_FLOAT, GL_FALSE, 0, 0);

        unbindGL();
    }

    void RenderableStreamNodes::updateVertexColorBuffer() {
        glBindVertexArray(_vertexArrayObject);
        glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

        const std::vector<float>& vertColor = _vertexColor;

            glBufferData(
                GL_ARRAY_BUFFER,
                vertColor.size() * sizeof(float),
                vertColor.data(),
                GL_STATIC_DRAW
            );

            glEnableVertexAttribArray(VaColor);
            glVertexAttribPointer(VaColor, 1, GL_FLOAT, GL_FALSE, 0, 0);

            unbindGL();
    }

    void RenderableStreamNodes::updateVertexFilteringBuffer() {
            glBindVertexArray(_vertexArrayObject);
            glBindBuffer(GL_ARRAY_BUFFER, _vertexFilteringBuffer);

            const std::vector<float>& vertexRadius = _vertexRadius;

            glBufferData(
                GL_ARRAY_BUFFER,
                vertexRadius.size() * sizeof(float),
                vertexRadius.data(),
                GL_STATIC_DRAW
            );

            glEnableVertexAttribArray(VaFiltering);
            glVertexAttribPointer(VaFiltering, 1, GL_FLOAT, GL_FALSE, 0, 0);

            unbindGL();
    }

    const std::vector<GLsizei>& RenderableStreamNodes::lineCount() const {
        return _lineCount;
    }

    const std::vector<GLint>& RenderableStreamNodes::lineStart() const {
        return _lineStart;
    }
    bool RenderableStreamNodes::loadJsonStatesIntoRAM(const std::string& outputFolder) {
        return true;
    }

} // namespace openspace
