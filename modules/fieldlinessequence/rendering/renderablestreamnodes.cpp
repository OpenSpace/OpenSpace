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
    constexpr const char* _loggerCat = "RenderableStreamNodes";

    //gl variables for shaders, probably needed some of them atleast
    constexpr const GLuint VaPosition = 0; // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaColor    = 1; // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaMasking  = 2; // MUST CORRESPOND TO THE SHADER PROGRAM


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

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    constexpr const char* ValueInputFileTypeCdf = "cdf";
    constexpr const char* ValueInputFileTypeJson = "json";
    constexpr const char* ValueInputFileTypeOsfls = "osfls";

    
    //properties::PropertyOwner _pStreamGroup;
    // Size of simulated flow particles
    constexpr openspace::properties::Property::PropertyInfo StreamColorInfo = {
        "color2",
        "Color",
        "Color of particles."
    };
    constexpr openspace::properties::Property::PropertyInfo StreamsenabledInfo = {
       "streamsEnabled",
       "Stream Direction",
       "Toggles the rendering of moving particles along the lines. Can, for example, "
       "illustrate magnetic flow."
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
 } //namespace

namespace openspace {
    using namespace properties;
    RenderableStreamNodes::RenderableStreamNodes(const ghoul::Dictionary& dictionary)

        : Renderable(dictionary)
        , _pStreamColor(StreamColorInfo,
            glm::vec4(0.96f, 0.88f, 0.8f, 0.5f),
            glm::vec4(0.f),
            glm::vec4(1.f))
        , _pStreamsEnabled(StreamsenabledInfo, true)
        , _pStreamGroup({"Streams"})

{
        _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
}

    void RenderableStreamNodes::initializeGL() {
        // EXTRACT MANDATORY INFORMATION FROM DICTIONARY
        auto vec = LoadJsonfile();
        SourceFileType sourceFileType = SourceFileType::Invalid;
        if (!extractMandatoryInfoFromDictionary(sourceFileType)) {
            return;
        }

        // EXTRACT OPTIONAL INFORMATION FROM DICTIONARY
        std::string outputFolderPath;
        extractOptionalInfoFromDictionary(outputFolderPath);

        // EXTRACT SOURCE FILE TYPE SPECIFIC INFOMRATION FROM DICTIONARY & GET STATES FROM
        // SOURCE
        if (!loadJsonStatesIntoRAM(outputFolderPath)) {
            return;
        }
        // dictionary is no longer needed as everything is extracted
        _dictionary.reset();


        // No need to store source paths in memory if they are already in RAM!
        if (!_loadingStatesDynamically) {
            _sourceFiles.clear();
        }

        setupProperties();

    // Setup shader program
    _shaderProgram = global::renderEngine.buildRenderProgram(
        "Streamnodes",
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/streamnodes_vs.glsl"),
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/streamnodes_fs.glsl")
    );
    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
    glGenBuffers(1, &_vertexMaskingBuffer);

    // Probably not needed, seems to be needed for additive blending
    setRenderBin(Renderable::RenderBin::Overlay);
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
    if(!_dictionary->getValue(KeyInputFileType, inputFileTypeString)){
        LERROR(fmt::format("{}: The field {} is missing", _identifier, KeyInputFileType));
    }
    else{
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

bool RenderableStreamNodes::extractJsonInfoFromDictionary(fls::Model& model){
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

    addPropertySubOwner(_pStreamGroup);
    _pStreamGroup.addProperty(_pStreamColor);
    _pStreamGroup.addProperty(_pStreamsEnabled);
}


void RenderableStreamNodes::deinitializeGL() {
    if (_shaderProgram) {
        global::renderEngine.removeRenderProgram(_shaderProgram.get());
        _shaderProgram = nullptr;
    }
}

bool RenderableStreamNodes::isReady() const {
    return _shaderProgram != nullptr;
}

// Extract J2000 time from file names
// Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.osfls'
void RenderableStreamNodes::extractTriggerTimesFromFileNames() {
    // number of  characters in filename (excluding '.osfls')
    constexpr const int FilenameSize = 23;
    // size(".osfls")
    constexpr const int ExtSize = 6;

    for (const std::string& filePath : _sourceFiles) {
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
        _startTimes.push_back(triggerTime);
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

        _shaderProgram->setUniform("lineColor", _pStreamColor);
        // Flow/Particles
        _shaderProgram->setUniform("usingParticles", _pStreamsEnabled);
    }
}

void RenderableStreamNodes::update(const UpdateData& data) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }
}



    std::vector<std::string> RenderableStreamNodes::LoadJsonfile() {
        /*if (path.empty()) {
            return std::vector<std::string>();
        }
        */
        std::string data;
        std::ifstream streamdata("data.json", std::ifstream::binary);
        json jsonobj;
        streamdata >> jsonobj;

        //printDebug(jsonobj["stream0"]);
        LDEBUG(jsonobj["stream0"]);
        log(ghoul::logging::LogLevel::Debug, _loggerCat, jsonobj["stream0"]);
        //for 
        //LWARNING(fmt::format("Testar json", data));

        //LWARNING(fmt::format("Testar json"));

        return std::vector<std::string>();
    }

bool RenderableStreamNodes::loadJsonStatesIntoRAM(const std::string& outputFolder) {
    return true;
}

} // namespace openspace
