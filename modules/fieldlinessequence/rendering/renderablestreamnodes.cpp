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
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>
#include <thread>
#include <openspace/json.h>

using json = nlohmann::json;
namespace {
    constexpr const char* _loggerCat = "RenderableStreamNodes";

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
       "Streamsenabled",
       "Stream Direction",
       "Toggles the rendering of moving particles along the lines. Can, for example, "
       "illustrate magnetic flow."
    };

   enum class SourceFileType : int {
       Cdf,
       Json = 0,
       Osfls,
       Invalid
   };
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
        "FieldlinesSequence",
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_vs.glsl"),
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_fs.glsl")
    );
   }

/**
 * Extracts the general information (from the lua modfile) that is mandatory for the class
 * to function; such as the file type and the location of the source files.
 * Returns false if it fails to extract mandatory information!
 */
bool RenderableStreamNodes::extractMandatoryInfoFromDictionary(
                                                          SourceFileType& SourceFileType){

    _dictionary->getValue(SceneGraphNode::KeyIdentifier, _identifier);

    // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
    std::string inputFileTypeString;
    if(!_dictionary->getValue(KeyInputFileType, inputFileTypeString)){
        LERROR(fmt::format("{}: The field {} is missing", _identifier, KeyInputFileType));
    }
    else{
     // Verify that the input type is corrects
        if (inputFileTypeString == ValueInputFileTypeJson) {
            //sourceFileType = SourceFileType::Json;
        }
        else {
            LERROR(fmt::format(
                "{}: {} is not a recognized {}",
                _identifier, inputFileTypeString, KeyInputFileType
            ));
            //sourceFileType = SourceFileType::Invalid;
            return false;
        }
    }
}

void RenderableStreamNodes::setupProperties() {

    // -------------- Add non-grouped properties (enablers and buttons) -------------- //


    addPropertySubOwner(_pStreamGroup);
    _pStreamGroup.addProperty(_pStreamColor);
    _pStreamGroup.addProperty(_pStreamsEnabled);

}
std::vector<std::string> RenderableStreamNodes::LoadJsonfile() { 
   /* if (path.empty()) {
        return std::vector<std::string>();
    }
    */
   /* std::string data;
    std::ifstream streamdata("data.json", std::ifstream::binary);
    streamdata >> data;
    LWARNING(fmt::format("Testar json", data));
    */
    LWARNING(fmt::format("Testar json"));

    return std::vector<std::string>();
   

}

bool RenderableStreamNodes::loadJsonStatesIntoRAM(const std::string& outputFolder) {
    return true;
}
bool RenderableStreamNodes::extractJsonInfoFromDictionary(fls::Model& model)
{
    return false;
}
} // namespace openspace
