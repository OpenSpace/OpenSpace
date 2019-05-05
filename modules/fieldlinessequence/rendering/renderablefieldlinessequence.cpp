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

#include <modules/fieldlinessequence/rendering/renderablefieldlinessequence.h>

#include <modules/fieldlinessequence/fieldlinessequencemodule.h>
#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
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

namespace {
    constexpr const char* _loggerCat = "RenderableFieldlinesSequence";

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
    // [STRING] Path to a .txt file containing seed points
    constexpr const char* KeyCdfSeedPointFile = "SeedPointFile";
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

    // ---------------------------- OPTIONAL MODFILE KEYS  ---------------------------- //
    // [STRING ARRAY] Values should be paths to .txt files
    constexpr const char* KeyColorTablePaths = "ColorTablePaths";
    // [VEC2 ARRAY] Values should be entered as {X, Y}, where X & Y are numbers
    constexpr const char* KeyColorTableRanges = "ColorTableRanges";
    // [VEC2 ARRAY] Values should be entered as {X, Y}, where X & Y are numbers
    constexpr const char* KeyMaskingRanges = "MaskingRanges";
    // [STRING] Value should be path to folder where states are saved (JSON/CDF input
    // => osfls output & oslfs input => JSON output)
    constexpr const char* KeyOutputFolder = "OutputFolder";

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    constexpr const char* ValueInputFileTypeCdf = "cdf";
    constexpr const char* ValueInputFileTypeJson = "json";
    constexpr const char* ValueInputFileTypeOsfls = "osfls";

    // --------------------------------- Property Info -------------------------------- //
    constexpr openspace::properties::Property::PropertyInfo ColorMethodInfo = {
        "colorMethod",
        "Color Method",
        "Color lines uniformly or using color tables based on extra quantities like, for "
        "examples, temperature or particle density."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorQuantityInfo = {
        "colorQuantity",
        "Quantity to Color By",
        "Quantity used to color lines if the 'By Quantity' color method is selected."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorQuantityMinInfo = {
        "colorQuantityMin",
        "ColorTable Min Value",
        "Value to map to the lowest end of the color table."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorQuantityMaxInfo = {
        "colorQuantityMax",
        "ColorTable Max Value",
        "Value to map to the highest end of the color table."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "colorTablePath",
        "Path to Color Table",
        "Color Table/Transfer Function to use for 'By Quantity' coloring."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorUniformInfo = {
        "uniform",
        "Uniform Line Color",
        "The uniform color of lines shown when 'Color Method' is set to 'Uniform'."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorUseABlendingInfo = {
        "aBlendingEnabled",
        "Additive Blending",
        "Activate/deactivate additive blending."
    };
    constexpr openspace::properties::Property::PropertyInfo DomainEnabledInfo = {
        "domainEnabled",
        "Domain Limits",
        "Enable/Disable domain limits"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainXInfo = {
        "limitsX",
        "X-limits",
        "Valid range along the X-axis. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainYInfo = {
        "limitsY",
        "Y-limits",
        "Valid range along the Y-axis. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainZInfo = {
        "limitsZ",
        "Z-limits",
        "Valid range along the Z-axis. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainRInfo = {
        "limitsR",
        "Radial limits",
        "Valid radial range. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "color",
        "Color",
        "Color of particles."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowEnabledInfo = {
        "flowEnabled",
        "Flow Direction",
        "Toggles the rendering of moving particles along the lines. Can, for example, "
        "illustrate magnetic flow."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowReversedInfo = {
        "reversed",
        "Reversed Flow",
        "Toggle to make the flow move in the opposite direction."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowParticleSizeInfo = {
        "particleSize",
        "Particle Size",
        "Size of the particles."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowParticleSpacingInfo = {
        "particleSpacing",
        "Particle Spacing",
        "Spacing inbetween particles."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowSpeedInfo = {
        "speed",
        "Speed",
        "Speed of the flow."
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingEnabledInfo = {
        "maskingEnabled",
        "Masking",
        "Enable/disable masking. Use masking to show lines where a given quantity is "
        "within a given range, for example, if you only want to see where the "
        "temperature is between 10 and 20 degrees. Also used for masking out line "
        "topologies like solar wind & closed lines."
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingMinInfo = {
        "maskingMinLimit",
        "Lower Limit",
        "Lower limit of the valid masking range"
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingMaxInfo = {
        "maskingMaxLimit",
        "Upper Limit",
        "Upper limit of the valid masking range"
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingQuantityInfo = {
        "maskingQuantity",
        "Quantity used for Masking",
        "Quantity used for masking."
    };
    constexpr openspace::properties::Property::PropertyInfo OriginButtonInfo = {
        "focusCameraOnParent",
        "Focus Camera",
        "Focus camera on parent."
    };
    constexpr openspace::properties::Property::PropertyInfo TimeJumpButtonInfo = {
        "timeJumpToStart",
        "Jump to Start Of Sequence",
        "Performs a time jump to the start of the sequence."
    };

    enum class SourceFileType : int {
        Cdf = 0,
        Json,
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
} // namespace

namespace openspace {
using namespace properties;

RenderableFieldlinesSequence::RenderableFieldlinesSequence(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _pColorGroup({ "Color" })
    , _pColorMethod(ColorMethodInfo, OptionProperty::DisplayType::Radio)
    , _pColorQuantity(ColorQuantityInfo, OptionProperty::DisplayType::Dropdown)
    , _pColorQuantityMin(ColorQuantityMinInfo)
    , _pColorQuantityMax(ColorQuantityMaxInfo)
    , _pColorTablePath(ColorTablePathInfo)
    , _pColorUniform(
        ColorUniformInfo,
        glm::vec4(0.75f, 0.5f, 0.0f, 0.5f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _pColorABlendEnabled(ColorUseABlendingInfo, true)
    , _pDomainEnabled(DomainEnabledInfo, true)
    , _pDomainGroup({ "Domain" })
    , _pDomainX(DomainXInfo)
    , _pDomainY(DomainYInfo)
    , _pDomainZ(DomainZInfo)
    , _pDomainR(DomainRInfo)
    , _pFlowColor(
        FlowColorInfo,
        glm::vec4(0.8f, 0.7f, 0.0f, 0.6f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _pFlowEnabled(FlowEnabledInfo, true)
    , _pFlowGroup({ "Flow" })
    , _pFlowParticleSize(FlowParticleSizeInfo, 5, 0, 500)
    , _pFlowParticleSpacing(FlowParticleSpacingInfo, 60, 0, 500)
    , _pFlowReversed(FlowReversedInfo, false)
    , _pFlowSpeed(FlowSpeedInfo, 20, 0, 1000)
    , _pMaskingEnabled(MaskingEnabledInfo, false)
    , _pMaskingGroup({ "Masking" })
    , _pMaskingMin(MaskingMinInfo)
    , _pMaskingMax(MaskingMaxInfo)
    , _pMaskingQuantity(MaskingQuantityInfo, OptionProperty::DisplayType::Dropdown)
    , _pFocusOnOriginBtn(OriginButtonInfo)
    , _pJumpToStartBtn(TimeJumpButtonInfo)
{
    _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
}

void RenderableFieldlinesSequence::initializeGL() {
    // EXTRACT MANDATORY INFORMATION FROM DICTIONARY
    SourceFileType sourceFileType = SourceFileType::Invalid;
    if (!extractMandatoryInfoFromDictionary(sourceFileType)) {
        return;
    }

    // Set a default color table, just in case the (optional) user defined paths are
    // corrupt or not provided!
    _colorTablePaths.push_back(FieldlinesSequenceModule::DefaultTransferFunctionFile);
    _transferFunction = std::make_unique<TransferFunction>(absPath(_colorTablePaths[0]));

    // EXTRACT OPTIONAL INFORMATION FROM DICTIONARY
    std::string outputFolderPath;
    extractOptionalInfoFromDictionary(outputFolderPath);

    // EXTRACT SOURCE FILE TYPE SPECIFIC INFOMRATION FROM DICTIONARY & GET STATES FROM
    // SOURCE
    switch (sourceFileType) {
        case SourceFileType::Cdf:
            if (!getStatesFromCdfFiles(outputFolderPath)) {
                return;
            }
            break;
        case SourceFileType::Json:
            if (!loadJsonStatesIntoRAM(outputFolderPath)) {
                return;
            }
            break;
        case SourceFileType::Osfls:
            extractOsflsInfoFromDictionary();
            if (_loadingStatesDynamically) {
                if (!prepareForOsflsStreaming()) {
                    return;
                }
            }
            else {
                loadOsflsStatesIntoRAM(outputFolderPath);
            }
            break;
        default:
            return;
    }

    // dictionary is no longer needed as everything is extracted
    _dictionary.reset();

    // No need to store source paths in memory if they are already in RAM!
    if (!_loadingStatesDynamically) {
        _sourceFiles.clear();
    }

    // At this point there should be at least one state loaded into memory!
    if (_states.empty()) {
        LERROR("Wasn't able to extract any valid states from provided source files!");
        return;
    }

    computeSequenceEndTime();
    setModelDependentConstants();

    setupProperties();

    // Setup shader program
    _shaderProgram = global::renderEngine.buildRenderProgram(
        "FieldlinesSequence",
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_vs.glsl"),
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_fs.glsl")
    );

    //------------------ Initialize OpenGL VBOs and VAOs-------------------------------//
    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
    glGenBuffers(1, &_vertexMaskingBuffer);

    // Needed for additive blending
    setRenderBin(Renderable::RenderBin::Overlay);
}

/**
 * Extracts the general information (from the lua modfile) that is mandatory for the class
 * to function; such as the file type and the location of the source files.
 * Returns false if it fails to extract mandatory information!
 */
bool RenderableFieldlinesSequence::extractMandatoryInfoFromDictionary(
                                                           SourceFileType& sourceFileType)
{

    _dictionary->getValue(SceneGraphNode::KeyIdentifier, _identifier);

    // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
    std::string inputFileTypeString;
    if (!_dictionary->getValue(KeyInputFileType, inputFileTypeString)) {
        LERROR(fmt::format("{}: The field {} is missing", _identifier, KeyInputFileType));
    }
    else {
        std::transform(
            inputFileTypeString.begin(),
            inputFileTypeString.end(),
            inputFileTypeString.begin(),
            [](char c) { return static_cast<char>(tolower(c)); }
        );
        // Verify that the input type is correct
        if (inputFileTypeString == ValueInputFileTypeCdf) {
            sourceFileType = SourceFileType::Cdf;
        }
        else if (inputFileTypeString == ValueInputFileTypeJson) {
            sourceFileType = SourceFileType::Json;
        }
        else if (inputFileTypeString == ValueInputFileTypeOsfls) {
            sourceFileType = SourceFileType::Osfls;
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

        // Remove all files that don't have <inputFileTypeString> as extension
        _sourceFiles.erase(
            std::remove_if(
                _sourceFiles.begin(),
                _sourceFiles.end(),
                [inputFileTypeString](const std::string& str) {
                    const size_t extLength = inputFileTypeString.length();
                    std::string sub = str.substr(str.length() - extLength, extLength);
                    std::transform(
                        sub.begin(),
                        sub.end(),
                        sub.begin(),
                        [](char c) { return static_cast<char>(::tolower(c)); }
                    );
                    return sub != inputFileTypeString;
                }),
            _sourceFiles.end()
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

void RenderableFieldlinesSequence::extractOptionalInfoFromDictionary(
                                                            std::string& outputFolderPath)
{

    // ------------------- EXTRACT OPTIONAL VALUES FROM DICTIONARY ------------------- //
    if (_dictionary->getValue(KeyOutputFolder, outputFolderPath)) {
        ghoul::filesystem::Directory outputFolder(outputFolderPath);
        if (FileSys.directoryExists(outputFolder)) {
            outputFolderPath = absPath(outputFolderPath);
        }
        else {
            LERROR(fmt::format(
                "{}: The specified output path: '{}', does not exist",
                _identifier, outputFolderPath
            ));
            outputFolderPath = "";
        }
    }

    ghoul::Dictionary colorTablesPathsDictionary;
    if (_dictionary->getValue(KeyColorTablePaths, colorTablesPathsDictionary)) {
        const size_t nProvidedPaths = colorTablesPathsDictionary.size();
        if (nProvidedPaths > 0) {
            // Clear the default! It is already specified in the transferFunction
            _colorTablePaths.clear();
            for (size_t i = 1; i <= nProvidedPaths; ++i) {
                _colorTablePaths.push_back(
                    colorTablesPathsDictionary.value<std::string>(std::to_string(i)));
            }
        }
    }

    ghoul::Dictionary colorTablesRangesDictionary;
    if (_dictionary->getValue(KeyColorTableRanges, colorTablesRangesDictionary)) {
        const size_t nProvidedRanges = colorTablesRangesDictionary.size();
        for (size_t i = 1; i <= nProvidedRanges; ++i) {
            _colorTableRanges.push_back(
                colorTablesRangesDictionary.value<glm::vec2>(std::to_string(i)));
        }
    }
    else {
        _colorTableRanges.push_back(glm::vec2(0, 1));
    }

    ghoul::Dictionary maskingRangesDictionary;
    if (_dictionary->getValue(KeyMaskingRanges, maskingRangesDictionary)) {
        const size_t nProvidedRanges = maskingRangesDictionary.size();
        for (size_t i = 1; i <= nProvidedRanges; ++i) {
            _maskingRanges.push_back(
                maskingRangesDictionary.value<glm::vec2>(std::to_string(i)));
        }
    }
    else {
        _maskingRanges.push_back(glm::vec2(-100000, 100000)); // Just some default values!
    }
}

/**
 * Returns false if it fails to extract mandatory information!
 */
bool RenderableFieldlinesSequence::extractJsonInfoFromDictionary(fls::Model& model) {
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

bool RenderableFieldlinesSequence::loadJsonStatesIntoRAM(const std::string& outputFolder)
{
    fls::Model model;
    if (!extractJsonInfoFromDictionary(model)) {
        return false;
    }
    // Load states into RAM!
    for (const std::string& filePath : _sourceFiles) {
        FieldlinesState newState;
        const bool loadedSuccessfully = newState.loadStateFromJson(
            filePath,
            model,
            _scalingFactor
        );
        if (loadedSuccessfully) {
            addStateToSequence(newState);
            if (!outputFolder.empty()) {
                newState.saveStateToOsfls(outputFolder);
            }
        }
    }
    return true;
}

bool RenderableFieldlinesSequence::prepareForOsflsStreaming() {
    extractTriggerTimesFromFileNames();
    FieldlinesState newState;
    if (!newState.loadStateFromOsfls(_sourceFiles[0])) {
        LERROR("The provided .osfls files seem to be corrupt!");
        return false;
    }
    _states.push_back(newState);
    _nStates = _startTimes.size();
    _activeStateIndex = 0;
    return true;
}

void RenderableFieldlinesSequence::loadOsflsStatesIntoRAM(const std::string& outputFolder)
{
    // Load states from .osfls files into RAM!
    for (const std::string& filePath : _sourceFiles) {
        FieldlinesState newState;
        if (newState.loadStateFromOsfls(filePath)) {
            addStateToSequence(newState);
            if (!outputFolder.empty()) {
                ghoul::filesystem::File tmpFile(filePath);
                newState.saveStateToJson(outputFolder + tmpFile.baseName());
            }
        }
        else {
            LWARNING(fmt::format("Failed to load state from: {}", filePath));
        }
    }
}

void RenderableFieldlinesSequence::extractOsflsInfoFromDictionary() {
    bool shouldLoadInRealtime = false;
    if (_dictionary->getValue(KeyOslfsLoadAtRuntime, shouldLoadInRealtime)) {
        _loadingStatesDynamically = shouldLoadInRealtime;
    }
    else {
        LWARNING(fmt::format(
            "{}: {} is not specified. States will be stored in RAM",
            _identifier, KeyOslfsLoadAtRuntime
        ));
    }
}

void RenderableFieldlinesSequence::setupProperties() {
    bool hasExtras = (_states[0].nExtraQuantities() > 0);

    // -------------- Add non-grouped properties (enablers and buttons) -------------- //
    addProperty(_pColorABlendEnabled);
    addProperty(_pDomainEnabled);
    addProperty(_pFlowEnabled);
    if (hasExtras) {
        addProperty(_pMaskingEnabled);
    }
    addProperty(_pFocusOnOriginBtn);
    addProperty(_pJumpToStartBtn);

    // ----------------------------- Add Property Groups ----------------------------- //
    addPropertySubOwner(_pColorGroup);
    addPropertySubOwner(_pDomainGroup);
    addPropertySubOwner(_pFlowGroup);
    if (hasExtras) {
        addPropertySubOwner(_pMaskingGroup);
    }

    // ------------------------- Add Properties to the groups ------------------------- //
    _pColorGroup.addProperty(_pColorUniform);
    _pDomainGroup.addProperty(_pDomainX);
    _pDomainGroup.addProperty(_pDomainY);
    _pDomainGroup.addProperty(_pDomainZ);
    _pDomainGroup.addProperty(_pDomainR);
    _pFlowGroup.addProperty(_pFlowReversed);
    _pFlowGroup.addProperty(_pFlowColor);
    _pFlowGroup.addProperty(_pFlowParticleSize);
    _pFlowGroup.addProperty(_pFlowParticleSpacing);
    _pFlowGroup.addProperty(_pFlowSpeed);
    if (hasExtras) {
        _pColorGroup.addProperty(_pColorMethod);
        _pColorGroup.addProperty(_pColorQuantity);
        _pColorGroup.addProperty(_pColorQuantityMin);
        _pColorGroup.addProperty(_pColorQuantityMax);
        _pColorGroup.addProperty(_pColorTablePath);
        _pMaskingGroup.addProperty(_pMaskingMin);
        _pMaskingGroup.addProperty(_pMaskingMax);
        _pMaskingGroup.addProperty(_pMaskingQuantity);

        // --------------------- Add Options to OptionProperties --------------------- //
        _pColorMethod.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");
        _pColorMethod.addOption(static_cast<int>(ColorMethod::ByQuantity), "By Quantity");
        // Add option for each extra quantity. Assumes there are just as many names to
        // extra quantities as there are extra quantities. Also assume that all states in
        // the given sequence have the same extra quantities! */
        const size_t nExtraQuantities = _states[0].nExtraQuantities();
        const std::vector<std::string>& extraNamesVec = _states[0].extraQuantityNames();
        for (int i = 0; i < static_cast<int>(nExtraQuantities); ++i) {
            _pColorQuantity.addOption(i, extraNamesVec[i]);
            _pMaskingQuantity.addOption(i, extraNamesVec[i]);
        }
        // Each quantity should have its own color table and color table range
        // no more, no less
        _colorTablePaths.resize(nExtraQuantities, _colorTablePaths.back());
        _colorTableRanges.resize(nExtraQuantities, _colorTableRanges.back());
        _maskingRanges.resize(nExtraQuantities, _maskingRanges.back());
    }

    definePropertyCallbackFunctions();

    if (hasExtras) {
        // Set defaults
        _pColorQuantity = 0;
        _pColorQuantityMin = std::to_string(_colorTableRanges[0].x);
        _pColorQuantityMax = std::to_string(_colorTableRanges[0].y);
        _pColorTablePath = _colorTablePaths[0];

        _pMaskingQuantity = 0;
        _pMaskingMin = std::to_string(_maskingRanges[0].x);
        _pMaskingMax = std::to_string(_maskingRanges[0].y);
    }
}

void RenderableFieldlinesSequence::definePropertyCallbackFunctions() {
    // Add Property Callback Functions
    bool hasExtras = (_states[0].nExtraQuantities() > 0);
    if (hasExtras) {
        _pColorQuantity.onChange([this] {
            _shouldUpdateColorBuffer = true;
            _pColorQuantityMin = std::to_string(_colorTableRanges[_pColorQuantity].x);
            _pColorQuantityMax = std::to_string(_colorTableRanges[_pColorQuantity].y);
            _pColorTablePath = _colorTablePaths[_pColorQuantity];
        });

        _pColorTablePath.onChange([this] {
            _transferFunction->setPath(_pColorTablePath);
            _colorTablePaths[_pColorQuantity] = _pColorTablePath;
        });

        _pColorQuantityMin.onChange([this] {
            const float f = stringToFloat(
                _pColorQuantityMin,
                _colorTableRanges[_pColorQuantity].x
            );
            _pColorQuantityMin = std::to_string(f);
            _colorTableRanges[_pColorQuantity].x = f;
        });

        _pColorQuantityMax.onChange([this] {
            const float f = stringToFloat(
                _pColorQuantityMax,
                _colorTableRanges[_pColorQuantity].y
            );
            _pColorQuantityMax = std::to_string(f);
            _colorTableRanges[_pColorQuantity].y = f;
        });

        _pMaskingQuantity.onChange([this] {
            _shouldUpdateMaskingBuffer = true;
            _pMaskingMin = std::to_string(_maskingRanges[_pMaskingQuantity].x);
            _pMaskingMax = std::to_string(_maskingRanges[_pMaskingQuantity].y);
        });

        _pMaskingMin.onChange([this] {
            const float f = stringToFloat(
                _pMaskingMin,
                _maskingRanges[_pMaskingQuantity].x
            );
            _pMaskingMin = std::to_string(f);
            _maskingRanges[_pMaskingQuantity].x = f;
        });

        _pMaskingMax.onChange([this] {
            const float f = stringToFloat(
                _pMaskingMax,
                _maskingRanges[_pMaskingQuantity].y
            );
            _pMaskingMax = std::to_string(f);
            _maskingRanges[_pMaskingQuantity].y = f;
        });
    }

    _pFocusOnOriginBtn.onChange([this] {
        SceneGraphNode* node = global::renderEngine.scene()->sceneGraphNode(_identifier);
        if (!node) {
            LWARNING(fmt::format(
                "Could not find a node in scenegraph called '{}'", _identifier
            ));
            return;
        }
        global::navigationHandler.setFocusNode(node->parent());
        global::navigationHandler.resetCameraDirection();
    });

    _pJumpToStartBtn.onChange([this] {
        global::timeManager.setTimeNextFrame(_startTimes[0]);
    });
}

// Calculate expected end time.
void RenderableFieldlinesSequence::computeSequenceEndTime() {
    if (_nStates > 1) {
        const double lastTriggerTime = _startTimes[_nStates - 1];
        const double sequenceDuration = lastTriggerTime - _startTimes[0];
        const double averageStateDuration = sequenceDuration /
            (static_cast<double>(_nStates) - 1.0);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    }
    else {
        // If there's just one state it should never disappear!
        _sequenceEndTime = DBL_MAX;
    }
}

void RenderableFieldlinesSequence::setModelDependentConstants() {
    const fls::Model simulationModel = _states[0].model();
    float limit = 100.f; // Just used as a default value.
    switch (simulationModel) {
        case fls::Model::Batsrus:
            _scalingFactor = fls::ReToMeter;
            limit = 300; // Should include a long magnetotail
            break;
        case fls::Model::Enlil:
            _pFlowReversed = true;
            _scalingFactor = fls::AuToMeter;
            limit = 50; // Should include Plutos furthest distance from the Sun
            break;
        case fls::Model::Pfss:
            _scalingFactor = fls::RsToMeter;
            limit = 100; // Just a default value far away from the solar surface
            break;
        default:
            break;
    }
    _pDomainX.setMinValue(glm::vec2(-limit));
    _pDomainX.setMaxValue(glm::vec2(limit));

    _pDomainY.setMinValue(glm::vec2(-limit));
    _pDomainY.setMaxValue(glm::vec2(limit));

    _pDomainZ.setMinValue(glm::vec2(-limit));
    _pDomainZ.setMaxValue(glm::vec2(limit));

    // Radial should range from 0 out to a corner of the cartesian box:
    // sqrt(3) = 1.732..., 1.75 is a nice and round number
    _pDomainR.setMinValue(glm::vec2(0));
    _pDomainR.setMaxValue(glm::vec2(limit * 1.75f));

    _pDomainX = glm::vec2(-limit, limit);
    _pDomainY = glm::vec2(-limit, limit);
    _pDomainZ = glm::vec2(-limit, limit);
    _pDomainR = glm::vec2(0, limit * 1.5f);
}

// Extract J2000 time from file names
// Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.osfls'
void RenderableFieldlinesSequence::extractTriggerTimesFromFileNames() {
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

void RenderableFieldlinesSequence::addStateToSequence(FieldlinesState& state) {
    _states.push_back(state);
    _startTimes.push_back(state.triggerTime());
    _nStates++;
}

bool RenderableFieldlinesSequence::getStatesFromCdfFiles(const std::string& outputFolder)
{
    std::string seedFilePath;
    std::string tracingVar;
    std::vector<std::string> extraVars;
    if (!extractCdfInfoFromDictionary(seedFilePath, tracingVar, extraVars)) {
        return false;
    }

    std::vector<glm::vec3> seedPoints;
    if (!extractSeedPointsFromFile(seedFilePath, seedPoints)) {
        return false;
    }

    std::vector<std::string> extraMagVars;
    extractMagnitudeVarsFromStrings(extraVars, extraMagVars);

    // Load states into RAM!
    for (const std::string& cdfPath : _sourceFiles) {
        FieldlinesState newState;
        bool isSuccessful = fls::convertCdfToFieldlinesState(
            newState,
            cdfPath,
            seedPoints,
            tracingVar,
            extraVars,
            extraMagVars
        );

        if (isSuccessful) {
            addStateToSequence(newState);
            if (!outputFolder.empty()) {
                newState.saveStateToOsfls(outputFolder);
            }
        }
    }
    return true;
}

/*
* Returns false if it fails to extract mandatory information!
*/
bool RenderableFieldlinesSequence::extractCdfInfoFromDictionary(std::string& seedFilePath,
                                                                std::string& tracingVar,
                                                      std::vector<std::string>& extraVars)
{

    if (_dictionary->getValue(KeyCdfSeedPointFile, seedFilePath)) {
        ghoul::filesystem::File seedPointFile(seedFilePath);
        if (FileSys.fileExists(seedPointFile)) {
            seedFilePath = absPath(seedFilePath);
        }
        else {
            LERROR(fmt::format(
                "{}: The specified seed poitn file: '{}' does not exist",
                _identifier, seedFilePath
            ));
            return false;
        }
    }
    else {
        LERROR(fmt::format("{}: Must specify '{}'", _identifier, KeyCdfSeedPointFile));
        return false;
    }

    if (!_dictionary->getValue(KeyCdfTracingVariable, tracingVar)) {
        tracingVar = "b"; //  Magnetic field variable as default
        LWARNING(fmt::format(
            "{}: No '{}', using default '{}'",
            _identifier, KeyCdfTracingVariable, tracingVar
        ));
    }

    ghoul::Dictionary extraQuantityNamesDictionary;
    if (_dictionary->getValue(KeyCdfExtraVariables, extraQuantityNamesDictionary)) {
        const size_t nProvidedExtras = extraQuantityNamesDictionary.size();
        for (size_t i = 1; i <= nProvidedExtras; ++i) {
            extraVars.push_back(
                extraQuantityNamesDictionary.value<std::string>(std::to_string(i))
            );
        }
    }

    return true;
}

bool RenderableFieldlinesSequence::extractSeedPointsFromFile(const std::string& path,
                                                           std::vector<glm::vec3>& outVec)
{

    std::ifstream seedFile(FileSys.relativePath(path));
    if (!seedFile.good()) {
        LERROR(fmt::format("Could not open seed points file '{}'", path));
        return false;
    }

    LDEBUG(fmt::format("Reading seed points from file '{}'", path));
    std::string line;
    while (std::getline(seedFile, line)) {
        std::stringstream ss(line);
        glm::vec3 point;
        ss >> point.x;
        ss >> point.y;
        ss >> point.z;
        outVec.push_back(std::move(point));
    }

    if (outVec.size() == 0) {
        LERROR(fmt::format("Found no seed points in: {}", path));
        return false;
    }

    return true;
}

void RenderableFieldlinesSequence::extractMagnitudeVarsFromStrings(
                                                      std::vector<std::string>& extraVars,
                                                   std::vector<std::string>& extraMagVars)
{

    for (int i = 0; i < static_cast<int>(extraVars.size()); i++) {
        const std::string& str = extraVars[i];
        // Check if string is in the format specified for magnitude variables
        if (str.substr(0, 2) == "|(" && str.substr(str.size() - 2, 2) == ")|") {
            std::istringstream ss(str.substr(2, str.size() - 4));
            std::string magVar;
            size_t counter = 0;
            while (std::getline(ss, magVar, ',')) {
                magVar.erase(
                    std::remove_if(
                        magVar.begin(),
                        magVar.end(),
                        ::isspace
                    ),
                    magVar.end()
                );
                extraMagVars.push_back(magVar);
                counter++;
                if (counter == 3) {
                    break;
                }
            }
            if (counter != 3 && counter > 0) {
                extraMagVars.erase(extraMagVars.end() - counter, extraMagVars.end());
            }
            extraVars.erase(extraVars.begin() + i);
            i--;
        }
    }
}

void RenderableFieldlinesSequence::deinitializeGL() {
    glDeleteVertexArrays(1, &_vertexArrayObject);
    _vertexArrayObject = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    glDeleteBuffers(1, &_vertexColorBuffer);
    _vertexColorBuffer = 0;

    glDeleteBuffers(1, &_vertexMaskingBuffer);
    _vertexMaskingBuffer = 0;

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

bool RenderableFieldlinesSequence::isReady() const {
    return _shaderProgram != nullptr;
}

void RenderableFieldlinesSequence::render(const RenderData& data, RendererTasks&) {
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

        _shaderProgram->setUniform("colorMethod",  _pColorMethod);
        _shaderProgram->setUniform("lineColor",    _pColorUniform);
        _shaderProgram->setUniform("usingDomain",  _pDomainEnabled);
        _shaderProgram->setUniform("usingMasking", _pMaskingEnabled);

        if (_pColorMethod == static_cast<int>(ColorMethod::ByQuantity)) {
            ghoul::opengl::TextureUnit textureUnit;
            textureUnit.activate();
            _transferFunction->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTable", textureUnit);
            _shaderProgram->setUniform("colorTableRange",
                                            _colorTableRanges[_pColorQuantity]);
        }

        if (_pMaskingEnabled) {
            _shaderProgram->setUniform("maskingRange", _maskingRanges[_pMaskingQuantity]);
        }

        _shaderProgram->setUniform("domainLimR", _pDomainR.value() * _scalingFactor);
        _shaderProgram->setUniform("domainLimX", _pDomainX.value() * _scalingFactor);
        _shaderProgram->setUniform("domainLimY", _pDomainY.value() * _scalingFactor);
        _shaderProgram->setUniform("domainLimZ", _pDomainZ.value() * _scalingFactor);

        // Flow/Particles
        _shaderProgram->setUniform("flowColor",       _pFlowColor);
        _shaderProgram->setUniform("usingParticles",  _pFlowEnabled);
        _shaderProgram->setUniform("particleSize",    _pFlowParticleSize);
        _shaderProgram->setUniform("particleSpacing", _pFlowParticleSpacing);
        _shaderProgram->setUniform("particleSpeed",   _pFlowSpeed);
        _shaderProgram->setUniform(
            "time",
            global::windowDelegate.applicationTime() * (_pFlowReversed ? -1 : 1)
        );

        bool additiveBlending = false;
        if (_pColorABlendEnabled) {
            const auto renderer = global::renderEngine.rendererImplementation();
            const bool usingFBufferRenderer = renderer ==
                                        RenderEngine::RendererImplementation::Framebuffer;

            const bool usingABufferRenderer = renderer ==
                                        RenderEngine::RendererImplementation::ABuffer;

            if (usingABufferRenderer) {
                _shaderProgram->setUniform("usingAdditiveBlending", _pColorABlendEnabled);
            }

            additiveBlending = usingFBufferRenderer;
            if (additiveBlending) {
                glDepthMask(false);
                glBlendFunc(GL_SRC_ALPHA, GL_ONE);
            }
        }

        glBindVertexArray(_vertexArrayObject);
        glMultiDrawArrays(
            GL_LINE_STRIP, //_drawingOutputType,
            _states[_activeStateIndex].lineStart().data(),
            _states[_activeStateIndex].lineCount().data(),
            static_cast<GLsizei>(_states[_activeStateIndex].lineStart().size())
        );

        glBindVertexArray(0);
        _shaderProgram->deactivate();

        if (additiveBlending) {
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            glDepthMask(true);
        }
    }
}

void RenderableFieldlinesSequence::update(const UpdateData& data) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }

    const double currentTime = data.time.j2000Seconds();
    const bool isInInterval = (currentTime >= _startTimes[0]) &&
                              (currentTime < _sequenceEndTime);

    // Check if current time in OpenSpace is within sequence interval
    if (isInInterval) {
        const size_t nextIdx = _activeTriggerTimeIndex + 1;
        if (
            // true => Previous frame was not within the sequence interval
            _activeTriggerTimeIndex < 0 ||
            // true => We stepped back to a time represented by another state
            currentTime < _startTimes[_activeTriggerTimeIndex] ||
            // true => We stepped forward to a time represented by another state
            (nextIdx < _nStates && currentTime >= _startTimes[nextIdx]))
        {
            updateActiveTriggerTimeIndex(currentTime);

            if (_loadingStatesDynamically) {
                _mustLoadNewStateFromDisk = true;
            } else {
                _needsUpdate = true;
                _activeStateIndex = _activeTriggerTimeIndex;
            }
        } // else {we're still in same state as previous frame (no changes needed)}
    } else {
        // Not in interval => set everything to false
        _activeTriggerTimeIndex   = -1;
        _mustLoadNewStateFromDisk = false;
        _needsUpdate              = false;
    }

    if (_mustLoadNewStateFromDisk) {
        if (!_isLoadingStateFromDisk && !_newStateIsReady) {
            _isLoadingStateFromDisk    = true;
            _mustLoadNewStateFromDisk  = false;
            std::string filePath = _sourceFiles[_activeTriggerTimeIndex];
            std::thread readBinaryThread([this, f = std::move(filePath)] {
                readNewState(f);
            });
            readBinaryThread.detach();
        }
    }

    if (_needsUpdate || _newStateIsReady) {
        if (_loadingStatesDynamically) {
            _states[0] = std::move(*_newState);
        }

        updateVertexPositionBuffer();

        if (_states[_activeStateIndex].nExtraQuantities() > 0) {
            _shouldUpdateColorBuffer = true;
            _shouldUpdateMaskingBuffer = true;
        }

        // Everything is set and ready for rendering!
        _needsUpdate = false;
        _newStateIsReady = false;
    }

    if (_shouldUpdateColorBuffer) {
        updateVertexColorBuffer();
        _shouldUpdateColorBuffer = false;
    }

    if (_shouldUpdateMaskingBuffer) {
        updateVertexMaskingBuffer();
        _shouldUpdateMaskingBuffer = false;
    }
}

// Assumes we already know that currentTime is within the sequence interval
void RenderableFieldlinesSequence::updateActiveTriggerTimeIndex(double currentTime) {
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
    } else {
        _activeTriggerTimeIndex = static_cast<int>(_nStates) - 1;
    }
}

// Reading state from disk. Must be thread safe!
void RenderableFieldlinesSequence::readNewState(const std::string& filePath) {
    _newState = std::make_unique<FieldlinesState>();
    if (_newState->loadStateFromOsfls(filePath)) {
        _newStateIsReady = true;
    }
    _isLoadingStateFromDisk = false;
}

// Unbind buffers and arrays
inline void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableFieldlinesSequence::updateVertexPositionBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    const std::vector<glm::vec3>& vertPos = _states[_activeStateIndex].vertexPositions();

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

void RenderableFieldlinesSequence::updateVertexColorBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    bool isSuccessful;
    const std::vector<float>& quantities = _states[_activeStateIndex].extraQuantity(
        _pColorQuantity,
        isSuccessful
    );

    if (isSuccessful) {
        glBufferData(
            GL_ARRAY_BUFFER,
            quantities.size() * sizeof(float),
            quantities.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(VaColor);
        glVertexAttribPointer(VaColor, 1, GL_FLOAT, GL_FALSE, 0, 0);

        unbindGL();
    }
}

void RenderableFieldlinesSequence::updateVertexMaskingBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexMaskingBuffer);

    bool isSuccessful;
    const std::vector<float>& maskings = _states[_activeStateIndex].extraQuantity(
        _pMaskingQuantity,
        isSuccessful
    );

    if (isSuccessful) {
        glBufferData(
            GL_ARRAY_BUFFER,
            maskings.size() * sizeof(float),
            maskings.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(VaMasking);
        glVertexAttribPointer(VaMasking, 1, GL_FLOAT, GL_FALSE, 0, 0);

        unbindGL();
    }
}

} // namespace openspace
