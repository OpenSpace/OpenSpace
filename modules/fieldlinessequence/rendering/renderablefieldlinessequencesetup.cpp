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

#include <modules/fieldlinessequence/rendering/renderablefieldlinessequence.h>

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    #include <ccmc/Kameleon.h>
    #include <modules/kameleon/include/kameleonhelper.h>
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/timemanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

#include <fstream>
#include <sstream>

namespace {
    std::string _loggerCat = "RenderableFieldlinesSequence";

    // ----- KEYS POSSIBLE IN MODFILE. EXPECTED DATA TYPE OF VALUE IN [BRACKETS]  ----- //
    // ---------------------------- MANDATORY MODFILE KEYS ---------------------------- //
    const char* KeyInputFileType        = "InputFileType";   // [STRING] "cdf", "json" or "osfls"
    const char* KeySourceFolder         = "SourceFolder";    // [STRING] should be path to folder containing the input files

    // ---------------------- MANDATORY INPUT TYPE SPECIFIC KEYS ---------------------- //
    const char* KeyCdfSeedPointFile     = "SeedPointFile";   // [STRING] Path to a .txt file containing seed points
    const char* KeyJsonSimulationModel  = "SimulationModel"; // [STRING] Currently supports: "batsrus", "enlil" & "pfss"

    // ----------------------- OPTIONAL INPUT TYPE SPECIFIC KEYS ---------------------- //
    const char* KeyCdfExtraVariables    = "ExtraVariables";  // [STRING ARRAY]
    const char* KeyCdfTracingVariable   = "TracingVariable"; // [STRING]
    const char* KeyJsonScalingFactor    = "ScaleToMeters";   // [STRING]
    const char* KeyOslfsLoadAtRuntime   = "LoadAtRuntime";   // [BOOLEAN] If value False => Load in initializing step and store in RAM

    // ---------------------------- OPTIONAL MODFILE KEYS  ---------------------------- //
    const char* KeyColorTablePaths      = "ColorTablePaths"; // [STRING ARRAY] Values should be paths to .txt files
    const char* KeyColorTableRanges     = "ColorTableRanges";// [VEC2 ARRAY] Values should be entered as {X, Y}, where X & Y are numbers
    const char* KeyMaskingRanges        = "MaskingRanges";   // [VEC2 ARRAY] Values should be entered as {X, Y}, where X & Y are numbers
    const char* KeyOutputFolder         = "OutputFolder";    // [STRING] Value should be path to folder where states are saved (JSON/CDF input => osfls output & oslfs input => JSON output)

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    const char* ValueInputFileTypeCdf   = "cdf";
    const char* ValueInputFileTypeJson  = "json";
    const char* ValueInputFileTypeOsfls = "osfls";

    // --------------------------------- Property Info -------------------------------- //
    static const openspace::properties::Property::PropertyInfo ColorMethodInfo = {
        "colorMethod", "Color Method", "Color lines uniformly or using color tables based on extra quantities like e.g. temperature or particle density."
    };
    static const openspace::properties::Property::PropertyInfo ColorQuantityInfo = {
        "colorQuantity", "Quantity to Color By", "Quantity used to color lines if the \"By Quantity\" color method is selected."
    };
    static const openspace::properties::Property::PropertyInfo ColorQuantityMinInfo = {
        "colorQuantityMin", "ColorTable Min Value", "Value to map to the lowest end of the color table."
    };
    static const openspace::properties::Property::PropertyInfo ColorQuantityMaxInfo = {
        "colorQuantityMax", "ColorTable Max Value", "Value to map to the highest end of the color table."
    };
    static const openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "colorTablePath", "Path to Color Table", "Color Table/Transfer Function to use for \"By Quantity\" coloring."
    };
    static const openspace::properties::Property::PropertyInfo ColorUniformInfo = {
        "uniform", "Uniform Line Color", "The uniform color of lines shown when \"Color Method\" is set to \"Uniform\"."
    };
    static const openspace::properties::Property::PropertyInfo ColorUseABlendingInfo = {
        "aBlendingEnabled", "Additive Blending", "Activate/deactivate additive blending."
    };
    static const openspace::properties::Property::PropertyInfo DomainEnabledInfo = {
        "domainEnabled", "Domain Limits", "Enable/Disable domain limits"
    };
    static const openspace::properties::Property::PropertyInfo DomainXInfo = {
        "limitsX", "X-limits", "Valid range along the X-axis. [Min, Max]"
    };
    static const openspace::properties::Property::PropertyInfo DomainYInfo = {
        "limitsY", "Y-limits", "Valid range along the Y-axis. [Min, Max]"
    };
    static const openspace::properties::Property::PropertyInfo DomainZInfo = {
        "limitsZ", "Z-limits", "Valid range along the Z-axis. [Min, Max]"
    };
    static const openspace::properties::Property::PropertyInfo DomainRInfo = {
        "limitsR", "Radial limits", "Valid radial range. [Min, Max]"
    };
    static const openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "color", "Color", "Color of particles."
    };
    static const openspace::properties::Property::PropertyInfo FlowEnabledInfo = {
        "flowEnabled", "Flow Direction",
        "Toggles the rendering of moving particles along the lines. Can e.g. illustrate magnetic flow."
    };
    static const openspace::properties::Property::PropertyInfo FlowReversedInfo = {
        "reversed", "Reversed Flow", "Toggle to make the flow move in the opposite direction."
    };
    static const openspace::properties::Property::PropertyInfo FlowParticleSizeInfo = {
        "particleSize", "Particle Size", "Size of the particles."
    };
    static const openspace::properties::Property::PropertyInfo FlowParticleSpacingInfo = {
        "particleSpacing", "Particle Spacing", "Spacing inbetween particles."
    };
    static const openspace::properties::Property::PropertyInfo FlowSpeedInfo = {
        "speed", "Speed", "Speed of the flow."
    };
    static const openspace::properties::Property::PropertyInfo MaskingEnabledInfo = {
        "maskingEnabled", "Masking",
        "Enable/disable masking. Use masking to show lines where a given quantity is within a given range, e.g. if you only want to see where the temperature is between 10 and 20 degrees. Also used for masking out line topologies like solar wind & closed lines."
    };
    static const openspace::properties::Property::PropertyInfo MaskingMinInfo = {
        "maskingMinLimit", "Lower Limit", "Lower limit of the valid masking range"
    };
    static const openspace::properties::Property::PropertyInfo MaskingMaxInfo = {
        "maskingMaxLimit", "Upper Limit", "Upper limit of the valid masking range"
    };
    static const openspace::properties::Property::PropertyInfo MaskingQuantityInfo = {
        "maskingQuantity", "Quantity used for Masking", "Quantity used for masking."
    };
    static const openspace::properties::Property::PropertyInfo OriginButtonInfo = {
        "focusCameraOnParent", "Focus Camera", "Focus camera on parent."
    };
    static const openspace::properties::Property::PropertyInfo TimeJumpButtonInfo = {
        "timeJumpToStart", "Jump to Start Of Sequence", "Performs a time jump to the start of the sequence."
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
        } catch (const std::invalid_argument& ia) {
            LWARNING("Invalid argument: " << ia.what() << ". '" << input <<
                "' is NOT a valid number!");
            return backupValue;
        }
        return tmp;
    }
} // namespace

namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary),
      _pColorGroup({ "Color" }),
      _pColorMethod(ColorMethodInfo, properties::OptionProperty::DisplayType::Radio),
      _pColorQuantity(ColorQuantityInfo, properties::OptionProperty::DisplayType::Dropdown),
      _pColorQuantityMin(ColorQuantityMinInfo),
      _pColorQuantityMax(ColorQuantityMaxInfo),
      _pColorTablePath(ColorTablePathInfo),
      _pColorUniform(ColorUniformInfo, glm::vec4(0.75f, 0.5f, 0.0f, 0.5f),
                                       glm::vec4(0.f), glm::vec4(1.f)),
      _pColorABlendEnabled(ColorUseABlendingInfo, true),
      _pDomainEnabled(DomainEnabledInfo, true),
      _pDomainGroup({ "Domain" }),
      _pDomainX(DomainXInfo),
      _pDomainY(DomainYInfo),
      _pDomainZ(DomainZInfo),
      _pDomainR(DomainRInfo),
      _pFlowColor(FlowColorInfo, glm::vec4(0.8f, 0.7f, 0.0f, 0.6f),
                                 glm::vec4(0.f), glm::vec4(1.f)),
      _pFlowEnabled(FlowEnabledInfo, true),
      _pFlowGroup({ "Flow" }),
      _pFlowParticleSize(FlowParticleSizeInfo, 5, 0, 500),
      _pFlowParticleSpacing(FlowParticleSpacingInfo, 60, 0, 500),
      _pFlowReversed(FlowReversedInfo, false),
      _pFlowSpeed(FlowSpeedInfo, 20, 0, 1000),
      _pMaskingEnabled(MaskingEnabledInfo, false),
      _pMaskingGroup({ "Masking" }),
      _pMaskingMin(MaskingMinInfo),
      _pMaskingMax(MaskingMaxInfo),
      _pMaskingQuantity(MaskingQuantityInfo, properties::OptionProperty::DisplayType::Dropdown),
      _pFocusOnOriginBtn(OriginButtonInfo),
      _pJumpToStartBtn(TimeJumpButtonInfo) {

    _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
}

void RenderableFieldlinesSequence::initialize() {
    LINFO("RenderableFieldlinesSequence::initialize()");

    // EXTRACT MANDATORY INFORMATION FROM DICTIONARY
    SourceFileType sourceFileType = SourceFileType::Invalid;
    if (!extractMandatoryInfoFromDictionary(sourceFileType)) {
        return;
    }

    // Set a default color table, just in case the (optional) user defined paths are corrupt/not provided!
    _colorTablePaths.push_back("${OPENSPACE_DATA}/scene/fieldlinessequence/colortables/kroyw.txt");
    _transferFunction = std::make_shared<TransferFunction>(absPath(_colorTablePaths[0]));

    // EXTRACT OPTIONAL INFORMATION FROM DICTIONARY
    std::string outputFolderPath;
    extractOptionalInfoFromDictionary(outputFolderPath);

    // EXTRACT SOURCE FILE TYPE SPECIFIC INFOMRATION FROM DICTIONARY & GET STATES FROM SOURCE
    switch (sourceFileType) {
        case SourceFileType::Cdf:
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
            if (!getStatesFromCdfFiles(outputFolderPath)) {
                return;
            }
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED
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
            } else {
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
    if (_states.size() == 0) {
        LERROR("Wasn't able to extract any valid states from provided source files!");
        return;
    }

    computeSequenceEndTime();
    setModelDependentConstants();

    setupProperties();

    // Setup shader program
    _shaderProgram = OsEng.renderEngine().buildRenderProgram(
        "FieldlinesSequence",
        "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_vs.glsl",
        "${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_fs.glsl"
    );

    if (!_shaderProgram) {
        LERROR("Shader program failed initialization!");
        sourceFileType = SourceFileType::Invalid;
    }

    //------------------ Initialize OpenGL VBOs and VAOs-------------------------------//
    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
    glGenBuffers(1, &_vertexMaskingBuffer);

    // Needed for additive blending
    setRenderBin(Renderable::RenderBin::Overlay);

    _isReady = true;
}

/**
 * Extracts the general information (from the lua modfile) that is mandatory for the class
 * to function; such as the file type and the location of the source files.
 * Returns false if it fails to extract mandatory information!
 */
bool RenderableFieldlinesSequence::extractMandatoryInfoFromDictionary(
        SourceFileType& sourceFileType) {

    _dictionary->getValue(SceneGraphNode::KeyName, _name);

    // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
    std::string inputFileTypeString;
    if (!_dictionary->getValue(KeyInputFileType, inputFileTypeString)) {
        LERROR(_name << ": The field " << std::string(KeyInputFileType) << " is missing!");
        return false;
    } else {
        std::transform(inputFileTypeString.begin(), inputFileTypeString.end(),
                       inputFileTypeString.begin(), ::tolower);
        // Verify that the input type is correct
        if (inputFileTypeString == ValueInputFileTypeCdf) {
            sourceFileType = SourceFileType::Cdf;
#ifndef OPENSPACE_MODULE_KAMELEON_ENABLED
            LERROR(_name << ": CDF file inputs requires the 'Kameleon' module to be enabled!");
            return false;
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED
        } else if (inputFileTypeString == ValueInputFileTypeJson) {
            sourceFileType = SourceFileType::Json;
        } else if (inputFileTypeString == ValueInputFileTypeOsfls) {
            sourceFileType = SourceFileType::Osfls;
        } else {
            LERROR(_name << ": " << inputFileTypeString << " is not a recognised "
                << KeyInputFileType);
            sourceFileType = SourceFileType::Invalid;
            return false;
        }
    }

    std::string sourceFolderPath;
    if (!_dictionary->getValue(KeySourceFolder, sourceFolderPath)) {
        LERROR(_name << ": The field " << std::string(KeySourceFolder) << " is missing!");
        return false;
    }

    // Ensure that the source folder exists and then extract
    // the files with the same extension as <inputFileTypeString>
    ghoul::filesystem::Directory sourceFolder(sourceFolderPath);
    if (FileSys.directoryExists(sourceFolder)) {
        // Extract all file paths from the provided folder (Non-recursively! Sorted!)
        _sourceFiles = sourceFolder.readFiles(ghoul::Boolean::No, ghoul::Boolean::Yes);

        // Remove all files that don't have <inputFileTypeString> as extension
        _sourceFiles.erase(std::remove_if(_sourceFiles.begin(), _sourceFiles.end(),
                [inputFileTypeString](std::string str) {
                    const size_t extLength = inputFileTypeString.length();
                    std::string sub = str.substr(str.length() - extLength, extLength);
                    std::transform(sub.begin(), sub.end(), sub.begin(), ::tolower);
                    return sub != inputFileTypeString;
                }), _sourceFiles.end());
        // Ensure that there are available and valid source files left
        if (_sourceFiles.empty()) {
            LERROR(_name << ": " << sourceFolderPath << " contains no ." << inputFileTypeString
                << " files!");
            return false;
        }
    } else {
        LERROR(_name << ": FieldlinesSequence" << sourceFolderPath
            << " is not a valid directory!");
        return false;
    }

    return true;
}

void RenderableFieldlinesSequence::extractOptionalInfoFromDictionary(
        std::string& outputFolderPath) {

    // ------------------- EXTRACT OPTIONAL VALUES FROM DICTIONARY ------------------- //
    if (_dictionary->getValue(KeyOutputFolder, outputFolderPath)) {
        ghoul::filesystem::Directory outputFolder(outputFolderPath);
        if (FileSys.directoryExists(outputFolder)) {
            outputFolderPath = absPath(outputFolderPath);
        } else {
            LERROR(_name << ": The specified output path: '" << outputFolderPath << "', does not exist!");
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
    } else {
        _colorTableRanges.push_back(glm::vec2(0, 1));
    }

    ghoul::Dictionary maskingRangesDictionary;
    if (_dictionary->getValue(KeyMaskingRanges, maskingRangesDictionary)) {
        const size_t nProvidedRanges = maskingRangesDictionary.size();
        for (size_t i = 1; i <= nProvidedRanges; ++i) {
            _maskingRanges.push_back(
                    maskingRangesDictionary.value<glm::vec2>(std::to_string(i)));
        }
    } else {
        _maskingRanges.push_back(glm::vec2(-100000, 100000)); // Just some default values!
    }
}

/**
 * Returns false if it fails to extract mandatory information!
 */
bool RenderableFieldlinesSequence::extractJsonInfoFromDictionary(fls::Model& model) {
    std::string modelStr;
    if (_dictionary->getValue(KeyJsonSimulationModel, modelStr)) {
        std::transform(modelStr.begin(), modelStr.end(), modelStr.begin(), ::tolower);
        model = fls::stringToModel(modelStr);
    } else {
        LERROR(_name << ": Must specify '" << KeyJsonSimulationModel << "'");
        return false;
    }

    float scaleFactor;
    if (_dictionary->getValue(KeyJsonScalingFactor, scaleFactor)) {
        _scalingFactor = scaleFactor;
    } else {
        LWARNING(_name << ": Does not provide scalingFactor! " <<
                 "Assumes coordinates are already expressed in meters!");
    }
    return true;
}

bool RenderableFieldlinesSequence::loadJsonStatesIntoRAM(const std::string& outputFolder) {
    fls::Model model;
    if (!extractJsonInfoFromDictionary(model)) {
        return false;
    }
    // Load states into RAM!
    for (std::string filePath : _sourceFiles) {
        FieldlinesState newState;
        bool loadedSuccessfully = newState.loadStateFromJson(filePath, model,
                                                             _scalingFactor);
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

void RenderableFieldlinesSequence::loadOsflsStatesIntoRAM(const std::string& outputFolder) {
    // Load states from .osfls files into RAM!
    for (const std::string filePath : _sourceFiles) {
        FieldlinesState newState;
        if (newState.loadStateFromOsfls(filePath)) {
            addStateToSequence(newState);
            if (!outputFolder.empty()) {
                ghoul::filesystem::File tmpFile(filePath);
                newState.saveStateToJson(outputFolder + tmpFile.baseName());
            }
        } else {
            LWARNING("Failed to load state from: " << filePath);
        }
    }
}

void RenderableFieldlinesSequence::extractOsflsInfoFromDictionary() {
    bool shouldLoadInRealtime = false;
    if (_dictionary->getValue(KeyOslfsLoadAtRuntime, shouldLoadInRealtime)) {
        _loadingStatesDynamically = shouldLoadInRealtime;
    } else {
        LWARNING(_name << ": " << KeyOslfsLoadAtRuntime <<
            " isn't specified! States will be stored in RAM!");
    }
}

void RenderableFieldlinesSequence::setupProperties() {
    bool hasExtras = _states[0].nExtraQuantities() > 0;

    // -------------- Add non-grouped properties (enablers and buttons) -------------- //
    addProperty(_pColorABlendEnabled);
    addProperty(_pDomainEnabled);
    addProperty(_pFlowEnabled);
    if (hasExtras) { addProperty(_pMaskingEnabled); }
    addProperty(_pFocusOnOriginBtn);
    addProperty(_pJumpToStartBtn);

    // ----------------------------- Add Property Groups ----------------------------- //
    addPropertySubOwner(_pColorGroup);
    addPropertySubOwner(_pDomainGroup);
    addPropertySubOwner(_pFlowGroup);
    if (hasExtras) { addPropertySubOwner(_pMaskingGroup); }

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
        _pColorMethod.addOption(ColorMethod::Uniform, "Uniform");
        _pColorMethod.addOption(ColorMethod::ByQuantity, "By Quantity");
        // Add option for each extra quantity. Assumes there are just as many names to
        // extra quantities as there are extra quantities. Also assume that all states in
        // the given sequence have the same extra quantities! */
        const size_t nExtraQuantities = _states[0].nExtraQuantities();
        const std::vector<std::string>& extraNamesVec = _states[0].extraQuantityNames();
        for (int i = 0; i < nExtraQuantities; ++i) {
            _pColorQuantity.addOption(i, extraNamesVec[i]);
            _pMaskingQuantity.addOption(i, extraNamesVec[i]);
        }
        // Each quantity should have its own color table and color table range, no more, no less
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
    bool hasExtras = _states[0].nExtraQuantities() > 0;
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
            float f = stringToFloat(_pColorQuantityMin, _colorTableRanges[_pColorQuantity].x);
            _pColorQuantityMin = std::to_string(f);
            _colorTableRanges[_pColorQuantity].x = f;
        });

        _pColorQuantityMax.onChange([this] {
            float f = stringToFloat(_pColorQuantityMax, _colorTableRanges[_pColorQuantity].y);
            _pColorQuantityMax = std::to_string(f);
            _colorTableRanges[_pColorQuantity].y = f;
        });

        _pMaskingQuantity.onChange([this] {
            _shouldUpdateMaskingBuffer = true;
            _pMaskingMin = std::to_string(_maskingRanges[_pMaskingQuantity].x);
            _pMaskingMax = std::to_string(_maskingRanges[_pMaskingQuantity].y);
        });

        _pMaskingMin.onChange([this] {
            float f = stringToFloat(_pMaskingMin, _maskingRanges[_pMaskingQuantity].x);
            _pMaskingMin = std::to_string(f);
            _maskingRanges[_pMaskingQuantity].x = f;
        });

        _pMaskingMax.onChange([this] {
            float f = stringToFloat(_pMaskingMax, _maskingRanges[_pMaskingQuantity].y);
            _pMaskingMax = std::to_string(f);
            _maskingRanges[_pMaskingQuantity].y = f;
        });
    }

    _pFocusOnOriginBtn.onChange([this] {
        SceneGraphNode* node = OsEng.renderEngine().scene()->sceneGraphNode(_name);
        if (!node) {
            LWARNING("Could not find a node in scenegraph called '" << _name << "'");
            return;
        }
        OsEng.navigationHandler().setFocusNode(node->parent());
        OsEng.navigationHandler().resetCameraDirection();
    });

    _pJumpToStartBtn.onChange([this] {
        OsEng.timeManager().time().setTime(_startTimes[0]);
    });
}

// Calculate expected end time.
void RenderableFieldlinesSequence::computeSequenceEndTime() {
    if (_nStates > 1) {
        const double lastTriggerTime      = _startTimes[_nStates - 1];
        const double sequenceDuration     = lastTriggerTime - _startTimes[0];
        const double averageStateDuration = sequenceDuration /
                                            (static_cast<double>(_nStates) - 1.0);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    } else {
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
    _pDomainX.setMinValue(glm::vec2(-limit)); _pDomainX.setMaxValue(glm::vec2(limit));
    _pDomainY.setMinValue(glm::vec2(-limit)); _pDomainY.setMaxValue(glm::vec2(limit));
    _pDomainZ.setMinValue(glm::vec2(-limit)); _pDomainZ.setMaxValue(glm::vec2(limit));
    // Radial should range from 0 out to a corner of the cartesian box: sqrt(3) = 1.732..., 1.75 is a nice and round number
    _pDomainR.setMinValue(glm::vec2(0));      _pDomainR.setMaxValue(glm::vec2(limit*1.75f));

    _pDomainX = glm::vec2(-limit, limit);
    _pDomainY = glm::vec2(-limit, limit);
    _pDomainZ = glm::vec2(-limit, limit);
    _pDomainR = glm::vec2(0, limit * 1.5f);
}

// Extract J2000 time from file names
// Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.osfls'
void RenderableFieldlinesSequence::extractTriggerTimesFromFileNames() {
    const size_t filenameSize = 23; // number of  characters in filename (excluding '.osfls')
    const size_t extSize = 6;  // size(".osfls")

    for (const std::string& filePath : _sourceFiles) {
        const size_t strLength = filePath.size();
        // Extract the filename from the path (without extension)
        std::string timeString = filePath.substr(strLength - filenameSize - extSize,
                                                 filenameSize - 1);
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

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
bool RenderableFieldlinesSequence::getStatesFromCdfFiles(const std::string& outputFolder) {

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
    for (std::string filePath : _sourceFiles) {
        // Create Kameleon object and open CDF file!
        std::unique_ptr<ccmc::Kameleon> kameleon =
                kameleonHelper::createKameleonObject(filePath);

        FieldlinesState newState;
        newState.setTriggerTime(kameleonHelper::getTime(kameleon.get()));

        if (newState.addLinesFromKameleon(kameleon.get(), seedPoints, tracingVar)) {
            // The line points are in their RAW format (unscaled & maybe spherical)
            // Before we scale to meters (and maybe cartesian) we must extract
            // the extraQuantites, as the iterpolator needs the unaltered positions
            newState.addExtraQuantities(kameleon.get(), extraVars, extraMagVars);
            switch (newState.model()) {
                case fls::Batsrus:
                    newState.scalePositions(fls::ReToMeter);
                    break;
                case fls::Enlil :
                    newState.convertLatLonToCartesian(fls::AuToMeter);
                    break;
                default:
                    break;
            }

            addStateToSequence(newState);
            if (!outputFolder.empty()) {
                newState.saveStateToOsfls(outputFolder);
            }
        }
    }
    return true;
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
/*
 * Returns false if it fails to extract mandatory information!
 */
bool RenderableFieldlinesSequence::extractCdfInfoFromDictionary(
        std::string& seedFilePath,
        std::string& tracingVar,
        std::vector<std::string>& extraVars) {

    if (_dictionary->getValue(KeyCdfSeedPointFile, seedFilePath)) {
        ghoul::filesystem::File seedPointFile(seedFilePath);
        if (FileSys.fileExists(seedPointFile)) {
            seedFilePath = absPath(seedFilePath);
        } else {
            LERROR(_name << ": The specified seed point file: '" << seedFilePath
                         << "', does not exist!");
            return false;
        }
    } else {
        LERROR(_name << ": Must specify '" << KeyCdfSeedPointFile << "'");
        return false;
    }

    if (!_dictionary->getValue(KeyCdfTracingVariable, tracingVar)) {
        tracingVar = "b"; //  Magnetic field variable as default
        LWARNING(_name << ": No '" << KeyCdfTracingVariable << "', using default: "
                       << tracingVar);
    }

    ghoul::Dictionary extraQuantityNamesDictionary;
    if (_dictionary->getValue(KeyCdfExtraVariables, extraQuantityNamesDictionary)) {
        const size_t nProvidedExtras = extraQuantityNamesDictionary.size();
        for (size_t i = 1; i <= nProvidedExtras; ++i) {
            extraVars.push_back(
                    extraQuantityNamesDictionary.value<std::string>(std::to_string(i)));
        }
    }

    return true;
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
bool RenderableFieldlinesSequence::extractSeedPointsFromFile(
            const std::string& path,
            std::vector<glm::vec3>& outVec) {

    std::ifstream seedFile(FileSys.relativePath(path));
    if (!seedFile.good()) {
        LERROR("Could not open seed points file '" << path << "'");
        return false;
    }

    LDEBUG("Reading seed points from file '" << path << "'");
    std::string line;
    while (std::getline(seedFile, line)) {
        glm::vec3 point;
        std::stringstream ss(line);
        ss >> point.x;
        ss >> point.y;
        ss >> point.z;
        outVec.push_back(std::move(point));
    }

    if (outVec.size() == 0) {
        LERROR("Found no seed points in: " << path);
        return false;
    }

    return true;
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
void RenderableFieldlinesSequence::extractMagnitudeVarsFromStrings(
        std::vector<std::string>& extraVars,
        std::vector<std::string>& extraMagVars) {


    for (int i = 0; i < extraVars.size(); i++) {
        const std::string str = extraVars[i];
        // Check if string is in the format specified for magnitude variables
        if (str.substr(0, 2) == "|(" && str.substr(str.size() - 2, 2) == ")|") {
            std::istringstream ss(str.substr(2, str.size() - 4));
            std::string magVar;
            size_t counter = 0;
            while(std::getline(ss, magVar, ',')) {
                magVar.erase(std::remove_if(magVar.begin(), magVar.end(), ::isspace),
                        magVar.end());
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
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

} // namespace openspace
