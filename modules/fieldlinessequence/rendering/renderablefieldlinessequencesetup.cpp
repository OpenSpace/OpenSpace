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

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/timemanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>

namespace {
    std::string _loggerCat = "RenderableFieldlinesSequence";

    // ----- KEYS POSSIBLE IN MODFILE. EXPECTED DATA TYPE OF VALUE IN [BRACKETS]  ----- //
    // ---------------------------- MANDATORY MODFILE KEYS ---------------------------- //
    const char* KEY_INPUT_FILE_TYPE         = "InputFileType";   // [STRING]
    const char* KEY_SOURCE_FOLDER           = "SourceFolder";    // [STRING]

    // ---------------------------- OPTIONAL MODFILE KEYS  ---------------------------- //
    const char* KEY_COLOR_TABLE_PATHS       = "ColorTablePaths"; // [STRING ARRAY] Values should be paths to .txt files
    const char* KEY_COLOR_TABLE_RANGES      = "ColorTableRanges";// [VEC2 ARRAY] Values should be entered as {X, Y}, where X & Y are numbers
    const char* KEY_MASKING_RANGES          = "MaskingRanges";   // [VEC2 ARRAY] Values should be entered as {X, Y}, where X & Y are numbers
    const char* KEY_OSLFS_LOAD_AT_RUNTIME   = "LoadAtRuntime";   // [BOOLEAN] If value False => Load in initializing step and store in RAM

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    const char* VALUE_INPUT_FILE_TYPE_CDF   = "cdf";
    const char* VALUE_INPUT_FILE_TYPE_JSON  = "json";
    const char* VALUE_INPUT_FILE_TYPE_OSFLS = "osfls";

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
        CDF = 0,
        JSON,
        OSFLS,
        INVALID
    };

    float stringToFloat(const std::string INPUT, const float BACKUP_VALUE = 0.f) {
        float tmp;
        try {
            tmp = std::stof(INPUT);
        } catch (const std::invalid_argument& ia) {
            LWARNING("Invalid argument: " << ia.what() << ". '" << INPUT <<
                "' is NOT a valid number!");
            return BACKUP_VALUE;
        }
        return tmp;
    }
} // namespace

namespace openspace {

RenderableFieldlinesSequence::RenderableFieldlinesSequence(const ghoul::Dictionary& DICTIONARY)
    : Renderable(DICTIONARY),
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

    _dictionary = std::make_unique<ghoul::Dictionary>(DICTIONARY);
}

void RenderableFieldlinesSequence::initialize() {
    LINFO("RenderableFieldlinesSequence::initialize()");

    // EXTRACT MANDATORY INFORMATION FROM DICTIONARY
    SourceFileType sourceFileType = SourceFileType::INVALID;
    if (!extractMandatoryInfoFromDictionary(sourceFileType)) {
        return;
    }

    // Set the default color table, just in case the (optional) user defined paths are corrupt!
    _colorTablePaths.push_back("${OPENSPACE_DATA}/colortables/kroyw.txt");
    _transferFunction = std::make_shared<TransferFunction>(absPath(_colorTablePaths[0]));

    // EXTRACT OPTIONAL INFORMATION FROM DICTIONARY
    std::string outputFolderPath;
    extractOptionalInfoFromDictionary(outputFolderPath);
    const bool SHOULD_SAVE_STATES = !outputFolderPath.empty();

    // EXTRACT SOURCE FILE TYPE SPECIFIC INFOMRATION FROM DICTIONARY & GET STATES FROM SOURCE
    switch (sourceFileType) {
        case SourceFileType::CDF:
            LERROR("CDF NOT YET IMPLEMENTED!"); return;
            break;
        case SourceFileType::JSON:
            LERROR("JSON NOT YET IMPLEMENTED!"); return;
            break;
        case SourceFileType::OSFLS:
            extractOsflsInfoFromDictionary();
            if (_loadingStatesDynamically) {
                extractTriggerTimesFromFileNames();
                FieldlinesState newState;
                bool loadedSuccessfully = newState.loadStateFromOsfls(_sourceFiles[0]);
                if (loadedSuccessfully) {
                    _states.push_back(newState);
                    _nStates = _startTimes.size();
                    _activeStateIndex = 0;
                } else {
                    LERROR("The provided .osfls files seem to be corrupt!");
                    sourceFileType = SourceFileType::INVALID;
                }
            } else {
                // Load states into RAM!
                for (std::string filePath : _sourceFiles) {
                    FieldlinesState newState;
                    bool loadedSuccessfully = newState.loadStateFromOsfls(filePath);
                    if (loadedSuccessfully) {
                        _states.push_back(newState);
                        _startTimes.push_back(newState.triggerTime());
                        _nStates++;
                    }
                }
            }
            break;
        default:
            return;
    }

    // dictionary is no longer needed as everything is extracted
    _dictionary.reset();

    // At this point there's at least one state loaded into memory!
    // No need to store source paths in memory if they are already in RAM!
    if (!_loadingStatesDynamically) {
        _sourceFiles.clear();
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
        sourceFileType = SourceFileType::INVALID;
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

/*
 * Returns false if it fails to extract mandatory information!
 */
bool RenderableFieldlinesSequence::extractMandatoryInfoFromDictionary(
        SourceFileType& sourceFileType) {

    _dictionary->getValue(SceneGraphNode::KeyName, _name);

    // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
    std::string inputFileTypeString;
    if (!_dictionary->getValue(KEY_INPUT_FILE_TYPE, inputFileTypeString)) {
        LERROR(_name << ": The field " << std::string(KEY_INPUT_FILE_TYPE) << " is missing!");
        return false;
    } else {
        std::transform(inputFileTypeString.begin(), inputFileTypeString.end(),
                       inputFileTypeString.begin(), ::tolower);
        // Verify that the input type is correct
        if (inputFileTypeString == VALUE_INPUT_FILE_TYPE_CDF) {
            sourceFileType = SourceFileType::CDF;
        } else if (inputFileTypeString == VALUE_INPUT_FILE_TYPE_JSON) {
            sourceFileType = SourceFileType::JSON;
        } else if (inputFileTypeString == VALUE_INPUT_FILE_TYPE_OSFLS) {
            sourceFileType = SourceFileType::OSFLS;
        } else {
            LERROR(_name << ": " << inputFileTypeString << " is not a recognised "
                << KEY_INPUT_FILE_TYPE);
            sourceFileType = SourceFileType::INVALID;
            return false;
        }
    }

    std::string sourceFolderPath;
    if (!_dictionary->getValue(KEY_SOURCE_FOLDER, sourceFolderPath)) {
        LERROR(_name << ": The field " << std::string(KEY_SOURCE_FOLDER) << " is missing!");
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
                    const size_t EXT_LENGTH = inputFileTypeString.length();
                    std::string sub = str.substr(str.length() - EXT_LENGTH, EXT_LENGTH);
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
    ghoul::Dictionary colorTablesPathsDictionary;
    if (_dictionary->getValue(KEY_COLOR_TABLE_PATHS, colorTablesPathsDictionary)) {
        const size_t N_PROVIDED_PATHS = colorTablesPathsDictionary.size();
        if (N_PROVIDED_PATHS > 0) {
            // Clear the default! It is already specified in the transferFunction
            _colorTablePaths.clear();
            for (size_t i = 1; i <= N_PROVIDED_PATHS; ++i) {
                _colorTablePaths.push_back(
                        colorTablesPathsDictionary.value<std::string>(std::to_string(i)));
            }
        }
    }

    ghoul::Dictionary colorTablesRangesDictionary;
    if (_dictionary->getValue(KEY_COLOR_TABLE_RANGES, colorTablesRangesDictionary)) {
        const size_t N_PROVIDED_RANGES = colorTablesRangesDictionary.size();
        for (size_t i = 1; i <= N_PROVIDED_RANGES; ++i) {
            _colorTableRanges.push_back(
                    colorTablesRangesDictionary.value<glm::vec2>(std::to_string(i)));
        }
    } else {
        _colorTableRanges.push_back(glm::vec2(0, 1));
    }

    ghoul::Dictionary maskingRangesDictionary;
    if (_dictionary->getValue(KEY_MASKING_RANGES, maskingRangesDictionary)) {
        const size_t N_PROVIDED_RANGES = maskingRangesDictionary.size();
        for (size_t i = 1; i <= N_PROVIDED_RANGES; ++i) {
            _maskingRanges.push_back(
                    maskingRangesDictionary.value<glm::vec2>(std::to_string(i)));
        }
    } else {
        _maskingRanges.push_back(glm::vec2(-100000, 100000)); // Just some default values!
    }
}

/*
 * Returns false if it fails to extract mandatory information!
 */
bool RenderableFieldlinesSequence::extractJsonInfoFromDictionary(fls::Model& model) {
    return true;
}

void RenderableFieldlinesSequence::extractOsflsInfoFromDictionary() {
    bool shouldLoadInRealtime = false;
    if (_dictionary->getValue(KEY_OSLFS_LOAD_AT_RUNTIME, shouldLoadInRealtime)) {
        _loadingStatesDynamically = shouldLoadInRealtime;
    } else {
        LWARNING(_name << ": " << KEY_OSLFS_LOAD_AT_RUNTIME <<
            " isn't specified! States will be stored in RAM!");
    }
}

void RenderableFieldlinesSequence::setupProperties() {
    // -------------- Add non-grouped properties (enablers and buttons) -------------- //
    addProperty(_pColorABlendEnabled);
    addProperty(_pDomainEnabled);
    addProperty(_pFlowEnabled);
    addProperty(_pMaskingEnabled);
    addProperty(_pFocusOnOriginBtn);
    addProperty(_pJumpToStartBtn);

    // ----------------------------- Add Property Groups ----------------------------- //
    addPropertySubOwner(_pColorGroup);
    addPropertySubOwner(_pDomainGroup);
    addPropertySubOwner(_pFlowGroup);
    addPropertySubOwner(_pMaskingGroup);

    // ------------------------- Add Properties to the groups ------------------------- //
    _pColorGroup.addProperty(_pColorMethod);
    _pColorGroup.addProperty(_pColorQuantity);
    _pColorGroup.addProperty(_pColorQuantityMin);
    _pColorGroup.addProperty(_pColorQuantityMax);
    _pColorGroup.addProperty(_pColorTablePath);
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

    _pMaskingGroup.addProperty(_pMaskingMin);
    _pMaskingGroup.addProperty(_pMaskingMax);
    _pMaskingGroup.addProperty(_pMaskingQuantity);

    // ----------------------- Add Options to OptionProperties ----------------------- //
    _pColorMethod.addOption(ColorMethod::UNIFORM, "Uniform");
    _pColorMethod.addOption(ColorMethod::BY_QUANTITY, "By Quantity");

    /* Add option for each extra quantity. We assume that there are just as many names to
       extra quantities as there are extra quantities. We also assume that all states in
       the given sequence have the same extra quantities! */
    const size_t N_EXTRA_QUANTITIES = _states[0].nExtraQuantities();
    auto EXTRA_VARIABLE_NAMES_VEC = _states[0].extraQuantityNames();
    for (int i = 0; i < N_EXTRA_QUANTITIES; ++i) {
        _pColorQuantity.addOption(i, EXTRA_VARIABLE_NAMES_VEC[i]);
        _pMaskingQuantity.addOption(i, EXTRA_VARIABLE_NAMES_VEC[i]);
    }
    // Each quantity should have its own color table and color table range, no more, no less
    _colorTablePaths.resize(N_EXTRA_QUANTITIES, _colorTablePaths.back());
    _colorTableRanges.resize(N_EXTRA_QUANTITIES, _colorTableRanges.back());
    _maskingRanges.resize(N_EXTRA_QUANTITIES, _maskingRanges.back());

    definePropertyCallbackFunctions();

    // Set defaults
    _pColorQuantity = 0;
    _pColorQuantityMin = std::to_string(_colorTableRanges[_pColorQuantity].x);
    _pColorQuantityMax = std::to_string(_colorTableRanges[_pColorQuantity].y);
    _pColorTablePath = _colorTablePaths[_pColorQuantity];

    _pMaskingQuantity = 0;
    _pMaskingMin = std::to_string(_maskingRanges[_pMaskingQuantity].x);
    _pMaskingMax = std::to_string(_maskingRanges[_pMaskingQuantity].y);
}

void RenderableFieldlinesSequence::definePropertyCallbackFunctions() {
    // Add Property Callback Functions
    _pColorQuantity.onChange([this] {
        LDEBUG("CHANGED COLORING QUANTITY");
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
        LDEBUG("CHANGED MIN VALUE");
        float f = stringToFloat(_pColorQuantityMin, _colorTableRanges[_pColorQuantity].x);
        _pColorQuantityMin = std::to_string(f);
        _colorTableRanges[_pColorQuantity].x = f;
    });

    _pColorQuantityMax.onChange([this] {
        LDEBUG("CHANGED MAX VALUE");
        float f = stringToFloat(_pColorQuantityMax, _colorTableRanges[_pColorQuantity].y);
        _pColorQuantityMax = std::to_string(f);
        _colorTableRanges[_pColorQuantity].y = f;
    });

    _pMaskingQuantity.onChange([this] {
        LDEBUG("CHANGED MASKING QUANTITY");
        _shouldUpdateMaskingBuffer = true;
        _pMaskingMin = std::to_string(_maskingRanges[_pMaskingQuantity].x);
        _pMaskingMax = std::to_string(_maskingRanges[_pMaskingQuantity].y);
    });

    _pMaskingMin.onChange([this] {
        LDEBUG("CHANGED LOWER MASKING LIMIT");
        float f = stringToFloat(_pMaskingMin, _maskingRanges[_pMaskingQuantity].x);
        _pMaskingMin = std::to_string(f);
        _maskingRanges[_pMaskingQuantity].x = f;
    });

    _pMaskingMax.onChange([this] {
        LDEBUG("CHANGED UPPER MASKING LIMIT");
        float f = stringToFloat(_pMaskingMax, _maskingRanges[_pMaskingQuantity].y);
        _pMaskingMax = std::to_string(f);
        _maskingRanges[_pMaskingQuantity].y = f;
    });

    _pFocusOnOriginBtn.onChange([this] {
        LDEBUG("SET FOCUS NODE TO PARENT");
        SceneGraphNode* node = OsEng.renderEngine().scene()->sceneGraphNode(_name);
        if (!node) {
            LWARNING("Could not find a node in scenegraph called '" << _name << "'");
            return;
        }
        OsEng.navigationHandler().setFocusNode(node->parent());
        OsEng.navigationHandler().resetCameraDirection();
    });

    _pJumpToStartBtn.onChange([this] {
        LDEBUG("Jump in time to start of sequence!");
        OsEng.timeManager().time().setTime(_startTimes[0]);
    });
}

// Calculate expected end time.
void RenderableFieldlinesSequence::computeSequenceEndTime() {
    if (_nStates > 1) {
        const double LAST_TRIGGER_TIME = _startTimes[_nStates - 1];
        const double SEQUENCE_DURATION = LAST_TRIGGER_TIME - _startTimes[0];
        const double AVERAGE_STATE_DURATION = SEQUENCE_DURATION /
                                              (static_cast<double>(_nStates) - 1.0);
        _sequenceEndTime = LAST_TRIGGER_TIME + AVERAGE_STATE_DURATION;
    } else {
        // If there's just one state it should never disappear!
        _sequenceEndTime = DBL_MAX;
    }
}

void RenderableFieldlinesSequence::setModelDependentConstants() {
    const fls::Model simulationModel = _states[0].model();
    float limit = 100.f; // Just used as a default value.
    switch (simulationModel) {
        case fls::Model::BATSRUS:
            _scalingFactor = fls::R_E_TO_METER;
            limit = 300; // Should include a long magnetotail
            break;
        case fls::Model::ENLIL:
            _pFlowReversed = true;
            _scalingFactor = fls::A_U_TO_METER;
            limit = 50; // Should include Plutos furthest distance from the Sun
            break;
        case fls::Model::PFSS:
            _scalingFactor = fls::R_S_TO_METER;
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
    _pDomainR = glm::vec2(0, limit*1.5f);
}

// Extract J2000 time from file names
// Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.osfls'
void RenderableFieldlinesSequence::extractTriggerTimesFromFileNames() {
    const size_t FILENAME_SIZE = 23; // number of  characters in filename (excluding '.osfls')
    const size_t EXT_SIZE = 6;  // size(".osfls")

    for (const std::string& FILEPATH : _sourceFiles) {
        const size_t STR_LENGTH = FILEPATH.size();
        // Extract the filename from the path (without extension)
        std::string timeString = FILEPATH.substr(STR_LENGTH - FILENAME_SIZE - EXT_SIZE,
                                                 FILENAME_SIZE - 1);
        // Ensure the separators are correct
        timeString.replace(4, 1, "-");
        timeString.replace(7, 1, "-");
        timeString.replace(13, 1, ":");
        timeString.replace(16, 1, ":");
        timeString.replace(19, 1, ".");
        const double TRIGGER_TIME = Time::convertTime(timeString);
        _startTimes.push_back(TRIGGER_TIME);
    }
}

} // namespace openspace
