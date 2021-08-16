/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <filesystem>
#include <fstream>
#include <map>
#include <optional>
#include <thread>

namespace {
    constexpr const char* _loggerCat = "RenderableFieldlinesSequence";

    constexpr const GLuint VaPosition = 0; // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaColor    = 1; // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaMasking  = 2; // MUST CORRESPOND TO THE SHADER PROGRAM

    // --------------------------------- Property Info -------------------------------- //
    constexpr openspace::properties::Property::PropertyInfo ColorMethodInfo = {
        "ColorMethod",
        "Color Method",
        "Color lines uniformly or using color tables based on extra quantities like, for "
        "examples, temperature or particle density."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorQuantityInfo = {
        "ColorQuantity",
        "Quantity to Color By",
        "Quantity used to color lines if the 'By Quantity' color method is selected."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorQuantityMinInfo = {
        "ColorQuantityMin",
        "ColorTable Min Value",
        "Value to map to the lowest end of the color table."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorQuantityMaxInfo = {
        "ColorQuantityMax",
        "ColorTable Max Value",
        "Value to map to the highest end of the color table."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "ColorTablePath",
        "Path to Color Table",
        "Color Table/Transfer Function to use for 'By Quantity' coloring."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorUniformInfo = {
        "Uniform",
        "Uniform Line Color",
        "The uniform color of lines shown when 'Color Method' is set to 'Uniform'."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorUseABlendingInfo = {
        "ABlendingEnabled",
        "Additive Blending",
        "Activate/deactivate additive blending."
    };
    constexpr openspace::properties::Property::PropertyInfo DomainEnabledInfo = {
        "DomainEnabled",
        "Domain Limits",
        "Enable/Disable domain limits"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainXInfo = {
        "LimitsX",
        "X-limits",
        "Valid range along the X-axis. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainYInfo = {
        "LimitsY",
        "Y-limits",
        "Valid range along the Y-axis. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainZInfo = {
        "LimitsZ",
        "Z-limits",
        "Valid range along the Z-axis. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainRInfo = {
        "LimitsR",
        "Radial limits",
        "Valid radial range. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "Color",
        "Color",
        "Color of particles."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowEnabledInfo = {
        "FlowEnabled",
        "Flow Direction",
        "Toggles the rendering of moving particles along the lines. Can, for example, "
        "illustrate magnetic flow."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowReversedInfo = {
        "Reversed",
        "Reversed Flow",
        "Toggle to make the flow move in the opposite direction."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowParticleSizeInfo = {
        "ParticleSize",
        "Particle Size",
        "Size of the particles."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowParticleSpacingInfo = {
        "ParticleSpacing",
        "Particle Spacing",
        "Spacing inbetween particles."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowSpeedInfo = {
        "Speed",
        "Speed",
        "Speed of the flow."
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingEnabledInfo = {
        "MaskingEnabled",
        "Masking",
        "Enable/disable masking. Use masking to show lines where a given quantity is "
        "within a given range, for example, if you only want to see where the "
        "temperature is between 10 and 20 degrees. Also used for masking out line "
        "topologies like solar wind & closed lines."
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingMinInfo = {
        "MaskingMinLimit",
        "Lower Limit",
        "Lower limit of the valid masking range"
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingMaxInfo = {
        "MaskingMaxLimit",
        "Upper Limit",
        "Upper limit of the valid masking range"
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingQuantityInfo = {
        "MaskingQuantity",
        "Quantity used for Masking",
        "Quantity used for masking."
    };
    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the field lines if the " 
        "selected rendering method includes lines."
    };
    constexpr openspace::properties::Property::PropertyInfo TimeJumpButtonInfo = {
        "TimeJumpToStart",
        "Jump to Start Of Sequence",
        "Performs a time jump to the start of the sequence."
    };
    enum class SourceFileType {
        Cdf = 0,
        Json,
        Osfls,
        Invalid
    };

    struct [[codegen::Dictionary(RenderableFieldlinesSequence)]] Parameters {
        // osfls, cdf or json  
        std::string inputFileType;

        // Should be path to folder containing the input files
        std::string sourceFolder;

        // Path to a .txt file containing seed points. Mandatory if CDF as input.
        // Files need time stamp in file name like so: yyyymmdd_hhmmss.txt
        std::optional<std::string> seedPointDirectory;

        // Currently supports: batsrus, enlil & pfss
        std::optional<std::string> simluationModel;
        
        // Extra variables such as rho, p or t
        std::optional<std::vector<std::string>> extraVariables;
        
        // Which variable in CDF file to trace. b is default for fieldline
        std::optional<std::string> tracingVariable;

        // 1.f is default, assuming meters as input.
        // In setup it is used to scale JSON coordinates. 
        // During runtime it is used to scale domain limits.
        std::optional<float> scaleToMeters;
        
        // If False (default) => Load in initializing step and store in RAM
        std::optional<bool> loadAtRuntime;

        // Values should be paths to .txt files
        std::optional<std::vector<std::string>> colorTablePaths;

        // Values should be entered as {X, Y}, where X & Y are numbers
        std::optional<std::vector<glm::vec2>> colorTableRanges;
        
        // Enables Flow
        std::optional<bool> flowEnabled;

        // Values should be entered as {X, Y}, where X & Y are numbers
        std::optional<std::vector<glm::vec2>> maskingRanges;
        
        // Value should be path to folder where states are saved. Specifying this
        // makes it use file type converter 
        // (JSON/CDF input => osfls output & oslfs input => JSON output)
        std::optional<std::string> outputFolder;
        
        // Line width of line
        std::optional<float> lineWidth;
        
        // If data sets parameter start_time differ from start of run, 
        // elapsed_time_in_seconds might be in relation to start of run. 
        // ManuelTimeOffset will be added to trigger time.
        std::optional<double> manualTimeOffset;
    };
#include "renderablefieldlinessequence_codegen.cpp"

    float stringToFloat(const std::string& input, float backupValue = 0.f) {
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

documentation::Documentation RenderableFieldlinesSequence::Documentation() {
    return codegen::doc<Parameters>("fieldlinessequence_renderablefieldlinessequence");
}

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
        glm::vec4(0.3f, 0.57f, 0.75f, 0.5f),
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
        glm::vec4(0.96f, 0.88f, 0.8f, 0.5f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _pFlowEnabled(FlowEnabledInfo, false)
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
    , _pLineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
    , _pJumpToStartBtn(TimeJumpButtonInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    // Extracts the general information (from the asset file) that 
    // is mandatory for the class to function;

    _inputFileTypeString = p.inputFileType;
    std::transform(
        _inputFileTypeString.begin(),
        _inputFileTypeString.end(),
        _inputFileTypeString.begin(),
        [](char c) { return static_cast<char>(tolower(c)); }
    );

    if (_inputFileTypeString == "osfls") _inputFileType = SourceFileType::Osfls;
    else if (_inputFileTypeString == "cdf") _inputFileType = SourceFileType::Cdf;
    else if (_inputFileTypeString == "json") _inputFileType = SourceFileType::Json;
    else _inputFileType = SourceFileType::Invalid;

    if (_inputFileTypeString == "cdf") {
        if( p.tracingVariable.has_value()) {
            _tracingVariable = *p.tracingVariable;
        }
        else {
            _tracingVariable = "b"; //  Magnetic field variable as default
            LWARNING(fmt::format(
                "No tracing variable, using default '{}'",
                _tracingVariable
            ));
        }
    }

    // Ensure that the source folder exists and then extract
    // the files with the same extension as <inputFileTypeString>
    std::string sourcePath = p.sourceFolder;
    if (!std::filesystem::is_directory(sourcePath)) {
        LERROR(fmt::format(
            "FieldlinesSequence {} is not a valid directory",
            sourcePath
        ));
    }

    // Extract all file paths from the provided folder
    _sourceFiles.clear();
    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::directory_iterator(sourcePath)) {
        if (e.is_regular_file()) {
            std::string eStr = e.path().string();
            _sourceFiles.push_back(eStr);
        }
    }
    std::sort(_sourceFiles.begin(), _sourceFiles.end());

    // Remove all files that don't have _inputFileTypeString as file extension
    std::string s = _inputFileTypeString;
    _sourceFiles.erase(
        std::remove_if(
            _sourceFiles.begin(),
            _sourceFiles.end(),
            [&s](const std::string& str) {
                const size_t extLength = s.length();
                std::string sub = str.substr(str.length() - extLength, extLength);
                std::transform(
                    sub.begin(),
                    sub.end(),
                    sub.begin(),
                    [](char c) { return static_cast<char>(::tolower(c)); }
                );
                return sub != s;
            }
        ),
        _sourceFiles.end()
    );

    // Ensure that there are available and valid source files left
    if (_sourceFiles.empty()) {
        LERROR(fmt::format("{} contains no {} files", sourcePath, _inputFileTypeString));
    }

    _colorTablePaths = p.colorTablePaths.value_or(_colorTablePaths);
    _extraVars = p.extraVariables.value_or(_extraVars);
    _pFlowEnabled = p.flowEnabled.value_or(_pFlowEnabled);
    _pLineWidth = p.lineWidth.value_or(_pLineWidth);
    _manualTimeOffset = p.manualTimeOffset.value_or(_manualTimeOffset);
    _modelStr = p.simluationModel.value_or(_modelStr);
    _seedPointDirectory = p.seedPointDirectory.value_or(_seedPointDirectory);

    if (p.colorTableRanges.has_value()) {
        _colorTableRanges = *p.colorTableRanges;
    }
    else {
        _colorTableRanges.push_back(glm::vec2(0.f, 1.f));
    }

    _loadingStatesDynamically = p.loadAtRuntime.value_or(_loadingStatesDynamically);
    if (_loadingStatesDynamically && _inputFileType != SourceFileType::Osfls ) {
        LWARNING("Load at run time is only supported for osfls file type");
        _loadingStatesDynamically = false;
    }

    if (p.maskingRanges.has_value()) {
        _maskingRanges = *p.maskingRanges;
    }
    else {
        _maskingRanges.push_back(glm::dvec2(-100000, 100000)); // Just some default values
    }

    _outputFolderPath = p.outputFolder.value_or(_outputFolderPath);
    if (!_outputFolderPath.empty() && !std::filesystem::is_directory(_outputFolderPath)) {
        _outputFolderPath.clear();
        LERROR(fmt::format(
            "The specified output path: '{}', does not exist", 
            _outputFolderPath
        ));
    }    
    
    _scalingFactor = p.scaleToMeters.value_or(_scalingFactor);
}

void RenderableFieldlinesSequence::initialize() {
    // Set a default color table, just in case the (optional) user defined paths are
    // corrupt or not provided!
    _colorTablePaths.push_back(FieldlinesSequenceModule::DefaultTransferFunctionFile);
    _transferFunction = std::make_unique<TransferFunction>(
        absPath(_colorTablePaths[0]).string()
    );

    // EXTRACT SOURCE FILE TYPE SPECIFIC INFOMRATION FROM DICTIONARY & GET STATES FROM
    // SOURCE
    switch (_inputFileType) {
        case SourceFileType::Cdf:

            if (!getStatesFromCdfFiles()) {
                return;
            }
            break;
        case SourceFileType::Json:
            if (!loadJsonStatesIntoRAM()) {
                return;
            }
            break;
        case SourceFileType::Osfls:
            if (_loadingStatesDynamically) {
                if (!prepareForOsflsStreaming()) {
                    return;
                }
            }
            else {
                loadOsflsStatesIntoRAM();
            }
            break;
        default:
            return;
    }

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
}

void RenderableFieldlinesSequence::initializeGL() {
    
    // Setup shader program
    _shaderProgram = global::renderEngine->buildRenderProgram(
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
 * Returns false if it fails to extract mandatory information!
 */
fls::Model RenderableFieldlinesSequence::extractJsonInfoFromDictionary() {

    //fls::Model model;
    if (!_modelStr.empty()) {
        std::transform(
            _modelStr.begin(),
            _modelStr.end(),
            _modelStr.begin(),
            [](char c) { return static_cast<char>(::tolower(c)); }
        );
        return fls::stringToModel(_modelStr);
    }
    else {
        LERROR("Must specify model");
        return fls::Model::Invalid;
    }
}

bool RenderableFieldlinesSequence::loadJsonStatesIntoRAM() {
    fls::Model model = extractJsonInfoFromDictionary();
    if (model == fls::Model::Invalid) {
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
            if (!_outputFolderPath.empty()) {
                newState.saveStateToOsfls(_outputFolderPath);
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

void RenderableFieldlinesSequence::loadOsflsStatesIntoRAM() {
    // Load states from .osfls files into RAM!
    for (const std::string& filePath : _sourceFiles) {
        FieldlinesState newState;
        if (newState.loadStateFromOsfls(filePath)) {
            addStateToSequence(newState);
            if (!_outputFolderPath.empty()) {
                newState.saveStateToJson(
                    _outputFolderPath + std::filesystem::path(filePath).stem().string()
                );
            }
        }
        else {
            LWARNING(fmt::format("Failed to load state from: {}", filePath));
        }
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
    addProperty(_pLineWidth);
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

    _pJumpToStartBtn.onChange([this] {
        global::timeManager->setTimeNextFrame(Time(_startTimes[0]));
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
    ++_nStates;
}

bool RenderableFieldlinesSequence::getStatesFromCdfFiles() {

    std::vector<std::string> extraMagVars = extractMagnitudeVarsFromStrings();

    std::unordered_map<std::string, std::vector<glm::vec3>> seedsPerFiles = 
        extractSeedPointsFromFiles();
    if (seedsPerFiles.empty()) {
        LERROR("No seed files found");
        return false;
    }

    // Load states into RAM!
    for (const std::string& cdfPath : _sourceFiles) {
        FieldlinesState newState;
        bool isSuccessful = fls::convertCdfToFieldlinesState(
            newState,
            cdfPath,
            seedsPerFiles,
            _manualTimeOffset,
            _tracingVariable,
            _extraVars,
            extraMagVars
        );

        if (isSuccessful) {
            addStateToSequence(newState);
            if (!_outputFolderPath.empty()) {
                newState.saveStateToOsfls(_outputFolderPath);
            }
        }
    }
    return true;
}

std::unordered_map<std::string, std::vector<glm::vec3>>
RenderableFieldlinesSequence::extractSeedPointsFromFiles() 
{
    std::vector<std::string> files;
    std::filesystem::path seedPointDir;
    std::unordered_map<std::string, std::vector<glm::vec3>> outMap;
    
    if (std::filesystem::is_directory(_seedPointDirectory)){
        seedPointDir = absPath(_seedPointDirectory);
        //_seedPointDirectory = seedPointDir.string();
    }
    else {
        LERROR(fmt::format(
            "The specified seed point directory: '{}' does not exist",
            _seedPointDirectory
        ));
        return outMap;
    }

    namespace fs = std::filesystem;
    for (const fs::directory_entry& spFile : fs::directory_iterator(seedPointDir)){
        std::string seedFilePath = spFile.path().string();
        if (!spFile.is_regular_file() || 
                        seedFilePath.substr(seedFilePath.find_last_of('.')+1) != "txt") {
            continue;
        }
        
        std::ifstream seedFile(spFile);
        if (!seedFile.good()) {
            LERROR(fmt::format("Could not open seed points file '{}'", seedFilePath));
            outMap.clear();
            return {};
        }

        LDEBUG(fmt::format("Reading seed points from file '{}'", seedFilePath));
        std::string line;
        std::vector<glm::vec3> outVec;
        while (std::getline(seedFile, line)) {
            std::stringstream ss(line);
            glm::vec3 point;
            ss >> point.x;
            ss >> point.y;
            ss >> point.z;
            outVec.push_back(std::move(point));
        }

        if (outVec.size() == 0) {
            LERROR(fmt::format("Found no seed points in: {}", seedFilePath));
            outMap.clear();
            return {};
        }

        size_t lastIndex = seedFilePath.find_last_of('.');
        std::string name = seedFilePath.substr(0, lastIndex);   // remove file extention
        size_t dateAndTimeSeperator = name.find_last_of('_');
        std::string time = name.substr(dateAndTimeSeperator + 1, name.length());
        std::string date = name.substr(dateAndTimeSeperator - 8, 8);    //8 for yyyymmdd
        std::string dateAndTime = date + time;
            
        // add outVec as value and time stamp as int as key
        outMap[dateAndTime] = outVec;
    }
    return outMap;
}

std::vector<std::string> RenderableFieldlinesSequence::extractMagnitudeVarsFromStrings() {
    std::vector<std::string> extraMagVars;
    for (int i = 0; i < static_cast<int>(_extraVars.size()); i++) {
        const std::string& str = _extraVars[i];
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
            _extraVars.erase(_extraVars.begin() + i);
            i--;
        }
    }
    return extraMagVars;
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
        global::renderEngine->removeRenderProgram(_shaderProgram.get());
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
            global::windowDelegate->applicationTime() * (_pFlowReversed ? -1 : 1)
        );

        bool additiveBlending = false;
        if (_pColorABlendEnabled) {
            additiveBlending = true;
            glDepthMask(false);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        }

        glBindVertexArray(_vertexArrayObject);
#ifndef __APPLE__
        glLineWidth(_pLineWidth);
#else      
        glLineWidth(1.f);
#endif

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
            }
            else {
                _needsUpdate = true;
                _activeStateIndex = _activeTriggerTimeIndex;
            }
        } // else {we're still in same state as previous frame (no changes needed)}
    }
    else {
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
    }
    else {
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
