/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <filesystem>
#include <fstream>
#include <map>
#include <optional>
#include <thread>

namespace {
    constexpr std::string_view _loggerCat = "RenderableFieldlinesSequence";

    constexpr openspace::properties::Property::PropertyInfo ColorMethodInfo = {
        "ColorMethod",
        "Color Method",
        "Color lines uniformly or using color tables based on extra quantities like, for "
        "examples, temperature or particle density.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo ColorQuantityInfo = {
        "ColorQuantity",
        "Quantity to Color By",
        "Quantity used to color lines if the 'By Quantity' color method is selected.",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo ColorMinMaxInfo = {
        "ColorQuantityMinMax",
        "ColorTable Min Value",
        "Value to map to the lowest and highest end of the color table.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "ColorTablePath",
        "Path to Color Table",
        "Color Table/Transfer Function to use for 'By Quantity' coloring.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo ColorUniformInfo = {
        "Color",
        "Uniform Line Color",
        "The uniform color of lines shown when 'Color Method' is set to 'Uniform'.",
        openspace::properties::Property::Visibility::NoviceUser
    };
    constexpr openspace::properties::Property::PropertyInfo ColorUseABlendingInfo = {
        "ABlendingEnabled",
        "Additive Blending",
        "Activate/deactivate additive blending.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo DomainEnabledInfo = {
        "DomainEnabled",
        "Domain Limits",
        "Enable/Disable domain limits.",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo DomainXInfo = {
        "LimitsX",
        "X-limits",
        "Valid range along the X-axis. [Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo DomainYInfo = {
        "LimitsY",
        "Y-limits",
        "Valid range along the Y-axis. [Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo DomainZInfo = {
        "LimitsZ",
        "Z-limits",
        "Valid range along the Z-axis. [Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo DomainRInfo = {
        "LimitsR",
        "Radial limits",
        "Valid radial range. [Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "FlowColor",
        "Flow Color",
        "Color of particles flow direction indication.",
        openspace::properties::Property::Visibility::NoviceUser
    };
    constexpr openspace::properties::Property::PropertyInfo FlowEnabledInfo = {
        "FlowEnabled",
        "Flow Direction",
        "Toggles the rendering of moving particles along the lines. Can, for example, "
        "illustrate magnetic flow.",
        openspace::properties::Property::Visibility::NoviceUser
    };
    constexpr openspace::properties::Property::PropertyInfo FlowReversedInfo = {
        "Reversed",
        "Reversed Flow",
        "Toggle to make the flow move in the opposite direction.",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo FlowParticleSizeInfo = {
        "ParticleSize",
        "Particle Size",
        "Size of the particles.",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo FlowParticleSpacingInfo = {
        "ParticleSpacing",
        "Particle Spacing",
        "Spacing inbetween particles.",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo FlowSpeedInfo = {
        "Speed",
        "Speed",
        "Speed of the flow.",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingEnabledInfo = {
        "MaskingEnabled",
        "Masking",
        "Enable/disable masking. Use masking to show lines where a given quantity is "
        "within a given range, for example, if you only want to see where the "
        "temperature is between 10 and 20 degrees. Also used for masking out line "
        "topologies like solar wind & closed lines.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingMinMaxInfo = {
        "MaskingMinLimit",
        "Lower Limit",
        "Lower and upper limit of the valid masking range.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingQuantityInfo = {
        "MaskingQuantity",
        "Quantity used for Masking",
        "Quantity used for masking.",
        openspace::properties::Property::Visibility::AdvancedUser
    };
    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the fieldlines.",
        openspace::properties::Property::Visibility::NoviceUser
    };
    constexpr openspace::properties::Property::PropertyInfo TimeJumpButtonInfo = {
        "TimeJumpToStart",
        "Jump to Start Of Sequence",
        "Performs a time jump to the start of the sequence.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    struct [[codegen::Dictionary(RenderableFieldlinesSequence)]] Parameters {
        enum class SourceFileType {
            Cdf,
            Json,
            Osfls
        };
        // Input file type. Should be cdf, json or osfls
        SourceFileType inputFileType;

        // Path to folder containing the input files
        std::filesystem::path sourceFolder [[codegen::directory()]];

        // Path to a .txt file containing seed points. Mandatory if CDF as input.
        // Files need time stamp in file name like so: yyyymmdd_hhmmss.txt
        std::optional<std::filesystem::path> seedPointDirectory [[codegen::directory()]];

        // Currently supports: batsrus, enlil & pfss
        std::optional<std::string> simulationModel;

        // Extra variables such as rho, p or t
        std::optional<std::vector<std::string>> extraVariables;

        // Which variable in CDF file to trace. b is default for fieldline
        std::optional<std::string> tracingVariable;

        // Convert the models distance unit, ex. AU for Enlil, to meters.
        // Can be used during runtime to scale domain limits.
        // 1.f is default, assuming meters as input.
        std::optional<float> scaleToMeters;

        // Set to true if you are streaming data during runtime
        std::optional<bool> loadAtRuntime;

        // [[codegen::verbatim(ColorUniformInfo.description)]]
        std::optional<glm::vec4> color [[codegen::color()]];

        // A list of paths to transferfunction .txt files containing color tables
        // used for colorizing the fieldlines according to different parameters
        std::optional<std::vector<std::filesystem::path>> colorTablePaths;

        // [[codegen::verbatim(ColorMethodInfo.description)]]
        std::optional<std::string> colorMethod;

        // [[codegen::verbatim(ColorQuantityInfo.description)]]
        std::optional<int> colorQuantity;

        // List of ranges for which their corresponding parameters values will be
        // colorized by. Should be entered as {min value, max value} per range
        std::optional<std::vector<glm::vec2>> colorTableRanges;

        // Enables flow, showing the direction, but not accurate speed, that particles
        // would be traveling
        std::optional<bool> flowEnabled;

        // [[codegen::verbatim(FlowColorInfo.description)]]
        std::optional<glm::vec4> flowColor [[codegen::color()]];

        // [[codegen::verbatim(FlowReversedInfo.description)]]
        std::optional<bool> reversedFlow;

        // [[codegen::verbatim(FlowParticleSizeInfo.description)]]
        std::optional<int> particleSize;

        // [[codegen::verbatim(FlowParticleSpacingInfo.description)]]
        std::optional<int> particleSpacing;

        // [[codegen::verbatim(FlowSpeedInfo.description)]]
        std::optional<int> flowSpeed;

        // [[codegen::verbatim(MaskingEnabledInfo.description)]]
        std::optional<bool> maskingEnabled;

        // [[codegen::verbatim(MaskingQuantityInfo.description)]]
        std::optional<int> maskingQuantity;

        // List of ranges for which their corresponding parameters values will be
        // masked by. Should be entered as {min value, max value} per range
        std::optional<std::vector<glm::vec2>> maskingRanges;

        // Value should be path to folder where states are saved. Specifying this
        // makes it use file type converter
        // (JSON/CDF input => osfls output & oslfs input => JSON output)
        std::optional<std::string> outputFolder;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // If data sets parameter start_time differ from start of run,
        // elapsed_time_in_seconds might be in relation to start of run.
        // ManuelTimeOffset will be added to trigger time.
        std::optional<double> manualTimeOffset;
    };
#include "renderablefieldlinessequence_codegen.cpp"
} // namespace

namespace openspace {
fls::Model stringToModel(std::string str);
std::unordered_map<std::string, std::vector<glm::vec3>>
    extractSeedPointsFromFiles(std::filesystem::path);
std::vector<std::string>
    extractMagnitudeVarsFromStrings(std::vector<std::string> extrVars);

documentation::Documentation RenderableFieldlinesSequence::Documentation() {
    return codegen::doc<Parameters>("fieldlinessequence_renderablefieldlinessequence");
}

RenderableFieldlinesSequence::RenderableFieldlinesSequence(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorGroup({ "Color" })
    , _colorMethod(ColorMethodInfo, properties::OptionProperty::DisplayType::Radio)
    , _colorQuantity(ColorQuantityInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _colorQuantityMinMax(
        ColorMinMaxInfo,
        glm::vec2(-0.f, 100.f),
        glm::vec2(-5000.f),
        glm::vec2(5000.f)
    )
    , _colorTablePath(ColorTablePathInfo)
    , _colorUniform(
        ColorUniformInfo,
        glm::vec4(0.3f, 0.57f, 0.75f, 0.5f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _colorABlendEnabled(ColorUseABlendingInfo, true)
    , _domainEnabled(DomainEnabledInfo, true)
    , _domainGroup({ "Domain" })
    , _domainX(DomainXInfo)
    , _domainY(DomainYInfo)
    , _domainZ(DomainZInfo)
    , _domainR(DomainRInfo)
    , _flowColor(
        FlowColorInfo,
        glm::vec4(0.96f, 0.88f, 0.8f, 0.5f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _flowEnabled(FlowEnabledInfo, false)
    , _flowGroup({ "Flow" })
    , _flowParticleSize(FlowParticleSizeInfo, 5, 0, 500)
    , _flowParticleSpacing(FlowParticleSpacingInfo, 60, 0, 500)
    , _flowReversed(FlowReversedInfo, false)
    , _flowSpeed(FlowSpeedInfo, 20, 0, 1000)
    , _maskingEnabled(MaskingEnabledInfo, false)
    , _maskingGroup({ "Masking" })
    , _maskingMinMax(
        MaskingMinMaxInfo,
        glm::vec2(0.f, 100.f),
        glm::vec2(-5000.f),
        glm::vec2(5000.f)
    )
    , _maskingQuantity(
        MaskingQuantityInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 20.f)
    , _jumpToStartBtn(TimeJumpButtonInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    // Extracts the general information (from the asset file) that
    // is mandatory for the class to function;
    std::string fileTypeString;
    switch (p.inputFileType) {
        case Parameters::SourceFileType::Cdf:
            _inputFileType = SourceFileType::Cdf;
            fileTypeString = "cdf";
            if (p.tracingVariable.has_value()) {
                _tracingVariable = *p.tracingVariable;
            }
            else {
                _tracingVariable = "b"; //  Magnetic field variable as default
                LWARNING(std::format(
                    "No tracing variable, using default '{}'", _tracingVariable
                ));
            }
            break;
        case Parameters::SourceFileType::Json:
            _inputFileType = SourceFileType::Json;
            fileTypeString = "json";
            break;
        case Parameters::SourceFileType::Osfls:
            _inputFileType = SourceFileType::Osfls;
            fileTypeString = "osfls";
            break;
    }

    // Ensure that the source folder exists and then extract
    // the files with the same extension as fileTypeString
    std::filesystem::path sourcePath = p.sourceFolder;
    if (!std::filesystem::is_directory(sourcePath)) {
        LERROR(std::format(
            "FieldlinesSequence '{}' is not a valid directory", sourcePath
        ));
    }

    // Extract all file paths from the provided folder
    namespace fs = std::filesystem;
    for (const fs::directory_entry& e : fs::directory_iterator(sourcePath)) {
        if (e.is_regular_file()) {
            std::string eStr = e.path().string();
            _sourceFiles.push_back(eStr);
        }
    }
    std::sort(_sourceFiles.begin(), _sourceFiles.end());

    // Remove all files that don't have fileTypeString as file extension
    _sourceFiles.erase(
        std::remove_if(
            _sourceFiles.begin(),
            _sourceFiles.end(),
            [&fileTypeString](const std::string& str) {
                const size_t extLength = fileTypeString.length();
                std::string sub = str.substr(str.length() - extLength, extLength);
                sub = ghoul::toLowerCase(sub);
                return sub != fileTypeString;
            }
        ),
        _sourceFiles.end()
    );

    // Ensure that there are available and valid source files left
    if (_sourceFiles.empty()) {
        LERROR(std::format("'{}' contains no {} files", sourcePath, fileTypeString));
    }
    _extraVars = p.extraVariables.value_or(_extraVars);
    _flowEnabled = p.flowEnabled.value_or(_flowEnabled);
    _flowColor = p.flowColor.value_or(_flowColor);
    _flowReversed = p.reversedFlow.value_or(_flowReversed);
    _flowParticleSize = p.particleSize.value_or(_flowParticleSize);
    _flowParticleSpacing = p.particleSpacing.value_or(_flowParticleSpacing);
    _flowSpeed = p.flowSpeed.value_or(_flowSpeed);
    _lineWidth = p.lineWidth.value_or(_lineWidth);
    _manualTimeOffset = p.manualTimeOffset.value_or(_manualTimeOffset);
    _modelStr = p.simulationModel.value_or(_modelStr);
    _seedPointDirectory = p.seedPointDirectory.value_or(_seedPointDirectory);
    _maskingEnabled = p.maskingEnabled.value_or(_maskingEnabled);
    _maskingQuantityTemp = p.maskingQuantity.value_or(_maskingQuantityTemp);
    if (p.colorTablePaths.has_value()) {
        _colorTablePaths = *p.colorTablePaths;
    }
    else {
        // Set a default color table, just in case the (optional) user defined paths are
        // corrupt or not provided
        _colorTablePaths.push_back(FieldlinesSequenceModule::DefaultTransferFunctionFile);
    }

    _colorUniform = p.color.value_or(_colorUniform);

    _colorMethod.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");
    _colorMethod.addOption(static_cast<int>(ColorMethod::ByQuantity), "By Quantity");
    if (p.colorMethod.has_value()) {
        if (p.colorMethod.value() == "Uniform") {
            _colorMethod = static_cast<int>(ColorMethod::Uniform);
        }
        else {
            _colorMethod = static_cast<int>(ColorMethod::ByQuantity);
        }
    }
    else {
        _colorMethod = static_cast<int>(ColorMethod::Uniform);
    }

    if (p.colorQuantity.has_value()) {
        _colorMethod = static_cast<int>(ColorMethod::ByQuantity);
        _colorQuantityTemp = *p.colorQuantity;
    }

    if (p.colorTableRanges.has_value()) {
        _colorTableRanges = *p.colorTableRanges;
    }
    else {
        _colorTableRanges.push_back(glm::vec2(0.f, 1.f));
    }

    _loadingStatesDynamically = p.loadAtRuntime.value_or(_loadingStatesDynamically);
    if (_loadingStatesDynamically && _inputFileType != SourceFileType::Osfls) {
        LWARNING("Load at run time is only supported for osfls file type");
        _loadingStatesDynamically = false;
    }

    if (p.maskingRanges.has_value()) {
        _maskingRanges = *p.maskingRanges;
    }
    else {
        _maskingRanges.push_back(glm::vec2(-100000.f, 100000.f)); // some default values
    }

    _outputFolderPath = p.outputFolder.value_or(_outputFolderPath);
    if (!_outputFolderPath.empty() && !std::filesystem::is_directory(_outputFolderPath)) {
        _outputFolderPath.clear();
        LERROR(std::format(
            "The specified output path '{}' does not exist", _outputFolderPath
        ));
    }

    _scalingFactor = p.scaleToMeters.value_or(_scalingFactor);
}

void RenderableFieldlinesSequence::initialize() {
    _transferFunction = std::make_unique<TransferFunction>(absPath(_colorTablePaths[0]));
}

void RenderableFieldlinesSequence::initializeGL() {
    // Setup shader program
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "FieldlinesSequence",
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_vs.glsl"),
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_fs.glsl")
    );

    // Extract source file type specific information from dictionary
    // & get states from source
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

    // No need to store source paths in memory if they are already in RAM
    if (!_loadingStatesDynamically) {
        _sourceFiles.clear();
    }

    // At this point there should be at least one state loaded into memory
    if (_states.empty()) {
        LERROR("Wasn't able to extract any valid states from provided source files");
        return;
    }

    computeSequenceEndTime();
    setModelDependentConstants();
    setupProperties();

    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
    glGenBuffers(1, &_vertexMaskingBuffer);

    // Needed for additive blending
    setRenderBin(Renderable::RenderBin::Overlay);
}

// Returns fls::Model::Invalid if it fails to extract mandatory information
fls::Model stringToModel(std::string str) {
    str = ghoul::toLowerCase(str);
    return fls::stringToModel(str);
}

bool RenderableFieldlinesSequence::loadJsonStatesIntoRAM() {
    fls::Model model = stringToModel(_modelStr);
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
        LERROR("The provided .osfls files seem to be corrupt");
        return false;
    }
    _states.push_back(newState);
    _nStates = _startTimes.size();
    if (_nStates == 1) {
        // loading dynamicaly is not nessesary if only having one set in the sequence
        _loadingStatesDynamically = false;
    }
    _activeStateIndex = 0;
    return true;
}

void RenderableFieldlinesSequence::loadOsflsStatesIntoRAM() {
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
            LWARNING(std::format("Failed to load state from '{}'", filePath));
        }
    }
}

void RenderableFieldlinesSequence::setupProperties() {
    bool hasExtras = (_states[0].nExtraQuantities() > 0);

    // Add non-grouped properties (enablers and buttons)
    addProperty(_colorABlendEnabled);
    addProperty(_domainEnabled);
    addProperty(_flowEnabled);
    if (hasExtras) {
        addProperty(_maskingEnabled);
    }
    addProperty(_lineWidth);
    addProperty(_jumpToStartBtn);

    // Add Property Groups
    addPropertySubOwner(_colorGroup);
    addPropertySubOwner(_domainGroup);
    addPropertySubOwner(_flowGroup);
    if (hasExtras) {
        addPropertySubOwner(_maskingGroup);
    }

    // Add Properties to the groups
    _colorUniform.setViewOption(properties::Property::ViewOptions::Color);
    _colorGroup.addProperty(_colorUniform);
    _domainGroup.addProperty(_domainX);
    _domainGroup.addProperty(_domainY);
    _domainGroup.addProperty(_domainZ);
    _domainGroup.addProperty(_domainR);
    _flowGroup.addProperty(_flowReversed);
    _flowColor.setViewOption(properties::Property::ViewOptions::Color);
    _flowGroup.addProperty(_flowColor);
    _flowGroup.addProperty(_flowParticleSize);
    _flowGroup.addProperty(_flowParticleSpacing);
    _flowGroup.addProperty(_flowSpeed);
    if (hasExtras) {
        _colorGroup.addProperty(_colorMethod);
        _colorGroup.addProperty(_colorQuantity);
        _colorQuantityMinMax.setViewOption(
            properties::Property::ViewOptions::MinMaxRange
        );
        _colorGroup.addProperty(_colorQuantityMinMax);
        _colorGroup.addProperty(_colorTablePath);
        _maskingGroup.addProperty(_maskingQuantity);
        _maskingMinMax.setViewOption(properties::Property::ViewOptions::MinMaxRange);
        _maskingGroup.addProperty(_maskingMinMax);

        // Add option for each extra quantity. Assumes there are just as many names to
        // extra quantities as there are extra quantities. Also assume that all states in
        // the given sequence have the same extra quantities
        const size_t nExtraQuantities = _states[0].nExtraQuantities();
        const std::vector<std::string>& extraNamesVec = _states[0].extraQuantityNames();
        for (int i = 0; i < static_cast<int>(nExtraQuantities); i++) {
            _colorQuantity.addOption(i, extraNamesVec[i]);
            _maskingQuantity.addOption(i, extraNamesVec[i]);
        }
        // Each quantity should have its own color table and color table range
        // no more, no less
        _colorTablePaths.resize(nExtraQuantities, _colorTablePaths.back());
        _colorTablePath = _colorTablePaths[0].string();
        _colorTableRanges.resize(nExtraQuantities, _colorTableRanges.back());
        _maskingRanges.resize(nExtraQuantities, _maskingRanges.back());
    }

    definePropertyCallbackFunctions();

    if (hasExtras) {
        // Set defaults
        _colorQuantity = _colorQuantityTemp;
        _colorQuantityMinMax = _colorTableRanges[_colorQuantity];

        _maskingQuantity = _maskingQuantityTemp;
        _maskingMinMax = _maskingRanges[_colorQuantity];
    }
}

void RenderableFieldlinesSequence::definePropertyCallbackFunctions() {
    // Add Property Callback Functions
    bool hasExtras = (_states[0].nExtraQuantities() > 0);
    if (hasExtras) {
        _colorQuantity.onChange([this]() {
            _shouldUpdateColorBuffer = true;
            _colorQuantityMinMax = _colorTableRanges[_colorQuantity];
            _colorTablePath = _colorTablePaths[_colorQuantity].string();
        });

        _colorTablePath.onChange([this]() {
            _transferFunction->setPath(_colorTablePath.value());
        });

        _colorQuantityMinMax.onChange([this]() {
            _colorTableRanges[_colorQuantity] = _colorQuantityMinMax;
        });

        _maskingQuantity.onChange([this]() {
            _shouldUpdateMaskingBuffer = true;
            _maskingMinMax = _maskingRanges[_maskingQuantity];
        });

        _maskingMinMax.onChange([this]() {
            _maskingRanges[_maskingQuantity] = _maskingMinMax;
        });
    }

    _jumpToStartBtn.onChange([this]() {
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
}

void RenderableFieldlinesSequence::setModelDependentConstants() {
    const fls::Model simulationModel = _states[0].model();
    float limit = 100.f; // Just used as a default value.
    switch (simulationModel) {
        case fls::Model::Batsrus:
            _scalingFactor = fls::ReToMeter;
            limit = 300.f; // Should include a long magnetotail
            break;
        case fls::Model::Enlil:
            _flowReversed = true;
            _scalingFactor = fls::AuToMeter;
            limit = 50.f; // Should include Plutos furthest distance from the Sun
            break;
        case fls::Model::Pfss:
            _scalingFactor = fls::RsToMeter;
            limit = 100.f; // Just a default value far away from the solar surface
            break;
        default:
            break;
    }
    _domainX.setMinValue(glm::vec2(-limit));
    _domainX.setMaxValue(glm::vec2(limit));

    _domainY.setMinValue(glm::vec2(-limit));
    _domainY.setMaxValue(glm::vec2(limit));

    _domainZ.setMinValue(glm::vec2(-limit));
    _domainZ.setMaxValue(glm::vec2(limit));

    // Radial should range from 0 out to a corner of the cartesian box:
    // sqrt(3) = 1.732..., 1.75 is a nice and round number
    _domainR.setMinValue(glm::vec2(0.f));
    _domainR.setMaxValue(glm::vec2(limit * 1.75f));

    _domainX = glm::vec2(-limit, limit);
    _domainY = glm::vec2(-limit, limit);
    _domainZ = glm::vec2(-limit, limit);
    _domainR = glm::vec2(0.f, limit * 1.5f);
}

// Extract J2000 time from file names
// Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.osfls'
void RenderableFieldlinesSequence::extractTriggerTimesFromFileNames() {
    // number of  characters in filename (excluding '.osfls')
    constexpr int FilenameSize = 23;
    // size(".osfls")
    constexpr int ExtSize = 6;

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
    std::vector<std::string> extraMagVars = extractMagnitudeVarsFromStrings(_extraVars);

    std::unordered_map<std::string, std::vector<glm::vec3>> seedsPerFiles =
        extractSeedPointsFromFiles(_seedPointDirectory);
    if (seedsPerFiles.empty()) {
        LERROR("No seed files found");
        return false;
    }

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
    extractSeedPointsFromFiles(std::filesystem::path filePath)
{
    std::unordered_map<std::string, std::vector<glm::vec3>> outMap;

    if (!std::filesystem::is_directory(filePath)) {
        LERROR(std::format(
            "The specified seed point directory '{}' does not exist", filePath
        ));
        return outMap;
    }

    namespace fs = std::filesystem;
    for (const fs::directory_entry& spFile : fs::directory_iterator(filePath)) {
        std::string seedFilePath = spFile.path().string();
        if (!spFile.is_regular_file() ||
            seedFilePath.substr(seedFilePath.find_last_of('.') + 1) != "txt")
        {
            continue;
        }

        std::ifstream seedFile(seedFilePath);
        if (!seedFile.good()) {
            LERROR(std::format("Could not open seed points file '{}'", seedFilePath));
            outMap.clear();
            return {};
        }

        LDEBUG(std::format("Reading seed points from file '{}'", seedFilePath));
        std::string line;
        std::vector<glm::vec3> outVec;
        while (ghoul::getline(seedFile, line)) {
            std::stringstream ss(line);
            glm::vec3 point;
            ss >> point.x;
            ss >> point.y;
            ss >> point.z;
            outVec.push_back(std::move(point));
        }

        if (outVec.empty()) {
            LERROR(std::format("Found no seed points in '{}'", seedFilePath));
            outMap.clear();
            return {};
        }

        size_t lastIndex = seedFilePath.find_last_of('.');
        std::string name = seedFilePath.substr(0, lastIndex);   // remove file extention
        size_t dateAndTimeSeperator = name.find_last_of('_');
        std::string time = name.substr(dateAndTimeSeperator + 1, name.length());
        std::string date = name.substr(dateAndTimeSeperator - 8, 8);    // 8 for yyyymmdd
        std::string dateAndTime = date + time;

        // add outVec as value and time stamp as int as key
        outMap[dateAndTime] = outVec;
    }
    return outMap;
}

std::vector<std::string>
    extractMagnitudeVarsFromStrings(std::vector<std::string> extrVars)
{
    std::vector<std::string> extraMagVars;
    for (int i = 0; i < static_cast<int>(extrVars.size()); i++) {
        const std::string& str = extrVars[i];
        // Check if string is in the format specified for magnitude variables
        if (str.substr(0, 2) == "|(" && str.substr(str.size() - 2, 2) == ")|") {
            std::istringstream ss(str.substr(2, str.size() - 4));
            std::string magVar;
            size_t counter = 0;
            while (ghoul::getline(ss, magVar, ',')) {
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
            extrVars.erase(extrVars.begin() + i);
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

    // Stall main thread until thread that's loading states is done
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
    if (_activeTriggerTimeIndex == -1) {
        return;
    }
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

    _shaderProgram->setUniform("colorMethod", _colorMethod);
    _shaderProgram->setUniform("lineColor", _colorUniform);
    _shaderProgram->setUniform("usingDomain", _domainEnabled);
    _shaderProgram->setUniform("usingMasking", _maskingEnabled);

    if (_colorMethod == static_cast<int>(ColorMethod::ByQuantity)) {
        ghoul::opengl::TextureUnit textureUnit;
        textureUnit.activate();
        _transferFunction->bind(); // Calls update internally
        _shaderProgram->setUniform("colorTable", textureUnit);
        _shaderProgram->setUniform("colorTableRange", _colorTableRanges[_colorQuantity]);
    }

    if (_maskingEnabled) {
        _shaderProgram->setUniform("maskingRange", _maskingRanges[_maskingQuantity]);
    }

    _shaderProgram->setUniform("domainLimR", _domainR.value() * _scalingFactor);
    _shaderProgram->setUniform("domainLimX", _domainX.value() * _scalingFactor);
    _shaderProgram->setUniform("domainLimY", _domainY.value() * _scalingFactor);
    _shaderProgram->setUniform("domainLimZ", _domainZ.value() * _scalingFactor);

    // Flow/Particles
    _shaderProgram->setUniform("flowColor", _flowColor);
    _shaderProgram->setUniform("usingParticles", _flowEnabled);
    _shaderProgram->setUniform("particleSize", _flowParticleSize);
    _shaderProgram->setUniform("particleSpacing", _flowParticleSpacing);
    _shaderProgram->setUniform("particleSpeed", _flowSpeed);
    _shaderProgram->setUniform(
        "time",
        global::windowDelegate->applicationTime() * (_flowReversed ? -1 : 1)
    );

    _shaderProgram->setUniform("opacity", opacity());

    bool additiveBlending = false;
    if (_colorABlendEnabled) {
        additiveBlending = true;
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }

    glBindVertexArray(_vertexArrayObject);
#ifndef __APPLE__
    glLineWidth(_lineWidth);
#else
    glLineWidth(1.f);
#endif

    glMultiDrawArrays(
        GL_LINE_STRIP,
        _states[_activeStateIndex].lineStart().data(),
        _states[_activeStateIndex].lineCount().data(),
        static_cast<GLsizei>(_states[_activeStateIndex].lineStart().size())
    );

    glBindVertexArray(0);
    _shaderProgram->deactivate();

    if (additiveBlending) {
        // Restores OpenGL Rendering State
        global::renderEngine->openglStateCache().resetBlendState();
        global::renderEngine->openglStateCache().resetDepthState();
    }
}

void RenderableFieldlinesSequence::update(const UpdateData& data) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }
    // True if new 'runtime-state' must be loaded from disk.
    // False => the previous frame's state should still be shown
    bool mustLoadNewStateFromDisk = false;
    // True if new 'in-RAM-state'  must be loaded.
    // False => the previous frame's state should still be shown
    bool needUpdate = false;
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
                mustLoadNewStateFromDisk = true;
            }
            else {
                needUpdate = true;
                _activeStateIndex = _activeTriggerTimeIndex;
            }
        } // else {we're still in same state as previous frame (no changes needed)}
    }
    // if only one state
    else if (_nStates == 1) {
        _activeTriggerTimeIndex = 0;
        _activeStateIndex = 0;
        if (!_hasBeenUpdated) {
            updateVertexPositionBuffer();
        }

        if (_states[_activeStateIndex].nExtraQuantities() > 0) {
            _shouldUpdateColorBuffer = true;
            _shouldUpdateMaskingBuffer = true;
        }

        _hasBeenUpdated = true;
    }
    else {
        // Not in interval => set everything to false
        _activeTriggerTimeIndex = -1;
        mustLoadNewStateFromDisk = false;
        needUpdate = false;
    }

    if (mustLoadNewStateFromDisk) {
        if (!_isLoadingStateFromDisk && !_newStateIsReady) {
            _isLoadingStateFromDisk = true;
            mustLoadNewStateFromDisk  = false;
            std::string filePath = _sourceFiles[_activeTriggerTimeIndex];
            std::thread readBinaryThread([this, f = std::move(filePath)]() {
                readNewState(f);
            });
            readBinaryThread.detach();
        }
    }

    if (needUpdate || _newStateIsReady) {
        if (_loadingStatesDynamically) {
            _states[0] = std::move(*_newState);
        }

        updateVertexPositionBuffer();

        if (_states[_activeStateIndex].nExtraQuantities() > 0) {
            _shouldUpdateColorBuffer = true;
            _shouldUpdateMaskingBuffer = true;
        }

        // Everything is set and ready for rendering
        needUpdate = false;
        _newStateIsReady = false;
    }

    if (_colorMethod == 1) { //By quantity
        if (_shouldUpdateColorBuffer) {
            updateVertexColorBuffer();
            _shouldUpdateColorBuffer = false;
        }

        if (_shouldUpdateMaskingBuffer) {
            updateVertexMaskingBuffer();
            _shouldUpdateMaskingBuffer = false;
        }
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
    if (_nStates == 1) {
        _activeTriggerTimeIndex = 0;
    }
}

// Reading state from disk. Must be thread safe
void RenderableFieldlinesSequence::readNewState(const std::string& filePath) {
    _newState = std::make_unique<FieldlinesState>();
    if (_newState->loadStateFromOsfls(filePath)) {
        _newStateIsReady = true;
    }
    _isLoadingStateFromDisk = false;
}

// Unbind buffers and arrays
void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableFieldlinesSequence::updateVertexPositionBuffer() {
    if (_activeStateIndex == -1) { return; }
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    const std::vector<glm::vec3>& vertPos = _states[_activeStateIndex].vertexPositions();

    glBufferData(
        GL_ARRAY_BUFFER,
        vertPos.size() * sizeof(glm::vec3),
        vertPos.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, 0);

    unbindGL();
}

void RenderableFieldlinesSequence::updateVertexColorBuffer() {
    if (_activeStateIndex == -1) { return; }
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    bool isSuccessful;
    const std::vector<float>& quantities = _states[_activeStateIndex].extraQuantity(
        _colorQuantity,
        isSuccessful
    );

    if (isSuccessful) {
        glBufferData(
            GL_ARRAY_BUFFER,
            quantities.size() * sizeof(float),
            quantities.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1, 1, GL_FLOAT, GL_FALSE, 0, 0);

        unbindGL();
    }
}

void RenderableFieldlinesSequence::updateVertexMaskingBuffer() {
    if (_activeStateIndex == -1) { return; }
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexMaskingBuffer);

    bool isSuccessful;
    const std::vector<float>& maskings = _states[_activeStateIndex].extraQuantity(
        _maskingQuantity,
        isSuccessful
    );

    if (isSuccessful) {
        glBufferData(
            GL_ARRAY_BUFFER,
            maskings.size() * sizeof(float),
            maskings.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(2);
        glVertexAttribPointer(2, 1, GL_FLOAT, GL_FALSE, 0, 0);

        unbindGL();
    }
}

} // namespace openspace
