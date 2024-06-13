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
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <thread>
#include <iostream>

namespace {
    constexpr std::string_view _loggerCat = "RenderableFieldlinesSequence";

    constexpr openspace::properties::Property::PropertyInfo ColorMethodInfo = {
        "ColorMethod",
        "Color Method",
        "Color lines uniformly or using color tables based on extra quantities like, for "
        "examples, temperature or particle density.",
        openspace::properties::Property::Visibility::User
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
    constexpr openspace::properties::Property::PropertyInfo FlowEnabledInfo = {
        "FlowEnabled",
        "Flow Enabled",
        "Toggles the rendering of moving particles along the lines. Can, for example, "
        "illustrate magnetic flow.",
        // @VISIBILITY(1.33)
        openspace::properties::Property::Visibility::NoviceUser
    };
    constexpr openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "FlowColor",
        "Flow Color",
        "Color of particles flow direction indication",
        // @VISIBILITY(1.67)
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
        "Masking Enabled",
        "Enable/disable masking. Use masking to show lines where a given quantity is "
        "within a given range, for example, if you only want to see where the "
        "temperature is between 10 and 20 degrees. Also used for masking out line "
        "topologies like solar wind & closed lines.",
        openspace::properties::Property::Visibility::User
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
        enum class [[codegen::map(openspace::RenderableFieldlinesSequence::ColorMethod)]] ColorMethod {
            Uniform = 0,
            ByQuantity = 1
        };
        // [[codegen::verbatim(ColorMethodInfo.description)]]
        std::optional<ColorMethod> colorMethod;
        // [[codegen::verbatim(ColorQuantityInfo.description)]]
        std::optional<int> colorQuantity;
        // [[codegen::verbatim(ColorUniformInfo.description)]]
        std::optional<glm::vec4> color [[codegen::color()]];
        // A list of paths to transferfunction .txt files containing color tables
        // used for colorizing the fieldlines according to different parameters
        std::optional<std::vector<std::string>> colorTablePaths;
        // Ranges for which their corresponding parameters values will be
        // colorized by. Should be entered as min value, max value
        std::optional<std::vector<glm::vec2>> colorTableRanges;
        // Specifies the total range
        std::optional<glm::vec2> colorMinMaxRange;

        // [[codegen::verbatim(FlowEnabledInfo.description)]]
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
        // Ranges for which their corresponding quantity parameter value will be
        // masked by. Should be entered as {min value, max value}
        std::optional<std::vector<glm::vec2>> maskingRanges;
        // Ranges for which their corresponding parameters values will be
        // masked by. Should be entered as min value, max value
        std::optional<glm::vec2> maskingMinMaxRange;

        // [[codegen::verbatim(DomainEnabledInfo.description)]]
        std::optional<bool> domainEnabled;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(ColorUseABlendingInfo.description)]]
        std::optional<bool> alphaBlendingEnabled;

        // Set if first/last file should render forever
        bool showAtAllTimes;

        // If data sets parameter start_time differ from start of run,
        // elapsed_time_in_seconds might be in relation to start of run.
        // ManuelTimeOffset will be added to trigger time.
        std::optional<float> manualTimeOffset;

        enum class [[codegen::map(openspace::fls::Model)]] Model {
            Batsrus,
            Enlil,
            Pfss
        };
        // Currently supports: batsrus, enlil & pfss. Not specified -> model == invalid
        // which just means that the scaleFactor (scaleToMeters) will be 1.f assuming
        // meter as input
        std::optional<Model> simulationModel;

        // Convert the models distance unit, ex. AU to meters for Enlil.
        // 1.f is default, assuming meters as input.
        // Does not need to be specified if simulationModel is specified.
        // Using a different model? Set this to scale your vertex positions to meters.
        std::optional<float> scaleToMeters;

        // choose type of loading:
        //0: static loading and static downloading
        //1: dynamic loading and static downloading
        //2: dynamic loading and dynamic downloading
        enum class [[codegen::map(openspace::RenderableFieldlinesSequence::LoadingType)]] LoadingType {
            StaticLoading,
            DynamicLoading,
            DynamicDownloading
        };
        std::optional<LoadingType> loadingType;
        // dataID that corresponds to what dataset to use if using dynamicWebContent
        std::optional<int> dataID;
        // number Of Files To Queue is a max value of the amount of files to queue up
        // so that not to big of a data set is downloaded nessesarily.
        std::optional<int> numberOfFilesToQueue;
        std::optional<std::string> infoURL;
        std::optional<std::string> dataURL;

        enum class [[codegen::map(openspace::RenderableFieldlinesSequence::SourceFileType)]] SourceFileType {
            Cdf,
            Json,
            Osfls
        };
        SourceFileType inputFileType;

        // Path to folder containing the input files
        std::optional<std::filesystem::path> sourceFolder [[codegen::directory()]];
        // Path to a .txt file containing seed points. Mandatory if CDF as input.
        // Files need time stamp in file name like so: yyyymmdd_hhmmss.txt
        std::optional<std::filesystem::path> seedPointDirectory [[codegen::directory()]];
        // Extra variables such as rho, p or t
        std::optional<std::vector<std::string>> extraVariables;
        // Which variable in CDF file to trace. b is default for fieldline
        std::optional<std::string> tracingVariable;
    };
#include "renderablefieldlinessequence_codegen.cpp"
} // namespace

namespace openspace {
const double extractTriggerTimeFromFilename(std::filesystem::path filePath);

documentation::Documentation RenderableFieldlinesSequence::Documentation() {
    return codegen::doc<Parameters>("fieldlinessequence_renderablefieldlinessequence");
}

RenderableFieldlinesSequence::RenderableFieldlinesSequence(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorGroup({ "Color" })
    , _colorMethod(ColorMethodInfo, properties::OptionProperty::DisplayType::Radio)
    , _colorQuantity(ColorQuantityInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _selectedColorRange(
        ColorMinMaxInfo,
        glm::vec2(0.f, 100.f),
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

    , _domainEnabled(DomainEnabledInfo, false)
    , _domainGroup({ "Domain" })
    , _domainX(DomainXInfo)
    , _domainY(DomainYInfo)
    , _domainZ(DomainZInfo)
    , _domainR(DomainRInfo)

    , _flowEnabled(FlowEnabledInfo, false)
    , _flowGroup({ "Flow" })
    , _flowColor(
        FlowColorInfo,
        glm::vec4(0.96f, 0.88f, 0.8f, 1.0f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _flowParticleSize(FlowParticleSizeInfo, 5, 0, 500)
    , _flowParticleSpacing(FlowParticleSpacingInfo, 60, 0, 500)
    , _flowReversed(FlowReversedInfo, false)
    , _flowSpeed(FlowSpeedInfo, 20, 0, 1000)

    , _maskingEnabled(MaskingEnabledInfo, false)
    , _maskingGroup({ "Masking" })
    , _selectedMaskingRange(
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

    _inputFileType = codegen::map<SourceFileType>(p.inputFileType);
    if (p.loadingType.has_value()) {
        _loadingType = codegen::map<LoadingType>(*p.loadingType);
    }
    else {
        _loadingType = LoadingType::StaticLoading;
    }

    if ((_loadingType == LoadingType::DynamicDownloading ||
         _loadingType == LoadingType::DynamicLoading) &&
        _inputFileType == SourceFileType::Cdf)
    {
        throw ghoul::RuntimeError(
            "Dynamic loading (or downloading) is only supported for osfls and json files"
        );
    }
    if ((_loadingType == LoadingType::StaticLoading ||
         _loadingType == LoadingType::DynamicLoading) &&
        !p.sourceFolder.has_value())
    {
        throw ghoul::RuntimeError(
            "specify dynamic downloading parameters or a syncfolder"
        );
    }

    if (p.simulationModel.has_value()) {
        try {
            _model = codegen::map<openspace::fls::Model>(*p.simulationModel);
        }
        catch (const std::exception&) {
            _model = fls::Model::Invalid;
        }
    }
    else {
        _model = fls::Model::Invalid;
    }

    //maybe this should be called even if model is invalid
    setModelDependentConstants();

    // setting scaling factor after model to support unknown model (model = invalid, but
    // scaling factor specified.
    _scalingFactor = p.scaleToMeters.value_or(_scalingFactor);

    if (_loadingType == LoadingType::DynamicDownloading) {
        setupDynamicDownloading(p.dataID, p.numberOfFilesToQueue, p.infoURL, p.dataURL);
    }
    else {
        ghoul_assert(
            p.sourceFolder.has_value(),
            "sourceFolder not specified though it should not be able to get here"
        );
        std::filesystem::path path = p.sourceFolder.value();
        namespace fsm = std::filesystem;
        for (const fsm::directory_entry& e : fsm::directory_iterator(path)) {
            if (!e.is_regular_file()) {
                continue;
            }
            File file;
            file.path = e.path();
            file.status = File::FileStatus::Downloaded;
            file.timestamp = -1.0;
            _files.push_back(std::move(file));
            if (_files[0].path.empty()) {
                throw ghoul::RuntimeError(
                    "path didnt transfer"
                );
            }
        }
    }

    _extraVars = p.extraVariables.value_or(_extraVars);
    _flowEnabled = p.flowEnabled.value_or(_flowEnabled);
    _flowColor = p.flowColor.value_or(_flowColor);
    _flowReversed = p.reversedFlow.value_or(_flowReversed);
    _flowParticleSize = p.particleSize.value_or(_flowParticleSize);
    _flowParticleSpacing = p.particleSpacing.value_or(_flowParticleSpacing);
    _flowSpeed = p.flowSpeed.value_or(_flowSpeed);
    _maskingEnabled = p.maskingEnabled.value_or(_maskingEnabled);
    _maskingQuantityTemp = p.maskingQuantity.value_or(_maskingQuantityTemp);
    _domainEnabled = p.domainEnabled.value_or(_domainEnabled);
    _lineWidth = p.lineWidth.value_or(_lineWidth);
    _colorABlendEnabled = p.alphaBlendingEnabled.value_or(_colorABlendEnabled);
    _renderForever = p.showAtAllTimes;
    _manualTimeOffset = p.manualTimeOffset.value_or(_manualTimeOffset);

    if (_loadingType == LoadingType::StaticLoading){
        staticallyLoadFiles(p.seedPointDirectory, p.tracingVariable);
        computeSequenceEndTime();
    }
    // Color group
    _colorTablePath = FieldlinesSequenceModule::DefaultTransferFunctionFile.string();
    if (p.colorTablePaths.has_value()) {
        for (auto path : *p.colorTablePaths) {
            if (std::filesystem::exists(path)) {
                _colorTablePaths.emplace_back(path);
            }
            else {
                _colorTablePaths.emplace_back(
                    FieldlinesSequenceModule::DefaultTransferFunctionFile
                );
                LERROR(std::format(
                    "Color table path {} is not a valid file.",
                    "Used a default transferfunction instead.",
                    path
                ));
            }
        }
    }
    if (!p.colorTablePaths.has_value() || _colorTablePaths.empty()) {
        _colorTablePaths.emplace_back(
            FieldlinesSequenceModule::DefaultTransferFunctionFile
        );
    }
    _colorUniform = p.color.value_or(_colorUniform);
    _colorMethod.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");
    _colorMethod.addOption(static_cast<int>(ColorMethod::ByQuantity), "By Quantity");
    if (p.colorMethod.has_value()) {
        _colorMethod = static_cast<int>(
            codegen::map<openspace::RenderableFieldlinesSequence::ColorMethod>(
                *p.colorMethod
            )
        );
    }
    else {
        _colorMethod = static_cast<int>(ColorMethod::Uniform);
    }
    _colorQuantityTemp = p.colorQuantity.value_or(_colorQuantityTemp);
    _colorQuantity.addOption(-1, "dummy_default");
    _colorQuantity = -1;

    if (p.colorTableRanges.has_value()) {
        _colorTableRanges = *p.colorTableRanges;
        // This causes error since colorTableRanges has not had its option assigned to
        // it yet:
        //_selectedColorRange = _colorTableRanges[_colorQuantityTemp];
    }
    else {
        _colorTableRanges.push_back(glm::vec2(0.f, 1.f));
        _selectedColorRange = glm::vec2(0.f, 1.f);
    }

    if (p.colorMinMaxRange.has_value()) {
        _selectedColorRange.setMinValue(glm::vec2(p.colorMinMaxRange->x));
        _selectedColorRange.setMaxValue(glm::vec2(p.colorMinMaxRange->y));
    }
    // To not change peoples masking settings i kept the parameter for the assets
    // to be "MaskingRanges", but a single vec2-value instead of a vector.
    // What is given from the asset, is stored as the selected range.
    _maskingRanges = p.maskingRanges.value_or(_maskingRanges);
    if (!_maskingRanges.empty()) {
        _selectedMaskingRange = _maskingRanges[_maskingQuantityTemp];
    }

    if (p.maskingMinMaxRange.has_value()) {
        _selectedMaskingRange.setMinValue(glm::vec2(p.maskingMinMaxRange->x));
        _selectedMaskingRange.setMaxValue(glm::vec2(p.maskingMinMaxRange->y));
    }
}

void RenderableFieldlinesSequence::setupDynamicDownloading(
                                           const std::optional<int>& dataID,
                                           const std::optional<int>& numberOfFilesToQueue,
                                           const std::optional<std::string>& infoURL,
                                           const std::optional<std::string>& dataURL)
{
    _dataID = dataID.value_or(_dataID);
    if (!_dataID) {
        throw ghoul::RuntimeError(
            "If running with dynamic downloading, dataID needs to be specified"
        );
    }
    _nOfFilesToQueue = numberOfFilesToQueue.value_or(_nOfFilesToQueue);
    _infoURL = infoURL.value();
    if (_infoURL.empty()) { throw ghoul::RuntimeError("InfoURL has to be provided"); }
    _dataURL = dataURL.value();
    if (_dataURL.empty()) { throw ghoul::RuntimeError("DataURL has to be provided"); }
    _dynamicFileDownloader = std::make_unique<DynamicFileSequenceDownloader>(
        _dataID, _infoURL, _dataURL, _nOfFilesToQueue
    );
}

void RenderableFieldlinesSequence::staticallyLoadFiles(
                           const std::optional<std::filesystem::path>& seedPointDirectory,
                           const std::optional<std::string>& tracingVariable)
{
    std::vector<std::thread> openThreads;
    for (File& file : _files) {
        bool loadSuccess = false;
        switch (_inputFileType) {
            case SourceFileType::Cdf: {
                _seedPointDirectory = seedPointDirectory.value_or(_seedPointDirectory);
                _tracingVariable = tracingVariable.value_or(_tracingVariable);
                std::vector<std::string> extraMagVars =
                    fls::extractMagnitudeVarsFromStrings(_extraVars);
                std::unordered_map<std::string, std::vector<glm::vec3>> seedsPerFiles =
                    fls::extractSeedPointsFromFiles(_seedPointDirectory);
                if (seedsPerFiles.empty()) {
                    LERROR("No seed files found");
                }
                loadSuccess = fls::convertCdfToFieldlinesState(
                    file.state,
                    file.path.string(),
                    seedsPerFiles,
                    _manualTimeOffset,
                    _tracingVariable,
                    _extraVars,
                    extraMagVars
                );
                break;
            }
            case SourceFileType::Json:
                loadSuccess =
                    file.state.loadStateFromJson(
                        file.path.string(), _model, _scalingFactor
                    );
                break;
            case SourceFileType::Osfls: {
                std::thread thread = loadFile(file);
                openThreads.push_back(std::move(thread));
                // loadSuccess = true;
                break;
            }
            default:
                break;
        }
        //if (loadSuccess) {
        //    file.status = File::FileStatus::Loaded;
        //    file.timestamp = extractTriggerTimeFromFilename(file.path);
        //}
        //else {
        //    LERROR(fmt::format("Failed to load state from: {}", file.path));
        //}
    }

    for (std::thread& thread : openThreads) {
        thread.join();
    }
    _isLoadingStateFromDisk = false;
    for (auto& file : _files) {
        if (!file.path.empty() && file.status != File::FileStatus::Loaded) {
            file.status = File::FileStatus::Loaded;
            file.timestamp = extractTriggerTimeFromFilename(file.path);
            _atLeastOneFileLoaded = true;
        }
    }
    std::sort(_files.begin(), _files.end());
}

void RenderableFieldlinesSequence::initialize() {
// not needed here because color path (singular) will be set in firstupdate()
//    _transferFunction = std::make_unique<TransferFunction>(
//        absPath(_colorTablePath).string()
//    );
}

void RenderableFieldlinesSequence::initializeGL() {
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "FieldlinesSequenceNew",
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_vs.glsl"),
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_fs.glsl")
    );
    setupProperties();
    definePropertyCallbackFunctions();

    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
    glGenBuffers(1, &_vertexMaskingBuffer);

    // Needed for additive blending
    setRenderBin(Renderable::RenderBin::Overlay);
}

void RenderableFieldlinesSequence::setupProperties() {
    addProperty(_colorABlendEnabled);
    addProperty(_lineWidth);
    addProperty(_jumpToStartBtn);

    // Add Property Groups
    addPropertySubOwner(_colorGroup);
    addPropertySubOwner(_domainGroup);
    addPropertySubOwner(_flowGroup);
    addPropertySubOwner(_maskingGroup); // hasExtra

    _colorUniform.setViewOption(properties::Property::ViewOptions::Color);
    _colorGroup.addProperty(_colorUniform);
    _colorGroup.addProperty(_colorMethod);
    _colorGroup.addProperty(_colorQuantity);// hasExtra
    _selectedColorRange.setViewOption(
        properties::Property::ViewOptions::MinMaxRange
    );
    _colorGroup.addProperty(_selectedColorRange);
    _colorGroup.addProperty(_colorTablePath);

    _domainGroup.addProperty(_domainEnabled);
    _domainGroup.addProperty(_domainX);
    _domainGroup.addProperty(_domainY);
    _domainGroup.addProperty(_domainZ);
    _domainGroup.addProperty(_domainR);

    _flowGroup.addProperty(_flowEnabled);
    _flowGroup.addProperty(_flowReversed);
    _flowColor.setViewOption(properties::Property::ViewOptions::Color);
    _flowGroup.addProperty(_flowColor);
    _flowGroup.addProperty(_flowParticleSize);
    _flowGroup.addProperty(_flowParticleSpacing);
    _flowGroup.addProperty(_flowSpeed);

    _maskingGroup.addProperty(_maskingEnabled); //hasExtra
    _maskingGroup.addProperty(_maskingQuantity);
    _selectedMaskingRange.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    _maskingGroup.addProperty(_selectedMaskingRange);
}

void RenderableFieldlinesSequence::definePropertyCallbackFunctions() {
    _colorQuantity.onChange([this]() {
        if (_colorTablePaths.size() == 0){
            return;
        }
        _shouldUpdateColorBuffer = true;
        //_selectedColorRange not needed to be set in constructor, due to this onChnage is
        // declared before firstupdate() function that sets _colorQuantity.
        if (_colorTableRanges.size() > _colorQuantity) {
            _selectedColorRange = _colorTableRanges[_colorQuantity];
        }
        // If fewer data ranges to be colored per parameter is given than
        // there are parameters in the data.
        // This would be the case where a better structure would be needed, because
        // it creates descrepancy which range belongs to which parameter
        else {
            _selectedColorRange = _colorTableRanges[0];
        }
        if (_colorTablePaths.size() > _colorQuantity) {
            _colorTablePath = _colorTablePaths[_colorQuantity].string();
        }
        else {
            _colorTablePath = _colorTablePaths[0].string();
        }
    });

    // This is to save the changes done in the gui for when you switch between options
    _selectedColorRange.onChange([this]() {
        _colorTableRanges[_colorQuantity] = _selectedColorRange;
    });

    _colorTablePath.onChange([this]() {
        std::filesystem::path newPath = absPath(_colorTablePath);

        if (std::filesystem::exists(newPath)) {
            _transferFunction = std::make_unique<TransferFunction>(newPath.string());
        }
        else
        {
            LWARNING("Invalid path to transferfunction, please enter new path.");
        }
    });

    _maskingQuantity.onChange([this]() {
        _shouldUpdateMaskingBuffer = true;
        _havePrintedQuantityRange = false;
        if (_maskingRanges.size() > _maskingQuantity) {
            _selectedMaskingRange= _maskingRanges[_maskingQuantity];
        }
        else {
            _selectedMaskingRange = _maskingRanges[0];
        }
    });

    _jumpToStartBtn.onChange([this]() {
        if (_atLeastOneFileLoaded) {
            global::timeManager->setTimeNextFrame(Time(_files[0].timestamp));
        }
    });
}

void RenderableFieldlinesSequence::setModelDependentConstants() {
    float limit = 100.f; // Just used as a default value.
    switch (_model) {
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

const double extractTriggerTimeFromFilename(std::filesystem::path filePath) {
    // number of  characters in filename (excluding '.osfls')
    //constexpr int FilenameSize = 23;
    // size(".osfls")
    //std::string wholefileName = filePath.filename().string();
    //std::string extension = filePath.extension().string();
    //int ExtSize = extension.length(); // 6 for .osfls
    std::string fileName = filePath.stem().string(); // excludes extention

    // Ensure the separators are correct
    fileName.replace(4, 1, "-");
    fileName.replace(7, 1, "-");
    fileName.replace(13, 1, ":");
    fileName.replace(16, 1, ":");
    fileName.replace(19, 1, ".");
    return Time::convertTime(fileName);
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
            LWARNING("Currently downloading file, exiting might take longer than usual");
            printedWarning = true;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(5));
    }
}

void RenderableFieldlinesSequence::computeSequenceEndTime() {
    if (_files.size() == 0) {
        _sequenceEndTime = 0.f;
    }
    else if (_files.size() == 1) {
        _sequenceEndTime = _files[0].timestamp + 3600.f;
        if (_loadingType == LoadingType::StaticLoading && !_renderForever) {
            //TODO: Alternativly check at construction and throw exeption.
            LWARNING("Only one file in data set, but ShowAtAllTimes set to false. "
                "Using arbitrary duration to visualize data file instead");
        }
    }
    else if (_files.size() > 1) {
        const double lastTriggerTime = _files.back().timestamp;
        const double sequenceDuration = lastTriggerTime - _files[0].timestamp;
        const double averageStateDuration = sequenceDuration / (_files.size() - 1);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    }
}

std::thread RenderableFieldlinesSequence::loadFile(File& file) {
    _isLoadingStateFromDisk = true;
//    if (_inputFileType == SourceFileType::Osfls) {
        std::thread readFileThread([this, &file]() {
            try {
                //std::lock_guard<std::mutex> lock(_mutex);
                file.state = FieldlinesState::createStateFromOsfls(file.path.string());
                //const bool success = file.state.loadStateFromOsfls(file.path.string());
                //file.status = File::FileStatus::Loaded;
            }
            catch(const std::exception& e ) {
                LERROR(e.what());
            }
        });
        return readFileThread;
//    }

    // So far uncleare why model is needed and if scalingFactor can be changed afterwards.
    // If these 2 things can be desided afterwards, without affecting Ganamyde fieldlines
    // that are using json input, then changes to fieldlinesstate function
    // loadStateFromJson can be changed. That will result in the option to dynamicaly
    // load and download json files as well.
    //
    // I don't think it matters if model is invalid and scalingFactor is 1.f when loading
    // json files anyway.

    //else if (_inputFileType == SourceFileType::Json) {
    //    std::thread readFileThread([this, &file, &success]() {
    //        success = file.state.loadStateFromJson(
    //            file.path.string(),
    //            fls::Model::Invalid,
    //            _scalingFactor
    //        );
    //    });
    //}

}

std::vector<RenderableFieldlinesSequence::File>::iterator
RenderableFieldlinesSequence::insertToFilesInOrder(File& file) {
    const std::vector<File>::const_iterator iter = std::upper_bound(
        _files.begin(), _files.end(),
        file.timestamp,
        [](double timeRef, const File& fileRef) {
            return timeRef < fileRef.timestamp;
        }
    );
    auto iterToInserted = _files.insert(iter, std::move(file));
    return iterToInserted;
}

int RenderableFieldlinesSequence::updateActiveIndex(const double nowTime) {
    if (_files.empty()) return -1;
    // if == nowTime, sets correct index if exactly the same
    // if size == 1 at this point, we can expect to not have a sequence and wants to show
    // the one files fieldlines at any point in time
    if (_files.begin()->timestamp == nowTime || _files.size() == 1) return 0;

    int index = 0;
    const std::vector<File>::const_iterator iter = std::upper_bound(
        _files.begin(), _files.end(), nowTime, [](double timeRef, const File& fileRef) {
            return timeRef < fileRef.timestamp;
        }
    );

    if (iter == _files.begin()) {
        index = 0;
    }
    else if (iter != _files.end()) {
        index = static_cast<int>(std::distance(_files.cbegin(), iter)) -1;
    }
    else {
        index = static_cast<int>(_files.size()) - 1;
    }
    return index;
}

bool RenderableFieldlinesSequence::isReady() const {
    return _shaderProgram != nullptr;
}

void RenderableFieldlinesSequence::updateDynamicDownloading(const double currentTime,
                                                                   const double deltaTime)
{
    _dynamicFileDownloader->update(currentTime, deltaTime);
    const std::vector<std::filesystem::path>& filesToRead =
        _dynamicFileDownloader->downloadedFiles();
    for (std::filesystem::path filePath : filesToRead) {
        File newFile;
        newFile.path = filePath;
        newFile.status = File::FileStatus::Downloaded;
        const double time = extractTriggerTimeFromFilename(filePath.filename());
        newFile.timestamp = time;

        auto inserted = insertToFilesInOrder(newFile);
    }

    // if all files are moved into _sourceFiles then we can
    // empty the DynamicFileSequenceDownloader _downloadedFiles;
    _dynamicFileDownloader->clearDownloaded();
}

void RenderableFieldlinesSequence::firstUpdate() {
    std::vector<File>::iterator file =
        std::find_if( _files.begin(), _files.end(), [](File& f) {
                return f.status == File::FileStatus::Loaded;
        });
    if (file == _files.end()) return;
    const std::vector<std::vector<float>>& quantities = file->state.extraQuantities();
    const std::vector<std::string>& extraNamesVec =
        file->state.extraQuantityNames();
    for (int i = 0; i < quantities.size(); ++i) {
        _colorQuantity.addOption(i, extraNamesVec[i]);
        _maskingQuantity.addOption(i, extraNamesVec[i]);
    }
    _firstLoad = false;
    _colorQuantity = _colorQuantityTemp;
    _maskingQuantity = _maskingQuantityTemp;



    std::filesystem::path newPath = absPath(_colorTablePath);

    if (std::filesystem::exists(newPath)) {
        _transferFunction = std::make_unique<TransferFunction>(newPath.string());
    }
    else
    {
        LWARNING("Invalid path to transferfunction, please enter new path.");
    }

    _shouldUpdateColorBuffer = true;
    _shouldUpdateMaskingBuffer = true;

    if (!_havePrintedQuantityRange && !quantities.empty()) {
        for (int i = 0; i < extraNamesVec.size(); ++i) {
            //if not given range, use min and max of data?
            std::vector<float> q = quantities[i];
            float minNr = *std::min_element(q.begin(), q.end());
            std::string min = std::to_string(minNr);
            float maxNr = *std::max_element(q.begin(), q.end());
            std::string max = std::to_string(maxNr);
            LWARNING(std::format("min :{}", min));
            LWARNING(std::format("max :{}", max));
            std::string name = extraNamesVec[i];
            LWARNING(std::format("name:{}", name));
        }
        _havePrintedQuantityRange = true;
    }
}

void RenderableFieldlinesSequence::update(const UpdateData& data) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }
    const double currentTime = data.time.j2000Seconds();
    const double deltaTime = global::timeManager->deltaTime();

    if (_loadingType == LoadingType::DynamicDownloading) {
        updateDynamicDownloading(currentTime, deltaTime);
    }
    if (_firstLoad && _atLeastOneFileLoaded) {
        firstUpdate();
    }

    _inInterval = _files.size() > 0 &&
        currentTime >= _files[0].timestamp &&
        currentTime < _sequenceEndTime;

    if (!_inInterval && _renderForever) {
        updateActiveIndex(currentTime);
    }

    // for the sake of this if statment, it is easiest to think of activeIndex as the
    // previous index and nextIndex as the current
    const int nextIndex = _activeIndex + 1;
    // if _activeIndex is -1 but we are in interval, it means we were before the start
    //     of the sequence in the previous frame
    if (_activeIndex == -1 ||
        // if currentTime < active timestamp, it means that we stepped back to a
        // time represented by another state
        // _activeIndex has already been checked if it is <0 in the line above
        currentTime < _files[_activeIndex].timestamp ||
        // if currentTime >= next timestamp, it means that we stepped forward to a
        // time represented by another state
        (nextIndex < _files.size() && currentTime >= _files[nextIndex].timestamp) ||
        // if this case is not taken care of yet
        _files[_activeIndex].status == File::FileStatus::Downloaded)
    {
        int previousIndex = _activeIndex;
        _activeIndex = updateActiveIndex(currentTime);
        // check index again after updating
        if (_activeIndex == -1) return;
        // If we have a new index, buffers needs to update
        if (previousIndex != _activeIndex && !_firstLoad) {
            _shouldUpdateColorBuffer = true;
            _shouldUpdateMaskingBuffer = true;
        }
        // here, for DynamicLoading, the dataset is known, but files not loaded yet
        if (_files[_activeIndex].status == File::FileStatus::Downloaded) {
            // if LoadingType is StaticLoading all files will be Loaded
            // would be optimal if loading of next file would happen in the background
            std::thread t = loadFile(_files[_activeIndex]);
            t.join();
            _isLoadingStateFromDisk = false;
            _files[_activeIndex].status = File::FileStatus::Loaded;
            _files[_activeIndex].timestamp =
                extractTriggerTimeFromFilename(_files[_activeIndex].path);
            _atLeastOneFileLoaded = true;
            computeSequenceEndTime();
        }

        updateVertexPositionBuffer();
    }

    if (_shouldUpdateColorBuffer) {
        updateVertexColorBuffer();
    }

    if (_shouldUpdateMaskingBuffer) {
        updateVertexMaskingBuffer();
    }
}

void RenderableFieldlinesSequence::render(const RenderData& data, RendererTasks&) {
    if (_files.empty()) return;
    if (!_inInterval && !_renderForever) return;

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
        _transferFunction->bind();
        _shaderProgram->setUniform("colorTable", textureUnit);
        _shaderProgram->setUniform("colorTableRange", _selectedColorRange);
    }

    if (_maskingEnabled) {
        _shaderProgram->setUniform("maskingRange", _selectedMaskingRange);
    }

    _shaderProgram->setUniform("domainLimR", _domainR.value() * _scalingFactor);
    _shaderProgram->setUniform("domainLimX", _domainX.value() * _scalingFactor);
    _shaderProgram->setUniform("domainLimY", _domainY.value() * _scalingFactor);
    _shaderProgram->setUniform("domainLimZ", _domainZ.value() * _scalingFactor);

    // Flow / Particles
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

    int loadedIndex = _activeIndex;
    if (loadedIndex > -1) {
        while (_files[loadedIndex].status != File::FileStatus::Loaded) {
            --loadedIndex;
            if (loadedIndex < 0) {
                LWARNING("no file at or before current time is loaded");
                return;
            }
        }
    }
    else return;

    const FieldlinesState& state = _files[loadedIndex].state;
    double timeTest = _files[_activeIndex].timestamp;
    ghoul_assert(timeTest != -1.0, "trying to use an empty state in render");
    glMultiDrawArrays(
        GL_LINE_STRIP,
        state.lineStart().data(),
        state.lineCount().data(),
        static_cast<GLsizei>(state.lineStart().size())
    );

    glBindVertexArray(0);
    _shaderProgram->deactivate();

    if (additiveBlending) {
        // Restores OpenGL Rendering State
        global::renderEngine->openglStateCache().resetBlendState();
        global::renderEngine->openglStateCache().resetDepthState();
    }
}

// Unbind buffers and arrays
void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableFieldlinesSequence::updateVertexPositionBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    const FieldlinesState& state = _files[_activeIndex].state;
    const std::vector<glm::vec3>& vertPos = state.vertexPositions();

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
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    const FieldlinesState& state = _files[_activeIndex].state;
    bool isSuccessful;
    const std::vector<float>& quantities = state.extraQuantity(
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

        _shouldUpdateColorBuffer = false;
    }
    unbindGL();
}

void RenderableFieldlinesSequence::updateVertexMaskingBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexMaskingBuffer);

    const FieldlinesState& state = _files[_activeIndex].state;
    bool success;
    const std::vector<float>& quantities = state.extraQuantity(
        _maskingQuantity,
        success
    );

    if (success) {
        glBufferData(
            GL_ARRAY_BUFFER,
            quantities.size() * sizeof(float),
            quantities.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(2);
        glVertexAttribPointer(2, 1, GL_FLOAT, GL_FALSE, 0, 0);

        unbindGL();
        _shouldUpdateMaskingBuffer = false;
    }
}

} // namespace openspace
