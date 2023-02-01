/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/fieldlinessequence/rendering/renderablefieldlinessequencenew.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/documentation/documentation.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/timemanager.h>
#include <openspace/rendering/renderengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <thread>

namespace {
    constexpr std::string_view _loggerCat = "RenderableFieldlinesSequenceNew";

    constexpr openspace::properties::Property::PropertyInfo ColorMethodInfo = {
        "ColorMethod",
        "Color Method",
        "Color lines uniformly or using color tables based on extra quantities like, for "
        "examples, temperature or particle density"
    };
    constexpr openspace::properties::Property::PropertyInfo ColorQuantityInfo = {
        "ColorQuantity",
        "Quantity to Color By",
        "Quantity used to color lines if the 'By Quantity' color method is selected"
    };
    constexpr openspace::properties::Property::PropertyInfo ColorMinMaxInfo = {
        "ColorQuantityMinMax",
        "ColorTable Min Value",
        "Value to map to the lowest and highest end of the color table"
    };
    constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "ColorTablePath",
        "Path to Color Table",
        "Color Table/Transfer Function to use for 'By Quantity' coloring"
    };
    constexpr openspace::properties::Property::PropertyInfo ColorUniformInfo = {
        "Color",
        "Uniform Line Color",
        "The uniform color of lines shown when 'Color Method' is set to 'Uniform'"
    };
    constexpr openspace::properties::Property::PropertyInfo ColorUseABlendingInfo = {
        "ABlendingEnabled",
        "Additive Blending",
        "Activate/deactivate additive blending"
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

    constexpr openspace::properties::Property::PropertyInfo FlowEnabledInfo = {
        "FlowEnabled",
        "Flow Direction",
        "Toggles the rendering of moving particles along the lines. Can, for example, "
        "illustrate magnetic flow"
    };
    constexpr openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "FlowColor",
        "Flow Color",
        "Color of particles flow direction indication"
    };
    constexpr openspace::properties::Property::PropertyInfo FlowReversedInfo = {
        "Reversed",
        "Reversed Flow",
        "Toggle to make the flow move in the opposite direction"
    };
    constexpr openspace::properties::Property::PropertyInfo FlowParticleSizeInfo = {
        "ParticleSize",
        "Particle Size",
        "Size of the particles"
    };
    constexpr openspace::properties::Property::PropertyInfo FlowParticleSpacingInfo = {
        "ParticleSpacing",
        "Particle Spacing",
        "Spacing inbetween particles"
    };
    constexpr openspace::properties::Property::PropertyInfo FlowSpeedInfo = {
        "Speed",
        "Speed",
        "Speed of the flow"
    };

    constexpr openspace::properties::Property::PropertyInfo MaskingEnabledInfo = {
        "MaskingEnabled",
        "Masking",
        "Enable/disable masking. Use masking to show lines where a given quantity is "
        "within a given range, for example, if you only want to see where the "
        "temperature is between 10 and 20 degrees. Also used for masking out line "
        "topologies like solar wind & closed lines"
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingMinMaxInfo = {
        "MaskingMinLimit",
        "Lower Limit",
        "Lower and upper limit of the valid masking range"
    };
    constexpr openspace::properties::Property::PropertyInfo MaskingQuantityInfo = {
        "MaskingQuantity",
        "Quantity used for Masking",
        "Quantity used for masking"
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "This value specifies the line width of the fieldlines"
    };

    struct [[codegen::Dictionary(RenderableFieldlinesSequenceNew)]] Parameters {
        enum class [[codegen::map(openspace::RenderableFieldlinesSequenceNew::ColorMethod)]] ColorMethod {
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
        // List of ranges for which their corresponding parameters values will be
        // colorized by. Should be entered as {min value, max value} per range
        std::optional<std::vector<glm::vec2>> colorTableRanges;

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
        // Rrange for which their corresponding quantity parameter value will be
        // masked by. Should be entered as {min value, max value}
        std::optional<glm::vec2> maskingRanges;

        // [[codegen::verbatim(DomainEnabledInfo.description)]]
        std::optional<bool> domainEnabled;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // If data sets parameter start_time differ from start of run,
        // elapsed_time_in_seconds might be in relation to start of run.
        // ManuelTimeOffset will be added to trigger time.
        std::optional<double> manualTimeOffset;

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
        enum class [[codegen::map(openspace::RenderableFieldlinesSequenceNew::LoadingType)]] LoadingType {
            StaticLoading = 0,
            DynamicLoading = 1,
            DynamicDownloading = 2
        };
        std::optional<LoadingType> loadingType;
        // dataID that corresponds to what dataset to use if using dynamicWebContent
        std::optional<int> dataID;
        // number Of Files To Queue is a max value of the amount of files to queue up
        // so that not to big of a data set is downloaded nessesarily.
        std::optional<int> numberOfFilesToQueue;
        std::optional<std::string> baseURL;
        std::optional<std::string> dataURL;

        enum class [[codegen::map(openspace::RenderableFieldlinesSequenceNew::SourceFileType)]] SourceFileType {
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
#include "renderablefieldlinessequencenew_codegen.cpp"
} // namespace

namespace openspace {

std::vector<std::string>
    extractMagnitudeVarsFromStrings(std::vector<std::string> extrVars);
std::unordered_map<std::string, std::vector<glm::vec3>>
    extractSeedPointsFromFiles(std::filesystem::path);
const double extractTriggerTimeFromFilename(std::filesystem::path filePath);

documentation::Documentation RenderableFieldlinesSequenceNew::Documentation() {
    return codegen::doc<Parameters>("fieldlinessequence_renderablefieldlinessequencenew");
}

RenderableFieldlinesSequenceNew::RenderableFieldlinesSequenceNew(
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

    , _flowEnabled(FlowEnabledInfo, false)
    , _flowGroup({ "Flow" })
    , _flowColor(
        FlowColorInfo,
        glm::vec4(0.96f, 0.88f, 0.8f, 0.5f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
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
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

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
        catch (const std::exception& e) {
            _model = fls::Model::Invalid;
        }
    }
    else {
        _model = fls::Model::Invalid;
        _domainEnabled = false;
    }
    //maybe this should be called even if model is invalid
    setModelDependentConstants();

    // setting scaling factor after model to support unknown model (model = invalid, but
    // scaling factor specified.
    _scalingFactor = p.scaleToMeters.value_or(_scalingFactor);

    if (_loadingType == LoadingType::DynamicDownloading) {
        setupDynamicDownloading(p.dataID, p.numberOfFilesToQueue, p.baseURL, p.dataURL);
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
            _files.push_back(file);
        }
    }

    if (_loadingType == LoadingType::StaticLoading){
        staticallyLoadFiles(p.seedPointDirectory, p.tracingVariable);
        computeSequenceEndTime();
    }

    _extraVars = p.extraVariables.value_or(_extraVars);
    _flowEnabled = p.flowEnabled.value_or(_flowEnabled);
    _flowColor = p.flowColor.value_or(_flowColor);
    _flowReversed = p.reversedFlow.value_or(_flowReversed);
    _flowParticleSize = p.particleSize.value_or(_flowParticleSize);
    _flowParticleSpacing = p.particleSpacing.value_or(_flowParticleSpacing);
    _flowSpeed = p.flowSpeed.value_or(_flowSpeed);
    _maskingEnabled = p.maskingEnabled.value_or(_maskingEnabled);
    _maskingQuantity = p.maskingQuantity.value_or(_maskingQuantity);
    _domainEnabled = p.domainEnabled.value_or(_domainEnabled);
    _lineWidth = p.lineWidth.value_or(_lineWidth);
    _manualTimeOffset = p.manualTimeOffset.value_or(_manualTimeOffset);

    // Color group
    if (p.colorTablePaths.has_value()) {
        _colorTablePaths = p.colorTablePaths.value();
    }
    _colorUniform = p.color.value_or(_colorUniform);
    _colorMethod.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");
    _colorMethod.addOption(static_cast<int>(ColorMethod::ByQuantity), "By Quantity");
    if (p.colorMethod.has_value()) {
        _colorMethod = static_cast<int>(
            codegen::map<openspace::RenderableFieldlinesSequenceNew::ColorMethod>(
                *p.colorMethod
            )
        );
    }
    else {
        _colorMethod = static_cast<int>(ColorMethod::Uniform);
    }

    if (p.colorQuantity.has_value()) {
        _colorMethod = static_cast<int>(ColorMethod::ByQuantity);
    }

    if (p.colorTableRanges.has_value()) {
        _colorTableRanges = *p.colorTableRanges;
    }
    else {
        _colorTableRanges.push_back(glm::vec2(0.f, 1.f));
    }

    // Sorry about the confusion here with the masking. To not change peoples masking
    // settings i kept the parameter for the assets to be "MaskingRanges", but a single
    // vec2-value instead of a vector. What is given from the asset, is stored as the
    // selected range.
    if (p.maskingRanges.has_value()) {
        _selectedMaskingRanges = *p.maskingRanges;
    }
    else {
        _maskingRanges.push_back(glm::vec2(-100000.f, 100000.f)); // some default values
    }
}

void RenderableFieldlinesSequenceNew::setupDynamicDownloading(
                                           const std::optional<int>& dataID,
                                           const std::optional<int>& numberOfFilesToQueue,
                                           const std::optional<std::string>& baseURL,
                                           const std::optional<std::string>& dataURL)
{
    _dataID = dataID.value_or(_dataID);
    if (!_dataID) {
        throw ghoul::RuntimeError(
            "If running with dynamic downloading, dataID needs to be specified"
        );
    }
    _nOfFilesToQueue = numberOfFilesToQueue.value_or(_nOfFilesToQueue);
    _baseURL = baseURL.value();
    if (_baseURL.empty()) { throw ghoul::RuntimeError("baseURL has to be provided"); }
    _dataURL = dataURL.value();
    if (_dataURL.empty()) { throw ghoul::RuntimeError("dataURL has to be provided"); }
    _dynamicdownloaderManager = std::make_unique<DynamicDownloaderManager>(
        _dataID, _baseURL, _dataURL, _nOfFilesToQueue
    );
}

void RenderableFieldlinesSequenceNew::staticallyLoadFiles(
                           const std::optional<std::filesystem::path>& seedPointDirectory,
                           const std::optional<std::string>& tracingVariable)
{
    std::vector<std::string> extraMagVars;
    std::unordered_map<std::string, std::vector<glm::vec3>> seedsPerFiles;

    if (_inputFileType == SourceFileType::Cdf) {
        _seedPointDirectory = seedPointDirectory.value_or(_seedPointDirectory);
        _tracingVariable = tracingVariable.value_or(_tracingVariable);
        extraMagVars = extractMagnitudeVarsFromStrings(_extraVars);
        seedsPerFiles = extractSeedPointsFromFiles(_seedPointDirectory);
        if (seedsPerFiles.empty()) {
            LERROR("No seed files found");
        }
    }

    for (File& file : _files) {
        bool loadSuccess = false;
        switch (_inputFileType) {
            case SourceFileType::Cdf:
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
            case SourceFileType::Json:
                loadSuccess =
                    file.state.loadStateFromJson(
                        file.path.string(), _model, _scalingFactor
                    );
                file.timestamp = extractTriggerTimeFromFilename(file.path);
                break;
            case SourceFileType::Osfls:
                loadFile(file);
                break;
            default:
                break;
        }
        if (loadSuccess) {
            file.status = File::FileStatus::Loaded;
        }
        else {
            LERROR(fmt::format("Failed to load state from: {}", file.path));
        }
    }
    std::sort(_files.begin(), _files.end());
    if (!_files.empty()) {
        _atLeastOneFileLoaded = true;
    }
}

std::vector<std::string>
extractMagnitudeVarsFromStrings(std::vector<std::string> extrVars)
{
    std::vector<std::string> extraMagVars;
    for (int i = 0; i < static_cast<int>(extrVars.size()); i++) {
        const std::string& str = extrVars[i];
        // Check if string is in the format specified for magnitude variables
        // As of now, unknown why parameters |( )| where used, or if it was used
        if (str.substr(0, 2) == "|(" ) {
            throw ghoul::RuntimeError(
                "The formating of specifying extra variables no longer requires |( )|"
            );
        }

        std::istringstream ss(str);
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
        extrVars.erase(extrVars.begin() + i);
        i--;
    }
    return extraMagVars;
}

std::unordered_map<std::string, std::vector<glm::vec3>>
extractSeedPointsFromFiles(std::filesystem::path filePath)
{
    std::unordered_map<std::string, std::vector<glm::vec3>> outMap;

    if (!std::filesystem::is_directory(filePath)) {
        LERROR(fmt::format(
            "The specified seed point directory: '{}' does not exist", filePath
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

        if (outVec.empty()) {
            LERROR(fmt::format("Found no seed points in: {}", seedFilePath));
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

void RenderableFieldlinesSequenceNew::initialize() {
    if (_colorTablePaths.empty()) {
        _colorMethod = static_cast<int>(ColorMethod::Uniform);
    }
    else {
        _transferFunction = std::make_unique<TransferFunction>(
            absPath(_colorTablePaths[0]).string()
        );
    }
}

void RenderableFieldlinesSequenceNew::initializeGL() {
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "FieldlinesSequenceNew",
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_vs.glsl"),
        absPath("${MODULE_FIELDLINESSEQUENCE}/shaders/fieldlinessequence_fs.glsl")
    );

    setupProperties();

    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
    glGenBuffers(1, &_vertexMaskingBuffer);

    // Needed for additive blending
    setRenderBin(Renderable::RenderBin::Overlay);
}

void RenderableFieldlinesSequenceNew::setupProperties() {
    addProperty(_colorABlendEnabled);
    addProperty(_domainEnabled);
    addProperty(_flowEnabled);
    addProperty(_maskingEnabled); //hasExtra
    addProperty(_lineWidth);

    // Add Property Groups
    addPropertySubOwner(_colorGroup);
    addPropertySubOwner(_domainGroup);
    addPropertySubOwner(_flowGroup);
    addPropertySubOwner(_maskingGroup); // hasExtra

    _colorUniform.setViewOption(properties::Property::ViewOptions::Color);
    _colorGroup.addProperty(_colorUniform);
    _colorGroup.addProperty(_colorMethod);
    _colorGroup.addProperty(_colorQuantity);// hasExtra
    _colorQuantityMinMax.setViewOption(
        properties::Property::ViewOptions::MinMaxRange
    );
    _colorGroup.addProperty(_colorQuantityMinMax);
    //_colorGroup.addProperty(_colorTablePaths[0].c_str());

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

    _maskingGroup.addProperty(_maskingQuantity);
    _maskingMinMax.setViewOption(properties::Property::ViewOptions::MinMaxRange);// hasExtra
    _maskingGroup.addProperty(_maskingMinMax);// hasExtra

    //if (!_maskingRanges.empty()) {
        _maskingMinMax = _maskingRanges[_maskingQuantity];// hasExtra
    //}

}

void RenderableFieldlinesSequenceNew::definePropertyCallbackFunctions() {

    _colorQuantity.onChange([this]() {
        _transferFunction->setPath(_colorTablePaths[_colorQuantity]);
    });

    _maskingQuantity.onChange([this]() {
        _shouldUpdateMaskingBuffer = true;
        _maskingMinMax = _maskingRanges[_maskingQuantity];
    });

    _maskingMinMax.onChange([this]() {
        _maskingRanges[_maskingQuantity] = _maskingMinMax;
    });
}

void RenderableFieldlinesSequenceNew::setModelDependentConstants() {
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

void RenderableFieldlinesSequenceNew::deinitializeGL() {
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

void RenderableFieldlinesSequenceNew::computeSequenceEndTime() {
    if (_files.size() > 1) {
        const double lastTriggerTime = _files.back().timestamp;
        const double sequenceDuration = lastTriggerTime - _files[0].timestamp;
        const double averageStateDuration = sequenceDuration / (_files.size() - 1);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    }
}

void RenderableFieldlinesSequenceNew::loadFile(File& file) {
    _isLoadingStateFromDisk = true;
    if (_inputFileType == SourceFileType::Osfls) {
        std::thread readFileThread([this, &file]() {
            try {
                file.state = FieldlinesState::createStateFromOsfls(file.path.string());
                //const bool success = file.state.loadStateFromOsfls(file.path.string());
                file.status = File::FileStatus::Loaded;
                file.timestamp = extractTriggerTimeFromFilename(file.path);
                _atLeastOneFileLoaded = true;
                _isLoadingStateFromDisk = false;
            }
            catch(const std::exception& e ) {
                LERROR(e.what());
            }
        });
    }

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
std::vector<RenderableFieldlinesSequenceNew::File>::iterator
RenderableFieldlinesSequenceNew::insertToFilesInOrder(File file) {
    auto iter = std::upper_bound(_files.begin(), _files.end(), file);
    auto iterToInserted = _files.insert(iter, std::move(file));
    return iterToInserted;
}

int RenderableFieldlinesSequenceNew::updateActiveIndex(const double nowTime) {
    if (_files.empty()) return -1;
    // if == nowTime, sets correct index if exactly the same
    // if size == 1 at this point, we can expect to not have a sequence and wants to show
    // the one files fieldlines at any point in time
    if (_files.begin()->timestamp == nowTime || _files.size() == 1) return 0;
    int index = 0;

    auto iter = std::upper_bound(
        _files.begin(), _files.end(), nowTime, [](double t, const File& f) {
            return f.timestamp < t;
        }
    );
    if (iter != _files.end()) {
        index = static_cast<int>(std::distance(_files.begin(), iter)) -1;
    }
    else {
        index = static_cast<int>(_files.size()) - 1;
    }
    return index;
}

bool RenderableFieldlinesSequenceNew::isReady() const {
    return true; // _shaderProgram != nullptr;
}

void RenderableFieldlinesSequenceNew::updateStaticLoading(const double currentTime,
                                                                   const double deltaTime)
{

}
void RenderableFieldlinesSequenceNew::updateDynamicLoading(const double currentTime,
                                                                   const double deltaTime)
{

}
void RenderableFieldlinesSequenceNew::updateDynamicDownloading(const double currentTime,
                                                                   const double deltaTime)
{
    _dynamicdownloaderManager->update(currentTime, deltaTime);
    const std::vector<std::filesystem::path>& filesToRead =
        _dynamicdownloaderManager->downloadedFiles();
    for (std::filesystem::path filePath : filesToRead) {
        File newFile;
        newFile.path = filePath;
        newFile.status = File::FileStatus::Downloaded;
        const double time = extractTriggerTimeFromFilename(filePath.filename());
        newFile.timestamp = time;

        auto inserted = insertToFilesInOrder(std::move(newFile));
    }

    // if all files are moved into _sourceFiles then we can
    // empty the DynamicDownloaderManager _downloadedFiles;
    _dynamicdownloaderManager->clearDownloaded();
}

void RenderableFieldlinesSequenceNew::update(const UpdateData& data) {
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }
    const double currentTime = data.time.j2000Seconds();
    const double deltaTime = global::timeManager->deltaTime();

    switch (_loadingType) {
        case LoadingType::StaticLoading:
            updateStaticLoading(currentTime, deltaTime);
            break;
        case LoadingType::DynamicLoading:
            updateDynamicLoading(currentTime, deltaTime);
            break;
        case LoadingType::DynamicDownloading:
            updateDynamicDownloading(currentTime, deltaTime);
            break;
        default:
            break;
    }
    computeSequenceEndTime();

    if (_firstLoad && _atLeastOneFileLoaded) {
        size_t nExtraQuantities;
            // loop just to find the first one that is loaded, then it will break out
        std::vector<File>::iterator f;
        for (f = _files.begin(); f != _files.end(); f++) {
            if (f->status != File::FileStatus::Loaded) {
                continue;
            }
            nExtraQuantities = f->state.nExtraQuantities();
            const std::vector<std::string>& extraNamesVec =
                f->state.extraQuantityNames();
            for (int i = 0; i < static_cast<int>(nExtraQuantities); ++i) {
                _colorQuantity.addOption(i, extraNamesVec[i]);
                _maskingQuantity.addOption(i, extraNamesVec[i]);
            }
            break;
        }
        _firstLoad = false;

        _transferFunction = std::make_unique<TransferFunction>(
            absPath(_colorTablePaths[0]).string()
        );

        /////////////////////////
        bool success;
        const std::vector<std::vector<float>>& quantities = f->state.extraQuantities();
        for (auto toMask : quantities) {
            float fmin = *std::min_element(toMask.begin(), toMask.end());
            std::string smin = std::to_string(fmin);
            float fmax = *std::max_element(toMask.begin(), toMask.end());
            std::string smax = std::to_string(fmax);
            LWARNING(fmt::format("min :{}", smin));
            LWARNING(fmt::format("max :{}", smax));
            ////////////////////////
            _maskingRanges.push_back({ fmin, fmax });
        }

        _colorTableRanges.resize(nExtraQuantities, _colorTableRanges.back());
        _maskingRanges.resize(nExtraQuantities, _maskingRanges.back());
    }

    bool isInInterval = _files.size() > 0 &&
        currentTime >= _files[0].timestamp &&
        currentTime < _sequenceEndTime;
    if (_files.size() == 1 && _loadingType == LoadingType::StaticLoading) {
        isInInterval = true;
    }
    if (!isInInterval) {
        return;
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
        (nextIndex < _files.size() && currentTime >= _files[nextIndex].timestamp))
    {
        _activeIndex = updateActiveIndex(currentTime);
        // check again after updating
        if (_activeIndex == -1) return;
        if (_files[_activeIndex].status == File::FileStatus::Downloaded) {
            // if LoadingType is StaticLoading all files will be Loaded
            loadFile(_files[_activeIndex]);
            computeSequenceEndTime();
        }

    }

//    if (_shouldUpdateColorBuffer) {
        updateVertexColorBuffer();
//    }

//    if (_shouldUpdateMaskingBuffer) {
        updateVertexMaskingBuffer();
//    }
        updateVertexPositionBuffer();
}

void RenderableFieldlinesSequenceNew::render(const RenderData& data, RendererTasks&) {
    if (_files.empty()) {
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
        _transferFunction->bind();
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

void RenderableFieldlinesSequenceNew::updateVertexPositionBuffer() {
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

void RenderableFieldlinesSequenceNew::updateVertexColorBuffer() {
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

        unbindGL();
    }
}

void RenderableFieldlinesSequenceNew::updateVertexMaskingBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexMaskingBuffer);

    const FieldlinesState& state = _files[_activeIndex].state;
    bool success;
    const std::vector<float>& maskings = state.extraQuantity(
        _maskingQuantity,
        success
    );

    float i = *std::min_element(maskings.begin(), maskings.end());
    std::string min = std::to_string(i);
    float a = *std::max_element(maskings.begin(), maskings.end());
    std::string max = std::to_string(a);
    LWARNING(fmt::format("min :{}", min));
    LWARNING(fmt::format("max :{}", max));
    const std::vector<std::string>& names = state.extraQuantityNames();
    std::string name = names[_maskingQuantity];
    LWARNING(fmt::format("name:{}", name));

    if (success) {
        glBufferData(
            GL_ARRAY_BUFFER,
            maskings.size() * sizeof(float),
            maskings.data(),
            GL_STATIC_DRAW
        );

        glEnableVertexAttribArray(2);
        glVertexAttribPointer(2, 1, GL_FLOAT, GL_FALSE, 0, 0);
    }
    unbindGL();
}

} // namespace openspace
