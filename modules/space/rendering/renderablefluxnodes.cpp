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

#include <modules/space/rendering/renderablefluxnodes.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <openspace/query/query.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>
#include <functional>
#include <optional>
#include <sys/stat.h>
#include <thread>

namespace {
    constexpr std::string_view _loggerCat = "RenderableFluxNodes";

    constexpr std::array<const char*, 29> UniformNames = {
        "streamColor", "nodeSize", "proximityNodesSize",
        "thresholdFlux", "colorMode", "filterLower", "filterUpper", "scalingMode",
        "colorTableRange", "domainLimZ", "nodeSkip", "nodeSkipDefault", "nodeSkipEarth",
        "nodeSkipMethod", "nodeSkipFluxThreshold", "nodeSkipRadiusThreshold",
        "fluxColorAlpha", "earthPos", "distanceThreshold", "time", "maxNodeDistanceSize",
        "usingCameraPerspective", "drawCircles", "drawHollow", "useGaussian",
        "perspectiveDistanceFactor", "minMaxNodeSize", "usingPulse",
        "usingGaussianPulse"
    };

    constexpr openspace::properties::Property::PropertyInfo GoesEnergyBinsInfo = {
        "GoesEnergy",
        "GOES Energy",
        "Select which energy bin you want to show. GOES = Geostationary Operational "
        "Environmental Satellites. Emin01 is values > 10 MeV, Default is Emin03 where "
        "values > 100 MeV.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorModeInfo = {
        "ColorMode",
        "Color Mode",
        "Color lines uniformly or using color tables based on specific values on nodes, "
        "for examples flux values.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "ColorTablePath",
        "Path to Color Table",
        "Color Table/Transfer Function to use for 'By Flux Value' coloring.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo StreamColorInfo = {
        "Color",
        "Color",
        "Color of particles.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo NodeSizeInfo = {
       "NodeSize",
       "Size of nodes",
       "Change the size of the rendered points of the nodes.",
       openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ThresholdFluxInfo = {
       "ThresholdFlux",
       "Threshold flux value",
       "This value specifies the threshold that will be changed with the flux value.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilteringInfo = {
        "FilterLower",
        "Filtering Lower Value in AU",
        "Use filtering to show nodes within a given range.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilteringUpperInfo = {
        "FilterUpper",
        "Filtering Upper Value in AU",
        "Use filtering to show nodes within a given range.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AmountofNodesInfo = {
        "AmountOfNodes",
        "Every nth node to render in",
        "Show only every nth node.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DefaultNodeSkipInfo = {
        "NodeSkip",
        "Every nth node to render default",
        "Show only every nth node outside of skippingmethod.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EarthNodeSkipInfo = {
       "NodeSkipEarth",
       "Every nth node to render close to Earth",
       "Show only every nth node outside of skippingmethod.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ScalingmethodInfo = {
        "ScalingFlux",
        "Scale the flux value with color table",
        "Use scaling to color nodes with a given method.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo NodeskipMethodInfo = {
        "SkippingNodes",
        "How to select nodes to skip",
        "Methods to select nodes to skip.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo colorTableRangeInfo = {
        "ColorTableRange",
        "Color Table Range",
        "Valid range for the color table as the exponent, with base 10, of flux values. "
        "[Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DomainZInfo = {
        "ZLimit",
        "Z-limits",
        "Valid range along the Z-axis. [Min, Max].",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FluxColorAlphaInfo = {
        "FluxColorAlpha",
        "Flux Color Alpha",
        "The value of alpha for the flux color mode.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FluxNodeskipThresholdInfo = {
        "SkippingNodesByFlux",
        "Skipping Nodes By Flux",
        "Select nodes to skip depending on flux value.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
                                                           RadiusNodeSkipThresholdInfo = {
        "SkippingNodesByRadius",
        "Skipping Nodes By Radius",
        "Select nodes to skip depending on Radius.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DistanceThresholdInfo = {
        "DistancePlanetThreshold",
        "Threshold for distance between planet",
        "Changes threshold distance for highlighting nodes close to earth.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ProximityNodesSizeInfo = {
        "ProximityNodesSize",
        "Earths Proximity Nodes Size",
        "Changes size of nodes only close to earth.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MaxNodeDistanceSizeInfo = {
        "MaxNodeDistanceSize",
        "Max Node Distance Size",
        "The maximum size of the nodes at a certin distance.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
                                                          CameraPerspectiveEnabledInfo = {
        "CameraPerspectiveEnabled",
        "Use Camera perspective",
        "Camera perspective changes the size of the nodes dependent on the distance from "
        "camera.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DrawingCirclesInfo = {
        "RenderingCircles",
        "Render as circles",
        "Using fragment shader to draw nodes as circles instead of squares.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DrawingHollowInfo = {
        "RenderingHollowCircles",
        "Render as hollow circles",
        "Using fragment shader to draw nodes as hollow circles.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo GaussiandAlphaFilterInfo = {
        "RenderingGaussianAlphaFilter",
        "Alpha by Gaussian",
        "Using fragment shader to draw nodes with Gaussian filter for alpha value.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
                                                         PerspectiveDistanceFactorInfo = {
        "PerspectiveDistanceFactor",
        "Perspective Distance factor",
        "This value decides how far away the camera must be to start impacting the node "
        "size.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MinMaxNodeSizeInfo = {
        "MinMaxNodeSize",
        "Min & Max node size",
        "The minimum and maximum node size.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo pulseEnabledInfo = {
        "PulseEnabled",
        "Nodes close to Earth pulsate",
        "Toggles the pulse for nodes close to Earth.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo gaussianPulseEnabledInfo = {
        "GaussianPulseEnabled",
        "Nodes close to Earth pulsate with alpha by gaussian",
        "Toggles the pulse with alpha by gaussian for nodes close to Earth.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableFluxNodes)]] Parameters {
        // Path to a source folder with the three binary files in it.
        std::filesystem::path sourceFolder [[codegen::directory()]];

        // [[codegen::verbatim(ColorTablePathInfo.description)]]
        std::string colorTablePath;

        // [[codegen::verbatim(GoesEnergyBinsInfo.description)]]
        std::optional<int> energyBin;

        // [[codegen::verbatim(colorTableRangeInfo.description)]]
        std::optional<glm::vec2> colorTableRange;
    };
#include "renderablefluxnodes_codegen.cpp"

} // namespace

namespace openspace {

documentation::Documentation RenderableFluxNodes::Documentation() {
    return codegen::doc<Parameters>("space_renderable_flux_nodes");
}

RenderableFluxNodes::RenderableFluxNodes(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _earthdistGroup({ "Earthfocus" })
    , _goesEnergyBins(GoesEnergyBinsInfo, properties::OptionProperty::DisplayType::Radio)
    , _styleGroup({ "Style" })
    , _colorMode(ColorModeInfo, properties::OptionProperty::DisplayType::Radio)
    , _streamColor(
        StreamColorInfo,
        glm::vec4(0.96f, 0.88f, 0.8f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _colorTablePath(ColorTablePathInfo)
    , _colorTableRange(colorTableRangeInfo, { -2.f, 4.f }, { -8.f, -8.f }, { 8.f, 8.f })
    , _fluxColorAlpha(FluxColorAlphaInfo, 1.f, 0.f, 1.f)
    , _streamGroup({ "Streams" })
    , _scalingMethod(ScalingmethodInfo, properties::OptionProperty::DisplayType::Radio)
    , _nodesAmountGroup({ "NodeGroup" })
    , _nodeSize(NodeSizeInfo, 2.f, 1.f, 10.f)
    , _distanceThreshold(DistanceThresholdInfo, 0.f, 0.f, 1.f)
    , _proximityNodesSize(ProximityNodesSizeInfo, 1.f, 0.f, 100.f)
    , _maxNodeDistanceSize(MaxNodeDistanceSizeInfo, 1.f, 1.f, 10.f)
    , _minMaxNodeSize(MinMaxNodeSizeInfo, { 2.f, 30.f }, { 1.f, 1.f }, { 10.f, 200.f })
    , _domainZ(DomainZInfo, { -2.5f, 2.5f }, { -2.5f, -2.5f }, { 2.5f, 2.5f })
    , _thresholdFlux(ThresholdFluxInfo, -1.5f, -50.f, 10.f)
    , _filteringLower(FilteringInfo, 0.f, 0.f, 5.f)
    , _filteringUpper(FilteringUpperInfo, 5.f, 0.f, 5.f)
    , _amountofNodes(AmountofNodesInfo, 1, 1, 100)
    , _nodeskipMethod(NodeskipMethodInfo, properties::OptionProperty::DisplayType::Radio)
    , _defaultNodeSkip(DefaultNodeSkipInfo, 1, 1, 100)
    , _fluxNodeskipThreshold(FluxNodeskipThresholdInfo, 0, -20, 10)
    , _earthNodeSkip(EarthNodeSkipInfo, 1, 1, 100)
    , _radiusNodeSkipThreshold(RadiusNodeSkipThresholdInfo, 0.f, 0.f, 5.f)
    , _cameraPerspectiveGroup({ "CameraPerspective" })
    , _cameraPerspectiveEnabled(CameraPerspectiveEnabledInfo, false)
    , _drawingCircles(DrawingCirclesInfo, true)
    , _drawingHollow(DrawingHollowInfo, false)
    , _gaussianAlphaFilter(GaussiandAlphaFilterInfo, false)
    , _perspectiveDistanceFactor(PerspectiveDistanceFactorInfo, 2.67f, 1.f, 20.f)
    , _pulseEnabled(pulseEnabledInfo, false)
    , _gaussianPulseEnabled(gaussianPulseEnabledInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _colorTablePath = p.colorTablePath;
    _transferFunction = std::make_unique<TransferFunction>(_colorTablePath.value());
    _colorTableRange = p.colorTableRange.value_or(_colorTableRange);

    _binarySourceFolderPath = p.sourceFolder;
    if (std::filesystem::is_directory(_binarySourceFolderPath)) {
        // Extract all file paths from the provided folder
        namespace fs = std::filesystem;
        for (const fs::directory_entry& e :
            fs::directory_iterator(_binarySourceFolderPath))
        {
            if (e.is_regular_file()) {
                _binarySourceFiles.push_back(e.path());
            }
        }
        std::sort(_binarySourceFiles.begin(), _binarySourceFiles.end());

        // Ensure that there are available and valid source files left
        if (_binarySourceFiles.empty()) {
            LERROR(std::format("'{}' contains no files", _binarySourceFolderPath));
        }
    }
    else {
        LERROR(std::format(
            "Source folder '{}' is not a valid directory", _binarySourceFolderPath
        ));
    }

    _goesEnergyBins.addOption(static_cast<int>(GoesEnergyBins::Emin01), "Emin01");
    _goesEnergyBins.addOption(static_cast<int>(GoesEnergyBins::Emin03), "Emin03");
    _colorMode.addOption(static_cast<int>(ColorMethod::ByFluxValue), "By Flux Value");
    _colorMode.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");

    _scalingMethod.addOption(static_cast<int>(ScalingMethod::Flux), "Flux");
    _scalingMethod.addOption(static_cast<int>(ScalingMethod::RFlux), "Radius * Flux");
    _scalingMethod.addOption(static_cast<int>(ScalingMethod::R2Flux), "Radius^2 * Flux");
    _scalingMethod.addOption(
        static_cast<int>(ScalingMethod::Log10RFlux), "log10(r) * Flux"
    );
    _scalingMethod.addOption(static_cast<int>(ScalingMethod::LnRFlux), "ln(r) * Flux");

    _nodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Uniform), "Uniform");
    _nodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Flux), "Flux");
    _nodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Radius), "Radius");

    if (p.energyBin.has_value()) {
        _goesEnergyBins.setValue(p.energyBin.value());
    }
    else { // default int 1 == Emin03 == MeV>100
        LINFO("Assuming default value 1, meaning Emin03");
        _goesEnergyBins.setValue(1);
    }
}

void RenderableFluxNodes::initialize() {
    populateStartTimes();
    loadNodeData(_goesEnergyBins.option().value);
    computeSequenceEndTime();
}

void RenderableFluxNodes::initializeGL() {
    // Setup shader program
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "Fluxnodes",
        absPath("${MODULE_SPACE}/shaders/fluxnodes_vs.glsl"),
        absPath("${MODULE_SPACE}/shaders/fluxnodes_fs.glsl")
    );

    _uniformCache.streamColor = _shaderProgram->uniformLocation("streamColor");
    _uniformCache.nodeSize = _shaderProgram->uniformLocation("nodeSize");
    _uniformCache.thresholdFlux = _shaderProgram->uniformLocation("thresholdFlux");

    ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, UniformNames);

    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
    glGenBuffers(1, &_vertexFilteringBuffer);

    // Needed for alpha transparency
    setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
    setupProperties();
}

void RenderableFluxNodes::definePropertyCallbackFunctions() {
    // Add Property Callback Functions
    _goesEnergyBins.onChange([this] {
        loadNodeData(_goesEnergyBins.option().value);
    });
    _colorTablePath.onChange([this]() {
        _transferFunction->setPath(_colorTablePath.value());
    });
}

void RenderableFluxNodes::loadNodeData(int energybinOption) {
    LDEBUG("Loading in binary files directly from sync folder");

    std::string energybin;
    switch (energybinOption) {
        case 0:
            energybin = "_emin01";
            break;
        case 1:
            energybin = "_emin03";
            break;
    }

    const std::string file = std::format(
        "{}/positions{}", _binarySourceFolderPath, energybin
    );
    const std::string file2 = std::format(
        "{}/fluxes/{}", _binarySourceFolderPath, energybin
    );
    const std::string file3 = std::format(
        "{}/radiuses{}", _binarySourceFolderPath, energybin
    );

    std::ifstream fileStream = std::ifstream(file, std::ifstream::binary);
    std::ifstream fileStream2 = std::ifstream(file2, std::ifstream::binary);
    std::ifstream fileStream3 = std::ifstream(file3, std::ifstream::binary);

    if (!fileStream.good()) {
        LERROR(std::format("Could not read file '{}'", file));
        return;
    }

    uint32_t nNodesPerTimestep = 0;
    fileStream.read(reinterpret_cast<char*>(&nNodesPerTimestep), sizeof(uint32_t));

    uint32_t nTimeSteps = 0;
    fileStream.read(reinterpret_cast<char*>(&nTimeSteps), sizeof(uint32_t));
    _nStates = nTimeSteps;

    if (_nStates != _startTimes.size()) {
        LERROR(
            "Number of states, _nStates, and number of start times, _startTimes, "
            "do not match"
        );
        return;
    }

    _statesColor.clear();
    _statesPos.clear();
    _statesRadius.clear();

    for (unsigned int i = 0; i < _nStates; i++) {
        _vertexPositions.resize(nNodesPerTimestep);
        fileStream.read(reinterpret_cast<char*>(
            _vertexPositions.data()), nNodesPerTimestep * sizeof(glm::vec3)
        );

        _statesPos.push_back(_vertexPositions);
        _vertexPositions.clear();
    }
    for (unsigned int i = 0; i < _nStates; i++) {
        _vertexColor.resize(nNodesPerTimestep);
        fileStream2.read(reinterpret_cast<char*>(
            _vertexColor.data()), nNodesPerTimestep * sizeof(float)
        );

        _statesColor.push_back(_vertexColor);
        _vertexColor.clear();
    }
    for (unsigned int i = 0; i < _nStates; i++) {
        _vertexRadius.resize(nNodesPerTimestep);
        fileStream3.read(reinterpret_cast<char*>(
            _vertexRadius.data()), nNodesPerTimestep * sizeof(float)
        );

        _statesRadius.push_back(_vertexRadius);
        _vertexRadius.clear();
    }
}

void RenderableFluxNodes::setupProperties() {
    addProperty(_goesEnergyBins);

    addPropertySubOwner(_styleGroup);
    addPropertySubOwner(_streamGroup);
    addPropertySubOwner(_nodesAmountGroup);
    addPropertySubOwner(_earthdistGroup);
    addPropertySubOwner(_cameraPerspectiveGroup);

    _cameraPerspectiveGroup.addProperty(_cameraPerspectiveEnabled);
    _cameraPerspectiveGroup.addProperty(_perspectiveDistanceFactor);
    _minMaxNodeSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    _cameraPerspectiveGroup.addProperty(_minMaxNodeSize);

    _earthdistGroup.addProperty(_distanceThreshold);
    _earthdistGroup.addProperty(_proximityNodesSize);
    _earthdistGroup.addProperty(_pulseEnabled);
    _earthdistGroup.addProperty(_gaussianPulseEnabled);

    _nodesAmountGroup.addProperty(_nodeskipMethod);
    _nodesAmountGroup.addProperty(_amountofNodes);
    _nodesAmountGroup.addProperty(_defaultNodeSkip);
    _nodesAmountGroup.addProperty(_earthNodeSkip);
    _nodesAmountGroup.addProperty(_nodeSize);
    _nodesAmountGroup.addProperty(_fluxNodeskipThreshold);
    _nodesAmountGroup.addProperty(_radiusNodeSkipThreshold);
    _nodesAmountGroup.addProperty(_maxNodeDistanceSize);

    _streamGroup.addProperty(_thresholdFlux);
    _streamGroup.addProperty(_filteringLower);
    _streamGroup.addProperty(_filteringUpper);
    _streamGroup.addProperty(_domainZ);

    _styleGroup.addProperty(_drawingCircles);
    _styleGroup.addProperty(_drawingHollow);
    _styleGroup.addProperty(_gaussianAlphaFilter);
    _styleGroup.addProperty(_colorMode);
    _styleGroup.addProperty(_scalingMethod);
    _colorTableRange.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    _styleGroup.addProperty(_colorTableRange);
    _styleGroup.addProperty(_colorTablePath);
    _styleGroup.addProperty(_streamColor);
    _styleGroup.addProperty(_fluxColorAlpha);

    definePropertyCallbackFunctions();
}

void RenderableFluxNodes::deinitializeGL() {
    glDeleteVertexArrays(1, &_vertexArrayObject);
    _vertexArrayObject = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    glDeleteBuffers(1, &_vertexColorBuffer);
    _vertexColorBuffer = 0;

    glDeleteBuffers(1, &_vertexFilteringBuffer);
    _vertexFilteringBuffer = 0;

    if (_shaderProgram) {
        global::renderEngine->removeRenderProgram(_shaderProgram.get());
        _shaderProgram = nullptr;
    }
}

bool RenderableFluxNodes::isReady() const {
    return _shaderProgram != nullptr;
}

void RenderableFluxNodes::populateStartTimes() {
    std::filesystem::path timeFile;
    for (const std::filesystem::path& filePath : _binarySourceFiles) {
        if (filePath.extension() == ".csv" || filePath.extension() == ".dat" ||
            filePath.extension() == ".txt")
        {
            timeFile = filePath;
            break;
        }

        const std::string f = filePath.string();
        // if no file extention but word "time" in file name
        if (f.find("time") != std::string::npos && f.find('.') == std::string::npos) {
            timeFile = filePath;
            break;
        }
    }

    if (timeFile.empty()) {
        LERROR(
            "Could not find a metadata file with time steps, such as a csv, dat, txt or "
            "no file extention with 'time' in filename"
        );
    }

    // time filestream
    std::ifstream tfs = std::ifstream(timeFile);
    if (!tfs.is_open()) {
        throw ghoul::RuntimeError("Could not open file");
    }

    std::string line;
    // gets only first line to "remove" header
    ghoul::getline(tfs, line);
    std::stringstream s;
    s << line;

    int nColumns = 0;
    std::string columnName;
    // loops through the names/columns in first line/header
    while (s >> columnName) {
        ++nColumns;
    }
    while (ghoul::getline(tfs, line)) {   // for each line of data
        std::istringstream iss(line);
        for (int i = 0; i < nColumns; i++) {    // for each column in line
            std::string columnValue;
            iss >> columnValue;
            if (i != nColumns - 1) {    // last column
                continue;
            }
            if (columnValue.length() == 23) {
                // Ensure the separators are correct
                columnValue.replace(4, 1, "-");
                columnValue.replace(7, 1, "-");
                columnValue.replace(13, 1, ":");
                columnValue.replace(16, 1, ":");
                columnValue.replace(19, 1, ".");
                const double triggerTime = Time::convertTime(columnValue);
                _startTimes.push_back(triggerTime);
            }
            else {
                LERROR(std::format(
                    "Error in file formating. Last column in file '{}' is not on UTC "
                    "ISO8601 format", timeFile
                ));
            }
        }
    }
}

void RenderableFluxNodes::updateActiveTriggerTimeIndex(double currentTime) {
    auto iter = std::upper_bound(_startTimes.begin(), _startTimes.end(), currentTime);
    if (iter != _startTimes.end()) {
        if (iter != _startTimes.begin()) {
            const std::ptrdiff_t idx = std::distance(_startTimes.begin(), iter);
            _activeTriggerTimeIndex = static_cast<int>(idx) - 1;
        }
        else {
            _activeTriggerTimeIndex = 0;
        }
    }
    else {
        _activeTriggerTimeIndex = static_cast<int>(_nStates) - 1;
    }
}
void RenderableFluxNodes::render(const RenderData& data, RendererTasks&) {
    if (_activeTriggerTimeIndex == -1) {
        return;
    }
    _shaderProgram->activate();

    // Calculate Model View MatrixProjection
    const glm::dmat4 rotationTransform = glm::dmat4(data.modelTransform.rotation);
    const glm::dmat4 modelTransform = calcModelTransform(
        data,
        { .rotation = rotationTransform }
    );

    _shaderProgram->setUniform(
        "modelViewProjection",
        glm::mat4(calcModelViewProjectionTransform(data, modelTransform))
    );

    SceneGraphNode* earthNode = sceneGraphNode("Earth");
    if (!earthNode) {
        LWARNING("Could not find scene graph node 'Earth'");
    }
    const glm::vec3 earthPos = glm::vec3(
        earthNode->worldPosition() * data.modelTransform.rotation
    );

    _shaderProgram->setUniform(_uniformCache.streamColor, _streamColor);
    _shaderProgram->setUniform(_uniformCache.nodeSize, _nodeSize);
    _shaderProgram->setUniform(_uniformCache.thresholdFlux, _thresholdFlux);
    _shaderProgram->setUniform(_uniformCache.colorMode, _colorMode);
    _shaderProgram->setUniform(_uniformCache.filterLower, _filteringLower);
    _shaderProgram->setUniform(_uniformCache.filterUpper, _filteringUpper);
    _shaderProgram->setUniform(_uniformCache.scalingMode, _scalingMethod);
    _shaderProgram->setUniform(_uniformCache.colorTableRange, _colorTableRange);
    _shaderProgram->setUniform(_uniformCache.domainLimZ, _domainZ);
    _shaderProgram->setUniform(_uniformCache.nodeSkip, _amountofNodes);
    _shaderProgram->setUniform(_uniformCache.nodeSkipDefault, _defaultNodeSkip);
    _shaderProgram->setUniform(_uniformCache.nodeSkipEarth, _earthNodeSkip);
    _shaderProgram->setUniform(_uniformCache.nodeSkipMethod, _nodeskipMethod);
    _shaderProgram->setUniform(
        _uniformCache.nodeSkipFluxThreshold,
        _fluxNodeskipThreshold
    );
    _shaderProgram->setUniform(
        _uniformCache.nodeSkipRadiusThreshold,
        _radiusNodeSkipThreshold
    );
    _shaderProgram->setUniform(_uniformCache.fluxColorAlpha, _fluxColorAlpha);

    _shaderProgram->setUniform(_uniformCache.earthPos, earthPos);
    _shaderProgram->setUniform(_uniformCache.distanceThreshold, _distanceThreshold);
    _shaderProgram->setUniform(_uniformCache.proximityNodesSize, _proximityNodesSize);
    _shaderProgram->setUniform(
        _uniformCache.time,
        global::windowDelegate->applicationTime()
    );
    _shaderProgram->setUniform(
        _uniformCache.maxNodeDistanceSize,
        _maxNodeDistanceSize
    );
    _shaderProgram->setUniform(
        _uniformCache.usingCameraPerspective,
        _cameraPerspectiveEnabled
    );
    _shaderProgram->setUniform(_uniformCache.drawCircles, _drawingCircles);
    _shaderProgram->setUniform(_uniformCache.drawHollow, _drawingHollow);
    _shaderProgram->setUniform(_uniformCache.useGaussian, _gaussianAlphaFilter);

    _shaderProgram->setUniform(
        _uniformCache.perspectiveDistanceFactor,
        _perspectiveDistanceFactor
    );
    _shaderProgram->setUniform(_uniformCache.minMaxNodeSize, _minMaxNodeSize);
    _shaderProgram->setUniform(_uniformCache.usingPulse, _pulseEnabled);
    _shaderProgram->setUniform(
        _uniformCache.usingGaussianPulse,
        _gaussianPulseEnabled
    );

    const glm::vec3 cameraPos = data.camera.positionVec3() * data.modelTransform.rotation;

    _shaderProgram->setUniform("cameraPos", cameraPos);

    if (_colorMode == static_cast<int>(ColorMethod::ByFluxValue)) {
        ghoul::opengl::TextureUnit textureUnit;
        textureUnit.activate();
        _transferFunction->bind(); // Calls update internally
        _shaderProgram->setUniform("colorTable", textureUnit);
    }

    glBindVertexArray(_vertexArrayObject);

    glDrawArrays(
        GL_POINTS,
        0,
        static_cast<GLsizei>(_vertexPositions.size())
    );

    glBindVertexArray(0);
    _shaderProgram->deactivate();
}

void RenderableFluxNodes::computeSequenceEndTime() {
    if (_nStates > 1) {
        const double lastTriggerTime = _startTimes[_nStates - 1];
        const double sequenceDuration = lastTriggerTime - _startTimes[0];
        const double averageStateDuration = sequenceDuration /
            (static_cast<double>(_nStates) - 1.0);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    }
    else if (_nStates == 1) {
        // If there's just one state it should never disappear!
        _sequenceEndTime = std::numeric_limits<double>::max();
    }
    else {
        LERROR("No states were found. The position file include this data");
    }
}

void RenderableFluxNodes::update(const UpdateData& data) {
    if (!_enabled) {
        return;
    }
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }
    bool needsUpdate = true;
    //Everything below is for updating depending on time
    const double currentTime = data.time.j2000Seconds();
    const bool isInInterval = (currentTime >= _startTimes[0]) &&
            (currentTime < _sequenceEndTime);
    //const bool isInInterval = true;
    if (isInInterval) {
        const size_t nextIdx = _activeTriggerTimeIndex + 1;
        if (
            // true => we were not in an interval the previous frame but now we are
            _activeTriggerTimeIndex == -1 ||
            // true => We stepped back to a time represented by another state
            currentTime < _startTimes[_activeTriggerTimeIndex] ||
            // true => We stepped forward to a time represented by another state
            (nextIdx < _nStates && currentTime >= _startTimes[nextIdx]))
        {
            updateActiveTriggerTimeIndex(currentTime);
        } // else {we're still in same state as previous frame (no changes needed)}
    }
    else {
        _activeTriggerTimeIndex = -1;
        needsUpdate = false;
    }

    if (needsUpdate && !_statesPos[_activeTriggerTimeIndex].empty()) {
        _vertexPositions = _statesPos[_activeTriggerTimeIndex];
        _vertexColor = _statesColor[_activeTriggerTimeIndex];
        _vertexRadius = _statesRadius[_activeTriggerTimeIndex];
        updatePositionBuffer();
        updateVertexColorBuffer();
        updateVertexFilteringBuffer();
    }

    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(
            *_shaderProgram,
            _uniformCache,
            UniformNames
        );
    }
}

void RenderableFluxNodes::updatePositionBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexPositions.size() * sizeof(glm::vec3),
        _vertexPositions.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glEnable(GL_PROGRAM_POINT_SIZE);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableFluxNodes::updateVertexColorBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexColor.size() * sizeof(float),
        _vertexColor.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 1, GL_FLOAT, GL_FALSE, 0, nullptr);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableFluxNodes::updateVertexFilteringBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexFilteringBuffer);

    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexRadius.size() * sizeof(float),
        _vertexRadius.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 1, GL_FLOAT, GL_FALSE, 0, nullptr);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}
} // namespace openspace
