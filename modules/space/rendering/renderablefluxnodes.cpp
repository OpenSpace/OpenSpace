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

#include <modules/space/rendering/renderablefluxnodes.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/updatestructures.h>
#include <openspace/query/query.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/logging/logmanager.h>
// Test debugging tools more then logmanager
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <fstream>
#include <functional> 
#include <optional>
#include <sys/stat.h>
#include <thread>

namespace {
    // log category
    constexpr const char* _loggerCat = "RenderableFluxNodes";

    // GL variables for shaders, probably needed some of them atleast
    constexpr const GLuint VaPosition   = 0;    // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaColor      = 1;    // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaFiltering  = 2;    // MUST CORRESPOND TO THE SHADER PROGRAM

    constexpr int8_t CurrentCacheVersion = 2;
    
    //streamColor, nodeSize, nodeSizeLargerFlux, thresholdFlux, 
    constexpr const std::array<const char*, 26> UniformNames = {
        "streamColor", "nodeSize", "nodeSizeLargerFlux", "thresholdFlux", "colorMode",
        "filterLower", "filterUpper", "scalingMode", "colorTableRange", "domainLimZ",
        "nodeSkip", "nodeSkipDefault", "nodeSkipEarth", "nodeSkipMethod", 
        "nodeSkipFluxThreshold", "nodeSkipRadiusThreshold", "fluxColorAlpha", 
        "fluxColorAlphaIlluminance", "earthPos", "distanceThreshold", 
        "enhanceMethod", "flowColor", "usingParticles", 
        "particleSize", "particleSpacing", "particleSpeed"
    };
    constexpr const std::array<const char*, 14> UniformNames2 = {
        "time", "flowColoring", "maxNodeDistanceSize", "usingCameraPerspective",
        "drawCircles", "drawHollow", "useGaussian", "usingRadiusPerspective",
        "perspectiveDistanceFactor", "maxNodeSize", "minNodeSize", "usingPulse",
        "usingGaussianPulse", "pulsatingAlways"
    };

    // --------------------------------- Property Info -------------------------------- //
    constexpr openspace::properties::Property::PropertyInfo GoesEnergyBinsInfo = {
        "GoesEnergy",
        "Goes Energy",
        "Select which energy bin you want to show. Emin01 is values > 10 Mev,"
        "Default is Emin03 where values > 100 Mev."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorModeInfo = {
        "ColorMode",
        "Color Mode",
        "Color lines uniformly or using color tables based on specific values on nodes,"
        "for examples flux values."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "ColorTablePath",
        "Path to Color Table",
        "Color Table/Transfer Function to use for 'By Flux Value' coloring."
    };
    constexpr openspace::properties::Property::PropertyInfo StreamColorInfo = {
        "Color",
        "Color",
        "Color of particles."
    };
    constexpr openspace::properties::Property::PropertyInfo NodeSizeInfo = {
       "NodeSize",
       "Size of nodes",
       "Change the size of the nodes"
    };
    constexpr openspace::properties::Property::PropertyInfo NodeSizeLargerFluxInfo = {
       "NodeSizeLargerFlux",
       "Size of nodes for larger flux",
       "Change the size of the nodes when flux is larger than flux threshold value"
    };
    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
       "LineWidth",
       "Line Width",
       "This value specifies the line width of the field lines if the "
       "selected render method includes lines."
    };
    constexpr openspace::properties::Property::PropertyInfo ThresholdFluxInfo = {
       "ThresholdFlux",
       "Threshold flux value",
       "This value specifies the threshold that will be changed with the flux value."
    };
    constexpr openspace::properties::Property::PropertyInfo FilteringInfo = {
        "FilterLower",
        "Filtering Lower Value in AU",
        "Use filtering to show nodes within a given range."
    };
    constexpr openspace::properties::Property::PropertyInfo FilteringUpperInfo = {
        "FilterUpper",
        "Filtering Upper Value in AU",
        "Use filtering to show nodes within a given range."
    };
    constexpr openspace::properties::Property::PropertyInfo AmountofNodesInfo = {
        "AmountOfNodes",
        "Every nth node to render in",
        "Show only every nth node"
    };
    constexpr openspace::properties::Property::PropertyInfo DefaultNodeSkipInfo = {
        "NodeSkip",
        "Every nth node to render default",
        "Show only every nth node outside of skippingmethod"
    };
    constexpr openspace::properties::Property::PropertyInfo EarthNodeSkipInfo = {
       "NodeSkipEarth",
       "Every nth node to render close to Earth",
       "Show only every nth node outside of skippingmethod"
    };
    constexpr openspace::properties::Property::PropertyInfo ScalingmethodInfo = {
        "ScalingFlux",
        "Scale the flux value with color table",
        "Use scaling to color nodes with a given method."
    };
    constexpr openspace::properties::Property::PropertyInfo NodeskipMethodInfo = {
        "SkippingNodes",
        "How to select nodes to skip",
        "Methods to select nodes to skip."
    };
    constexpr openspace::properties::Property::PropertyInfo colorTableRangeInfo = {
        "ColorTableRange",
        "Color Table Range",
        "Valid range for the color table. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainZInfo = {
        "ZLimit",
        "Z-limits",
        "Valid range along the Z-axis. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo FluxColorAlphaInfo = {
        "FluxColorAlpha",
        "Flux Color Alpha",
        "The value of alpha for the flux color mode."
    };
    constexpr openspace::properties::Property::PropertyInfo 
                                                         FluxColorAlphaIlluminanceInfo = {
        "FluxColorAlphaIlluminance",
        "Flux Color Alpha for illuminance",
        "The value of alpha for the flux color mode."
    };
    constexpr openspace::properties::Property::PropertyInfo FluxNodeskipThresholdInfo = {
        "SkippingNodesByFlux",
        "Skipping Nodes By Flux",
        "Select nodes to skip depending on flux value."
    };
    constexpr openspace::properties::Property::PropertyInfo 
                                                           RadiusNodeSkipThresholdInfo = {
        "SkippingNodesByRadius",
        "Skipping Nodes By Radius",
        "Select nodes to skip depending on Radius."
    };
    constexpr openspace::properties::Property::PropertyInfo EnhanceMethodInfo = {
        "EnhanceMethod",
        "Enhance Method",
        "Deciding what method to use for nodes close to earth"
    };
    constexpr openspace::properties::Property::PropertyInfo DistanceplanetInfo = {
        "Distanceplanet",
        "Distance Planet",
        "Deciding what planet to check distance to."
    };
    constexpr openspace::properties::Property::PropertyInfo DistanceThresholdInfo = {
        "DistancePlanetThreshold",
        "Threshold for distance between planet",
        "Enhance the size of nodes dependent on distance to planet."
    };
    constexpr openspace::properties::Property::PropertyInfo MisalignedIndexInfo = {
        "MisalignedIndex",
        "Index to shift sequence number",
        "The misalignement number for sequence for fluxnodes vs Fieldlines"
    };
    constexpr openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "Flowcolor",
        "Color of Flow",
        "Color of Flow."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowEnabledInfo = {
        "FlowEnabled",
        "Flow Direction",
        "Toggles the rendering of moving particles along the lines. Can, for example, "
        "illustrate magnetic flow."
    };
    constexpr openspace::properties::Property::PropertyInfo InterestingStreamsInfo = {
        "InterestingStreamsEnabled",
        "Interesting Streams Enabled",
        "Toggles the rendering of selected streams."
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
    constexpr openspace::properties::Property::PropertyInfo UseFlowColorInfo = {
        "Coloring",
        "Color either by Flowcolor or Flow colortable",
        "If set to true the flow will be colored by Flowcolor."
    };
    constexpr openspace::properties::Property::PropertyInfo TempInfo1 = {
        "Temp1",
        "temp",
        "Temp"
    };
    constexpr openspace::properties::Property::PropertyInfo MaxNodeDistanceSizeInfo = {
        "MaxNodeDistanceSize",
        "Max Node Distance Size",
        "The maximum size of the nodes at a certin distance."
    };
    constexpr openspace::properties::Property::PropertyInfo NodeDistanceThresholdInfo = {
        "NodeDistanceThreshold",
        "Node Distance Threshold",
        "Threshold for where to interpolate between the max and min node distance."
    };
    constexpr openspace::properties::Property::PropertyInfo 
                                                          CameraPerspectiveEnabledInfo = {
        "CameraPerspectiveEnabled",
        "Use Camera perspective",
        "Camera perspective changes the size of the nodes dependent on "
        "distance from camera."
    };
    constexpr openspace::properties::Property::PropertyInfo DrawingCirclesInfo = {
        "Renderingcircles",
        "Render as circles",
        "Using fragment shader to draw nodes as circles instead of squares."
    };
    constexpr openspace::properties::Property::PropertyInfo DrawingHollowInfo = {
        "RenderingHollowCircles",
        "Render as hollow circles",
        "Using fragment shader to draw nodes as hollow circles."
    };
    constexpr openspace::properties::Property::PropertyInfo GaussiandAlphaFilterInfo = {
        "RenderingGaussianAlphaFilter",
        "Alpha by Gaussian",
        "Using fragment shader to draw nodes with Gaussian filter for alpha value."

    };
    constexpr openspace::properties::Property::PropertyInfo 
                                                          RadiusPerspectiveEnabledInfo = {
        "RadiusPerspectiveEnabled",
        "Include radius with cameraperspective",
        "If false, then nodes closer to the sun will not be larger "
        "regardless of distance to camera."
    };
    constexpr openspace::properties::Property::PropertyInfo 
                                                         PerspectiveDistanceFactorInfo = {
        "PerspectiveDistanceFactor",
        "Perspective Distance factor",
        "This value decides how far away the camera must be to start "
        "impacting the node size."
    };
    constexpr openspace::properties::Property::PropertyInfo MinNodeSizeInfo = {
        "MinNodeSize",
        "Minimum node size",
        "The minimum node size."
    };
    constexpr openspace::properties::Property::PropertyInfo MaxNodeSizeInfo = {
        "MaxNodeSize",
        "Maximum node size",
        "The minimum node size."
    };
    constexpr openspace::properties::Property::PropertyInfo AlwaysPulseInfo = {
        "AlwaysPulsate",
        "Pulsate regardless of camera position",
        "Always have nodes close to earth pulsate regardless of position."
    };
    constexpr openspace::properties::Property::PropertyInfo pulseEnabledInfo = {
        "PulseEnabled",
        "Nodes close to Earth pulsate",
        "Toggles the pulse for nodes close to Earth."
    };
    constexpr openspace::properties::Property::PropertyInfo gaussianPulseEnabledInfo = {
        "GaussianPulseEnabled",
        "Nodes close to Earth pulsate with alpha by gaussian",
        "Toggles the pulse with alpha by gaussian for nodes close to Earth."
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

    struct [[codegen::Dictionary(RenderableFluxNodes)]] Parameters {
        // path to source folder with the 3 binary files in it
        std::filesystem::path sourceFolder [[codegen::directory()]];
        //
        struct TransferFunctions {
            std::string standard;
            std::string flow;
            std::string earth;
            std::string cmr [[codegen::key("CMR")]];
        };
        // [[codegen::verbatim(ColorTablePathInfo.description)]]
        TransferFunctions colorTablePaths;
        // [[codegen::verbatim(LineWidthInfo.description)]]
        //float lineWidth;
        // [[codegen::verbatim(GoesEnergyBinsInfo.description)]]
        std::optional<int> energyBin;
    };
#include "renderablefluxnodes_codegen.cpp"

} // namespace

namespace openspace {

documentation::Documentation RenderableFluxNodes::Documentation() {
    return codegen::doc<Parameters>("space_renderable_flux_nodes");
}

RenderableFluxNodes::RenderableFluxNodes(const ghoul::Dictionary& dictionary)

    : Renderable(dictionary)
    , _goesEnergyBins(GoesEnergyBinsInfo, properties::OptionProperty::DisplayType::Radio)
    , _colorGroup({ "Color" })
    , _colorMode(ColorModeInfo, properties::OptionProperty::DisplayType::Radio)
    , _scalingmethod(ScalingmethodInfo, properties::OptionProperty::DisplayType::Radio)
    , _nodeskipMethod(NodeskipMethodInfo, properties::OptionProperty::DisplayType::Radio)
    , _enhancemethod(EnhanceMethodInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _colorTablePath(ColorTablePathInfo)
    , _streamColor(StreamColorInfo,
        glm::vec4(0.96f, 0.88f, 0.8f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f))
    , _streamGroup({ "Streams" })
    , _nodesamountGroup({ "NodeGroup" })
    , _nodeSize(NodeSizeInfo, 2.f, 1.f, 10.f)
    , _nodeSizeLargerFlux(NodeSizeLargerFluxInfo, 2.f, 1.f, 10.f)
    , _lineWidth(LineWidthInfo, 4.f, 1.f, 20.f)
    , _colorTableRange(colorTableRangeInfo)
    , _domainZ(DomainZInfo)
    , _fluxColorAlpha(FluxColorAlphaInfo, 0.f, 0.f, 1.f)
    , _fluxColorAlphaIlluminance(FluxColorAlphaIlluminanceInfo, 1.f, 0.f, 1.f)
    , _thresholdFlux(ThresholdFluxInfo, -1.5f, -50.f, 10.f)
    , _filteringLower(FilteringInfo, 0.f, 0.f, 5.f)
    , _filteringUpper(FilteringUpperInfo, 5.f, 0.f, 5.f)
    , _amountofNodes(AmountofNodesInfo, 1, 1, 100)
    , _defaultNodeSkip(DefaultNodeSkipInfo, 1, 1, 100)
    , _earthNodeSkip(EarthNodeSkipInfo, 1, 1, 100)
    , _fluxNodeskipThreshold(FluxNodeskipThresholdInfo, 0, -20, 10)
    , _radiusNodeSkipThreshold(RadiusNodeSkipThresholdInfo, 0.f, 0.f, 5.f)
    , _earthdistGroup({ "Earthfocus" })
    , _distanceThreshold(DistanceThresholdInfo, 0.0f, 0.0f, 1.0f)
    , _misalignedIndex(MisalignedIndexInfo, 0, -5, 20)
    , _flowColor(
        FlowColorInfo,
        glm::vec4(0.96f, 0.88f, 0.8f, 0.5f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _flowEnabled(FlowEnabledInfo, false)
    , _interestingStreamsEnabled(InterestingStreamsInfo, false)
    , _flowGroup({ "Flow" })
    , _flowParticleSize(FlowParticleSizeInfo, 5, 0, 500)
    , _flowParticleSpacing(FlowParticleSpacingInfo, 60, 0, 500)
    , _flowSpeed(FlowSpeedInfo, 20, 0, 1000)
    , _useFlowColor(UseFlowColorInfo, false)
    , _scaleFactor(TempInfo1, 150.f, 1.f, 500.f)
    , _maxNodeDistanceSize(MaxNodeDistanceSizeInfo, 1.f, 1.f, 10.f)
    , _nodeDistanceThreshold(NodeDistanceThresholdInfo, 0.f, 0.f, 40.f)
    , _cameraPerspectiveEnabled(CameraPerspectiveEnabledInfo, false)
    , _drawingCircles(DrawingCirclesInfo, false)
    , _cameraPerspectiveGroup({" CameraPerspective"})
    , _drawingHollow(DrawingHollowInfo, false)
    , _gaussianAlphaFilter(GaussiandAlphaFilterInfo, false)
    , _radiusPerspectiveEnabled(RadiusPerspectiveEnabledInfo, true)
    , _perspectiveDistanceFactor(PerspectiveDistanceFactorInfo, 2.67f, 1.f, 20.f)
    , _maxNodeSize(MaxNodeSizeInfo, 30.f, 1.f, 200.f)
    , _minNodeSize(MinNodeSizeInfo, 2.f, 1.f, 10.f)
    , _pulseEnabled(pulseEnabledInfo, false)
    , _gaussianPulseEnabled(gaussianPulseEnabledInfo, false)
    , _pulseAlways(AlwaysPulseInfo, false)
{

    const Parameters p = codegen::bake<Parameters>(dictionary);

    _transferFunction = 
        std::make_unique<TransferFunction>(p.colorTablePaths.standard);
    _transferFunctionCMR =
        std::make_unique<TransferFunction>(p.colorTablePaths.cmr);
    _transferFunctionEarth =
        std::make_unique<TransferFunction>(p.colorTablePaths.earth);
    _transferFunctionFlow = 
        std::make_unique<TransferFunction>(p.colorTablePaths.flow);
    
    _colorTablePath = p.colorTablePaths.standard;

    _binarySourceFolderPath = p.sourceFolder;
    if (std::filesystem::is_directory(_binarySourceFolderPath)) {
        // Extract all file paths from the provided folder
        _binarySourceFiles.clear();
        namespace fs = std::filesystem;
        for (const fs::directory_entry& e : fs::directory_iterator(
            _binarySourceFolderPath)) {
            if (e.is_regular_file()) {
                _binarySourceFiles.push_back(e.path().string());
            }
        }
        std::sort(_binarySourceFiles.begin(), _binarySourceFiles.end());

        // Ensure that there are available and valid source files left
        if (_binarySourceFiles.empty()) {
            LERROR(fmt::format(
                "{} contains no files", _binarySourceFolderPath
            ));
        }
    }
    else {
        LERROR(fmt::format(
            "Source folder {} is not a valid directory",
            _binarySourceFolderPath
        ));
    }

    // --------------------- Add Options to OptionProperties --------------------- //
    _goesEnergyBins.addOption(static_cast<int>(GoesEnergyBins::Emin01), "Emin01");
    _goesEnergyBins.addOption(static_cast<int>(GoesEnergyBins::Emin03), "Emin03");
    _colorMode.addOption(static_cast<int>(ColorMethod::ByFluxValue), "By Flux Value");
    _colorMode.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");

    _scalingmethod.addOption(static_cast<int>(ScalingMethod::Flux), "Flux");
    _scalingmethod.addOption(static_cast<int>(ScalingMethod::RFlux), "Radius * Flux");
    _scalingmethod.addOption(static_cast<int>(ScalingMethod::R2Flux), "Radius^2 * Flux");
    _scalingmethod.addOption(
        static_cast<int>(ScalingMethod::log10RFlux), "log10(r) * Flux");
    _scalingmethod.addOption(static_cast<int>(ScalingMethod::lnRFlux), "ln(r) * Flux");

    _nodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Uniform), "Uniform");
    _nodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Flux), "Flux");
    _nodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Radius), "Radius");
    _nodeskipMethod.addOption(
        static_cast<int>(NodeSkipMethod::Streamnumber), "Streamnumber");

    _enhancemethod.addOption(
        static_cast<int>(EnhanceMethod::Sizescaling), "SizeScaling");
    _enhancemethod.addOption(
        static_cast<int>(EnhanceMethod::Colortables), "ColorTables");
    _enhancemethod.addOption(
        static_cast<int>(EnhanceMethod::Sizeandcolor), "Sizescaling and colortables");
    _enhancemethod.addOption(
        static_cast<int>(EnhanceMethod::Illuminance), "Illuminance");

    if (p.energyBin.has_value()) {
        _goesEnergyBins.setValue(p.energyBin.value());
    }
    else { // default int 1 == Emin03 == MeV>100
        LINFO("Assuming default value 1, meaning Emin03");
        _goesEnergyBins.setValue(1);
    }
}

void RenderableFluxNodes::initialize() {
    setModelDependentConstants();

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
    _uniformCache.nodeSizeLargerFlux = 
                                    _shaderProgram->uniformLocation("nodeSizeLargerFlux");
    _uniformCache.thresholdFlux = _shaderProgram->uniformLocation("thresholdFlux");

    ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, UniformNames);
    ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache2, UniformNames2); 
      
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

    _colorTablePath.onChange([this] {
        _transferFunction->setPath(_colorTablePath);
    });

    _goesEnergyBins.onChange([this] {
        loadNodeData(_goesEnergyBins.option().value);
    });
}

void RenderableFluxNodes::setModelDependentConstants() {
    // Just used as a default value.
    float limit = 8.f;
    _colorTableRange.setMinValue(glm::vec2(-limit));
    _colorTableRange.setMaxValue(glm::vec2(limit));
    _colorTableRange = glm::vec2(-2.f, 4.f);

    float limitZMin = -2.5f;
    float limitZMax = 2.5f;

    _domainZ.setMinValue(glm::vec2(limitZMin));
    _domainZ.setMaxValue(glm::vec2(limitZMax));
    _domainZ = glm::vec2(limitZMin, limitZMax);
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

    std::string file = _binarySourceFolderPath.string() + "\\positions" + energybin;
    std::string file2 = _binarySourceFolderPath.string() + "\\fluxes" + energybin;
    std::string file3 = _binarySourceFolderPath.string() + "\\radiuses" + energybin;

    std::ifstream fileStream(file, std::ifstream::binary);
    std::ifstream fileStream2(file2, std::ifstream::binary);
    std::ifstream fileStream3(file3, std::ifstream::binary);

    if (!fileStream.good()) {
        LERROR(fmt::format("Could not read file '{}'", file));
        return;
    }

    uint32_t nNodesPerTimestep = 0;
    fileStream.read(reinterpret_cast<char*>(&nNodesPerTimestep), sizeof(uint32_t));

    uint32_t nTimeSteps = 0;
    fileStream.read(reinterpret_cast<char*>(&nTimeSteps), sizeof(uint32_t));
    _nStates = nTimeSteps;

    if (_nStates != _startTimes.size()) {
        LERROR("Number of states, _nStates, and number of start times, _startTimes, "
            "do not match");
        return;
    }

    _statesColor.clear();
    _statesPos.clear();
    _statesRadius.clear();

    for (unsigned int i = 0; i < _nStates; ++i) {
        _vertexPositions.resize(nNodesPerTimestep);
        fileStream.read(reinterpret_cast<char*>(
            _vertexPositions.data()), nNodesPerTimestep * sizeof(glm::vec3)
        );

        _statesPos.push_back(_vertexPositions);
        _vertexPositions.clear();
    }
    for (unsigned int i = 0; i < _nStates; ++i) {
        _vertexColor.resize(nNodesPerTimestep);
        fileStream2.read(reinterpret_cast<char*>(
            _vertexColor.data()), nNodesPerTimestep * sizeof(float)
        );

        _statesColor.push_back(_vertexColor);
        _vertexColor.clear();
    }
    for (unsigned int i = 0; i < _nStates; ++i) {
        _vertexRadius.resize(nNodesPerTimestep);
        fileStream3.read(reinterpret_cast<char*>(
            _vertexRadius.data()), nNodesPerTimestep * sizeof(float)
        );

        _statesRadius.push_back(_vertexRadius);
        _vertexRadius.clear();
    }
}

void RenderableFluxNodes::setupProperties() {
    // -------------- Add non-grouped properties (enablers and buttons) -------------- //
    addProperty(_goesEnergyBins);
    addProperty(_lineWidth);
    addProperty(_misalignedIndex);
    addProperty(_scaleFactor);
        
    // ----------------------------- Add Property Groups ----------------------------- //
    addPropertySubOwner(_colorGroup);
    addPropertySubOwner(_streamGroup);
    addPropertySubOwner(_nodesamountGroup);
    addPropertySubOwner(_earthdistGroup);
    addPropertySubOwner(_cameraPerspectiveGroup);
    _earthdistGroup.addPropertySubOwner(_flowGroup);

    // ------------------------- Add Properties to the groups ------------------------ //
    _colorGroup.addProperty(_colorMode);
    _colorGroup.addProperty(_scalingmethod);
    _colorGroup.addProperty(_colorTableRange);
    _colorGroup.addProperty(_colorTablePath);
    _colorGroup.addProperty(_streamColor);
    _colorGroup.addProperty(_fluxColorAlpha);
    _colorGroup.addProperty(_fluxColorAlphaIlluminance);

    _streamGroup.addProperty(_thresholdFlux);
    _streamGroup.addProperty(_filteringLower);
    _streamGroup.addProperty(_filteringUpper);
    _streamGroup.addProperty(_domainZ);

    _nodesamountGroup.addProperty(_nodeskipMethod);
    _nodesamountGroup.addProperty(_amountofNodes);
    _nodesamountGroup.addProperty(_defaultNodeSkip);
    _nodesamountGroup.addProperty(_earthNodeSkip);
    _nodesamountGroup.addProperty(_nodeSize);
    _nodesamountGroup.addProperty(_nodeSizeLargerFlux);
    _nodesamountGroup.addProperty(_fluxNodeskipThreshold);
    _nodesamountGroup.addProperty(_radiusNodeSkipThreshold);
    _nodesamountGroup.addProperty(_maxNodeDistanceSize);
    _nodesamountGroup.addProperty(_nodeDistanceThreshold);

    _earthdistGroup.addProperty(_distanceThreshold);
    _earthdistGroup.addProperty(_enhancemethod);
    _earthdistGroup.addProperty(_interestingStreamsEnabled);

    _flowGroup.addProperty(_flowEnabled);
    _flowGroup.addProperty(_flowColor);
    _flowGroup.addProperty(_flowParticleSize);
    _flowGroup.addProperty(_flowParticleSpacing);
    _flowGroup.addProperty(_flowSpeed);
    _flowGroup.addProperty(_useFlowColor);

    _cameraPerspectiveGroup.addProperty(_cameraPerspectiveEnabled);
    _cameraPerspectiveGroup.addProperty(_perspectiveDistanceFactor);
    _cameraPerspectiveGroup.addProperty(_drawingCircles);
    _cameraPerspectiveGroup.addProperty(_drawingHollow);
    _cameraPerspectiveGroup.addProperty(_gaussianAlphaFilter);
    _cameraPerspectiveGroup.addProperty(_radiusPerspectiveEnabled);
    _cameraPerspectiveGroup.addProperty(_maxNodeSize);
    _cameraPerspectiveGroup.addProperty(_minNodeSize);
    _cameraPerspectiveGroup.addProperty(_pulseEnabled);
    _cameraPerspectiveGroup.addProperty(_gaussianPulseEnabled);
    _cameraPerspectiveGroup.addProperty(_pulseAlways);

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
    // number of  characters in UTC ISO8601 format (without additional Z)
    // 'YYYY-MM-DDTHH-MM-SS-XXX'
    constexpr const int timeFormatSize = 23;

    std::string timeFile;
    std::string fileType;
    for (const std::string& filePath : _binarySourceFiles) {
        timeFile = filePath;

        if (filePath.substr(filePath.find_last_of(".") + 1) == "csv" ) {
            fileType = "csv";
            break;
        }
        else if (filePath.substr(filePath.find_last_of(".") + 1) == "dat") {
            fileType = "dat";
            break;
        }
        else if (filePath.substr(filePath.find_last_of(".") + 1) == "txt") {
            fileType = "txt";
            break;
        }
        //if no file extention but word "time" in file name
        else if (filePath.find("time") != std::string::npos && 
                    filePath.find(".") == std::string::npos) {
            break;
        }
        else {
            LERROR(fmt::format("Error in file type or nameing of file '{}'. ",
                "Time meta file supports csv, dat, txt or without file extention ",
                "(but then have to include 'time' in filename)", filePath
            ));
            timeFile.clear();
        }
    }

    if (timeFile.empty()) {
        LERROR("Could not find a metadata file with time steps," 
            " such as a csv, dat, txt or no file extention with 'time' in filename");
    }

    // time filestream
    std::ifstream tfs(timeFile);
    if (!tfs.is_open()) {
        throw std::runtime_error("Could not open file");
    }

    std::string line;
    std::getline(tfs, line);    //gets only first line
    std::stringstream s;
    s << line;

    int nColumns = 0;
    std::string columnName;
    //loops through the names/columns in first line/header
    while (s >> columnName) {
        ++nColumns;
    }
    while (std::getline(tfs, line)) {   //for each line of data
        std::istringstream iss(line);
        for (int i = 0; i < nColumns; ++i) {    //for each column in line
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
                LDEBUG("timestring " + columnValue);
                _startTimes.push_back(triggerTime);
            }
            else {
                LERROR(fmt::format("Error in file formating. Last column in ",
                    "file '{}' is not on UTC ISO8601 format", timeFile
                ));
            }
            
        }
    }
}

void RenderableFluxNodes::updateActiveTriggerTimeIndex(double currentTime) {
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
void RenderableFluxNodes::render(const RenderData& data, RendererTasks&) {
    if (_activeTriggerTimeIndex != -1) {
        _shaderProgram->activate();

        // Calculate Model View MatrixProjection
        const glm::dmat4 rotMat = glm::dmat4(data.modelTransform.rotation);
        const glm::dmat4 modelMat =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
            rotMat *
            glm::dmat4(glm::scale(
                glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)
            ));
        const glm::dmat4 modelViewMat = data.camera.combinedViewMatrix() * modelMat;

        //not in use atm.
        _shaderProgram->setUniform("modelViewProjection",
            data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewMat)
        );

        SceneGraphNode* earthNode = sceneGraphNode("Earth");
        if (!earthNode) {
            LWARNING("Could not find scene graph node 'Earth'.");
        }
        glm::vec3 earthPos = earthNode->worldPosition() * data.modelTransform.rotation;
    
        _shaderProgram->setUniform(_uniformCache.streamColor, _streamColor);
        _shaderProgram->setUniform(_uniformCache.nodeSize, _nodeSize);
        _shaderProgram->setUniform(
            _uniformCache.nodeSizeLargerFlux, 
            _nodeSizeLargerFlux
        );
        _shaderProgram->setUniform(_uniformCache.thresholdFlux, _thresholdFlux);
        _shaderProgram->setUniform(_uniformCache.colorMode, _colorMode);
        _shaderProgram->setUniform(_uniformCache.filterLower, _filteringLower);
        _shaderProgram->setUniform(_uniformCache.filterUpper, _filteringUpper);
        _shaderProgram->setUniform(_uniformCache.scalingMode, _scalingmethod);
        _shaderProgram->setUniform(
            _uniformCache.colorTableRange, 
            _colorTableRange.value()
        );
        _shaderProgram->setUniform(_uniformCache.domainLimZ, _domainZ.value());
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
        _shaderProgram->setUniform(
            _uniformCache.fluxColorAlphaIlluminance, 
            _fluxColorAlphaIlluminance
        );
        _shaderProgram->setUniform(_uniformCache.earthPos, earthPos);
        _shaderProgram->setUniform(_uniformCache.distanceThreshold, _distanceThreshold);
        _shaderProgram->setUniform(_uniformCache.enhanceMethod, _enhancemethod);
        _shaderProgram->setUniform(_uniformCache.flowColor, _flowColor);
        _shaderProgram->setUniform(_uniformCache.usingParticles, _flowEnabled);
        _shaderProgram->setUniform(_uniformCache.particleSize, _flowParticleSize);
        _shaderProgram->setUniform(_uniformCache.particleSpacing, _flowParticleSpacing);
        _shaderProgram->setUniform(_uniformCache.particleSpeed, _flowSpeed);
        _shaderProgram->setUniform(
            _uniformCache2.time,
            global::windowDelegate->applicationTime() * -1
        );
        _shaderProgram->setUniform(_uniformCache2.flowColoring, _useFlowColor);
        _shaderProgram->setUniform(
            _uniformCache2.maxNodeDistanceSize, 
            _maxNodeDistanceSize
        );
        _shaderProgram->setUniform(
            _uniformCache2.usingCameraPerspective, 
            _cameraPerspectiveEnabled
        );
        _shaderProgram->setUniform(_uniformCache2.drawCircles, _drawingCircles);
        _shaderProgram->setUniform(_uniformCache2.drawHollow, _drawingHollow);
        _shaderProgram->setUniform(_uniformCache2.useGaussian, _gaussianAlphaFilter);
        _shaderProgram->setUniform(
            _uniformCache2.usingRadiusPerspective, 
            _radiusPerspectiveEnabled
        );
        _shaderProgram->setUniform(
            _uniformCache2.perspectiveDistanceFactor, 
            _perspectiveDistanceFactor
        );
        _shaderProgram->setUniform(_uniformCache2.maxNodeSize, _maxNodeSize);
        _shaderProgram->setUniform(_uniformCache2.minNodeSize, _minNodeSize);
        _shaderProgram->setUniform(_uniformCache2.usingPulse, _pulseEnabled);
        _shaderProgram->setUniform(
            _uniformCache2.usingGaussianPulse, 
            _gaussianPulseEnabled
        );
        _shaderProgram->setUniform(_uniformCache2.pulsatingAlways, _pulseAlways);
        
        glm::vec3 cameraPos = data.camera.positionVec3() * data.modelTransform.rotation;
    
        _shaderProgram->setUniform("cameraPos", cameraPos);
    
        ghoul::opengl::TextureUnit textureUnit;
        ghoul::opengl::TextureUnit textureUnitCMR;
        ghoul::opengl::TextureUnit textureUnitEarth;
        ghoul::opengl::TextureUnit textureUnitFlow;
        if (_colorMode == static_cast<int>(ColorMethod::ByFluxValue)) {
            textureUnit.activate();
            _transferFunction->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTable", textureUnit);

            textureUnitCMR.activate();
            _transferFunctionCMR->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTableCMR", textureUnitCMR);

            textureUnitEarth.activate();
            _transferFunctionEarth->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTableEarth", textureUnitEarth);

            textureUnitFlow.activate();
            _transferFunctionFlow->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTableFlow", textureUnitFlow);
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
            // true => We stepped back to a time represented by another state
            currentTime < _startTimes[_activeTriggerTimeIndex] ||
            // true => We stepped forward to a time represented by another state
            (nextIdx < _nStates && currentTime >= _startTimes[nextIdx]))
        {
            updateActiveTriggerTimeIndex(currentTime);
            needsUpdate = true;
        } // else {we're still in same state as previous frame (no changes needed)}
    }
    else {
        _activeTriggerTimeIndex = -1;
        needsUpdate = false;
    }

    if (needsUpdate) {
        if (!_statesPos[_activeTriggerTimeIndex].empty()) {
            //if (_activeTriggerTimeIndex > _pMisalignedIndex) {
            //    _activeTriggerTimeIndex += -_pMisalignedIndex;
            //}
            _vertexPositions = _statesPos[_activeTriggerTimeIndex];//TODO urgent. 
            _vertexColor = _statesColor[_activeTriggerTimeIndex];  //access violation
            _vertexRadius = _statesRadius[_activeTriggerTimeIndex];
            needsUpdate = false;
            updatePositionBuffer();
            updateVertexColorBuffer();
            updateVertexFilteringBuffer();
        }
    }

    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(
            *_shaderProgram, 
            _uniformCache, 
            UniformNames
        );
        ghoul::opengl::updateUniformLocations(
            *_shaderProgram,
            _uniformCache2,
            UniformNames2
        );
    }
}

void RenderableFluxNodes::updatePositionBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);

    const std::vector<glm::vec3>& vertPos = _vertexPositions;

    glBufferData(
        GL_ARRAY_BUFFER,
        vertPos.size() * sizeof(glm::vec3),
        vertPos.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(VaPosition);
    glEnable(GL_PROGRAM_POINT_SIZE);
    glVertexAttribPointer(VaPosition, 3, GL_FLOAT, GL_FALSE, 0, 0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}
void RenderableFluxNodes::updateVertexColorBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexColorBuffer);

    const std::vector<float>& vertColor = _vertexColor;

    glBufferData(
        GL_ARRAY_BUFFER,
        vertColor.size() * sizeof(float),
        vertColor.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(VaColor);
    glVertexAttribPointer(VaColor, 1, GL_FLOAT, GL_FALSE, 0, 0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}
void RenderableFluxNodes::updateVertexFilteringBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexFilteringBuffer);

    const std::vector<float>& vertexRadius = _vertexRadius;

    glBufferData(
        GL_ARRAY_BUFFER,
        vertexRadius.size() * sizeof(float),
        vertexRadius.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(VaFiltering);
    glVertexAttribPointer(VaFiltering, 1, GL_FLOAT, GL_FALSE, 0, 0);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}
} // namespace openspace
