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
#include <modules/streamnodes/rendering/renderablestreamnodes.h>

// Includes from fieldlinessequence, might not need all of them
//#include <modules/fieldlinessequence/fieldlinessequencemodule.h>
//#include <modules/fieldlinessequence/util/kameleonfieldlinehelper.h>
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
// Test debugging tools more then logmanager
#include <ghoul/logging/consolelog.h>
#include <ghoul/logging/visualstudiooutputlog.h>
#include <ghoul/filesystem/cachemanager.h>

#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <functional> 
#include <fstream>
#include <thread>
#include <openspace/json.h>
#include <openspace/query/query.h>
#include <sys/stat.h>
// This is a call to use the nlohmann json file
#pragma

using json = nlohmann::json;
#pragma optimize("", off)

namespace {
    // log category
    constexpr const char* _loggerCat = "renderableStreamNodes";

    // GL variables for shaders, probably needed some of them atleast
    constexpr const GLuint VaPosition   = 0;    // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaColor      = 1;    // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaFiltering  = 2;    // MUST CORRESPOND TO THE SHADER PROGRAM
    constexpr const GLuint VaStreamnumber = 3;  // MUST CORRESPOND TO THE SHADER PROGRAM
   // constexpr const GLuint Arrow = 4;           // MUST CORRESPOND TO THE SHADER PROGRAM

    constexpr int8_t CurrentCacheVersion = 2;
    
    //streamColor, nodeSize, nodeSizeLargerFlux, thresholdFlux, 
    constexpr const std::array<const char*, 28> UniformNames = {
        "streamColor", "nodeSize", "nodeSizeLargerFlux", "thresholdFlux", "colorMode",
        "filterLower", "filterUpper", "scalingMode", "colorTableRange", "domainLimZ",
        "nodeSkip", "nodeSkipDefault", "nodeSkipEarth", "nodeSkipMethod", 
        "nodeSkipFluxThreshold", "nodeSkipRadiusThreshold", "fluxColorAlpha", 
        "fluxColorAlphaIlluminance", "earthPos", "distanceThreshold", "activeStreamNumber",
        "enhanceMethod", "flowColor", "usingParticles", "usingInterestingStreams",
        "particleSize", "particleSpacing", "particleSpeed"
    };
    constexpr const std::array<const char*, 14> UniformNames2 = {
        "time", "flowColoring", "maxNodeDistanceSize", "usingCameraPerspective",
        "drawCircles", "drawHollow", "useGaussian", "usingRadiusPerspective",
        "perspectiveDistanceFactor", "maxNodeSize", "minNodeSize", "usingPulse",
        "usingGaussianPulse", "pulsatingAlways"
    };

    // ----- KEYS POSSIBLE IN MODFILE. EXPECTED DATA TYPE OF VALUE IN [BRACKETS]  ----- //
    // ---------------------------- MANDATORY MODFILE KEYS ---------------------------- //
    // [STRING] "json"
    // constexpr const char* KeyInputFileType = "InputFileType";
    // [STRING] should be path to folder containing the input files
    //constexpr const char* KeySourceFolder = "SourceFolder";
    // [STRING] should be path to folder containing data in binary format
    constexpr const char* KeyBinarySourceFolder = "BinarySourceFolder";

    // ---------------------- MANDATORY INPUT TYPE SPECIFIC KEYS ---------------------- //
    // [STRING] Currently supports: "batsrus", "enlil" & "pfss"
    constexpr const char* KeySimulationModel = "SimulationModel";

    // ----------------------- OPTIONAL INPUT TYPE SPECIFIC KEYS ---------------------- //
    // [STRING]
    constexpr const char* KeyJsonScalingFactor = "ScaleToMeters";
    //[INT] Threshold Radius should have a range
    constexpr const char* KeyThresholdRadius = "ThresholdRadius";

    // ---------------------------- OPTIONAL MODFILE KEYS  ---------------------------- //
    // [STRING ARRAY] Values should be paths to .txt files
    constexpr const char* KeyColorTablePaths = "ColorTablePaths";
    //[INT] Line Width should have a range
    constexpr const char* KeyLineWidth = "LineWidth";

    // ------------- POSSIBLE STRING VALUES FOR CORRESPONDING MODFILE KEY ------------- //
    //constexpr const char* ValueInputFileTypeJson = "json";


    // --------------------------------- Property Info -------------------------------- //
    constexpr openspace::properties::Property::PropertyInfo GoesEnergyBinsInfo = {
        "GoesEnergy",
        "Goes Energy",
        "Select which energy bin you want to show. Emin01 is values > 10 Mev,"
        "Emin03 is values > 100 Mev."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorModeInfo = {
        "colorMode",
        "Color Mode",
        "Color lines uniformly or using color tables based on specific values on nodes,"
        "for examples flux values."
    };
    constexpr openspace::properties::Property::PropertyInfo ColorTablePathInfo = {
        "colorTablePath",
        "Path to Color Table",
        "Color Table/Transfer Function to use for 'By Flux Value' coloring."
    };
    constexpr openspace::properties::Property::PropertyInfo StreamColorInfo = {
        "color",
        "Color",
        "Color of particles."
    };
    constexpr openspace::properties::Property::PropertyInfo NodeSizeInfo = {
       "nodeSize",
       "Size of nodes",
       "Change the size of the nodes"
    };
    constexpr openspace::properties::Property::PropertyInfo NodeSizeLargerFluxInfo = {
       "nodeSizeLargerFlux",
       "Size of nodes for larger flux",
       "Change the size of the nodes when flux is larger than flux threshold value"
    };
    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
       "lineWidth",
       "Line Width",
       "This value specifies the line width of the field lines if the "
       "selected rendering method includes lines."
    };
    constexpr openspace::properties::Property::PropertyInfo ThresholdFluxInfo = {
       "thresholdFlux",
       "Threshold flux value",
       "This value specifies the threshold that will be changed with the flux value."
    };
    constexpr openspace::properties::Property::PropertyInfo FilteringInfo = {
        "filterLower",
        "Filtering Lower Value in AU",
        "Use filtering to show nodes within a given range."
    };
    constexpr openspace::properties::Property::PropertyInfo FilteringUpperInfo = {
        "filterUpper",
        "Filtering Upper Value in AU",
        "Use filtering to show nodes within a given range."
    };
    constexpr openspace::properties::Property::PropertyInfo AmountofNodesInfo = {
        "amountOfNodes",
        "Every nth node to render in",
        "Show only every nth node"
    };
    constexpr openspace::properties::Property::PropertyInfo DefaultNodeSkipInfo = {
        "nodeSkip",
        "Every nth node to render default",
        "Show only every nth node outside of skippingmethod"
    };
    constexpr openspace::properties::Property::PropertyInfo EarthNodeSkipInfo = {
       "nodeSkipEarth",
       "Every nth node to render close to Earth",
       "Show only every nth node outside of skippingmethod"
    };
    constexpr openspace::properties::Property::PropertyInfo ScalingmethodInfo = {
        "scalingFlux",
        "Scale the flux value with color table",
        "Use scaling to color nodes with a given method."
    };
    constexpr openspace::properties::Property::PropertyInfo NodeskipMethodInfo = {
        "skippingNodes",
        "How to select nodes to skip",
        "Methods to select nodes to skip."
    };
    constexpr openspace::properties::Property::PropertyInfo colorTableRangeInfo = {
        "colorTableRange",
        "Color Table Range",
        "Valid range for the color table. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo DomainZInfo = {
        "zLimit",
        "Z-limits",
        "Valid range along the Z-axis. [Min, Max]"
    };
    constexpr openspace::properties::Property::PropertyInfo FluxColorAlphaInfo = {
        "fluxColorAlpha",
        "Flux Color Alpha",
        "The value of alpha for the flux color mode."
    };
    constexpr openspace::properties::Property::PropertyInfo FluxColorAlphaIlluminanceInfo = {
        "fluxColorAlphaIlluminance",
        "Flux Color Alpha for illuminance",
        "The value of alpha for the flux color mode."
    };
    constexpr openspace::properties::Property::PropertyInfo FluxNodeskipThresholdInfo = {
        "skippingNodesByFlux",
        "Skipping Nodes By Flux",
        "Select nodes to skip depending on flux value."
    };
    constexpr openspace::properties::Property::PropertyInfo RadiusNodeSkipThresholdInfo = {
        "skippingNodesByRadius",
        "Skipping Nodes By Radius",
        "Select nodes to skip depending on Radius."
    };
    constexpr openspace::properties::Property::PropertyInfo EnhanceMethodInfo = {
        "enhanceMethod",
        "Enhance Method",
        "Deciding what method to use for nodes close to earth"
    };
    constexpr openspace::properties::Property::PropertyInfo DistanceplanetInfo = {
        "distanceplanet",
        "Distance Planet",
        "Deciding what planet to check distance to."
    };
    constexpr openspace::properties::Property::PropertyInfo DistanceThresholdInfo = {
        "distancePlanetThreshold",
        "Threshold for distance between planet",
        "Enhance the size of nodes dependent on distance to planet."
    };
    constexpr openspace::properties::Property::PropertyInfo ActiveStreamNumberInfo = {
        "activeStreamNumber",
        "activeStream",
        "The active stream to show"
    };
    constexpr openspace::properties::Property::PropertyInfo MisalignedIndexInfo = {
        "misalignedIndex",
        "Index to shift sequence number",
        "The misalignement number for sequence for streamnodes vs Fieldlines"
    };
    constexpr openspace::properties::Property::PropertyInfo FlowColorInfo = {
        "flowcolor",
        "Color of Flow",
        "Color of Flow."
    };
    constexpr openspace::properties::Property::PropertyInfo FlowEnabledInfo = {
        "flowEnabled",
        "Flow Direction",
        "Toggles the rendering of moving particles along the lines. Can, for example, "
        "illustrate magnetic flow."
    };
    constexpr openspace::properties::Property::PropertyInfo InterestingStreamsInfo = {
        "interestingStreamsEnabled",
        "Interesting Streams Enabled",
        "Toggles the rendering of selected streams."
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
    constexpr openspace::properties::Property::PropertyInfo UseFlowColorInfo = {
        "coloring",
        "Color either by Flowcolor or Flow colortable",
        "If set to true the flow will be colored by Flowcolor."
    };
    constexpr openspace::properties::Property::PropertyInfo TempInfo1 = {
        "temp1",
        "temp",
        "Temp"
    };
    constexpr openspace::properties::Property::PropertyInfo MaxNodeDistanceSizeInfo = {
        "maxNodeDistanceSize",
        "Max Node Distance Size",
        "The maximum size of the nodes at a certin distance."
    };
    /*constexpr openspace::properties::Property::PropertyInfo MinNodeDistanceSizeInfo = {
        "minNodeDistanceSize",
        "Min Node Distance Size",
        "The minimum size of the nodes at a certin distance"
    };*/
    constexpr openspace::properties::Property::PropertyInfo NodeDistanceThresholdInfo = {
        "nodeDistanceThreshold",
        "Node Distance Threshold",
        "Threshold for where to interpolate between the max and min node distance."
    };
    constexpr openspace::properties::Property::PropertyInfo CameraPerspectiveEnabledInfo = {
        "cameraPerspectiveEnabled",
        "Use Camera perspective",
        "Camera perspective changes the size of the nodes dependent on distance from camera."
    };
    constexpr openspace::properties::Property::PropertyInfo DrawingCirclesInfo = {
        "renderingcircles",
        "Render as circles",
        "Using fragment shader to draw nodes as circles instead of squares."
    };
    constexpr openspace::properties::Property::PropertyInfo DrawingHollowInfo = {
        "renderingHollowCircles",
        "Render as hollow circles",
        "Using fragment shader to draw nodes as hollow circles."
    };
    constexpr openspace::properties::Property::PropertyInfo GaussiandAlphaFilterInfo = {
        "renderingGaussianAlphaFilter",
        "Alpha by Gaussian",
        "Using fragment shader to draw nodes with Gaussian filter for alpha value."

    };
    constexpr openspace::properties::Property::PropertyInfo RadiusPerspectiveEnabledInfo = {
        "radiusPerspectiveEnabled",
        "Include radius with cameraperspective",
        "If false, then nodes closer to the sun will not be larger regardless of distance to camera."
    };
    constexpr openspace::properties::Property::PropertyInfo PerspectiveDistanceFactorInfo = {
        "perspectiveDistanceFactor",
        "Perspective Distance factor",
        "This value decides how far away the camera must be to start impacting the node size."
    };
    constexpr openspace::properties::Property::PropertyInfo MinNodeSizeInfo = {
        "minNodeSize",
        "Minimum node size",
        "The minimum node size."
    };
    constexpr openspace::properties::Property::PropertyInfo MaxNodeSizeInfo = {
        "maxNodeSize",
        "Maximum node size",
        "The minimum node size."
    };
    constexpr openspace::properties::Property::PropertyInfo AlwaysPulseInfo = {
        "alwaysPulsate",
        "Pulsate regardless of camera position",
        "Always have nodes close to earth pulsate regardless of position."
    };
    constexpr openspace::properties::Property::PropertyInfo pulseEnabledInfo = {
       "pulseEnabled",
       "Nodes close to Earth pulsate",
       "Toggles the pulse for nodes close to Earth."
    };
    constexpr openspace::properties::Property::PropertyInfo gaussianPulseEnabledInfo = {
       "gaussianPulseEnabled",
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

    // Changed everything from dvec3 to vec3
    glm::vec3 sphericalToCartesianCoord(glm::vec3 position) {
        glm::vec3 cartesianPosition = glm::vec3();

        // ρsinφcosθ 
        cartesianPosition.x = position.x * sin(position.z) * cos(position.y);
        // ρsinφsinθ
        cartesianPosition.y = position.x * sin(position.z) * sin(position.y);
        // ρcosφ
        cartesianPosition.z = position.x * cos(position.z);

        return cartesianPosition;
    }
} //namespace

namespace openspace {
using namespace properties;
RenderableStreamNodes::RenderableStreamNodes(const ghoul::Dictionary& dictionary)

    : Renderable(dictionary)
    , _pGoesEnergyBins(GoesEnergyBinsInfo, OptionProperty::DisplayType::Radio)
    , _pColorGroup({ "Color" })
    , _pColorMode(ColorModeInfo, OptionProperty::DisplayType::Radio)
    , _pScalingmethod(ScalingmethodInfo, OptionProperty::DisplayType::Radio)
    , _pNodeskipMethod(NodeskipMethodInfo, OptionProperty::DisplayType::Radio)
    , _pEnhancemethod(EnhanceMethodInfo, OptionProperty::DisplayType::Dropdown)
    , _pColorTablePath(ColorTablePathInfo)
    , _pStreamColor(StreamColorInfo,
        glm::vec4(0.96f, 0.88f, 0.8f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f))
    , _pStreamGroup({ "Streams" })
    , _pNodesamountGroup({ "NodeGroup" })
    , _pNodeSize(NodeSizeInfo, 2.f, 1.f, 10.f)
    , _pNodeSizeLargerFlux(NodeSizeLargerFluxInfo, 2.f, 1.f, 10.f)
    , _pLineWidth(LineWidthInfo, 4.f, 1.f, 20.f)
    , _pColorTableRange(colorTableRangeInfo)
    , _pDomainZ(DomainZInfo)
    , _pFluxColorAlpha(FluxColorAlphaInfo, 0.f, 0.f, 1.f)
    , _pFluxColorAlphaIlluminance(FluxColorAlphaIlluminanceInfo, 1.f, 0.f, 1.f)
    , _pThresholdFlux(ThresholdFluxInfo, -1.5f, -50.f, 10.f)
    , _pFilteringLower(FilteringInfo, 0.f, 0.f, 5.f)
    , _pFilteringUpper(FilteringUpperInfo, 5.f, 0.f, 5.f)
    , _pAmountofNodes(AmountofNodesInfo, 1, 1, 100)
    , _pDefaultNodeSkip(DefaultNodeSkipInfo, 1, 1, 100)
    , _pEarthNodeSkip(EarthNodeSkipInfo, 1, 1, 100)
    , _pFluxNodeskipThreshold(FluxNodeskipThresholdInfo, 0, -20, 10)
    , _pRadiusNodeSkipThreshold(RadiusNodeSkipThresholdInfo, 0.f, 0.f, 5.f)
    , _pEarthdistGroup({ "Earthfocus" })
    , _pDistanceThreshold(DistanceThresholdInfo, 0.0f, 0.0f, 1.0f)
    , _pActiveStreamNumber(ActiveStreamNumberInfo, 0, 0, _numberofStreams)
    , _pMisalignedIndex(MisalignedIndexInfo, 0, -5, 20)
    , _pFlowColor(
        FlowColorInfo,
        glm::vec4(0.96f, 0.88f, 0.8f, 0.5f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _pFlowEnabled(FlowEnabledInfo, false)
    , _pInterestingStreamsEnabled(InterestingStreamsInfo, false)
    , _pFlowGroup({ "Flow" })
    , _pFlowParticleSize(FlowParticleSizeInfo, 5, 0, 500)
    , _pFlowParticleSpacing(FlowParticleSpacingInfo, 60, 0, 500)
    , _pFlowSpeed(FlowSpeedInfo, 20, 0, 1000)
    , _pUseFlowColor(UseFlowColorInfo, false)
    , _scaleFactor(TempInfo1, 150.f, 1.f, 500.f)
    //, _pMinNodeDistanceSize(MinNodeDistanceSizeInfo, 1.f, 1.f, 7.f)
    , _pMaxNodeDistanceSize(MaxNodeDistanceSizeInfo, 1.f, 1.f, 10.f)
    , _pNodeDistanceThreshold(NodeDistanceThresholdInfo, 0.f, 0.f, 40.f)
    , _pCameraPerspectiveEnabled(CameraPerspectiveEnabledInfo, false)
    , _pDrawingCircles(DrawingCirclesInfo, false)
    , _pCameraPerspectiveGroup({" CameraPerspective"})
    , _pDrawingHollow(DrawingHollowInfo, false)
    , _pGaussianAlphaFilter(GaussiandAlphaFilterInfo, false)
    , _pRadiusPerspectiveEnabled(RadiusPerspectiveEnabledInfo, true)
    , _pPerspectiveDistanceFactor(PerspectiveDistanceFactorInfo, 2.67f, 1.f, 20.f)
    , _pMaxNodeSize(MaxNodeSizeInfo, 30.f, 1.f, 200.f)
    , _pMinNodeSize(MinNodeSizeInfo, 2.f, 1.f, 10.f)
    , _pPulseEnabled(pulseEnabledInfo, false)
    , _pGaussianPulseEnabled(gaussianPulseEnabledInfo, false)
    , _pPulseAlways(AlwaysPulseInfo, false)
    //, _pTestChange(TestChangeInfo, 0.5f, 0.0f, 1.f)

{
    _dictionary = std::make_unique<ghoul::Dictionary>(dictionary);
}

void RenderableStreamNodes::definePropertyCallbackFunctions() {
// Add Property Callback Functions

    _pColorTablePath.onChange([this] {
        _transferFunction->setPath(_pColorTablePath);
        _colorTablePaths[0] = _pColorTablePath; 
    });

    _pGoesEnergyBins.onChange([this] {
        if (_pGoesEnergyBins == 1) {  // 1 == Emin03 == Mev > 100
            if (_shouldreadBinariesDirectly) {
                bool success = loadBinaryfilesDirectly("_emin03");
                if (success) return;
            }                         
        }
        else if(_pGoesEnergyBins == 0) {  // 0 == Emin01 == Mev > 10
            if (_shouldreadBinariesDirectly) {
                bool success = loadBinaryfilesDirectly("");
                if (success) return;
            }
        }
        //Should never occur. Emin01 = >10 MeV. Emin03 = >100 Mev
        else {  
            throw ghoul::RuntimeError("Error: Unknown EnergyBin. Supports 0=Emin01 and 1=Emin03");
            return;
        }
     });
}

void RenderableStreamNodes::setModelDependentConstants() {
    // Just used as a default value.
    float limit = 8.f; 
    _pColorTableRange.setMinValue(glm::vec2(-limit));
    _pColorTableRange.setMaxValue(glm::vec2(limit));
    _pColorTableRange = glm::vec2(-2, 4);

    //float limitZMin = -1000000000000;
    float limitZMin = -2.5f;
    //float limitZMax = 1000000000000;
    //float limitZMax = 1000000000000;
    float limitZMax = 2.5f;

    _pDomainZ.setMinValue(glm::vec2(limitZMin));
    _pDomainZ.setMaxValue(glm::vec2(limitZMax));
    _pDomainZ = glm::vec2(limitZMin, limitZMax);
}
    
void RenderableStreamNodes::initializeGL() {
    // EXTRACT MANDATORY INFORMATION FROM DICTIONARY
    // std::string filepath = "C:/Users/chrad171//openspace/OpenSpace/sync/http/bastille_day_streamnodes/1/datawithoutprettyprint_newmethod.json";
       
    if (!extractMandatoryInfoFromDictionary()) {
        return;
    }
    // Setup shader program
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "Streamnodes",
        absPath("${MODULE_STREAMNODES}/shaders/streamnodes_vs.glsl"),
        absPath("${MODULE_STREAMNODES}/shaders/streamnodes_fs.glsl")
    );

    _uniformCache.streamColor = _shaderProgram->uniformLocation("streamColor");
    _uniformCache.nodeSize = _shaderProgram->uniformLocation("nodeSize");
    _uniformCache.nodeSizeLargerFlux = _shaderProgram->uniformLocation("nodeSizeLargerFlux");
    _uniformCache.thresholdFlux = _shaderProgram->uniformLocation("thresholdFlux");

    ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, UniformNames);
    ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache2, UniformNames2);

    if (_dictionary->hasValue<ghoul::Dictionary>((KeyColorTablePaths))) {
        ghoul::Dictionary colorTablesPathsDictionary =
            _dictionary->value<ghoul::Dictionary>(KeyColorTablePaths);
        const size_t nProvidedPaths = colorTablesPathsDictionary.size();
        if (nProvidedPaths > 0) {
            // Clear the default! It is already specified in the transferFunction
            _colorTablePaths.clear();
            for (size_t i = 1; i <= nProvidedPaths; ++i) {
                _colorTablePaths.push_back(
                    colorTablesPathsDictionary.value<std::string>(std::to_string(i)));
            }
        }
        // Set a default color table, just in case the (optional) user defined paths are
        // corrupt or not provided!
        //_colorTablePaths.push_back(FieldlinesSequenceModule::DefaultTransferFunctionFile);
        _transferFunction = std::make_unique<TransferFunction>(absPath(_colorTablePaths[0]));
        _transferFunctionCMR = std::make_unique<TransferFunction>(absPath(_colorTablePaths[1]));
        _transferFunctionEarth = std::make_unique<TransferFunction>(absPath(_colorTablePaths[2]));  // what if not in order?
        _transferFunctionFlow = std::make_unique<TransferFunction>(absPath(_colorTablePaths[3]));
        //_transferFunctionIlluminance = std::make_unique<TransferFunction>(absPath(_colorTablePaths[4]));
        //_transferFunctionIlluminance2 = std::make_unique<TransferFunction>(absPath(_colorTablePaths[5]));
    }

    // EXTRACT OPTIONAL INFORMATION FROM DICTIONARY
    //std::string outputFolderPath;
    //extractOptionalInfoFromDictionary(outputFolderPath);

    // dictionary is no longer needed as everything is extracted
    _dictionary.reset();

    // No need to store source paths in memory if they are already in RAM!
    //if (!_loadingStatesDynamically) {
    //    _sourceFiles.clear();
    //}
    //_nStates = 274;
    setModelDependentConstants();
       
    //extractTriggerTimesFromFileNames();
    populateStartTimes();

    createStreamnumberVector();
    // Either we load in the data dynamically or statically at the start. 
    // If we should load in everything to Ram this if statement is true.
    if (!_loadingStatesDynamically) {
        loadNodeData();
    }
    computeSequenceEndTime();

    //float distanceThreshold = 65525112832.f;
    //float distanceThreshold = 33561643008.f;
    //ExtractandwriteInterestingStreams(distanceThreshold);
    //ReadInterestingStreamsFromJson();
    

    // If we are loading in states dynamically we would read new states during runtime, 
    // parsing json files pretty slowly.
      
    glGenVertexArrays(1, &_vertexArrayObject);
    glGenBuffers(1, &_vertexPositionBuffer);
    glGenBuffers(1, &_vertexColorBuffer);
    glGenBuffers(1, &_vertexFilteringBuffer);
    glGenBuffers(1, &_vertexStreamNumberBuffer);
   // glGenBuffers(1, &_arrow);

    // Needed for alpha transparency
    setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
    setupProperties();
}

void RenderableStreamNodes::loadNodeData() {

    if (_shouldreadBinariesDirectly) {
        bool success = false;
        if(_shouldloademin03directly){
            success = loadBinaryfilesDirectly("_emin03");
            _pGoesEnergyBins = 1;
        }
        else {
            success = loadBinaryfilesDirectly("");
        }
        if(success) return;
    }
    std::string _file = "StreamnodesCachePositionv3";
    std::string _file2 = "StreamnodesCacheColorv3";
    std::string _file3 = "StreamnodesCacheRadiusv3";
    if (_shouldwritecacheforemin03) {
        _file = "StreamnodesCachePosition_emin03";
        _file2 = "StreamnodesCacheColor_emin03";
        _file3 = "StreamnodesCacheRadius_emin03";
    }
    //if the files doesn't exist we create them, this is just so that we then can 
    // cache the actual binary files
    if (!FileSys.fileExists(_file)) {
        std::ofstream fileStream(_file, std::ofstream::binary);
        std::ofstream fileStream2(_file2, std::ofstream::binary);
        std::ofstream fileStream3(_file3, std::ofstream::binary);

        fileStream.write(
            reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t)
        );
        fileStream2.write(
            reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t)
        );
        fileStream3.write(
            reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t)
        );
    }

    std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        _file,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );
     //Check if we have a cached binary file for the data
    bool hasCachedFile = FileSys.fileExists(cachedFile);

    if (hasCachedFile) {
        LINFO(fmt::format("Cached file '{}' used for Speck file '{}'",
            cachedFile, _file
        ));
         //Read in the data from the cached file
        bool success = loadBinaryfilesDirectly("_emin03"); //readCachedFile(cachedfile, "")
        if (!success) {
            // If something went wrong it is probably because we changed 
            // the cache version or some file was not found.
            LWARNING("Cache file removed, something went wrong loading it.");
            // If thats the case we want to load in the files from json format 
            // and then write new cached files. 
            loadFilesIntoRam(); //~40min
            writeCachedFile();
        }
    }
    else {
         //We could not find the cachedfiles, parse the data statically 
         //instead and write it to binary format.
        loadFilesIntoRam();
        writeCachedFile();
    }

}

void RenderableStreamNodes::createStreamnumberVector() {
    int nPoints = 1999;
    int lineStartIdx = 0;
    
    for (int i = 0; i < _numberofStreams; ++i) {
        for (int k = 0; k < nPoints; ++k) {
                
            _vertexStreamnumber.push_back(i);
            //lineStartIdx++;
        }
            
        _lineCount.push_back(static_cast<GLsizei>(nPoints));
        _lineStart.push_back(static_cast<GLsizei>(lineStartIdx));
        lineStartIdx += nPoints;
    }
}

bool RenderableStreamNodes::loadFilesIntoRam() {
    LDEBUG("Did not find cached file, loading in data and converting only for this run, this step wont be needed next time you run Openspace ");
    // Loop through all the files dependent on how many states we would like to read in
    for (size_t j = 0; j < _nStates; ++j) {
            
        std::ifstream streamdata(_sourceFiles[j]);
        if (!streamdata.is_open())
        {
            LDEBUG("did not read the data.json file");
            return false;
        }
        json jsonobj = json::parse(streamdata);
           
        //const char* sNode = "node0";
        //const char* sStream = "stream0";
        //const char* sData = "data";

        //const json& jTmp = *(jsonobj.begin()); // First node in the file
        //const char* sTime = "time";
        //std::string testtime = jsonobj["time"];
          
        size_t lineStartIdx = 0;
        //const int _numberofStreams = 383;
       // const int _numberofStreams = 863;
        constexpr const float AuToMeter = 149597870700.f;  // Astronomical Units
            
        // Clear all the vectors in order to not have old states information in them
        _vertexPositions.clear();
        _lineCount.clear();
        _lineStart.clear();
        _vertexRadius.clear();
        _vertexColor.clear();

        int counter = 0;

        const size_t nPoints = 1;

        // Loop through all the streams
        for (int i = 0; i < _numberofStreams; ++i) {

            // Make an iterator at stream number i, then loop through that stream 
            // by iterating forward
            for (json::iterator lineIter = jsonobj["stream" + std::to_string(i)].begin();
                lineIter != jsonobj["stream" + std::to_string(i)].end(); ++lineIter) {
                    
                //get all the nodepositional values and Flux value
                std::string r = (*lineIter)["R"].get<std::string>();
                std::string phi = (*lineIter)["Phi"].get<std::string>();
                std::string theta = (*lineIter)["Theta"].get<std::string>();
                std::string flux = (*lineIter)["Flux"].get<std::string>();
                          
                // Convert the values to float
                float rValue = stringToFloat(r);
                float phiValue = stringToFloat(phi);
                float thetaValue = stringToFloat(theta);
                float fluxValue = stringToFloat(flux);
                //float ninetyDeToRad = 1.57079633f * 2;
                //const float pi = 3.14159265359f;

                // Push back values in order to be able to filter and color nodes 
                // by different threshold etc.
                float rTimesFluxValue = fluxValue;
                _vertexColor.push_back(rTimesFluxValue);
                _vertexRadius.push_back(rValue);
                rValue = rValue * AuToMeter;

                glm::vec3 sphericalcoordinates = glm::vec3(rValue, phiValue, thetaValue);

                // Convert the position from spherical coordinates to cartesian.
                glm::vec3 position = sphericalToCartesianCoord(sphericalcoordinates);
                  
                _vertexPositions.push_back(position);
              
                _lineCount.push_back(static_cast<GLsizei>(nPoints));
                _lineStart.push_back(static_cast<GLsizei>(lineStartIdx));
                lineStartIdx += nPoints;
            }
        }
        LDEBUG("Loaded in: " + std::to_string(_statesPos.size()) + 
            " frames of nodedata out of " + std::to_string(_nStates) + " total.");
           
        // Push back the vectors into our statesvectors
        _statesPos.push_back(_vertexPositions);     
        _statesColor.push_back(_vertexColor);
        _statesRadius.push_back(_vertexRadius);
    }    
    return true;
}

void RenderableStreamNodes::writeCachedFile() const {
    // Todo, write all of the vertexobjects into here 
    std::string _file = "StreamnodesCachePositionv3";
    std::string _file2 = "StreamnodesCacheColorv3";
    std::string _file3 = "StreamnodesCacheRadiusv3";

    if(_shouldwritecacheforemin03){
        _file = "StreamnodesCachePosition_emin03";
        _file2 = "StreamnodesCacheColor_emin03";
        _file3 = "StreamnodesCacheRadius_emin03";
    }
    std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        _file,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );
    std::ofstream fileStream(cachedFile, std::ofstream::binary);

    if (!fileStream.good()) {
        LERROR(fmt::format("Error opening file '{}' for save cache file", 
            "StreamnodesCache_emin03"
        ));
        return;
    }

    fileStream.write(
        reinterpret_cast<const char*>(&CurrentCacheVersion),
        sizeof(int8_t)
    );
        
    std::string cachedFile2 = FileSys.cacheManager()->cachedFilename(
        _file2,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );
    std::ofstream fileStream2(cachedFile2, std::ofstream::binary);

    std::string cachedFile3 = FileSys.cacheManager()->cachedFilename(
        _file3,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );
    std::ofstream fileStream3(cachedFile3, std::ofstream::binary);

    int32_t nValues = static_cast<int32_t>(_vertexRadius.size());
    if (nValues == 0) {
        throw ghoul::RuntimeError("Error writing cache: No values were loaded");
        return;
    }

    fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

    for(int i = 0; i < _nStates; ++i){
        fileStream.write(reinterpret_cast<const char*>(_statesPos[i].data()),
            nValues * sizeof(glm::vec3));
        fileStream2.write(reinterpret_cast<const char*>(_statesColor[i].data()), 
            nValues * sizeof(float));
        fileStream3.write(reinterpret_cast<const char*>(_statesRadius[i].data()), 
            nValues * sizeof(float));
   
    }
}

bool RenderableStreamNodes::loadBinaryfilesDirectly(const std::string& energybin) { // on init
    constexpr const float AuToMeter = 149597870700.f;  // Astronomical Units

    LDEBUG("Loading in binary files directly from sync folder");
    //std::string _file = _binarySourceFilePath  + "\\StreamnodesCachePositionv3" + energybin;
    //std::string _file2 = _binarySourceFilePath + "\\StreamnodesCacheColorv3" + energybin;
    //std::string _file3 = _binarySourceFilePath + "\\StreamnodesCacheRadiusv3" + energybin;

    std::string _file = _binarySourceFilePath + "\\positions";
    std::string _file2 = _binarySourceFilePath + "\\fluxes";
    std::string _file3 = _binarySourceFilePath + "\\radiuses";
    //ghoul::filesystem::File file(_file);

    //std::string cachedFile = FileSys.cacheManager()->cachedFilename(
    //    _file, //file,
    //    ghoul::filesystem::CacheManager::Persistent::Yes
    //);
    //std::string cachedFile2 = FileSys.cacheManager()->cachedFilename(
    //    _file2,
    //    ghoul::filesystem::CacheManager::Persistent::Yes
    //);
    //std::string cachedFile3 = FileSys.cacheManager()->cachedFilename(
    //    _file3,
    //    ghoul::filesystem::CacheManager::Persistent::Yes
    //);

    std::ifstream fileStream(_file, std::ifstream::binary);
    std::ifstream fileStream2(_file2, std::ifstream::binary);
    std::ifstream fileStream3(_file3, std::ifstream::binary);

    if (fileStream.good()) {
        //int8_t version = 0;
        //fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
        //if (version != CurrentCacheVersion) {
        //    LINFO("The format of the cached file has changed: deleting old cache");
        //    LDEBUG("Version: " + std::to_string(version));
        //    fileStream.close();
        //    //FileSys.deleteFile(file);
        //    //FileSys.deleteFile(cachedFile2);
        //    //FileSys.deleteFile(cachedFile3);
        //    return false;
        //}
        //LDEBUG("testar int8" + std::to_string(version));
        uint32_t nNodesPerTimestep = 0;
        fileStream.read(reinterpret_cast<char*>(&nNodesPerTimestep), sizeof(uint32_t));

        uint32_t nTimeSteps = 0;
        fileStream.read(reinterpret_cast<char*>(&nTimeSteps), sizeof(uint32_t));
        _nStates = nTimeSteps;

        _statesColor.clear();
        _statesPos.clear();
        _statesRadius.clear();

        for (int i = 0; i < _nStates; ++i) {
            _vertexPositions.resize(nNodesPerTimestep);
            fileStream.read(reinterpret_cast<char*>(
                _vertexPositions.data()),
                nNodesPerTimestep * sizeof(glm::vec3));

            _statesPos.push_back(_vertexPositions);
            _vertexPositions.clear();
        }
        for (int i = 0; i < _nStates; ++i) {
            _vertexColor.resize(nNodesPerTimestep);
            fileStream2.read(reinterpret_cast<char*>(
                _vertexColor.data()),
                nNodesPerTimestep * sizeof(float));

            _statesColor.push_back(_vertexColor);
            _vertexColor.clear();
        }
        for (int i = 0; i < _nStates; ++i) {
            _vertexRadius.resize(nNodesPerTimestep);
            fileStream3.read(reinterpret_cast<char*>(
                _vertexRadius.data()),
                nNodesPerTimestep * sizeof(float));

            _statesRadius.push_back(_vertexRadius);
            _vertexRadius.clear();
        }

        _isLoadingNewEnergyBin = false;
        bool success = fileStream.good();

        return success;
    }
    _isLoadingNewEnergyBin = false;
    return false;

}
/**
* Extracts the general information (from the lua modfile) that is mandatory for the class
* to function; such as the file type and the location of the source files.
* Returns false if it fails to extract mandatory information!
**/
bool RenderableStreamNodes::extractMandatoryInfoFromDictionary()
{
    //_identifier = _dictionary->value<std::string>(SceneGraphNode::KeyIdentifier);

    // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
    //std::string inputFileTypeString;
    //if (!_dictionary->hasValue<std::string>(KeyInputFileType)) {
    //    LERROR(fmt::format("{}: The field {} is missing", _identifier, KeyInputFileType));
    //}
    //else {
    //    // Verify that the input type is corrects
    //    inputFileTypeString =
    //        _dictionary->value<std::string>(KeyInputFileType);
    //    if (inputFileTypeString == ValueInputFileTypeJson) {    // == "json" 
    //    }
    //    else if(inputFileTypeString == "") {
    //    }
    //    else {
    //        LERROR(fmt::format(
    //            "{}: {} is not a recognized {}",
    //            _identifier, inputFileTypeString, KeyInputFileType
    //            ));
    //        return false;
    //    }
    //}

    //_colorTableRanges.push_back(glm::vec2(0, 1));   

    //if (!_dictionary->hasValue<std::string>(KeySourceFolder)) {
    //    LERROR(fmt::format("{}: The field {} is missing", _identifier, KeySourceFolder));
    //    return false;
    //}
    if (!_dictionary->hasValue<std::string>(KeyBinarySourceFolder)) {
        LERROR(fmt::format("{}: The field {} is missing", _identifier, KeyBinarySourceFolder));
        return false;
    }
    //constexpr const char temp = '\';
    //std::string sourceFolderPath =
    //    _dictionary->value<std::string>(KeySourceFolder);
    std::string binarySourceFolderPath = 
        _dictionary->value<std::string>(KeyBinarySourceFolder);
    _binarySourceFilePath = binarySourceFolderPath;
    LDEBUG(binarySourceFolderPath);
    ghoul::filesystem::Directory binarySourceFolder(binarySourceFolderPath);
    if (FileSys.directoryExists(binarySourceFolder)) {
        // Extract all file paths from the provided folder
        _binarySourceFiles = binarySourceFolder.readFiles(
            ghoul::filesystem::Directory::Recursive::No,
            ghoul::filesystem::Directory::Sort::Yes
        );
        // Ensure that there are available and valid source files left
        if (_binarySourceFiles.empty()) {
            LERROR(fmt::format(
                "{}: {} contains no files",
                _identifier, binarySourceFolderPath
            ));
            return false;
        }
    }
    else {
        LERROR(fmt::format(
            "{}: SourceFolder {} is not a valid directory",
            _identifier,
            binarySourceFolderPath
        ));
        return false;
    }

    //// Ensure that the source folder exists and then extract
    //// the files with the same extension as <inputFileTypeString>
    //ghoul::filesystem::Directory sourceFolder(sourceFolderPath);
    //if (FileSys.directoryExists(sourceFolder)) {
    //    // Extract all file paths from the provided folder
    //    _sourceFiles = sourceFolder.readFiles(
    //        ghoul::filesystem::Directory::Recursive::No,
    //        ghoul::filesystem::Directory::Sort::Yes
    //    );
    //    // Ensure that there are available and valid source files left
    //    if (_sourceFiles.empty()) {
    //        LERROR(fmt::format(
    //            "{}: {} contains no {} files",
    //            _identifier, sourceFolderPath, inputFileTypeString
    //        ));
    //        return false;
    //    }
    //}
    //else {
    //    LERROR(fmt::format(
    //        "{}: SourceFolder {} is not a valid directory",
    //        _identifier,
    //        sourceFolderPath
    //    ));
    //    return false;
    //}

    return true;
}

bool RenderableStreamNodes::extractJsonInfoFromDictionary(fls::Model& model) {
    std::string modelStr;
    if (_dictionary->hasValue<std::string>(KeySimulationModel)) {
        modelStr = _dictionary->value<std::string>(KeySimulationModel);
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
            "{}: Must specify '{}'", _identifier, KeySimulationModel
            ));
        return false;
    }

    if (_dictionary->hasValue<std::string>(KeyLineWidth)) {
        _pLineWidth = stringToFloat(_dictionary->value<std::string>(KeyLineWidth));
    }
    if (_dictionary->hasValue<std::string>(KeyThresholdRadius)) {
        _pThresholdFlux = stringToFloat(_dictionary->value<std::string>(KeyThresholdRadius));
    }
    if (_dictionary->hasValue<std::string>(KeyJsonScalingFactor)) {
        _scalingFactor = stringToFloat(_dictionary->value<std::string>(KeyJsonScalingFactor));
    }
    else {
        LWARNING(fmt::format(
            "{}: Does not provide scalingFactor. Assumes coordinates are in meters",
            _identifier
            ));
    }
    return true;
}

void RenderableStreamNodes::setupProperties() {

    // -------------- Add non-grouped properties (enablers and buttons) -------------- //
    addProperty(_pGoesEnergyBins);
    //we are using _pLineWidth at the moment
    addProperty(_pLineWidth);
    addProperty(_pMisalignedIndex);
    addProperty(_scaleFactor);
        
    // ----------------------------- Add Property Groups ----------------------------- //
    addPropertySubOwner(_pColorGroup);
    addPropertySubOwner(_pStreamGroup);
    addPropertySubOwner(_pNodesamountGroup);
    addPropertySubOwner(_pEarthdistGroup);
    addPropertySubOwner(_pCameraPerspectiveGroup);
    _pEarthdistGroup.addPropertySubOwner(_pFlowGroup);

    // ------------------------- Add Properties to the groups ------------------------ //
    _pColorGroup.addProperty(_pColorMode);
    _pColorGroup.addProperty(_pScalingmethod);
    _pColorGroup.addProperty(_pColorTableRange);
    _pColorGroup.addProperty(_pColorTablePath);
    _pColorGroup.addProperty(_pStreamColor);
    _pColorGroup.addProperty(_pFluxColorAlpha);
    _pColorGroup.addProperty(_pFluxColorAlphaIlluminance);

    _pStreamGroup.addProperty(_pThresholdFlux);
    _pStreamGroup.addProperty(_pFilteringLower);
    _pStreamGroup.addProperty(_pFilteringUpper);
    _pStreamGroup.addProperty(_pDomainZ);

    _pNodesamountGroup.addProperty(_pNodeskipMethod);
    _pNodesamountGroup.addProperty(_pAmountofNodes);
    _pNodesamountGroup.addProperty(_pDefaultNodeSkip);
    _pNodesamountGroup.addProperty(_pEarthNodeSkip);
    _pNodesamountGroup.addProperty(_pNodeSize);
    _pNodesamountGroup.addProperty(_pNodeSizeLargerFlux);
    _pNodesamountGroup.addProperty(_pFluxNodeskipThreshold);
    _pNodesamountGroup.addProperty(_pRadiusNodeSkipThreshold);
    _pNodesamountGroup.addProperty(_pActiveStreamNumber);
    //_pNodesamountGroup.addProperty(_pMinNodeDistanceSize);
    _pNodesamountGroup.addProperty(_pMaxNodeDistanceSize);
    _pNodesamountGroup.addProperty(_pNodeDistanceThreshold);

    _pEarthdistGroup.addProperty(_pDistanceThreshold);
    _pEarthdistGroup.addProperty(_pEnhancemethod);
    _pEarthdistGroup.addProperty(_pInterestingStreamsEnabled);

    _pFlowGroup.addProperty(_pFlowEnabled);
    _pFlowGroup.addProperty(_pFlowColor);
    _pFlowGroup.addProperty(_pFlowParticleSize);
    _pFlowGroup.addProperty(_pFlowParticleSpacing);
    _pFlowGroup.addProperty(_pFlowSpeed);
    _pFlowGroup.addProperty(_pUseFlowColor);

   // _pStreamGroup.addProperty(_pTestChange);

    // --------------------- Add Options to OptionProperties --------------------- //
    _pGoesEnergyBins.addOption(static_cast<int>(GoesEnergyBins::Emin01), "Emin01");
    _pGoesEnergyBins.addOption(static_cast<int>(GoesEnergyBins::Emin03), "Emin03");
    _pColorMode.addOption(static_cast<int>(ColorMethod::ByFluxValue), "By Flux Value");
    _pColorMode.addOption(static_cast<int>(ColorMethod::Uniform), "Uniform");

    _pScalingmethod.addOption(static_cast<int>(ScalingMethod::Flux), "Flux");
    _pScalingmethod.addOption(static_cast<int>(ScalingMethod::RFlux), "Radius * Flux");
    _pScalingmethod.addOption(static_cast<int>(ScalingMethod::R2Flux), "Radius^2 * Flux");
    _pScalingmethod.addOption(static_cast<int>(ScalingMethod::log10RFlux), "log10(r) * Flux");
    _pScalingmethod.addOption(static_cast<int>(ScalingMethod::lnRFlux), "ln(r) * Flux");
        
    _pNodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Uniform), "Uniform");
    _pNodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Flux), "Flux");
    _pNodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Radius), "Radius");
    _pNodeskipMethod.addOption(static_cast<int>(NodeSkipMethod::Streamnumber), "Streamnumber");

    _pEnhancemethod.addOption(static_cast<int>(EnhanceMethod::Sizescaling), "SizeScaling");
    _pEnhancemethod.addOption(static_cast<int>(EnhanceMethod::Colortables), "ColorTables");
    _pEnhancemethod.addOption(static_cast<int>(EnhanceMethod::Sizeandcolor), "Sizescaling and colortables");
    _pEnhancemethod.addOption(static_cast<int>(EnhanceMethod::Illuminance), "Illuminance");

    _pCameraPerspectiveGroup.addProperty(_pCameraPerspectiveEnabled);
    _pCameraPerspectiveGroup.addProperty(_pPerspectiveDistanceFactor);
    _pCameraPerspectiveGroup.addProperty(_pDrawingCircles);
    _pCameraPerspectiveGroup.addProperty(_pDrawingHollow);
    _pCameraPerspectiveGroup.addProperty(_pGaussianAlphaFilter);
    _pCameraPerspectiveGroup.addProperty(_pRadiusPerspectiveEnabled);
    _pCameraPerspectiveGroup.addProperty(_pMaxNodeSize);
    _pCameraPerspectiveGroup.addProperty(_pMinNodeSize);
    _pCameraPerspectiveGroup.addProperty(_pPulseEnabled);
    _pCameraPerspectiveGroup.addProperty(_pGaussianPulseEnabled);
    _pCameraPerspectiveGroup.addProperty(_pPulseAlways);

    definePropertyCallbackFunctions();
    // Set default
    _pColorTablePath = _colorTablePaths[0];
}

void RenderableStreamNodes::deinitializeGL() {
    glDeleteVertexArrays(1, &_vertexArrayObject);
    _vertexArrayObject = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    glDeleteBuffers(1, &_vertexColorBuffer);
    _vertexColorBuffer = 0;

    glDeleteBuffers(1, &_vertexFilteringBuffer);
    _vertexFilteringBuffer = 0;

    glDeleteBuffers(1, &_vertexStreamNumberBuffer);
    _vertexStreamNumberBuffer = 0;

    //glDeleteBuffers(1, &_arrow);
    //_arrow = 0;

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
        // TODO Replace sleep, (at least this is not during runtime)
        std::this_thread::sleep_for(std::chrono::milliseconds(5));
    }
}

bool RenderableStreamNodes::isReady() const {
    return _shaderProgram != nullptr;
}

// Extract J2000 time from file names
// Requires files to be named as such: 'YYYY-MM-DDTHH-MM-SS-XXX.json'
void RenderableStreamNodes::extractTriggerTimesFromFileNames() {
    // number of  characters in filename (excluding '.json')
    constexpr const int FilenameSize = 23;
    // size(".json")
    constexpr const int ExtSize = 5;

    for (const std::string& filePath : _sourceFiles) {
        LDEBUG("filepath " + filePath);
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
        LDEBUG("timestring " + timeString);
        _startTimes.push_back(triggerTime);
    }
}

void RenderableStreamNodes::populateStartTimes() {

    // number of  characters in UTC ISO8601 format (without additional Z)
    // 'YYYY-MM-DDTHH-MM-SS-XXX'
    constexpr const int timeFormatSize = 23;
    // size(".json")
    int ExtSize = 3;

    std::string timeFile = "";
    std::string fileType = "";
    for (const std::string& filePath : _binarySourceFiles) {

        if (filePath.substr(filePath.find_last_of(".") + 1) == "csv" ) {
            timeFile = filePath;
            fileType = "csv";
            break;
        }

        else if (filePath.substr(filePath.find_last_of(".") + 1) == "dat") {
            timeFile = filePath;
            fileType = "dat";
            break;
        }

        else if (filePath.substr(filePath.find_last_of(".") + 1) == "txt") {
            timeFile = filePath;
            fileType = "txt";
            break;
        }
        //if no file extention but word "time" in file name
        else if (filePath.find("time") != std::string::npos && 
                    filePath.find(".") == std::string::npos) {
            timeFile = filePath;
            ExtSize = 0;
            break;
        }
        else {
            LERROR(fmt::format("Error in file type or nameing of file '{}'.",
                " Time meta file supports csv, dat, txt or without file extention",
                " (but then have to include 'time' in filename)", timeFile
            ));
        }
    }

    if (timeFile.empty()) {
        LERROR("Could not find a metadata file with time steps,", 
            " such as a csv, dat, txt or no file extention with /"time/" in filename");
    }
    // time filestream
    std::ifstream tfs(timeFile);
    if (!tfs.is_open()) throw std::runtime_error("Could not open file");

    std::string line;
    std::getline(tfs, line);    //gets only first line
    std::stringstream s;
    s << line;

    int nColumns = 0;
    std::string columnName;
    //loops through the names/columns in first line/header
    while (s >> columnName) ++nColumns; 

    while (std::getline(tfs, line)) {   //for each line of data
        std::istringstream iss(line);
        for (int i = 0; i < nColumns; ++i) {    //for each column in line
            std::string columnValue;
            iss >> columnValue;
            if (i == nColumns - 1) {    // last column
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
                    LERROR(fmt::format("Error in file formating. Last column in file '{}'",
                        " is not on UTC ISO8601 format", timeFile
                    ));
                }
            }
        }
    }
}

void RenderableStreamNodes::updateActiveTriggerTimeIndex(double currentTime) {
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
void RenderableStreamNodes::render(const RenderData& data, RendererTasks&) {
    if (_activeTriggerTimeIndex != -1) {
        _shaderProgram->activate();

        // Calculate Model View MatrixProjection
        const glm::dmat4 rotMat = glm::dmat4(data.modelTransform.rotation);
        const glm::dmat4 modelMat =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
            rotMat *
            glm::dmat4(glm::scale(glm::dmat4(1), glm::dvec3(data.modelTransform.scale)));
        const glm::dmat4 modelViewMat = data.camera.combinedViewMatrix() * modelMat;

        //not in use atm.
        _shaderProgram->setUniform("modelViewProjection",
            data.camera.sgctInternal.projectionMatrix() * glm::mat4(modelViewMat));

        //glm::vec3 earthPos = glm::vec3(94499869340, -115427843118, 11212075887.3);
        SceneGraphNode* earthNode = sceneGraphNode("Earth");
        //earthNode->position() = 
        //Earthnode worldposition, is not aligned with the actual position shown as it seems right now.
        glm::vec3 earthPos = earthNode->worldPosition() * data.modelTransform.rotation;
    
        // this returns a value that goes from the sun, prolly because it is the root node. 
        //glm::vec3 earthPos = earthNode->position();
        //earthPos : 136665866240.000000, 44111921152.000000, -49989160960.000000
        //     Jon : 94499869340,         -115427843118,       11212075887.3 

        _shaderProgram->setUniform(_uniformCache.streamColor, _pStreamColor);
        _shaderProgram->setUniform(_uniformCache.nodeSize, _pNodeSize);
        _shaderProgram->setUniform(_uniformCache.nodeSizeLargerFlux, 
            _pNodeSizeLargerFlux);
        _shaderProgram->setUniform(_uniformCache.thresholdFlux, _pThresholdFlux);
        _shaderProgram->setUniform(_uniformCache.colorMode, _pColorMode);
        _shaderProgram->setUniform(_uniformCache.filterLower, _pFilteringLower);
        _shaderProgram->setUniform(_uniformCache.filterUpper, _pFilteringUpper);
        _shaderProgram->setUniform(_uniformCache.scalingMode, _pScalingmethod);
        _shaderProgram->setUniform(_uniformCache.colorTableRange, 
            _pColorTableRange.value());
        _shaderProgram->setUniform(_uniformCache.domainLimZ, _pDomainZ.value());
        _shaderProgram->setUniform(_uniformCache.nodeSkip, _pAmountofNodes);
        _shaderProgram->setUniform(_uniformCache.nodeSkipDefault, _pDefaultNodeSkip);
        _shaderProgram->setUniform(_uniformCache.nodeSkipEarth, _pEarthNodeSkip);
        _shaderProgram->setUniform(_uniformCache.nodeSkipMethod, _pNodeskipMethod);
        _shaderProgram->setUniform(_uniformCache.nodeSkipFluxThreshold, 
            _pFluxNodeskipThreshold);
        _shaderProgram->setUniform(_uniformCache.nodeSkipRadiusThreshold, 
            _pRadiusNodeSkipThreshold);
        _shaderProgram->setUniform(_uniformCache.fluxColorAlpha, _pFluxColorAlpha);
        _shaderProgram->setUniform(_uniformCache.fluxColorAlphaIlluminance, 
            _pFluxColorAlphaIlluminance);
        _shaderProgram->setUniform(_uniformCache.earthPos, earthPos);
        _shaderProgram->setUniform(_uniformCache.distanceThreshold, _pDistanceThreshold);
        _shaderProgram->setUniform(_uniformCache.activeStreamNumber,
            _pActiveStreamNumber);
        _shaderProgram->setUniform(_uniformCache.enhanceMethod, _pEnhancemethod);
        _shaderProgram->setUniform(_uniformCache.flowColor, _pFlowColor);
        _shaderProgram->setUniform(_uniformCache.usingParticles, _pFlowEnabled);
        _shaderProgram->setUniform(_uniformCache.usingInterestingStreams, 
            _pInterestingStreamsEnabled);
        _shaderProgram->setUniform(_uniformCache.particleSize, _pFlowParticleSize);
        _shaderProgram->setUniform(_uniformCache.particleSpacing, _pFlowParticleSpacing);
        _shaderProgram->setUniform(_uniformCache.particleSpeed, _pFlowSpeed);
        
        _shaderProgram->setUniform(_uniformCache2.time,
            global::windowDelegate->applicationTime() * -1);
        _shaderProgram->setUniform(_uniformCache2.flowColoring, _pUseFlowColor);
        //_shaderProgram->setUniform("minNodeDistanceSize", _pMinNodeDistanceSize);
        _shaderProgram->setUniform(_uniformCache2.maxNodeDistanceSize, 
            _pMaxNodeDistanceSize);
        //_shaderProgram->setUniform("nodeDistanceThreshold", _pNodeDistanceThreshold);
        _shaderProgram->setUniform(_uniformCache2.usingCameraPerspective, 
            _pCameraPerspectiveEnabled);
        _shaderProgram->setUniform(_uniformCache2.drawCircles, _pDrawingCircles);
        _shaderProgram->setUniform(_uniformCache2.drawHollow, _pDrawingHollow);
        _shaderProgram->setUniform(_uniformCache2.useGaussian, _pGaussianAlphaFilter);
        _shaderProgram->setUniform(_uniformCache2.usingRadiusPerspective, 
            _pRadiusPerspectiveEnabled);
        _shaderProgram->setUniform(_uniformCache2.perspectiveDistanceFactor, 
            _pPerspectiveDistanceFactor);
        //_shaderProgram->setUnifor("testChange", _pTestChange);
        _shaderProgram->setUniform(_uniformCache2.maxNodeSize, _pMaxNodeSize);
        _shaderProgram->setUniform(_uniformCache2.minNodeSize, _pMinNodeSize);
        _shaderProgram->setUniform(_uniformCache2.usingPulse, _pPulseEnabled);
        _shaderProgram->setUniform(_uniformCache2.usingGaussianPulse, 
            _pGaussianPulseEnabled);
        _shaderProgram->setUniform(_uniformCache2.pulsatingAlways, _pPulseAlways);
        //////// test for camera perspective: 
        /*
        glm::dmat4 modelMatrix =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
            glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));
    
        glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
        glm::mat4 projectionMatrix = data.camera.projectionMatrix();

        glm::dmat4 modelViewProjectionMatrix = glm::dmat4(projectionMatrix) * modelViewMatrix;

        glm::dvec3 cameraViewDirectionWorld = -data.camera.viewDirectionWorldSpace();
        glm::dvec3 cameraUpDirectionWorld = data.camera.lookUpVectorWorldSpace();
        glm::dvec3 orthoRight = glm::normalize(
            glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
        );
        if (orthoRight == glm::dvec3(0.0)) {
            glm::dvec3 otherVector(
                cameraUpDirectionWorld.y,
                cameraUpDirectionWorld.x,
                cameraUpDirectionWorld.z
            );
            orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
        }
        glm::dvec3 orthoUp = glm::normalize(glm::cross(cameraViewDirectionWorld, orthoRight));
        */
        glm::vec3 cameraPos = data.camera.positionVec3() * data.modelTransform.rotation;
    
        //this gives the same referenceframe as the nodes and makes it possible to see the
        //the distance between the camera and the nodes. 
        //cameraPos = cameraPos * data.modelTransform.rotation;
    
        _shaderProgram->setUniform("cameraPos", cameraPos);
        //glm::vec3 cameraPos = data.camera.unsynchedPositionVec3();
        //LDEBUG("camerapos x: " + std::to_string(cameraPos.x));
        //LDEBUG("camerapos y: " + std::to_string(cameraPos.z));
        //LDEBUG("camerapos z: " + std::to_string(cameraPos.y));
    
       // glm::vec4 cameraPostemp = glm::vec4(cameraPos, 1.0) * modelMatrix;
        
       // cameraPostemp = cameraPostemp * glm::dmat4(glm::dmat4(glm::inverse(data.camera.projectionMatrix())) * glm::inverse(data.camera.combinedViewMatrix()));
       // cameraPostemp = cameraPostemp * glm::dmat4(glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix());
       // cameraPos.x = cameraPostemp.x;
       // cameraPos.y = cameraPostemp.y;
       // cameraPos.z = cameraPostemp.z;
       // _shaderProgram->setUniform("scaleFactor", _scaleFactor);
       /* _shaderProgram->setUniform(
    
            "up",
            glm::vec3(data.camera.lookUpVectorWorldSpace())
        ); 
        _shaderProgram->setUniform("modelMatrix", modelMatrix);
        _shaderProgram->setUniform(
            "cameraViewProjectionMatrix",
            glm::mat4(
                glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix()
            )
        );
    
        //_shaderProgram->setUniform("minPointSize", 3.f); // in pixels
        //_shaderProgram->setUniform("maxPointSize", 30.f); // in pixels
        _shaderProgram->setUniform("up", glm::vec3(orthoUp));
        _shaderProgram->setUniform("right", glm::vec3(orthoRight));
        //_shaderProgram->setUniform(_uniformCache.fadeInValue, fadeInVariable);
        _shaderProgram->setUniform(
            "correctionSizeEndDistance",
            17.f
        );
        GLint viewport[4];
        glGetIntegerv(GL_VIEWPORT, viewport);
        */
       // _shaderProgram->setUniform("screenSize", glm::vec2(viewport[2], viewport[3]));

        //_shaderProgram->setUniform("camerapos", data.camera.)
        //data.camera.
        //glm::vec3 testvec = data.camera.positionVec3();
        //LDEBUG("test: " + std::to_string(testvec.x));
        if (_pColorMode == static_cast<int>(ColorMethod::ByFluxValue)) {
            ghoul::opengl::TextureUnit textureUnit;
            textureUnit.activate();
            _transferFunction->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTable", textureUnit);

            ghoul::opengl::TextureUnit textureUnitCMR;
            textureUnitCMR.activate();
            _transferFunctionCMR->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTableCMR", textureUnitCMR);

            ghoul::opengl::TextureUnit textureUnitEarth;
            textureUnitEarth.activate();
            _transferFunctionEarth->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTableEarth", textureUnitEarth);

            ghoul::opengl::TextureUnit textureUnitFlow;
            textureUnitFlow.activate();
            _transferFunctionFlow->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTableFlow", textureUnitFlow);

            /*ghoul::opengl::TextureUnit textureUnitIlluminance;
            textureUnitIlluminance.activate();
            _transferFunctionIlluminance->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTableIlluminance", textureUnitIlluminance);

            ghoul::opengl::TextureUnit textureUnitIlluminance2;
            textureUnitIlluminance2.activate();
            _transferFunctionIlluminance2->bind(); // Calls update internally
            _shaderProgram->setUniform("colorTableIlluminance2", textureUnitIlluminance2);*/
        }

        //const std::vector<glm::vec3>& vertPos = _vertexPositions;
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
inline void unbindGL() {
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableStreamNodes::computeSequenceEndTime() {
    if (_nStates > 1) {
        const double lastTriggerTime = _startTimes[_nStates - 1];
        const double sequenceDuration = lastTriggerTime - _startTimes[0];
        const double averageStateDuration = sequenceDuration /
            (static_cast<double>(_nStates) - 1.0);
        _sequenceEndTime = lastTriggerTime + averageStateDuration;
    }
    else if (_nStates == 1) {
        // If there's just one state it should never disappear!
        _sequenceEndTime = DBL_MAX;
    }
    else {
        LWARNING("Start up or error?");
    }
}
//void RenderableStreamNodes::ExtractandwriteInterestingStreams(float distanceThreshold) {
//    LDEBUG("we entered the extract function");
//    glm::vec3 earthPos = glm::vec3(94499869340, -115427843118, 11212075887.3);
//    //65525112832
//    std::vector<std::string> interestingStreams;
//    //for (int i = 0; i < _nStates; i++) {
//    _vertexPositions = _statesPos[100];
//    //for(int j = 0; j < 383; j++){
//    int counter = 0;
//    int streamnumber = 0;
//
//    for (int k = 0; k < _vertexPositions.size(); k++) {
//        if (counter > 1999) {
//            counter = 0;
//            streamnumber++;
//        }
//        //LDEBUG("Vi kom in i extract function test2");
//        if (glm::distance(_vertexPositions[k], earthPos) < distanceThreshold) {
//            // k++;
//            interestingStreams.push_back(std::to_string(streamnumber));
//            LDEBUG("We pushed back: " + std::to_string(streamnumber));
//
//            k = k + (1999 - counter);
//            streamnumber++;
//            //break;
//        }
//        counter++;
//    }
//
//    std::string fileoutputpath = absPath("${ASSETS}") +
//        "/scene/solarsystem/sun/heliosphere/mas/bastille_day/StreamSelection/streamSelection1.json";
//    std::ofstream streamdata(fileoutputpath);
//    json jsonobj;
//    jsonobj["test"] = interestingStreams;
//    //interestingStreams << jsonobj;
//    streamdata << jsonobj << std::endl;
//}
//void RenderableStreamNodes::ReadInterestingStreamsFromJson() {
//
//    std::string fileinputpath = absPath("${ASSETS}") +
//        "/scene/solarsystem/sun/heliosphere/mas/bastille_day/StreamSelection/streamSelection1.json";
//    std::ifstream streamdata(fileinputpath);
//    json jsonobj = json::parse(streamdata);
//    for (json::iterator lineIter = jsonobj["test"].begin();
//        lineIter != jsonobj["test"].end(); ++lineIter) {
//        std::string streamnumber = (*lineIter).get<std::string>();
//        
//        //LDEBUG("interestingstreams: " + std::to_string(_interestingStreams[1]));
//        LDEBUG("Interestingstreams: " + streamnumber);
//        int sn = std::stoi(streamnumber);
//        _interestingStreams.push_back(sn);
//    }
//}

void RenderableStreamNodes::update(const UpdateData& data) {
    if (!this->_enabled) return;
    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
    }
    //Everything below is for updating depending on time
    const double currentTime = data.time.j2000Seconds();
    const bool isInInterval = (currentTime >= _startTimes[0]) &&
            (currentTime < _sequenceEndTime);
    //const bool isInInterval = true;
    if (isInInterval) {
        const size_t nextIdx = _activeTriggerTimeIndex + 1;
        if (
            // true => Previous frame was not within the sequence interval
            //_activeTriggerTimeIndex < 0 ||
            // true => We stepped back to a time represented by another state
            currentTime < _startTimes[_activeTriggerTimeIndex] ||
            // true => We stepped forward to a time represented by another state
            (nextIdx < _nStates && currentTime >= _startTimes[nextIdx]))
        {
            updateActiveTriggerTimeIndex(currentTime);

            // _mustLoadNewStateFromDisk = true;

            _needsUpdate = true;
            _activeStateIndex = _activeTriggerTimeIndex;

        } // else {we're still in same state as previous frame (no changes needed)}
    }
    else {
        //not in interval => set everything to false
        //LDEBUG("not in interval");
        _activeTriggerTimeIndex = -1;
        _needsUpdate = false;
    }

    if (_needsUpdate) {
        if(_loadingStatesDynamically){
            if (!_isLoadingStateFromDisk) {
                _isLoadingStateFromDisk = true;
                if (_activeTriggerTimeIndex > _pMisalignedIndex) {
                    _activeTriggerTimeIndex += -_pMisalignedIndex;
                }
                LDEBUG("triggertime: " + std::to_string(_activeTriggerTimeIndex));

                std::string filePath = _sourceFiles[_activeTriggerTimeIndex];
                // auto vec = LoadJsonfile(filePath);
                std::thread readBinaryThread([this, f = std::move(filePath)]{
                    auto vec = LoadJsonfile(f);
                });
                readBinaryThread.detach();
            }
        
            _needsUpdate = false;

            if(_vertexPositions.size() > 5800){ //TODO urgent. 
                updatePositionBuffer();
                updateVertexColorBuffer();
                updateVertexFilteringBuffer();
                updateVertexStreamNumberBuffer();
                //updateArrow();
            }
        }
        // Needs fix, right now it stops cuz it cant find the states
        else if(!_statesPos[_activeTriggerTimeIndex].empty()) { 
            //&& !_isLoadingNewEnergyBin){
            if (_activeTriggerTimeIndex > _pMisalignedIndex) {
                _activeTriggerTimeIndex += -_pMisalignedIndex;
            }
            _vertexPositions = _statesPos[_activeTriggerTimeIndex];//TODO urgent. 
            _vertexColor = _statesColor[_activeTriggerTimeIndex];  //access violation
            _vertexRadius = _statesRadius[_activeTriggerTimeIndex];
            _needsUpdate = false;
            updatePositionBuffer();
            updateVertexColorBuffer();
            updateVertexFilteringBuffer();
            updateVertexStreamNumberBuffer();
        }
    }

    if (_shaderProgram->isDirty()) {
        _shaderProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache, 
            UniformNames);
        ghoul::opengl::updateUniformLocations(*_shaderProgram, _uniformCache2, 
            UniformNames2);
    }
}

std::vector<std::string> RenderableStreamNodes::LoadJsonfile(std::string filepath) {
       
    std::ifstream streamdata(filepath);
    if (!streamdata.is_open())
    {
        LDEBUG("did not read the data.json file");
    }
    json jsonobj = json::parse(streamdata);


    size_t lineStartIdx = 0;

    //Loop through all the nodes
    constexpr const float AuToMeter = 149597870700.f;
        _vertexPositions.clear();
        _lineCount.clear();
        _lineStart.clear();
        _vertexRadius.clear();
        _vertexColor.clear();
    int counter = 0;
        
    const size_t nPoints = 1;
    for (int i = 0; i < _numberofStreams; ++i) {
            
        for (json::iterator lineIter = jsonobj["stream" + std::to_string(i)].begin();
            lineIter != jsonobj["stream" + std::to_string(i)].end(); ++lineIter) {
            std::string r = (*lineIter)["R"].get<std::string>();
            std::string phi = (*lineIter)["Phi"].get<std::string>();
            std::string theta = (*lineIter)["Theta"].get<std::string>();
            std::string flux = (*lineIter)["Flux"].get<std::string>();

            float rValue = stringToFloat(r);
            float phiValue = stringToFloat(phi);
            float thetaValue = stringToFloat(theta);
            float fluxValue = stringToFloat(flux);
            const float pi = 3.14159265359f;
            float rTimesFluxValue = fluxValue;
            _vertexColor.push_back(rTimesFluxValue);
            _vertexRadius.push_back(rValue);
            rValue = rValue * AuToMeter;
                
            glm::vec3 sphericalcoordinates =
                glm::vec3(rValue, phiValue, thetaValue);

            glm::vec3 position = sphericalToCartesianCoord(sphericalcoordinates);

            _vertexPositions.push_back(position);
            ++counter;
        

            _lineCount.push_back(static_cast<GLsizei>(nPoints));
            _lineStart.push_back(static_cast<GLsizei>(lineStartIdx));
            lineStartIdx += nPoints;

        }
    }
    LDEBUG("vertPos size:" + std::to_string(_vertexPositions.size()));
    LDEBUG("counter for how many times we push back" + std::to_string(counter));

    _isLoadingStateFromDisk = false;

    return std::vector<std::string>();
}
void RenderableStreamNodes::updatePositionBuffer() {
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

    unbindGL();
}
void RenderableStreamNodes::updateVertexColorBuffer() {
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

        unbindGL();
}
void RenderableStreamNodes::updateVertexFilteringBuffer() {
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

        unbindGL();
}
void RenderableStreamNodes::updateVertexStreamNumberBuffer() {
    glBindVertexArray(_vertexArrayObject);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexStreamNumberBuffer);

    const std::vector<int>& vertexStreamnumber = _vertexStreamnumber;

    glBufferData(
        GL_ARRAY_BUFFER,
        vertexStreamnumber.size() * sizeof(float),
        vertexStreamnumber.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(VaStreamnumber);
    glVertexAttribPointer(VaStreamnumber, 1, GL_FLOAT, GL_FALSE, 0, 0);

    unbindGL();
}
} // namespace openspace
