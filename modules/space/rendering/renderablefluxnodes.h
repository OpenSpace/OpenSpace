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

#include <openspace/rendering/renderable.h>

#include <modules/base/rendering/renderabletrail.h>
#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/transferfunction.h>
#include <atomic>



namespace openspace {

class RenderableFluxNodes : public Renderable {
public:
    RenderableFluxNodes(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    std::vector<GLsizei> _lineCount;
    std::vector<GLint> _lineStart;
    // ------------------------------------- ENUMS -------------------------------------//
    // Used to determine if lines should be colored UNIFORMLY or by Flux Value
    enum class ColorMethod {
        ByFluxValue = 0,
        Uniform = 1
    };
    enum class GoesEnergyBins {
        Emin01 = 0,
        Emin03 = 1
    };
    enum class ScalingMethod {
        Flux = 0,
        RFlux = 1,
        R2Flux = 2,
        log10RFlux = 3,
        lnRFlux = 4
    };
    enum class NodeSkipMethod {
        Uniform = 0,
        Flux = 1,
        Radius = 2,
        Streamnumber = 3
    };
    enum class EnhanceMethod {
        Sizescaling = 0,
        Colortables = 1,
        Sizeandcolor = 2,
        Illuminance = 3,
    };

    UniformCache(streamColor, nodeSize, nodeSizeLargerFlux, thresholdFlux, colorMode,
        filterLower, filterUpper, scalingMode, colorTableRange, domainLimZ, nodeSkip,
        nodeSkipDefault, nodeSkipEarth, nodeSkipMethod, nodeSkipFluxThreshold, 
        nodeSkipRadiusThreshold, fluxColorAlpha, fluxColorAlphaIlluminance, earthPos,
        distanceThreshold, enhanceMethod, flowColor, usingParticles,
        particleSize, particleSpacing, particleSpeed)
        _uniformCache;
    UniformCache(time, flowColoring, maxNodeDistanceSize, usingCameraPerspective,
        drawCircles, drawHollow, useGaussian, usingRadiusPerspective, 
        perspectiveDistanceFactor, maxNodeSize, minNodeSize, usingPulse, 
        usingGaussianPulse, pulsatingAlways) 
        _uniformCache2;

    // ------------------------------------ STRINGS ------------------------------------//
    std::string _binarySourceFolderPath;

    // --------------------------------- NUMERICALS ----------------------------------- //

    // Active index of _startTimes
    int _activeTriggerTimeIndex = -1;
    // Number of states in the sequence
    uint32_t _nStates = 0;

    // Estimated end of sequence.
    double _sequenceEndTime;
    // OpenGL Vertex Array Object
    GLuint _vertexArrayObject = 0;
    // OpenGL Vertex Buffer Object containing the vertex positions
    GLuint _vertexPositionBuffer = 0;
    // OpenGL Vertex Buffer Object containing the Flux values used for coloring
    // the nodes
    GLuint _vertexColorBuffer = 0;
    // OpenGL Vertex Buffer Object containing the positions to filter the nodes
    GLuint _vertexFilteringBuffer = 0;
    // OpenGL Vertex Buffer Object containing the index of nodes
    GLuint _vertexindexBuffer = 0;

    // ----------------------------------- POINTERS ------------------------------------//
    // The Lua-Modfile-Dictionary used during initialization
    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;

    // Transfer function used to color lines when _pColorMethod is set to BY_FLUX_VALUE
    std::unique_ptr<TransferFunction> _transferFunction;
    // Transfer function used to color with the CMR map
    std::unique_ptr<TransferFunction> _transferFunctionCMR;
    // Transfer function used to color line near Earth
    std::unique_ptr<TransferFunction> _transferFunctionEarth;
    // Transfer function used to color line flow
    std::unique_ptr<TransferFunction> _transferFunctionFlow;

    // ------------------------------------ VECTORS ----------------------------------- //
    // Contains the _triggerTimes for all streams in the sequence
    std::vector<double> _startTimes;
    // Contains vertexPositions
    std::vector<glm::vec3> _vertexPositions;
    // Contains vertex flux values for color
    std::vector<float> _vertexColor;
    // Contains radius of vertices
    std::vector<float> _vertexRadius;
    // Stores the states position
    std::vector<std::vector<glm::vec3>> _statesPos;
    // Stores the states color
    std::vector<std::vector<float>> _statesColor;
    // Stores the states radius
    std::vector<std::vector<float>> _statesRadius;

    // ---------------------------------- Properties ---------------------------------- //   
    // Group to hold properties regarding distance to earth
    properties::PropertyOwner _pEarthdistGroup;

    //Property to show different energybins
    properties::OptionProperty _pGoesEnergyBins;
    // Group to hold the color properties
    properties::PropertyOwner _pColorGroup;
    // Uniform/transfer function
    properties::OptionProperty _pColorMode;
    // Uniform stream Color
    properties::Vec4Property _pStreamColor;
    // Choose different distant to earth enhanchement method. 
    properties::OptionProperty _pEnhancemethod;
    // Color table/transfer function for "By Flux value" coloring
    properties::StringProperty _pColorTablePath;
    // Valid range for the color table
    properties::Vec2Property _pColorTableRange;
    // The value of alpha for the flux color mode
    properties::FloatProperty _pFluxColorAlpha;
    // The value of alpha for the flux illuminance color mode
    properties::FloatProperty _pFluxColorAlphaIlluminance;
    // Group to hold the particle properties
    properties::PropertyOwner _pStreamGroup;
    // Scaling options
    properties::OptionProperty _pScalingmethod;
    // Group for how many nodes to render dependent on radius and flux
    properties::PropertyOwner _pNodesamountGroup;
    // Size of simulated node particles
    properties::FloatProperty _pNodeSize;
    // Size of nodes for larger flux
    properties::FloatProperty _pNodeSizeLargerFlux;
    // Threshold from earth to decide the distance for which the nodeSize gets larger
    properties::FloatProperty _pDistanceThreshold;
    // Minimum size of nodes at a certin distance
    //properties::FloatProperty _pMinNodeDistanceSize;
    // Maximum size of nodes at a certin distance
    properties::FloatProperty _pMaxNodeDistanceSize;
    // Threshold for where to interpolate between the max and min node distance
    properties::FloatProperty _pNodeDistanceThreshold;
    // Toggle selected streams [ON/OFF]
    properties::BoolProperty _pInterestingStreamsEnabled;

    properties::FloatProperty _pMaxNodeSize;
    properties::FloatProperty _pMinNodeSize;
    /// Line width for the line rendering part
    properties::FloatProperty _pLineWidth;
    // Valid range along the Z-axis
    properties::Vec2Property _pDomainZ;
    // Threshold flux value
    properties::FloatProperty _pThresholdFlux;
    // Filtering nodes within a range
    properties::FloatProperty _pFilteringLower;
    // Filtering nodes with a upper range
    properties::FloatProperty _pFilteringUpper;
    // Amount of nodes to show
    properties::IntProperty _pAmountofNodes;
    // Nodeskipping options
    properties::OptionProperty _pNodeskipMethod;
    // amount of nodes to show outside of filterrange
    properties::IntProperty _pDefaultNodeSkip;
    // The Flux threshold to decide the line between
    //_pDefaultNodeSkip and _pAmountofNodes
    properties::FloatProperty _pFluxNodeskipThreshold;
    //The nodeskip for values within the range of the radius threshold to Earth
    properties::IntProperty _pEarthNodeSkip;
    // The Radius threshold to decide the line between 
    //_pDefaultNodeSkip and _pAmountofNodes
    properties::FloatProperty _pRadiusNodeSkipThreshold;
    //The active stream that we want to look at
    //properties::IntProperty _pActiveStreamNumber;

    //Misaligned index for fieldlines vs Fluxnodes
    properties::IntProperty _pMisalignedIndex;

    // Flow Properties
    // Simulated particles' color
    properties::Vec4Property _pFlowColor;
    // Toggle flow [ON/OFF]
    properties::BoolProperty _pFlowEnabled;
    // Group to hold the flow/particle properties
    properties::PropertyOwner _pFlowGroup;
    // Size of simulated flow particles
    properties::IntProperty _pFlowParticleSize;
    // Size of simulated flow particles
    properties::IntProperty _pFlowParticleSpacing;
    // Toggle flow direction [FORWARDS/BACKWARDS]
    //properties::BoolProperty _pFlowReversed;
    // Speed of simulated flow
    properties::IntProperty _pFlowSpeed;
    //Either use flowcolortable or FlowColor.
    properties::BoolProperty _pUseFlowColor;

    properties::PropertyOwner _pCameraPerspectiveGroup;
    properties::BoolProperty _pCameraPerspectiveEnabled;
    properties::BoolProperty _pDrawingCircles;
    properties::BoolProperty _pDrawingHollow;
    properties::BoolProperty _pGaussianAlphaFilter;
    properties::BoolProperty _pRadiusPerspectiveEnabled;
    properties::FloatProperty _pPerspectiveDistanceFactor;
    properties::BoolProperty _pPulseEnabled;
    properties::BoolProperty _pGaussianPulseEnabled;
    properties::BoolProperty _pPulseAlways;
    properties::FloatProperty _scaleFactor;

    // initialization
    std::vector<std::string> _binarySourceFiles;

    // --------------------- FUNCTIONS USED DURING INITIALIZATION --------------------- //    
    void definePropertyCallbackFunctions();
    void populateStartTimes();
    void computeSequenceEndTime();
    void setModelDependentConstants();
    void setupProperties();
    void updateActiveTriggerTimeIndex(double currentTime);

    void loadNodeData(int energybinOption);
    // ------------------------- FUNCTIONS USED DURING RUNTIME ------------------------ //
    void updatePositionBuffer();
    void updateVertexColorBuffer();
    void updateVertexFilteringBuffer();
};
} // namespace openspace
