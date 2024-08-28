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

#ifndef __OPENSPACE_MODULE_SPACE___RENDERABLEFLUXNODES___H__
#define __OPENSPACE_MODULE_SPACE___RENDERABLEFLUXNODES___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/transferfunction.h>
#include <ghoul/opengl/uniformcache.h>
#include <atomic>
#include <filesystem>

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
    void definePropertyCallbackFunctions();
    void populateStartTimes();
    void computeSequenceEndTime();
    void setupProperties();
    void updateActiveTriggerTimeIndex(double currentTime);

    void loadNodeData(int energybinOption);
    void updatePositionBuffer();
    void updateVertexColorBuffer();
    void updateVertexFilteringBuffer();

    std::vector<GLsizei> _lineCount;
    std::vector<GLint> _lineStart;
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
        Log10RFlux = 3,
        LnRFlux = 4
    };
    enum class NodeSkipMethod {
        Uniform = 0,
        Flux = 1,
        Radius = 2,
        Streamnumber = 3
    };
    enum class EnhanceMethod {
        SizeScaling = 0,
        ColorTables = 1,
        SizeAndColor = 2,
        Illuminance = 3,
    };

    UniformCache(streamColor, nodeSize, proximityNodesSize,
        thresholdFlux, colorMode, filterLower, filterUpper, scalingMode, colorTableRange,
        domainLimZ, nodeSkip, nodeSkipDefault, nodeSkipEarth, nodeSkipMethod,
        nodeSkipFluxThreshold, nodeSkipRadiusThreshold, fluxColorAlpha,
        earthPos, distanceThreshold, time, maxNodeDistanceSize, usingCameraPerspective,
        drawCircles, drawHollow, useGaussian, perspectiveDistanceFactor, minMaxNodeSize,
        usingPulse, usingGaussianPulse) _uniformCache;

    std::filesystem::path _binarySourceFolderPath;

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

    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;

    // Transfer function used to color lines when _colorMethod is set to by_flux_value
    std::unique_ptr<TransferFunction> _transferFunction;

    std::vector<std::filesystem::path> _binarySourceFiles;
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

    // Group to hold properties regarding distance to earth
    properties::PropertyOwner _earthdistGroup;

    // Property to show different energybins
    properties::OptionProperty _goesEnergyBins;
    // Group to hold the color properties
    properties::PropertyOwner _styleGroup;
    // Uniform/transfer function
    properties::OptionProperty _colorMode;
    // Uniform stream Color
    properties::Vec4Property _streamColor;
    // Path to transferfunction
    properties::StringProperty _colorTablePath;
    // Valid range for the color table
    properties::Vec2Property _colorTableRange;
    // The value of alpha for the flux color mode
    properties::FloatProperty _fluxColorAlpha;
    // Group to hold the particle properties
    properties::PropertyOwner _streamGroup;
    // Scaling options
    properties::OptionProperty _scalingMethod;
    // Group for how many nodes to render dependent on radius and flux
    properties::PropertyOwner _nodesAmountGroup;
    // Size of simulated node particles
    properties::FloatProperty _nodeSize;
    // Threshold from earth to decide the distance for which the nodeSize gets larger
    properties::FloatProperty _distanceThreshold;
    // Change size of nodes close to earth
    properties::FloatProperty _proximityNodesSize;
    // Maximum size of nodes at a certin distance
    properties::FloatProperty _maxNodeDistanceSize;

    properties::Vec2Property _minMaxNodeSize;

    // Valid range along the Z-axis
    properties::Vec2Property _domainZ;
    // Threshold flux value
    properties::FloatProperty _thresholdFlux;
    // Filtering nodes within a range
    properties::FloatProperty _filteringLower;
    // Filtering nodes with a upper range
    properties::FloatProperty _filteringUpper;
    // Amount of nodes to show
    properties::IntProperty _amountofNodes;
    // Nodeskipping options
    properties::OptionProperty _nodeskipMethod;
    // amount of nodes to show outside of filterrange
    properties::IntProperty _defaultNodeSkip;
    // The Flux threshold to decide the line between
    //_pDefaultNodeSkip and _pAmountofNodes
    properties::FloatProperty _fluxNodeskipThreshold;
    //The nodeskip for values within the range of the radius threshold to Earth
    properties::IntProperty _earthNodeSkip;
    // The Radius threshold to decide the line between
    //_pDefaultNodeSkip and _pAmountofNodes
    properties::FloatProperty _radiusNodeSkipThreshold;

    properties::PropertyOwner _cameraPerspectiveGroup;
    properties::BoolProperty _cameraPerspectiveEnabled;
    properties::BoolProperty _drawingCircles;
    properties::BoolProperty _drawingHollow;
    properties::BoolProperty _gaussianAlphaFilter;
    properties::FloatProperty _perspectiveDistanceFactor;
    properties::BoolProperty _pulseEnabled;
    properties::BoolProperty _gaussianPulseEnabled;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SPACE___RENDERABLEFLUXNODES___H__
