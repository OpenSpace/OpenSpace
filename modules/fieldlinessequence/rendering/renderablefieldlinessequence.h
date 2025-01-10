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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__

#include <openspace/rendering/renderable.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/transferfunction.h>
#include <atomic>

namespace openspace {

class RenderableFieldlinesSequence : public Renderable {
public:
    RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary);
    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
    void addStateToSequence(FieldlinesState& STATE);
    void computeSequenceEndTime();
    void definePropertyCallbackFunctions();
    void extractTriggerTimesFromFileNames();
    bool loadJsonStatesIntoRAM();
    void loadOsflsStatesIntoRAM();
    bool getStatesFromCdfFiles();
    void setModelDependentConstants();
    void setupProperties();
    bool prepareForOsflsStreaming();

    void readNewState(const std::string& filePath);
    void updateActiveTriggerTimeIndex(double currentTime);
    void updateVertexPositionBuffer();
    void updateVertexColorBuffer();
    void updateVertexMaskingBuffer();

    // Used to determine if lines should be colored UNIFORMLY or by an extraQuantity
    enum class ColorMethod {
        Uniform = 0,
        ByQuantity = 1
    };
    enum class SourceFileType {
        Cdf = 0,
        Json = 1,
        Osfls = 2
    };

    // cdf, osfls or json
    SourceFileType _inputFileType;
    // Output folder path in case of file conversion
    std::string _outputFolderPath;
    // which tracing vaiable to trace. 'b' for fieldline is default
    std::string _tracingVariable;
    // path to directory with seed point files
    std::filesystem::path _seedPointDirectory;
    // optional except when using json input
    std::string _modelStr;

    // Used for 'runtime-states'. True when loading a new state from disk on another
    // thread.
    std::atomic_bool _isLoadingStateFromDisk = false;
    // False => states are stored in RAM (using 'in-RAM-states'), True => states are
    // loaded from disk during runtime (using 'runtime-states')
    bool _loadingStatesDynamically  = false;
    // Used for 'runtime-states'. True when finished loading a new state from disk on
    // another thread.
    std::atomic_bool _newStateIsReady = false;
    // True when new state is loaded or user change which quantity to color the lines by
    bool _shouldUpdateColorBuffer   = false;
    // True when new state is loaded or user change which quantity used for masking out
    // line segments
    bool _shouldUpdateMaskingBuffer = false;
    // note Elon: rework the case of only one state
    // hasBeenUpdated only gets sets once, first iteration of update function, to
    // guarantee the vertext position buffer to be initialized.
    bool _hasBeenUpdated = false;

    // Active index of _states. If(==-1)=>no state available for current time. Always the
    // same as _activeTriggerTimeIndex if(_loadingStatesDynamically==true), else
    // always = 0
    int _activeStateIndex = -1;
    // Active index of _startTimes
    int _activeTriggerTimeIndex = -1;
    // Manual time offset
    double _manualTimeOffset = 0.0;
    // Number of states in the sequence
    size_t _nStates = 0;
    // In setup it is used to scale JSON coordinates. During runtime it is used to scale
    // domain limits.
    float _scalingFactor = 1.f;
    // Estimated end of sequence.
    // If there's just one state it should never disappear
    double _sequenceEndTime = std::numeric_limits<double>::max();
    // OpenGL Vertex Array Object
    GLuint _vertexArrayObject = 0;
    // OpenGL Vertex Buffer Object containing the extraQuantity values used for coloring
    // the lines
    GLuint _vertexColorBuffer = 0;
    // OpenGL Vertex Buffer Object containing the extraQuantity values used for masking
    // out segments of the lines
    GLuint _vertexMaskingBuffer = 0;
    // OpenGL Vertex Buffer Object containing the vertex positions
    GLuint _vertexPositionBuffer = 0;

    // The Lua-Modfile-Dictionary used during initialization
    // Used for 'runtime-states' when switching out current state to a new state
    std::unique_ptr<FieldlinesState> _newState;
    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;
    // Transfer function used to color lines when _pColorMethod is set to BY_QUANTITY
    std::unique_ptr<TransferFunction> _transferFunction;

    // Paths to color tables. One for each 'extraQuantity'
    std::vector<std::filesystem::path> _colorTablePaths;
    // Values represents min & max values represented in the color table
    std::vector<glm::vec2> _colorTableRanges;
    // Values represents min & max limits for valid masking range
    std::vector<glm::vec2> _maskingRanges;
    // Stores the provided source file paths if using 'runtime-states', else emptied after
    // initialization
    std::vector<std::string> _sourceFiles;
    // Extra variables such as rho, p or t
    std::vector<std::string> _extraVars;
    // Contains the _triggerTimes for all FieldlineStates in the sequence
    std::vector<double> _startTimes;
    // Stores the FieldlineStates
    std::vector<FieldlinesState> _states;

    // Group to hold the color properties
    properties::PropertyOwner _colorGroup;
    // Uniform/transfer function/topology?
    properties::OptionProperty _colorMethod;
    // Index of the extra quantity to color lines by
    properties::OptionProperty _colorQuantity;
    // Used to save property for later initialization
    int _colorQuantityTemp = 0;
    // Color table/transfer function min and max range
    properties::Vec2Property _colorQuantityMinMax;
    // Color table/transfer function for "By Quantity" coloring
    properties::StringProperty _colorTablePath;
    // Uniform Field Line Color
    properties::Vec4Property _colorUniform;
    // Whether or not to use additive blending
    properties::BoolProperty _colorABlendEnabled;

    // Whether or not to use Domain
    properties::BoolProperty _domainEnabled;
    // Group to hold the Domain properties
    properties::PropertyOwner _domainGroup;
    // Domain Limits along x-axis
    properties::Vec2Property _domainX;
    // Domain Limits along y-axis
    properties::Vec2Property _domainY;
    // Domain Limits along z-axis
    properties::Vec2Property _domainZ;
    // Domain Limits radially
    properties::Vec2Property _domainR;

    // Simulated particles' color
    properties::Vec4Property _flowColor;
    // Toggle flow [ON/OFF]
    properties::BoolProperty _flowEnabled;
    // Group to hold the flow/particle properties
    properties::PropertyOwner _flowGroup;
    // Size of simulated flow particles
    properties::IntProperty _flowParticleSize;
    // Size of simulated flow particles
    properties::IntProperty _flowParticleSpacing;
    // Toggle flow direction [FORWARDS/BACKWARDS]
    properties::BoolProperty _flowReversed;
    // Speed of simulated flow
    properties::IntProperty _flowSpeed;

    // Whether or not to use masking
    properties::BoolProperty _maskingEnabled;
    // Group to hold the masking properties
    properties::PropertyOwner _maskingGroup;
    // Lower and upper range limit for allowed values
    properties::Vec2Property _maskingMinMax;
    // Index of the extra quantity to use for masking
    properties::OptionProperty _maskingQuantity;
    // used to save property for later initialization
    int _maskingQuantityTemp = 0;

    // Line width for the line rendering part
    properties::FloatProperty _lineWidth;
    // Button which executes a time jump to start of sequence
    properties::TriggerProperty _jumpToStartBtn;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
