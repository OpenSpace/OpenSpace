/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

namespace { enum class SourceFileType; }

namespace openspace {

class RenderableFieldlinesSequence : public Renderable {
public:
    RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

private:
    // ------------------------------------- ENUMS -------------------------------------//
    // Used to determine if lines should be colored UNIFORMLY or by an extraQuantity
    enum class ColorMethod : int {
        Uniform = 0,
        ByQuantity
    };

    // ------------------------------------ STRINGS ------------------------------------//
    std::string _identifier;                               // Name of the Node!

    // ------------------------------------- FLAGS -------------------------------------//
    // Used for 'runtime-states'. True when loading a new state from disk on another
    // thread.
    std::atomic_bool _isLoadingStateFromDisk = false;
    // False => states are stored in RAM (using 'in-RAM-states'), True => states are
    // loaded from disk during runtime (using 'runtime-states')
    bool _loadingStatesDynamically  = false;
    // Used for 'runtime-states': True if new 'runtime-state' must be loaded from disk.
    // False => the previous frame's state should still be shown
    bool _mustLoadNewStateFromDisk  = false;
    // Used for 'in-RAM-states' : True if new 'in-RAM-state'  must be loaded.
    // False => the previous frame's state should still be shown
    bool _needsUpdate = false;
    // Used for 'runtime-states'. True when finished loading a new state from disk on
    // another thread.
    std::atomic_bool _newStateIsReady = false;
    // True when new state is loaded or user change which quantity to color the lines by
    bool _shouldUpdateColorBuffer   = false;
    // True when new state is loaded or user change which quantity used for masking out
    // line segments
    bool _shouldUpdateMaskingBuffer = false;

    // --------------------------------- NUMERICALS ----------------------------------- //
    // Active index of _states. If(==-1)=>no state available for current time. Always the
    // same as _activeTriggerTimeIndex if(_loadingStatesDynamically==true), else
    // always = 0
    int _activeStateIndex = -1;
    // Active index of _startTimes
    int _activeTriggerTimeIndex = -1;
    // Number of states in the sequence
    size_t _nStates = 0;
    // In setup it is used to scale JSON coordinates. During runtime it is used to scale
    // domain limits.
    float _scalingFactor = 1.f;
    // Estimated end of sequence.
    double _sequenceEndTime;
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

    // ----------------------------------- POINTERS ------------------------------------//
    // The Lua-Modfile-Dictionary used during initialization
    std::unique_ptr<ghoul::Dictionary> _dictionary;
    // Used for 'runtime-states' when switching out current state to a new state
    std::unique_ptr<FieldlinesState> _newState;
    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;
    // Transfer function used to color lines when _pColorMethod is set to BY_QUANTITY
    std::unique_ptr<TransferFunction> _transferFunction;

    // ------------------------------------ VECTORS ----------------------------------- //
    // Paths to color tables. One for each 'extraQuantity'
    std::vector<std::string> _colorTablePaths;
    // Values represents min & max values represented in the color table
    std::vector<glm::vec2> _colorTableRanges;
    // Values represents min & max limits for valid masking range
    std::vector<glm::vec2> _maskingRanges;
    // Stores the provided source file paths if using 'runtime-states', else emptied after
    // initialization
    std::vector<std::string> _sourceFiles;
    // Contains the _triggerTimes for all FieldlineStates in the sequence
    std::vector<double> _startTimes;
    // Stores the FieldlineStates
    std::vector<FieldlinesState> _states;

    // ---------------------------------- Properties ---------------------------------- //
    // Group to hold the color properties
    properties::PropertyOwner _pColorGroup;
    // Uniform/transfer function/topology?
    properties::OptionProperty _pColorMethod;
    // Index of the extra quantity to color lines by
    properties::OptionProperty _pColorQuantity;
    // Color table/transfer function min
    properties::StringProperty _pColorQuantityMin;
    // Color table/transfer function max
    properties::StringProperty _pColorQuantityMax;
    // Color table/transfer function for "By Quantity" coloring
    properties::StringProperty _pColorTablePath;
    // Uniform Field Line Color
    properties::Vec4Property _pColorUniform;
    // Whether or not to use additive blending
    properties::BoolProperty _pColorABlendEnabled;

// Whether or not to use Domain
    properties::BoolProperty _pDomainEnabled;
    // Group to hold the Domain properties
    properties::PropertyOwner _pDomainGroup;
    // Domain Limits along x-axis
    properties::Vec2Property _pDomainX;
    // Domain Limits along y-axis
    properties::Vec2Property _pDomainY;
    // Domain Limits along z-axis
    properties::Vec2Property _pDomainZ;
    // Domain Limits radially
    properties::Vec2Property _pDomainR;

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
    properties::BoolProperty _pFlowReversed;
    // Speed of simulated flow
    properties::IntProperty _pFlowSpeed;

    // Whether or not to use masking
    properties::BoolProperty _pMaskingEnabled;
    // Group to hold the masking properties
    properties::PropertyOwner _pMaskingGroup;
    // Lower limit for allowed values
    properties::StringProperty _pMaskingMin;
    // Upper limit for allowed values
    properties::StringProperty _pMaskingMax;
    // Index of the extra quantity to use for masking
    properties::OptionProperty _pMaskingQuantity;

// Button which sets camera focus to parent node of the renderable
    properties::TriggerProperty _pFocusOnOriginBtn;
    // Button which executes a time jump to start of sequence
    properties::TriggerProperty _pJumpToStartBtn;

    // --------------------- FUNCTIONS USED DURING INITIALIZATION --------------------- //
    void addStateToSequence(FieldlinesState& STATE);
    void computeSequenceEndTime();
    void definePropertyCallbackFunctions();
    bool extractCdfInfoFromDictionary(std::string& seedFilePath, std::string& tracingVar,
        std::vector<std::string>& extraVars);
    bool extractJsonInfoFromDictionary(fls::Model& model);
    void extractMagnitudeVarsFromStrings(std::vector<std::string>& extraVars,
        std::vector<std::string>& extraMagVars);
    bool extractMandatoryInfoFromDictionary(SourceFileType& sourceFileType);
    void extractOptionalInfoFromDictionary(std::string& outputFolderPath);
    void extractOsflsInfoFromDictionary();
    bool extractSeedPointsFromFile(const std::string& path,
        std::vector<glm::vec3>& outVec);
    void extractTriggerTimesFromFileNames();
    bool loadJsonStatesIntoRAM(const std::string& outputFolder);
    void loadOsflsStatesIntoRAM(const std::string& outputFolder);
    bool getStatesFromCdfFiles(const std::string& outputFolder);
    void setModelDependentConstants();
    void setupProperties();
    bool prepareForOsflsStreaming();

    // ------------------------- FUNCTIONS USED DURING RUNTIME ------------------------ //
    void readNewState(const std::string& filePath);
    void updateActiveTriggerTimeIndex(double currentTime);
    void updateVertexPositionBuffer();
    void updateVertexColorBuffer();
    void updateVertexMaskingBuffer();
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
