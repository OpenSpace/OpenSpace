/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/transferfunction.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>

#include <atomic>

namespace {
    enum class SourceFileType;
}

namespace openspace {

class RenderableFieldlinesSequence : public Renderable {
public:
    RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary);

    void initialize() override;
    void deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;
private:
    // ------------------------------------- ENUMS -------------------------------------//
    enum ColorMethod : int {    // Used to determine if lines should be colored UNIFORMLY or by an extraQuantity
        Uniform = 0,
        ByQuantity
    };

    // ------------------------------------ STRINGS ------------------------------------//
    std::string       _name;                               // Name of the Node!

    // ------------------------------------- FLAGS -------------------------------------//
    std::atomic<bool> _isLoadingStateFromDisk    { false}; // Used for 'runtime-states'. True when loading a new state from disk on another thread.
    bool              _isReady                   = false;  // If initialization proved successful
    bool              _loadingStatesDynamically  = false;  // False => states are stored in RAM (using 'in-RAM-states'), True => states are loaded from disk during runtime (using 'runtime-states')
    bool              _mustLoadNewStateFromDisk  = false;  // Used for 'runtime-states': True if new 'runtime-state' must be loaded from disk. False => the previous frame's state should still be shown
    bool              _needsUpdate               = false;  // Used for 'in-RAM-states' : True if new 'in-RAM-state'  must be loaded.           False => the previous frame's state should still be shown
    std::atomic<bool> _newStateIsReady           { false}; // Used for 'runtime-states'. True when finished loading a new state from disk on another thread.
    bool              _shouldUpdateColorBuffer   = false;  // True when new state is loaded or user change which quantity to color the lines by
    bool              _shouldUpdateMaskingBuffer = false;  // True when new state is loaded or user change which quantity used for masking out line segments

    // --------------------------------- NUMERICALS ----------------------------------- //
    int               _activeStateIndex          = -1;     // Active index of _states. If(==-1)=>no state available for current time. Always the same as _activeTriggerTimeIndex if(_loadingStatesDynamically==true), else always = 0
    int               _activeTriggerTimeIndex    = -1;     // Active index of _startTimes
    size_t            _nStates                   = 0;      // Number of states in the sequence
    float             _scalingFactor             = 1.f;    // In setup it is used to scale JSON coordinates. During runtime it is used to scale domain limits.
    double            _sequenceEndTime;                    // Estimated end of sequence.
    GLuint            _vertexArrayObject         = 0;      // OpenGL Vertex Array Object
    GLuint            _vertexColorBuffer         = 0;      // OpenGL Vertex Buffer Object containing the extraQuantity values used for coloring the lines
    GLuint            _vertexMaskingBuffer       = 0;      // OpenGL Vertex Buffer Object containing the extraQuantity values used for masking out segments of the lines
    GLuint            _vertexPositionBuffer      = 0;      // OpenGL Vertex Buffer Object containing the vertex positions

    // ----------------------------------- POINTERS ------------------------------------//
    std::unique_ptr<ghoul::Dictionary>            _dictionary;       // The Lua-Modfile-Dictionary used during initialization
    std::unique_ptr<FieldlinesState>              _newState;         // Used for 'runtime-states' when switching out current state to a new state
    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;
    std::shared_ptr<TransferFunction>             _transferFunction; // Transfer function used to color lines when _pColorMethod is set to BY_QUANTITY

    // ------------------------------------ VECTORS ----------------------------------- //
    std::vector<std::string>     _colorTablePaths;      // Paths to color tables. One for each 'extraQuantity'
    std::vector<glm::vec2>       _colorTableRanges;     // Values represents min & max values represented in the color table
    std::vector<glm::vec2>       _maskingRanges;        // Values represents min & max limits for valid masking range
    std::vector<std::string>     _sourceFiles;          // Stores the provided source file paths if using 'runtime-states', else emptied after initialization
    std::vector<double>          _startTimes;           // Contains the _triggerTimes for all FieldlineStates in the sequence
    std::vector<FieldlinesState> _states;               // Stores the FieldlineStates

    // ---------------------------------- Properties ---------------------------------- //
    properties::PropertyOwner    _pColorGroup;          // Group to hold the color properties
    properties::OptionProperty   _pColorMethod;         // Uniform/transfer function/topology?
    properties::OptionProperty   _pColorQuantity;       // Index of the extra quantity to color lines by
    properties::StringProperty   _pColorQuantityMin;    // Color table/transfer function min
    properties::StringProperty   _pColorQuantityMax;    // Color table/transfer function max
    properties::StringProperty   _pColorTablePath;      // Color table/transfer function for "By Quantity" coloring
    properties::Vec4Property     _pColorUniform;        // Uniform Field Line Color
    properties::BoolProperty     _pColorABlendEnabled;  // Whether or not to use additive blending

    properties::BoolProperty     _pDomainEnabled;       // Whether or not to use Domain
    properties::PropertyOwner    _pDomainGroup;         // Group to hold the Domain properties
    properties::Vec2Property     _pDomainX;             // Domain Limits along x-axis
    properties::Vec2Property     _pDomainY;             // Domain Limits along y-axis
    properties::Vec2Property     _pDomainZ;             // Domain Limits along z-axis
    properties::Vec2Property     _pDomainR;             // Domain Limits radially

    properties::Vec4Property     _pFlowColor;           // Simulated particles' color
    properties::BoolProperty     _pFlowEnabled;         // Toggle flow [ON/OFF]
    properties::PropertyOwner    _pFlowGroup;           // Group to hold the flow/particle properties
    properties::IntProperty      _pFlowParticleSize;    // Size of simulated flow particles
    properties::IntProperty      _pFlowParticleSpacing; // Size of simulated flow particles
    properties::BoolProperty     _pFlowReversed;        // Toggle flow direction [FORWARDS/BACKWARDS]
    properties::IntProperty      _pFlowSpeed;           // Speed of simulated flow

    properties::BoolProperty     _pMaskingEnabled;      // Whether or not to use masking
    properties::PropertyOwner    _pMaskingGroup;        // Group to hold the masking properties
    properties::StringProperty   _pMaskingMin;          // Lower limit for allowed values
    properties::StringProperty   _pMaskingMax;          // Upper limit for allowed values
    properties::OptionProperty   _pMaskingQuantity;     // Index of the extra quantity to use for masking

    properties::TriggerProperty  _pFocusOnOriginBtn;    // Button which sets camera focus to parent node of the renderable
    properties::TriggerProperty  _pJumpToStartBtn;      // Button which executes a time jump to start of sequence

    // --------------------- FUNCTIONS USED DURING INITIALIZATION --------------------- //
    void   addStateToSequence(FieldlinesState& STATE);
    void   computeSequenceEndTime();
    void   definePropertyCallbackFunctions();
    bool   extractJsonInfoFromDictionary(fls::Model& model);
    bool   extractMandatoryInfoFromDictionary(SourceFileType& sourceFileType);
    void   extractOptionalInfoFromDictionary(std::string& outputFolderPath);
    void   extractOsflsInfoFromDictionary();
    void   extractTriggerTimesFromFileNames();
    bool   loadJsonStatesIntoRAM(const std::string& outputFolder);
    void   loadOsflsStatesIntoRAM(const std::string& outputFolder);
    void   setModelDependentConstants();
    void   setupProperties();
    bool   prepareForOsflsStreaming();
#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    // --- Initialization functions which require the kameleon module to be loaded! --- //
    bool   extractCdfInfoFromDictionary(std::string& seedFilePath,
                                        std::string& tracingVar,
                                        std::vector<std::string>& extraVars);
    bool   extractSeedPointsFromFile(const std::string& path,
                                     std::vector<glm::vec3>& outVec);
    void   extractMagnitudeVarsFromStrings(std::vector<std::string>& extraVars,
                                           std::vector<std::string>& extraMagVars);
    bool   getStatesFromCdfFiles(const std::string& outputFolder);
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

    // ------------------------- FUNCTIONS USED DURING RUNTIME ------------------------ //
    inline bool isWithinSequenceInterval(const double currentTime) const;
    void   readNewState(const std::string& filePath);
    void   updateActiveTriggerTimeIndex(const double currentTime);
    void   updateVertexPositionBuffer();
    void   updateVertexColorBuffer();
    void   updateVertexMaskingBuffer();
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
