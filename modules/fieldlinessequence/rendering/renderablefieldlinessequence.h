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

namespace openspace {

class ghoul::Dictionary;

class RenderableFieldlinesSequence : public Renderable {
public:
    RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary);
    // ~RenderableFieldlinesSequence();

    void initialize() override;
    void deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;
private:
    enum ColorMethod : int {
        UNIFORM = 0,
        BY_QUANTITY
    };

    enum SourceFileType : int {
        CDF = 0,
        JSON,
        OSFLS,
        INVALID
    };

    std::string _name;

    int             _activeStateIndex          = -1;
    int             _activeTriggerTimeIndex    = -1;
    bool            _loadingStatesDynamically  = false;  // False => loading osfls files into RAM in initializing step
    bool            _mustLoadNewStateFromDisk  = false;
    bool            _needsUpdate               = false; // If still in same state as previous frame == false
    bool            _shouldUpdateColorBuffer   = false;
    bool            _shouldUpdateMaskingBuffer = false;
    FieldlinesState _newState;
    size_t          _nStates                   = 0;
    float           _scalingFactor             = 1.f;
    double          _sequenceEndTime;
    SourceFileType  _sourceFileType;

    std::atomic<bool> _isLoadingStateFromDisk{false};
    std::atomic<bool> _newStateIsReady{false};

    std::unique_ptr<ghoul::Dictionary>            _dictionary;
    std::shared_ptr<TransferFunction>             _transferFunction;        // Transfer funtion (tf)
    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;

    std::vector<std::string>     _colorTablePaths {"${OPENSPACE_DATA}/colortables/kroyw.txt"}; // Default in case user doesn't specify otherwise
    std::vector<std::string>     _sourceFiles;                // Stored in RAM if files are loaded at runtime, else emptied after initialization
    std::vector<double>          _startTimes;
    std::vector<FieldlinesState> _states;
    std::vector<glm::vec2>       _colorTableRanges;       // Values represents min & max values represented in the color table
    std::vector<glm::vec2>       _maskingRanges;          // Values represents min & max values for valid masking range

    GLuint _vertexArrayObject       = 0;
    GLuint _vertexPositionBuffer    = 0;
    GLuint _vertexColorBuffer       = 0;
    GLuint _vertexMaskingBuffer     = 0;

    // THESE MUST CORRESPOND TO THE SHADER PROGRAM
    // TODO: THIS CAN BE DETERMINED BY ASKING THE SHADER PROGRAM TOO
    const GLuint _VA_POSITION = 0;
    const GLuint _VA_COLOR    = 1;
    const GLuint _VA_MASKING  = 2;

    // ----------------------------- Properties -----------------------------
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

    void   computeSequenceEndTime();
    bool   extractInfoFromDictionary(const ghoul::Dictionary& dictionary);
    void   extractTriggerTimesFromFileNames();
    void   readNewState(const std::string& FILEPATH);

    inline bool isWithinSequenceInterval(const double CURRENT_TIME) const;

    void   updateActiveTriggerTimeIndex(const double CURRENT_TIME);
    void   updateVertexPositionBuffer();
    void   updateVertexColorBuffer();
    void   updateVertexMaskingBuffer();

    void   definePropertyCallbackFunctions();
    void   setupProperties();

    void   setModelDependentConstants();
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
