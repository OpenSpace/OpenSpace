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
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/transferfunction.h>


#include <modules/fieldlinessequence/util/fieldlinesstate.h>

#include <atomic>

namespace openspace {

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
    enum SourceFileType : int {
        CDF = 0,
        JSON,
        OSFLS,
        INVALID
    };

    int             _activeStateIndex         = -1;
    int             _activeTriggerTimeIndex   = -1;
    bool            _isLoadingStatesAtRuntime = true;  // False => loading osfls at runtime
    bool            _mustLoadNewStateFromDisk = false;
    bool            _needsUpdate              = false; // If still in same state as previous frame == false
    bool            _shouldUpdateColorBuffer  = false;
    FieldlinesState _newState;
    size_t          _nStates                  = 0;
    double          _sequenceEndTime;
    SourceFileType  _sourceFileType;

    std::atomic<bool> _isLoadingStateFromDisk{false};
    std::atomic<bool> _newStateIsReady{false};

    std::shared_ptr<TransferFunction>             _transferFunction;        // Transfer funtion (tf)
    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;

    std::string*                 _activeColorTable;
    std::vector<std::string>     _colorTablePaths {"${OPENSPACE_DATA}/colortables/kroyw.txt"}; // Default in case user doesn't specify otherwise
    std::vector<std::string>     _sourceFiles;                // Stored in RAM if files are loaded at runtime, else emptied after initialization
    std::vector<double>          _startTimes;
    std::vector<FieldlinesState> _states;
    std::vector<glm::vec2>       _colorTableRanges;       // Values represents min & max values represented in the color table

    GLuint _vertexArrayObject       = 0;
    GLuint _vertexPositionBuffer    = 0;
    GLuint _vertexColorBuffer       = 0;

    // THESE MUST CORRESPOND TO THE SHADER PROGRAM
    // TODO: THIS CAN BE DETERMINED BY ASKING THE SHADER PROGRAM TOO
    const GLuint _VA_POSITION = 0;
    const GLuint _VA_COLOR    = 1;

    // ----------------------------- Properties -----------------------------
    properties::PropertyOwner  _colorGroup;          // Group to hold the color properties
    properties::OptionProperty _colorMethod;         // Uniform/transfer function/topology?
    properties::OptionProperty _colorQuantity;       // Index of the extra quantity to color lines by
    properties::StringProperty _colorQuantityMin;    // Color table/transfer function min
    properties::StringProperty _colorQuantityMax;    // Color table/transfer function max
    properties::StringProperty _colorTablePath;      // Color table/transfer function for "By Quantity" coloring
    properties::Vec4Property   _colorUniform;        // Uniform Field Line Color

    properties::Vec4Property   _flowColor;           // Simulated particles' color
    properties::BoolProperty   _flowEnabled;         // Toggle flow [ON/OFF]
    properties::PropertyOwner  _flowGroup;           // Gropu to hold the flow/particle properties
    properties::IntProperty    _flowParticleSize;    // Size of simulated flow particles
    properties::IntProperty    _flowParticleSpacing; // Size of simulated flow particles
    properties::BoolProperty   _flowReversed;        // Toggle flow direction [FORWARDS/BACKWARDS]
    properties::IntProperty    _flowSpeed;           // Speed of simulated flow


    void   computeSequenceEndTime();
    bool   extractInfoFromDictionary(const ghoul::Dictionary& dictionary);
    void   extractTriggerTimesFromFileNames();
    void   readNewState(const std::string& FILEPATH);

    inline bool isWithinSequenceInterval(const double CURRENT_TIME) const;

    void   updateActiveTriggerTimeIndex(const double CURRENT_TIME);
    void   updateVertexPositionBuffer();
    void   updateVertexColorBuffer();
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
