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
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/vec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <openspace/rendering/transferfunction.h>

#include <modules/fieldlinessequence/util/fieldlinesstate.h>

namespace ghoul {
    namespace opengl {
        class TextureUnit;
    }
}

namespace openspace {

class RenderableFieldlinesSequence : public Renderable {
public:
    RenderableFieldlinesSequence(const ghoul::Dictionary& dictionary);
    // ~RenderableFieldlinesSequence();

    bool initialize() override;
    bool deinitialize() override;

    bool isReady() const override;

    void render(const RenderData& data) override;
    void update(const UpdateData& data) override;
private:
    // -------------------- MAIN VARIABLES, STATE & TIME --------------------
    bool _needsUpdate  = false; // If still in same state as previous frame == false
    bool _shouldRender = false; // only temporary, unnecessary?

    std::vector<glm::vec3> _seedPoints; // TODO: no need to store these here?

    // State variables
    std::vector<FieldlinesState> _states;

    int _activeStateIndex = -1;
    size_t _numberOfStates;

    // Time variables
                                     // Sorted vector of each state's start time.
    std::vector<double> _startTimes; // Last item is == _seqEndTime (unnecessary?)

    double _currentTime;
    double _seqStartTime; // redundant, but hey.. nice n clear
    double _seqEndTime;

    float _stateProgress; // Time progression of active state. Range: 0 to 1


    // ------------------------- LUA .mod file info -------------------------
    ghoul::Dictionary _dictionary;

    // ----------------------------- Properties -----------------------------
    properties::BoolProperty _isClampingColorValues;// == false => fragments are discarded
    properties::BoolProperty _isMorphing;           // Time interpolation/morphing
    properties::BoolProperty _show3DLines;          // 3D "ropes". Billboards
    properties::BoolProperty _showSeedPoints;
    properties::BoolProperty _useABlending;         // Toggles additive blending on/off
    properties::BoolProperty _useNearestSampling;   // TF: Nearest or linear interpolation
    properties::BoolProperty _usePointDrawing;      // Toggles outputting lines vs points

    properties::FloatProperty _lineWidth;           // Interactive Line Width
    properties::FloatProperty _seedPointSize;       // Interactive Seed Point Size

    properties::IntProperty _fieldlineParticleSize; // Size of simulated line particles
    properties::IntProperty _modulusDivider;        // Related to simulated line particles
    properties::IntProperty _timeMultiplier;        // Related to simulated line particles

    properties::OptionProperty _colorMethod;        // Uniform/transfer function/topology?
    properties::OptionProperty _colorizingQuantity; // Which quantity to use in tf

    properties::PropertyOwner _colorGroup;
    properties::PropertyOwner _domainGroup;
    properties::PropertyOwner _particleGroup;
    properties::PropertyOwner _seedGroup;

    properties::StringProperty _transferFunctionPath;   // Color table for unit coloring
    properties::StringProperty _transferFunctionMinVal; // Value corresponding to 0 in tf
    properties::StringProperty _transferFunctionMaxVal; // Value corresponding to 1 in tf

    properties::Vec2Property _domainLimR;        // Radial domain in which to render
    properties::Vec2Property _domainLimX;        // Cartesian domain limits in which to
    properties::Vec2Property _domainLimY;        // render. The x component corresponds
    properties::Vec2Property _domainLimZ;        // to min value and y component to max
    properties::Vec2Property _unitInterestRange; // Value range of interest from TF

    properties::Vec4Property _fieldlineColor;         // Uniform fieldline color
    properties::Vec4Property _fieldlineParticleColor; // Simulated line particles' color
    properties::Vec4Property _uniformSeedPointColor;

    // -------- Colorize fieldline depending on additional variables --------
    bool _hasUnitColoring;
    bool _isSpherical;
    bool _updateColorBuffer = false;

    std::vector<glm::vec2> _transferFunctionLimits; // .x corresponds to 0 in tf, .y to 1
    std::vector<glm::vec2> _tFInterestRange;        // range of interest for each colorVar
    std::vector<glm::vec2> _tFInterestRangeLimits;  // limits of interest

    std::shared_ptr<TransferFunction> _transferFunction;        // Transfer funtion (tf)

    std::unique_ptr<ghoul::opengl::TextureUnit> _textureUnit;

    // --------------------------- OpenGL Related ----------------------------
    // shader program related
    std::unique_ptr<ghoul::opengl::ProgramObject> _seedPointProgram;
    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    std::unique_ptr<ghoul::opengl::ProgramObject> _ropeProgram;
    ghoul::opengl::ProgramObject* _activeProgramPtr;

    // Vertex Array Object
    GLuint _vertexArrayObject       = 0;
    GLuint _seedArrayObject         = 0;

    // Buffers // TODO: Make an array instead?
    GLuint _vertexPositionBuffer    = 0;
    GLuint _seedPositionBuffer      = 0;
    GLuint _vertexColorBuffer       = 0;
    GLuint _morphToPositionBuffer   = 0;
    GLuint _quickMorphBuffer        = 0;

    GLuint _vertAttrVertexPos       = 0;
    GLuint _vertAttrMorphToPos      = 1;
    GLuint _vertAttrMorphQuick      = 2;
    GLuint _vertAttrColorQuantity   = 3;

    GLenum _drawingOutputType       = GL_LINE_STRIP;            // Draw points or lines

    GLfloat _maxLineWidthOpenGl; // hardware related variable?
    float _scalingFactor;
    float _scalingFactorLineWidth;

    std::string _scalingFactorUnit;

    bool isWithinSequenceInterval();

    void updateActiveStateIndex();
    void updateColorBuffer();
    void updateMorphingBuffers();
    void updateVertexPosBuffer();
    bool getSourceFilesFromDictionary(const std::string& fileExt, std::vector<std::string>& validSourceFilePaths);
    bool getSeedPointsFromDictionary();
    bool getUnsignedIntFromModfile(const std::string& key, unsigned int& val);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
