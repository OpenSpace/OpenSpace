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

#include <modules/fieldlinessequence/util/fieldlinesstate.h>

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

    int            _activeStateIndex         = -1;
    bool           _needsUpdate              = false; // If still in same state as previous frame == false
    bool           _isLoadingStatesAtRuntime = false;  // False => loading osfls at runtime
    size_t         _nStates                  = 0;
    double         _sequenceEndTime;
    SourceFileType _sourceFileType;

    std::unique_ptr<ghoul::opengl::ProgramObject> _shaderProgram;

    std::vector<double>          _startTimes;
    std::vector<FieldlinesState> _states;
    std::vector<std::string>     _sourceFiles;                // Stored in RAM if files are loaded at runtime, else emptied after initialization

    GLuint _vertexArrayObject       = 0;
    GLuint _vertexPositionBuffer    = 0;

    // THESE MUST CORRESPOND TO THE SHADER PROGRAM
    // TODO: THIS CAN BE DETERMINED BY ASKING THE SHADER PROGRAM TOO
    GLuint _vertAttrVertexPos = 0;

    void computeSequenceEndTime();
    bool extractInfoFromDictionary(const ghoul::Dictionary& dictionary);
    inline bool isWithinSequenceInterval(const double CURRENT_TIME);
    inline void updateActiveStateIndex(const double CURRENT_TIME);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___RENDERABLEFIELDLINESSEQUENCE___H__
