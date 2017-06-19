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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__

#include <vector>
#include <ghoul/opengl/ghoul_gl.h> // TODO forward declare?
#include <ghoul/glm.h>

namespace openspace {

class FieldlinesState {
public:
    enum Model : int {
        batsrus = 0,
        enlil = 1,
        pfss = 2
    };

    FieldlinesState() {}
    FieldlinesState(size_t numLines);
    // ~FieldlinesState();

    // TODO: MOVE TO PRIVATE
    std::vector<glm::vec3>  _vertexPositions;
    std::vector<glm::vec4>  _vertexColors;

    std::vector<GLfloat>    _quickMorph;
    std::vector<GLint>      _lineStart;
    std::vector<GLsizei>    _lineCount;

    // TODO: start/trigger time as double in j2000 time
    double _triggerTime = -1.0;

    bool _isMorphable = false;

    std::vector<std::vector<float>> _extraVariables;
    std::vector<std::string> _extraVariableNames;

    std::string _modelName = "";
    Model _model;

    void reserveSize(size_t size);

    void saveStateToBinaryFile(const std::string& absoluteFilePath);
    void saveStateSubsetToBinaryFile(const std::string& absFilePath,
                                     const size_t& numPointsToSkipEachStep);

    void setModel(const Model& modelNumber);
private:
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__
