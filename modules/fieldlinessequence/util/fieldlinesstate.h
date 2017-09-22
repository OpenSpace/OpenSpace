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

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>

#include <string>
#include <vector>

using std::vector;

namespace openspace {

class FieldlinesState {
public:
    enum Model : int {
        batsrus = 0,
        enlil   = 1,
        pfss    = 2
    };

    FieldlinesState();
    FieldlinesState(const std::string& PATH_TO_OSFLS_FILE, bool& loadSucessful);

    bool   loadStateFromOsfls(const std::string& PATH_TO_OSFLS_FILE);

    // ------------------------------GETTERS-----------------------------------------//
    const vector<vector<float>>& extraQuantities()    const { return _extraQuantities; }
    const vector<std::string>&   extraQuantityNames() const { return _extraQuantityNames; }
    const vector<GLsizei>&       lineCount()          const { return _lineCount; }
    const vector<GLint>&         lineStart()          const { return _lineStart; }
    size_t                       nExtraQuantities()   const { return _extraQuantities.size(); }
    Model                        model()              const { return _model; }
    double                       triggerTime()        const { return _triggerTime; }
    const vector<glm::vec3>&     vertexPositions()    const { return _vertexPositions; }

    // Special getter. Returns extraQuantities[INDEX].
    const vector<float>&       extraQuantity(const size_t INDEX, bool& isSuccesful) const;

private:
    bool    _isMorphable = false;
    double  _triggerTime = -1.0;
    Model   _model;

    vector<glm::vec3>          _vertexPositions;
    vector<GLint>              _lineStart;
    vector<GLsizei>            _lineCount;
    vector<vector<float>>      _extraQuantities;
    vector<std::string>        _extraQuantityNames;
    // TODO: Maybe introduce a vector containing seed point indices
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__
