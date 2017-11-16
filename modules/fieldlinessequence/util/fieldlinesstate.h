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

#include <modules/fieldlinessequence/util/commons.h>

#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/glm.h>

#include <string>
#include <vector>

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
namespace ccmc {
    class Kameleon;
}
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

namespace openspace {

class FieldlinesState {
public:
    void convertLatLonToCartesian(const float scale = 1.f);
    void scalePositions(const float scale);

    bool loadStateFromOsfls(const std::string& pathToOsflsFile);
    void saveStateToOsfls(const std::string& pathToOsflsFile);

    bool loadStateFromJson(const std::string& pathToJsonFile,
                           const fls::Model model, const float coordToMeters);
    void saveStateToJson(const std::string& pathToJsonFile);

    // ----------------------------------- GETTERS ----------------------------------- //
    const std::vector<std::vector<float>>& extraQuantities()    const;
    const std::vector<std::string>&        extraQuantityNames() const;
    const std::vector<GLsizei>&            lineCount()          const;
    const std::vector<GLint>&              lineStart()          const;
    fls::Model                             model()              const;
    size_t                                 nExtraQuantities()   const;
    double                                 triggerTime()        const;
    const std::vector<glm::vec3>&          vertexPositions()    const;

    // Special getter. Returns extraQuantities[INDEX].
    std::vector<float> extraQuantity(const size_t INDEX, bool& isSuccesful) const;

    void setModel(const fls::Model m)   { _model = m; }
    void setTriggerTime(const double t) { _triggerTime = t; }
    void setExtraQuantityNames(std::vector<std::string>& names);

    void addLine(std::vector<glm::vec3>& line);
    void appendToExtra(size_t idx, float val) { _extraQuantities[idx].push_back(val); }

private:
    bool                            _isMorphable = false;
    double                          _triggerTime = -1.0;
    fls::Model                      _model;

    std::vector<std::vector<float>> _extraQuantities;
    std::vector<std::string>        _extraQuantityNames;
    std::vector<GLsizei>            _lineCount;
    std::vector<GLint>              _lineStart;
    std::vector<glm::vec3>          _vertexPositions;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__
