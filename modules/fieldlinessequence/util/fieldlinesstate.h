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

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    bool addLinesFromKameleon(ccmc::Kameleon* kameleon,
                              const std::vector<glm::vec3>& seedPoints,
                              const std::string tracingVar);
    void addExtraQuantities(ccmc::Kameleon* kameleon,
                            std::vector<std::string>& xtraScalarVars,
                            std::vector<std::string>& xtraMagVars);
    void   convertLatLonToCartesian(const float scale = 1.f);
    void   scalePositions(const float scale);
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED

    bool   loadStateFromOsfls(const std::string& pathToOsflsFile);
    void   saveStateToOsfls(const std::string& pathToOsflsFile);

    bool   loadStateFromJson(const std::string& pathToJsonFile,
                             const fls::Model model, const float coordToMeters);
    void   saveStateToJson(const std::string& pathToJsonFile);

    // ----------------------------------- GETTERS ----------------------------------- //
    const std::vector<std::vector<float>>& extraQuantities()    const { return _extraQuantities; }
    const std::vector<std::string>&        extraQuantityNames() const { return _extraQuantityNames; }
    const std::vector<GLsizei>&            lineCount()          const { return _lineCount; }
    const std::vector<GLint>&              lineStart()          const { return _lineStart; }
    size_t                                 nExtraQuantities()   const { return _extraQuantities.size(); }
    fls::Model                             model()              const { return _model; }
    double                                 triggerTime()        const { return _triggerTime; }
    const std::vector<glm::vec3>&          vertexPositions()    const { return _vertexPositions; }

    // Special getter. Returns extraQuantities[INDEX].
    const std::vector<float>& extraQuantity(const size_t INDEX, bool& isSuccesful) const;

    void setTriggerTime(const double T) { _triggerTime = T; }

private:
    bool                            _isMorphable = false;
    double                          _triggerTime = -1.0;
    fls::Model                      _model;

    std::vector<std::vector<float>> _extraQuantities;
    std::vector<std::string>        _extraQuantityNames;
    std::vector<GLsizei>            _lineCount;
    std::vector<GLint>              _lineStart;
    std::vector<glm::vec3>          _vertexPositions;

#ifdef OPENSPACE_MODULE_KAMELEON_ENABLED
    void loadExtrasIntoKameleon(ccmc::Kameleon* kameleon,
                                std::vector<std::string>& xtraScalarVars,
                                std::vector<std::string>& xtraMagVars);
#endif // OPENSPACE_MODULE_KAMELEON_ENABLED
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___FIELDLINESSTATE___H__
