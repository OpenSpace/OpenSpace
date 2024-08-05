/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_MODULE_VOLUME___ENVELOPE___H__
#define __OPENSPACE_MODULE_VOLUME___ENVELOPE___H__

#include <openspace/json.h>
#include <ghoul/glm.h>

struct lua_State;

namespace openspace::volume {

class EnvelopePoint {
public:
    EnvelopePoint(glm::vec3 c, float x, float y);
    EnvelopePoint(std::string c, float x, float y);

    int hexadecimalToDecimal(const std::string& hex) const;
    std::string decimalToHexadecimal(int dec) const;
    glm::vec3 hexadecimalToRGBConversion(const std::string& hex) const;
    std::string hexadecimalFromVec3(const glm::vec3& vec) const;

    glm::vec3 color = glm::vec3(0.f);
    std::string colorHex;
    std::pair<float, float> position;
};

class Envelope {
public:
    Envelope() = default;
    Envelope(std::vector<EnvelopePoint> vec);

    void setPoints(std::vector<EnvelopePoint> vec);
    const std::vector<EnvelopePoint>& points() const;

    glm::vec4 valueAtPosition(float pos) const;
    glm::vec3 normalizeColor(const glm::vec3& vec) const;
    nlohmann::json jsonPoints() const;
    nlohmann::json jsonEnvelope() const;
    void setEnvelopeLuaTable(lua_State* state) const;

    bool isValueInEnvelope(float pos) const;
    bool isEnvelopeValid() const;

    bool operator!=(const Envelope& env) const;

private:
    std::vector<EnvelopePoint> _points;
};

} //namespace openspace::volume

#endif // __OPENSPACE_MODULE_VOLUME___ENVELOPE___H__
