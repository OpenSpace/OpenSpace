/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/volume/envelope.h>

#include <ghoul/lua/ghoul_lua.h>

using json = nlohmann::json;

namespace openspace::volume {

EnvelopePoint::EnvelopePoint(glm::vec3 c, float x, float y)
    : color(c)
    , colorHex(hexadecimalFromVec3(std::move(c)))
    , position({ x, y })
{}

EnvelopePoint::EnvelopePoint(std::string c, float x, float y)
    : color(hexadecimalToRGBConversion(c))
    , colorHex(std::move(c))
    , position(std::make_pair(x, y))
{}

Envelope::Envelope(std::vector<EnvelopePoint> vec) {
    _points = std::move(vec);
}

bool Envelope::operator!=(const Envelope& env) const {
    constexpr const double MinDist = 0.0001;

    if (_points.size() != env._points.size()) {
        return true;
    }

    for (auto iter = _points.begin(), envIter = env._points.begin();
         iter != _points.end();
         ++iter, ++envIter)
    {
        if (abs(iter->position.first - envIter->position.first) > MinDist ||
            abs(iter->position.second - envIter->position.second) > MinDist ||
            iter->color != envIter->color)
        {
            return true;
        }
    }
    return false;
}

void Envelope::setPoints(std::vector<EnvelopePoint> vec) {
    _points = std::move(vec);
}

const std::vector<EnvelopePoint>& Envelope::points() const {
    return _points;
}

bool Envelope::isValueInEnvelope(float pos) const {
    return !_points.empty() && (_points.front().position.first <= pos) &&
           (_points.back().position.first >= pos);
}

bool Envelope::isEnvelopeValid() const {
    for (auto currentIter = _points.begin(), nextIter = currentIter + 1;
         nextIter != _points.end();
         ++currentIter, ++nextIter)
    {
        if (currentIter->position.first > nextIter->position.first)
            return false;
    }
    return true;
}

glm::vec3 Envelope::normalizeColor(glm::vec3 vec) const {
    return { vec.r / 255.f, vec.g / 255.f , vec.b / 255.f };
}

glm::vec4 Envelope::valueAtPosition(float pos) const {
    auto afterIter = _points.begin();
    while (afterIter->position.first < pos ) {
        if (afterIter == _points.end()) {
            return { 0.f, 0.f, 0.f ,0.f };
        }
        ++afterIter;
    }
    if (afterIter->position.first == pos) {
        return { afterIter->color, afterIter->position.second };
    }
    auto beforeIter = afterIter - 1;

    float dist = afterIter->position.first - beforeIter->position.first;
    if (dist < 0.0001) {
        return {
            normalizeColor((beforeIter->color + afterIter->color) / 2.f),
            std::max(beforeIter->position.second, afterIter->position.second)
        };
    }
    else {
        return {
            normalizeColor(
                beforeIter->color * (abs(pos - afterIter->position.first) / dist) +
                afterIter->color * (abs(pos - beforeIter->position.first) / dist)
            ),
            beforeIter->position.second * (abs(pos - afterIter->position.first) / dist) +
                afterIter->position.second *
                (abs(pos - beforeIter->position.first) / dist)
        };
    }
}

int EnvelopePoint::hexadecimalToDecimal(const std::string& hex) const {
    const size_t hexLength = hex.length();
    double dec = 0;
    for (size_t i = 0; i < hexLength; ++i) {
        char b = hex[i];

        if (b >= 48 && b <= 57) {
            b -= 48;
        }
        else if (b >= 65 && b <= 70) {
            b -= 55;
        }
        else if (b >= 97 && b <= 102) {
            b -= 87;
        }
        dec += b * pow(16, ((hexLength - i) - 1));
    }
    return static_cast<int>(dec);
}

std::string EnvelopePoint::decimalToHexadecimal(int dec) const {
    if (dec < 1) {
        return "00";
    }

    int hex = dec;
    std::string hexStr;

    while (dec > 0) {
        hex = dec % 16;

        if (hex < 10) {
            hexStr = hexStr.insert(0, std::string(1, static_cast<char>(hex + 48)));
        }
        else {
            hexStr = hexStr.insert(0, std::string(1, static_cast<char>(hex + 55)));
        }
        dec /= 16;
    }
    return hexStr;
}

glm::vec3 EnvelopePoint::hexadecimalToRGBConversion(const std::string& hex) const {
    const float r = static_cast<float>(hexadecimalToDecimal(hex.substr(1, 2)));
    const float g = static_cast<float>(hexadecimalToDecimal(hex.substr(3, 2)));
    const float b = static_cast<float>(hexadecimalToDecimal(hex.substr(5, 2)));

    return glm::vec3(r, g, b);
}

std::string EnvelopePoint::hexadecimalFromVec3(const glm::vec3& vec) const {
    std::string r = decimalToHexadecimal(static_cast<int>(vec.r));
    std::string g = decimalToHexadecimal(static_cast<int>(vec.g));
    std::string b = decimalToHexadecimal(static_cast<int>(vec.b));

    return ("#" + r + g + b);
}

json Envelope::jsonPoints() const {
    json j;
    for (size_t i = 0; i < _points.size(); i++) {
        j[i] = {
            {
                "color",
                _points.at(i).colorHex
            },
            {
                "position",
                {
                    { "x", _points.at(i).position.first },
                    { "y", _points.at(i).position.second },
                },
            }
        };
    }
    return j;
}

json Envelope::jsonEnvelope() const {
    json j;
    j["points"] = jsonPoints();
    return j;
}

void Envelope::setEnvelopeLuaTable(lua_State* state) const {
    for (auto iter = _points.begin(); iter != _points.end(); ++iter) {
        lua_newtable(state);
        ghoul::lua::push(state, iter->colorHex);
        lua_setfield(state, -2, "color");
        lua_newtable(state);
        ghoul::lua::push(state, iter->position.first);
        lua_setfield(state, -2, "x");
        ghoul::lua::push(state, iter->position.second);
        lua_setfield(state, -2, "y");
        lua_setfield(state, -2, "position");
        lua_setfield(
            state,
            -2,
            ("[\"" + std::to_string(iter - _points.begin() + 1) + "\"]").c_str()
        );
    }
}

}  //namespace openspace::volume
