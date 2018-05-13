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

namespace openspace::volume {

EnvelopePoint::EnvelopePoint(glm::vec3 c, float x, float y)
    : color(c)
    , colorHex(getHexadecimalFromVec3(c))
    , position(std::make_pair(x, y))
{}

EnvelopePoint::EnvelopePoint(std::string c, float x, float y)
    : color(hexadecimalToRGBConversion(c))
    , colorHex(c)
    , position(std::make_pair(x, y))
{}

Envelope::Envelope() {}

Envelope::Envelope(std::vector<EnvelopePoint> vec) {
    _points = vec;
}

bool Envelope::operator!=(const Envelope& env) const {
    const double minDist = 0.0001;

    if (_points.size() != env._points.size()) {
        return true;
    }

    auto iter = _points.begin();
    auto envIter = env._points.begin();
    for (; iter != _points.end(); ++iter, ++envIter) {
        if (abs(iter->position.first - envIter->position.first) > minDist ||
            abs(iter->position.second - envIter->position.second) > minDist ||
             iter->color != envIter->color)
            return true;
    }
    return false;
}


void Envelope::setPoints(std::vector<EnvelopePoint> vec) {
    this->_points = vec;
}

std::vector<EnvelopePoint> Envelope::getPoints() {
    return _points;
}

bool Envelope::isValueInEnvelope(float pos) const {
    if (!_points.empty()) {
        if (_points.front().position.first <= pos && _points.back().position.first >= pos)
            return true;
    }
    return false;
}

bool Envelope::isEnvelopeValid() const {
    auto currentIter = _points.begin();
    auto nextIter = currentIter + 1;
    for (; nextIter != _points.end(); ++currentIter, ++nextIter) {
        if (currentIter->position.first > nextIter->position.first)
            return false;
    }
    return true;
}

glm::vec3 Envelope::normalizeColor(glm::vec3 vec) const{

    return glm::vec3{ vec.r / 255.f, vec.g / 255.f , vec.b / 255.f };
}

glm::vec4 Envelope::getValueAtPosition(float pos) const {
    auto afterIter = _points.begin();
    glm::vec4 color{ 0.f, 0.f , 0.f , 0.f };
    while (afterIter->position.first < pos ) {
        if(afterIter == _points.end())
            return color;
        ++afterIter;
    }
    if (afterIter->position.first == pos) {
        return glm::vec4{ afterIter->color, afterIter->position.second };
    }
    auto beforeIter = afterIter -1;

    float dist = afterIter->position.first - beforeIter->position.first;
    float alpha;
    if(dist < 0.0001)
        color = { normalizeColor((beforeIter->color + afterIter->color) / 2.f),
                std::max(beforeIter->position.second, afterIter->position.second) };
    else
        color = {
            normalizeColor(
                beforeIter->color * (abs(pos - afterIter->position.first) / dist) +
                afterIter->color * (abs(pos - beforeIter->position.first) / dist)
            ),
            beforeIter->position.second * (abs(pos - afterIter->position.first) / dist) +
                afterIter->position.second *
                (abs(pos - beforeIter->position.first) / dist)
        };

    return color;
}

int EnvelopePoint::HexadecimalToDecimal(std::string hex) const {
    int hexLength = hex.length();
    double dec = 0;
    for (int i = 0; i < hexLength; ++i)
    {
        char b = hex[i];

        if (b >= 48 && b <= 57)
            b -= 48;
        else if (b >= 65 && b <= 70)
            b -= 55;
        else if (b >= 97 && b <= 102)
            b -= 87;
        dec += b * pow(16, ((hexLength - i) - 1));
    }
    return (int)dec;
}

std::string EnvelopePoint::DecimalToHexadecimal(int dec) const {
    if (dec < 1) return "00";

    int hex = dec;
    std::string hexStr = "";

    while (dec > 0)
    {
        hex = dec % 16;

        if (hex < 10)
            hexStr = hexStr.insert(0, std::string(1, (hex + 48)));
        else
            hexStr = hexStr.insert(0, std::string(1, (hex + 55)));
        dec /= 16;
    }
    return hexStr;
}

glm::vec3 EnvelopePoint::hexadecimalToRGBConversion(std::string hex) const {
    float r = static_cast<float>(HexadecimalToDecimal(hex.substr(1, 2)));
    float g = static_cast<float>(HexadecimalToDecimal(hex.substr(3, 2)));
    float b = static_cast<float>(HexadecimalToDecimal(hex.substr(5, 2)));

    return glm::vec3(r, g, b);
}

std::string EnvelopePoint::getHexadecimalFromVec3(glm::vec3 vec) const {

    std::string r = DecimalToHexadecimal(static_cast<int>(vec.r));
    std::string g = DecimalToHexadecimal(static_cast<int>(vec.g));
    std::string b = DecimalToHexadecimal(static_cast<int>(vec.b));

    return ("#" + r + g + b);
}

json Envelope::getJSONPoints() const {
    json j;
    for (int i = 0; i < _points.size(); i++) {
        j[i] = {
            { "color", _points.at(i).colorHex },
            { "position",{
                { "x", _points.at(i).position.first },
                { "y", _points.at(i).position.second },
            },
            }
        };
    }
    return j;
}

json Envelope::getJSONEnvelope() const {
    json j;
    j["points"] = getJSONPoints();
    return j;
}

void Envelope::setEnvelopeLuaTable(lua_State* state) const {
    for (auto iter = _points.begin(); iter != _points.end(); ++iter) {
        lua_newtable(state);
        lua_pushstring(state, iter->colorHex.c_str());
        lua_setfield(state, -2, "color");
        lua_newtable(state);
        lua_pushnumber(state, static_cast<lua_Number>(iter->position.first));
        lua_setfield(state, -2, "x");
        lua_pushnumber(state, static_cast<lua_Number>(iter->position.second));
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
