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

#include <modules/volume/transferfunction.h>

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/opengl/texture.h>
#include <fstream>

using json = nlohmann::json;

namespace openspace::volume {

TransferFunction::TransferFunction(const std::string& s) {
    setEnvelopesFromString(s);
}

bool TransferFunction::setEnvelopesFromString(const std::string& s) {
    json j = json::parse(s);
    for (json::iterator it = j.begin(); it != j.end(); ++it) {
        Envelope env;
        std::vector<EnvelopePoint> tmpVec;
        for (size_t i = 0; i < 4; i++) {
            std::string color = (*it)["points"][i]["color"].get<std::string>();
            float x_value = (*it)["points"][i]["position"]["x"].get<float>();
            float y_value = (*it)["points"][i]["position"]["y"].get<float>();
            tmpVec.emplace_back(color, x_value, y_value);
        }
        env.setPoints(tmpVec);
        _envelopes.emplace_back(env);
    }
    return true;
}
//TODO, implement this
bool TransferFunction::setEnvelopesFromLua(lua_State* state) {
    ghoul_assert(false, "Implement this");

    bool success = (lua_istable(state, -1) == 1);
    if (success) {
        lua_pushnil(state);
        while (lua_next(state, -2)) {
            Envelope env;
            std::vector<EnvelopePoint> tmpVec;

            /*lua_pushnil(state);
            while (lua_next(state, -2)) {
                lua_pushnil(state);
                while (lua_next(state, -2)) {
                    PrintTable(state);
                    std::string color = static_cast<std::string>(lua_tostring(state, -1));
                    lua_pop(state, 1);
                    lua_pushnil(state);
                    lua_next(state, -2);
                    float x_value = static_cast<float>(lua_tonumber(state, -1));

                    lua_pop(state, 1);
                    lua_next(state, -2);
                    float y_value = static_cast<float>(lua_tonumber(state, -1));
                    lua_pop(state, 1);
                    tmpVec.emplace_back(color, x_value, y_value);
                    lua_pop(state, 1);
                }
            }*/
            lua_pop(state, 2);
        }
        lua_pop(state, 1);
    }
    return success;
}

bool TransferFunction::envelopesToLua(lua_State* state) {
    lua_newtable(state);
    for (auto iter = _envelopes.begin(); iter != _envelopes.end(); ++iter) {
        lua_newtable(state);
        iter->setEnvelopeLuaTable(state);
        lua_setfield(
            state,
            -2,
            ("[\"" + std::to_string(iter - _envelopes.begin() + 1) + "\"]").c_str()
        );
    }
    return true;
}

void TransferFunction::loadEnvelopesFromFile(const std::string& path) {
    lua_State* L = luaL_newstate();
    ghoul::Dictionary dictionary;
    ghoul::lua::loadDictionaryFromFile(path, dictionary, L);

    const std::vector<std::string>& tfKeys = dictionary.keys();
    for (const std::string& key : tfKeys) {
        ghoul::Dictionary tfDictionary = dictionary.value<ghoul::Dictionary>(key);

        const std::vector<std::string>& envelopeKeys = tfDictionary.keys();
        for (const std::string& envelopeKey : envelopeKeys) {
            ghoul::Dictionary envelopeDictionary =
                tfDictionary.value<ghoul::Dictionary>(envelopeKey);
            const std::vector<std::string>& pointKeys = envelopeDictionary.keys();
            Envelope env;
            std::vector<EnvelopePoint> tmpVec;
            for (const std::string& pointKey : pointKeys) {
                ghoul::Dictionary pointDictionary =
                    envelopeDictionary.value<ghoul::Dictionary>(pointKey);

                ghoul::Dictionary positionDictionary =
                    pointDictionary.value<ghoul::Dictionary>("position");

                std::string color = pointDictionary.value<std::string>("color");
                float posX = positionDictionary.value<float>("x");
                float posY = positionDictionary.value<float>("y");
                tmpVec.emplace_back(color, posX, posY);
            }
            env.setPoints(tmpVec);
            _envelopes.emplace_back(env);
        }
    }
}

void TransferFunction::saveEnvelopesToFile(const std::string& path) {
    ghoul::Dictionary dictionary;
    lua_State* state = luaL_newstate();
    envelopesToLua(state);

    ghoul::lua::luaDictionaryFromState(state, dictionary);
    ghoul::DictionaryLuaFormatter formatter;
    std::ofstream tfFile;
    tfFile.open(path);
    tfFile << "return {";
    tfFile << formatter.format(dictionary);
    tfFile << "}";
    tfFile.close();
}

bool TransferFunction::operator!=(const TransferFunction& tf) {
    if (_envelopes.size() != tf._envelopes.size()) {
        return true;
    }

    if (_loadableFilePath != tf._loadableFilePath) {
        return true;
    }

    auto iter = _envelopes.begin();
    auto tfIter = tf._envelopes.begin();
    for (; iter != _envelopes.end(); ++iter, ++tfIter) {
        if (*iter != *tfIter)
            return true;
    }
    return false;
}

bool TransferFunction::hasEnvelopes() const {
    return !_envelopes.empty();
}

std::string TransferFunction::serializedToString() const {
    if (_envelopes.empty()) {
        return "";
    }
    json j;
    for (auto envIter = _envelopes.begin(); envIter != _envelopes.end(); ++envIter) {
        j[std::distance(_envelopes.begin(), envIter)] = { envIter->jsonEnvelope() };
    }
    return j.dump();
}

bool TransferFunction::createTexture(ghoul::opengl::Texture& ptr) {
    if (_envelopes.empty()) {
        return false;
    }
    else {
        float* transferFunction = new float[_width * 4];
        std::memset(transferFunction, 0, _width * 4 * sizeof(float));

        for (int i = 0; i < _width ; ++i) {
            const float position = static_cast<float>(i) / static_cast<float>(_width);
            int count = 0;
            glm::vec4 rgbFromEnvelopes(0.f, 0.f, 0.f, 0.f);
            float alpha = 0.f;
            for (const Envelope& env : _envelopes) {
                if (env.isValueInEnvelope(position) && env.isEnvelopeValid()) {
                    count++;
                    const glm::vec4 tmp = env.valueAtPosition(position);
                    rgbFromEnvelopes.r += tmp.r * tmp.a;
                    rgbFromEnvelopes.g += tmp.g * tmp.a;
                    rgbFromEnvelopes.b += tmp.b * tmp.a;
                    alpha = std::min(alpha, tmp.a);
                }
            }
            rgbFromEnvelopes /= (count == 0) ? 1.f : static_cast<float>(count);
            rgbFromEnvelopes.w = alpha;

            for (int channel = 0; channel < 4; ++channel) {
                const int p = 4 * i + channel;
                const float value = rgbFromEnvelopes[channel];
                transferFunction[p] = value;
            }
        }
        ptr.setPixelData(transferFunction);
        return true;
    }
}

} // namespace openspace::volume

