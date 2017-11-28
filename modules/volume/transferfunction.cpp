/****************************************************************************************
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

#include <modules/volume/transferfunction.h>
#include <ext/json/json.hpp>
#include <memory>

using json = nlohmann::json;

namespace openspace {
    namespace volume {

        TransferFunction::TransferFunction() {
        }

        TransferFunction::TransferFunction(int s) {
        }
        
        TransferFunction::TransferFunction(std::string s) {
            setEnvelopesFromString(s);
        }

        TransferFunction::TransferFunction(const TransferFunction& tf) : _envelopes(tf._envelopes) { }

        TransferFunction::TransferFunction(TransferFunction&& tf) : _envelopes(std::move(tf._envelopes)) { }

        TransferFunction& TransferFunction::operator=(const TransferFunction& tf) {
            _envelopes = tf._envelopes;
            return *this;
        }

        TransferFunction& TransferFunction::operator=(TransferFunction&& tf) {
            _envelopes = std::move(tf._envelopes);
            return *this;
        }

        bool TransferFunction::setEnvelopesFromString(std::string s) {
            json j = json::parse(s);
            for (json::iterator it = j.begin(); it != j.end(); ++it) {
                Envelope env;
                std::vector < EnvelopePoint> tmpVec;
                int height = (*it)["height"].get<int>();
                int width = (*it)["width"].get<int>();
                for (size_t i = 0; i < 4; i++) {
                    std::string color = (*it)["points"][i]["color"].get<std::string>();
                    int x_value = (*it)["points"][i]["position"]["x"].get<int>();
                    int y_value = (*it)["points"][i]["position"]["y"].get<int>();
                    tmpVec.emplace_back(color, static_cast<float>(x_value) / width, static_cast<float>(y_value) / height);
                }
                env.setPoints(tmpVec);
                _envelopes.emplace_back(env);
            }
            return true;
        }

        bool TransferFunction::setEnvelopesFromLua(lua_State* state) {
            bool success = (lua_istable(state, -1) == 1);
            if (success) {
                lua_pushnil(state);
                while (lua_next(state, -2)) {
                    Envelope env;
                    std::vector < EnvelopePoint> tmpVec;
                    lua_pushnil(state);
                    while (lua_next(state, -2)) {
                        float x_value = static_cast<float>(lua_tonumber(state, -1));
                        lua_pop(state, 1);
                        float y_value = static_cast<float>(lua_tonumber(state, -1));
                        lua_pop(state, 1);
                        std::string color = static_cast<std::string>(lua_tostring(state, -1));
                        lua_pop(state, 1);
                        tmpVec.emplace_back(color, x_value, y_value);
                        lua_pop(state, 1);
                    }
                    lua_pop(state, 2);
                }
                lua_pop(state, 1);
            }
            return success;
        }

        bool TransferFunction::getEnvelopesToLua(lua_State* state) {
            lua_newtable(state);
            for (auto iter = _envelopes.begin(); iter != _envelopes.end(); ++iter) {
                iter->setEnvelopeLuaTable(state);
            }
            return true;
        }

        bool TransferFunction::operator!=(const TransferFunction& tf) { 
            
            if (_envelopes.size() != tf._envelopes.size())
                return true;

            auto iter = _envelopes.begin();
            auto tfIter = tf._envelopes.begin();
            for (; iter != _envelopes.end(); ++iter, ++tfIter) {
                if (*iter != *tfIter)
                    return true;
            }
            return false; 
        }

        std::string TransferFunction::getSerializedToString() const {
            if (_envelopes.empty())
                return "";
            json j;
            auto envIter = _envelopes.begin();
            for (; envIter != _envelopes.end(); ++envIter) {
                j[std::distance(_envelopes.begin(), envIter)] = { envIter->getJSONEnvelope() };
            }
            return j.dump();
        }

        bool TransferFunction::createTexture(std::shared_ptr<ghoul::opengl::Texture> ptr) {
            if (_envelopes.empty()) {
                return false;
            }
            else {
                float* transferFunction = new float[_width * 4]();
                for (int i = 0; i < 4 * _width; ++i) {
                    transferFunction[i] = 0.0f;
                }

                for (size_t i = 0; i <= (_width - 1); i++) {
                    float position = static_cast<float>(i) / static_cast<float>(_width);
                    int count = 0;
                    glm::vec4 rgbFromEnvelopes{0.f, 0.f, 0.f, 0.f};
                    float alpha = 0.f;
                    for (auto iter = _envelopes.begin(); iter != _envelopes.end(); ++iter) {
                        if (iter->isValueInEnvelope(position) && iter->isEnvelopeValid()) {
                            count++;
                            glm::vec4 tmp = iter->getValueAtPosition(position);
                            rgbFromEnvelopes.r += tmp.r * tmp.a;
                            rgbFromEnvelopes.g += tmp.g * tmp.a;
                            rgbFromEnvelopes.b += tmp.b * tmp.a;
                            if (alpha < tmp.a)
                                alpha = tmp.a;
                        }
                    }
                    rgbFromEnvelopes /= static_cast<float>(count == 0 ? 1 : count);
                    rgbFromEnvelopes.w = alpha;

                    for (size_t channel = 0; channel < 4; ++channel) {
                        size_t position = 4 * i + channel;
                        float value = rgbFromEnvelopes[channel];
                        transferFunction[position] = value;
                    }
                }
                ptr->setPixelData(transferFunction);
                return true;
            }
        }
    }
}
