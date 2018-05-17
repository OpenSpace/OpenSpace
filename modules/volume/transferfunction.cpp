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

#include <ghoul/misc/dictionary.h>
#include <ext/json/json.hpp>
#include <memory>
#include <iostream>
#include <fstream>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <ghoul/filesystem/filesystem.h>

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

        TransferFunction::TransferFunction(const TransferFunction& tf) : _envelopes(tf._envelopes), _loadableFilePath(tf._loadableFilePath) { }

        TransferFunction::TransferFunction(TransferFunction&& tf) : _envelopes(std::move(tf._envelopes)), _loadableFilePath(std::move(tf._loadableFilePath)) { }

        TransferFunction& TransferFunction::operator=(const TransferFunction& tf) {
            _envelopes = tf._envelopes;
            _loadableFilePath = tf._loadableFilePath;
            return *this;
        }

        TransferFunction& TransferFunction::operator=(TransferFunction&& tf) {
            _envelopes = std::move(tf._envelopes);
            _loadableFilePath = std::move(tf._loadableFilePath);
            return *this;
        }

        bool TransferFunction::setEnvelopesFromString(std::string s) {
            json j = json::parse(s);
            for (json::iterator it = j.begin(); it != j.end(); ++it) {
                Envelope env;
                std::vector < EnvelopePoint> tmpVec;
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

        bool TransferFunction::getEnvelopesToLua(lua_State* state) {
            lua_newtable(state);
            for (auto iter = _envelopes.begin(); iter != _envelopes.end(); ++iter) {
                lua_newtable(state);
                iter->setEnvelopeLuaTable(state);
                lua_setfield(state, -2, ("[\"" + std::to_string(iter - _envelopes.begin() + 1) + "\"]").c_str());
            }
            return true;
        }

        void TransferFunction::loadEnvelopesFromFile(const std::string& path) {
            lua_State* L = luaL_newstate();
            std::string absFilename = absPath(path);
            ghoul::Dictionary dictionary;
            ghoul::lua::loadDictionaryFromFile(path, dictionary, L);

            std::vector<std::string> transferfunctionKeys = dictionary.keys();
            for (auto transferfunctionKey : transferfunctionKeys) {
                ghoul::Dictionary transferfunctionDictionary = dictionary.value<ghoul::Dictionary>(transferfunctionKey);
                std::vector<std::string> envelopeKeys = transferfunctionDictionary.keys();
                for (auto envelopeKey : envelopeKeys) {
                    ghoul::Dictionary envelopeDictionary = transferfunctionDictionary.value<ghoul::Dictionary>(envelopeKey);
                    std::vector<std::string> pointKeys = envelopeDictionary.keys();
                    Envelope env;
                    std::vector < EnvelopePoint> tmpVec;
                    for (auto pointKey : pointKeys) {
                        ghoul::Dictionary pointDictionary = envelopeDictionary.value<ghoul::Dictionary>(pointKey);
                        std::string color = pointDictionary.value<std::string>("color");
                        ghoul::Dictionary positionDictionary = pointDictionary.value<ghoul::Dictionary>("position");
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
            getEnvelopesToLua(state);
            
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
            
            if (_envelopes.size() != tf._envelopes.size())
                return true;

            if (_loadableFilePath != tf._loadableFilePath)
                return true;

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
