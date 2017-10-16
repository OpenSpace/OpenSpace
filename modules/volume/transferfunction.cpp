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

#include <modules/volume/transferfunction.h>
#include <ext/json/json.hpp>
#include <memory>
using json = nlohmann::json;

namespace openspace {
    namespace volume {
        
        ghoul::opengl::Texture::FilterMode filtermode = ghoul::opengl::Texture::FilterMode::Linear;
        ghoul::opengl::Texture::WrappingMode wrappingmode = ghoul::opengl::Texture::WrappingMode::ClampToEdge;

        TransferFunction::TransferFunction() {
        }

        TransferFunction::TransferFunction(int s) {
        }
        
        TransferFunction::TransferFunction(std::string s) {
            json j = json::parse(s);
            for (json::iterator it = j.begin(); it != j.end(); ++it) {
                Envelope env;
                env.setColor((*it)["color"].get<std::string>());
                std::vector<std::pair<float, float>> tmpVec;
                for (int i = 0; i < 4; i++)
                    tmpVec.emplace_back(std::make_pair((*it)["points"][i]["position"]["x"].get<float>(), (*it)["points"][0]["position"]["y"].get<float>()));
                env.setPoints(tmpVec);
                _envelopes.emplace_back(env);
            }
        }

        TransferFunction::TransferFunction(const TransferFunction& tf) : _envelopes(tf._envelopes), _textureSet(tf._textureSet) { }

        TransferFunction::TransferFunction(TransferFunction&& tf) : _envelopes(std::move(tf._envelopes)), _textureSet(std::move(tf._textureSet)) { }

        TransferFunction& TransferFunction::operator=(TransferFunction&& tf) {
            _envelopes = std::move(tf._envelopes);
            return *this;
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

        bool TransferFunction::isTextureSet() const {
            if (!_envelopes.empty())
                return _textureSet;
        }

        std::string TransferFunction::getSerializedToString() const {
            if (_envelopes.empty())
                return "";
            json j;
            auto envIter = _envelopes.begin();
            for (; envIter != _envelopes.end(); ++envIter) {
                j[std::distance(_envelopes.begin(), envIter)] = { envIter->getSerializedEnvelope() };
            }
            return j.dump();
        }

        void TransferFunction::createTexture(std::unique_ptr<ghoul::opengl::Texture> *ptr) {
            //TODO: check lower and upper values of envelopes and change dependin on lower/upper set
            if (!_envelopes.empty()) {
                float* transferFunction = new float[width * 4]();
                for (int i = 0; i < 4 * width; ++i) {
                    float position = static_cast<float>(i) / static_cast<float>(width);
                    glm::vec4 val = _envelopes.at(0).getValueAtPosition(position);

                    for (size_t channel = 0; channel < 4; ++channel) {
                        size_t position = 4 * i + channel;
                        float value = val[channel] / 255.f;
                        transferFunction[position] = value;
                    }
                }
                _textureSet = true;
                *ptr = std::make_unique<ghoul::opengl::Texture>(transferFunction,
                    glm::size3_t(width, 1, 1), ghoul::opengl::Texture::Format::RGBA,
                    GL_RGBA, GL_FLOAT, filtermode, wrappingmode);
            }
        }
    }
}
