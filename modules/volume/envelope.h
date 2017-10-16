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

#ifndef __OPENSPACE_MODULE_VOLUME___ENVELOPE___H__
#define __OPENSPACE_MODULE_VOLUME___ENVELOPE___H__

#include <ghoul/opengl/texture.h>
#include <ghoul/glm.h>
#include <ext/json/json.hpp>

using json = nlohmann::json;

namespace openspace {
    namespace volume {

        class Envelope {
        public:
            Envelope();
            Envelope(glm::vec3 col, std::vector<std::pair<float, float>> vec);

            void setColor(std::string hex);
            void setPoints(std::vector<std::pair<float, float>> vec);

            int HexadecimalToDecimal(std::string hex) const;
            std::string DecimalToHexadecimal(int dec) const;
            glm::vec3 hexadecimalToRGBConversion(std::string hex) const;
            std::string getHexadecimal() const;

            glm::vec4 getValueAtPosition(float pos) const;
            bool isValueInEnvelope(float pos) const;
            json getSerializedPoints() const;
            json getSerializedEnvelope() const;

            bool operator!=(const Envelope& env) const;

        private:
            glm::vec3 color{ 0.f, 0.f, 0.f };
            std::vector<std::pair<float, float>> _points;
        };

    }//namespace volume
}//namespace openspace
#endif __OPENSPACE_MODULE_VOLUME___ENVELOPE___H__
