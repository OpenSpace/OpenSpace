/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#ifndef __TRANSFERFUNCTION_H__
#define __TRANSFERFUNCTION_H__

#include <string>
#include <glm/glm.hpp>
#include <functional>
#include <ghoul/opengl/texture.h>
#include <ghoul/filesystem/file.h>
#include <memory>

namespace openspace {

    class TransferFunction {
    public:
        typedef std::function<void (const TransferFunction&)> TfChangedCallback;

        TransferFunction(const std::string& filepath, TfChangedCallback tfChangedCallback = TfChangedCallback());
        void setPath(const std::string& filepath);
        ghoul::opengl::Texture& getTexture();
        void update();
        glm::vec4 sample(size_t t);
        size_t width();
        void setCallback(TfChangedCallback callback);
    private:
        void setTextureFromTxt();
        void setTextureFromImage();
        void uploadTexture();

        std::string _filepath;
        std::unique_ptr<ghoul::filesystem::File> _file = nullptr;
        std::unique_ptr<ghoul::opengl::Texture> _texture = nullptr;
        bool _needsUpdate = false;
        TfChangedCallback _tfChangedCallback;
    };

    struct MappingKey {
        float position{0.0f};
        glm::vec4 color{0.0f,0.0f,0.0f,0.0f};
        MappingKey(float p, const glm::vec4& c): position(p), color(c) {};
        MappingKey(float p): position(p), color(glm::vec4(0.0f)) {};
        bool operator<(const MappingKey& rhs) {return position < rhs.position;};
    };
} // namespace openspace

#endif

