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

#ifndef __OPENSPACE_CORE___TRANSFERFUNCTION___H__
#define __OPENSPACE_CORE___TRANSFERFUNCTION___H__

#include <ghoul/glm.h>
#include <filesystem>
#include <functional>
#include <memory>
#include <string>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl { class Texture; }

namespace openspace {

class TransferFunction {
public:
    using TfChangedCallback = std::function<void (const TransferFunction&)>;

    TransferFunction(const std::filesystem::path& filepath,
        TfChangedCallback tfChangedCallback = TfChangedCallback());
    ~TransferFunction();

    TransferFunction(TransferFunction&& rhs) = default;

    void setPath(const std::filesystem::path& filepath);
    ghoul::opengl::Texture& texture();
    void bind();
    void update();
    glm::vec4 sample(size_t offset);
    size_t width();
    void setCallback(TfChangedCallback callback);
    void setTextureFromTxt();

private:
    void setTextureFromImage();
    void uploadTexture();

    std::filesystem::path _filepath;
    std::unique_ptr<ghoul::filesystem::File> _file;
    std::shared_ptr<ghoul::opengl::Texture> _texture;
    bool _needsUpdate = false;
    TfChangedCallback _tfChangedCallback;
};

struct MappingKey {
    MappingKey(float p, const glm::vec4& c): position(p), color(c) {}
    MappingKey(float p): position(p), color(glm::vec4(0.f)) {}
    bool operator<(const MappingKey& rhs) { return position < rhs.position; }

    float position = 0.f;
    glm::vec4 color = glm::vec4(0.f);
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TRANSFERFUNCTION___H__
