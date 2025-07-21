/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___TEXTURECOMPONENT___H__
#define __OPENSPACE_CORE___TEXTURECOMPONENT___H__

#include <ghoul/opengl/texture.h>
#include <filesystem>

namespace ghoul::filesystem { class File; }
namespace ghoul::opengl {class Texture; }

namespace openspace {

class TextureComponent {
public:
    // nDimensions must be 1, 2, 3
    explicit TextureComponent(int nDimensions);

    const ghoul::opengl::Texture* texture() const;
    ghoul::opengl::Texture* texture();

    void setFilterMode(ghoul::opengl::Texture::FilterMode filterMode);
    void setWrapping(ghoul::opengl::Texture::WrappingMode wrapping);
    void setShouldWatchFileForChanges(bool value);
    void setShouldPurgeFromRAM(bool value);

    void bind();
    void uploadToGpu();

    // Loads a texture from a file on disk
    void loadFromFile(const std::filesystem::path& path);

    // Function to call in a renderable's update function to make sure
    // the texture is kept up to date
    void update();

private:
    std::unique_ptr<ghoul::filesystem::File> _textureFile;
    std::unique_ptr<ghoul::opengl::Texture> _texture;

    ghoul::opengl::Texture::FilterMode _filterMode =
        ghoul::opengl::Texture::FilterMode::LinearMipMap;
    ghoul::opengl::Texture::WrappingMode _wrappingMode =
        ghoul::opengl::Texture::WrappingMode::Repeat;

    bool _shouldWatchFile = true;
    bool _shouldPurgeFromRAM = true;

    bool _fileIsDirty = false;
    bool _textureIsDirty = false;

    const int _nDimensions;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TEXTURECOMPONENT___H__
