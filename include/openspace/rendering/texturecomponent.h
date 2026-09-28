/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/opengl/texture.h>
#include <filesystem>
#include <memory>

namespace openspace {

namespace filesystem { class File; }

class TextureComponent {
public:
    /**
     * \param nDimensions Must be 1, 2, 3
     */
    explicit TextureComponent(int nDimensions);

    const opengl::Texture* texture() const;
    opengl::Texture* texture();

    void setFilterMode(opengl::Texture::FilterMode filterMode);
    void setWrapping(opengl::Texture::WrappingMode wrapping);
    void setShouldWatchFileForChanges(bool value);

    /**
     * Loads a texture from a file on disk.
     */
    void loadFromFile(const std::filesystem::path& path);

    void unloadTexture();

    /**
     * Function to call in a renderable's update function to make sure the texture is kept
     * up to date.
     */
    void update();

private:
    std::unique_ptr<filesystem::File> _textureFile;
    std::unique_ptr<opengl::Texture> _texture;

    opengl::Texture::FilterMode _filterMode = opengl::Texture::FilterMode::LinearMipMap;
    opengl::Texture::WrappingMode _wrappingMode = opengl::Texture::WrappingMode::Repeat;

    bool _shouldWatchFile = true;

    bool _fileIsDirty = false;

    const int _nDimensions;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TEXTURECOMPONENT___H__
