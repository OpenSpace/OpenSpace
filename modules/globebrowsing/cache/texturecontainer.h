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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TEXTURE_CONTAINER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TEXTURE_CONTAINER___H__

#include <modules/globebrowsing/tile/tiletextureinitdata.h>

#include <memory>
#include <vector>

namespace openspace::globebrowsing::cache {

/**
 * Owner of texture data used for tiles. Instead of dynamically allocating textures one
 * by one, they are created once and reused.
 */
class TextureContainer {
public:
    /**
     * \param initData is the description of the texture type.
     * \param numTextures is the number of textures to allocate.
     */
    TextureContainer(TileTextureInitData initData, size_t numTextures);

    ~TextureContainer() = default;

    void reset();
    void reset(size_t numTextures);

    /**
     * \return A pointer to a texture if there is one texture never used before. If there
     *         are no textures left, nullptr is returned. TextureContainer still owns the
     *         texture so no delete should be called on the raw pointer.
     */
    ghoul::opengl::Texture* getTextureIfFree();

    const TileTextureInitData& tileTextureInitData() const;

    /**
     * \returns the number of textures in this TextureContainer
     */
    size_t size() const;

private:
    std::vector<std::unique_ptr<ghoul::opengl::Texture>> _textures;

    const TileTextureInitData _initData;
    size_t _freeTexture = 0;
    size_t _numTextures;
};

} // namespace openspace::globebrowsing::cache

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TEXTURE_CONTAINER___H__
