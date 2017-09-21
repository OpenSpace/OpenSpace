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

#include <modules/globebrowsing/cache/texturecontainer.h>
#include <modules/globebrowsing/tile/tiletextureinitdata.h>

namespace openspace::globebrowsing::cache {

TextureContainer::TextureContainer(TileTextureInitData initData, size_t numTextures)
    : _initData(initData)
    , _freeTexture(0)
    , _numTextures(numTextures)
{
    reset();
}

void TextureContainer::reset() {
    _textures.clear();
    _freeTexture = 0;
    ghoul::opengl::Texture::AllocateData allocate =
        _initData.shouldAllocateDataOnCPU() ?
        ghoul::opengl::Texture::AllocateData::Yes :
        ghoul::opengl::Texture::AllocateData::No;
    for (size_t i = 0; i < _numTextures; ++i)
    {
        auto tex = std::make_unique<ghoul::opengl::Texture>(
            _initData.dimensions(),
            _initData.ghoulTextureFormat(),
            _initData.glTextureFormat(),
            _initData.glType(),
            ghoul::opengl::Texture::FilterMode::Linear,
            ghoul::opengl::Texture::WrappingMode::ClampToEdge,
            allocate
        );
        
        tex->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::Yes);
        tex->uploadTexture();
        tex->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        
        _textures.push_back(std::move(tex));
    }
}

void TextureContainer::reset(size_t numTextures) {
    _numTextures = numTextures;
    reset();
}

ghoul::opengl::Texture* TextureContainer::getTextureIfFree() {
    ghoul::opengl::Texture* texture = nullptr;
    if (_freeTexture < _textures.size()) {
        texture = _textures[_freeTexture].get();
        _freeTexture++;
    }
    return texture;
}

const openspace::globebrowsing::TileTextureInitData& TextureContainer::tileTextureInitData() const {
    return _initData;
}

size_t TextureContainer::size() const {
    return _textures.size();
}

} // namespace openspace::globebrowsing::cache
