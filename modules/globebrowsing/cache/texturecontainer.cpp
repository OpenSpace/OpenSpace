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

#include <modules/globebrowsing/cache/texturecontainer.h>

namespace openspace::globebrowsing::cache {

TextureContainer::TextureContainer(TileTextureInitData initData, size_t numTextures)
    : _initData(std::move(initData))
    , _numTextures(numTextures)
{
    reset();
}

void TextureContainer::reset() {
    _textures.clear();
    _freeTexture = 0;
    for (size_t i = 0; i < _numTextures; ++i) {
        using namespace ghoul::opengl;
        std::unique_ptr<Texture> tex = std::make_unique<Texture>(
            _initData.dimensions(),
            _initData.ghoulTextureFormat(),
            _initData.glTextureFormat(),
            _initData.glType(),
            Texture::FilterMode::Linear,
            Texture::WrappingMode::ClampToEdge,
            Texture::AllocateData(_initData.shouldAllocateDataOnCPU())
        );

        tex->setDataOwnership(Texture::TakeOwnership::Yes);
        tex->uploadTexture();
        tex->setFilter(Texture::FilterMode::AnisotropicMipMap);

        _textures.push_back(std::move(tex));
    }
}

void TextureContainer::reset(size_t numTextures) {
    _numTextures = numTextures;
    reset();
}

ghoul::opengl::Texture* TextureContainer::getTextureIfFree() {
    if (_freeTexture < _textures.size()) {
        ghoul::opengl::Texture* texture = _textures[_freeTexture].get();
        _freeTexture++;
        return texture;
    }
    else {
        return nullptr;
    }
}

const TileTextureInitData& TextureContainer::tileTextureInitData() const {
    return _initData;
}

size_t TextureContainer::size() const {
    return _textures.size();
}

} // namespace openspace::globebrowsing::cache
