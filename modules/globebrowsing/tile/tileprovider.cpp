/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#include <modules/globebrowsing/geometry/geodetic2.h>

#include <modules/globebrowsing/tile/tileprovider.h>
#include <modules/globebrowsing/tile/tilediskcache.h>
#include <modules/globebrowsing/tile/tileprovidermanager.h>

#include <modules/globebrowsing/chunk/chunkindex.h>

#include <openspace/engine/downloadmanager.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <ghoul/font/fontrenderer.h>
#include <ghoul/font/fontmanager.h>

#include <openspace/engine/openspaceengine.h>

#include <sstream>




namespace {
    const std::string _loggerCat = "TileProvider";
}


namespace openspace {

    const Tile Tile::TileUnavailable = {nullptr, nullptr, Tile::Status::Unavailable };
    

    Tile Tile::createPlainTile(const glm::uvec2& size, const glm::uvec4& color) {
        using namespace ghoul::opengl;
        
        // Create pixel data
        int numBytes = size.x * size.y * 4 * 1;
        char* pixels = new char[numBytes];
        size_t numPixels = size.x * size.y;
        size_t i = 0;
        for (size_t p = 0; p < numPixels; p++){
            pixels[i++] = color.r;
            pixels[i++] = color.g;
            pixels[i++] = color.b;
            pixels[i++] = color.a;
        }

        // Create ghoul texture
        auto texture = std::make_shared<Texture>(glm::uvec3(size, 1));
        texture->setDataOwnership(Texture::TakeOwnership::Yes);
        texture->setPixelData(pixels);
        texture->uploadTexture();
        texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

        // Create tile
        Tile tile;
        tile.status = Tile::Status::OK;
        tile.preprocessData = nullptr;
        tile.texture = texture;

        return tile;
    }


    //////////////////////////////////////////////////////////////////////////////////////
    //                            Chunk Index Tile Provider                             //
    //////////////////////////////////////////////////////////////////////////////////////

    ChunkIndexTileProvider::ChunkIndexTileProvider(const glm::uvec2& textureSize, size_t fontSize)
        : _tileCache(500)
        , _textureSize(textureSize)
        , _fontSize(fontSize)
    {
        using namespace ghoul::fontrendering;

        _font = OsEng.fontManager().font("Mono", _fontSize);
        _fontRenderer = std::unique_ptr<FontRenderer>(FontRenderer::createDefault());
        _fontRenderer->setFramebufferSize(textureSize);


        glGenFramebuffers(1, &_fbo);
    }

    ChunkIndexTileProvider::~ChunkIndexTileProvider() {
        glDeleteFramebuffers(1, &_fbo);
    }

    Tile ChunkIndexTileProvider::getTile(const ChunkIndex& chunkIndex) {
        ChunkHashKey key = chunkIndex.hashKey();
        
        if (!_tileCache.exist(key)) {
            _tileCache.put(key, createChunkIndexTile(chunkIndex));
        }

        return _tileCache.get(key);
    }

    Tile ChunkIndexTileProvider::getDefaultTile() {
        return Tile::TileUnavailable;
    }


    Tile::Status ChunkIndexTileProvider::getTileStatus(const ChunkIndex& index) {
        return Tile::Status::OK;
    }

    TileDepthTransform ChunkIndexTileProvider::depthTransform() {
        TileDepthTransform transform;
        transform.depthOffset = 0.0f;
        transform.depthScale = 1.0f;
        return transform;
    }

    void ChunkIndexTileProvider::update() {
        // nothing to be done
    }

    void ChunkIndexTileProvider::reset() {
        _tileCache.clear();
    }

    Tile ChunkIndexTileProvider::createChunkIndexTile(const ChunkIndex& chunkIndex) {
        glm::uvec4 color = { 0, 0, 0, 0 };
        Tile tile = Tile::createPlainTile(_textureSize, color);

        // Keep track of defaultFBO and viewport to be able to reset state when done
        GLint defaultFBO;
        GLint viewport[4];
        glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
        glGetIntegerv(GL_VIEWPORT, viewport);

        // Render to texture
        glBindFramebuffer(GL_FRAMEBUFFER, _fbo);
        glFramebufferTexture2D(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            GL_TEXTURE_2D,
            *(tile.texture),
            0
            );

        GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
        //LDEBUG(status);

        glViewport(
            0, 0,
            static_cast<GLsizei>(tile.texture->width()),
            static_cast<GLsizei>(tile.texture->height())
            );
        
        _fontRenderer->render(
            *_font,
            glm::vec2(
                _textureSize.x / 4 - (_textureSize.x / 32) * log10(1 << chunkIndex.level),
                _textureSize.y / 2 + _fontSize),
            glm::vec4(1.0, 0.0, 0.0, 1.0),
            "level: %i \nx: %i \ny: %i",
            chunkIndex.level, chunkIndex.x, chunkIndex.y
            );

        // Reset state: bind default FBO and set viewport to what it was
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

        return tile;
    }


    int ChunkIndexTileProvider::maxLevel() {
        return 1337; // unlimited
    }




    //////////////////////////////////////////////////////////////////////////////////////
    //                           Single Image Tile Provider                             //
    //////////////////////////////////////////////////////////////////////////////////////


    SingleImagePrivoder::SingleImagePrivoder(const std::string& imagePath)
        : _imagePath(imagePath)
    {
        reset();
    }

    Tile SingleImagePrivoder::getTile(const ChunkIndex& chunkIndex) {
        return _tile;
    }

    Tile SingleImagePrivoder::getDefaultTile() {
        return _tile;
    }


    Tile::Status SingleImagePrivoder::getTileStatus(const ChunkIndex& index) {
        return _tile.status;
    }

    TileDepthTransform SingleImagePrivoder::depthTransform() {
        TileDepthTransform transform;
        transform.depthOffset = 0.0f;
        transform.depthScale = 1.0f;
        return transform;
    }

    void SingleImagePrivoder::update() {
        // nothing to be done
    }

    void SingleImagePrivoder::reset() {
        _tile = Tile();
        _tile.texture = std::shared_ptr<Texture>(ghoul::io::TextureReader::ref().loadTexture(_imagePath).release());
        _tile.status = _tile.texture != nullptr ? Tile::Status::OK : Tile::Status::IOError;
        _tile.preprocessData = nullptr;

        _tile.texture->uploadTexture();
        _tile.texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
    }

    int SingleImagePrivoder::maxLevel() {
        return 1337; // unlimited
    }

    




    //////////////////////////////////////////////////////////////////////////////////////
    //                              Caching Tile Provider                               //
    //////////////////////////////////////////////////////////////////////////////////////

    CachingTileProvider::CachingTileProvider(std::shared_ptr<AsyncTileDataProvider> tileReader, 
        std::shared_ptr<TileCache> tileCache,
        int framesUntilFlushRequestQueue)
        : _asyncTextureDataProvider(tileReader)
        , _tileCache(tileCache)
        , _framesUntilRequestFlush(framesUntilFlushRequestQueue)
        , _framesSinceLastRequestFlush(0)
    {
        
    }


    CachingTileProvider::~CachingTileProvider(){
        clearRequestQueue();
    }


    void CachingTileProvider::update() {
        initTexturesFromLoadedData();
        if (_framesSinceLastRequestFlush++ > _framesUntilRequestFlush) {
            clearRequestQueue();
        }
    }

    void CachingTileProvider::reset() {
        _tileCache->clear();
        _asyncTextureDataProvider->reset();
    }

    int CachingTileProvider::maxLevel() {
        return _asyncTextureDataProvider->getTextureDataProvider()->maxChunkLevel();
    }

    Tile CachingTileProvider::getTile(const ChunkIndex& chunkIndex) {
        Tile tile = Tile::TileUnavailable;

        
        if (chunkIndex.level > maxLevel()) {
            tile.status = Tile::Status::OutOfRange;
            return tile;
        }

        ChunkHashKey key = chunkIndex.hashKey();

        if (_tileCache->exist(key)) {
            return _tileCache->get(key);
        }
        else {
            _asyncTextureDataProvider->enqueueTileIO(chunkIndex);
        }
        
        return tile;
    }

    Tile CachingTileProvider::getDefaultTile() {
        if (_defaultTile.texture == nullptr) {
            _defaultTile = createTile(_asyncTextureDataProvider->getTextureDataProvider()->defaultTileData());
        }
        return _defaultTile;
    }


    void CachingTileProvider::initTexturesFromLoadedData() {
        auto readyTileIOResults = _asyncTextureDataProvider->getTileIOResults();
        for(auto tileIOResult : readyTileIOResults){
            ChunkHashKey key = tileIOResult->chunkIndex.hashKey();
            Tile tile = createTile(tileIOResult);
            _tileCache->put(key, tile);
        }
    }

    void CachingTileProvider::clearRequestQueue() {
        _asyncTextureDataProvider->clearRequestQueue();
        _framesSinceLastRequestFlush = 0;
    }

    Tile::Status CachingTileProvider::getTileStatus(const ChunkIndex& chunkIndex) {
        auto tileDataset = _asyncTextureDataProvider->getTextureDataProvider();
        if (chunkIndex.level > tileDataset->maxChunkLevel()) {
            return Tile::Status::OutOfRange;
        }

        ChunkHashKey key = chunkIndex.hashKey();

        if (_tileCache->exist(key)) {
            return _tileCache->get(key).status;
        }

        return Tile::Status::Unavailable;
    }


    Tile CachingTileProvider::getOrStartFetchingTile(ChunkIndex chunkIndex) {
        ChunkHashKey hashkey = chunkIndex.hashKey();
        if (_tileCache->exist(hashkey)) {
            return _tileCache->get(hashkey);
        }
        else {
            _asyncTextureDataProvider->enqueueTileIO(chunkIndex);
            return Tile::TileUnavailable;
        }
    }

    TileDepthTransform CachingTileProvider::depthTransform() {
        return _asyncTextureDataProvider->getTextureDataProvider()->getDepthTransform();
    }


    Tile CachingTileProvider::createTile(std::shared_ptr<TileIOResult> tileIOResult) {
        ChunkHashKey key = tileIOResult->chunkIndex.hashKey();
        TileDataLayout dataLayout = _asyncTextureDataProvider->getTextureDataProvider()->getDataLayout();
        Texture* texturePtr = new Texture(
            tileIOResult->imageData,
            tileIOResult->dimensions,
            dataLayout.textureFormat.ghoulFormat,
            dataLayout.textureFormat.glFormat,
            dataLayout.glType,
            Texture::FilterMode::Linear,
            Texture::WrappingMode::ClampToEdge);

        // The texture should take ownership of the data
        std::shared_ptr<Texture> texture = std::shared_ptr<Texture>(texturePtr);
        
        texture->uploadTexture();
        // AnisotropicMipMap must be set after texture is uploaded. Why?!
        texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

        Tile tile = {
            texture,
            tileIOResult->preprocessData,
            tileIOResult->error == CE_None ? Tile::Status::OK : Tile::Status::IOError
        };

        return tile;
    }



}  // namespace openspace
