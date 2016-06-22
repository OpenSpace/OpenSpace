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


#include <sstream>




namespace {
    const std::string _loggerCat = "TileProvider";
}


namespace openspace {

    const Tile Tile::TileUnavailable = {nullptr, nullptr, Tile::Status::Unavailable };

    double Tile::sampleValueAsDouble(glm::vec2 uv, int band) {
        if (texture == nullptr || status != Status::OK) {
            return 0;
        }
        glm::uvec3 dimensions = texture->dimensions();
        if (band >= dimensions.z || band < 0) {
            throw ghoul::RuntimeError("Cannot sample from band " + std::to_string(band) + "in texture tile.");
        }
        glm::vec2 samplePos = uv * glm::vec2(dimensions.xy());
        glm::uvec2 samplePos00 = samplePos;
        samplePos00 = glm::clamp(samplePos00, glm::uvec2(0, 0), dimensions.xy() - glm::uvec2(1));
        glm::vec2 samplePosFract = samplePos - glm::vec2(samplePos00);

        glm::uvec2 samplePos10 = glm::min(samplePos00 + glm::uvec2(1, 0), dimensions.xy() - glm::uvec2(1));
        glm::uvec2 samplePos01 = glm::min(samplePos00 + glm::uvec2(0, 1), dimensions.xy() - glm::uvec2(1));
        glm::uvec2 samplePos11 = glm::min(samplePos00 + glm::uvec2(1, 1), dimensions.xy() - glm::uvec2(1));

        unsigned int linearSamplePos00 = (dimensions.x * dimensions.z) * samplePos00.y + (samplePos00.x * dimensions.z) + band;
        unsigned int linearSamplePos10 = (dimensions.x * dimensions.z) * samplePos10.y + (samplePos10.x * dimensions.z) + band;
        unsigned int linearSamplePos01 = (dimensions.x * dimensions.z) * samplePos01.y + (samplePos01.x * dimensions.z) + band;
        unsigned int linearSamplePos11 = (dimensions.x * dimensions.z) * samplePos11.y + (samplePos11.x * dimensions.z) + band;

        GLuint dataType = texture->dataType();
        switch (dataType)
        {
            case GL_UNSIGNED_BYTE: {
                const GLubyte* pixelData = reinterpret_cast<const GLubyte*>(texture->pixelData());
                double sample00 = static_cast<double>(pixelData[linearSamplePos00]);
                double sample10 = static_cast<double>(pixelData[linearSamplePos10]);
                double sample01 = static_cast<double>(pixelData[linearSamplePos01]);
                double sample11 = static_cast<double>(pixelData[linearSamplePos11]);

                double sample0 = sample00 * (1.0 - samplePosFract.x) + sample10 * samplePosFract.x;
                double sample1 = sample01 * (1.0 - samplePosFract.x) + sample11 * samplePosFract.x;

                double sample = sample0 * (1.0 - samplePosFract.y) + sample1 * samplePosFract.y;

                return sample;
            }
            case GL_UNSIGNED_SHORT: {
                const GLushort* pixelData = reinterpret_cast<const GLushort*>(texture->pixelData());
                double sample00 = static_cast<double>(pixelData[linearSamplePos00]);
                double sample10 = static_cast<double>(pixelData[linearSamplePos10]);
                double sample01 = static_cast<double>(pixelData[linearSamplePos01]);
                double sample11 = static_cast<double>(pixelData[linearSamplePos11]);

                double sample0 = sample00 * (1.0 - samplePosFract.x) + sample10 * samplePosFract.x;
                double sample1 = sample01 * (1.0 - samplePosFract.x) + sample11 * samplePosFract.x;

                double sample = sample0 * (1.0 - samplePosFract.y) + sample1 * samplePosFract.y;

                return sample;            }
            case GL_SHORT: {
                const GLshort* pixelData = reinterpret_cast<const GLshort*>(texture->pixelData());
                double sample00 = static_cast<double>(pixelData[linearSamplePos00]);
                double sample10 = static_cast<double>(pixelData[linearSamplePos10]);
                double sample01 = static_cast<double>(pixelData[linearSamplePos01]);
                double sample11 = static_cast<double>(pixelData[linearSamplePos11]);

                double sample0 = sample00 * (1.0 - samplePosFract.x) + sample10 * samplePosFract.x;
                double sample1 = sample01 * (1.0 - samplePosFract.x) + sample11 * samplePosFract.x;

                double sample = sample0 * (1.0 - samplePosFract.y) + sample1 * samplePosFract.y;

                return sample;            }
            case GL_UNSIGNED_INT: {
                const GLuint* pixelData = reinterpret_cast<const GLuint*>(texture->pixelData());
                double sample00 = static_cast<double>(pixelData[linearSamplePos00]);
                double sample10 = static_cast<double>(pixelData[linearSamplePos10]);
                double sample01 = static_cast<double>(pixelData[linearSamplePos01]);
                double sample11 = static_cast<double>(pixelData[linearSamplePos11]);

                double sample0 = sample00 * (1.0 - samplePosFract.x) + sample10 * samplePosFract.x;
                double sample1 = sample01 * (1.0 - samplePosFract.x) + sample11 * samplePosFract.x;

                double sample = sample0 * (1.0 - samplePosFract.y) + sample1 * samplePosFract.y;

                return sample;            }
            case GL_INT: {
                const GLint* pixelData = reinterpret_cast<const GLint*>(texture->pixelData());
                double sample00 = static_cast<double>(pixelData[linearSamplePos00]);
                double sample10 = static_cast<double>(pixelData[linearSamplePos10]);
                double sample01 = static_cast<double>(pixelData[linearSamplePos01]);
                double sample11 = static_cast<double>(pixelData[linearSamplePos11]);

                double sample0 = sample00 * (1.0 - samplePosFract.x) + sample10 * samplePosFract.x;
                double sample1 = sample01 * (1.0 - samplePosFract.x) + sample11 * samplePosFract.x;

                double sample = sample0 * (1.0 - samplePosFract.y) + sample1 * samplePosFract.y;

                return sample;            }
            case GL_FLOAT: {
                const GLfloat* pixelData = reinterpret_cast<const GLfloat*>(texture->pixelData());
                double sample00 = static_cast<double>(pixelData[linearSamplePos00]);
                double sample10 = static_cast<double>(pixelData[linearSamplePos10]);
                double sample01 = static_cast<double>(pixelData[linearSamplePos01]);
                double sample11 = static_cast<double>(pixelData[linearSamplePos11]);

                double sample0 = sample00 * (1.0 - samplePosFract.x) + sample10 * samplePosFract.x;
                double sample1 = sample01 * (1.0 - samplePosFract.x) + sample11 * samplePosFract.x;

                double sample = sample0 * (1.0 - samplePosFract.y) + sample1 * samplePosFract.y;

                return sample;            }
            case GL_DOUBLE: {
                const GLdouble* pixelData = reinterpret_cast<const GLdouble*>(texture->pixelData());
                double sample00 = static_cast<double>(pixelData[linearSamplePos00]);
                double sample10 = static_cast<double>(pixelData[linearSamplePos10]);
                double sample01 = static_cast<double>(pixelData[linearSamplePos01]);
                double sample11 = static_cast<double>(pixelData[linearSamplePos11]);

                double sample0 = sample00 * (1.0 - samplePosFract.x) + sample10 * samplePosFract.x;
                double sample1 = sample01 * (1.0 - samplePosFract.x) + sample11 * samplePosFract.x;

                double sample = sample0 * (1.0 - samplePosFract.y) + sample1 * samplePosFract.y;

                return sample;            }
            default:
                LWARNING("Unknown GL type in texture. Returning 0.");
                return 0;
        }
    }


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


    void CachingTileProvider::prerender() {
        initTexturesFromLoadedData();
        if (_framesSinceLastRequestFlush++ > _framesUntilRequestFlush) {
            clearRequestQueue();
        }
    }

    std::shared_ptr<AsyncTileDataProvider> CachingTileProvider::getAsyncTileReader() {
        return _asyncTextureDataProvider;
    }

    Tile CachingTileProvider::getTile(const ChunkIndex& chunkIndex) {
        Tile tile = Tile::TileUnavailable;

        auto tileDataset = _asyncTextureDataProvider->getTextureDataProvider();
        if (chunkIndex.level > tileDataset->getMaximumLevel()) {
            tile.status = Tile::Status::OutOfRange;
            return tile;
        }

        HashKey key = chunkIndex.hashKey();

        if (_tileCache->exist(key)) {
            return _tileCache->get(key);
        }
        else {
            _asyncTextureDataProvider->enqueueTextureData(chunkIndex);
        }
        
        return tile;
    }

    void CachingTileProvider::initTexturesFromLoadedData() {
        while (_asyncTextureDataProvider->hasLoadedTextureData()) {
            std::shared_ptr<TileIOResult> tileIOResult = _asyncTextureDataProvider->nextTileIOResult();
            initializeAndAddToCache(tileIOResult);
        }
    }

    void CachingTileProvider::clearRequestQueue() {
        _asyncTextureDataProvider->clearRequestQueue();
        _framesSinceLastRequestFlush = 0;
    }

    Tile::Status CachingTileProvider::getTileStatus(const ChunkIndex& chunkIndex) {
        auto tileDataset = _asyncTextureDataProvider->getTextureDataProvider();
        if (chunkIndex.level > tileDataset->getMaximumLevel()) {
            return Tile::Status::OutOfRange;
        }

        HashKey key = chunkIndex.hashKey();

        if (_tileCache->exist(key)) {
            return _tileCache->get(key).status;
        }

        return Tile::Status::Unavailable;
    }


    Tile CachingTileProvider::getOrStartFetchingTile(ChunkIndex chunkIndex) {
        HashKey hashkey = chunkIndex.hashKey();
        if (_tileCache->exist(hashkey)) {
            return _tileCache->get(hashkey);
        }
        else {
            _asyncTextureDataProvider->enqueueTextureData(chunkIndex);
            return Tile::TileUnavailable;
        }
    }

    TileDepthTransform CachingTileProvider::depthTransform() {
        return _asyncTextureDataProvider->getTextureDataProvider()->getDepthTransform();
    }


    void CachingTileProvider::initializeAndAddToCache(std::shared_ptr<TileIOResult> tileIOResult) {
        HashKey key = tileIOResult->chunkIndex.hashKey();
        TileDataset::DataLayout dataLayout = _asyncTextureDataProvider->getTextureDataProvider()->getDataLayout();
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
        //texture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);

        texture->uploadTexture();
        

        Tile tile = {
            texture,
            tileIOResult->preprocessData,
            tileIOResult->error == CE_None ? Tile::Status::OK : Tile::Status::IOError
        };

        _tileCache->put(key, tile);
    }



}  // namespace openspace
