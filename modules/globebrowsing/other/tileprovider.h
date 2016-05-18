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

#ifndef __TILE_PROVIDER_H__
#define __TILE_PROVIDER_H__

#include "gdal_priv.h"

#include <openspace/engine/downloadmanager.h>
#include <set>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // absPath
#include <ghoul/opengl/texture.h>
#include <ghoul/io/texture/texturereader.h>

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/other/lrucache.h>
#include <modules/globebrowsing/other/concurrentjobmanager.h>
#include <modules/globebrowsing/other/texturedataprovider.h>

//////////////////////////////////////////////////////////////////////////////////////////
//									TILE PROVIDER									    //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {
    using namespace ghoul::opengl;
    
    struct TileDepthTransform
    {
        float depthScale;
        float depthOffset;
    };

    struct TileUvTransform
    {
        glm::vec2 uvOffset;
        glm::vec2 uvScale;
    };

    struct Tile {
        std::shared_ptr<Texture> texture;
        TileUvTransform uvTransform;
    };

    /**
        Provides tiles through GDAL datasets which can be defined with xml files
        for example for wms.
    */
    class TileProvider {
    public:
        TileProvider(const std::string& fileName, int tileCacheSize, int minimumPixelSize,
            int framesUntilRequestFlush);
        ~TileProvider();

        Tile getHighestResolutionTile(ChunkIndex chunkIndex);

        std::shared_ptr<Texture> getOrStartFetchingTile(ChunkIndex chunkIndex);
        std::shared_ptr<Texture> getDefaultTexture();
        TileDepthTransform depthTransform();

        void prerender();

    private:

        friend class TextureTileLoadJob;



        //////////////////////////////////////////////////////////////////////////////////
        //                                Helper functions                              //
        //////////////////////////////////////////////////////////////////////////////////
        Tile getOrEnqueueHighestResolutionTile(const ChunkIndex& ci, TileUvTransform& uvTransform);


        void transformFromParent(const ChunkIndex& ci, TileUvTransform& uv) const;

        /**
            Fetches all the needeed texture data from the GDAL dataset.
        */
        std::shared_ptr<TextureData> getTextureData(
            const ChunkIndex& chunkIndex);
        
        /**
            Creates an OpenGL texture and pushes the data to the GPU.
        */
        std::shared_ptr<Texture> initializeTexture(
            std::shared_ptr<TextureData> uninitedTexture);

        bool enqueueTileRequest(const ChunkIndex& ci);

        void clearRequestQueue();

        void initTexturesFromLoadedData();




        //////////////////////////////////////////////////////////////////////////////////
        //                                Member variables                              //
        //////////////////////////////////////////////////////////////////////////////////

        LRUCache<HashKey, std::shared_ptr<Texture>> _tileCache;
        std::set<HashKey> _queuedTileRequests;

        int _framesSinceLastRequestFlush;
        int _framesUntilRequestFlush;

        const std::string _filePath;

        static bool hasInitializedGDAL;
        GDALDataset* _gdalDataSet;

        // Converters are needed for all different data types since they are templated.
        TextureDataProvider<GLubyte>  _uByteTextureTileDataProvider;
        TextureDataProvider<GLushort> _uShortTextureDataProvider;
        TextureDataProvider<GLshort>  _shortTextureDataProvider;
        TextureDataProvider<GLuint>   _uIntTextureDataProvider;
        TextureDataProvider<GLint>    _intTextureDataProvider;
        TextureDataProvider<GLfloat>  _floatTextureDataProvider;
        TextureDataProvider<GLdouble> _doubleTextureDataProvider;

        ConcurrentJobManager<TextureData> _tileLoadManager;

        std::shared_ptr<Texture> _defaultTexture;
        int _tileLevelDifference;
        TileDepthTransform _depthTransform;
    };

}  // namespace openspace









namespace openspace {

    using namespace ghoul::opengl;

    struct TextureTileLoadJob : public Job<TextureData> {
        TextureTileLoadJob(TileProvider * tileProvider, const ChunkIndex& chunkIndex)
            : _tileProvider(tileProvider)
            , _chunkIndex(chunkIndex) {

        }

        virtual ~TextureTileLoadJob() { }

        virtual void execute() {
            _uninitedTexture = _tileProvider->getTextureData(_chunkIndex);

        }

        virtual std::shared_ptr<TextureData> product() {
            return _uninitedTexture;
        }


    private:
        ChunkIndex _chunkIndex;
        TileProvider * _tileProvider;
        std::shared_ptr<TextureData> _uninitedTexture;
    };
}

#endif  // __TILE_PROVIDER_H__