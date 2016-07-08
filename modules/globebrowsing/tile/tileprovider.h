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

#include <modules/globebrowsing/geometry/geodetic2.h>

#include <modules/globebrowsing/tile/asynctilereader.h>

#include <modules/globebrowsing/other/lrucache.h>


//////////////////////////////////////////////////////////////////////////////////////////
//									TILE PROVIDER									    //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {
    using namespace ghoul::opengl;

    


    struct Tile {
        std::shared_ptr<Texture> texture;
        std::shared_ptr<TilePreprocessData> preprocessData;

        enum class Status { Unavailable, OutOfRange, IOError, OK } status;
    
        static const Tile TileUnavailable;
    };


    
    class TileProvider {
    public:
        virtual ~TileProvider() { }

        virtual Tile getTile(const ChunkIndex& chunkIndex) = 0;
        virtual Tile getDefaultTile() = 0;
        virtual Tile::Status getTileStatus(const ChunkIndex& index) = 0;
        virtual TileDepthTransform depthTransform() = 0;
        virtual void update() = 0;
        virtual int maxLevel() = 0;
    };


    typedef LRUCache<ChunkHashKey, Tile> TileCache;


    class SingleImagePrivoder : public TileProvider {
    public:
        SingleImagePrivoder(const std::string& imagePath);
        virtual ~SingleImagePrivoder() { }

        virtual Tile getTile(const ChunkIndex& chunkIndex);
        virtual Tile getDefaultTile();
        virtual Tile::Status getTileStatus(const ChunkIndex& index);
        virtual TileDepthTransform depthTransform();
        virtual void update();
        virtual int maxLevel();
    private:
        Tile _tile;

    };


    /**
        Provides tiles through GDAL datasets which can be defined with xml files
        for example for wms.
    */
    class CachingTileProvider : public TileProvider {
    public:


        CachingTileProvider(
            std::shared_ptr<AsyncTileDataProvider> tileReader, 
            std::shared_ptr<TileCache> tileCache,
            int framesUntilFlushRequestQueue);

        virtual ~CachingTileProvider();
        
        virtual Tile getTile(const ChunkIndex& chunkIndex);
        virtual Tile getDefaultTile();
        virtual Tile::Status getTileStatus(const ChunkIndex& index);
        virtual TileDepthTransform depthTransform();
        virtual void update();
        virtual int maxLevel();


    private:


        //////////////////////////////////////////////////////////////////////////////////
        //                                Helper functions                              //
        //////////////////////////////////////////////////////////////////////////////////
        
        Tile getOrStartFetchingTile(ChunkIndex chunkIndex);


        
        /**
            Creates an OpenGL texture and pushes the data to the GPU.
        */

        Tile createTile(std::shared_ptr<TileIOResult> res);

        void clearRequestQueue();

        void initTexturesFromLoadedData();



        //////////////////////////////////////////////////////////////////////////////////
        //                                Member variables                              //
        //////////////////////////////////////////////////////////////////////////////////

        std::shared_ptr<TileCache> _tileCache;
        Tile _defaultTile;

        int _framesSinceLastRequestFlush;
        int _framesUntilRequestFlush;


        std::shared_ptr<AsyncTileDataProvider> _asyncTextureDataProvider;
    };

    


}  // namespace openspace




#endif  // __TILE_PROVIDER_H__