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

#ifndef __CACHING_TILE_PROVIDER_H__
#define __CACHING_TILE_PROVIDER_H__


#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h> // absPath
#include <ghoul/opengl/texture.h>
#include <ghoul/io/texture/texturereader.h>

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/tile/asynctilereader.h>
#include <modules/globebrowsing/other/lrucache.h>


//////////////////////////////////////////////////////////////////////////////////////////
//                                    TILE PROVIDER                                     //
//////////////////////////////////////////////////////////////////////////////////////////


namespace openspace {

    /**
    * Provides tiles loaded by <code>AsyncTileDataProvider</code> and 
    * caches them in memory using LRU caching
    */
    class CachingTileProvider : public TileProvider {
    public:

        CachingTileProvider(const ghoul::Dictionary& dictionary);

        CachingTileProvider(
            std::shared_ptr<AsyncTileDataProvider> tileReader, 
            std::shared_ptr<TileCache> tileCache,
            int framesUntilFlushRequestQueue);

        virtual ~CachingTileProvider();
        
        /**
        * \returns a Tile with status OK iff it exists in in-memory 
        * cache. If not, it may enqueue some IO operations on a 
        * separate thread.
        */
        virtual Tile getTile(const ChunkIndex& chunkIndex);

        virtual Tile getDefaultTile();
        virtual Tile::Status getTileStatus(const ChunkIndex& chunkIndex);
        virtual TileDepthTransform depthTransform();
        virtual void update();
        virtual void reset();
        virtual int maxLevel();


    private:


        //////////////////////////////////////////////////////////////////////////////////
        //                                Helper functions                              //
        //////////////////////////////////////////////////////////////////////////////////

        /**
        * Collects all asynchronously downloaded <code>TileIOResult</code>
        * and uses <code>createTile</code> to create <code>Tile</code>s, 
        * which are put in the LRU cache - potentially pushing out outdated
        * Tiles.
        */
        void initTexturesFromLoadedData();

        /**
        * \returns A tile with <code>Tile::Status::OK</code> if no errors
        * occured, a tile with <code>Tile::Status::IOError</code> otherwise
        */
        Tile createTile(std::shared_ptr<TileIOResult> res);

        /**
        * Deletes all enqueued, but not yet started async downloads of textures.
        * Note that this does not cancel any currently ongoing async downloads.
        */
        void clearRequestQueue();


        //////////////////////////////////////////////////////////////////////////////////
        //                                Member variables                              //
        //////////////////////////////////////////////////////////////////////////////////

        std::shared_ptr<AsyncTileDataProvider> _asyncTextureDataProvider;
        std::shared_ptr<TileCache> _tileCache;

        int _framesSinceLastRequestFlush;
        int _framesUntilRequestFlush;

        Tile _defaultTile;
    };

}  // namespace openspace




#endif  // __CACHING_TILE_PROVIDER_H__