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
#include <modules/globebrowsing/other/asynctilereader.h>

//////////////////////////////////////////////////////////////////////////////////////////
//									TILE PROVIDER									    //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {
    using namespace ghoul::opengl;

    
    struct TileUvTransform
    {
        glm::vec2 uvOffset;
        glm::vec2 uvScale;
    };
    
    struct Tile {
        std::shared_ptr<Texture> texture;
        TileUvTransform uvTransform;
    };
    
    struct MetaTexture {
        std::shared_ptr<Texture> texture;
        CPLErr ioError;
    };





    /**
        Provides tiles through GDAL datasets which can be defined with xml files
        for example for wms.
    */
    class TileProvider {
    public:

        TileProvider(std::shared_ptr<AsyncTileDataProvider> tileReader, int tileCacheSize,
            int framesUntilFlushRequestQueue);

        ~TileProvider();

        
        Tile getHighestResolutionTile(ChunkIndex chunkIndex, int parents = 0, 
            TileUvTransform uvTransform = { glm::vec2(0, 0), glm::vec2(1, 1)});

        TileDepthTransform depthTransform();

        void prerender();


    private:


        //////////////////////////////////////////////////////////////////////////////////
        //                                Helper functions                              //
        //////////////////////////////////////////////////////////////////////////////////
        Tile getOrEnqueueHighestResolutionTile(const ChunkIndex& ci, 
            TileUvTransform& uvTransform);
        
        std::shared_ptr<Texture> getOrStartFetchingTile(ChunkIndex chunkIndex);


        void transformFromParent(const ChunkIndex& ci, TileUvTransform& uv) const;


        
        /**
            Creates an OpenGL texture and pushes the data to the GPU.
        */
        void initializeAndAddToCache(std::shared_ptr<TileIOResult> uninitedTexture);

        void clearRequestQueue();

        void initTexturesFromLoadedData();



        //////////////////////////////////////////////////////////////////////////////////
        //                                Member variables                              //
        //////////////////////////////////////////////////////////////////////////////////

        LRUCache<HashKey, MetaTexture> _tileCache;

        int _framesSinceLastRequestFlush;
        int _framesUntilRequestFlush;


        std::shared_ptr<AsyncTileDataProvider> _asyncTextureDataProvider;
    };

    


}  // namespace openspace




#endif  // __TILE_PROVIDER_H__