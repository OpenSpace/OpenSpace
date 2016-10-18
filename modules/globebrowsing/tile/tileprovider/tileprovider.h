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

#include <ghoul/filesystem/filesystem.h> // absPath
#include <ghoul/opengl/texture.h>
#include <ghoul/misc/dictionary.h>

#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/other/lrucache.h>

//////////////////////////////////////////////////////////////////////////////////////////
//                                    TILE PROVIDER                                     //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {
namespace globebrowsing {

    using namespace ghoul::opengl;
    
    /**
    * Interface for providing <code>Tile</code>s given a 
    * <code>TileIndex</code>. 
    */
    class TileProvider {
    public:

        /**
        * Factory method for instantiating different implementations of 
        * <code>TileProviders</code>. The provided dictionary must 
        * define a key specifying what implementation of TileProvider
        * to be instantiated.
        */
        static TileProvider* createFromDictionary(const ghoul::Dictionary& dictionary);

        /** 
        * Empty default constructor 
        */
        TileProvider() {};

        /**
        * Implementations of the TileProvider interface must implement 
        * a constructor taking a dictionary as input. The provided 
        * dictionary must define a key specifying what implementation 
        * of TileProvider to be instantiated.
        */
        TileProvider(const ghoul::Dictionary& dictionary);

        /**
        * Virtual destructor that subclasses should override to do
        * clean up.
        */
        virtual ~TileProvider() { }

        /**
        * Method for querying tiles, given a specified <code>TileIndex</code>.
        *
        * This method is expected to be invoked multiple times per frame,
        * and should therefore return quickly, e.g. not perform heavy I/O 
        * operations. However, invoking this method may spawn separate threads
        * to perform such operations. Therefore, programmers shoud 
        * note that there is no guarantee that the <code>Tile</code> 
        * status and texture will be consistent over different invocations
        * of this method.
        *
        * \param tileIndex specifying a region of a map for which 
        * we want tile data.
        *
        * \returns The tile corresponding to the TileIndex by the time
        * the method was invoked.
        */
        virtual Tile getTile(const TileIndex& tileIndex) = 0;

        /**
        * TileProviders must be able to provide a defualt
        * <code>Tile</code> which may be used by clients in cases when
        * requested tiles were unavailable.
        *
        * \returns A default tile
        */
        virtual Tile getDefaultTile() = 0;

        /**
        * Returns the status of a <code>Tile</code>. The <code>Tile::Status</code>
        * corresponds the <code>Tile</code> that would be returned
        * if the function <code>getTile</code> would be invoked with the same
        * <code>TileIndex</code> argument at this point in time.
        */
        virtual Tile::Status getTileStatus(const TileIndex& index) = 0;

        /**
        * Get the associated depth transform for this TileProvider.
        * This is necessary for TileProviders serving height map 
        * data, in order to correcly map pixel values to meters.
        */
        virtual TileDepthTransform depthTransform() = 0;

        /**
        * This method should be called once per frame. Here, TileProviders
        * are given the opportunity to update their internal state.
        */
        virtual void update() = 0;

        /**
        * Provides a uniform way of all TileProviders to reload or 
        * restore all of its internal state. This is mainly useful 
        * for debugging purposes.
        */
        virtual void reset() = 0;

        /**
        * \returns The maximum level as defined by <code>TileIndex</code>
        * that this TileProvider is able provide.
        */
        virtual int maxLevel() = 0;
    };

    typedef LRUCache<TileHashKey, Tile> TileCache;

    struct TileProviderInitData {
        int minimumPixelSize;
        int threads;
        int cacheSize;
        int framesUntilRequestQueueFlush;
        bool preprocessTiles = false;
    };

} // namespace globebrowsing
} // namespace openspace

#endif  // __TILE_PROVIDER_H__