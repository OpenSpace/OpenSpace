/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__TILEPROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__TILEPROVIDER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/layergroupid.h>
#include <modules/globebrowsing/src/tileindex.h>
#include <modules/globebrowsing/src/tiletextureinitdata.h>
#include <modules/globebrowsing/src/timequantizer.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/util/ellipsoid.h>
#include <unordered_map>
#include <ghoul/opengl/programobject.h>

struct CPLXMLNode;

namespace ghoul::fontrendering {
    class Font;
    class FontRenderer;
} // namespace ghoul::fontrendering

namespace openspace { class PixelBuffer; }

namespace openspace::globebrowsing {
    class AsyncTileDataProvider;
    struct RawTile;
    struct TileIndex;
    namespace cache { class MemoryAwareTileCache; }
} // namespace openspace::globebrowsing

namespace openspace::globebrowsing {

// If you add a new type, also add it to shaders/texturetilemapping.glsl
enum class Type {
    DefaultTileProvider = 0,
    SingleImageTileProvider,
    ImageSequenceTileProvider,
    SizeReferenceTileProvider,
    TemporalTileProvider,
    TileIndexTileProvider,
    ByIndexTileProvider,
    ByLevelTileProvider,
    InterpolateTileProvider,
    FfmpegTileProvider
};

struct TileProvider : public properties::PropertyOwner {
    static unsigned int NumTileProviders;

    static std::unique_ptr<TileProvider> createFromDictionary(
        const ghoul::Dictionary& dictionary);

    static void initializeDefaultTile();
    static void deinitializeDefaultTile();

    TileProvider();
    ~TileProvider() override = default;

    void initialize();
    void deinitialize();

    virtual Tile tile(const TileIndex& tileIndex) = 0;

    /**
     * Returns the status of a `Tile`. The `Tile::Status` corresponds the `Tile` that
     * would be returned if the function `tile` would be invoked with the same `TileIndex`
     * argument at this point in time.
     */
    virtual Tile::Status tileStatus(const TileIndex& index) = 0;

    /**
     * Get the associated depth transform for this TileProvider. This is necessary for
     * TileProviders serving height map data, in order to correcly map pixel values to
     * meters.
     */
    virtual TileDepthTransform depthTransform() = 0;

    /**
     * This method should be called once per frame. Here, TileProviders are given the
     * opportunity to update their internal state.
     */
    virtual void update() = 0;

    /**
     * Provides a uniform way of all TileProviders to reload or restore all of its
     * internal state. This is mainly useful for debugging purposes.
     */
    virtual void reset() = 0;

    /**
     * \return The minimum level as defined by the `TileIndex` that this TileProvider is
     *         capable of providing
     */
    virtual int minLevel() = 0;

    /**
     * \return The maximum level as defined by `TileIndex` that this TileProvider is able
     *         to provide
     */
    virtual int maxLevel() = 0;

    /**
     * \return The no data value for the dataset. Default is the minimum float value
     */
    virtual float noDataValueAsFloat() = 0;


    virtual ChunkTile chunkTile(TileIndex tileIndex, int parents = 0,
        int maxParents = 1337);
    ChunkTilePile chunkTilePile(TileIndex tileIndex, int pileSize);


    std::string name;

    uint16_t uniqueIdentifier = 0;
    bool isInitialized = false;

protected:
    ChunkTile traverseTree(TileIndex tileIndex, int parents, int maxParents,
        const std::function<void(TileIndex&, TileUvTransform&)>& ascendToParent,
        TileUvTransform& uvTransform);

private:
    virtual void internalInitialize();
    virtual void internalDeinitialize();
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILEPROVIDER__TILEPROVIDER___H__
