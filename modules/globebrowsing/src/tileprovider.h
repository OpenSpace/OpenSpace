/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE_PROVIDER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/ellipsoid.h>
#include <modules/globebrowsing/src/layergroupid.h>
#include <modules/globebrowsing/src/tileindex.h>
#include <modules/globebrowsing/src/tiletextureinitdata.h>
#include <modules/globebrowsing/src/timequantizer.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <unordered_map>

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

namespace openspace::globebrowsing::tileprovider {

enum class Type {
    DefaultTileProvider = 0,
    SingleImageTileProvider,
    SizeReferenceTileProvider,
    TemporalTileProvider,
    TileIndexTileProvider,
    ByIndexTileProvider,
    ByLevelTileProvider
};


struct TileProvider : public properties::PropertyOwner {
    static unsigned int NumTileProviders;

    Type type;

    TileProvider();
    virtual ~TileProvider() = default;

    std::string name;

    unsigned int uniqueIdentifier = 0;
    bool isInitialized = false;
};

struct DefaultTileProvider : public TileProvider {
    DefaultTileProvider(const ghoul::Dictionary& dictionary);

    std::unique_ptr<AsyncTileDataProvider> asyncTextureDataProvider;

    cache::MemoryAwareTileCache* tileCache = nullptr;

    properties::StringProperty filePath;
    properties::IntProperty tilePixelSize;
    layergroupid::GroupID layerGroupID = layergroupid::GroupID::Unknown;
    bool performPreProcessing = false;
    bool padTiles = true;
};

struct SingleImageProvider : public TileProvider {
    SingleImageProvider(const ghoul::Dictionary& dictionary);

    std::unique_ptr<ghoul::opengl::Texture> tileTexture;
    Tile tile;

    properties::StringProperty filePath;
};

struct TextTileProvider : public TileProvider {
    TextTileProvider(TileTextureInitData initData, size_t fontSize = 48);

    const TileTextureInitData initData;

    std::unique_ptr<ghoul::fontrendering::FontRenderer> fontRenderer;
    std::shared_ptr<ghoul::fontrendering::Font> font;
    size_t fontSize;

    std::string text;
    glm::vec2 textPosition;
    glm::vec4 textColor;

    GLuint fbo = 0;

    cache::MemoryAwareTileCache* tileCache;
};

struct SizeReferenceTileProvider : public TextTileProvider {
    SizeReferenceTileProvider(const ghoul::Dictionary& dictionary);

    Ellipsoid ellipsoid;
};

struct TileIndexTileProvider : public TextTileProvider {
    TileIndexTileProvider(const ghoul::Dictionary& dictionary);
};

struct TileProviderByIndex : public TileProvider {
    TileProviderByIndex(const ghoul::Dictionary& dictionary);

    std::unordered_map<
        TileIndex::TileHashKey, std::unique_ptr<TileProvider>
    > tileProviderMap;
    std::unique_ptr<TileProvider> defaultTileProvider;
};

struct TileProviderByLevel : public TileProvider {
    TileProviderByLevel(const ghoul::Dictionary& dictionary);

    std::vector<int> providerIndices;
    std::vector<std::unique_ptr<TileProvider>> levelTileProviders;
};


/**
 * Provide <code>Tile</code>s from web map services that have temporal resolution.
 *
 * TemporalTileProviders are instantiated using a ghoul::Dictionary,
 * and must define a filepath to a Openspace Temporal dataset description file.
 * This is an xml-file that defines the same meta data as the GDAL wms description
 * (http://www.gdal.org/frmt_wms.html), but augmented with some
 * extra tags describing the temporal properties of the dataset. See
 * <code>TemporalTileProvider::TemporalXMLTags</code>
 *
 */
struct TemporalTileProvider : public TileProvider {
    enum class TimeFormatType {
        YYYY_MM_DD = 0,
        YYYYMMDD_hhmmss,
        YYYYMMDD_hhmm,
        YYYY_MM_DDThhColonmmColonssZ,
        YYYY_MM_DDThh_mm_ssZ
    };

    using TimeKey = std::string;

    TemporalTileProvider(const ghoul::Dictionary& dictionary);

    ghoul::Dictionary initDict;
    properties::StringProperty filePath;
    std::string gdalXmlTemplate;

    std::unordered_map<TimeKey, std::unique_ptr<TileProvider>> tileProviderMap;

    TileProvider* currentTileProvider = nullptr;

    TimeFormatType timeFormat;
    TimeQuantizer timeQuantizer;

    bool successfulInitialization = false;
};



void initializeDefaultTile();
void deinitializeDefaultTile();

std::unique_ptr<TileProvider> createFromDictionary(layergroupid::TypeID layerTypeID,
    const ghoul::Dictionary& dictionary);

bool initialize(TileProvider& tp);
bool deinitialize(TileProvider& tp);

Tile tile(TileProvider& tp, const TileIndex& tileIndex);

ChunkTile chunkTile(TileProvider& tp, TileIndex tileIndex, int parents = 0,
    int maxParents = 1337);

ChunkTilePile chunkTilePile(TileProvider& tp, TileIndex tileIndex, int pileSize);

/**
 * Returns the status of a <code>Tile</code>. The <code>Tile::Status</code>
 * corresponds the <code>Tile</code> that would be returned
 * if the function <code>tile</code> would be invoked with the same
 * <code>TileIndex</code> argument at this point in time.
 */
Tile::Status tileStatus(TileProvider& tp, const TileIndex& index);

/**
 * Get the associated depth transform for this TileProvider.
 * This is necessary for TileProviders serving height map
 * data, in order to correcly map pixel values to meters.
 */
TileDepthTransform depthTransform(TileProvider& tp);

/**
 * This method should be called once per frame. Here, TileProviders
 * are given the opportunity to update their internal state.
 *
 * \return The number of tiles that have been updated in this call
 */
int update(TileProvider& tp);

/**
 * Provides a uniform way of all TileProviders to reload or
 * restore all of its internal state. This is mainly useful
 * for debugging purposes.
 */
void reset(TileProvider& tp);

/**
 * \returns The maximum level as defined by <code>TileIndex</code>
 * that this TileProvider is able provide.
 */
int maxLevel(TileProvider& tp);

/**
 * \returns the no data value for the dataset. Default is the minimum float avalue.
 */
float noDataValueAsFloat(TileProvider& tp);

} // namespace openspace::globebrowsing::tileprovider

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_PROVIDER___H__
