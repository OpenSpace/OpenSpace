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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE_PROVIDER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/geometry/ellipsoid.h>
#include <modules/globebrowsing/rendering/layer/layergroupid.h>
#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/tiletextureinitdata.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/scalar/intproperty.h>

#ifdef GLOBEBROWSING_USE_GDAL
#include <modules/globebrowsing/other/timequantizer.h>
#include <openspace/properties/stringproperty.h>
#include <unordered_map>

struct CPLXMLNode;

#endif // GLOBEBROWSING_USE_GDAL

namespace ghoul::fontrendering {
    class Font;
    class FontRenderer;
} // namespace ghoul::fontrendering

namespace openspace { class PixelBuffer; }

namespace openspace::globebrowsing {
    class AsyncTileDataProvider;
    struct RawTile;

    namespace cache { class MemoryAwareTileCache; }
} // namespace openspace::globebrowsing


namespace openspace::globebrowsing {
    struct ChunkTile;
    using ChunkTilePile = std::vector<ChunkTile>;
    struct TileDepthTransform;
    struct TileIndex;
} // namespace openspace::globebrowsing

namespace openspace::globebrowsing::tileprovider {

enum class Type {
    Default = 0,
    //Projection,
    SingleImage,
    SizeReference,
    Temporal,
    TileIndex,
    ByIndex,
    ByLevel,
    SolidColor
};






struct TileProvider : public properties::PropertyOwner {
    static unsigned int NumTileProviders;

    Type type;

    TileProvider(const ghoul::Dictionary& dictionary);
    virtual ~TileProvider() = default;

    std::string name;

    unsigned int uniqueIdentifier = 0;
    bool isInitialized = false;

    std::unique_ptr<ghoul::opengl::Texture> defaultTileTexture;
    Tile defaultTile = Tile(nullptr, nullptr, Tile::Status::Unavailable);
};

struct DefaultTileProvider : public TileProvider {
    DefaultTileProvider(const ghoul::Dictionary& dictionary);
    // This one still used?
    //DefaultTileProvider(std::shared_ptr<AsyncTileDataProvider> reader);

    std::shared_ptr<AsyncTileDataProvider> _asyncTextureDataProvider;

    cache::MemoryAwareTileCache* _tileCache = nullptr;

    properties::StringProperty _filePath;
    properties::IntProperty _tilePixelSize;
    layergroupid::GroupID _layerGroupID = layergroupid::GroupID::Unknown;
    int _preCacheLevel = 0;
    bool _performPreProcessing = false;
    bool _padTiles = true;
};

struct SingleImageProvider : public TileProvider {
    SingleImageProvider(const ghoul::Dictionary& dictionary);
    //SingleImageProvider(const std::string& imagePath);

    std::unique_ptr<ghoul::opengl::Texture> _tileTexture;
    Tile _tile;

    properties::StringProperty _filePath;
};

struct TextTileProvider : public TileProvider {
    TextTileProvider(const ghoul::Dictionary& dictionary,
        const TileTextureInitData& initData, size_t fontSize = 48);

    const TileTextureInitData _initData;
    std::shared_ptr<ghoul::fontrendering::Font> _font;
    size_t _fontSize;

    std::unique_ptr<ghoul::fontrendering::FontRenderer> _fontRenderer;

    std::string text;
    glm::vec2 textPosition;
    glm::vec4 textColor;
    bool textIsDirty = true;

    GLuint _fbo = 0;

    cache::MemoryAwareTileCache* _tileCache;

};

struct SizeReferenceTileProvider : public TextTileProvider {
    SizeReferenceTileProvider(const ghoul::Dictionary& dictionary);

    Ellipsoid _ellipsoid;
    Tile _backgroundTile = Tile::TileUnavailable;
};

struct TileIndexTileProvider : public TextTileProvider {
    TileIndexTileProvider(const ghoul::Dictionary& dict);
};

struct TileProviderByIndex : public TileProvider {
    TileProviderByIndex(const ghoul::Dictionary& dictionary);

    std::unordered_map<
        TileIndex::TileHashKey, std::shared_ptr<TileProvider>
    > _tileProviderMap;
    std::shared_ptr<TileProvider> _defaultTileProvider;
};

struct TileProviderByLevel : public TileProvider {
    TileProviderByLevel(const ghoul::Dictionary& dictionary);

    std::vector<int> _providerIndices;
    std::vector<std::shared_ptr<TileProvider>> _levelTileProviders;

};


#ifdef GLOBEBROWSING_USE_GDAL

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

    TemporalTileProvider(const ghoul::Dictionary& dictionary);

    using TimeKey = std::string;

    /**
     * A placeholder string that must be provided in the WMS template url. This
     * placeholder will be replaced by quantized date-time strings during run time
     * in order to access the datasets for different instances of time.
     */
    constexpr static const char* UrlTimePlaceholder = "${OpenSpaceTimeId}";

    /**
     * These are tags that TemporalTileProviders must be able to read from the XML
     * file provided in the ghoul::Dictionary used to create this provider. These
     * tags describe the temporal properties of the dataset.
     */
    struct TemporalXMLTags {
        /**
         * Tag should contain a ISO8601 time specifying the datasets start time
         */
        constexpr static const char* TimeStart = "OpenSpaceTimeStart";

        /**
         * Tag should contain a ISO8601 time specifying the datasets end time
         * Example 1: "2016 SEP 08".
         * Example 2: "now" - sets the dataset's end time to the current time.
         */
        constexpr static const char* TimeEnd = "OpenSpaceTimeEnd";

        /**
         * Tag should contain the time resolution of the dataset.
         * The resolution is defined by a number along with a unit specifying how
         * often the dataset is updated temporally. Supported units are:
         * (s)econds, (m)inutes, (h)ours, (d)ays, (y)ears.
         *
         * Example 1: "2d" - dataset updated every other day.
         * Example 2: "1h" - dataset is updated every hour.
         */
        constexpr static const char* TimeResolution = "OpenSpaceTimeResolution";

        /**
         * Tag should contain a string specifying the date-time format expected by the
         * WMS.
         */
        constexpr static const char* TimeFormat = "OpenSpaceTimeIdFormat";
    };

    /**
     * Create a GDAL dataset description based on the time t,
     *
     * \param t Time to generate a GDAL dataset description for
     * \return a GDAL dataset description
     */
    //std::string getGdalDatasetXML(const Time& t);

    /**
     * Create a GDAL dataset description associated with the provided TimeKey
     *
     * \param timeKey The TimeKey specifying time
     * \return a GDAL dataset description
     */
    //std::string getGdalDatasetXML(const TimeKey& timeKey);

    /**
     * Instantiates a new TileProvder for the temporal dataset at the time
     * specified.
     *
     * This method replaced the <code>UrlTimePlaceholder</code> in the template URL
     * with the provided timekey, the opens a new GDAL dataset with that URL.
     *
     * \param timekey time specifying dataset's temporality
     * \return newly instantiated TileProvider
     */
    //std::shared_ptr<TileProvider> initTileProvider(TimeKey timekey);

    /**
     * Takes as input a Openspace Temporal dataset description, extracts the temporal
     * metadata provided by reading the <code>TemporalXMLTags</code>, removes the
     * read tags from the description, and returns a GDAL template GDAL dataset
     * description. The template GDAL dataset description has the a
     * <code>UrlTimePlaceholder</code> still in it, which needs to be replaced before
     * GDAL can open it as a GDALDataset.
     *
     * \param xml Openspace Temporal dataset description
     * \returns a GDAL template data description.
     */
    //std::string consumeTemporalMetaData(const std::string &xml);

    /**
     * Helper method to read a XML value from a XML tree.
     *
     * \param node XML tree to search in
     * \param key XML tag to find the value for
     * \param defaultVal value to return if key was not found
     * \return the value of the Key, or defaultVal if key was undefined.
     */
    //std::string getXMLValue(CPLXMLNode* node, const std::string& key,
    //    const std::string& defaultVal);

    /**
     * Ensures that the TemporalTileProvider is up to date.
     */
    //void ensureUpdated();

    //bool readFilePath();

    // Used for creation of time specific instances of CachingTileProvider
    ghoul::Dictionary _initDict;
    properties::StringProperty _filePath;
    std::string _gdalXmlTemplate;

    std::unordered_map<TimeKey, std::shared_ptr<TileProvider>> _tileProviderMap;


    std::shared_ptr<TileProvider> _currentTileProvider;

    TimeFormatType _timeFormat;
    TimeQuantizer _timeQuantizer;

    std::vector<Time> _preCacheTimes;

    bool _successfulInitialization;
};

#endif // GLOBEBROWSING_USE_GDAL



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
 */
void update(TileProvider& tp);

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

void initializeDefaultTile(TileProvider& tp);

///**
// * Interface for providing <code>Tile</code>s given a
// * <code>TileIndex</code>.
// */
//class TileProvider : public properties::PropertyOwner {
//public:
//    /**
//     * Factory method for instantiating different implementations of
//     * <code>TileProviders</code>. The provided dictionary must
//     * define a key specifying what implementation of TileProvider
//     * to be instantiated.
//     */
//    static std::unique_ptr<TileProvider> createFromDictionary(
//        layergroupid::TypeID layerTypeID, const ghoul::Dictionary& dictionary);
//
//    /**
//     * Implementations of the TileProvider interface must implement
//     * a constructor taking a dictionary as input. The provided
//     * dictionary must define a key specifying what implementation
//     * of TileProvider to be instantiated.
//     */
//    TileProvider(const ghoul::Dictionary& dictionary = ghoul::Dictionary());
//
//    /**
//     * Virtual destructor that subclasses should override to do
//     * clean up.
//     */
//    virtual ~TileProvider();
//
//    virtual bool initialize();
//    virtual bool deinitialize();
//
//    /**
//     * Method for querying tiles, given a specified <code>TileIndex</code>.
//     *
//     * This method is expected to be invoked multiple times per frame,
//     *  and should therefore return quickly, e.g. not perform heavy I/O
//     * operations. However, invoking this method may spawn separate threads
//     * to perform such operations. Therefore, programmers shoud
//     *  note that there is no guarantee that the <code>Tile</code>
//     * status and texture will be consistent over different invocations
//     * of this method.
//     *
//     * \param tileIndex specifying a region of a map for which
//     * we want tile data.
//     *
//     * \returns The tile corresponding to the TileIndex by the time
//     * the method was invoked.
//     */
//    virtual Tile tile(const TileIndex& tileIndex) = 0;
//
//
//    virtual ChunkTile chunkTile(TileIndex tileIndex, int parents = 0,
//        int maxParents = 1337);
//
//    virtual ChunkTilePile chunkTilePile(TileIndex tileIndex, int pileSize);
//
//    Tile defaultTile() const;
//
//    /**
//     * Returns the status of a <code>Tile</code>. The <code>Tile::Status</code>
//     * corresponds the <code>Tile</code> that would be returned
//     * if the function <code>tile</code> would be invoked with the same
//     * <code>TileIndex</code> argument at this point in time.
//     */
//    virtual Tile::Status tileStatus(const TileIndex& index) = 0;
//
//    /**
//     * Get the associated depth transform for this TileProvider.
//     * This is necessary for TileProviders serving height map
//     * data, in order to correcly map pixel values to meters.
//     */
//    virtual TileDepthTransform depthTransform() = 0;
//
//    /**
//     * This method should be called once per frame. Here, TileProviders
//     * are given the opportunity to update their internal state.
//     */
//    virtual void update() = 0;
//
//    /**
//     * Provides a uniform way of all TileProviders to reload or
//     * restore all of its internal state. This is mainly useful
//     * for debugging purposes.
//     */
//    virtual void reset() = 0;
//
//    /**
//     * \returns The maximum level as defined by <code>TileIndex</code>
//     * that this TileProvider is able provide.
//     */
//    virtual int maxLevel() = 0;
//
//    /**
//     * \returns the no data value for the dataset. Default is the minimum float avalue.
//     */
//    virtual float noDataValueAsFloat();
//
//    /**
//     * \returns a unique identifier for the <code>TileProvider<\code>. All
//     * <code>TileProviders<\code> have an ID starting at 0 from the first created.
//     * The maximum number of unique identifiers is UINT_MAX
//     */
//    unsigned int uniqueIdentifier() const;
//
//protected:
//    std::string _name;
//
//private:
//    void initializeDefaultTile();
//
//    static unsigned int _numTileProviders;
//    unsigned int _uniqueIdentifier = 0;
//    bool _isInitialized = false;
//
//    std::unique_ptr<ghoul::opengl::Texture> _defaultTileTexture;
//    Tile _defaultTile = Tile(nullptr, nullptr, Tile::Status::Unavailable);
//};

} // namespace openspace::globebrowsing::tileprovider

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE_PROVIDER___H__
