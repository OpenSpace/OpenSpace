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

#include <modules/globebrowsing/tile/tileprovider.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/cache/memoryawaretilecache.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/tile/asynctiledataprovider.h>
#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/tile/rawtiledatareader/gdalrawtiledatareader.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>

#ifdef GLOBEBROWSING_USE_GDAL
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/file.h>
#include <fstream>
#include "cpl_minixml.h"
#endif // GLOBEBROWSING_USE_GDAL

#pragma optimize ("", off)

#ifdef GLOBEBROWSING_USE_GDAL
namespace ghoul {
    template <>
    openspace::globebrowsing::tileprovider::TemporalTileProvider::TimeFormatType
        from_string(const std::string& string)
    {
        using namespace openspace::globebrowsing::tileprovider;
        if (string == "YYYY-MM-DD") {
            return TemporalTileProvider::TimeFormatType::YYYY_MM_DD;
        }
        else if (string == "YYYY-MM-DDThh:mm:ssZ") {
            return TemporalTileProvider::TimeFormatType::YYYY_MM_DDThhColonmmColonssZ;
        }
        else if (string == "YYYY-MM-DDThh_mm_ssZ") {
            return TemporalTileProvider::TimeFormatType::YYYY_MM_DDThh_mm_ssZ;
        }
        else if (string == "YYYYMMDD_hhmmss") {
            return TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmmss;
        }
        else if (string == "YYYYMMDD_hhmm") {
            return TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmm;
        }
        else {
            throw ghoul::RuntimeError("Unknown timeformat " + string);
        }
    }
} // namespace ghoul
#endif // GLOBEBROWSING_USE_GDAL


namespace openspace::globebrowsing::tileprovider {

namespace {

namespace defaultprovider {
    constexpr const char* KeyPerformPreProcessing = "PerformPreProcessing";
    constexpr const char* KeyTilePixelSize = "TilePixelSize";
    constexpr const char* KeyFilePath = "FilePath";
    constexpr const char* KeyPreCacheLevel = "PreCacheLevel";
    constexpr const char* KeyPadTiles = "PadTiles";

    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "The path of the GDAL file or the image file that is to be used in this tile "
        "provider."
    };

    constexpr openspace::properties::Property::PropertyInfo TilePixelSizeInfo = {
        "TilePixelSize",
        "Tile Pixel Size",
        "This value is the preferred size (in pixels) for each tile. Choosing the right "
        "value is a tradeoff between more efficiency (larger images) and better quality "
        "(smaller images). The tile pixel size has to be smaller than the size of the "
        "complete image if a single image is used."
    };
} // namespace defaultprovider

namespace singleimageprovider {
    constexpr const char* KeyFilePath = "FilePath";

    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "The file path that is used for this image provider. The file must point to an "
        "image that is then loaded and used for all tiles."
    };
} // namespace singleimageprovider

namespace sizereferenceprovider {
    constexpr const char* KeyRadii = "Radii";
} // namespace sizereferenceprovider

namespace byindexprovider {
    constexpr const char* KeyDefaultProvider = "DefaultProvider";
    constexpr const char* KeyProviders = "IndexTileProviders";
    constexpr const char* KeyTileIndex = "TileIndex";
    constexpr const char* KeyTileProvider = "TileProvider";
} // namespace byindexprovider

namespace bylevelprovider {
    constexpr const char* KeyProviders = "LevelTileProviders";
    constexpr const char* KeyMaxLevel = "MaxLevel";
    constexpr const char* KeyTileProvider = "TileProvider";
    constexpr const char* KeyLayerGroupID = "LayerGroupID";
} // namespace bylevelprovider

#ifdef  GLOBEBROWSING_USE_GDAL
namespace temporal {
    constexpr const char* KeyFilePath = "FilePath";
    constexpr const char* KeyBasePath = "BasePath";
    constexpr const char* KeyPreCacheStartTime = "PreCacheStartTime";
    constexpr const char* KeyPreCacheEndTime = "PreCacheEndTime";

    constexpr const char* UrlTimePlaceholder = "${OpenSpaceTimeId}";
    constexpr const char* TimeStart = "OpenSpaceTimeStart";
    constexpr const char* TimeEnd = "OpenSpaceTimeEnd";
    constexpr const char* TimeResolution = "OpenSpaceTimeResolution";
    constexpr const char* TimeFormat = "OpenSpaceTimeIdFormat";

    constexpr openspace::properties::Property::PropertyInfo FilePathInfo = {
        "FilePath",
        "File Path",
        "This is the path to the XML configuration file that describes the temporal tile "
        "information."
    };
} // namespace temporal
#endif // GLOBEBROWSING_USE_GDAL

Type toType(const layergroupid::TypeID& id) {
    using T = layergroupid::TypeID;
    switch (id) {
        case T::Unknown:
            throw ghoul::MissingCaseException();
        case T::DefaultTileLayer:
            return Type::DefaultTileProvider;
        case T::SingleImageTileLayer:
            return Type::SingleImageTileProvider;
        case T::SizeReferenceTileLayer:
            return Type::SizeReferenceTileProvider;
        case T::TemporalTileLayer:
            return Type::TemporalTileProvider;
        case T::TileIndexTileLayer:
            return Type::TileIndexTileProvider;
        case T::ByIndexTileLayer:
            return Type::ByIndexTileProvider;
        case T::ByLevelTileLayer:
            return Type::ByLevelTileProvider;
        default:
            throw ghoul::MissingCaseException();
    }
}

void initAsyncTileDataReader(DefaultTileProvider& t, TileTextureInitData initData) {
    RawTileDataReader::PerformPreprocessing preprocess =
        RawTileDataReader::PerformPreprocessing(t.performPreProcessing);

    // Initialize instance variables
#ifdef GLOBEBROWSING_USE_GDAL
    std::shared_ptr<GdalRawTileDataReader> tileDataset =
        std::make_shared<GdalRawTileDataReader>(t.filePath, initData, preprocess);
#else // GLOBEBROWSING_USE_GDAL
    std::shared_ptr<SimpleRawTileDataReader> tileDataset =
        std::make_shared<SimpleRawTileDataReader>(t._filePath, initData, preprocess);
#endif // GLOBEBROWSING_USE_GDAL

    t.asyncTextureDataProvider = std::make_unique<AsyncTileDataProvider>(
        t.name,
        tileDataset
    );
}

void initTexturesFromLoadedData(DefaultTileProvider& t) {
    if (t.asyncTextureDataProvider) {
        std::shared_ptr<RawTile> tile = t.asyncTextureDataProvider->popFinishedRawTile();
        if (tile) {
            const cache::ProviderTileKey key = { tile->tileIndex, t.uniqueIdentifier };
            ghoul_assert(!t.tileCache->exist(key), "Tile must not be existing in cache");
            t.tileCache->createTileAndPut(key, *tile);
        }
    }
}

Tile createChunkIndexTile(TextTileProvider& t, const TileIndex& tileIndex) {
    ghoul::opengl::Texture* texture = t.tileCache->texture(t.initData);

    // Keep track of defaultFBO and viewport to be able to reset state when done
    GLint defaultFBO;
    GLint viewport[4];
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    glGetIntegerv(GL_VIEWPORT, viewport);

    // Render to texture
    glBindFramebuffer(GL_FRAMEBUFFER, t.fbo);
    glFramebufferTexture2D(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D,
        *texture,
        0
    );

    glViewport(
        0,
        0,
        static_cast<GLsizei>(texture->width()),
        static_cast<GLsizei>(texture->height())
    );

    glClearColor(0.f, 0.f, 0.f, 0.f);
    glClear(GL_COLOR_BUFFER_BIT);

    ghoul_assert(t.fontRenderer != nullptr, "_fontRenderer must not be null");

    t.fontRenderer->render(*t.font, t.textPosition, t.text, t.textColor);

    // Reset state: bind default FBO and set viewport to what it was
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

    return Tile(texture, nullptr, Tile::Status::OK);
}

void initialize(TextTileProvider& t) {
    t.font = global::fontManager.font("Mono", static_cast<float>(t.fontSize));
    t.fontRenderer = ghoul::fontrendering::FontRenderer::createDefault();
    t.fontRenderer->setFramebufferSize(glm::vec2(t.initData.dimensions()));
    glGenFramebuffers(1, &t.fbo);
}

void deinitialize(TextTileProvider& t) {
    glDeleteFramebuffers(1, &t.fbo);
}

Tile tile(TextTileProvider& t, const TileIndex& tileIndex) {
    cache::ProviderTileKey key = { tileIndex, t.uniqueIdentifier };
    Tile tile = t.tileCache->get(key);
    if (!tile.texture()) {
        tile = createChunkIndexTile(t, tileIndex);
        t.tileCache->put(key, t.initData.hashKey(), tile);
    }
    return tile;
}

void reset(TextTileProvider& t) {
    t.tileCache->clear();
}

TileProvider* levelProvider(TileProviderByLevel& t, int level) {
    if (!t.levelTileProviders.empty()) {
        int clampedLevel = glm::clamp(
            level,
            0,
            static_cast<int>(t.providerIndices.size() - 1)
        );
        int idx = t.providerIndices[clampedLevel];
        return t.levelTileProviders[idx].get();
    }
    else {
        return nullptr;
    }
}

std::string timeStringify(TemporalTileProvider::TimeFormatType type, const Time& t) {
    switch (type) {
        case TemporalTileProvider::TimeFormatType::YYYY_MM_DD:
            return t.ISO8601().substr(0, 10);
        case TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmmss: {
            std::string ts = t.ISO8601().substr(0, 19);

            // YYYY_MM_DDThh_mm_ss -> YYYYMMDD_hhmmss
            ts.erase(std::remove(ts.begin(), ts.end(), '-'), ts.end());
            ts.erase(std::remove(ts.begin(), ts.end(), ':'), ts.end());
            replace(ts.begin(), ts.end(), 'T', '_');
            return ts;
        }
        case TemporalTileProvider::TimeFormatType::YYYYMMDD_hhmm: {
            std::string ts = t.ISO8601().substr(0, 16);

            // YYYY_MM_DDThh_mm -> YYYYMMDD_hhmm
            ts.erase(std::remove(ts.begin(), ts.end(), '-'), ts.end());
            ts.erase(std::remove(ts.begin(), ts.end(), ':'), ts.end());
            replace(ts.begin(), ts.end(), 'T', '_');
            return ts;
        }
        case TemporalTileProvider::TimeFormatType::YYYY_MM_DDThhColonmmColonssZ:
            return t.ISO8601().substr(0, 19) + "Z";
        case TemporalTileProvider::TimeFormatType::YYYY_MM_DDThh_mm_ssZ: {
            std::string timeString = t.ISO8601().substr(0, 19) + "Z";
            replace(timeString.begin(), timeString.end(), ':', '_');
            return timeString;
        }
        default:
            throw ghoul::MissingCaseException();
    }
}

std::string getGdalDatasetXML(TemporalTileProvider& t,
                              const TemporalTileProvider::TimeKey& timeKey)
{
    std::string xmlTemplate(t.gdalXmlTemplate);
    const size_t pos = xmlTemplate.find(temporal::UrlTimePlaceholder);
    const size_t numChars = strlen(temporal::UrlTimePlaceholder);
    // @FRAGILE:  This will only find the first instance. Dangerous if that instance is
    // commented out ---abock
    const std::string timeSpecifiedXml = xmlTemplate.replace(pos, numChars, timeKey);
    return timeSpecifiedXml;
}

std::shared_ptr<TileProvider> initTileProvider(TemporalTileProvider& t,
                                               TemporalTileProvider::TimeKey timekey)
{
    static const std::vector<std::string> AllowedTokens = {
        // From: http://www.gdal.org/frmt_wms.html
        // @FRAGILE:  What happens if a user specifies one of these as path tokens?
        // ---abock
        "${x}",
        "${y}",
        "${z}",
        "${version}",
        "${format}",
        "${layer}"
    };

    std::string gdalDatasetXml = getGdalDatasetXML(t, std::move(timekey));
    FileSys.expandPathTokens(gdalDatasetXml, AllowedTokens);

    t.initDict.setValue<std::string>(temporal::KeyFilePath, gdalDatasetXml);
    return std::make_shared<DefaultTileProvider>(t.initDict);
}

std::shared_ptr<TileProvider> getTileProvider(TemporalTileProvider& t,
                                             const TemporalTileProvider::TimeKey& timekey)
{
    const auto it = t.tileProviderMap.find(timekey);
    if (it != t.tileProviderMap.end()) {
        return it->second;
    }
    else {
        std::shared_ptr<TileProvider> tileProvider = initTileProvider(t, timekey);
        initialize(*tileProvider);

        t.tileProviderMap[timekey] = tileProvider;
        return tileProvider;
    }
}

std::shared_ptr<TileProvider> getTileProvider(TemporalTileProvider& t, const Time& time) {
    Time tCopy(time);
    if (t.timeQuantizer.quantize(tCopy, true)) {
        TemporalTileProvider::TimeKey timeKey = timeStringify(t.timeFormat, tCopy);
        try {
            return getTileProvider(t, timeKey);
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC("TemporalTileProvider", e.message);
            return nullptr;
        }
    }
    return nullptr;
}

void ensureUpdated(TemporalTileProvider& t) {
    if (!t.currentTileProvider) {
        update(t);
    }
}

std::string getXMLValue(TemporalTileProvider& t, CPLXMLNode* node, const std::string& key,
                        const std::string& defaultVal)
{
    CPLXMLNode* n = CPLSearchXMLNode(node, key.c_str());
    if (!n) {
        throw ghoul::RuntimeError(
            fmt::format("Unable to parse file {}. {} missing", t.filePath.value(), key)
        );
    }

    const bool hasValue = n && n->psChild && n->psChild->pszValue;
    return hasValue ? n->psChild->pszValue : defaultVal;
}

std::string consumeTemporalMetaData(TemporalTileProvider& t, const std::string& xml) {
    CPLXMLNode* node = CPLParseXMLString(xml.c_str());

    std::string timeStart = getXMLValue(t, node, temporal::TimeStart, "2000 Jan 1");
    std::string timeResolution = getXMLValue(t, node, temporal::TimeResolution, "2d");
    std::string timeEnd = getXMLValue(t, node, temporal::TimeEnd, "Today");
    std::string timeIdFormat = getXMLValue(
        t,
        node,
        temporal::TimeFormat,
        "YYYY-MM-DDThh:mm:ssZ"
    );

    Time start;
    start.setTime(std::move(timeStart));
    Time end(Time::now());
    if (timeEnd == "Yesterday") {
        end.advanceTime(-60.0 * 60.0 * 24.0); // Go back one day
    }
    else if (timeEnd != "Today") {
        end.setTime(std::move(timeEnd));
    }

    try {
        t.timeQuantizer = TimeQuantizer(start, end, timeResolution);
    }
    catch (const ghoul::RuntimeError& e) {
        throw ghoul::RuntimeError(fmt::format(
            "Could not create time quantizer for Temporal GDAL dataset '{}'. {}",
            t.filePath.value(), e.message
        ));
    }
    t.timeFormat = ghoul::from_string<TemporalTileProvider::TimeFormatType>(timeIdFormat);

    std::string gdalDescription;
    CPLXMLNode* gdalNode = CPLSearchXMLNode(node, "GDAL_WMS");
    if (gdalNode) {
        gdalDescription = CPLSerializeXMLTree(gdalNode);
    }
    else {
        gdalNode = CPLSearchXMLNode(node, "FilePath");
        gdalDescription = std::string(gdalNode->psChild->pszValue);
    }

    return gdalDescription;
}

bool readFilePath(TemporalTileProvider& t) {
    std::ifstream in(t.filePath.value().c_str());
    std::string xml;
    if (in.is_open()) {
        // read file
        xml = std::string(
            std::istreambuf_iterator<char>(in),
            (std::istreambuf_iterator<char>())
        );
    }
    else {
        // Assume that it is already an xml
        xml = t.filePath;
    }

    // File path was not a path to a file but a GDAL config or empty
    ghoul::filesystem::File f(t.filePath);
    if (FileSys.fileExists(f)) {
        t.initDict.setValue<std::string>(temporal::KeyBasePath, f.directoryName());
    }

    t.gdalXmlTemplate = consumeTemporalMetaData(t, xml);
    return true;
}

} // namespace

unsigned int TileProvider::NumTileProviders = 0;

std::unique_ptr<TileProvider> createFromDictionary(layergroupid::TypeID layerTypeID,
                                                   const ghoul::Dictionary& dictionary)
{
    const char* type = layergroupid::LAYER_TYPE_NAMES[static_cast<int>(layerTypeID)];
    auto factory = FactoryManager::ref().factory<TileProvider>();
    std::unique_ptr<TileProvider> result = factory->create(type, dictionary);
    return result;
}

TileProvider::TileProvider() : properties::PropertyOwner({ "tileProvider" }) {}

DefaultTileProvider::DefaultTileProvider(const ghoul::Dictionary& dictionary)
    : filePath(defaultprovider::FilePathInfo, "")
    , tilePixelSize(defaultprovider::TilePixelSizeInfo, 32, 32, 2048)
{
    type = Type::DefaultTileProvider;

    tileCache = global::moduleEngine.module<GlobeBrowsingModule>()->tileCache();
    name = "Name unspecified";
    if (dictionary.hasKeyAndValue<std::string>("Name")) {
        name = dictionary.value<std::string>("Name");
    }
    std::string _loggerCat = "DefaultTileProvider (" + name + ")";

    // 1. Get required Keys
    filePath = dictionary.value<std::string>(defaultprovider::KeyFilePath);
    layerGroupID = dictionary.value<layergroupid::GroupID>("LayerGroupID");

    // 2. Initialize default values for any optional Keys
    // getValue does not work for integers
    int pixelSize = 0;
    if (dictionary.hasKeyAndValue<double>(defaultprovider::KeyTilePixelSize)) {
        pixelSize = static_cast<int>(
            dictionary.value<double>(defaultprovider::KeyTilePixelSize)
        );
        LDEBUG(fmt::format("Default pixel size overridden: {}", pixelSize));
    }

    if (dictionary.hasKeyAndValue<bool>(defaultprovider::KeyPadTiles)) {
        padTiles = dictionary.value<bool>(defaultprovider::KeyPadTiles);
    }

    TileTextureInitData initData(
        getTileTextureInitData(layerGroupID, padTiles, pixelSize)
    );
    tilePixelSize = initData.dimensions().x;

    performPreProcessing = shouldPerformPreProcessingOnLayerGroup(layerGroupID);
    if (dictionary.hasKeyAndValue<bool>(defaultprovider::KeyPerformPreProcessing)) {
        performPreProcessing = dictionary.value<bool>(
            defaultprovider::KeyPerformPreProcessing
        );
        LDEBUG(fmt::format(
            "Default PerformPreProcessing overridden: {}", performPreProcessing
        ));
    }

    initAsyncTileDataReader(*this, initData);

    addProperty(filePath);
    addProperty(tilePixelSize);
}





SingleImageProvider::SingleImageProvider(const ghoul::Dictionary& dictionary)
    : tile(nullptr, nullptr, Tile::Status::Unavailable)
    , filePath(singleimageprovider::FilePathInfo)
{
    type = Type::SingleImageTileProvider;

    filePath = dictionary.value<std::string>(singleimageprovider::KeyFilePath);
    addProperty(filePath);

    reset(*this);
}





TextTileProvider::TextTileProvider(const ghoul::Dictionary& dictionary,
                                   const TileTextureInitData& initData, size_t fontSize)
    : initData(initData)
    , fontSize(fontSize)
{
    tileCache = global::moduleEngine.module<GlobeBrowsingModule>()->tileCache();
}





SizeReferenceTileProvider::SizeReferenceTileProvider(const ghoul::Dictionary& dictionary)
    : TextTileProvider(
        dictionary,
        getTileTextureInitData(layergroupid::GroupID::ColorLayers, false)
    )
{
    type = Type::SizeReferenceTileProvider;

    font = global::fontManager.font("Mono", static_cast<float>(fontSize));

    if (dictionary.hasKeyAndValue<glm::dvec3>(sizereferenceprovider::KeyRadii)) {
        ellipsoid = dictionary.value<glm::dvec3>(sizereferenceprovider::KeyRadii);
    }
}





TileIndexTileProvider::TileIndexTileProvider(const ghoul::Dictionary& dictionary)
    : TextTileProvider(
        dictionary,
        getTileTextureInitData(layergroupid::GroupID::ColorLayers, false)
    )
{
    type = Type::TileIndexTileProvider;
}





TileProviderByIndex::TileProviderByIndex(const ghoul::Dictionary& dictionary) {
    type = Type::ByIndexTileProvider;

    const ghoul::Dictionary& defaultProviderDict = dictionary.value<ghoul::Dictionary>(
        byindexprovider::KeyDefaultProvider
    );

    layergroupid::TypeID type;
    if (defaultProviderDict.hasKeyAndValue<std::string>("Type")) {
        const std::string& t = defaultProviderDict.value<std::string>("Type");
        type = ghoul::from_string<layergroupid::TypeID>(t);

        if (type == layergroupid::TypeID::Unknown) {
            throw ghoul::RuntimeError("Unknown layer type: " + t);
        }
    }
    else {
        type = layergroupid::TypeID::DefaultTileLayer;
    }

    defaultTileProvider = createFromDictionary(type, defaultProviderDict);

    const ghoul::Dictionary& indexProvidersDict = dictionary.value<ghoul::Dictionary>(
        byindexprovider::KeyProviders
    );
    for (size_t i = 1; i <= indexProvidersDict.size(); i++) {
        ghoul::Dictionary indexProviderDict = indexProvidersDict.value<ghoul::Dictionary>(
            std::to_string(i)
        );
        ghoul::Dictionary tileIndexDict = indexProviderDict.value<ghoul::Dictionary>(
            byindexprovider::KeyTileIndex
        );
        ghoul::Dictionary providerDict = indexProviderDict.value<ghoul::Dictionary>(
            byindexprovider::KeyTileProvider
        );

        const TileIndex tileIndex(tileIndexDict);

        layergroupid::TypeID providerTypeID = layergroupid::TypeID::DefaultTileLayer;
        if (defaultProviderDict.hasKeyAndValue<std::string>("Type")) {
            const std::string& t = defaultProviderDict.value<std::string>("Type");
            providerTypeID = ghoul::from_string<layergroupid::TypeID>(t);

            if (providerTypeID == layergroupid::TypeID::Unknown) {
                throw ghoul::RuntimeError("Unknown layer type: " + t);
            }
        }

        std::shared_ptr<TileProvider> stp = createFromDictionary(
            providerTypeID,
            providerDict
        );
        TileIndex::TileHashKey key = tileIndex.hashKey();
        tileProviderMap.insert(std::make_pair(key, stp));
    }
}





TileProviderByLevel::TileProviderByLevel(const ghoul::Dictionary& dictionary) {
    type = Type::ByLevelTileProvider;

    layergroupid::GroupID layerGroupID = dictionary.value<layergroupid::GroupID>(
        bylevelprovider::KeyLayerGroupID
    );

    if (dictionary.hasKeyAndValue<ghoul::Dictionary>(bylevelprovider::KeyProviders)) {
        ghoul::Dictionary providers = dictionary.value<ghoul::Dictionary>(
            bylevelprovider::KeyProviders
        );

        for (size_t i = 1; i <= providers.size(); i++) {
            ghoul::Dictionary levelProviderDict = providers.value<ghoul::Dictionary>(
                std::to_string(i)
            );
            double floatMaxLevel = levelProviderDict.value<double>(
                bylevelprovider::KeyMaxLevel
            );
            int maxLevel = static_cast<int>(std::round(floatMaxLevel));

            ghoul::Dictionary providerDict = levelProviderDict.value<ghoul::Dictionary>(
                bylevelprovider::KeyTileProvider
            );
            providerDict.setValue(bylevelprovider::KeyLayerGroupID, layerGroupID);

            layergroupid::TypeID typeID;
            if (providerDict.hasKeyAndValue<std::string>("Type")) {
                const std::string& typeString = providerDict.value<std::string>("Type");
                typeID = ghoul::from_string<layergroupid::TypeID>(typeString);

                if (typeID == layergroupid::TypeID::Unknown) {
                    throw ghoul::RuntimeError("Unknown layer type: " + typeString);
                }
            }
            else {
                typeID = layergroupid::TypeID::DefaultTileLayer;
            }

            std::shared_ptr<TileProvider> tp = std::shared_ptr<TileProvider>(
                createFromDictionary(typeID, providerDict)
            );
            levelTileProviders.push_back(tp);

            std::string provId = providerDict.value<std::string>("Identifier");
            tp->setIdentifier(provId);
            std::string providerName = providerDict.value<std::string>("Name");
            tp->setGuiName(providerName);
            addPropertySubOwner(tp.get());

            // Ensure we can represent the max level
            if (static_cast<int>(providerIndices.size()) < maxLevel) {
                providerIndices.resize(maxLevel + 1, -1);
            }

            // map this level to the tile provider index
            providerIndices[maxLevel] = static_cast<int>(levelTileProviders.size()) - 1;
        }
    }

    // Fill in the gaps (value -1 ) in provider indices, from back to end
    for (int i = static_cast<int>(providerIndices.size()) - 2; i >= 0; --i) {
        if (providerIndices[i] == -1) {
            providerIndices[i] = providerIndices[i + 1];
        }
    }
}





TemporalTileProvider::TemporalTileProvider(const ghoul::Dictionary& dictionary) 
    : initDict(dictionary)
    , filePath(temporal::FilePathInfo)
{
    type = Type::TemporalTileProvider;

    filePath = dictionary.value<std::string>(temporal::KeyFilePath);
    addProperty(filePath);

    successfulInitialization = readFilePath(*this);

    if (!successfulInitialization) {
        LERRORC("TemporalTileProvider", "Unable to read file " + filePath.value());
    }
}






bool initialize(TileProvider& tp) {
    ghoul_assert(!tp.isInitialized, "TileProvider can only be initialized once.");

    initializeDefaultTile(tp);
    tp.uniqueIdentifier = tp.NumTileProviders;
    ++tp.NumTileProviders;
    if (tp.NumTileProviders == std::numeric_limits<unsigned int>::max()) {
        --tp.NumTileProviders;
        return false;
    }

    tp.isInitialized = true;

    switch (tp.type) {
        case Type::DefaultTileProvider:
            break;
        case Type::SingleImageTileProvider:
            break;
        case Type::SizeReferenceTileProvider: {
            SizeReferenceTileProvider& t = static_cast<SizeReferenceTileProvider&>(tp);
            initialize(t);
            break;
        }
        case Type::TileIndexTileProvider: {
            TileIndexTileProvider& t = static_cast<TileIndexTileProvider&>(tp);
            initialize(t);
            break;
        }
        case Type::ByIndexTileProvider:
            break;
        case Type::ByLevelTileProvider: {
            TileProviderByLevel& t = static_cast<TileProviderByLevel&>(tp);
            bool success = true;
            for (const std::shared_ptr<TileProvider>& prov : t.levelTileProviders) {
                success &= initialize(*prov);
            }
            return success;
        }
        case Type::TemporalTileProvider: 
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    return true;
}






bool deinitialize(TileProvider& tp) {
    switch (tp.type) {
        case Type::DefaultTileProvider:
            break;
        case Type::SingleImageTileProvider:
            break;
        case Type::SizeReferenceTileProvider: {
            SizeReferenceTileProvider& t = static_cast<SizeReferenceTileProvider&>(tp);
            deinitialize(t);
            break;
        }
        case Type::TileIndexTileProvider: {
            TileIndexTileProvider& t = static_cast<TileIndexTileProvider&>(tp);
            deinitialize(t);
            break;
        }
        case Type::ByIndexTileProvider:
            break;
        case Type::ByLevelTileProvider: {
            TileProviderByLevel& t = static_cast<TileProviderByLevel&>(tp);
            bool success = true;
            for (const std::shared_ptr<TileProvider>& prov : t.levelTileProviders) {
                success &= deinitialize(*prov);
            }
            return success;
        }
        case Type::TemporalTileProvider:
            break;
        default:
            throw ghoul::MissingCaseException();
    }
    return true;
}






Tile tile(TileProvider& tp, const TileIndex& tileIndex) {
    switch (tp.type) {
        case Type::DefaultTileProvider: {
            DefaultTileProvider& t = static_cast<DefaultTileProvider&>(tp);
            if (t.asyncTextureDataProvider) {
                if (tileIndex.level > maxLevel(t)) {
                    return Tile(nullptr, nullptr, Tile::Status::OutOfRange);
                }
                const cache::ProviderTileKey key = { tileIndex, t.uniqueIdentifier };
                const Tile tile = t.tileCache->get(key);

                if (!tile.texture()) {
                    t.asyncTextureDataProvider->enqueueTileIO(tileIndex);
                }

                return tile;
            }
            else {
                return Tile(nullptr, nullptr, Tile::Status::Unavailable);
            }
        }
        case Type::SingleImageTileProvider: {
            SingleImageProvider& t = static_cast<SingleImageProvider&>(tp);
            return t.tile;
        }
        case Type::SizeReferenceTileProvider: {
            SizeReferenceTileProvider& t = static_cast<SizeReferenceTileProvider&>(tp);

            const GeodeticPatch patch(tileIndex);
            const bool aboveEquator = patch.isNorthern();
            const double lat = aboveEquator ? patch.minLat() : patch.maxLat();
            const double lon1 = patch.minLon();
            const double lon2 = patch.maxLon();
            int l = static_cast<int>(t.ellipsoid.longitudalDistance(lat, lon1, lon2));

            const bool useKm = l > 9999;
            if (useKm) {
                l /= 1000;
            }
            l = static_cast<int>(std::round(l));
            if (useKm) {
                l *= 1000;
            }
            double tileLongitudalLength = l;

            const char* unit;
            if (tileLongitudalLength > 9999) {
                tileLongitudalLength *= 0.001;
                unit = "km";
            }
            else {
                unit = "m";
            }

            t.text = fmt::format(" {:.0f} {:s}", tileLongitudalLength, unit);
            t.textPosition = {
                0.f,
                aboveEquator ?
                    t.fontSize / 2.f :
                    t.initData.dimensions().y - 3.f * t.fontSize / 2.f
            };
            t.textColor = glm::vec4(1.f);

            return tile(t, tileIndex);
        }
        case Type::TileIndexTileProvider: {
            TileIndexTileProvider& t = static_cast<TileIndexTileProvider&>(tp);
            t.text = fmt::format(
                "level: {}\nx: {}\ny: {}", tileIndex.level, tileIndex.x, tileIndex.y
            );
            t.textPosition = glm::vec2(
                t.initData.dimensions().x / 4 -
                (t.initData.dimensions().x / 32) * log10(1 << tileIndex.level),
                t.initData.dimensions().y / 2 + t.fontSize
            );
            t.textColor = glm::vec4(1.f);

            return tile(t, tileIndex);
        }
        case Type::ByIndexTileProvider: {
            TileProviderByIndex& t = static_cast<TileProviderByIndex&>(tp);
            const auto it = t.tileProviderMap.find(tileIndex.hashKey());
            const bool hasProvider = it != t.tileProviderMap.end();
            return hasProvider ? tile(*it->second, tileIndex) : Tile::TileUnavailable;
        }
        case Type::ByLevelTileProvider: {
            TileProviderByLevel& t = static_cast<TileProviderByLevel&>(tp);
            TileProvider* provider = levelProvider(t, tileIndex.level);
            if (provider) {
                return tile(*provider, tileIndex);
            }
            else {
                return Tile::TileUnavailable;
            }
        }
        case Type::TemporalTileProvider: {
            TemporalTileProvider& t = static_cast<TemporalTileProvider&>(tp);
            if (t.successfulInitialization) {
                ensureUpdated(t);
                return tile(*t.currentTileProvider, tileIndex);
            }
            else {
                return Tile::TileUnavailable;
            }
        }
        default:
            throw ghoul::MissingCaseException();
    }
}






ChunkTile chunkTile(TileProvider& tp, TileIndex tileIndex, int parents, int maxParents) {
    ghoul_assert(tp.isInitialized, "TileProvider was not initialized.");
    TileUvTransform uvTransform = { glm::vec2(0.f, 0.f), glm::vec2(1.f, 1.f) };

    // Step 1. Traverse 0 or more parents up the chunkTree as requested by the caller
    for (int i = 0; i < parents && tileIndex.level > 1; i++) {
        tileselector::ascendToParent(tileIndex, uvTransform);
    }
    maxParents -= parents;

    // Step 2. Traverse 0 or more parents up the chunkTree to make sure we're inside
    //         the range of defined data.
    int maximumLevel = maxLevel(tp);
    while (tileIndex.level > maximumLevel) {
        tileselector::ascendToParent(tileIndex, uvTransform);
        maxParents--;
    }
    if (maxParents < 0) {
        return ChunkTile { Tile::TileUnavailable, uvTransform, TileDepthTransform() };
    }

    // Step 3. Traverse 0 or more parents up the chunkTree until we find a chunk that
    //         has a loaded tile ready to use.
    while (tileIndex.level > 1) {
        Tile t = tile(tp, tileIndex);
        if (t.status() != Tile::Status::OK) {
            if (--maxParents < 0) {
                return ChunkTile {
                    Tile::TileUnavailable,
                    uvTransform,
                    TileDepthTransform()
                };
            }
            tileselector::ascendToParent(tileIndex, uvTransform);
        }
        else {
            return ChunkTile { std::move(t), uvTransform, TileDepthTransform() };
        }
    }

    return ChunkTile { Tile::TileUnavailable, uvTransform, TileDepthTransform() };
}






ChunkTilePile chunkTilePile(TileProvider& tp, TileIndex tileIndex, int pileSize) {
    ghoul_assert(tp.isInitialized, "TileProvider was not initialized.");
    ghoul_assert(pileSize >= 0, "pileSize must be positive");

    ChunkTilePile chunkTilePile(pileSize);
    for (int i = 0; i < pileSize; ++i) {
        chunkTilePile[i] = chunkTile(tp, tileIndex, i);
        if (chunkTilePile[i].tile.status() == Tile::Status::Unavailable) {
            if (i > 0) {
                // First iteration
                chunkTilePile[i].tile = chunkTilePile[i - 1].tile;
                chunkTilePile[i].uvTransform.uvOffset =
                    chunkTilePile[i - 1].uvTransform.uvOffset;
                chunkTilePile[i].uvTransform.uvScale =
                    chunkTilePile[i - 1].uvTransform.uvScale;
            }
            else {
                chunkTilePile[i].tile = tp.defaultTile;
                chunkTilePile[i].uvTransform.uvOffset = { 0.f, 0.f };
                chunkTilePile[i].uvTransform.uvScale = { 1.f, 1.f };
            }
        }
    }
    return chunkTilePile;
}






Tile::Status tileStatus(TileProvider& tp, const TileIndex& index) {
    switch (tp.type) {
        case Type::DefaultTileProvider: {
            DefaultTileProvider& t = static_cast<DefaultTileProvider&>(tp);
            if (t.asyncTextureDataProvider) {
                const std::shared_ptr<RawTileDataReader>& rawTileDataReader =
                    t.asyncTextureDataProvider->rawTileDataReader();

                if (index.level > rawTileDataReader->maxChunkLevel()) {
                    return Tile::Status::OutOfRange;
                }

                const cache::ProviderTileKey key = { index, t.uniqueIdentifier };
                return t.tileCache->get(key).status();
            }
            else {
                return Tile::Status::Unavailable;
            }
        }
        case Type::SingleImageTileProvider: {
            SingleImageProvider& t = static_cast<SingleImageProvider&>(tp);
            return t.tile.status();
        }
        case Type::SizeReferenceTileProvider:
            return Tile::Status::OK;
        case Type::TileIndexTileProvider:
            return Tile::Status::OK;
        case Type::ByIndexTileProvider: {
            TileProviderByIndex& t = static_cast<TileProviderByIndex&>(tp);
            const auto it = t.tileProviderMap.find(index.hashKey());
            const bool hasProvider = it != t.tileProviderMap.end();
            return hasProvider ?
                tileStatus(*it->second, index) :
                Tile::Status::Unavailable;
        }
        case Type::ByLevelTileProvider: {
            TileProviderByLevel& t = static_cast<TileProviderByLevel&>(tp);
            TileProvider* provider = levelProvider(t, index.level);
            return provider ? tileStatus(*provider, index) : Tile::Status::Unavailable;
        }
        case Type::TemporalTileProvider: {
            TemporalTileProvider& t = static_cast<TemporalTileProvider&>(tp);
            if (t.successfulInitialization) {
                ensureUpdated(t);
                return tileStatus(*t.currentTileProvider, index);
            }
            else {
                return Tile::Status::Unavailable;
            }
        }
        default:
            throw ghoul::MissingCaseException();
    }
}






TileDepthTransform depthTransform(TileProvider& tp) {
    switch (tp.type) {
        case Type::DefaultTileProvider: {
            DefaultTileProvider& t = static_cast<DefaultTileProvider&>(tp);
            if (t.asyncTextureDataProvider) {
                return t.asyncTextureDataProvider->rawTileDataReader()->depthTransform();
            }
            else {
                return { 1.f, 0.f };
            }
        }
        case Type::SingleImageTileProvider:
            return { 0.f, 1.f };
        case Type::SizeReferenceTileProvider:
            return { 0.f, 1.f };
        case Type::TileIndexTileProvider:
            return { 0.f, 1.f };
        case Type::ByIndexTileProvider: {
            TileProviderByIndex& t = static_cast<TileProviderByIndex&>(tp);
            return depthTransform(*t.defaultTileProvider);
        }
        case Type::ByLevelTileProvider:
            return { 0.f, 1.f };
        case Type::TemporalTileProvider: {
            TemporalTileProvider& t = static_cast<TemporalTileProvider&>(tp);
            if (t.successfulInitialization) {
                ensureUpdated(t);
                return depthTransform(*t.currentTileProvider);
            }
            else {
                return { 1.f, 0.f };
            }
        }
        default:
            throw ghoul::MissingCaseException();
    }
}






void update(TileProvider& tp) {
    switch (tp.type) {
        case Type::DefaultTileProvider: {
            DefaultTileProvider& t = static_cast<DefaultTileProvider&>(tp);
            if (!t.asyncTextureDataProvider) {
                break;
            }

            t.asyncTextureDataProvider->update();
            initTexturesFromLoadedData(t);

            if (t.asyncTextureDataProvider->shouldBeDeleted()) {
                t.asyncTextureDataProvider = nullptr;
                TileTextureInitData initData(
                    getTileTextureInitData(t.layerGroupID, t.padTiles, t.tilePixelSize)
                );
                initAsyncTileDataReader(t, initData);
            }
            break;
        }
        case Type::SingleImageTileProvider:
            break;
        case Type::SizeReferenceTileProvider:
            break;
        case Type::TileIndexTileProvider:
            break;
        case Type::ByIndexTileProvider: {
            TileProviderByIndex& t = static_cast<TileProviderByIndex&>(tp);
            using K = TileIndex::TileHashKey;
            using V = std::shared_ptr<TileProvider>;
            for (std::pair<const K, V>& it : t.tileProviderMap) {
                update(*it.second);
            }
            update(*t.defaultTileProvider);
            break;
        }
        case Type::ByLevelTileProvider: {
            TileProviderByLevel& t = static_cast<TileProviderByLevel&>(tp);
            for (const std::shared_ptr<TileProvider>& provider : t.levelTileProviders) {
                update(*provider);
            }
            break;
        }
        case Type::TemporalTileProvider: {
            TemporalTileProvider& t = static_cast<TemporalTileProvider&>(tp);
            if (t.successfulInitialization) {
                std::shared_ptr<TileProvider> newCurrent = getTileProvider(t,
                    global::timeManager.time()
                );
                if (newCurrent) {
                    t.currentTileProvider = newCurrent;
                }
                update(*t.currentTileProvider);
            }
            break;
        }
        default:
            throw ghoul::MissingCaseException();
    }
}






void reset(TileProvider& tp) {
    switch (tp.type) {
        case Type::DefaultTileProvider: {
            DefaultTileProvider& t = static_cast<DefaultTileProvider&>(tp);
            t.tileCache->clear();
            if (t.asyncTextureDataProvider) {
                t.asyncTextureDataProvider->prepairToBeDeleted();
            }
            else {
                TileTextureInitData initData(
                    getTileTextureInitData(t.layerGroupID, t.padTiles, t.tilePixelSize)
                );
                initAsyncTileDataReader(t, initData);
            }
        }
        case Type::SingleImageTileProvider: {
            SingleImageProvider& t = static_cast<SingleImageProvider&>(tp);

            if (t.filePath.value().empty()) {
                return;
            }
            t.tileTexture = ghoul::io::TextureReader::ref().loadTexture(t.filePath);
            if (!t.tileTexture) {
                throw ghoul::RuntimeError(
                    fmt::format("Unable to load texture '{}'", t.filePath.value())
                );
            }
            Tile::Status tileStatus = Tile::Status::OK;

            t.tileTexture->uploadTexture();
            t.tileTexture->setFilter(
                ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
            );

            t.tile = Tile(t.tileTexture.get(), nullptr, tileStatus);
        }
        case Type::SizeReferenceTileProvider: {
            SizeReferenceTileProvider& t = static_cast<SizeReferenceTileProvider&>(tp);
            reset(t);
            break;
        }
        case Type::TileIndexTileProvider: {
            TileIndexTileProvider& t = static_cast<TileIndexTileProvider&>(tp);
            reset(t);
            break;
        }
        case Type::ByIndexTileProvider: {
            TileProviderByIndex& t = static_cast<TileProviderByIndex&>(tp);
            using K = TileIndex::TileHashKey;
            using V = std::shared_ptr<TileProvider>;
            for (std::pair<const K, V>& it : t.tileProviderMap) {
                reset(*it.second);
            }
            reset(*t.defaultTileProvider);
            break;
        }
        case Type::ByLevelTileProvider: {
            TileProviderByLevel& t = static_cast<TileProviderByLevel&>(tp);
            for (const std::shared_ptr<TileProvider>& provider : t.levelTileProviders) {
                reset(*provider);
            }
            break;
        }
        case Type::TemporalTileProvider: {
            TemporalTileProvider& t = static_cast<TemporalTileProvider&>(tp);
            if (t.successfulInitialization) {
                using K = TemporalTileProvider::TimeKey;
                using V = std::shared_ptr<TileProvider>;
                for (std::pair<const K, V>& it : t.tileProviderMap) {
                    reset(*it.second);
                }
            }
            break;
        }
        default:
            throw ghoul::MissingCaseException();
    }
}






int maxLevel(TileProvider& tp) {
    switch (tp.type) {
        case Type::DefaultTileProvider: {
            DefaultTileProvider& t = static_cast<DefaultTileProvider&>(tp);
            // 22 is the current theoretical maximum based on the number of hashes that
            // are possible to uniquely identify a tile. See ProviderTileHasher in
            // memoryawaretilecache.h
            return t.asyncTextureDataProvider ?
                t.asyncTextureDataProvider->rawTileDataReader()->maxChunkLevel() :
                22;
                return 22;
        }
        case Type::SingleImageTileProvider:
            return 1337; // unlimited
        case Type::SizeReferenceTileProvider:
            return 1337; // unlimited
        case Type::TileIndexTileProvider:
            return 1337; // unlimited
        case Type::ByIndexTileProvider: {
            TileProviderByIndex& t = static_cast<TileProviderByIndex&>(tp);
            return maxLevel(*t.defaultTileProvider);
        }
        case Type::ByLevelTileProvider: {
            TileProviderByLevel& t = static_cast<TileProviderByLevel&>(tp);
            return static_cast<int>(t.providerIndices.size() - 1);
        }
        case Type::TemporalTileProvider: {
            TemporalTileProvider& t = static_cast<TemporalTileProvider&>(tp);
            if (t.successfulInitialization) {
                ensureUpdated(t);
                return maxLevel(*t.currentTileProvider);
            }
            else {
                return 0;
            }
        }
        default:
            throw ghoul::MissingCaseException();
    }
}






float noDataValueAsFloat(TileProvider& tp) {
    ghoul_assert(tp.isInitialized, "TileProvider was not initialized.");
    switch (tp.type) {
        case Type::DefaultTileProvider: {
            DefaultTileProvider& t = static_cast<DefaultTileProvider&>(tp);
            return t.asyncTextureDataProvider ?
                t.asyncTextureDataProvider->noDataValueAsFloat() :
                std::numeric_limits<float>::min();
        }
        case Type::SingleImageTileProvider:
            return std::numeric_limits<float>::min();
        case Type::SizeReferenceTileProvider:
            return std::numeric_limits<float>::min();
        case Type::TileIndexTileProvider:
            return std::numeric_limits<float>::min();
        case Type::ByIndexTileProvider:
            return std::numeric_limits<float>::min();
        case Type::ByLevelTileProvider:
            return std::numeric_limits<float>::min();
        case Type::TemporalTileProvider:
            return std::numeric_limits<float>::min();
        default:
            throw ghoul::MissingCaseException();
    }
}






void initializeDefaultTile(TileProvider& tp) {
    ghoul_assert(!tp.defaultTile.texture(), "Default tile should not have been created");
    using namespace ghoul::opengl;

    // Create pixel data
    TileTextureInitData initData(
        8,
        8,
        GL_UNSIGNED_BYTE,
        Texture::Format::RGBA,
        TileTextureInitData::PadTiles::No,
        TileTextureInitData::ShouldAllocateDataOnCPU::Yes
    );
    const size_t numBytes = initData.totalNumBytes();
    char* pixels = new char[numBytes];
    memset(pixels, 0, numBytes * sizeof(char));

    // Create ghoul texture
    tp.defaultTileTexture = std::make_unique<Texture>(initData.dimensions());
    tp.defaultTileTexture->setDataOwnership(Texture::TakeOwnership::Yes);
    tp.defaultTileTexture->setPixelData(pixels);
    tp.defaultTileTexture->uploadTexture();
    tp.defaultTileTexture->setFilter(ghoul::opengl::Texture::FilterMode::LinearMipMap);

    // Create tile
    tp.defaultTile = Tile(tp.defaultTileTexture.get(), nullptr, Tile::Status::OK);
}

} // namespace openspace::globebrowsing::tileprovider
