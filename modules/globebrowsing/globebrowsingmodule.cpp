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

#include <modules/globebrowsing/globebrowsingmodule.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/dashboarditemglobelocation.h>
#include <modules/globebrowsing/src/gdalwrapper.h>
#include <modules/globebrowsing/src/geodeticpatch.h>
#include <modules/globebrowsing/src/geojson/geojsoncomponent.h>
#include <modules/globebrowsing/src/geojson/geojsonmanager.h>
#include <modules/globebrowsing/src/geojson/geojsonproperties.h>
#include <modules/globebrowsing/src/globelabelscomponent.h>
#include <modules/globebrowsing/src/layer.h>
#include <modules/globebrowsing/src/layeradjustment.h>
#include <modules/globebrowsing/src/layergroup.h>
#include <modules/globebrowsing/src/layermanager.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <modules/globebrowsing/src/tileprovider/defaulttileprovider.h>
#include <modules/globebrowsing/src/tileprovider/imagesequencetileprovider.h>
#include <modules/globebrowsing/src/tileprovider/singleimagetileprovider.h>
#include <modules/globebrowsing/src/tileprovider/sizereferencetileprovider.h>
#include <modules/globebrowsing/src/tileprovider/spoutimageprovider.h>
#include <modules/globebrowsing/src/tileprovider/temporaltileprovider.h>
#include <modules/globebrowsing/src/tileprovider/tileindextileprovider.h>
#include <modules/globebrowsing/src/tileprovider/tileprovider.h>
#include <modules/globebrowsing/src/tileprovider/tileproviderbydate.h>
#include <modules/globebrowsing/src/tileprovider/tileproviderbyindex.h>
#include <modules/globebrowsing/src/tileprovider/tileproviderbylevel.h>
#include <openspace/camera/camera.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/navigationstate.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/geodetic.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <vector>

#include <gdal.h>

#ifdef _MSC_VER
#pragma warning (push)
// CPL throws warning about missing DLL interface
#pragma warning (disable : 4251)
#endif // _MSC_VER

#include <cpl_conv.h>
#include <cpl_string.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#include "globebrowsingmodule_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "GlobeBrowsingModule";

    constexpr openspace::properties::Property::PropertyInfo TileCacheSizeInfo = {
        "TileCacheSize",
        "Tile Cache Size",
        "The maximum size of the MemoryAwareTileCache, on the CPU and GPU.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DefaultGeoPointTextureInfo = {
        "DefaultGeoPointTexture",
        "Default Geo Point Texture",
        "A path to a texture to use as default for GeoJson points."
    };

    constexpr openspace::properties::Property::PropertyInfo MRFCacheEnabledInfo = {
        "MRFCacheEnabled",
        "MRF Cache Enabled",
        "Determines whether automatic caching of globe browsing data is enabled.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MRFCacheLocationInfo = {
        "MRFCacheLocation",
        "MRF Cache Location",
        "The location of the root folder for the MRF cache of globe browsing data.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    openspace::GlobeBrowsingModule::Capabilities
    parseSubDatasets(char** subDatasets, int nSubdatasets)
    {
        // Idea: Iterate over the list of sublayers keeping a current layer and identify
        //       it by its number.  If this number changes, we know that we have a new
        //       layer

        using Layer = openspace::GlobeBrowsingModule::Layer;
        std::vector<Layer> result;

        int currentLayerNumber = -1;
        Layer currentLayer;
        for (int i = 0; i < nSubdatasets; i++) {
            int iDataset = -1;
            std::array<char, 256> IdentifierBuffer;
            std::fill(IdentifierBuffer.begin(), IdentifierBuffer.end(), '\0');
            const int ret = sscanf(
                subDatasets[i],
                "SUBDATASET_%i_%255[^=]",
                &iDataset,
                IdentifierBuffer.data()
            );
            if (ret != 2) {
                LERROR("Error parsing dataset");
                continue;
            }


            if (iDataset != currentLayerNumber) {
                // We are done with the previous version
                result.push_back(std::move(currentLayer));
                currentLayer = Layer();
                currentLayerNumber = iDataset;
            }

            const std::string identifier = std::string(IdentifierBuffer.data());
            const std::string ds(subDatasets[i]);
            const std::string value = ds.substr(ds.find_first_of('=') + 1);

            // The DESC/NAME difference is not a typo
            if (identifier == "DESC") {
                currentLayer.name = value;
            }
            else if (identifier == "NAME") {
                currentLayer.url = value;
            }
            else {
                LINFOC(
                    "GlobeBrowsingGUI", "Unknown subdataset identifier: " + identifier
                );
            }
        }

        return result;
    }

    struct [[codegen::Dictionary(GlobeBrowsingModule)]] Parameters {
        // [[codegen::verbatim(TileCacheSizeInfo.description)]]
        std::optional<int> tileCacheSize;

        // [[codegen::verbatim(DefaultGeoPointTextureInfo.description)]]
        std::optional<std::string> defaultGeoPointTexture;

        // [[codegen::verbatim(MRFCacheEnabledInfo.description)]]
        std::optional<bool> mrfCacheEnabled [[codegen::key("MRFCacheEnabled")]];

        // [[codegen::verbatim(MRFCacheLocationInfo.description)]]
        std::optional<std::string> mrfCacheLocation [[codegen::key("MRFCacheLocation")]];
    };
#include "globebrowsingmodule_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation GlobeBrowsingModule::Documentation() {
    return codegen::doc<Parameters>("module_globebrowsing");
}

GlobeBrowsingModule::GlobeBrowsingModule()
    : OpenSpaceModule(Name)
    , _tileCacheSizeMB(TileCacheSizeInfo, 1024)
    , _defaultGeoPointTexturePath(DefaultGeoPointTextureInfo)
    , _mrfCacheEnabled(MRFCacheEnabledInfo, false)
    , _mrfCacheLocation(MRFCacheLocationInfo, "${BASE}/cache_mrf")
{
    addProperty(_tileCacheSizeMB);

    addProperty(_defaultGeoPointTexturePath);

    addProperty(_mrfCacheEnabled);
    addProperty(_mrfCacheLocation);
}

void GlobeBrowsingModule::internalInitialize(const ghoul::Dictionary& dict) {
    using namespace globebrowsing;

    const Parameters p = codegen::bake<Parameters>(dict);
    _tileCacheSizeMB = p.tileCacheSize.value_or(_tileCacheSizeMB);

    _defaultGeoPointTexturePath.onChange([this]() {
        if (_defaultGeoPointTexturePath.value().empty()) {
            _hasDefaultGeoPointTexture = false;
            return;
        }
        std::filesystem::path path = _defaultGeoPointTexturePath.value();
        if (std::filesystem::exists(path)) {
            _hasDefaultGeoPointTexture = true;
        }
        else {
            LWARNINGC(
                "GlobeBrowsingModule",
                std::format(
                    "The provided texture file '{}' for the default geo point texture "
                    "does not exist", path
                )
            );
        }
    });

    if (p.defaultGeoPointTexture.has_value()) {
        _defaultGeoPointTexturePath = absPath(*p.defaultGeoPointTexture).string();
    }

    _mrfCacheEnabled = p.mrfCacheEnabled.value_or(_mrfCacheEnabled);
    _mrfCacheLocation = p.mrfCacheLocation.value_or(_mrfCacheLocation);

    // Initialize
    global::callback::initializeGL->emplace_back([this]() {
        ZoneScopedN("GlobeBrowsingModule");

        _tileCache = std::make_unique<cache::MemoryAwareTileCache>(_tileCacheSizeMB);
        addPropertySubOwner(_tileCache.get());

        TileProvider::initializeDefaultTile();

        // Convert from MB to Bytes
        GdalWrapper::create(
            512ULL * 1024ULL * 1024ULL, // 512 MB
            static_cast<size_t>(CpuCap.installedMainMemory() * 0.25 * 1024 * 1024)
        );
        addPropertySubOwner(GdalWrapper::ref());
    });

    global::callback::deinitializeGL->emplace_back([]() {
        ZoneScopedN("GlobeBrowsingModule");

        TileProvider::deinitializeDefaultTile();
    });

    // Render
    global::callback::render->emplace_back([this]() {
        ZoneScopedN("GlobeBrowsingModule");

        _tileCache->update();
    });

    // Deinitialize
    global::callback::deinitialize->emplace_back([]() {
        ZoneScopedN("GlobeBrowsingModule");

        GdalWrapper::destroy();
    });

    ghoul::TemplateFactory<Renderable>* fRenderable =
        FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");
    fRenderable->registerClass<globebrowsing::RenderableGlobe>("RenderableGlobe");

    FactoryManager::ref().addFactory<TileProvider>("TileProvider");

    ghoul::TemplateFactory<TileProvider>* fTileProvider =
        FactoryManager::ref().factory<TileProvider>();
    ghoul_assert(fTileProvider, "TileProvider factory was not created");


    fTileProvider->registerClass<DefaultTileProvider>("DefaultTileProvider");
    fTileProvider->registerClass<SingleImageProvider>("SingleImageProvider");
    fTileProvider->registerClass<ImageSequenceTileProvider>("ImageSequenceTileProvider");
    fTileProvider->registerClass<SpoutImageProvider>("SpoutImageProvider");
    fTileProvider->registerClass<TemporalTileProvider>("TemporalTileProvider");
    fTileProvider->registerClass<TileIndexTileProvider>("TileIndexTileProvider");
    fTileProvider->registerClass<SizeReferenceTileProvider>("SizeReferenceTileProvider");
    fTileProvider->registerClass<TileProviderByDate>("TileProviderByDate");
    fTileProvider->registerClass<TileProviderByLevel>("TileProviderByLevel");
    fTileProvider->registerClass<TileProviderByIndex>("TileProviderByIndex");

    ghoul::TemplateFactory<DashboardItem>* fDashboard =
        FactoryManager::ref().factory<DashboardItem>();
    ghoul_assert(fDashboard, "Dashboard factory was not created");

    fDashboard->registerClass<DashboardItemGlobeLocation>("DashboardItemGlobeLocation");
}

globebrowsing::cache::MemoryAwareTileCache* GlobeBrowsingModule::tileCache() {
    return _tileCache.get();
}

std::vector<documentation::Documentation> GlobeBrowsingModule::documentations() const {
    return {
        globebrowsing::Layer::Documentation(),
        globebrowsing::LayerAdjustment::Documentation(),
        globebrowsing::RenderableGlobe::Documentation(),
        globebrowsing::DefaultTileProvider::Documentation(),
        globebrowsing::ImageSequenceTileProvider::Documentation(),
        globebrowsing::SingleImageProvider::Documentation(),
        globebrowsing::SizeReferenceTileProvider::Documentation(),
        globebrowsing::TemporalTileProvider::Documentation(),
        globebrowsing::TileIndexTileProvider::Documentation(),
        globebrowsing::TileProviderByDate::Documentation(),
        globebrowsing::TileProviderByIndex::Documentation(),
        globebrowsing::TileProviderByLevel::Documentation(),
        globebrowsing::GeoJsonComponent::Documentation(),
        globebrowsing::GeoJsonProperties::Documentation(),
        GlobeLabelsComponent::Documentation(),
        RingsComponent::Documentation(),
        ShadowComponent::Documentation(),
        DashboardItemGlobeLocation::Documentation()
    };
}

void GlobeBrowsingModule::goToChunk(const SceneGraphNode& node,
                                    int x, int y, int level)
{
    ghoul_assert(level < std::numeric_limits<uint8_t>::max(), "Level way too big");
    using namespace openspace;
    using namespace globebrowsing;

    const GeodeticPatch patch = GeodeticPatch(
        globebrowsing::TileIndex(x, y, static_cast<uint8_t>(level))
    );
    const Geodetic2 corner = patch.corner(SOUTH_WEST);
    Geodetic2 positionOnPatch = patch.size();
    positionOnPatch.lat *= 0.5f;
    positionOnPatch.lon *= 0.5f;
    const Geodetic2 pointPosition = {
        .lat = corner.lat + positionOnPatch.lat,
        .lon = corner.lon + positionOnPatch.lon
    };

    const double altitude = altitudeFromCamera(node);

    goToGeodetic3(node, { pointPosition, altitude });
}

const globebrowsing::RenderableGlobe*
GlobeBrowsingModule::castFocusNodeRenderableToGlobe()
{
    using namespace globebrowsing;

    const Renderable* renderable =
        global::navigationHandler->orbitalNavigator().anchorNode()->renderable();

    if (!renderable) {
        return nullptr;
    }
    using namespace globebrowsing;
    if (const RenderableGlobe* globe = dynamic_cast<const RenderableGlobe*>(renderable)) {
        return globe;
    }
    else {
        return nullptr;
    }
}

void GlobeBrowsingModule::loadWMSCapabilities(std::string name, std::string globe,
                                              std::string url)
{
    auto downloadFunction = [](const std::string& downloadUrl) {
        LDEBUG("Opening WMS capabilities: " + downloadUrl);
        CPLSetConfigOption("GDAL_HTTP_TIMEOUT", "15"); // 3 seconds
        GDALDatasetH dataset = GDALOpen(
            downloadUrl.c_str(),
            GA_ReadOnly
        );
        CPLSetConfigOption("GDAL_HTTP_TIMEOUT", "3"); // 3 seconds

        if (!dataset) {
            LWARNING("Could not open dataset: " + downloadUrl);
            return Capabilities();
        }
        char** subDatasets = GDALGetMetadata(dataset, "SUBDATASETS");
        const int nSubdatasets = CSLCount(subDatasets);
        Capabilities cap = parseSubDatasets(subDatasets, nSubdatasets);
        GDALClose(dataset);
        LDEBUG("Finished WMS capabilities: " + downloadUrl);
        return cap;
    };

    _inFlightCapabilitiesMap[name] = std::async(
        std::launch::async,
        downloadFunction,
        url
    );

    //_capabilitiesMap[name] = downloadFunction(url);

    _urlList.emplace(std::move(globe), UrlInfo{ std::move(name), std::move(url) });
}

GlobeBrowsingModule::Capabilities
GlobeBrowsingModule::capabilities(const std::string& name)
{
    // First check the ones that have already finished
    const auto it = _capabilitiesMap.find(name);
    if (it != _capabilitiesMap.end()) {
        return it->second;
    }
    else {
        const auto inFlightIt = _inFlightCapabilitiesMap.find(name);
        if (inFlightIt != _inFlightCapabilitiesMap.end()) {
            // If the download and the parsing has not finished yet, this will block,
            // otherwise it will just return
            const Capabilities cap = inFlightIt->second.get();
            _capabilitiesMap[name] = cap;
            _inFlightCapabilitiesMap.erase(inFlightIt);
            return cap;
        }
        else {
            return {};
        }
    }
}

void GlobeBrowsingModule::removeWMSServer(const std::string& name) {
    // First delete all the capabilities that are currently in flight
    const auto inFlightIt = _inFlightCapabilitiesMap.find(name);
    if (inFlightIt != _inFlightCapabilitiesMap.end()) {
        _inFlightCapabilitiesMap.erase(inFlightIt);
    }

    // Then download the ones that are already finished
    const auto capIt = _capabilitiesMap.find(name);
    if (capIt != _capabilitiesMap.end()) {
        _capabilitiesMap.erase(capIt);
    }

    // Then remove the calues from the globe server list
    for (auto it = _urlList.begin(); it != _urlList.end();) {
        // We have to increment first because the erase will invalidate the iterator
        const auto eraseIt = it++;

        if (eraseIt->second.name == name) {
            _urlList.erase(eraseIt);
        }
    }
}

std::vector<GlobeBrowsingModule::UrlInfo>
GlobeBrowsingModule::urlInfo(const std::string& globe) const
{
    const auto range = _urlList.equal_range(globe);
    std::vector<UrlInfo> res;
    for (auto i = range.first; i != range.second; i++) {
        res.emplace_back(i->second);
    }
    return res;
}

bool GlobeBrowsingModule::hasUrlInfo(const std::string& globe) const {
    return _urlList.find(globe) != _urlList.end();
}

bool GlobeBrowsingModule::isMRFCachingEnabled() const {
    return _mrfCacheEnabled;
}

std::string GlobeBrowsingModule::mrfCacheLocation() const {
    return _mrfCacheLocation;
}

bool GlobeBrowsingModule::hasDefaultGeoPointTexture() const {
    return _hasDefaultGeoPointTexture;
}

std::filesystem::path GlobeBrowsingModule::defaultGeoPointTexture() const {
    return _defaultGeoPointTexturePath.value();
}

scripting::LuaLibrary GlobeBrowsingModule::luaLibrary() const {
    return {
        .name = "globebrowsing",
        .functions = {
            codegen::lua::AddLayer,
            codegen::lua::DeleteLayer,
            codegen::lua::Layers,
            codegen::lua::MoveLayer,
            codegen::lua::GoToChunk,
            codegen::lua::GeoPositionForCamera,
            codegen::lua::LoadWMSCapabilities,
            codegen::lua::RemoveWMSServer,
            codegen::lua::CapabilitiesWMS,
            codegen::lua::AddGeoJson,
            codegen::lua::DeleteGeoJson,
            codegen::lua::AddGeoJsonFromFile
        },
        .scripts = {
            absPath("${MODULE_GLOBEBROWSING}/scripts/layer_support.lua"),
            absPath("${MODULE_GLOBEBROWSING}/scripts/node_support.lua")
        }
    };
}

} // namespace openspace
