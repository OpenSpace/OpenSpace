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

#include <modules/globebrowsing/globebrowsingmodule.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/dashboarditemglobelocation.h>
#include <modules/globebrowsing/src/gdalwrapper.h>
#include <modules/globebrowsing/src/geodeticpatch.h>
#include <modules/globebrowsing/src/globetranslation.h>
#include <modules/globebrowsing/src/memoryawaretilecache.h>
#include <modules/globebrowsing/src/tileprovider.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/misc/assert.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>
#include <vector>

#include <gdal.h>

#ifdef _MSC_VER
#pragma warning (push)
// CPL throws warning about missing DLL interface
#pragma warning (disable : 4251)
#endif // _MSC_VER

#include <cpl_string.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif // _MSC_VER

#include "globebrowsingmodule_lua.inl"

namespace {
    constexpr const char* _loggerCat = "GlobeBrowsingModule";
    constexpr const char* _factoryName = "TileProvider";

    constexpr const openspace::properties::Property::PropertyInfo CacheEnabledInfo = {
        "CacheEnabled",
        "Cache Enabled",
        "Determines whether automatic caching of WMS servers is enabled. Changing the "
        "value of this property will not affect already created WMS datasets."
    };

    constexpr const openspace::properties::Property::PropertyInfo OfflineModeInfo = {
        "OfflineMode",
        "Offline Mode",
        "Determines whether loaded WMS servers should be used in offline mode, that is "
        "not even try to retrieve images through an internet connection. Please note "
        "that this setting is only reasonable, if the caching is enabled and there is "
        "available cached data. Changing the value of this property will not affect "
        "already created WMS datasets."
    };

    constexpr const openspace::properties::Property::PropertyInfo CacheLocationInfo = {
        "CacheLocation",
        "Cache Location",
        "The location of the cache folder for WMS servers. Changing the value of this "
        "property will not affect already created WMS datasets."
    };

    constexpr const openspace::properties::Property::PropertyInfo CacheSizeInfo = {
        "CacheSize",
        "Cache Size",
        "The maximum size of the cache for each WMS server. Changing the value of this "
        "property will not affect already created WMS datasets."
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
        for (int i = 0; i < nSubdatasets; ++i) {
            int iDataset = -1;
            static char IdentifierBuffer[64];
            sscanf(
                subDatasets[i],
                "SUBDATASET_%i_%[^=]",
                &iDataset,
                IdentifierBuffer
            );


            if (iDataset != currentLayerNumber) {
                // We are done with the previous version
                result.push_back(std::move(currentLayer));
                currentLayer = Layer();
                currentLayerNumber = iDataset;
            }

            const std::string identifier = std::string(IdentifierBuffer);
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
} // namespace

namespace openspace {

GlobeBrowsingModule::GlobeBrowsingModule()
    : OpenSpaceModule(Name)
    , _cacheEnabled(CacheEnabledInfo, false)
    , _offlineMode(OfflineModeInfo, false)
    , _cacheLocation(CacheLocationInfo, "${BASE}/cache_gdal")
    , _cacheSizeMB(CacheSizeInfo, 1024)
{
    addProperty(_cacheEnabled);
    addProperty(_offlineMode);
    addProperty(_cacheLocation);
    addProperty(_cacheSizeMB);
}

void GlobeBrowsingModule::internalInitialize(const ghoul::Dictionary& dict) {
    using namespace globebrowsing;

    if (dict.hasKeyAndValue<bool>(CacheEnabledInfo.identifier)) {
        _cacheEnabled = dict.value<bool>(CacheEnabledInfo.identifier);
    }
    if (dict.hasKeyAndValue<bool>(OfflineModeInfo.identifier)) {
        _offlineMode = dict.value<bool>(OfflineModeInfo.identifier);
    }
    if (dict.hasKeyAndValue<std::string>(CacheLocationInfo.identifier)) {
        _cacheLocation = dict.value<std::string>(CacheLocationInfo.identifier);
    }
    if (dict.hasKeyAndValue<double>(CacheSizeInfo.identifier)) {
        _cacheSizeMB = static_cast<int>(dict.value<double>(CacheSizeInfo.identifier));
    }

    // Sanity check
    const bool noWarning = dict.hasKeyAndValue<bool>("NoWarning") ?
        dict.value<bool>("NoWarning") :
        false;

    if (!_cacheEnabled && _offlineMode && !noWarning) {
        LWARNINGC(
            "GlobeBrowsingModule",
            "WMS caching is disabled, but offline mode is enabled. Unless you know "
            "what you are doing, this will probably cause many servers to stop working. "
            "If you want to silence this warning, set the 'NoWarning' parameter to "
            "'true'."
        );
    }


    // Initialize
    global::callback::initializeGL.emplace_back([&]() {
        _tileCache = std::make_unique<globebrowsing::cache::MemoryAwareTileCache>();
        addPropertySubOwner(*_tileCache);

        tileprovider::initializeDefaultTile();

        // Convert from MB to Bytes
        GdalWrapper::create(
            512ULL * 1024ULL * 1024ULL, // 512 MB
            static_cast<size_t>(CpuCap.installedMainMemory() * 0.25 * 1024 * 1024)
        );
        addPropertySubOwner(GdalWrapper::ref());
    });

    global::callback::deinitializeGL.emplace_back([]() {
        tileprovider::deinitializeDefaultTile();
    });


    // Render
    global::callback::render.emplace_back([&]() { _tileCache->update(); });

    // Deinitialize
    global::callback::deinitialize.emplace_back([&]() { GdalWrapper::destroy(); });

    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");
    fRenderable->registerClass<globebrowsing::RenderableGlobe>("RenderableGlobe");

    auto fTranslation = FactoryManager::ref().factory<Translation>();
    ghoul_assert(fTranslation, "Translation factory was not created");
    fTranslation->registerClass<globebrowsing::GlobeTranslation>("GlobeTranslation");

    auto fTileProvider =
        std::make_unique<ghoul::TemplateFactory<tileprovider::TileProvider>>();
    ghoul_assert(fTileProvider, "TileProvider factory was not created");

    fTileProvider->registerClass<tileprovider::DefaultTileProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(
            layergroupid::TypeID::DefaultTileLayer
        )]
    );
    fTileProvider->registerClass<tileprovider::SingleImageProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(
            layergroupid::TypeID::SingleImageTileLayer
        )]
    );
    fTileProvider->registerClass<tileprovider::TemporalTileProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(
            layergroupid::TypeID::TemporalTileLayer
        )]
    );
    fTileProvider->registerClass<tileprovider::TileIndexTileProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(
            layergroupid::TypeID::TileIndexTileLayer
        )]
    );
    fTileProvider->registerClass<tileprovider::SizeReferenceTileProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(
            layergroupid::TypeID::SizeReferenceTileLayer
        )]
    );
    fTileProvider->registerClass<tileprovider::TileProviderByLevel>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(
            layergroupid::TypeID::ByLevelTileLayer
        )]
    );
    fTileProvider->registerClass<tileprovider::TileProviderByIndex>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(
            layergroupid::TypeID::ByIndexTileLayer
        )]
    );

    FactoryManager::ref().addFactory(std::move(fTileProvider), _factoryName);

    auto fDashboard = FactoryManager::ref().factory<DashboardItem>();
    ghoul_assert(fDashboard, "Dashboard factory was not created");

    fDashboard->registerClass<DashboardItemGlobeLocation>("DashboardItemGlobeLocation");
}

globebrowsing::cache::MemoryAwareTileCache* GlobeBrowsingModule::tileCache() {
    return _tileCache.get();
}

scripting::LuaLibrary GlobeBrowsingModule::luaLibrary() const {
    std::string listLayerGroups = layerGroupNamesList();

    scripting::LuaLibrary res;
    res.name = "globebrowsing";
    res.functions = {
        {
            "addLayer",
            &globebrowsing::luascriptfunctions::addLayer,
            {},
            "string, string, table",
            "Adds a layer to the specified globe. The first argument specifies the "
            "name of the scene graph node of which to add the layer. The renderable "
            "of the specified scene graph node needs to be a renderable globe. "
            "The second argument is the layer group which can be any of "
            + listLayerGroups + ". The third argument is the dictionary defining the "
            "layer."
        },
        {
            "deleteLayer",
            &globebrowsing::luascriptfunctions::deleteLayer,
            {},
            "string, string",
            "Removes a layer from the specified globe. The first argument specifies "
            "the name of the scene graph node of which to remove the layer. "
            "The renderable of the specified scene graph node needs to be a "
            "renderable globe. The second argument is the layer group which can be "
            "any of " + listLayerGroups + ". The third argument is the dictionary"
            "defining the layer."
        },
        {
            "goToChunk",
            &globebrowsing::luascriptfunctions::goToChunk,
            {},
            "void",
            "Go to chunk with given index x, y, level"
        },
        {
            "goToGeo",
            &globebrowsing::luascriptfunctions::goToGeo,
            {},
            "number, number, number",
            "Go to geographic coordinates latitude and longitude"
        },
        {
            "getGeoPosition",
            &globebrowsing::luascriptfunctions::getGeoPosition,
            {},
            "name, latitude, longitude, altitude",
            "Returns the specified surface position on the globe as three floating point "
            "values"
        },
        {
            "getGeoPositionForCamera",
            &globebrowsing::luascriptfunctions::getGeoPositionForCamera,
            {},
            "void",
            "Get geographic coordinates of the camera poosition in latitude, "
            "longitude, and altitude"
        },
        {
            "loadWMSCapabilities",
            &globebrowsing::luascriptfunctions::loadWMSCapabilities,
            {},
            "string, string, string",
            "Loads and parses the WMS capabilities xml file from a remote server. "
            "The first argument is the name of the capabilities that can be used to "
            "later refer to the set of capabilities. The second argument is the "
            "globe for which this server is applicable. The third argument is the "
            "URL at which the capabilities file can be found."
        },
        {
            "removeWMSServer",
            &globebrowsing::luascriptfunctions::removeWMSServer,
            {},
            "string",
            "Removes the WMS server identified by the first argument from the list "
            "of available servers. The parameter corrsponds to the first argument in "
            "the loadWMSCapabilities call that was used to load the WMS server."
        },
        {
            "capabilitiesWMS",
            &globebrowsing::luascriptfunctions::capabilities,
            {},
            "string",
            "Returns an array of tables that describe the available layers that are "
            "supported by the WMS server identified by the provided name. The 'URL'"
            "component of the returned table can be used in the 'FilePath' argument "
            "for a call to the 'addLayer' function to add the value to a globe."
        }
    };
    res.scripts = {
        absPath("${MODULE_GLOBEBROWSING}/scripts/layer_support.lua")
    };

    return res;
}

void GlobeBrowsingModule::goToChunk(int x, int y, int level) {
    Camera* cam = global::navigationHandler.camera();
    goToChunk(*cam, globebrowsing::TileIndex(x,y,level), glm::vec2(0.5f, 0.5f), true);
}

void GlobeBrowsingModule::goToGeo(double latitude, double longitude) {
    using namespace globebrowsing;
    Camera* cam = global::navigationHandler.camera();
    goToGeodetic2(
        *cam,
        Geodetic2{ glm::radians(latitude), glm::radians(longitude) },
        true
    );
}

void GlobeBrowsingModule::goToGeo(double latitude, double longitude,
                                double altitude)
{
    using namespace globebrowsing;

    Camera* cam = global::navigationHandler.camera();
    goToGeodetic3(
        *cam,
        {
            Geodetic2{ glm::radians(latitude), glm::radians(longitude) },
            altitude
        },
        true
    );
}

glm::vec3 GlobeBrowsingModule::cartesianCoordinatesFromGeo(
                                              const globebrowsing::RenderableGlobe& globe,
                                       double latitude, double longitude, double altitude)
{
    using namespace globebrowsing;

    const Geodetic3 pos = {
        { glm::radians(latitude), glm::radians(longitude) },
        altitude
    };

    const glm::dvec3 positionModelSpace = globe.ellipsoid().cartesianPosition(pos);
    //glm::dmat4 modelTransform = globe.modelTransform();
    //glm::dvec3 positionWorldSpace = glm::dvec3(modelTransform *
        //glm::dvec4(positionModelSpace, 1.0));

    return glm::vec3(positionModelSpace);
}

void GlobeBrowsingModule::goToChunk(Camera& camera, const globebrowsing::TileIndex& ti,
                                    glm::vec2 uv, bool doResetCameraDirection)
{
    using namespace globebrowsing;

    const RenderableGlobe* globe = castFocusNodeRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    // Camera position in model space
    const glm::dvec3 camPos = camera.positionVec3();
    const glm::dmat4 inverseModelTransform = glm::inverse(globe->modelTransform());
    const glm::dvec3 cameraPositionModelSpace = glm::dvec3(
        inverseModelTransform * glm::dvec4(camPos, 1)
    );

    const GeodeticPatch patch(ti);
    const Geodetic2 corner = patch.corner(SOUTH_WEST);
    Geodetic2 positionOnPatch = patch.size();
    positionOnPatch.lat *= uv.y;
    positionOnPatch.lon *= uv.x;
    const Geodetic2 pointPosition = {
        corner.lat + positionOnPatch.lat,
        corner.lon + positionOnPatch.lon
    };

    const glm::dvec3 positionOnEllipsoid = globe->ellipsoid().geodeticSurfaceProjection(
        cameraPositionModelSpace
    );
    const double altitude = glm::length(cameraPositionModelSpace - positionOnEllipsoid);

    goToGeodetic3(camera, {pointPosition, altitude}, doResetCameraDirection);
}

void GlobeBrowsingModule::goToGeodetic2(Camera& camera, globebrowsing::Geodetic2 geo2,
                                        bool doResetCameraDirection)
{
    using namespace globebrowsing;

    const RenderableGlobe* globe = castFocusNodeRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    const glm::dvec3 cameraPosition = global::navigationHandler.camera()->positionVec3();
    const glm::dmat4 inverseModelTransform =
        global::navigationHandler.orbitalNavigator().anchorNode()->inverseModelTransform();
    const glm::dvec3 cameraPositionModelSpace =
        glm::dvec3(inverseModelTransform * glm::dvec4(cameraPosition, 1.0));
    const SurfacePositionHandle posHandle = globe->calculateSurfacePositionHandle(
        cameraPositionModelSpace
    );

    const glm::dvec3 centerToActualSurface = posHandle.centerToReferenceSurface +
                       posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;
    const double altitude = glm::length(cameraPositionModelSpace - centerToActualSurface);

    goToGeodetic3(camera, { geo2, altitude }, doResetCameraDirection);
}

void GlobeBrowsingModule::goToGeodetic3(Camera& camera, globebrowsing::Geodetic3 geo3,
                                        bool doResetCameraDirection)
{
    using namespace globebrowsing;

    const RenderableGlobe* globe = castFocusNodeRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    const glm::dvec3 positionModelSpace = globe->ellipsoid().cartesianPosition(geo3);
    const glm::dmat4 modelTransform = globe->modelTransform();
    const glm::dvec3 positionWorldSpace = glm::dvec3(modelTransform *
                                    glm::dvec4(positionModelSpace, 1.0));
    camera.setPositionVec3(positionWorldSpace);

    if (doResetCameraDirection) {
        resetCameraDirection(camera, geo3.geodetic2);
    }
}

void GlobeBrowsingModule::resetCameraDirection(Camera& camera,
                                               globebrowsing::Geodetic2 geo2)
{
    using namespace globebrowsing;

    const RenderableGlobe* globe = castFocusNodeRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    // Camera is described in world space
    const glm::dmat4 modelTransform = globe->modelTransform();

    // Lookup vector
    const glm::dvec3 positionModelSpace = globe->ellipsoid().cartesianSurfacePosition(
        geo2
    );
    const glm::dvec3 slightlyNorth = globe->ellipsoid().cartesianSurfacePosition(
        Geodetic2{ geo2.lat + 0.001, geo2.lon }
    );
    const glm::dvec3 lookUpModelSpace = glm::normalize(
        slightlyNorth - positionModelSpace
    );
    const glm::dvec3 lookUpWorldSpace = glm::dmat3(modelTransform) * lookUpModelSpace;

    // Lookat vector
    const glm::dvec3 lookAtWorldSpace = glm::dvec3(
        modelTransform * glm::dvec4(positionModelSpace, 1.0)
    );

    // Eye position
    const glm::dvec3 eye = camera.positionVec3();

    // Matrix
    const glm::dmat4 lookAtMatrix = glm::lookAt(eye, lookAtWorldSpace, lookUpWorldSpace);

    // Set rotation
    const glm::dquat rotation = glm::quat_cast(inverse(lookAtMatrix));
    camera.setRotation(rotation);
}

const globebrowsing::RenderableGlobe*
GlobeBrowsingModule::castFocusNodeRenderableToGlobe()
{
    using namespace globebrowsing;

    const Renderable* renderable =
        global::navigationHandler.orbitalNavigator().anchorNode()->renderable();

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

std::string GlobeBrowsingModule::layerGroupNamesList() {
    std::string listLayerGroups;
    for (int i = 0; i < globebrowsing::layergroupid::NUM_LAYER_GROUPS - 1; ++i) {
        listLayerGroups += globebrowsing::layergroupid::LAYER_GROUP_IDENTIFIERS[i] +
                           std::string(", ");
    }
    listLayerGroups += std::string(" and ") +
        globebrowsing::layergroupid::LAYER_GROUP_IDENTIFIERS[
            globebrowsing::layergroupid::NUM_LAYER_GROUPS - 1
        ];
    return listLayerGroups;
}

std::string GlobeBrowsingModule::layerTypeNamesList() {
    std::string listLayerTypes;
    for (int i = 0; i < globebrowsing::layergroupid::NUM_LAYER_TYPES - 1; ++i) {
        listLayerTypes += std::string(globebrowsing::layergroupid::LAYER_TYPE_NAMES[i]) +
                          ", ";
    }
    listLayerTypes += std::string(" and ") +
        globebrowsing::layergroupid::LAYER_TYPE_NAMES[
            globebrowsing::layergroupid::NUM_LAYER_TYPES - 1
        ];
    return listLayerTypes;
}

void GlobeBrowsingModule::loadWMSCapabilities(std::string name, std::string globe,
                                              std::string url)
{
    auto downloadFunction = [](const std::string& downloadUrl) {
        LDEBUG("Opening WMS capabilities: " + downloadUrl);
        GDALDatasetH dataset = GDALOpen(
            downloadUrl.c_str(),
            GA_ReadOnly
        );

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
    for (auto it = _urlList.begin(); it != _urlList.end(); ) {
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
    for (auto i = range.first; i != range.second; ++i) {
        res.emplace_back(i->second);
    }
    return res;
}

bool GlobeBrowsingModule::hasUrlInfo(const std::string& globe) const {
    return _urlList.find(globe) != _urlList.end();
}

bool GlobeBrowsingModule::isCachingEnabled() const {
    return _cacheEnabled;
}

bool GlobeBrowsingModule::isInOfflineMode() const {
    return _offlineMode;
}

std::string GlobeBrowsingModule::cacheLocation() const {
    return _cacheLocation;
}

uint64_t GlobeBrowsingModule::cacheSize() const {
    uint64_t size = _cacheSizeMB;
    return size * 1024 * 1024;
}


} // namespace openspace
