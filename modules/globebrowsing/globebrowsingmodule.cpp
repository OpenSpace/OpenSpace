/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/cache/memoryawaretilecache.h>
#include <modules/globebrowsing/geometry/angle.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/other/distanceswitch.h>
#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/rawtiledatareader/gdalwrapper.h>
#include <modules/globebrowsing/tile/tileprovider/defaulttileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/singleimageprovider.h>
#include <modules/globebrowsing/tile/tileprovider/sizereferencetileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/temporaltileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/texttileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/tileindextileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/tileproviderbylevel.h>
#include <modules/globebrowsing/tile/tileprovider/tileproviderbyindex.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/rendering/layer/layer.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/templatefactory.h>
#include <ghoul/misc/assert.h>
#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>

#ifdef GLOBEBROWSING_USE_GDAL
#include <gdal.h>
#include <cpl_string.h>
#endif // GLOBEBROWSING_USE_GDAL

#include "globebrowsingmodule_lua.inl"


namespace {
    const char* _loggerCat = "GlobeBrowsingModule";

#ifdef GLOBEBROWSING_USE_GDAL
    openspace::GlobeBrowsingModule::Capabilities
    parseSubDatasets(char** subDatasets, int nSubdatasets)
    {
        // Idea:  Iterate over the list of sublayers keeping a current layer and identify it
        //        by its number.  If this number changes, we know that we have a new layer


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

            std::string identifier = std::string(IdentifierBuffer);
            std::string ds(subDatasets[i]);
            std::string value = ds.substr(ds.find_first_of('=') + 1);

            // The DESC/NAME difference is not a typo
            if (identifier == "DESC") {
                currentLayer.name = value;
            }
            else if (identifier == "NAME") {
                currentLayer.url = value;
            }
            else {
                LINFOC("GlobeBrowsingGUI", "Unknown subdataset identifier: " + identifier);
            }
        }

        return result;
    }

#endif // GLOBEBROWSING_USE_GDAL

} // namespace

namespace openspace {

GlobeBrowsingModule::GlobeBrowsingModule() : OpenSpaceModule(Name) {}

void GlobeBrowsingModule::internalInitialize() {
    using namespace globebrowsing;

    // Initialize
    OsEng.registerModuleCallback(OpenSpaceEngine::CallbackOption::Initialize, [&] {
        _tileCache = std::make_unique<globebrowsing::cache::MemoryAwareTileCache>();
        addPropertySubOwner(*_tileCache);
#ifdef GLOBEBROWSING_USE_GDAL
        // Convert from MB to Bytes
        GdalWrapper::create(
            16ULL * 1024ULL * 1024ULL, // 16 MB
            static_cast<size_t>(CpuCap.installedMainMemory() * 0.25 * 1024 * 1024)); // 25% of total RAM
        addPropertySubOwner(GdalWrapper::ref());
#endif // GLOBEBROWSING_USE_GDAL
    });
  
    // Render
    OsEng.registerModuleCallback(OpenSpaceEngine::CallbackOption::Render, [&]{
        _tileCache->update();
    });

    // Deinitialize
    OsEng.registerModuleCallback(OpenSpaceEngine::CallbackOption::Deinitialize, [&]{
#ifdef GLOBEBROWSING_USE_GDAL
        GdalWrapper::ref().destroy();
#endif // GLOBEBROWSING_USE_GDAL
    });

    // Get factories
    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");
    // Create factory for TileProviders
    auto fTileProvider =
        std::make_unique<ghoul::TemplateFactory<tileprovider::TileProvider>>();
    ghoul_assert(fTileProvider, "TileProvider factory was not created");
  
    // Register renderable class
    fRenderable->registerClass<globebrowsing::RenderableGlobe>("RenderableGlobe");

    // Register TileProvider classes
    fTileProvider->registerClass<tileprovider::DefaultTileProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(layergroupid::TypeID::DefaultTileLayer)]);
    fTileProvider->registerClass<tileprovider::SingleImageProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(layergroupid::TypeID::SingleImageTileLayer)]);
#ifdef GLOBEBROWSING_USE_GDAL
    fTileProvider->registerClass<tileprovider::TemporalTileProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(layergroupid::TypeID::TemporalTileLayer)]);
#endif // GLOBEBROWSING_USE_GDAL
    fTileProvider->registerClass<tileprovider::TileIndexTileProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(layergroupid::TypeID::TileIndexTileLayer)]);
    fTileProvider->registerClass<tileprovider::SizeReferenceTileProvider>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(layergroupid::TypeID::SizeReferenceTileLayer)]);
    fTileProvider->registerClass<tileprovider::TileProviderByLevel>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(layergroupid::TypeID::ByLevelTileLayer)]);
    fTileProvider->registerClass<tileprovider::TileProviderByIndex>(
        layergroupid::LAYER_TYPE_NAMES[static_cast<int>(layergroupid::TypeID::ByIndexTileLayer)]);
  
    FactoryManager::ref().addFactory(std::move(fTileProvider));
}

globebrowsing::cache::MemoryAwareTileCache* GlobeBrowsingModule::tileCache() {
    return _tileCache.get();
}

scripting::LuaLibrary GlobeBrowsingModule::luaLibrary() const {
    std::string listLayerGroups = layerGroupNamesList();
    return {
        "globebrowsing",
        {
            {
                "addLayer",
                &globebrowsing::luascriptfunctions::addLayer,
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
                "void",
                "Go to chunk with given index x, y, level"
            },
            {
                "goToGeo",
                &globebrowsing::luascriptfunctions::goToGeo,
                "number, number, number",
                "Go to geographic coordinates latitude and longitude"
            },
            {
                "getGeoPosition",
                &globebrowsing::luascriptfunctions::getGeoPosition,
                "void",
                "Get geographic coordinates of the camera poosition in latitude, "
                "longitude, and altitude"
            },
            {
                "loadWMSCapabilities",
                &globebrowsing::luascriptfunctions::loadWMSCapabilities,
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
                "string",
                "Removes the WMS server identified by the first argument from the list "
                "of available servers. The parameter corrsponds to the first argument in "
                "the loadWMSCapabilities call that was used to load the WMS server."
            },
            {
                "capabilitiesWMS",
                &globebrowsing::luascriptfunctions::capabilities,
                "string",
                "Returns an array of tables that describe the available layers that are "
                "supported by the WMS server identified by the provided name. The 'URL'"
                "component of the returned table can be used in the 'FilePath' argument "
                "for a call to the 'addLayer' function to add the value to a globe."
            }
        },
        {
            "${MODULE_GLOBEBROWSING}/scripts/layer_support.lua"
        },
        {
            // Documentation
        }
    };
}

void GlobeBrowsingModule::goToChunk(int x, int y, int level) {
    using namespace globebrowsing;
    Camera* cam = OsEng.navigationHandler().camera();
    goToChunk(*cam, TileIndex(x,y,level), glm::vec2(0.5, 0.5), true);
}

void GlobeBrowsingModule::goToGeo(double latitude, double longitude) {
    using namespace globebrowsing;
    Camera* cam = OsEng.navigationHandler().camera();
    goToGeodetic2(*cam, Geodetic2(
        Angle<double>::fromDegrees(latitude).asRadians(),
        Angle<double>::fromDegrees(longitude).asRadians()), true);
}

void GlobeBrowsingModule::goToGeo(double latitude, double longitude,
                                double altitude)
{
    using namespace globebrowsing;
    
    Camera* cam = OsEng.navigationHandler().camera();
    goToGeodetic3(
        *cam,
        {
            Geodetic2(
                Angle<double>::fromDegrees(latitude).asRadians(),
                Angle<double>::fromDegrees(longitude).asRadians()),
            altitude
        },
        true
    );
}

void GlobeBrowsingModule::goToChunk(Camera& camera, globebrowsing::TileIndex ti,
                                    glm::vec2 uv, bool resetCameraDirection)
{
    using namespace globebrowsing;

    RenderableGlobe* globe = castFocusNodeRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    // Camera position in model space
    glm::dvec3 camPos = camera.positionVec3();
    glm::dmat4 inverseModelTransform = globe->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
    glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));
    
    GeodeticPatch patch(ti);
    Geodetic2 corner = patch.getCorner(SOUTH_WEST);
    Geodetic2 positionOnPatch = patch.getSize();
    positionOnPatch.lat *= uv.y;
    positionOnPatch.lon *= uv.x;
    Geodetic2 pointPosition = corner + positionOnPatch;
    
    glm::dvec3 positionOnEllipsoid =
        globe->ellipsoid().geodeticSurfaceProjection(cameraPositionModelSpace);
    double altitude = glm::length(cameraPositionModelSpace - positionOnEllipsoid);
    
    goToGeodetic3(camera, {pointPosition, altitude}, resetCameraDirection);
}

void GlobeBrowsingModule::goToGeodetic2(Camera& camera,
                                        globebrowsing::Geodetic2 geo2,
                                        bool resetCameraDirection)
{
    using namespace globebrowsing;

    RenderableGlobe* globe = castFocusNodeRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }
    
    glm::dvec3 cameraPosition = OsEng.navigationHandler().camera()->positionVec3();
    glm::dmat4 inverseModelTransform =
        OsEng.navigationHandler().focusNode()->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
        inverseModelTransform * glm::dvec4(cameraPosition, 1.0);
    SurfacePositionHandle posHandle = globe->calculateSurfacePositionHandle(
                                                                cameraPositionModelSpace);
    
    glm::dvec3 centerToActualSurface = posHandle.centerToReferenceSurface +
        posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;
    double altitude = glm::length(cameraPositionModelSpace - centerToActualSurface);

    goToGeodetic3(camera, {geo2, altitude}, resetCameraDirection);
}
    
void GlobeBrowsingModule::goToGeodetic3(Camera& camera, globebrowsing::Geodetic3 geo3,
                                        bool resetCameraDirection)
{
    using namespace globebrowsing;

    RenderableGlobe* globe = castFocusNodeRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    glm::dvec3 positionModelSpace = globe->ellipsoid().cartesianPosition(geo3);
    glm::dmat4 modelTransform = globe->modelTransform();
    glm::dvec3 positionWorldSpace = modelTransform * glm::dvec4(positionModelSpace, 1.0);
    camera.setPositionVec3(positionWorldSpace);

    if (resetCameraDirection) {
        this->resetCameraDirection(camera, geo3.geodetic2);
    }
}

void GlobeBrowsingModule::resetCameraDirection(Camera& camera, globebrowsing::Geodetic2 geo2)
{
    using namespace globebrowsing;

    RenderableGlobe* globe = castFocusNodeRenderableToGlobe();
    if (!globe) {
        LERROR("Focus node must have a RenderableGlobe renderable.");
        return;
    }

    // Camera is described in world space
    glm::dmat4 modelTransform = globe->modelTransform();
    
    // Lookup vector
    glm::dvec3 positionModelSpace = globe->ellipsoid().cartesianSurfacePosition(geo2);
    glm::dvec3 slightlyNorth = globe->ellipsoid().cartesianSurfacePosition(
        Geodetic2(geo2.lat + 0.001, geo2.lon));
    glm::dvec3 lookUpModelSpace = glm::normalize(slightlyNorth - positionModelSpace);
    glm::dvec3 lookUpWorldSpace = glm::dmat3(modelTransform) * lookUpModelSpace;
    
    // Lookat vector
    glm::dvec3 lookAtWorldSpace = modelTransform * glm::dvec4(positionModelSpace, 1.0);

    // Eye position
    glm::dvec3 eye = camera.positionVec3();
    
    // Matrix
    glm::dmat4 lookAtMatrix = glm::lookAt(
                    eye, lookAtWorldSpace, lookUpWorldSpace);
    
    // Set rotation
    glm::dquat rotation = glm::quat_cast(inverse(lookAtMatrix));
    camera.setRotation(rotation);
}

globebrowsing::RenderableGlobe* GlobeBrowsingModule::castFocusNodeRenderableToGlobe() {
    using namespace globebrowsing;

    Renderable* baseRenderable = OsEng.navigationHandler().focusNode()->renderable();
    if (!baseRenderable) {
        return nullptr;
    }
    if (globebrowsing::RenderableGlobe* globe =
            dynamic_cast<RenderableGlobe*>(baseRenderable))
    {
        return globe;
    }
    else {
        return nullptr;
    }
}

std::string GlobeBrowsingModule::layerGroupNamesList() {
    std::string listLayerGroups("");
    for (int i = 0; i < globebrowsing::layergroupid::NUM_LAYER_GROUPS - 1; ++i) {
        listLayerGroups +=
            globebrowsing::layergroupid::LAYER_GROUP_NAMES[i] + std::string(", ");
    }
    listLayerGroups +=
        std::string(" and ") + globebrowsing::layergroupid::LAYER_GROUP_NAMES[
            globebrowsing::layergroupid::NUM_LAYER_GROUPS - 1];
    return listLayerGroups;
}

std::string GlobeBrowsingModule::layerTypeNamesList() {
    std::string listLayerTypes;
    for (int i = 0; i < globebrowsing::layergroupid::NUM_LAYER_TYPES - 1; ++i) {
        listLayerTypes += std::string(globebrowsing::layergroupid::LAYER_TYPE_NAMES[i]) + ", ";
    }
    listLayerTypes +=
        " and " + std::string(globebrowsing::layergroupid::LAYER_TYPE_NAMES[globebrowsing::layergroupid::NUM_LAYER_TYPES - 1]);
    return listLayerTypes;
}

#ifdef GLOBEBROWSING_USE_GDAL

void GlobeBrowsingModule::loadWMSCapabilities(std::string name, std::string globe,
                                              std::string url)
{
    auto downloadFunction = [](const std::string& url) {
        GDALDatasetH dataset = GDALOpen(
            url.c_str(),
            GA_ReadOnly
        );
        //    GDAL_OF_READONLY | GDAL_OF_RASTER | GDAL_OF_VERBOSE_ERROR,
        //    nullptr,
        //    nullptr,
        //    nullptr
        //);

        char** subDatasets = GDALGetMetadata(dataset, "SUBDATASETS");
        int nSubdatasets = CSLCount(subDatasets);
        Capabilities cap = parseSubDatasets(subDatasets, nSubdatasets);
        GDALClose(dataset);
        return cap;
    };
    
    _inFlightCapabilitiesMap[name] = std::async(std::launch::async, downloadFunction, url);

    _urlList.emplace(std::move(globe), UrlInfo{ std::move(name), std::move(url) });
}

GlobeBrowsingModule::Capabilities
GlobeBrowsingModule::capabilities(const std::string& name)
{
    // First check the ones that have already finished
    auto it = _capabilitiesMap.find(name);
    if (it != _capabilitiesMap.end()) {
        return it->second;
    }
    else {
        auto inFlightIt = _inFlightCapabilitiesMap.find(name);
        if (inFlightIt != _inFlightCapabilitiesMap.end()) {
            // If the download and the parsing has not finished yet, this will block,
            // otherwise it will just return
            Capabilities cap = inFlightIt->second.get();
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
    auto inFlightIt = _inFlightCapabilitiesMap.find(name);
    if (inFlightIt != _inFlightCapabilitiesMap.end()) {
        _inFlightCapabilitiesMap.erase(inFlightIt);
    }

    // Then download the ones that are already finished
    auto capIt = _capabilitiesMap.find(name);
    if (capIt != _capabilitiesMap.end()) {
        _capabilitiesMap.erase(capIt);
    }

    // Then remove the calues from the globe server list
    for (auto it = _urlList.begin(); it != _urlList.end(); ) {
        // We have to increment first because the erase will invalidate the iterator
        auto eraseIt = it++;

        if (eraseIt->second.name == name) {
            _urlList.erase(eraseIt);
        }
    }
}


std::vector<GlobeBrowsingModule::UrlInfo>
GlobeBrowsingModule::urlInfo(const std::string& globe) const
{
    auto range = _urlList.equal_range(globe);
    std::vector<UrlInfo> res;
    for (auto i = range.first; i != range.second; ++i) {
        res.emplace_back(i->second);
    }
    return res;
}


#endif // GLOBEBROWSING_USE_GDAL

} // namespace openspace
