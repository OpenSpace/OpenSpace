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
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/other/distanceswitch.h>
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
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/templatefactory.h>
#include <ghoul/misc/assert.h>

#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>

#include "globebrowsingmodule_lua.inl"

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
            CpuCap.installedMainMemory() * 0.25 * 1024 * 1024); // 25% of total RAM
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
        },
        {
            "${MODULE_GLOBEBROWSING}/scripts/layer_support.lua"
        },
        {
            // Documentation
        }
    };
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
    std::string listLayerTypes("");
    for (int i = 0; i < globebrowsing::layergroupid::NUM_LAYER_TYPES - 1; ++i) {
        listLayerTypes += globebrowsing::layergroupid::LAYER_TYPE_NAMES[i] + ", ";
    }
    listLayerTypes +=
        " and " + globebrowsing::layergroupid::LAYER_TYPE_NAMES[globebrowsing::layergroupid::NUM_LAYER_TYPES - 1];
    return listLayerTypes;
}

} // namespace openspace
