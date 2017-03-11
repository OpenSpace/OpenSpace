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

#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/other/distanceswitch.h>
#include <modules/globebrowsing/cache/memoryawaretilecache.h>
#include <modules/globebrowsing/tile/rawtiledatareader/gdalrawtiledatareader.h>
#include <modules/globebrowsing/tile/tileprovider/cachingtileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/singleimageprovider.h>
#include <modules/globebrowsing/tile/tileprovider/sizereferencetileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/temporaltileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/texttileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/tileindextileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/tile/tileprovider/tileproviderbylevel.h>
#include <modules/globebrowsing/tile/tileprovider/tileproviderbyindex.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderable.h>
#include <openspace/util/factorymanager.h>

#include <ghoul/misc/templatefactory.h>
#include <ghoul/misc/assert.h>

#include <ghoul/systemcapabilities/generalcapabilitiescomponent.h>

namespace openspace {

GlobeBrowsingModule::GlobeBrowsingModule()
: OpenSpaceModule("GlobeBrowsing")
{
}

void GlobeBrowsingModule::internalInitialize() {
    using namespace globebrowsing;

    OsEng.registerModuleCallback(OpenSpaceEngine::CallbackOption::Initialize, [&]{
        _openSpaceMaximumTileCacheSize = std::make_unique<properties::IntProperty>(
            "maximumTileCacheSize", "Maximum tile cache size",
            512, // Default: 512 MB
            0,    // Minimum: No caching
            CpuCap.installedMainMemory() * 0.25, // 25% Of total RAM
            1);   // Step: One MB
        
        _GDALMaximumTileCacheSize = std::make_unique<properties::IntProperty> (
            "maximumGDALBlockCacheSize", "Maximum GDAL block cache size",
            16, // Default: 16 MB
            0,  // Minimum: No caching
            CpuCap.installedMainMemory() * 0.25, // 25% Of total RAM
            1); // Step: One MB

        _logGDALErrors = std::make_unique<properties::BoolProperty> (
            "logGDALErrors", "Log GDAL errors", false);
      
        _clearTileCache = std::make_unique<properties::TriggerProperty> (
            "clearTileCache", "Clear tile cache");
      
        // Convert from MB to KB
        cache::MemoryAwareTileCache::create(*_openSpaceMaximumTileCacheSize * 1024);

        _openSpaceMaximumTileCacheSize->onChange(
        [&]{
            // Convert from MB to KB
            globebrowsing::cache::MemoryAwareTileCache::ref().setMaximumSize(
                *_openSpaceMaximumTileCacheSize * 1024);
        });
        /*
        _GDALMaximumTileCacheSize->onChange(
        [&]{
            // Convert from MB to KB
            globebrowsing::TileDataset::setGDALMaximumCacheSize(
                *_GDALMaximumTileCacheSize * 1024);
        });
        */
        _clearTileCache->onChange(
        [&]{
            globebrowsing::cache::MemoryAwareTileCache::ref().clear();
        });
        /*
        _logGDALErrors->onChange(
        [&]{
            globebrowsing::TileDataset::logGDALErrors = *_logGDALErrors;
        });
        */

        addProperty(*_openSpaceMaximumTileCacheSize);
        addProperty(*_GDALMaximumTileCacheSize);
		addProperty(*_clearTileCache);
		addProperty(*_logGDALErrors);
	});
  
    OsEng.registerModuleCallback(OpenSpaceEngine::CallbackOption::Deinitialize, [&]{
        globebrowsing::cache::MemoryAwareTileCache::ref().clear();
    });

    auto fRenderable = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(fRenderable, "Renderable factory was not created");
    fRenderable->registerClass<globebrowsing::RenderableGlobe>("RenderableGlobe");

    // add Tile Provider factory
    auto fTileProvider = std::make_unique<ghoul::TemplateFactory<tileprovider::TileProvider>>();

    fTileProvider->registerClass<tileprovider::CachingTileProvider>("LRUCaching");
    fTileProvider->registerClass<tileprovider::SingleImageProvider>("SingleImage");
    fTileProvider->registerClass<tileprovider::TemporalTileProvider>("Temporal");
    fTileProvider->registerClass<tileprovider::TileIndexTileProvider>("TileIndex");
    fTileProvider->registerClass<tileprovider::SizeReferenceTileProvider>("SizeReference");

    // Combining Tile Providers
    fTileProvider->registerClass<tileprovider::TileProviderByLevel>("ByLevel");
    fTileProvider->registerClass<tileprovider::TileProviderByIndex>("ByIndex");

    FactoryManager::ref().addFactory(std::move(fTileProvider));
}

} // namespace openspace
