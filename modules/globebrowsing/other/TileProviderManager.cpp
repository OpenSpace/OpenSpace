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

#include <modules/globebrowsing/other/tileprovidermanager.h>
#include <ghoul/logging/logmanager.h>


namespace {
    const std::string _loggerCat = "TileProviderManager";

    const std::string keyColorTextures = "ColorTextures";
    const std::string keyHeightMaps = "HeightMaps";
}


namespace openspace {

    ThreadPool TileProviderManager::tileRequestThreadPool(1);


    TileProviderManager::TileProviderManager(const ghoul::Dictionary& texDict){

        // manually add a temporal tile provider for testing
        std::string filename = "map_service_configs/VIIRS_SNPP_CorrectedReflectance_TrueColor_temporal.xml";
        TileProviderInitData initData;
        initData.minimumPixelSize = 1024;
        initData.threads = 1;
        initData.cacheSize = 50;
        initData.framesUntilRequestQueueFlush = 60;

        std::shared_ptr<TileProvider> colorTextureProvider = std::shared_ptr<TemporalTileProvider>(
            new TemporalTileProvider(filename, initData));
        std::string name = "Temporal VIIRS SNPP";
        _colorTextureProviders.push_back({ name, colorTextureProvider, true });


        ghoul::Dictionary colorTexturesDict;
        texDict.getValue(keyColorTextures, colorTexturesDict);

        TileProviderInitData colorInitData;
        colorInitData.minimumPixelSize = 1024;
        colorInitData.threads = 1;
        colorInitData.cacheSize = 500;
        colorInitData.framesUntilRequestQueueFlush = 60;

        initTexures(_colorTextureProviders, colorTexturesDict, colorInitData);


        ghoul::Dictionary heightTexturesDict;
        texDict.getValue(keyHeightMaps, heightTexturesDict);

        TileProviderInitData heightInitData;
        heightInitData.minimumPixelSize = 64;
        heightInitData.threads = 1;
        heightInitData.cacheSize = 500;
        heightInitData.framesUntilRequestQueueFlush = 60;

        initTexures(_heightMapProviders, heightTexturesDict, heightInitData);
    }

    TileProviderManager::~TileProviderManager()
    {
    }

    void TileProviderManager::initTexures(std::vector<TileProviderWithName>& dest,
        const ghoul::Dictionary& texturesDict, const TileProviderInitData& initData)
    {

        // Create TileProviders for all color textures
        for (size_t i = 0; i < texturesDict.size(); i++) {
            std::string name, path;
            std::string dictKey = std::to_string(i + 1);
            ghoul::Dictionary texDict = texturesDict.value<ghoul::Dictionary>(dictKey);
            texDict.getValue("Name", name);
            texDict.getValue("FilePath", path);


            std::shared_ptr<TileProvider> tileProvider = initProvider(path, initData);

            bool enabled = dest.size() == 0; // Only enable first layer
            dest.push_back({ name, tileProvider, enabled });
        }
    }


    std::shared_ptr<TileProvider> TileProviderManager::initProvider(const std::string& file,
        const TileProviderInitData& initData)
    {
        std::shared_ptr<TileDataset> tileDataset = std::shared_ptr<TileDataset>(
            new TileDataset(file, initData.minimumPixelSize));

        std::shared_ptr<ThreadPool> threadPool = std::shared_ptr<ThreadPool>(
            new ThreadPool(1));

        std::shared_ptr<AsyncTileDataProvider> tileReader = std::shared_ptr<AsyncTileDataProvider>(
            new AsyncTileDataProvider(tileDataset, threadPool));

        std::shared_ptr<TileProvider> tileProvider = std::shared_ptr<TileProvider>(
            new CachingTileProvider(tileReader, initData.cacheSize, initData.framesUntilRequestQueueFlush));

        return tileProvider;
    }

    std::vector<TileProviderManager::TileProviderWithName>&
        TileProviderManager::heightMapProviders()
    {
        return _heightMapProviders;
    }

    std::vector<TileProviderManager::TileProviderWithName>&
        TileProviderManager::colorTextureProviders()
    {
        return _colorTextureProviders;
    }

    const std::vector<std::shared_ptr<TileProvider> >
        TileProviderManager::getActiveHeightMapProviders()
    {
        std::vector<std::shared_ptr<TileProvider> > tileProviders;
        for (auto it = _heightMapProviders.begin(); it != _heightMapProviders.end(); it++)
        {
            if (it->isActive) {
                tileProviders.push_back(it->tileProvider);
            }
        }
        return tileProviders;
    }

    const std::vector<std::shared_ptr<TileProvider> >
        TileProviderManager::getActiveColorTextureProviders()
    {
        std::vector<std::shared_ptr<TileProvider> > tileProviders;
        for (auto it = _colorTextureProviders.begin(); it != _colorTextureProviders.end(); it++)
        {
            if (it->isActive) {
                tileProviders.push_back(it->tileProvider);
            }
        }
        return tileProviders;
    }


}  // namespace openspace
