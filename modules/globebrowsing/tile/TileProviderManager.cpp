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

#include <modules/globebrowsing/tile/tileprovidermanager.h>

#include <ghoul/logging/logmanager.h>

#include "cpl_minixml.h"


namespace {
    const std::string _loggerCat = "TileProviderManager";
}


namespace openspace {

    ThreadPool TileProviderManager::tileRequestThreadPool(1);

    TileProviderManager::TileProviderManager(
        const ghoul::Dictionary& textureCategoriesDictionary,
        const ghoul::Dictionary& textureInitDictionary){
        // Create all the categories of tile providers
        for (size_t i = 0; i < textureCategoriesDictionary.size(); i++) {
            ghoul::Dictionary texturesDict = textureCategoriesDictionary.value<ghoul::Dictionary>(
                LayeredTextures::TEXTURE_CATEGORY_NAMES[i]);

            ghoul_assert(texturesDict.size() <=
                LayeredTextures::MAX_NUM_TEXTURES_PER_CATEGORY,
                "Too many textures! Number of textures per category must be less than or equal to "
                << LayeredTextures::MAX_NUM_TEXTURES_PER_CATEGORY);

            TileProviderInitData initData;

            if (i == LayeredTextures::ColorTextures ||
                i == LayeredTextures::NightTextures ||
                i == LayeredTextures::WaterMasks) {
                initData.minimumPixelSize = textureInitDictionary.value<double>("ColorTextureMinimumSize");
            }
            else if (i == LayeredTextures::Overlays) {
                initData.minimumPixelSize = textureInitDictionary.value<double>("OverlayMinimumSize");
            }
            else if (i == LayeredTextures::HeightMaps) {
                initData.minimumPixelSize = textureInitDictionary.value<double>("HeightMapMinimumSize");
            }
            else {
                initData.minimumPixelSize = 512;
            }

            initData.threads = 1;
            initData.cacheSize = 500;
            initData.framesUntilRequestQueueFlush = 60;
            initData.preprocessTiles = i == LayeredTextures::HeightMaps; // Only preprocess height maps.

            initTexures(
                _layerCategories[i],
                texturesDict,
                initData);
        }
    }

    TileProviderManager::~TileProviderManager()
    {
    }

    void TileProviderManager::initTexures(std::vector<TileProviderWithName>& dest,
        const ghoul::Dictionary& texturesDict, const TileProviderInitData& initData)
    {
        // Create TileProviders for all textures within this category
        for (size_t i = 0; i < texturesDict.size(); i++) {
            std::string name, path;
            // Default to enabled = true on case it's not defined in the dictionary
            bool enabled = true;
            std::string dictKey = std::to_string(i + 1);
            ghoul::Dictionary texDict = texturesDict.value<ghoul::Dictionary>(dictKey);
            texDict.getValue("Name", name);
            texDict.getValue("FilePath", path);
            texDict.getValue("Enabled", enabled);

            std::shared_ptr<TileProvider> tileProvider;
            try {
                tileProvider = initProvider(path, initData);
            }
            catch (const ghoul::RuntimeError& e) {
                LERROR(e.message);
                continue;
            }
            dest.push_back({ name, tileProvider, enabled });
        }
    }


    std::shared_ptr<TileProvider> TileProviderManager::initProvider(const std::string& file,
        const TileProviderInitData& initData)
    {
        std::shared_ptr<TileProvider> tileProvider;
        CPLXMLNode * node = CPLParseXMLFile(file.c_str());
        if (!node) {
            throw ghoul::RuntimeError("Unable to parse XML:\n" + file);
        }
        if (std::string(node->pszValue) == "OpenSpaceTemporalGDALDataset") {
            tileProvider = std::make_shared<TemporalTileProvider>(file, initData);
            return tileProvider;
        }

        auto tileDataset = std::make_shared<TileDataset>(file, initData.minimumPixelSize, initData.preprocessTiles);
        auto threadPool = std::make_shared<ThreadPool>(1);
        auto tileReader = std::make_shared<AsyncTileDataProvider>(tileDataset, threadPool);
        auto tileCache = std::make_shared<TileCache>(initData.cacheSize);
        tileProvider = std::make_shared<CachingTileProvider>(tileReader, tileCache, initData.framesUntilRequestQueueFlush);

        return tileProvider;
    }

    TileProviderManager::LayerCategory& TileProviderManager::getLayerCategory(LayeredTextures::TextureCategory category)
    {
        return _layerCategories[category];
    }

    void TileProviderManager::update() {
        for each (auto layerCategory in _layerCategories) {
            for each (auto tileProviderWithName in layerCategory) {
                if (tileProviderWithName.isActive) {
                    tileProviderWithName.tileProvider->update();
                }
            }
        }
    }

    const std::vector<std::shared_ptr<TileProvider> >
        TileProviderManager::getActivatedLayerCategory(
            LayeredTextures::TextureCategory textureCategory)
    {
        std::vector<std::shared_ptr<TileProvider> > tileProviders;
        for each (auto tileProviderWithName in _layerCategories[textureCategory]) {
            if (tileProviderWithName.isActive) {
                tileProviders.push_back(tileProviderWithName.tileProvider);
            }
        }
        return tileProviders;
    }

}  // namespace openspace
