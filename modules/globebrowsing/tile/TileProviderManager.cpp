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
#include <modules/globebrowsing/tile/tileproviderfactory.h>

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
                i == LayeredTextures::WaterMasks    ||
                i == LayeredTextures::GrayScaleOverlays) {
                initData.minimumPixelSize = textureInitDictionary.value<double>("ColorTextureMinimumSize");
            }
            else if (i == LayeredTextures::Overlays) {
                initData.minimumPixelSize = textureInitDictionary.value<double>("OverlayMinimumSize");
            }
            else if (i == LayeredTextures::HeightMaps || i == LayeredTextures::HeightMapOverlays) {
                initData.minimumPixelSize = textureInitDictionary.value<double>("HeightMapMinimumSize");
            }
            else {
                initData.minimumPixelSize = 512;
            }

            initData.threads = 1;
            initData.cacheSize = 5000;
            initData.framesUntilRequestQueueFlush = 60;
            initData.preprocessTiles =
                i == LayeredTextures::HeightMaps ||
                i == LayeredTextures::HeightMapOverlays; // Only preprocess height maps.

            initTexures(
                _layerCategories[i],
                texturesDict,
                initData);

            // init level blending to be true
            levelBlendingEnabled[i] = true;
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
            
            std::string dictKey = std::to_string(i + 1);
            ghoul::Dictionary texDict = texturesDict.value<ghoul::Dictionary>(dictKey);
            texDict.getValue("Name", name);
            texDict.getValue("FilePath", path);

            std::string type = "LRUCaching"; // if type is unspecified
            texDict.getValue("Type", type);

            
            std::shared_ptr<TileProvider> tileProvider = TileProviderFactory::ref()->create(type, path, initData);
            if (tileProvider == nullptr) {
                continue;
            }

            bool enabled = false; // defaults to false if unspecified
            texDict.getValue("Enabled", enabled);

            dest.push_back({ name, tileProvider, enabled });
        }
    }

    TileProviderManager::LayerCategory& TileProviderManager::getLayerCategory(LayeredTextures::TextureCategory category)
    {
        return _layerCategories[category];
    }

    void TileProviderManager::update() {
        for (auto layerCategory : _layerCategories) {
            for (auto tileProviderWithName : layerCategory) {
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
        for (auto tileProviderWithName : _layerCategories[textureCategory]) {
            if (tileProviderWithName.isActive) {
                tileProviders.push_back(tileProviderWithName.tileProvider);
            }
        }
        return tileProviders;
    }

}  // namespace openspace
