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

#include <openspace/util/factorymanager.h>

#include <modules/globebrowsing/tile/tileprovidermanager.h>

#include <ghoul/logging/logmanager.h>

#include "cpl_minixml.h"


namespace {
    const std::string _loggerCat = "TileProviderManager";
}


namespace openspace {


    //////////////////////////////////////////////////////////////////////////////////////
    //                            Tile Provider Group                                   //
    //////////////////////////////////////////////////////////////////////////////////////

    void TileProviderGroup::update() {
        for (auto tileProviderWithName : tileProviders) {
            if (tileProviderWithName.isActive) {
                tileProviderWithName.tileProvider->update();
            }
        }
    }


    const std::vector<std::shared_ptr<TileProvider>> TileProviderGroup::getActiveTileProviders() const {
        std::vector<std::shared_ptr<TileProvider>> activeTileProviders;
        for (auto tileProviderWithName : tileProviders) {
            if (tileProviderWithName.isActive) {
                activeTileProviders.push_back(tileProviderWithName.tileProvider);
            }
        }
        return activeTileProviders;
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //                           Tile Provider Manager                                  //
    //////////////////////////////////////////////////////////////////////////////////////


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
            else if (i == LayeredTextures::HeightMaps) {
                initData.minimumPixelSize = textureInitDictionary.value<double>("HeightMapMinimumSize");
            }
            else {
                initData.minimumPixelSize = 512;
            }

            initData.threads = 1;
            initData.cacheSize = 5000;
            initData.framesUntilRequestQueueFlush = 60;
            // Only preprocess height maps.
            initData.preprocessTiles = i == LayeredTextures::HeightMaps;

            initTexures(
                _layerCategories[i].tileProviders,
                texturesDict,
                initData);

            // init level blending to be true
            _layerCategories[i].levelBlendingEnabled = true;
        }
    }

    TileProviderManager::~TileProviderManager()
    {
    }

    void TileProviderManager::initTexures(std::vector<NamedTileProvider>& dest,
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


            auto tileProviderFactory = FactoryManager::ref().factory<TileProvider>();
            TileProvider* tileProvider = tileProviderFactory->create(type, texDict);
            if (tileProvider == nullptr) {
                LERROR("Unable to create TileProvider '" << name << "' of type '" << type << "'");
                continue;
            }

            bool enabled = false; // defaults to false if unspecified
            texDict.getValue("Enabled", enabled);

            dest.push_back({ name, std::shared_ptr<TileProvider>(tileProvider), enabled });
        }
    }

    TileProviderGroup& TileProviderManager::getTileProviderGroup(size_t groupId) {
        return _layerCategories[groupId];
    }

    TileProviderGroup& TileProviderManager::getTileProviderGroup(LayeredTextures::TextureCategory category) {
        return _layerCategories[category];
    }

    void TileProviderManager::update() {
        for (auto tileProviderGroup : _layerCategories) {
            tileProviderGroup.update();
        }
    }

    void TileProviderManager::reset(bool includingInactive) {
        for (auto layerCategory : _layerCategories) {
            for (auto tileProviderWithName : layerCategory.tileProviders) {
                if (tileProviderWithName.isActive) {
                    tileProviderWithName.tileProvider->reset();
                }
                else if (includingInactive) {
                    tileProviderWithName.tileProvider->reset();
                }
            }
        }
    }

   

}  // namespace openspace
