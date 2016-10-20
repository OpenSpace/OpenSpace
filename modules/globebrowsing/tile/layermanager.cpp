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

#include <modules/globebrowsing/tile/layermanager.h>

#include <openspace/properties/scalarproperty.h>

#include <ghoul/logging/logmanager.h>

#include "cpl_minixml.h"

namespace {
    const std::string _loggerCat = "LayerManager";
}

namespace openspace {
namespace globebrowsing {


    //////////////////////////////////////////////////////////////////////////////////////
    //                               Layer Group                                        //
    //////////////////////////////////////////////////////////////////////////////////////

    void LayerGroup::update() {
        _activeLayers.clear();

        for (Layer& layer : layers) {
            if (layer.isActive) {
                layer.tileProvider->update();
                _activeLayers.push_back(layer);
            }
        }
    }

    const std::vector<Layer>& LayerGroup::activeLayers() const {
        return _activeLayers;
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //                           Tile Provider Manager                                  //
    //////////////////////////////////////////////////////////////////////////////////////
    LayerManager::LayerManager(
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
                layerGroups[i].layers,
                texturesDict,
                initData);

            // init level blending to be true
            layerGroups[i].levelBlendingEnabled = true;
        }
    }

    LayerManager::~LayerManager()
    {
    }

    void LayerManager::initTexures(std::vector<Layer>& dest,
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

            TileProvider* tileProvider;
            auto tileProviderFactory = FactoryManager::ref().factory<TileProvider>();
            try {
                tileProvider = tileProviderFactory->create(type, texDict);
            }
            catch (const ghoul::RuntimeError& e) {
                LERROR(e.what());
                continue;
            }

            // Something else went wrong and no exception was thrown
            if (tileProvider == nullptr) {
                LERROR("Unable to create TileProvider '" << name << "' of type '"
                    << type << "'");
                continue;
            }

            bool enabled = false; // defaults to false if unspecified
            texDict.getValue("Enabled", enabled);

            dest.push_back({ name, std::shared_ptr<TileProvider>(tileProvider), enabled });
        }
    }

    LayerGroup& LayerManager::layerGroup(size_t groupId) {
        return layerGroups[groupId];
    }

    LayerGroup& LayerManager::layerGroup(LayeredTextures::TextureCategory category) {
        return layerGroups[category];
    }

    void LayerManager::update() {
        for (LayerGroup& layerGroup : layerGroups) {
            layerGroup.update();
        }
    }

    void LayerManager::reset(bool includingInactive) {
        for (LayerGroup& layerGroup : layerGroups) {
            for (Layer& layer : layerGroup.layers) {
                if (layer.isActive) {
                    layer.tileProvider->reset();
                }
                else if (includingInactive) {
                    layer.tileProvider->reset();
                }
            }
        }
    }

} // namespace globebrowsing
} // namespace openspace
