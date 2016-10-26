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

    LayerRenderSettings::LayerRenderSettings()
        : opacity(properties::FloatProperty("opacity", "opacity", 1, 0, 1))    
        , gamma(properties::FloatProperty("gamma", "gamma", 1, 0, 5))
        , multiplier(properties::FloatProperty("multiplier", "multiplier", 1, 0, 20))
    {
        setName("settings");
        addProperty(opacity);
        addProperty(gamma);
        addProperty(multiplier);
    }
    
    //////////////////////////////////////////////////////////////////////////////////////
    //                                     Layer                                        //
    //////////////////////////////////////////////////////////////////////////////////////

    Layer::Layer(const ghoul::Dictionary& layerDict)
        : _enabled(properties::BoolProperty("enabled", "enabled", false))
    {
        std::string layerName = "error!";
        layerDict.getValue("Name", layerName);
        setName(layerName);
        
        
        _tileProvider = std::shared_ptr<TileProvider>(TileProvider::createFromDictionary(layerDict));
        
        // Something else went wrong and no exception was thrown
        if (_tileProvider == nullptr) {
            throw ghoul::RuntimeError("Unable to create TileProvider '" + name() + "'");
        }

        bool enabled = false; // defaults to false if unspecified
        layerDict.getValue("Enabled", enabled);
        _enabled.setValue(enabled);
        addProperty(_enabled);

        addPropertySubOwner(_renderSettings);
    }

    Layer::~Layer(){

    }

    ChunkTilePile Layer::getChunkTilePile(const TileIndex& tileIndex, int pileSize) const{
        return std::move(TileSelector::getHighestResolutionTilePile(_tileProvider.get(), tileIndex, pileSize));
    }

    
    //////////////////////////////////////////////////////////////////////////////////////
    //                               Layer Group                                        //
    //////////////////////////////////////////////////////////////////////////////////////

    LayerGroup::LayerGroup(std::string name)
        : _levelBlendingEnabled("blendTileLevels", "blend tile levels", true)
    {
        setName(name);
        addProperty(_levelBlendingEnabled);
    }

    LayerGroup::LayerGroup(std::string name, const ghoul::Dictionary& dict)
        : LayerGroup(name)
    {
        for (size_t i = 0; i < dict.size(); i++) {
            std::string dictKey = std::to_string(i + 1);
            ghoul::Dictionary layerDict = dict.value<ghoul::Dictionary>(dictKey);

            try {
                _layers.push_back(std::make_shared<Layer>(layerDict));
            }
            catch (const ghoul::RuntimeError& e) {
                LERROR(e.what());
                continue;
            }
        }

        for(auto layer : _layers){
            addPropertySubOwner(layer.get());
        }
    }

    void LayerGroup::update() {
        _activeLayers.clear();

        for (auto layer : _layers) {
            if (layer->enabled()) {
                layer->tileProvider()->update();
                _activeLayers.push_back(layer);
            }
        }
    }

    const std::vector<std::shared_ptr<Layer>>& LayerGroup::layers() const {
        return _layers;
    }

    const std::vector<std::shared_ptr<Layer>>& LayerGroup::activeLayers() const {
        return _activeLayers;
    }

    int LayerGroup::pileSize() const{
        return _levelBlendingEnabled.value() ? 3 : 1;
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //                                 LayerManager                                     //
    //////////////////////////////////////////////////////////////////////////////////////

    const std::string LayerManager::LAYER_GROUP_NAMES[NUM_LAYER_GROUPS] = {
        "ColorTextures",
        "GrayScaleTextures",
        "GrayScaleOverlays",
        "NightTextures",
        "WaterMasks",
        "Overlays",
        "HeightMaps",
    };

    LayerManager::LayerManager(const ghoul::Dictionary& layerGroupsDict) {

        setName("Layers");
        
        // Create all the categories of tile providers
        for (size_t i = 0; i < layerGroupsDict.size(); i++) {
            std::string groupName = LayerManager::LAYER_GROUP_NAMES[i];
            ghoul::Dictionary layerGroupDict = layerGroupsDict.value<ghoul::Dictionary>(groupName);

            TileProviderInitData initData;
            initData.minimumPixelSize = 512;
            initData.threads = 1;
            initData.cacheSize = 5000;
            initData.framesUntilRequestQueueFlush = 60;
            initData.preprocessTiles = (i == LayerManager::HeightMaps); // Only preprocess height maps.
            
            _layerGroups.push_back(std::make_shared<LayerGroup>(groupName, layerGroupDict));
        }
        
        for(auto layerGroup : _layerGroups){
            addPropertySubOwner(layerGroup.get());
        }
    }

    LayerManager::~LayerManager(){
        
    }

    LayerGroup& LayerManager::layerGroup(size_t groupId) {
        return *_layerGroups[groupId];
    }

    LayerGroup& LayerManager::layerGroup(LayerGroupId groupId) {
        return *_layerGroups[groupId];
    }

    bool LayerManager::hasAnyBlendingLayersEnabled() const {
        for (const auto& layerGroup : _layerGroups){
            if (layerGroup->layerBlendingEnabled() && layerGroup->activeLayers().size() > 0){
                return true;
            }
        }
        return false;
    }

    const std::vector<std::shared_ptr<LayerGroup>>& LayerManager::layerGroups() const {
        return _layerGroups;
    }
    

    void LayerManager::update() {
        for (auto& layerGroup : _layerGroups) {
            layerGroup->update();
        }
    }

    void LayerManager::reset(bool includeDisabled) {
        for (auto& layerGroup : _layerGroups) {
            for (auto layer : layerGroup->layers()) {
                if (layer->enabled() || includeDisabled) {
                    layer->tileProvider()->reset();
                }
            }
        }
    }

} // namespace globebrowsing
} // namespace openspace
