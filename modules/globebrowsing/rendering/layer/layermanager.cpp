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

#include <modules/globebrowsing/rendering/layer/layermanager.h>

#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace openspace {
namespace globebrowsing {

const char* LayerManager::LAYER_GROUP_NAMES[NUM_LAYER_GROUPS] = {
    "HeightLayers",
    "ColorLayers",
    "ColorOverlays",
    "GrayScaleLayers",
    "GrayScaleColorOverlays",
    "NightLayers",
    "WaterMasks"
};

LayerManager::LayerManager(const ghoul::Dictionary& layerGroupsDict) 
    : properties::PropertyOwner("Layers")
{
    if (NUM_LAYER_GROUPS != layerGroupsDict.size()) {
        throw ghoul::RuntimeError(
            "Number of Layer Groups must be equal to " + NUM_LAYER_GROUPS);
    }

    // Create all the categories of tile providers
    for (size_t i = 0; i < layerGroupsDict.size(); i++) {
        std::string groupName = LayerManager::LAYER_GROUP_NAMES[i];
        ghoul::Dictionary layerGroupDict = 
            layerGroupsDict.value<ghoul::Dictionary>(groupName);

        _layerGroups.push_back(
            std::make_shared<LayerGroup>(groupName, layerGroupDict));
    }
        
    for (const auto& layerGroup : _layerGroups) {
        addPropertySubOwner(layerGroup.get());
    }
}

const LayerGroup& LayerManager::layerGroup(size_t groupId) {
    return *_layerGroups[groupId];
}

const LayerGroup& LayerManager::layerGroup(LayerGroupId groupId) {
    return *_layerGroups[groupId];
}

bool LayerManager::hasAnyBlendingLayersEnabled() const {
    for (const auto& layerGroup : _layerGroups) {
        if (layerGroup->layerBlendingEnabled() && layerGroup->activeLayers().size() > 0) {
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
