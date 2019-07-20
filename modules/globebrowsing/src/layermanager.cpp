/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/globebrowsing/src/layermanager.h>

#include <modules/globebrowsing/src/layer.h>
#include <modules/globebrowsing/src/layergroup.h>
#include <modules/globebrowsing/src/tileprovider.h>
#include <modules/globebrowsing/src/tiletextureinitdata.h>
#include <ghoul/logging/logmanager.h>

namespace openspace::globebrowsing {

LayerManager::LayerManager() : properties::PropertyOwner({ "Layers" }) {}

void LayerManager::initialize(const ghoul::Dictionary& layerGroupsDict) {
    // First create empty layer groups in case not all are specified
    for (size_t i = 0; i < _layerGroups.size(); ++i) {
        _layerGroups[i] = std::make_unique<LayerGroup>(layergroupid::GroupID(i));
    }

    const std::vector<std::string>& layerGroupNamesInDict = layerGroupsDict.keys();

    // Create all the layer groups
    for (const std::string& groupName : layerGroupNamesInDict) {
        layergroupid::GroupID id = ghoul::from_string<layergroupid::GroupID>(groupName);

        if (id != layergroupid::GroupID::Unknown) {
            ghoul::Dictionary d = layerGroupsDict.value<ghoul::Dictionary>(groupName);
            _layerGroups[static_cast<int>(id)]->setLayersFromDict(d);
        }
        else {
            LWARNINGC("LayerManager", "Unknown layer group: " + groupName);
        }
    }

    for (const std::unique_ptr<LayerGroup>& layerGroup : _layerGroups) {
        addPropertySubOwner(layerGroup.get());
    }

    for (const std::unique_ptr<LayerGroup>& lg : _layerGroups) {
        lg->initialize();
    }
}

void LayerManager::deinitialize() {
    for (const std::unique_ptr<LayerGroup>& lg : _layerGroups) {
        lg->deinitialize();
    }
}

Layer* LayerManager::addLayer(layergroupid::GroupID groupId,
                              const ghoul::Dictionary& layerDict)
{
    ghoul_assert(groupId != layergroupid::Unknown, "Layer group ID must be known");
    try {
        return _layerGroups[groupId]->addLayer(layerDict);
    }
    catch (const ghoul::RuntimeError& e) {
        LERRORC(e.component, e.message);
        return nullptr;
    }
}

void LayerManager::deleteLayer(layergroupid::GroupID id, const std::string& layerName) {
    ghoul_assert(id != layergroupid::Unknown, "Layer group ID must be known");
    _layerGroups[id]->deleteLayer(layerName);
}

const LayerGroup& LayerManager::layerGroup(layergroupid::GroupID groupId) const {
    return *_layerGroups[groupId];
}

bool LayerManager::hasAnyBlendingLayersEnabled() const {
    return std::any_of(
        _layerGroups.begin(),
        _layerGroups.end(),
        [](const std::unique_ptr<LayerGroup>& lg) {
            return lg->layerBlendingEnabled() && !lg->activeLayers().empty();
        }
    );
}

std::array<LayerGroup*, LayerManager::NumLayerGroups> LayerManager::layerGroups() const
{
    std::array<LayerGroup*, NumLayerGroups> res = {};
    for (int i = 0; i < NumLayerGroups; ++i) {
        res[i] = _layerGroups[i].get();
    }
    return res;
}

int LayerManager::update() {
    int res = 0;
    for (std::unique_ptr<LayerGroup>& layerGroup : _layerGroups) {
        res += layerGroup->update();
    }
    return res;
}

void LayerManager::reset(bool includeDisabled) {
    for (std::unique_ptr<LayerGroup>& layerGroup : _layerGroups) {
        for (Layer* layer : layerGroup->layers()) {
            if (layer->enabled() || includeDisabled) {
                tileprovider::reset(*layer->tileProvider());
            }
        }
    }
}

void LayerManager::onChange(std::function<void(Layer*)> callback) {
    for (std::unique_ptr<LayerGroup>& layerGroup : _layerGroups) {
        layerGroup->onChange(callback);
    }
}

} // namespace openspace::globebrowsing
