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
#include <modules/globebrowsing/globes/chunkedlodglobe.h>

namespace openspace {
namespace globebrowsing {

namespace {
	const char* _loggerCat = "LayerManager";
}

LayerManager::LayerManager(const ghoul::Dictionary& layerGroupsDict)
    : properties::PropertyOwner("Layers")
{
	// First create empty layer groups in case not all are specified
    _layerGroups.resize(layergroupid::NUM_LAYER_GROUPS);
    for (int i = 0; i < _layerGroups.size(); ++i) {
		ghoul::Dictionary emptyDict;
        _layerGroups[i] = std::make_shared<LayerGroup>(
			static_cast<layergroupid::GroupID>(i), emptyDict
		);
    }

    std::vector<std::string> layerGroupNamesInDict = layerGroupsDict.keys();

    // Create all the layer groups
    for (const std::string groupName : layerGroupNamesInDict) {

        layergroupid::GroupID groupId = layergroupid::getGroupIDFromName(groupName);
        
        if (groupId != layergroupid::GroupID::Unknown) {
            ghoul::Dictionary layerGroupDict =
                layerGroupsDict.value<ghoul::Dictionary>(groupName);
			_layerGroups[static_cast<int>(groupId)] =
				std::make_shared<LayerGroup>(groupId, layerGroupDict);
        }
        else {
            LWARNING("Unknown layer group: " + groupName);
        }
    }
        
    for (const std::shared_ptr<LayerGroup>& layerGroup : _layerGroups) {
        addPropertySubOwner(layerGroup.get());
    }
}

void LayerManager::addLayer(layergroupid::GroupID groupId, ghoul::Dictionary layerDict) {
    ghoul_assert(groupId != layergroupid::Unknown, "Layer group ID must be known");
    _layerGroups[groupId]->addLayer(layerDict);
}

void LayerManager::deleteLayer(layergroupid::GroupID groupId, std::string layerName) {
    ghoul_assert(groupId != layergroupid::Unknown, "Layer group ID must be known");
    _layerGroups[groupId]->deleteLayer(layerName);
}

const LayerGroup& LayerManager::layerGroup(size_t groupId) {
    return *_layerGroups[groupId];
}

const LayerGroup& LayerManager::layerGroup(layergroupid::GroupID groupId) {
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
    for (std::shared_ptr<LayerGroup>& layerGroup : _layerGroups) {
        layerGroup->update();
    }
}

void LayerManager::reset(bool includeDisabled) {
    for (std::shared_ptr<LayerGroup>& layerGroup : _layerGroups) {
        for (std::shared_ptr<Layer> layer : layerGroup->layers()) {
            if (layer->enabled() || includeDisabled) {
                layer->tileProvider()->reset();
            }
        }
    }
}

TileTextureInitData LayerManager::getTileTextureInitData(layergroupid::GroupID id,
    size_t preferredTileSize)
{
    switch (id) {
        case layergroupid::GroupID::HeightLayers: {
            size_t tileSize = preferredTileSize ? preferredTileSize : 64;
            return TileTextureInitData(tileSize, tileSize, GL_FLOAT,
                ghoul::opengl::Texture::Format::Red,
                TileTextureInitData::ShouldAllocateDataOnCPU::Yes);
        }
        case layergroupid::GroupID::ColorLayers: {
            size_t tileSize = preferredTileSize ? preferredTileSize : 512;
            return TileTextureInitData(tileSize, tileSize, GL_UNSIGNED_BYTE,
                ghoul::opengl::Texture::Format::BGRA);
        }
        case layergroupid::GroupID::Overlays: {
            size_t tileSize = preferredTileSize ? preferredTileSize : 512;
            return TileTextureInitData(tileSize, tileSize, GL_UNSIGNED_BYTE,
                ghoul::opengl::Texture::Format::BGRA);
        }
        case layergroupid::GroupID::NightLayers: {
            size_t tileSize = preferredTileSize ? preferredTileSize : 512;
            return TileTextureInitData(tileSize, tileSize, GL_UNSIGNED_BYTE,
                ghoul::opengl::Texture::Format::BGRA);
        }
        case layergroupid::GroupID::WaterMasks: {
            size_t tileSize = preferredTileSize ? preferredTileSize : 512;
            return TileTextureInitData(tileSize, tileSize, GL_UNSIGNED_BYTE,
                ghoul::opengl::Texture::Format::BGRA);
        }
        default: {
            ghoul_assert(false, "Unknown layer group ID");
        }
    }
}

bool LayerManager::shouldPerformPreProcessingOnLayergroup(layergroupid::GroupID id) {
    // Only preprocess height layers by default
    switch (id) {
        case layergroupid::GroupID::HeightLayers: return true;
        default: return false;
    }
}

void LayerManager::onChange(std::function<void(void)> callback) {
    for (std::shared_ptr<LayerGroup>& layerGroup : _layerGroups) {
        layerGroup->onChange(callback);
    }
}

} // namespace globebrowsing
} // namespace openspace
