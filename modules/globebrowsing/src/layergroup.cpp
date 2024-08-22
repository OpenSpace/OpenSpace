/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/globebrowsing/src/layergroup.h>

#include <modules/globebrowsing/src/layer.h>
#include <modules/globebrowsing/src/layermanager.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace {
    constexpr std::string_view _loggerCat = "LayerGroup";

    constexpr openspace::properties::Property::PropertyInfo BlendTileInfo = {
        "BlendTileLevels",
        "Blend between levels",
        "If this value is enabled, images between different levels are interpolated, "
        "rather than switching between levels abruptly. This makes transitions smoother "
        "and more visually pleasing.",
        openspace::properties::Property::Visibility::User
    };
} // namespace

namespace openspace::globebrowsing {

LayerGroup::LayerGroup(layers::Group group)
    : properties::PropertyOwner({ std::string(group.identifier), std::string(group.name)})
    , _groupId(group.id)
    , _levelBlendingEnabled(BlendTileInfo, true)
{
    addProperty(_levelBlendingEnabled);
}

void LayerGroup::setLayersFromDict(const ghoul::Dictionary& dict) {
    for (size_t i = 1; i <= dict.size(); i++) {
        const ghoul::Dictionary layer = dict.value<ghoul::Dictionary>(std::to_string(i));

        try {
            addLayer(layer);
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    }
}

void LayerGroup::initialize() {
    ZoneScoped;

    for (const std::unique_ptr<Layer>& l : _layers) {
        l->initialize();
    }
}

void LayerGroup::deinitialize() {
    ZoneScoped;

    for (const std::unique_ptr<Layer>& l : _layers) {
        l->deinitialize();
    }
}

void LayerGroup::update() {
    ZoneScoped;

    for (const std::string& layer : _layersToDelete) {
        deleteLayer(layer);
    }
    _layersToDelete.clear();

    _activeLayers.clear();

    for (const std::unique_ptr<Layer>& layer : _layers) {
        if (layer->enabled() && layer->isInitialized()) {
            layer->update();
            _activeLayers.push_back(layer.get());
        }
    }
}

Layer* LayerGroup::addLayer(const ghoul::Dictionary& layerDict) {
    ZoneScoped;

    const documentation::TestResult res = documentation::testSpecification(
        Layer::Documentation(),
        layerDict
    );
    if (!res.success) {
        LERROR("Error adding layer. " + ghoul::to_string(res));
    }

    if (!layerDict.hasValue<std::string>("Identifier")) {
        LERROR("'Identifier' must be specified for layer");
        return nullptr;
    }
    const std::string identifier = layerDict.value<std::string>("Identifier");
    if (hasPropertySubOwner(identifier)) {
        LINFO("Layer with identifier '" + identifier + "' already exists");
        _levelBlendingEnabled.setVisibility(properties::Property::Visibility::User);
        return nullptr;
    }

    std::unique_ptr<Layer> layer = std::make_unique<Layer>(_groupId, layerDict, *this);
    layer->onChange(_onChangeCallback);
    Layer* ptr = layer.get();
    _layers.push_back(std::move(layer));

    // Re-sort the layer list according to the z-index. Using the stable_sort here is
    // important as we need to keep the same order as it already exists _within_ each
    // z-index grouping
    std::stable_sort(
        _layers.begin(),
        _layers.end(),
        [](const std::unique_ptr<Layer>& a, const std::unique_ptr<Layer>& b) {
            return a->zIndex() < b->zIndex();
        }
    );

    update();
    if (_onChangeCallback) {
        _onChangeCallback(ptr);
    }
    addPropertySubOwner(ptr);

    // Re-sort the SubPropertyOwner list according to the z-index so the list in the GUI
    // is sorted correctly. Using the stable_sort here is important as we need to keep the
    // same order as it already exists _within_ each z-index grouping
    // @TODO (malej, 2024-05-07): Add event so the GUI can be aware that it needs to
    // reload all or parts of itself. Right now it is up the caller to reload it if needed
    auto compareZIndexSubOwners = [](const PropertyOwner* a, const PropertyOwner* b) {
        const Layer* aLayer = dynamic_cast<const Layer*>(a);
        const Layer* bLayer = dynamic_cast<const Layer*>(b);
        ghoul_assert(aLayer, "a is not a layer");
        ghoul_assert(bLayer, "b is not a layer");

        return aLayer->zIndex() < bLayer->zIndex();
    };
    std::stable_sort(_subOwners.begin(), _subOwners.end(), compareZIndexSubOwners);

    _levelBlendingEnabled.setVisibility(properties::Property::Visibility::User);
    return ptr;
}

void LayerGroup::deleteLayer(const std::string& layerName) {
    for (std::vector<std::unique_ptr<Layer>>::iterator it = _layers.begin();
         it != _layers.end();
         it++)
    {
        if (it->get()->identifier() == layerName) {
            // we need to make a copy as the layername is only a reference
            // which will no longer be valid once it is deleted
            removePropertySubOwner(it->get());
            (*it)->deinitialize();
            properties::PropertyOwner* layerGroup = it->get()->owner();
            properties::PropertyOwner* layerManager = layerGroup->owner();
            properties::PropertyOwner* globe = layerManager->owner();
            properties::PropertyOwner* sceneGraphNode = globe->owner();

            // We need to keep the name of the layer since we only get it as a reference
            // and the name needs to survive the deletion
            const std::string lName = layerName;
            _layers.erase(it);
            update();
            if (_onChangeCallback) {
                _onChangeCallback(nullptr);
            }
            LINFO(std::format("Deleted layer {}", lName));

            if (_layers.empty()) {
                _levelBlendingEnabled.setVisibility(
                    properties::Property::Visibility::Hidden
                );
            }
            return;
        }
    }
    LERROR("Could not find layer " + layerName);
}

void LayerGroup::scheduleDeleteLayer(const std::string& layerName) {
    _layersToDelete.push_back(layerName);
}

void LayerGroup::moveLayer(int oldPosition, int newPosition) {
    if (_layers.size() == 1) {
        ghoul_assert(
            oldPosition == newPosition,
            "There is only one item but different positions"
        );

        // If there is only one item in the list, there is no need to move anything, but
        // we still want to fix the layer index
        _layers[oldPosition]->setZIndex(1);
        return;
    }

    oldPosition = std::max(0, oldPosition);
    newPosition = std::min(newPosition, static_cast<int>(_layers.size() - 1));

    // There are two synchronous vectors that we have to update here. The _layers vector
    // is used to determine the order while rendering, the _subOwners is the order in
    // which the layers are shown in the UI

    // Layer list
    auto oldLayerPos = _layers.begin() + oldPosition;
    std::unique_ptr<Layer> layer = std::move(*oldLayerPos);
    _layers.erase(oldLayerPos);
    auto newLayerPos = _layers.begin() + newPosition;

    // There is a separate check at the top of the function to ensure that there is more
    // than one item
    ghoul_assert(!_layers.empty(), "The list should not be empty at this point");
    if (newLayerPos == _layers.begin()) {
        // If the layer is moved to the first spot in the list
        Layer* nextLayer = (_layers.begin() + 1)->get();
        layer->setZIndex(nextLayer->zIndex());
    }
    else if (newLayerPos == _layers.end()) {
        // If the layer is moved to the last spot in the list, we use the previously last
        // layer's z-index
        unsigned int zIndex = _layers.back()->zIndex();
        layer->setZIndex(zIndex);
    }
    else {
        // If the layer is moved to somewhere in the middle, we use the z-index of the
        // layer that is currently in that location
        unsigned int zIndex = (*newLayerPos)->zIndex();
        layer->setZIndex(zIndex);
    }

    _layers.insert(newLayerPos, std::move(layer));
    ghoul_assert(
        std::is_sorted(
            _layers.begin(),
            _layers.end(),
            [](const std::unique_ptr<Layer>& a, const std::unique_ptr<Layer>& b) {
                return a->zIndex() < b->zIndex();
            }
        ),
        "Layer list is not sorted after move"
    );

    // SubOwners list
    auto oldPosOwner = _subOwners.begin() + oldPosition;
    PropertyOwner* owner = *oldPosOwner;
    _subOwners.erase(oldPosOwner);
    auto newPosOwner = _subOwners.begin() + newPosition;
    _subOwners.insert(newPosOwner, owner);

    // If we change the order of the layers, we need to inform the RenderableGlobe's
    // shader as it might no longer be valid. This can happen if we change the order of
    // two layers with different types and the uniforms between the two types are not
    // compatible
    LayerManager* manager = dynamic_cast<LayerManager*>(_owner);
    ghoul_assert(manager, "Hierarchy error: Layer. Owner is not LayerManager");
    RenderableGlobe* renderable = dynamic_cast<RenderableGlobe*>(manager->owner());
    ghoul_assert(manager, "Hierarchy error: LayerManager. Owner is not RenderableGlobe");
    renderable->invalidateShader();

    // Notify that the layers are in a different order
    global::eventEngine->publishEvent<events::EventPropertyTreeUpdated>(uri());
}

std::vector<Layer*> LayerGroup::layers() const {
    std::vector<Layer*> res;
    res.reserve(_layers.size());
    for (const std::unique_ptr<Layer>& layer : _layers) {
        res.push_back(layer.get());
    }
    return res;
}

const std::vector<Layer*>& LayerGroup::activeLayers() const {
    return _activeLayers;
}

int LayerGroup::pileSize() const {
    return _levelBlendingEnabled ? 3 : 1;
}

bool LayerGroup::layerBlendingEnabled() const {
    return _levelBlendingEnabled;
}

void LayerGroup::onChange(std::function<void(Layer*)> callback) {
    _onChangeCallback = std::move(callback);
    _levelBlendingEnabled.onChange([this]() {_onChangeCallback(nullptr); });
    for (const std::unique_ptr<Layer>& layer : _layers) {
        layer->onChange(_onChangeCallback);
    }
}

bool LayerGroup::isHeightLayer() const {
    return _groupId == layers::Group::ID::HeightLayers;
}

} // namespace openspace::globebrowsing
