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

    // Re-sort the layer list according to the z-index
    auto compareZIndex = [](const std::unique_ptr<Layer>& a,
                            const std::unique_ptr<Layer>& b)
    {
        return a->zIndex() < b->zIndex();
    };
    std::stable_sort(_layers.begin(), _layers.end(), compareZIndex);

    update();
    if (_onChangeCallback) {
        _onChangeCallback(ptr);
    }
    addPropertySubOwner(ptr);

    // Re-sort the SubPropertyOwner list according to the z-index so the list in the GUI
    // is sorted correctly.
    // TODO (malej 2024-MAY-07): Add event so the GUI can be aware that it needs to reload
    // all or parts of itself. Right now it is up the caller to reload it if needed.
    auto compareZIndexSubOwners = [](const PropertyOwner* a, const PropertyOwner* b) {
        const Layer* aLayer = dynamic_cast<const Layer*>(a);
        const Layer* bLayer = dynamic_cast<const Layer*>(b);
        ghoul_assert(aLayer, "a is not a layer");
        ghoul_assert(bLayer, "b is not a layer");

        return aLayer->zIndex() < bLayer->zIndex();
    };
    std::stable_sort(_subOwners.begin(), _subOwners.end(), compareZIndexSubOwners);

    _levelBlendingEnabled.setVisibility(properties::Property::Visibility::User);

    properties::PropertyOwner* layerGroup = ptr->owner();
    properties::PropertyOwner* layerManager = layerGroup->owner();

    // @TODO (emmbr, 2021-11-03) If the layer is added as part of the globe's
    // dictionary during construction this function is called in the LayerManager's
    // initialize function. This means that the layerManager does not exists yet, and
    // we cannot find which SGN it belongs to... Want to avoid doing this check, so
    // this should be fixed (probably as part of a cleanup/rewite of the LayerManager)
    if (!layerManager) {
        global::eventEngine->publishEvent<events::EventLayerAdded>(
            "", // we don't know this yet
            layerGroup->identifier(),
            ptr->identifier()
        );
    }
    else {
        properties::PropertyOwner* globe = layerManager->owner();
        properties::PropertyOwner* sceneGraphNode = globe->owner();
        global::eventEngine->publishEvent<events::EventLayerAdded>(
            sceneGraphNode->identifier(),
            layerGroup->identifier(),
            ptr->identifier()
        );
    }

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
            global::eventEngine->publishEvent<events::EventLayerRemoved>(
                sceneGraphNode->identifier(),
                layerGroup->identifier(),
                it->get()->identifier()
            );
            _layers.erase(it);
            update();
            if (_onChangeCallback) {
                _onChangeCallback(nullptr);
            }
            LINFO("Deleted layer " + layerName);

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

void LayerGroup::moveLayer(int oldPosition, int newPosition) {
    oldPosition = std::max(0, oldPosition);
    newPosition = std::min(newPosition, static_cast<int>(_layers.size() - 1));

    // There are two synchronous vectors that we have to update here. The _layers vector
    // is used to determine the order while rendering, the _subOwners is the order in
    // which the layers are shown in the UI

    // Layer list
    auto oldPosLayers = _layers.begin() + oldPosition;
    std::unique_ptr<Layer> layer = std::move(*oldPosLayers);
    _layers.erase(oldPosLayers);
    auto newPosLayers = _layers.begin() + newPosition;

    // Since this layer has now manually been moved, it now is considered to have been
    // given a z-index by the user
    layer->setHasGivenZIndex(true);

    // The z-index of the layer just before the new postion the layer is moved into
    int prevZIndex;

    // Check if the layer is moved to the last spot in the list
    if (newPosLayers < _layers.end()) {
        prevZIndex = newPosLayers->get()->zIndex();
    }
    else {
        // In that case use the value for the last item to not go beyond the list
        prevZIndex = (_layers.end() - 1)->get()->zIndex();
    }

    // Check if the layer is moved to the first spot in the list
    if (newPosition == 0) {
        // Check if there is an item afterwards
        auto nextLayer = _layers.begin() + 1;
        if (nextLayer < _layers.end()) {
            // Take the same value as the next item in the list and rely on the
            // stable_sort to keep the order
            layer->setZIndex(nextLayer->get()->zIndex());
        }
        else {
            // There are no next layer in the list so put the item first
            layer->setZIndex(1);
        }
    }
    // Otherwise take the same value for the z-index as what already exist before and
    // rely on the stable_sort to keep the order
    else {
        layer->setZIndex(prevZIndex);
    }

    _layers.insert(newPosLayers, std::move(layer));
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
