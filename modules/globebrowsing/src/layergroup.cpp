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

#include <modules/globebrowsing/src/layergroup.h>

#include <modules/globebrowsing/src/layer.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "LayerGroup";
    constexpr const char* KeyFallback = "Fallback";

    constexpr openspace::properties::Property::PropertyInfo BlendTileInfo = {
        "BlendTileLevels",
        "Blend between levels",
        "If this value is enabled, images between different levels are interpolated, "
        "rather than switching between levels abruptly. This makes transitions smoother "
        "and more visually pleasing.",
        openspace::properties::Property::Visibility::Hidden
    };
} // namespace

namespace openspace::globebrowsing {

LayerGroup::LayerGroup(layergroupid::GroupID id)
    : properties::PropertyOwner({
        layergroupid::LAYER_GROUP_IDENTIFIERS[id],
        layergroupid::LAYER_GROUP_NAMES[id]
    })
    , _groupId(id)
    , _levelBlendingEnabled(BlendTileInfo, true)
{
    addProperty(_levelBlendingEnabled);
}

void LayerGroup::setLayersFromDict(const ghoul::Dictionary& dict) {
    for (size_t i = 1; i <= dict.size(); i++) {
        ghoul::Dictionary layerDict = dict.value<ghoul::Dictionary>(std::to_string(i));

        try {
            addLayer(layerDict);
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);

            if (layerDict.hasKeyAndValue<ghoul::Dictionary>(KeyFallback)) {
                LWARNING("Unable to create layer. Initializing fallback layer.");
                ghoul::Dictionary fallbackLayerDict =
                    layerDict.value<ghoul::Dictionary>(KeyFallback);
                try {
                    addLayer(fallbackLayerDict);
                }
                catch (const ghoul::RuntimeError& except) {
                    LERRORC(except.component, except.message);
                }
            }
        }
    }
}

void LayerGroup::initialize() {
    for (const std::unique_ptr<Layer>& l : _layers) {
        l->initialize();
    }
}

void LayerGroup::deinitialize() {
    for (const std::unique_ptr<Layer>& l : _layers) {
        l->deinitialize();
    }
}

int LayerGroup::update() {
    int res = 0;
    _activeLayers.clear();

    for (const std::unique_ptr<Layer>& layer : _layers) {
        if (layer->enabled()) {
            res += layer->update();
            _activeLayers.push_back(layer.get());
        }
    }

    return res;
}

Layer* LayerGroup::addLayer(const ghoul::Dictionary& layerDict) {
    if (!layerDict.hasKeyAndValue<std::string>("Identifier")) {
        LERROR("'Identifier' must be specified for layer.");
        return nullptr;
    }
    std::unique_ptr<Layer> layer = std::make_unique<Layer>(_groupId, layerDict, *this);
    layer->onChange(_onChangeCallback);
    if (hasPropertySubOwner(layer->identifier())) {
        LINFO("Layer with identifier " + layer->identifier() + " already exists.");
        _levelBlendingEnabled.setVisibility(properties::Property::Visibility::User);
        return nullptr;
    }

    Layer* ptr = layer.get();
    _layers.push_back(std::move(layer));
    update();
    if (_onChangeCallback) {
        _onChangeCallback(ptr);
    }
    addPropertySubOwner(ptr);
    _levelBlendingEnabled.setVisibility(properties::Property::Visibility::User);
    return ptr;
}

void LayerGroup::deleteLayer(const std::string& layerName) {
    for (std::vector<std::unique_ptr<Layer>>::iterator it = _layers.begin();
         it != _layers.end();
         ++it)
    {
        if (it->get()->identifier() == layerName) {
            removePropertySubOwner(it->get());
            (*it)->deinitialize();
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

} // namespace openspace::globebrowsing
