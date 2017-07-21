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

#include <modules/globebrowsing/rendering/layer/layergroup.h>

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace {
    const char* _loggerCat = "LayerGroup";
}

namespace openspace::globebrowsing {

LayerGroup::LayerGroup(layergroupid::GroupID id)
    : properties::PropertyOwner(std::move(layergroupid::LAYER_GROUP_NAMES[id]))
    , _groupId(id)
    , _levelBlendingEnabled("blendTileLevels", "blend tile levels", false)
{
    addProperty(_levelBlendingEnabled);
}

LayerGroup::LayerGroup(layergroupid::GroupID id, const ghoul::Dictionary& dict)
    : LayerGroup(id)
{
    for (size_t i = 0; i < dict.size(); i++) {
        std::string dictKey = std::to_string(i + 1);
        ghoul::Dictionary layerDict = dict.value<ghoul::Dictionary>(dictKey);

        try {
            addLayer(layerDict);
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
            continue;
        }
    }
}

void LayerGroup::update() {
    _activeLayers.clear();

    for (const auto& layer : _layers) {
        if (layer->enabled()) {
            layer->update();
            _activeLayers.push_back(layer);
        }
    }
}

void LayerGroup::addLayer(const ghoul::Dictionary& layerDict) {
    if (!layerDict.hasKeyAndValue<std::string>("Name")) {
        LERROR("'Name' must be specified for layer.");
        return;
    }
    auto layer = std::make_shared<Layer>(_groupId, layerDict);
    layer->onChange(_onChangeCallback);
    if (hasPropertySubOwner(layer->name())) {
        LINFO("Layer with name " + layer->name() + " already exists.");
    }
    else {
        _layers.push_back(layer);
        update();
        if(_onChangeCallback) {
            _onChangeCallback();
        }
        addPropertySubOwner(layer.get());
    }
}

void LayerGroup::deleteLayer(const std::string& layerName) {
    for (std::vector<std::shared_ptr<Layer>>::iterator it = _layers.begin(); it != _layers.end(); ++it) {
        if (it->get()->name() == layerName) {
            removePropertySubOwner(it->get());
            _layers.erase(it);
            update();
            if(_onChangeCallback) {
                _onChangeCallback();
            }
            LINFO("Deleted layer " + layerName);
            return;
        }
    }
    LERROR("Could not find layer " + layerName);
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

void LayerGroup::onChange(std::function<void(void)> callback) {
    _onChangeCallback = std::move(callback);
    _levelBlendingEnabled.onChange(_onChangeCallback);
    for (const std::shared_ptr<Layer>& layer : _layers) {
        layer->onChange(_onChangeCallback);
    }
}

} // namespace openspace::globebrowsing
