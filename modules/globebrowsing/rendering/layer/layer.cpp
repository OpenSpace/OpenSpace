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

#include <modules/globebrowsing/rendering/layer/layer.h>

#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace openspace {
namespace globebrowsing {

namespace {
    const char* keyName = "Name";
    const char* keyEnabled = "Enabled";
    const char* keyLayerGroupID = "LayerGroupID";
    const char* keySettings = "Settings";
}

Layer::Layer(layergroupid::ID id, const ghoul::Dictionary& layerDict)
    : properties::PropertyOwner(layerDict.value<std::string>(keyName))
    , _enabled(properties::BoolProperty("enabled", "Enabled", false))
    , _reset("reset", "Reset")
{
    // We add the id to the dictionary since it needs to be known by
    // the tile provider
    ghoul::Dictionary newLayerDict = layerDict;
    newLayerDict.setValue(keyLayerGroupID, id);
  
    _tileProvider = std::shared_ptr<tileprovider::TileProvider>(
        tileprovider::TileProvider::createFromDictionary(newLayerDict));
        
    // Something else went wrong and no exception was thrown
    if (_tileProvider == nullptr) {
        throw ghoul::RuntimeError("Unable to create TileProvider '" + name() + "'");
    }

    bool enabled = false; // defaults to false if unspecified
    layerDict.getValue(keyEnabled, enabled);
    _enabled.setValue(enabled);

    ghoul::Dictionary settingsDict;
    if (layerDict.getValue(keySettings, settingsDict)) {
        _renderSettings.setValuesFromDictionary(settingsDict);
    }
    if (id == layergroupid::ID::GrayScaleColorOverlays) {
        _renderSettings.addProperty(_renderSettings.valueBlending);
        _renderSettings.useValueBlending = true;
    }

    _reset.onChange([&](){
        _tileProvider->reset();
    });

    addProperty(_enabled);
    addProperty(_reset);

    addPropertySubOwner(_renderSettings);
    addPropertySubOwner(*_tileProvider);
}

ChunkTilePile Layer::getChunkTilePile(const TileIndex& tileIndex, int pileSize) const {
    return _tileProvider->getChunkTilePile(tileIndex, pileSize);
}

void Layer::onChange(std::function<void(void)> callback) {
    _enabled.onChange(callback);
}

} // namespace globebrowsing
} // namespace openspace
