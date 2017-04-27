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

Layer::Layer(const ghoul::Dictionary& layerDict)
    : properties::PropertyOwner(layerDict.value<std::string>("Name"))
    , _enabled(properties::BoolProperty("enabled", "enabled", false))
{
    _tileProvider = std::shared_ptr<tileprovider::TileProvider>(
        tileprovider::TileProvider::createFromDictionary(layerDict));
        
    // Something else went wrong and no exception was thrown
    if (_tileProvider == nullptr) {
        throw ghoul::RuntimeError("Unable to create TileProvider '" + name() + "'");
    }

    bool enabled = false; // defaults to false if unspecified
    layerDict.getValue("Enabled", enabled);
    _enabled.setValue(enabled);
    addProperty(_enabled);

    addPropertySubOwner(_renderSettings);
    addPropertySubOwner(*_tileProvider);
}

ChunkTilePile Layer::getChunkTilePile(const TileIndex& tileIndex, int pileSize) const {
    return _tileProvider->getChunkTilePile(tileIndex, pileSize);
}

} // namespace globebrowsing
} // namespace openspace
