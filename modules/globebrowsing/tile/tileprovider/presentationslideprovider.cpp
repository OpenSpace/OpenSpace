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

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/tile/tileprovider/presentationslideprovider.h>
#include <modules/globebrowsing/tile/tileindex.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <openspace/engine/openspaceengine.h>

namespace {
    const std::string _loggerCat = "PresentationSlideProvider";
    
    const std::string KeyDefaultProvider = "DefaultProvider";
    const std::string KeySlideProviders = "SlideProviders";
    const std::string KeyTileIndex = "TileIndex";
    const std::string KeyTileProvider = "TileProvider";
}

namespace openspace {
namespace globebrowsing {
namespace tileprovider {

PresentationSlideProvider::PresentationSlideProvider(const ghoul::Dictionary& dictionary)
    : _slideIndex("slideIndex", "slideIndex", 0, 0, _slideProviders.size() - 1)
{
    setName("SlideProvider");
    ghoul::Dictionary defaultProviderDict = dictionary.value<ghoul::Dictionary>(KeyDefaultProvider);
    _defaultProvider = TileProvider::createFromDictionary(defaultProviderDict);

    ghoul::Dictionary tileIndexDict = dictionary.value<ghoul::Dictionary>(KeyTileIndex);
    _tileIndex = TileIndex(tileIndexDict);
        
    ghoul::Dictionary slideProvidersDict = dictionary.value<ghoul::Dictionary>(KeySlideProviders);
    _slideProviders.resize(slideProvidersDict.size());
    for (size_t i = 0; i < slideProvidersDict.size(); i++) {
        std::string dictKey = std::to_string(i + 1);
        ghoul::Dictionary providerDict = slideProvidersDict.value<ghoul::Dictionary>(dictKey);
        _slideProviders[i] = TileProvider::createFromDictionary(providerDict);
    }
        
    _slideIndex.setMaxValue(_slideProviders.size() - 1);
    addProperty(_slideIndex);
}

Tile PresentationSlideProvider::getTile(const TileIndex& tileIndex) {
    if(tileIndex == _tileIndex){
        return slideProvider()->getTile(tileIndex);
    }
    return Tile::TileUnavailable;
        
}

Tile PresentationSlideProvider::getDefaultTile() {
    return _defaultProvider->getDefaultTile();
}

Tile::Status PresentationSlideProvider::getTileStatus(const TileIndex& tileIndex) {
    if(tileIndex == _tileIndex){
        return slideProvider()->getTileStatus(tileIndex);
    }
    return Tile::Status::Unavailable;
}

TileDepthTransform PresentationSlideProvider::depthTransform() {
    return slideProvider()->depthTransform();
}

void PresentationSlideProvider::update() {
    slideProvider()->update();
    _defaultProvider->update();
}

void PresentationSlideProvider::reset() {
    for(auto& tp : _slideProviders){
        tp->reset();
    }
    _defaultProvider->reset();
}

int PresentationSlideProvider::maxLevel() {
    return _defaultProvider->maxLevel();
}

TileProvider* PresentationSlideProvider::slideProvider() {
    int maxIndex = (int)_slideProviders.size() - 1;
    int clampedIndex = std::max(0, std::min(_slideIndex.value(), maxIndex));
    _slideIndex.setValue(clampedIndex);
    return _slideProviders[clampedIndex].get();
}
    
} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace
