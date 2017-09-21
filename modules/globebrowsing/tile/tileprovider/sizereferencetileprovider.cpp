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

#include <modules/globebrowsing/tile/tileprovider/sizereferencetileprovider.h>

#include <modules/globebrowsing/geometry/geodeticpatch.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/rendering/layer/layergroupid.h>

#include <openspace/engine/openspaceengine.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/misc/dictionary.h>

namespace {
    const char* KeyRadii = "Radii";
} // namespace

namespace openspace::globebrowsing::tileprovider {

SizeReferenceTileProvider::SizeReferenceTileProvider(const ghoul::Dictionary& dictionary)
    : TextTileProvider(
        LayerManager::getTileTextureInitData(layergroupid::GroupID::ColorLayers, false))
    , _backgroundTile(Tile::TileUnavailable)
{
    _fontSize = 50;
    _font = OsEng.fontManager().font("Mono", static_cast<float>(_fontSize));

    glm::dvec3 radii(1,1,1);
    dictionary.getValue(KeyRadii, radii);
    _ellipsoid = Ellipsoid(radii);
}

void SizeReferenceTileProvider::renderText(const ghoul::fontrendering::FontRenderer&
                                           fontRenderer,
                                           const TileIndex& tileIndex) const
{
    GeodeticPatch patch(tileIndex);
    bool aboveEquator = patch.isNorthern();
        
    double tileLongitudalLength = roundedLongitudalLength(tileIndex);

    std::string unit = "m";
    if (tileLongitudalLength > 9999) {
        tileLongitudalLength *= 0.001;
        unit = "km";
    }

    glm::vec2 textPosition;
    textPosition.x = 0;
    textPosition.y = aboveEquator ?
        _fontSize / 2.f :
        _initData.dimensions().y - 3.f * _fontSize / 2.f;
    glm::vec4 color(1.0, 1.0, 1.0, 1.0);

    fontRenderer.render(
        *_font,
        textPosition,
        color,
        " %.0f %s",
        tileLongitudalLength, unit.c_str()
    );
}

int SizeReferenceTileProvider::roundedLongitudalLength(const TileIndex& tileIndex) const {
    GeodeticPatch patch(tileIndex);
    bool aboveEquator = patch.isNorthern();
    double lat = aboveEquator ? patch.minLat() : patch.maxLat();
    double lon1 = patch.minLon();
    double lon2 = patch.maxLon();
    int l = static_cast<int>(_ellipsoid.longitudalDistance(lat, lon1, lon2));

    bool useKm = l > 9999;
    if (useKm) {
        l /= 1000;
    }
    l = static_cast<int>(std::round(l));
    if (useKm) {
        l *= 1000;
    }

    return l;
}

TileIndex::TileHashKey SizeReferenceTileProvider::toHash(const TileIndex& tileIndex) const {
    int l = roundedLongitudalLength(tileIndex);
    TileIndex::TileHashKey key = static_cast<TileIndex::TileHashKey>(l);
    return key;
}

} // namespace openspace::globebrowsing::tileprovider
