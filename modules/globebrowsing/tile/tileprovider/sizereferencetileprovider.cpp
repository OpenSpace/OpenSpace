/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <openspace/engine/globals.h>
#include <ghoul/fmt.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>

namespace {
    constexpr const char* KeyRadii = "Radii";
} // namespace

namespace openspace::globebrowsing::tileprovider {

SizeReferenceTileProvider::SizeReferenceTileProvider(const ghoul::Dictionary& dictionary)
    : TextTileProvider(
        LayerManager::getTileTextureInitData(
            layergroupid::GroupID::ColorLayers,
            LayerManager::PadTiles::No
        )
    )
{
    _fontSize = 50;
    _font = global::fontManager.font("Mono", static_cast<float>(_fontSize));

    if (dictionary.hasKeyAndValue<glm::dvec3>(KeyRadii)) {
        _ellipsoid = dictionary.value<glm::dvec3>(KeyRadii);
    }
}

void SizeReferenceTileProvider::renderText(
                                   const ghoul::fontrendering::FontRenderer& fontRenderer,
                                                         const TileIndex& tileIndex) const
{
    const GeodeticPatch patch(tileIndex);
    const bool aboveEquator = patch.isNorthern();

    double tileLongitudalLength = roundedLongitudalLength(tileIndex);

    const char* unit;
    if (tileLongitudalLength > 9999) {
        tileLongitudalLength *= 0.001;
        unit = "km";
    }
    else {
        unit = "m";
    }

    const glm::vec2 textPosition = {
        0.f,
        aboveEquator ? _fontSize / 2.f : _initData.dimensions().y - 3.f * _fontSize / 2.f
    };

    fontRenderer.render(
        *_font,
        textPosition,
        fmt::format(" {:.0f} {:s}", tileLongitudalLength, unit),
        { 1.f, 1.f, 1.f, 1.f }
    );
}

int SizeReferenceTileProvider::roundedLongitudalLength(const TileIndex& tileIndex) const {
    const GeodeticPatch patch(tileIndex);
    const bool aboveEquator = patch.isNorthern();
    const double lat = aboveEquator ? patch.minLat() : patch.maxLat();
    const double lon1 = patch.minLon();
    const double lon2 = patch.maxLon();
    int l = static_cast<int>(_ellipsoid.longitudalDistance(lat, lon1, lon2));

    const bool useKm = l > 9999;
    if (useKm) {
        l /= 1000;
    }
    l = static_cast<int>(std::round(l));
    if (useKm) {
        l *= 1000;
    }

    return l;
}

TileIndex::TileHashKey SizeReferenceTileProvider::toHash(const TileIndex& tileIndex) const
{
    const int l = roundedLongitudalLength(tileIndex);
    const TileIndex::TileHashKey key = static_cast<TileIndex::TileHashKey>(l);
    return key;
}

} // namespace openspace::globebrowsing::tileprovider
