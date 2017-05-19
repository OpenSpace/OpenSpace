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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___PRESENTATION_SLIDE_PROVIDER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___PRESENTATION_SLIDE_PROVIDER___H__

#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

#include <openspace/properties/scalarproperty.h>

#include <memory>
#include <unordered_map>

namespace openspace {
namespace globebrowsing {
namespace tileprovider {

class PresentationSlideProvider : public TileProvider {
public:
    PresentationSlideProvider(const ghoul::Dictionary& dictionary);
    PresentationSlideProvider(const std::string& imagePath);
    virtual ~PresentationSlideProvider() { }

    virtual Tile getTile(const TileIndex& tileIndex);
    virtual Tile getDefaultTile();
    virtual Tile::Status getTileStatus(const TileIndex& index);
    virtual TileDepthTransform depthTransform();
    virtual void update();
    virtual void reset();
    virtual int maxLevel();

private:
    TileProvider* slideProvider();

    TileIndex _tileIndex;
    properties::IntProperty _slideIndex;
    std::vector<std::unique_ptr<TileProvider>> _slideProviders;
    std::unique_ptr<TileProvider> _defaultProvider;
};

} // namespace tileprovider
} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___PRESENTATION_SLIDE_PROVIDER___H__
