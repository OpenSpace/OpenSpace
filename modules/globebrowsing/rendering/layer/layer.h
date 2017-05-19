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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/rendering/layer/layergroupid.h>
#include <modules/globebrowsing/rendering/layer/layerrendersettings.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/triggerproperty.h>

namespace openspace {
namespace globebrowsing {

namespace tileprovider {
    class TileProvider;
}

/**
 * Simple struct which is used to enable/disable <code>TileProvider</code> 
 * and associate is with a name. It also holds layer specific information
 * which is used in rendering of layer.
 */
class Layer : public properties::PropertyOwner {
public:
    Layer(layergroupid::ID id, const ghoul::Dictionary& layerDict);

    ChunkTilePile getChunkTilePile(const TileIndex& tileIndex, int pileSize) const;

    bool enabled() const { return _enabled.value(); }
    tileprovider::TileProvider* tileProvider() const { return _tileProvider.get(); }
    const LayerRenderSettings& renderSettings() const { return _renderSettings; }

    void onChange(std::function<void(void)> callback);

private:
    properties::BoolProperty _enabled;
    properties::TriggerProperty _reset;
    std::shared_ptr<tileprovider::TileProvider> _tileProvider;
    LayerRenderSettings _renderSettings;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__
