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

#include <openspace/properties/vectorproperty.h>
#include <openspace/properties/optionproperty.h>

namespace openspace {
namespace globebrowsing {

namespace tileprovider {
    class TileProvider;
}

class Layer : public properties::PropertyOwner {
public:
    Layer(layergroupid::GroupID id, const ghoul::Dictionary& layerDict);

    ChunkTilePile getChunkTilePile(const TileIndex& tileIndex, int pileSize) const;
    Tile::Status getTileStatus(const TileIndex& index) const;

    layergroupid::TypeID type() const {return static_cast<layergroupid::TypeID>(_typeOption.value()); };
    layergroupid::BlendModeID blendMode() const {return static_cast<layergroupid::BlendModeID>(_blendModeOption.value()); };
    TileDepthTransform depthTransform() const;
    bool enabled() const { return _enabled.value(); }
    tileprovider::TileProvider* tileProvider() const { return _tileProvider.get(); }
    const LayerRenderSettings& renderSettings() const { return _renderSettings; }
    
    void onChange(std::function<void(void)> callback);
    
    void update();

    // TODO: Make private and structure adjustment layer variables
    properties::Vec3Property color;
private:
    void initializeBasedOnType(layergroupid::TypeID typeId, ghoul::Dictionary initDict);
    void removeVisibleProperties();
    void addVisibleProperties();
    
    properties::OptionProperty _typeOption;
    properties::OptionProperty _blendModeOption;

    properties::BoolProperty _enabled;
    properties::TriggerProperty _reset;
    std::shared_ptr<tileprovider::TileProvider> _tileProvider;
    LayerRenderSettings _renderSettings;

    const layergroupid::GroupID _layerGroupId;
  
    std::function<void(void)> _onChangeCallback;
  };

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__
