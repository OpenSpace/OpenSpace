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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/layeradjustment.h>
#include <modules/globebrowsing/src/layerrendersettings.h>
#include <modules/globebrowsing/src/tileprovider.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>

namespace openspace::globebrowsing {

struct LayerGroup;
struct TileIndex;

namespace tileprovider { struct TileProvider; }

class Layer : public properties::PropertyOwner {
public:
    Layer(layergroupid::GroupID id, const ghoul::Dictionary& layerDict,
        LayerGroup& parent);

    void initialize();
    void deinitialize();

    ChunkTilePile chunkTilePile(const TileIndex& tileIndex, int pileSize) const;
    Tile::Status tileStatus(const TileIndex& index) const;

    layergroupid::TypeID type() const;
    layergroupid::BlendModeID blendMode() const;
    TileDepthTransform depthTransform() const;
    void setEnabled(bool enabled);
    bool enabled() const;
    tileprovider::TileProvider* tileProvider() const;
    glm::vec3 solidColor() const;
    const LayerRenderSettings& renderSettings() const;
    const LayerAdjustment& layerAdjustment() const;

    void onChange(std::function<void(Layer*)> callback);

    // Return:  number of tiles that were updated
    int update();

    glm::ivec2 tilePixelStartOffset() const;
    glm::ivec2 tilePixelSizeDifference() const;
    glm::vec2 tileUvToTextureSamplePosition(const TileUvTransform& uvTransform,
        const glm::vec2& tileUV, const glm::uvec2& resolution);

private:
    void initializeBasedOnType(layergroupid::TypeID typeId, ghoul::Dictionary initDict);
    void addVisibleProperties();

    LayerGroup& _parent;

    properties::OptionProperty _typeOption;
    properties::OptionProperty _blendModeOption;
    properties::BoolProperty _enabled;
    properties::TriggerProperty _reset;
    properties::TriggerProperty _remove;

    layergroupid::TypeID _type;
    std::unique_ptr<tileprovider::TileProvider> _tileProvider;
    properties::Vec3Property _solidColor;
    LayerRenderSettings _renderSettings;
    LayerAdjustment _layerAdjustment;

    glm::ivec2 _padTilePixelStartOffset;
    glm::ivec2 _padTilePixelSizeDifference;

    const layergroupid::GroupID _layerGroupId;

    std::function<void(Layer*)> _onChangeCallback;
  };

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__
