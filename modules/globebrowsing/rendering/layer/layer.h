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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__

#include <openspace/properties/propertyowner.h>

#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/tile/tile.h>
#include <modules/globebrowsing/rendering/layer/layeradjustment.h>
#include <modules/globebrowsing/rendering/layer/layerrendersettings.h>

#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>

namespace openspace::globebrowsing {

struct LayerGroup;
struct TileIndex;
struct TileDepthTransform;

namespace tileprovider { class TileProvider; }

class Layer : public properties::PropertyOwner {
public:
    /**
     * Properties used when the layer type is not a tile type layer. These properties
     * can be added or removed depending on the layer type.
     */
    struct OtherTypesProperties {
        properties::Vec3Property color;
    };

    Layer(layergroupid::GroupID id, const ghoul::Dictionary& layerDict,
        LayerGroup& parent);

    void initialize();
    void deinitialize();

    ChunkTilePile chunkTilePile(const TileIndex& tileIndex, int pileSize) const;
    Tile::Status tileStatus(const TileIndex& index) const;

    layergroupid::TypeID type() const;
    layergroupid::BlendModeID blendMode() const;
    TileDepthTransform depthTransform() const;
    bool enabled() const;
    tileprovider::TileProvider* tileProvider() const;
    const OtherTypesProperties& otherTypesProperties() const;
    const LayerRenderSettings& renderSettings() const;
    const LayerAdjustment& layerAdjustment() const;

    void onChange(std::function<void(void)> callback);

    void update();

    glm::ivec2 tilePixelStartOffset() const;
    glm::ivec2 tilePixelSizeDifference() const;
    glm::vec2 compensateSourceTextureSampling(glm::vec2 startOffset, glm::vec2 sizeDiff,
        glm::uvec2 resolution, glm::vec2 tileUV);
    glm::vec2 TileUvToTextureSamplePosition(const TileUvTransform& uvTransform,
        glm::vec2 tileUV, glm::uvec2 resolution);

private:
    layergroupid::TypeID parseTypeIdFromDictionary(
        const ghoul::Dictionary& initDict) const;

    void initializeBasedOnType(layergroupid::TypeID typeId, ghoul::Dictionary initDict);
    void addVisibleProperties();
    void removeVisibleProperties();

    LayerGroup& _parent;

    properties::OptionProperty _typeOption;
    properties::OptionProperty _blendModeOption;
    properties::BoolProperty _enabled;
    properties::TriggerProperty _reset;
    properties::TriggerProperty _remove;

    layergroupid::TypeID _type;
    std::shared_ptr<tileprovider::TileProvider> _tileProvider;
    OtherTypesProperties _otherTypesProperties;
    LayerRenderSettings _renderSettings;
    LayerAdjustment _layerAdjustment;

    glm::ivec2 _padTilePixelStartOffset;
    glm::ivec2 _padTilePixelSizeDifference;

    const layergroupid::GroupID _layerGroupId;

    std::function<void(void)> _onChangeCallback;
  };

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__
