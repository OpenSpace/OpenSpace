/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/rendering/fadeable.h>

#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/layeradjustment.h>
#include <modules/globebrowsing/src/layerrendersettings.h>
#include <modules/globebrowsing/src/tileprovider/tileprovider.h>
#include <openspace/properties/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>

namespace openspace::documentation { struct Documentation; }

namespace openspace::globebrowsing {

struct LayerGroup;
struct TileIndex;
struct TileProvider;

class Layer : public properties::PropertyOwner, public Fadeable {
public:
    Layer(layers::Group::ID id, const ghoul::Dictionary& layerDict, LayerGroup& parent);

    void initialize();
    void deinitialize();

    ChunkTilePile chunkTilePile(const TileIndex& tileIndex, int pileSize) const;
    Tile::Status tileStatus(const TileIndex& index) const;

    layers::Layer::ID type() const;
    layers::Blend::ID blendMode() const;
    TileDepthTransform depthTransform() const;
    void setEnabled(bool enabled);
    bool enabled() const;
    bool isInitialized() const;
    TileProvider* tileProvider() const;
    glm::vec3 solidColor() const;
    const LayerRenderSettings& renderSettings() const;
    const LayerAdjustment& layerAdjustment() const;

    void setZIndex(unsigned int value);
    unsigned int zIndex() const;

    void onChange(std::function<void(Layer*)> callback);

    void update();

    glm::vec2 tileUvToTextureSamplePosition(const TileUvTransform& uvTransform,
        const glm::vec2& tileUV);

    static documentation::Documentation Documentation();

private:
    void initializeBasedOnType(layers::Layer::ID typeId, ghoul::Dictionary initDict);
    void addVisibleProperties();

    LayerGroup& _parent;

    properties::OptionProperty _typeOption;
    properties::OptionProperty _blendModeOption;
    properties::BoolProperty _enabled;
    properties::TriggerProperty _reset;
    properties::TriggerProperty _remove;
    properties::StringProperty _guiDescription;

    layers::Layer::ID _typeId;
    std::unique_ptr<TileProvider> _tileProvider;
    properties::Vec3Property _solidColor;
    LayerRenderSettings _renderSettings;
    LayerAdjustment _layerAdjustment;

    const layers::Group::ID _layerGroupId;

    std::function<void(Layer*)> _onChangeCallback;
    bool _isInitialized = false;

    unsigned int _zIndex = 0;
    bool _hasManualZIndex = false;
  };

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___LAYER___H__
