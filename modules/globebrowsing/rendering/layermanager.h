/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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

#ifndef __LAYERMANAGER_H__
#define __LAYERMANAGER_H__

#include <modules/globebrowsing/tile/chunktile.h>

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/util/gpudata.h>

#include <memory>
#include <vector>
#include <string>

namespace openspace {
namespace globebrowsing {

    class TileProvider;

    struct LayerRenderSettings : public properties::PropertyOwner{
        LayerRenderSettings();
        properties::FloatProperty opacity;
        properties::FloatProperty gamma;
        properties::FloatProperty multiplier;
    };

    /**
    * Simple struct which is used to enable/disable <code>TileProvider</code> 
    * and associate is with a name. It also holds layer specific information
    * which is used in rendering of layer.
    */
    class Layer : public properties::PropertyOwner {
    public:
        Layer(const ghoul::Dictionary& layerDict);
        ~Layer();

        ChunkTilePile getChunkTilePile(const TileIndex& tileIndex, int pileSize) const;

        bool enabled() const { return _enabled.value(); }
        TileProvider* tileProvider() const { return _tileProvider.get(); }
        const LayerRenderSettings& renderSettings() const { return _renderSettings; }

    private:
        properties::BoolProperty _enabled;
        std::shared_ptr<TileProvider> _tileProvider;
        LayerRenderSettings _renderSettings;
    };

    /**
     * Convenience class for dealing with multiple <code>Layer</code>s.
     */
    struct LayerGroup : public properties::PropertyOwner {
        LayerGroup(std::string name);
        LayerGroup(std::string name, const ghoul::Dictionary& dict);

        /// Updates all layers tile providers within this group
        void update();

        /// @returns const vector of all layers
        const std::vector<std::shared_ptr<Layer>>& layers() const;

        /// @returns const vector of all active layers
        const std::vector<std::shared_ptr<Layer>>& activeLayers() const;

        /// @returns the size of the pile to be used in rendering of this layer
        int pileSize() const;

        bool layerBlendingEnabled() const { return _levelBlendingEnabled.value(); }

    private:
        std::vector<std::shared_ptr<Layer>> _layers;
        std::vector<std::shared_ptr<Layer>> _activeLayers;

        properties::BoolProperty _levelBlendingEnabled;
    };

    /**
    * Manages multiple LayerGroups.
    */
    class LayerManager : public properties::PropertyOwner  {
    public:
        static const size_t NUM_LAYER_GROUPS = 7;
        static const std::string LAYER_GROUP_NAMES[NUM_LAYER_GROUPS];
        static enum LayerGroupId{
            HeightLayers,
            ColorLayers,
            ColorOverlays,
            GrayScaleLayers,
            GrayScaleColorOverlays,
            NightLayers,
            WaterMasks,
        };

        LayerManager(const ghoul::Dictionary& textureCategoriesDictionary);
        ~LayerManager();

        const LayerGroup& layerGroup(size_t groupId);
        const LayerGroup& layerGroup(LayerGroupId);

        bool hasAnyBlendingLayersEnabled() const;

        const std::vector<std::shared_ptr<LayerGroup>>& layerGroups() const;

        void update();
        void reset(bool includingDisabled = false);

    private:
        std::vector<std::shared_ptr<LayerGroup>> _layerGroups;
    };

} // namespace globebrowsing
} // namespace openspace

#endif  // __LAYERMANAGER_H__
