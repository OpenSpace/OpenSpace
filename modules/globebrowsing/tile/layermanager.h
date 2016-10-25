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

#ifndef __TILE_PROVIDER_MANAGER_H__
#define __TILE_PROVIDER_MANAGER_H__


#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <modules/globebrowsing/tile/chunktile.h>
#include <modules/globebrowsing/tile/tileselector.h>
#include <modules/globebrowsing/layered_rendering/layeredtextures.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/assert.h>

#include <openspace/util/gpudata.h>

#include <memory>
#include <vector>
#include <string>

namespace openspace {
namespace globebrowsing {

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

        bool enabled() const { return _enabled.value(); }
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

        /// @returns const vector of all active layers
        const std::vector<std::shared_ptr<Layer>>& activeLayers() const;

        /// @returns the size of the pile to be used in rendering of this layer
        int pileSize() const;


        std::vector<std::shared_ptr<Layer>> layers;

        properties::BoolProperty levelBlendingEnabled;

    private:
        std::vector<std::shared_ptr<Layer>> _activeLayers;
    };

    /**
    * Manages multiple LayerGroups.
    */
    class LayerManager : public properties::PropertyOwner  {
    public:

        LayerManager(const ghoul::Dictionary& textureCategoriesDictionary);
        ~LayerManager();

        LayerGroup& layerGroup(size_t groupId);
        LayerGroup& layerGroup(LayeredTextures::TextureCategory);

        void update();
        void reset(bool includingInactive = false);

    private:

        std::vector<std::shared_ptr<LayerGroup>> layerGroups;

    };

} // namespace globebrowsing
} // namespace openspace

#endif  // __TILE_PROVIDER_MANAGER_H__
