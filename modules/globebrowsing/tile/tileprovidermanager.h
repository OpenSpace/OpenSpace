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
#include <modules/globebrowsing/layered_rendering/layeredtextures.h>

#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/assert.h>

#include <memory>
#include <vector>
#include <string>

namespace openspace {
namespace globebrowsing {

    struct Layer {
        std::string name;
        std::shared_ptr<TileProvider> tileProvider;
        bool isActive;

        PerLayerSettings settings;
    };

    struct LayerGroup {

        void update();
        const std::vector<Layer>& activeLayers() const;

        std::vector<Layer> layers;
        bool levelBlendingEnabled;

    private:
        std::vector<Layer> _activeLayers;
    };

    class TileProviderManager {
    public:

        TileProviderManager(
            const ghoul::Dictionary& textureCategoriesDictionary,
            const ghoul::Dictionary& textureInitDictionary);
        ~TileProviderManager();

        LayerGroup& layerGroup(size_t groupId);
        LayerGroup& layerGroup(LayeredTextures::TextureCategory);

        void update();
        void reset(bool includingInactive = false);
    private:
        static void initTexures(
            std::vector<Layer>& destination, 
            const ghoul::Dictionary& dict, 
            const TileProviderInitData& initData);

        std::array<LayerGroup, LayeredTextures::NUM_TEXTURE_CATEGORIES> layerGroups;

    };

} // namespace globebrowsing
} // namespace openspace

#endif  // __TILE_PROVIDER_MANAGER_H__
