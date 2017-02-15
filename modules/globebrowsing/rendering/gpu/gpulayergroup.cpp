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

#include <modules/globebrowsing/rendering/gpu/gpulayergroup.h>

#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/rendering/gpu/gpuheightlayer.h>

namespace openspace {
namespace globebrowsing {

void GPULayerGroup::setValue(ProgramObject* programObject, const LayerGroup& layerGroup,
                             const TileIndex& tileIndex)
{
    auto& activeLayers = layerGroup.activeLayers();
    ghoul_assert(
        activeLayers.size() == gpuActiveLayers.size(),
        "GPU and CPU active layers must have same size!"
    );
    for (int i = 0; i < activeLayers.size(); ++i) {
        gpuActiveLayers[i]->setValue(
            programObject,
            *activeLayers[i],
            tileIndex,
            layerGroup.pileSize()
        );
    }
}

void GPULayerGroup::bind(ProgramObject* programObject, const LayerGroup& layerGroup, 
                         const std::string& nameBase, int category)
{
    auto activeLayers = layerGroup.activeLayers();
    gpuActiveLayers.resize(activeLayers.size());
    int pileSize = layerGroup.pileSize();
    for (size_t i = 0; i < gpuActiveLayers.size(); ++i) {
        // should maybe a proper GPULayer factory
        gpuActiveLayers[i] = (category == LayerManager::HeightLayers) ?
            std::make_unique<GPUHeightLayer>() : 
            std::make_unique<GPULayer>();
        std::string nameExtension = "[" + std::to_string(i) + "].";
        gpuActiveLayers[i]->bind(
            programObject,
            *activeLayers[i],
            nameBase + nameExtension,
            pileSize
        );
    }
}

void GPULayerGroup::deactivate() {
    for (size_t i = 0; i < gpuActiveLayers.size(); ++i) {
        gpuActiveLayers[i]->deactivate();
    }
}
    
}  // namespace globebrowsing
}  // namespace openspace
