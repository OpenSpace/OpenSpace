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

#include <modules/globebrowsing/rendering/gpu/gpulayergroup.h>

#include <modules/globebrowsing/rendering/layer/layergroup.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/rendering/gpu/gpuheightlayer.h>
#include <modules/globebrowsing/rendering/gpu/gpulayer.h>

namespace openspace::globebrowsing {

GPULayerGroup::~GPULayerGroup() {} // NOLINT

void GPULayerGroup::setValue(ghoul::opengl::ProgramObject* programObject,
                             const LayerGroup& layerGroup, const TileIndex& tileIndex)
{
    auto& activeLayers = layerGroup.activeLayers();
    ghoul_assert(
        activeLayers.size() == _gpuActiveLayers.size(),
        "GPU and CPU active layers must have same size!"
    );
    for (unsigned int i = 0; i < activeLayers.size(); ++i) {
        _gpuActiveLayers[i]->setValue(
            programObject,
            *activeLayers[i],
            tileIndex,
            layerGroup.pileSize()
        );
    }
}

void GPULayerGroup::bind(ghoul::opengl::ProgramObject* programObject,
                         const LayerGroup& layerGroup, const std::string& nameBase,
                         int category)
{
    const std::vector<std::shared_ptr<Layer>>& activeLayers = layerGroup.activeLayers();
    _gpuActiveLayers.resize(activeLayers.size());
    const int pileSize = layerGroup.pileSize();
    for (size_t i = 0; i < _gpuActiveLayers.size(); ++i) {
        // should maybe a proper GPULayer factory
        _gpuActiveLayers[i] = (category == layergroupid::GroupID::HeightLayers) ?
            std::make_unique<GPUHeightLayer>() :
            std::make_unique<GPULayer>();
        _gpuActiveLayers[i]->bind(
            programObject,
            *activeLayers[i],
            nameBase + "[" + std::to_string(i) + "].",
            pileSize
        );
    }
}

void GPULayerGroup::deactivate() {
    for (std::unique_ptr<GPULayer>& l : _gpuActiveLayers) {
        l->deactivate();
    }
}

}  // namespace openspace::globebrowsing
