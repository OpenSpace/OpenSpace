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

#include <modules/globebrowsing/rendering/gpu/gpulayermanager.h>

#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/rendering/gpu/gpulayer.h>
#include <modules/globebrowsing/rendering/gpu/gpulayergroup.h>

namespace openspace::globebrowsing {

GPULayerManager::~GPULayerManager() {} // NOLINT

void GPULayerManager::setValue(ghoul::opengl::ProgramObject* programObject,
                               const LayerManager& manager, const TileIndex& tileIndex)
{
    const std::vector<std::shared_ptr<LayerGroup>>& layerGroups = manager.layerGroups();

    for (size_t i = 0; i < layerGroups.size(); ++i) {
        _gpuLayerGroups[i]->setValue(programObject, *layerGroups[i], tileIndex);
    }
}

void GPULayerManager::bind(ghoul::opengl::ProgramObject* programObject,
                           const LayerManager& manager)
{
    const std::vector<std::shared_ptr<LayerGroup>>& layerGroups = manager.layerGroups();
    if (_gpuLayerGroups.size() != layerGroups.size()) {
        _gpuLayerGroups.resize(layerGroups.size());
        for (std::unique_ptr<GPULayerGroup>& gpuLayerGroup : _gpuLayerGroups) {
            gpuLayerGroup = std::make_unique<GPULayerGroup>();
        }
    }

    for (size_t i = 0; i < layerGroups.size(); ++i) {
        const std::string& nameBase = layergroupid::LAYER_GROUP_IDENTIFIERS[i];
        _gpuLayerGroups[i]->bind(
            programObject,
            *layerGroups[i],
            nameBase,
            static_cast<int>(i)
        );
    }
}

void GPULayerManager::deactivate() {
    for (std::unique_ptr<GPULayerGroup>& l : _gpuLayerGroups) {
        l->deactivate();
    }
}

}  // namespace openspace::globebrowsing
