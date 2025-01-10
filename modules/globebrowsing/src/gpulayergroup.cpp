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

#include <modules/globebrowsing/src/gpulayergroup.h>

#include <modules/globebrowsing/src/layer.h>
#include <modules/globebrowsing/src/layergroup.h>
#include <modules/globebrowsing/src/layermanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/texture.h>

namespace openspace::globebrowsing {

void GPULayerGroup::setValue(ghoul::opengl::ProgramObject& program,
                             const LayerGroup& layerGroup, const TileIndex& tileIndex)
{
    ZoneScoped;

    ghoul_assert(
        layerGroup.activeLayers().size() == _gpuActiveLayers.size(),
        "GPU and CPU active layers must have same size"
    );

    const std::vector<Layer*>& activeLayers = layerGroup.activeLayers();
    for (unsigned int i = 0; i < activeLayers.size(); i++) {
        const GPULayer& gal = _gpuActiveLayers[i];
        const auto& galuc = gal.uniformCache;
        const Layer& al = *activeLayers[i];

        program.setUniform(galuc.opacity, al.opacity());
        program.setUniform(galuc.gamma, al.renderSettings().gamma);
        program.setUniform(galuc.multiplier, al.renderSettings().multiplier);
        program.setUniform(galuc.offset, al.renderSettings().offset);

        if (al.layerAdjustment().type() == layers::Adjustment::ID::ChromaKey) {
            program.setUniform(
                galuc.chromaKeyColor,
                al.layerAdjustment().chromaKeyColor()
            );
            program.setUniform(
                galuc.chromaKeyTolerance,
                al.layerAdjustment().chromaKeyTolerance()
            );
        }

        switch (al.type()) {
            // Intentional fall through. Same for all tile layers
            case layers::Layer::ID::DefaultTileProvider:
            case layers::Layer::ID::SingleImageProvider:
            case layers::Layer::ID::SpoutImageProvider:
            case layers::Layer::ID::VideoTileProvider:
            case layers::Layer::ID::ImageSequenceTileProvider:
            case layers::Layer::ID::SizeReferenceTileProvider:
            case layers::Layer::ID::TemporalTileProvider:
            case layers::Layer::ID::TileIndexTileProvider:
            case layers::Layer::ID::TileProviderByDate:
            case layers::Layer::ID::TileProviderByIndex:
            case layers::Layer::ID::TileProviderByLevel: {
                const ChunkTilePile& ctp = al.chunkTilePile(
                    tileIndex,
                    layerGroup.pileSize()
                );
                for (size_t j = 0; j < _gpuActiveLayers[i].gpuChunkTiles.size(); j++) {
                    GPULayer::GPUChunkTile& t = _gpuActiveLayers[i].gpuChunkTiles[j];
                    ghoul_assert(ctp[j].has_value(), "Wrong ChunkTiles number in pile");
                    const ChunkTile& ct = *ctp[j];

                    t.texUnit.activate();
                    if (ct.tile.texture) {
                        ct.tile.texture->bind();
                    }
                    program.setUniform(t.uniformCache.texture, t.texUnit);

                    program.setUniform(t.uniformCache.uvOffset, ct.uvTransform.uvOffset);
                    program.setUniform(t.uniformCache.uvScale, ct.uvTransform.uvScale);
                }
                break;
            }
            case layers::Layer::ID::SolidColor:
                program.setUniform(galuc.color, al.solidColor());
                break;
        }

        if (gal.isHeightLayer) {
            program.setUniform(galuc.depthOffset, al.depthTransform().offset);
            program.setUniform(galuc.depthScale, al.depthTransform().scale);
        }
    }
}

void GPULayerGroup::bind(ghoul::opengl::ProgramObject& p, const LayerGroup& layerGroup) {
    const std::vector<Layer*>& activeLayers = layerGroup.activeLayers();
    _gpuActiveLayers.resize(activeLayers.size());
    const int pileSize = layerGroup.pileSize();
    for (size_t i = 0; i < _gpuActiveLayers.size(); i++) {
        GPULayer& gal = _gpuActiveLayers[i];
        auto& galuc = gal.uniformCache;
        const Layer& al = *activeLayers[i];
        const std::string name = std::format("{}[{}].", layerGroup.identifier(), i);

        if (layerGroup.isHeightLayer()) {
            gal.isHeightLayer = true;
        }

        galuc.opacity = p.uniformLocation(name + "settings.opacity");
        galuc.gamma = p.uniformLocation(name + "settings.gamma");
        galuc.multiplier = p.uniformLocation(name + "settings.multiplier");
        galuc.offset = p.uniformLocation(name + "settings.offset");

        if (al.layerAdjustment().type() == layers::Adjustment::ID::ChromaKey) {
            galuc.chromaKeyColor = p.uniformLocation(
                name + "adjustment.chromaKeyColor"
            );
            galuc.chromaKeyTolerance = p.uniformLocation(
                name + "adjustment.chromaKeyTolerance"
            );
        }

        switch (al.type()) {
            // Intentional fall through. Same for all tile layers
            case layers::Layer::ID::DefaultTileProvider:
            case layers::Layer::ID::SingleImageProvider:
            case layers::Layer::ID::SpoutImageProvider:
            case layers::Layer::ID::VideoTileProvider:
            case layers::Layer::ID::ImageSequenceTileProvider:
            case layers::Layer::ID::SizeReferenceTileProvider:
            case layers::Layer::ID::TemporalTileProvider:
            case layers::Layer::ID::TileIndexTileProvider:
            case layers::Layer::ID::TileProviderByDate:
            case layers::Layer::ID::TileProviderByIndex:
            case layers::Layer::ID::TileProviderByLevel: {
                gal.gpuChunkTiles.resize(pileSize);
                for (size_t j = 0; j < gal.gpuChunkTiles.size(); j++) {
                    GPULayer::GPUChunkTile& t = gal.gpuChunkTiles[j];
                    auto& tuc = t.uniformCache;
                    const std::string n = std::format("{}pile.chunkTile{}.", name, j);

                    tuc.texture = p.uniformLocation(n + "textureSampler");
                    tuc.uvOffset = p.uniformLocation(n + "uvTransform.uvOffset");
                    tuc.uvScale = p.uniformLocation(n + "uvTransform.uvScale");
                }
                break;
            }
            case layers::Layer::ID::SolidColor:
                galuc.color = p.uniformLocation(name + "color");
                break;
        }

        if (gal.isHeightLayer) {
            galuc.depthOffset = p.uniformLocation(name + "depthTransform.depthOffset");
            galuc.depthScale = p.uniformLocation(name + "depthTransform.depthScale");
        }
    }
}

void GPULayerGroup::deactivate() {
    for (GPULayer& gal : _gpuActiveLayers) {
        for (GPULayer::GPUChunkTile& t : gal.gpuChunkTiles) {
            t.texUnit.deactivate();
        }
    }
}

}  // namespace openspace::globebrowsing
