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

#include <modules/globebrowsing/rendering/gpu/gpulayer.h>

#include <modules/globebrowsing/rendering/layer/layer.h>

namespace openspace::globebrowsing {

void GPULayer::setValue(ghoul::opengl::ProgramObject* programObject, const Layer& layer,
                        const TileIndex& tileIndex, int pileSize)
{
    //gpuRenderSettings.setValue(programObject, layer.renderSettings());
    gpuOpacity.setValue(programObject, layer.renderSettings().opacity.value());
    gpuGamma.setValue(programObject, layer.renderSettings().gamma.value());
    gpuMultiplier.setValue(programObject, layer.renderSettings().multiplier.value());
    gpuOffset.setValue(programObject, layer.renderSettings().offset.value());


    //gpuLayerAdjustment.setValue(programObject, layer.layerAdjustment());
    switch (layer.layerAdjustment().type()) {
        case layergroupid::AdjustmentTypeID::None:
            break;
        case layergroupid::AdjustmentTypeID::ChromaKey: {
            gpuChromaKeyColor.setValue(
                programObject,
                layer.layerAdjustment().chromaKeyColor()
            );
            gpuChromaKeyTolerance.setValue(
                programObject,
                layer.layerAdjustment().chromaKeyTolerance()
            );
            break;
        }
        case layergroupid::AdjustmentTypeID::TransferFunction:
            break;
    }


    switch (layer.type()) {
        // Intentional fall through. Same for all tile layers
        case layergroupid::TypeID::DefaultTileLayer:
        case layergroupid::TypeID::SingleImageTileLayer:
        case layergroupid::TypeID::SizeReferenceTileLayer:
        case layergroupid::TypeID::TemporalTileLayer:
        case layergroupid::TypeID::TileIndexTileLayer:
        case layergroupid::TypeID::ByIndexTileLayer:
        case layergroupid::TypeID::ByLevelTileLayer: {
            ChunkTilePile chunkTilePile = layer.chunkTilePile(tileIndex, pileSize);
            //gpuChunkTilePile.setValue(programObject, chunkTilePile);
            for (size_t i = 0; i < gpuChunkTiles.size(); ++i) {
                //gpuChunkTiles[i].setValue(programObject, chunkTilePile[i]);
                gpuChunkTiles[i].gpuTexture.setValue(programObject, chunkTilePile[i].tile.texture());
                //gpuChunkTiles[i].gpuTileUvTransform.setValue(programObject, chunkTilePile[i].uvTransform);
                gpuChunkTiles[i].gpuUvOffset.setValue(programObject, chunkTilePile[i].uvTransform.uvOffset);
                gpuChunkTiles[i].gpuUvScale.setValue(programObject, chunkTilePile[i].uvTransform.uvScale);


            }

            paddingStartOffset.setValue(programObject, layer.tilePixelStartOffset());
            paddingSizeDifference.setValue(
                programObject,
                layer.tilePixelSizeDifference()
            );
            break;
        }
        case layergroupid::TypeID::SolidColor:
            gpuColor.setValue(programObject, layer.otherTypesProperties().color);
            break;
        default:
            break;
    }

    if (isHeightLayer) {
        //_gpuDepthTransform.setValue(programObject, layer.depthTransform());
        _gpuDepthOffset.setValue(programObject, layer.depthTransform().depthOffset);
        _gpuDepthScale.setValue(programObject, layer.depthTransform().depthScale);

    }
}

void GPULayer::bind(ghoul::opengl::ProgramObject* programObject, const Layer& layer,
                    const std::string& nameBase, int pileSize)
{
    //gpuRenderSettings.bind(layer.renderSettings(), programObject, nameBase + "settings.");
    gpuOpacity.bind(programObject, nameBase + "settings.opacity");
    gpuGamma.bind(programObject, nameBase + "settings.gamma");
    gpuMultiplier.bind(programObject, nameBase + "settings.multiplier");
    gpuOffset.bind(programObject, nameBase + "settings.offset");

    //gpuLayerAdjustment.bind(
    //    layer.layerAdjustment(),
    //    programObject,
    //    nameBase + "adjustment."
    //);

    switch (layer.layerAdjustment().type()) {
        case layergroupid::AdjustmentTypeID::None:
            break;
        case layergroupid::AdjustmentTypeID::ChromaKey: {
            gpuChromaKeyColor.bind(programObject, nameBase + "adjustment.chromaKeyColor");
            gpuChromaKeyTolerance.bind(programObject, nameBase + "adjustment.chromaKeyTolerance");
            break;
        }
        case layergroupid::AdjustmentTypeID::TransferFunction:
            break;
    }

    switch (layer.type()) {
        // Intentional fall through. Same for all tile layers
        case layergroupid::TypeID::DefaultTileLayer:
        case layergroupid::TypeID::SingleImageTileLayer:
        case layergroupid::TypeID::SizeReferenceTileLayer:
        case layergroupid::TypeID::TemporalTileLayer:
        case layergroupid::TypeID::TileIndexTileLayer:
        case layergroupid::TypeID::ByIndexTileLayer:
        case layergroupid::TypeID::ByLevelTileLayer: {
            //gpuChunkTilePile.bind(programObject, nameBase + "pile.", pileSize);
            gpuChunkTiles.resize(pileSize);
            for (size_t i = 0; i < gpuChunkTiles.size(); ++i) {
                std::string nameExtension = "pile.chunkTile" + std::to_string(i) + ".";
                //gpuChunkTiles[i].bind(programObject, nameBase + nameExtension);
                gpuChunkTiles[i].gpuTexture.bind(programObject, nameBase + nameExtension + "textureSampler");
                //gpuChunkTiles[i].gpuTileUvTransform.bind(programObject, nameBase + nameExtension + "uvTransform.");
                gpuChunkTiles[i].gpuUvOffset.bind(programObject, nameBase + nameExtension + "uvTransform.uvOffset");
                gpuChunkTiles[i].gpuUvScale.bind(programObject, nameBase + nameExtension + "uvTransform.uvScale");
            }

            paddingStartOffset.bind(programObject, nameBase + "padding.startOffset");
            paddingSizeDifference.bind(
                programObject,
                nameBase + "padding.sizeDifference"
            );
            break;
        }
        case layergroupid::TypeID::SolidColor:
            gpuColor.bind(programObject, nameBase + "color");
            break;
        default:
            break;
    }

    if (isHeightLayer) {
        //_gpuDepthTransform.bind(programObject, nameBase + "depthTransform.");
        _gpuDepthOffset.bind(programObject, nameBase + "depthTransform.depthOffset");
        _gpuDepthScale.bind(programObject, nameBase + "depthTransform.depthScale");

    }
}

void GPULayer::deactivate() {
    //gpuChunkTilePile.deactivate();
    for (GPUChunkTile& t : gpuChunkTiles) {
        //t.deactivate();
        t.gpuTexture.deactivate();
    }

}

}  // namespace openspace::globebrowsing
