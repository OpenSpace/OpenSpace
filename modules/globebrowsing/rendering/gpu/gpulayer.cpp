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
    gpuRenderSettings.setValue(programObject, layer.renderSettings());
    gpuLayerAdjustment.setValue(programObject, layer.layerAdjustment());

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
            gpuChunkTilePile.setValue(programObject, chunkTilePile);
            paddingStartOffset.setValue(programObject, layer.tilePixelStartOffset());
            paddingSizeDifference.setValue(
                programObject,
                layer.tilePixelSizeDifference()
            );
            break;
        }
        case layergroupid::TypeID::SolidColor:
            gpuColor.setValue(programObject, layer.otherTypesProperties().color.value());
            break;
        default:
            break;
    }
}

void GPULayer::bind(ghoul::opengl::ProgramObject* programObject, const Layer& layer,
                    const std::string& nameBase, int pileSize)
{
    gpuRenderSettings.bind(layer.renderSettings(), programObject, nameBase + "settings.");
    gpuLayerAdjustment.bind(
        layer.layerAdjustment(),
        programObject,
        nameBase + "adjustment."
    );

    switch (layer.type()) {
        // Intentional fall through. Same for all tile layers
        case layergroupid::TypeID::DefaultTileLayer:
        case layergroupid::TypeID::SingleImageTileLayer:
        case layergroupid::TypeID::SizeReferenceTileLayer:
        case layergroupid::TypeID::TemporalTileLayer:
        case layergroupid::TypeID::TileIndexTileLayer:
        case layergroupid::TypeID::ByIndexTileLayer:
        case layergroupid::TypeID::ByLevelTileLayer: {
            gpuChunkTilePile.bind(programObject, nameBase + "pile.", pileSize);
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
}

void GPULayer::deactivate() {
    gpuChunkTilePile.deactivate();
}

}  // namespace openspace::globebrowsing
