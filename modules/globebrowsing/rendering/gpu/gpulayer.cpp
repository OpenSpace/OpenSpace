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

#include <modules/globebrowsing/rendering/gpu/gpulayer.h>

#include <modules/globebrowsing/rendering/layer/layer.h>

namespace openspace {
namespace globebrowsing {


void GPUAdjustmentLayer::setValue(ghoul::opengl::ProgramObject* programObject,
                      const AdjustmentLayer& adjustmentLayer)
{
	gpuType.setValue(programObject, static_cast<int>(adjustmentLayer.type));
	switch (adjustmentLayer.type) {
		case AdjustmentLayer::TypeID::NONE:
			break;
		case AdjustmentLayer::TypeID::COLOR:
			gpuColor.setValue(programObject, adjustmentLayer.color.value());
			break;
		default:
			break;
	}
}

void GPUAdjustmentLayer::bind(ghoul::opengl::ProgramObject* programObject,
                  const AdjustmentLayer& adjustmentLayer,
                  const std::string& nameBase)
{
	gpuType.bind(programObject, nameBase + "type");
	switch (adjustmentLayer.type) {
		case AdjustmentLayer::TypeID::NONE:
			break;
		case AdjustmentLayer::TypeID::COLOR:
    		gpuColor.bind(programObject, nameBase + "color");
			break;
		default:
			break;
	}
}






void GPULayer::setValue(ghoul::opengl::ProgramObject* programObject, const Layer& layer,
                        const TileIndex& tileIndex, int pileSize)
{
    ChunkTilePile chunkTilePile = layer.getChunkTilePile(tileIndex, pileSize);
    gpuChunkTilePile.setValue(programObject, chunkTilePile);
    gpuRenderSettings.setValue(programObject, layer.renderSettings());
    gpuAdjustmentLayer.setValue(programObject, layer.adjustmentLayer());
}

void GPULayer::bind(ghoul::opengl::ProgramObject* programObject, const Layer& layer,
                    const std::string& nameBase, int pileSize)
{
    gpuChunkTilePile.bind(programObject, nameBase + "pile.", pileSize);
    gpuRenderSettings.bind(layer.renderSettings(), programObject, nameBase + "settings.");
    gpuAdjustmentLayer.bind(programObject, layer.adjustmentLayer(),
    						nameBase + "adjustmentLayer.");
}

void GPULayer::deactivate() {
    gpuChunkTilePile.deactivate();
}

}  // namespace globebrowsing
}  // namespace openspace
