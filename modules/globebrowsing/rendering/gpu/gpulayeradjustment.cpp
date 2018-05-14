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

#include <modules/globebrowsing/rendering/gpu/gpulayeradjustment.h>

#include <modules/globebrowsing/rendering/layer/layeradjustment.h>

namespace openspace::globebrowsing {

void GPULayerAdjustment::setValue(ghoul::opengl::ProgramObject* programObject,
                                  const LayerAdjustment& layerAdjustment)
{
    switch (layerAdjustment.type()) {
        case layergroupid::AdjustmentTypeID::None:
            break;
        case layergroupid::AdjustmentTypeID::ChromaKey: {
            gpuChromaKeyColor.setValue(
                programObject,
                layerAdjustment.chromaKeyColor()
            );
            gpuChromaKeyTolerance.setValue(
                programObject,
                layerAdjustment.chromaKeyTolerance()
            );
            break;
        }
        case layergroupid::AdjustmentTypeID::TransferFunction:
            break;
    }
}

void GPULayerAdjustment::bind(const LayerAdjustment& layerAdjustment,
                              ghoul::opengl::ProgramObject* programObject,
                              const std::string& nameBase)
{
    switch (layerAdjustment.type()) {
        case layergroupid::AdjustmentTypeID::None:
            break;
        case layergroupid::AdjustmentTypeID::ChromaKey: {
            gpuChromaKeyColor.bind(programObject, nameBase + "chromaKeyColor");
            gpuChromaKeyTolerance.bind(programObject, nameBase + "chromaKeyTolerance");
            break;
        }
        case layergroupid::AdjustmentTypeID::TransferFunction:
            break;
    }
}

}  // namespace openspace::globebrowsing
