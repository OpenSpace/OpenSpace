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

#include <modules/globebrowsing/rendering/gpu/gpulayerrendersettings.h>

#include <modules/globebrowsing/rendering/layer/layerrendersettings.h>

namespace openspace::globebrowsing {

void GPULayerRenderSettings::setValue(ghoul::opengl::ProgramObject* programObject,
                                      const LayerRenderSettings& layerSettings)
{
    gpuOpacity.setValue(programObject, layerSettings.opacity.value());
    gpuGamma.setValue(programObject, layerSettings.gamma.value());
    gpuMultiplier.setValue(programObject, layerSettings.multiplier.value());
    gpuOffset.setValue(programObject, layerSettings.offset.value());

    if (layerSettings.useValueBlending) {
        gpuValueBlending.setValue(programObject, layerSettings.valueBlending.value());
    }
}

void GPULayerRenderSettings::bind(const LayerRenderSettings& layerSettings,
                                  ghoul::opengl::ProgramObject* programObject,
                                  const std::string& nameBase)
{
    gpuOpacity.bind(programObject, nameBase + "opacity");
    gpuGamma.bind(programObject, nameBase + "gamma");
    gpuMultiplier.bind(programObject, nameBase + "multiplier");
    gpuOffset.bind(programObject, nameBase + "offset");
    
    if (layerSettings.useValueBlending) {
        gpuValueBlending.bind(programObject, nameBase + "valueBlending");
    }
}

}  // namespace openspace::globebrowsing
