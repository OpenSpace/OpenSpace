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

#include <modules/globebrowsing/rendering/gpu/gputileuvtransform.h>

//#include <modules/globebrowsing/rendering/layermanager.h>
//#include <modules/globebrowsing/tile/chunktile.h>
//#include <modules/globebrowsing/tile/tile.h>
//#include <modules/globebrowsing/tile/tiledepthtransform.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>

namespace openspace {
namespace globebrowsing {

void GPUTileUvTransform::setValue(ghoul::opengl::ProgramObject* programObject,
                                  const TileUvTransform& tileUvTransform)
{
    _gpuUvOffset.setValue(programObject, tileUvTransform.uvOffset);
    _gpuUvScale.setValue(programObject, tileUvTransform.uvScale);
}

void GPUTileUvTransform::bind(ghoul::opengl::ProgramObject* programObject,
                              const std::string& nameBase)
{
    _gpuUvOffset.bind(programObject, nameBase + "uvOffset");
    _gpuUvScale.bind(programObject, nameBase + "uvScale");
}

}  // namespace globebrowsing
}  // namespace openspace
