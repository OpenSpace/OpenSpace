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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GPUHEIGHTLAYER___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GPUHEIGHTLAYER___H__

#include <modules/globebrowsing/rendering/gpu/gpulayer.h>

#include <modules/globebrowsing/rendering/gpu/gputiledepthtransform.h>
#include <string>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace::globebrowsing {

class Layer;
struct TileIndex;

/**
 * Manages a GPU representation of a <code>Layer</code> representing
 * a height map.
 */
class GPUHeightLayer : public GPULayer {
public:
    virtual ~GPUHeightLayer() override = default;

    /**
     * Sets the value of <code>Layer</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been
     * called before setting using this method.
     */
    virtual void setValue(ghoul::opengl::ProgramObject* programObject, const Layer& layer,
        const TileIndex& tileIndex, int pileSize) override;

    /**
     * Binds this object with GLSL variables with identifiers starting
     * with nameBase within the provided shader program.
     * After this method has been called, users may invoke setValue.
     */
    virtual void bind(ghoul::opengl::ProgramObject* programObject, const Layer& layer,
        const std::string& nameBase, int pileSize) override;

private:
    GPUTileDepthTransform _gpuDepthTransform;
};

} // namespace openspace::globebrowsing

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GPUHEIGHTLAYER___H__
