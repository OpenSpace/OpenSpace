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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___GPUCHUNKTILEPILE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___GPUCHUNKTILEPILE___H__

#include <modules/globebrowsing/rendering/gpu/gpuchunktile.h>

#include <modules/globebrowsing/tile/chunktile.h>

#include <openspace/util/gpudata.h>

#include <string>

namespace ghoul { namespace opengl {
class ProgramObject;
}}

namespace openspace {
namespace globebrowsing {

/**
 * Manages a GPU representation of a <code>ChunkTilePile</code>
 */
class GPUChunkTilePile {
public:

    /**
     * Sets the value of <code>ChunkTilePile</code> to its corresponding
     * GPU struct. OBS! Users must ensure bind has been 
     * called before setting using this method.
     */
    void setValue(ProgramObject* programObject, const ChunkTilePile& chunkTilePile);

    /** 
     * Binds this object with GLSL variables with identifiers starting 
     * with nameBase within the provided shader program.
     * After this method has been called, users may invoke setValue.
     */
    void bind(ProgramObject* programObject, const std::string& nameBase, 
              int pileSize);
    /**
    * Deactivates any <code>TextureUnit</code>s assigned by this object.
    * This method should be called after the OpenGL draw call.
    */
    void deactivate();

private:
    std::vector<GPUChunkTile> gpuChunkTiles;
};

} // namespace globebrowsing
} // namespace openspace

#endif // __OPENSPACE_MODULE_GLOBEBROWSING___GPUCHUNKTILEPILE___H__
