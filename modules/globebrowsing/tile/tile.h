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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE___H__

#include <modules/globebrowsing/tile/tilemetadata.h>
#include <memory>

namespace ghoul::opengl { class Texture; }

namespace openspace::globebrowsing {

struct TileUvTransform;

/**
 * Defines a status and may have a Texture and TileMetaData
 */
class Tile {
public:
     /**
     * Describe if this Tile is good for usage (OK) or otherwise
     * the reason why it is not.
     */
    enum class Status {
        /**
         * E.g when texture data is not currently in memory.
         * texture and tileMetaData are both null
         */
        Unavailable,

        /**
         * Can be set by <code>TileProvider</code>s if the requested
         * <code>TileIndex</code> is undefined for that particular
         * provider.
         * texture and metaData are both null
         */
        OutOfRange,

        /**
         * An IO Error happend
         * texture and metaData are both null
         */
        IOError,

        /**
         * The Texture is uploaded to the GPU and good for usage.
         * texture is defined. metaData may be defined.
         */
        OK
    };

    Tile(ghoul::opengl::Texture* texture, std::shared_ptr<TileMetaData> metaData,
        Status status);
    ~Tile() = default;

    TileMetaData* metaData() const;
    Status status() const;
    ghoul::opengl::Texture* texture() const;

    /**
    * A tile with status unavailable that any user can return to
    * indicate that a tile was unavailable.
    */
    static const Tile TileUnavailable;

private:
    ghoul::opengl::Texture* _texture;
    std::shared_ptr<TileMetaData> _metaData;
    Status _status;
};

} // namespace openspace::globebrowsing


#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE___H__
