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

#ifndef __OPENSPACE_MODULE_GLOBEBROWSING___TILE___H__
#define __OPENSPACE_MODULE_GLOBEBROWSING___TILE___H__

#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/tileuvtransform.h>

#include <modules/globebrowsing/cache/memoryawarecacheable.h>

#include <memory>

namespace ghoul { namespace opengl {
    class Texture;
}}

namespace openspace {
namespace globebrowsing {

struct TileMetaData;
struct TileUvTransform;

/**
 * Defines a status and may have a Texture and TileMetaData
 */
class Tile : public cache::MemoryAwareCacheable {
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
    
    Tile(std::shared_ptr<ghoul::opengl::Texture> texture,
         std::shared_ptr<TileMetaData> metaData,
         Status status);
    ~Tile() = default;

	std::shared_ptr<TileMetaData> metaData() const { return _metaData; };
	Status status() const { return _status; };
	std::shared_ptr<ghoul::opengl::Texture> texture() const { return _texture; };

    /**
     * Instantiates a new tile with a single color. 
     * 
     * \param size The size of texture to be created
     * \param color defined RGBA values in range 0-255.
     *
     * \returns a Tile with status OK and the a texture 
     * with the requested size and color
     */
    static Tile createPlainTile(const glm::uvec2& size, const glm::uvec4& color);
    static glm::vec2 compensateSourceTextureSampling(glm::vec2 startOffset, 
        glm::vec2 sizeDiff, glm::uvec2 resolution, glm::vec2 tileUV);
    static glm::vec2 TileUvToTextureSamplePosition(const TileUvTransform& uvTransform,
        glm::vec2 tileUV, glm::uvec2 resolution);
	/**
	* A tile with status unavailable that any user can return to
	* indicate that a tile was unavailable.
	*/
	static const Tile TileUnavailable;

private:
	std::shared_ptr<ghoul::opengl::Texture> _texture;
	std::shared_ptr<TileMetaData> _metaData;
	Status _status;
};

} // namespace globebrowsing
} // namespace openspace


#endif // __OPENSPACE_MODULE_GLOBEBROWSING___TILE___H__
