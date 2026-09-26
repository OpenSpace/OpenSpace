/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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
 *****************************************************************************************
 * This code is based on the Freetype GL engine as developed by Nicolas P. Rougier. The  *
 * library is available at http://code.google.com/p/freetype-gl/. His implementation of  *
 * the TextureAtlas is based on the article by Jukka Jylaanki: "A Thousand Ways to Pack  *
 * the Bin - A Practical Approach to Two-Dimensional Rectangle Bin Packing", 2010.       *
 * More precisely, this is an implementation of the Skyline Bottom-Left algorithm based  *
 * on C++ sources provided by Jukka Jylaanki at:                                         *
 * http://clb.demon.fi/files/RectangleBinPack/                                           *
 ****************************************************************************************/

#ifndef __GHOUL___TEXTUREATLAS___H__
#define __GHOUL___TEXTUREATLAS___H__

#include <ghoul/glm.h>
#include <ghoul/misc/exception.h>
#include <ghoul/opengl/texture.h>
#include <memory>
#include <string>
#include <vector>

namespace ghoul::opengl {

/**
 * This class represents a texture atlas which automatically organizes smaller textures in
 * a compact representation. The TextureAtlas is useful if many small textures are needed,
 * but the overhead of creating a separate Texture for each is not desireable. The
 * TextureAtlas is created with a `size`, and in order to fill the atlas, new regions have
 * to first be requested (#newRegion) and then filled with data (#setRegionData). Due to
 * the fact that the TextureAtlas is represented by a single Texture on the GPU, the
 * `depth` can only be `1`, `2`, `3`, or `4`. Before the atlas can be used, it has to be
 * uploaded to the GPU first (#upload).
 */
class TextureAtlas {
public:
    /**
     * Exception that gets thrown if an invalid region would be returned or is used.
     */
    struct InvalidRegionException : RuntimeError {
        explicit InvalidRegionException(std::string msg);
    };

    using RegionHandle = int;

    /**
     * The constructor completely initializes the Texture Atlas. No additional
     * initialization step is necessary. Due to the fact that the underlying Texture is
     * initialized here, it requires a valid OpenGL context. The size is defined as
     * `width`, `height`, and `depth`.
     *
     * \param size The size (`width`, `height`, `depth`) of the TextureAtlas
     *
     * \pre \p size's width and height has to be bigger than `4` and smaller than the GPU
     *      limit for 2D textures. The \p size's depth has to be `1`, `2`, `3`, or `4`
     * \pre `width` component must be bigger than `4` and smaller than the GPU limit for
     *      2D tetxures
     * \pre `height` component must be bigger than `4` and smaller than the GPU limit for
     *      2D tetxures
     * \pre `depth` component must be `1`, `2`, `3`, or `4`
     */
    explicit TextureAtlas(glm::ivec3 size);

    /**
     * Move constructor that leaves the incoming atlas invalid.
     *
     * \param rhs The origin TextureAtlas
     */
    TextureAtlas(TextureAtlas&& rhs) noexcept;

    /**
     * Move operator that moves the data into the new atlas without performing any copy
     * operations. This leaves the original atlas invalid.
     *
     * \param rhs The original TextureAtlas
     * \return The atlas into which the original values were moved into
     */
    TextureAtlas& operator=(TextureAtlas&& rhs) noexcept;

    /**
     * Initializes the TextureAtlas and creates its backend storage. This method requires
     * a valid OpenGL context.
     */
    void initialize();

    /**
    * Deinitializes the TextureAtlas and cleans its backend storage. This method
    * requires a valid OpenGL context.
    */
    void deinitialize();

    /**
     * Uploads the TextureAtlas to the graphics card. This function requires a valid
     * OpenGL context.
     */
    void upload();

    /**
     * Clears the TextureAtlas of all data, but leaves the underlying Texture unchanged.
     * A separate call to #upload is required to change the representation on the GPU as
     * well.
     */
    void clear();

    /**
     * Allocate a new region in the TextureAtlas with the desired \p width and \p height.
     * Please note that the internal width and height of the region will be increased by 1
     * pixel to account for a margin and prevent interpolation issues. The only
     * implication for the usage is the increase storage requirement.
     *
     * \param width The width of the requested region
     * \param height The height of the requested region
     * \return A handle to the new region that can be passed to the #setRegionData and
     *         #textureCoordinates functions
     *
     * \throw InvalidRegionException If the new requested region does not fit in the
     *        TextureAtlas
     */
    RegionHandle newRegion(int width, int height);

    /**
     * Sets the data in the region designated by the \p handle to the passed \p data.
     * <b>Warning</b>: Please note that this function does not check for buffer overflows
     * or underflows. In all cases, `width(region) * height(region) * depth(atlas)` number
     * of bytes are read from the `data` block.
     *
     * \param handle The handle of the region for which the data is provided
     * \param data The data that should be set for the specified region
     *
     * \pre \p data must not be a `nullptr`
     */
    void setRegionData(RegionHandle handle, void* data);

    /**
     * Structure that is returned from textureCoordinates function.
     */
    struct TextureCoordinatesResult {
        glm::vec2 topLeft = glm::vec2(0.f);
        glm::vec2 bottomRight = glm::vec2(0.f);
    };

    /**
     * Returns the texture coordinates that define the provided region. If the returned
     * `topLeft` and `bottomRight` coordinates are used as texture coordinates, the result
     * will be the same as if the data would have been bound to a separate texture. The
     * \p windowing parameter provides possiblity to offset the starting points (with the
     * first two arguments and restrict the width (with the third and fourth arguments).
     * ```
     *  -------------------
     * |         b         |
     * |    -----------    |
     * | a |           | c |
     * |   |           |   |
     * |    -----------    |
     * |         d         |
     * -------------------
     * ```
     *   - `windowing.x = a`
     *   - `windowing.y = b`
     *   - `windowing.z = c`
     *   - `windowing.w = d`
     *
     *
     * \param handle The handle of the region for which the texture coordinates shall be
     *        retrieved
     * \param windowing Determines whether a subset of the region should be retrieved. If
     *        this parameter is equal to `glm::ivec4(0)`, the full region is returned. The
     *        first two parameters `x` and `y` determine an offset for the top left
     *        corner, while the third and fourth parameters are subtracted from the bottom
     *        right corner. That means that if `windowing` is equal to
     *        `glm::ivec4(width / 4, height / 4, width / 4, height / 4`, a subset in the
     *        center of the region is returned
     * \return A TextureCoordinatesResult structure containing the `topLeft` corner of the
     *         region in texture coordinates and that `bottomRight` corner of the region
     *         in texture coordinates
     */
    TextureCoordinatesResult textureCoordinates(
        RegionHandle handle, const glm::ivec4& windowing = glm::ivec4(0)) const;

    /**
     * Returns the size of the TextureAtlas in `width`, `height`, and `depth`.
     *
     * \return The size of the TextureAtlas in `width`, `height`, and `depth`
     */
    glm::ivec3 size() const;

    /**
     * Returns the amount of pixels out of the maximum size (`width * height`) that are
     * currently in use. Please note that this is *not* equal to the amount of pixels that
     * can possiblity be used due to fragmentation in the atlas.
     *
     * \return The amount of pixels that are currently in use in the atlas
     */
    int spaceUsed() const;

    /**
     * Returns the Texture that is the underlying storage for the TextureAtlas. This
     * Texture can bound to a ProgramObject and subsequently sampled to retrieve the
     * stored textures.
     *
     * \return The Texture that is the underlying storage for this TextureAtlas
     */
    const Texture& texture() const;

private:
    int atlasFit(size_t index, int width, int height) const;
    void atlasMerge();

    std::vector<glm::ivec3> _nodes;

    /// All of the individual elements that are stored in the atlas
    std::vector<glm::u16vec4> _handleInformation;

    /// Size of the texture
    glm::ivec3 _size = glm::ivec3(0);

    /// Allocated surface size
    int _nUsed = 0;

    /// Pointer to the texture that is used as the atlas
    std::unique_ptr<Texture> _texture;

    /// Backend data storage for the texture
    std::vector<std::byte> _data;
};

} // namespace ghoul::opengl

#endif // __GHOUL___TEXTUREATLAS___H__
