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
 ****************************************************************************************/

#ifndef __GHOUL___TEXTURE___H__
#define __GHOUL___TEXTURE___H__

#include <ghoul/glm.h>
#include <ghoul/misc/boolean.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <array>
#include <optional>
#include <string>
#include <variant>

namespace ghoul::opengl {

/**
 * This class is a wrapper for an OpenGL texture. It wraps the OpenGL functions for
 * working with textures. Textures are creates as immutable, which means that the format
 * can not be modified after the texture has been created. The pixel data (or parts of it
 * can be modified however.
 * The main usage of the class in OpenGL draw calls is through the overloaded conversion
 * operator for `GLuint` that returns the OpenGL name that has been created in the
 * constructor.
 *
 * The constructor will already allocate the necessary memory on the GPU and pass the data
 * to the GPU if the `data` pointer contained data.
 * At any point, the `setPixelData` function can be used to update the contents of the
 * Texture, and the provided data will be immediately uploaded to the GPU.
 *
 * Note that the `texel` and the `texelAsFloat` function require the call of the
 * `downloadTexture` function, which will download the contents of the texture from the
 * GPU into CPU memory. That memory can be freed through the `clearDownloadedTexture`.
 */
class Texture {
public:
    BooleanType(KeepMemory);

    /**
     * This enum specifies the allowed formats for the Texture%s. These are directly
     * mapped to the appropriate OpenGL constants.
     *
     * \see https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexImage1D.xhtml
     */
    enum class Format : std::underlying_type_t<GLenum> {
        Red = static_cast<std::underlying_type_t<GLenum>>(GL_RED),
        RG = static_cast<std::underlying_type_t<GLenum>>(GL_RG),
        RGB = static_cast<std::underlying_type_t<GLenum>>(GL_RGB),
        BGR = static_cast<std::underlying_type_t<GLenum>>(GL_BGR),
        RGBA = static_cast<std::underlying_type_t<GLenum>>(GL_RGBA),
        BGRA = static_cast<std::underlying_type_t<GLenum>>(GL_BGRA),
        DepthComponent = static_cast<std::underlying_type_t<GLenum>>(GL_DEPTH_COMPONENT)
    };

    /**
     * This enum specifies the filtering method this texture will use to interpolate
     * between two texel. The values for this enum correspond directly to OpenGL settings.
     * See the OpenGL specification for details.
     */
    enum class FilterMode {
        Nearest, ///< GL_NEAREST
        Linear, ///< GL_LINEAR
        LinearMipMap, ///< GL_LINEAR_MIPMAP_LINEAR
        AnisotropicMipMap
    };

    /**
     * This enum specifies the wrapping mode this texture will use at the edges of the
     * texture. The values for this enum correspond directly to OpenGL settings. See the
     * OpenGL specification for details.
     */
    enum class WrappingMode : std::underlying_type_t<GLenum> {
        Repeat = static_cast<std::underlying_type_t<GLenum>>(GL_REPEAT),
        Clamp = static_cast<std::underlying_type_t<GLenum>>(GL_CLAMP),
        ClampToEdge = static_cast<std::underlying_type_t<GLenum>>(GL_CLAMP_TO_EDGE),
        ClampToBorder = static_cast<std::underlying_type_t<GLenum>>(GL_CLAMP_TO_BORDER),
        MirroredRepeat = static_cast<std::underlying_type_t<GLenum>>(GL_MIRRORED_REPEAT)
    };

    /**
     * Encapsulating the wrapping mode state for 1D, 2D, and 3D textures.  1D textures
     * only use `s`, 2D textures use `s` and `t`, where as 3D textures use all three
     * specified wrapping modes.
     */
    struct WrappingModes {
        WrappingMode s;
        WrappingMode t = s;
        WrappingMode r = t;
    };

    struct FormatInit {
        /// The size of the new texture. The dimensionality of the dimensions must agree
        /// with the type of the texture. If a 2D texture is created, the `z` component of
        /// the dimension should be set to `1`
        glm::uvec3 dimensions;

        /// The type of the texture, should be one of GL_TEXTURE_1D, GL_TEXTURE_2D,
        /// GL_TEXTURE_3D, etc
        GLenum type;

        /// Specifies the format of the data
        Format format;

        /// The data type of the pixel data. See
        /// http://www.opengl.org/sdk/docs/man/xhtml/glTexImage1D.xml for a list of
        /// possible values
        GLenum dataType;

        /// The internal format for the texture. See
        /// https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexImage1D.xhtml
        /// Tables 1, 2, and 3 for concrete values. In addition, the S3TC_DXT formats can
        /// be used to support hardware compression. See
        /// http://www.opengl.org/wiki/Image_Format#S3TC.2FDXT for more information.
        /// If this value is not specified, a suitable internal format will be
        /// automatically selected based on the passed `format` and `dataType` parameters
        std::optional<GLenum> internalFormat = std::nullopt;
    };

    struct SamplerInit {
        /// The Texture::FilterMode that will be used to interpolate between texels
        FilterMode filter = FilterMode::Linear;

        /// The Texture::WrappingMode that will be used to generate values on the border
        /// of the texture
        std::variant<WrappingMode, WrappingModes> wrapping = WrappingMode::Repeat;

        /// If the \p filter is set to \c LinearMipMap or \c AnisotropicMipMap, this
        /// specifies the level to be used
        std::optional<int> mipMapLevel = std::nullopt;

        /// Sets the border color of the texture
        std::optional<glm::vec4> borderColor = std::nullopt;

        /// Changes the general swizzle mask of the texture. Note that single-channel
        /// grayscale images, i.e., with only a Red channel, will be automatically
        /// swizzled to RGB if `autoSwizzleGrayscale`  is true (this is the default)
        std::optional<std::array<GLenum, 4>> swizzleMask = std::nullopt;

        /// If true and the texture has a single Red channel, a swizzle mask of
        /// { GL_RED, GL_RED, GL_RED, GL_ONE } will be applied to the texture.
        /// If the swizzle mask is already set, this option is ignored
        bool autoSwizzleGrayscale = true;
    };

    /**
     * Creates a new Texture. If the \p data pointer is provided, the data will be
     * uploaded to the GPU. The data pointed to by \p data needs to be large enough to
     * accomodate `format.dimensions` and `format.dataType`.
     *
     * \param format The initialization struct describing the format of the Texture data
     * \param sampler The initialization struct describing the sampling of the data
     * \param data The data for the texture that should be uploaded or \c nullptr if no
     *        data should be uploaded
     * \param pixelAlignment The pixel alignment of the provided data
     * \param keepMemory If this is `Yes`, the passed memory in the `data` pointer will be
     *        kept on the CPU. The memory behind the `data` pointer will still be owned by
     *        the caller. This object will make a local copy of the data. If it is `No` it
     *        will not be stored after passing the data to the GPU
     */
    explicit Texture(FormatInit format, SamplerInit sampler,
        const std::byte* data = nullptr, int pixelAlignment = 1,
        KeepMemory keepMemory = KeepMemory::No);

    /**
     * Unloads the Texture from GPU memory and destroys the id. The destructor will also
     * remove the data associated with this texture if there is any.
     */
    ~Texture();

    /**
     * Resizes the textures to the new size. If the new size is different from the
     * previous size, the contents of the texture are erased and if the Texture class
     * owned the RAM pixel data, it is erased. As the texture created by this class is
     * immutable, this operation will invalidate the OpenGL name of the object, so if that
     * has been cached outside, it is considered invalid after a call to `resize`.
     *
     * \param dimensions The new size of the texture
     */
    void resize(glm::uvec3 dimensions);

    /**
     * Returns the OpenGL name of this texture.
     *
     * \return The OpenGL name of this texture
     */
    operator GLuint() const;

    /**
     * Sets a new user-friendly name for this texture. This name is also provided to the
     * OpenGL context and can be used in debugging programs to identify a texture. If the
     * empty string is passed, the user-friendly name used by OpenGL will be removed.
     *
     * \param name The new user-friendly name that should be used for this texture
     */
    void setName(std::string name);

    /**
     * Returns a user-friendly (optional) name for this Texture. The name is not used
     * internally and is solely for external purposes. One possible use is the filename
     * from which the texture was loaded.
     *
     * \return The name for this texture
     */
    const std::string& name() const;

    /**
     * Returns the size of this texture.
     *
     * \return The size of this texture
     */
    glm::uvec3 dimensions() const;

    /**
     * Returns the number of channels that this texture contains.
     *
     * \return The number of channels that this texture contains
     */
    int numberOfChannels() const;

    /**
     * Returns the type for this texture.
     *
     * \return The type for this texture. This value can either be `GL_TEXTURE_1D`,
     *         `GL_TEXTURE_2D` or `GL_TEXTURE_3D` depending on the dimension of the stored
     *         texture
     */
    GLenum type() const;

    /**
     * Returns the format for this texture.
     *
     * \return The format for this texture
     */
    Format format() const;

    /**
     * Returns the internal format for this texture. See
     * https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexImage1D.xhtml for more
     * information and the possible return values.
     */
    GLenum internalFormat() const;

    /**
     * Returns the storage data type for this Texture. For a complete list of available
     * return values see
     * https://registry.khronos.org/OpenGL-Refpages/gl4/html/glTexImage1D.xhtml for more
     * information.
     *
     * \return The storage data type
     */
    GLenum dataType() const;

    /**
     * Returns the stored data of the texture. The data will be downloaded from the GPU
     * for this call and returned to the called. This texture object will not retain a
     * copy of that data.
     *
     * \return The data of this texture
     */
    std::vector<std::byte> pixelData() const;

    /**
     * Returns the recommended `Format` corresponding to the given number of color
     * channels.
     *
     * \param nChannels The number of channels to return a format for
     * \return The recommended `Format`
     */
    static Format formatFromNumChannels(int nChannels);

    /**
     * Downloads the contents of this texture into CPU memory and stores it locally. The
     * The data can then be retrieved through a call to `cachedPixelData` which will use
     * this cached data. The cache can be cleared and the memory reclaimed through a call
     * to the `clearDownloadedTexture` function.
     */
    void downloadTexture();

    /**
     * Returns the data that has been previously cached through a call to the
     * `downloadTexture` function. This function must have been called at least once
     * before or else this function will error.
     *
     * \return The texture data
     */
    const std::vector<std::byte> cachedPixelData() const;

    /**
     * Clears the cache of the texture contents that has been created through a previous
     * call to the `downloadTexture` function, a call to `setPixelData` with
     * `KeepMemory::Yes` or the data passed in the constructor if `KeepMemory::Yes` was
     * set.
     */
    void clearDownloadedTexture();

    /**
     * Returns the size the pixel data should have according to the dimensionality and the
     * bytes per pixel. `dimensions.x * dimensions.y * dimensions.z * bpp`.
     *
     * \return The size of the pixel data according to the dimensionality and the bytes
     *         per pixel
     */
    int expectedPixelDataSize() const;

    /**
     * Sets new data for the texture to use. The number of pixels has to be the same as
     * determined by the sizes when the texture was created initially or else an error
     * message will be raised by OpenGL.
     *
     * \param pixels The pointer to the new data array that should be used
     * \param pixelAlignment The byte-alignment for each of the pixels in the provided
     *        \p pixels array
     * \param keepMemory If this is set to `Yes`, a copy of the passed memory will be
     *        retained by this object. The memory behind the `data` pointer will still be
     *        owned by the caller. A copy of the data will be made and stored instead
     */
    void setPixelData(const std::byte* pixels, int pixelAlignment = 1,
        KeepMemory keepMemory = KeepMemory::No);

    /**
     * Returns the texel at the specified position as a `float` vector. Independent of the
     * underlying type, each component of the vector is in the range of `[0,1]`. Only
     * Texture%s with a data type of the following list can be used to fetch textures:
     * `GL_UNSIGNED_BYTE`, `GL_BYTE`, `GL_UNSIGNED_SHORT`, `GL_SHORT`, `GL_UNSIGNED_INT`,
     * `GL_INT`, `GL_FLOAT`. Trying to use this function on another type will lead to
     * undefined behavior in the return value.
     *
     * \param pos The coordinate of the texel
     * \return The texel at the specified position as a vector with each component in
     *         `[0,1]`
     *
     * \pre `pos.x` must be smaller than the width of the Texture
     * \pre `pos.y` must be smaller than the height of the Texture
     * \pre `pos.z` must be smaller than the depth of the Texture
     */
    glm::vec4 texelAsFloat(const glm::uvec3& pos) const;

    /**
     * Accesses the texel at `pos` from the data array. Only Textures with a data type of
     * the following list can be used to fetch textures: `GL_UNSIGNED_BYTE`, `GL_BYTE`,
     * `GL_UNSIGNED_SHORT`, `GL_SHORT`, `GL_UNSIGNED_INT`, `GL_INT`, `GL_FLOAT`. Trying to
     * use this function on another type will lead to undefined behavior in the return
     * value.
     *
     * \param pos The coordinate of the texel
     * \return The texel at the specified position cast to the requested type T
     *
     * \pre The size of T must be equal to the bytes per pixel stored in the Texture
     * \pre `pos.x` must be smaller than the width of the Texture
     * \pre `pos.y` must be smaller than the height of the Texture
     * \pre `pos.z` must be smaller than the height of the Texture
     */
    template <class T>
    const T& texel(const glm::uvec3& pos) const;

protected:
    /**
     * Initializes the OpenGL state of this texture.
     */
    void initialize(const std::byte* data);

    /**
     * Upload the provided data to the GPU.
     */
    void uploadTexture(const std::byte* data) const;

private:
    /// The OpenGL name for this texture
    GLuint _id = 0;

    /// The texture type (GL_TEXTURE_1D, GL_TEXTURE_2D, etc)
    const GLenum _type;

    /// The dimensions for this textures. 1D textures only have the `x` component
    /// specified, 2D textures only `x` and `y`
    glm::uvec3 _dimensions = glm::uvec3(0);

    /// The format of the texture
    const Format _format;

    /// The sized internal format of the texture
    const GLenum _internalFormat;

    /// The data type of the pixel data
    const GLenum _dataType;

    /// Which filtering mode is used to sample data from the texture on the GPU
    const FilterMode _filter;

    /// The wrapping modes for each dimension used when sampling data on the GPU
    const WrappingModes _wrapping;

    /// The border color. If no border color was specified, this value is `std::nullopt`
    const std::optional<glm::vec4> _borderColor;

    /// The swizzle mask. If no swizzle mask was specified, this value is `std::nullopt`
    const std::optional<std::array<GLenum, 4>> _swizzleMask;

    /// If this value is `true` and new data is uploaded, it will cause the mipmap to be
    /// regenerated
    bool _usesMipMapping = false;

    /// The desired mipmap level for this texture. This value is only used if the
    /// `_filter` used is one of the mip mapping filters
    const int _mipMapLevel;

    /// The user-friendly name for this texture
    std::string _name;

    /// The maximum anisotropy level for this texture. This value is only set and used if
    /// the Anisotropic filtering mode was used
    float _anisotropyLevel = -1.f;

    /// The pixel alignment for this texture that was used to upload the last pixel data
    int _pixelAlignment = 1;

    /// This CPU pixel data for this texture. This vector is empty unless the data has
    /// been kept or has been explicitly downloaded
    std::vector<std::byte> _pixels;
};

extern template const uint8_t& Texture::texel(const glm::uvec3& pos) const;
extern template const uint16_t& Texture::texel(const glm::uvec3& pos) const;
extern template const uint32_t& Texture::texel(const glm::uvec3& pos) const;
extern template const int8_t& Texture::texel(const glm::uvec3& pos) const;
extern template const int16_t& Texture::texel(const glm::uvec3& pos) const;
extern template const int32_t& Texture::texel(const glm::uvec3& pos) const;
extern template const float& Texture::texel(const glm::uvec3& pos) const;

extern template const glm::tvec2<uint8_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec2<uint16_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec2<uint32_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec2<int8_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec2<int16_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec2<int32_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec2<float>& Texture::texel(const glm::uvec3& pos) const;

extern template const glm::tvec3<uint8_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec3<uint16_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec3<uint32_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec3<int8_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec3<int16_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec3<int32_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec3<float>& Texture::texel(const glm::uvec3& pos) const;

extern template const glm::tvec4<uint8_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec4<uint16_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec4<uint32_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec4<int8_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec4<int16_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec4<int32_t>& Texture::texel(const glm::uvec3& pos) const;
extern template const glm::tvec4<float>& Texture::texel(const glm::uvec3& pos) const;

} // namespace ghoul::opengl

#endif // __GHOUL___TEXTURE___H__
