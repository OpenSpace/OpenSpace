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

#ifndef __GHOUL___TEXTUREREADER___H__
#define __GHOUL___TEXTUREREADER___H__

#include <ghoul/glm.h>
#include <ghoul/misc/exception.h>
#include <ghoul/opengl/texture.h>
#include <filesystem>
#include <future>
#include <memory>
#include <string>
#include <vector>

namespace ghoul::io::texture {

/**
 * Exception that gets thrown when the provided \p extension is not supported.
 */
struct MissingReaderException final : public RuntimeError {
    MissingReaderException(std::string extension, std::filesystem::path file_);

    const std::string fileExtension;
    const std::filesystem::path file;
};

/**
 * The exception that gets thrown if there was an error loading the Texture.
 */
struct TextureLoadException final : public RuntimeError {
    TextureLoadException(std::filesystem::path name, std::string msg);

    /// The filename that caused the exception to be thrown
    const std::filesystem::path filename;

    /// The error message that occurred
    const std::string errorMessage;
};

/**
 * Exception that gets thrown when an invalid load result occurs.
 */
struct InvalidLoadException final : public RuntimeError {
    InvalidLoadException(void* memory, size_t size);

    const void* _memory;
    const size_t _size;
};

/**
 * Holds the information about a loaded image including its dimensions, channel count,
 * and raw pixel data.
 */
struct ImageInfo {
    /// The width and height of the image in pixels
    glm::ivec2 dimensions;

    /// The number of color channels (e.g., 3 for RGB, 4 for RGBA)
    int nChannels;

    /// The raw pixel data of the image
    std::vector<std::byte> data;
};

/**
 * Loads the provided \p filename using the STB image library and returns the image
 * information including dimensions, channel count, and raw pixel data. The image format
 * is determined by the extension of the \p filename.
 *
 * \param filename The name of the file which should be loaded
 * \return An ImageInfo structure containing the image dimensions, number of channels,
 *         and raw pixel data
 *
 * \throw TextureLoadException If there was an error reading the \p filename
 * \throw MissingReaderException If the extension in the \p filename is not supported
 * \pre \p filename must not be empty
 * \pre \p filename must have an extension
 * \pre The extension of \p filename must be among the supported file extensions
 */
ImageInfo loadImage(const std::filesystem::path& filename);

/**
 * Loads the provided \p filename using the STB image library asynchronously and returns a
 * future to the image information including dimensions, channel count, and raw pixel
 * data. The image format is determined by the extension of the \p filename.
 *
 * \param filename The name of the file which should be loaded
 * \return A future for a ImageInfo structure containing the image dimensions, number of
 *         channels, and raw pixel data
 *
 * \throw TextureLoadException If there was an error reading the \p filename
 * \throw MissingReaderException If the extension in the \p filename is not supported
 * \pre \p filename must not be empty
 * \pre \p filename must have an extension
 * \pre The extension of \p filename must be among the supported file extensions
 */
std::future<ImageInfo> loadImageAsync(const std::filesystem::path& filename);

/**
 * Loads an image from the memory pointed at by \p memory using the STB image library and
 * returns the image information including dimensions, channel count, and raw pixel data.
 * The memory block must contain at least \p size number of bytes.
 *
 * \param memory The memory that contains the bytes of the image to be loaded
 * \param size The number of bytes contained in \p memory
 * \param format The format of the image pointed to by \p memory. This parameter should
 *        be the same as the usual file extension for the image and is used to determine
 *        if the file type is supported for reading
 * \return An ImageInfo structure containing the image dimensions, number of channels,
 *         and raw pixel data
 *
 * \throw TextureLoadException If there was an error reading the \p memory
 * \throw MissingReaderException If the \p format is not supported
 * \throw InvalidLoadException If the load result is invalid
 * \pre \p memory must not be `nullptr`
 * \pre \p size must be > 0
 * \pre \p format must be among the supported file extensions
 */
ImageInfo loadImage(void* memory, size_t size, const std::string& format);

/**
 * Loads the texture provided by the \p info data.
 *
 * \param info The information used to initialize and create the texture
 * \param nDimensions The number of dimensions of the texture that are returned when using
 *        this function. This parameter is necessary as it is not always possible to
 *        automatically detect this based on the image information. For example, someone
 *        might want to load a 128x1 texture but use it as a 2D texture instead
 * \param samplerSettings The settings that should be used for the Texture that is
 *        created from the contents of the \p filename
 */
std::unique_ptr<opengl::Texture> loadTexture(const ImageInfo& info, int nDimensions,
    opengl::Texture::SamplerInit samplerSettings = {});

/**
 * Loads the provided \p filename using the STB image library into a Texture and returns
 * it. The image format is determined by the extension of the \p filename.
 *
 * \param filename The name of the file which should be loaded into a texture
 * \param nDimensions The number of dimensions of the texture that are returned when using
 *        this function. This parameter is necessary as it is not always possible to
 *        automatically detect this based on the image information. For example, someone
 *        might want to load a 128x1 texture but use it as a 2D texture instead
 * \param samplerSettings The settings that should be used for the Texture that is
 *        created from the contents of the \p filename
 *
 * \throw TextureLoadException If there was an error reading the \p filename
 * \throw MissingReaderException If the extension in the \p filename is not supported
 * \pre \p filename must not be empty
 * \pre \p filename must have an extension
 * \pre \p nDimensions The number of texture dimension must be 1, 2, or 3
 * \pre The extension of \p filename must be among the supported file extensions
 */
std::unique_ptr<opengl::Texture> loadTexture(const std::filesystem::path& filename,
    int nDimensions, opengl::Texture::SamplerInit samplerSettings = {});

/**
 * Loads a Texture from the memory pointed at by \p memory using the STB image library.
 * The memory block must contain at least \p size number of bytes.
 *
 * \param memory The memory that contains the bytes of the Texture to be loaded
 * \param size The number of bytes contained in \p memory
 * \param nDimensions The number of dimensions of the texture that are returned when using
 *        this function. This parameter is necessary as it is not always possible to
 *        automatically detect this based on the image information. For example, someone
 *        might want to load a 128x1 texture but use it as a 2D texture instead
 * \param format The format of the image pointed to by \p memory. This parameter should
 *        be the same as the usual file extension for the image and is used to determine
 *        if the file type is supported for reading
 * \param samplerSettings The settings that should be used for the Texture that is
 *        created from the contents of the \p memory
 *
 * \throw TextureLoadException If there was an error reading the \p memory
 * \throw MissingReaderException If the extension in the \p filename is not supported
 * \pre \p memory must not be `nullptr`
 * \pre \p size must be > 0
 * \pre \p format must be among the supported file extensions
 * \pre \p nDimensions The number of texture dimension must be 1, 2, or 3
 * \pre The extension of \p filename must be among the supported file extensions
 */
std::unique_ptr<opengl::Texture> loadTexture(void* memory, size_t size,
    int nDimensions, opengl::Texture::SamplerInit samplerSettings = {},
    const std::string& format = "");

/**
 * Loads the information about the image at the provided \p filename using the STB image
 * library, without fully loading the image data.
 *
 * \param filename The image file that should be inspected
 * \return The size of the image in pixels
 *
 * \throw TextureLoadException If there was an error loading the texture
 * \pre \p filename must not be empty
 * \pre The extension of \p filename must be among the supported file extensions
 */
ImageInfo imageInfo(const std::filesystem::path& filename);

/**
 * Returns Whether the provided file \p extension is supported by this reader, i.e., which
 * are supported in the STB image library.
 *
 * \return True if the provided \p extension is supported, false otherwise
 */
bool isSupportedReadExtension(const std::string& extension);

/**
 * Returns a list of all the extensions that are supported for reading, i.e., which are
 * supported in the STB image library.
 *
 * \return A list of all supported extensions
 */
std::vector<std::string> supportedReadExtensions();

} // namespace ghoul::io::texture

#endif // __GHOUL___TEXTUREREADER___H__
