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

#ifndef __GHOUL___TEXTUREWRITER___H__
#define __GHOUL___TEXTUREWRITER___H__

#include <ghoul/misc/exception.h>
#include <string>
#include <vector>

namespace ghoul::opengl { class Texture; }

namespace ghoul::io::texture {

/**
 * Exception that gets thrown when the provided \p extension is not supported.
 */
struct MissingWriterException final : public RuntimeError {
    explicit MissingWriterException(std::string extension);
    const std::string fileExtension;
};

/**
 * The exception that gets thrown if there was an error writing the Texture.
 */
struct TextureWriteException final : public RuntimeError {
    TextureWriteException(std::string name, std::string msg);

    /// The filename that caused the exception to be thrown
    const std::string filename;

    /// The error message that occurred
    const std::string errorMessage;
};

/**
 * Saves the provided \p texture into the \p filename on disk, using the STB image writer
 * library. The image file format is determined by the extension of the \p filename.
 *
 * Supported file formats include:
 *   - JPEG (.jpeg, .jpg)
 *   - PNG (.png)
 *   - BMP (.bmp)
 *   - TGA (.tga)
 *
 * \param texture The Texture that is to be written to disk
 * \param filename The target filename for \p filename
 *
 * \throw TextureWriteException If there was an error writing the \p filename
 * \throw MissingWriterException If the extension in the \p filename is not supported
 * \pre \p filename must not be empty
 * \pre \p filename must have an extension
 */
void saveTexture(const opengl::Texture& texture, const std::string& filename);

/**
 * Returns Whether the provided file \p extension is supported for texture writing.
 *
 * \return True if the provided \p extension is supported, false otherwise
 */
bool isSupportedWriteExtension(const std::string& extension);

/**
 * Returns the supported file extensions for texture writing.
 *
 * \return The supported file extensions
 */
std::vector<std::string> supportedWriteExtensions();

} // namespace ghoul::io::texture

#endif // __GHOUL___TEXTUREWRITER___H__
