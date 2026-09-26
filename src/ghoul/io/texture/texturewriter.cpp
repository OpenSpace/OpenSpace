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

#include <ghoul/io/texture/texturewriter.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/texture.h>
#include <algorithm>
#include <filesystem>
#include <stb_image_write.h>
#include <utility>

namespace ghoul::io::texture {

MissingWriterException::MissingWriterException(std::string extension)
    : RuntimeError(std::format("No writer was found for extension '{}'", extension), "IO")
    , fileExtension(std::move(extension))
{}

TextureWriteException::TextureWriteException(std::string name, std::string msg)
    : RuntimeError(std::format("Error writing texture '{}'", name), "IO")
    , filename(std::move(name))
    , errorMessage(std::move(msg))
{}

void saveTexture(const opengl::Texture& texture, const std::string& filename) {
    ghoul_assert(!filename.empty(), "Filename must not be empty");

    std::string extension = std::filesystem::path(filename).extension().string();
    if (!extension.empty()) {
        // Remove the leading . of the extension
        extension = extension.substr(1);
    }
    ghoul_assert(!extension.empty(), "Filename must have an extension");

    if (!isSupportedWriteExtension(extension)) {
        throw MissingWriterException(extension);
    }

    if (texture.dimensions().z > 1) {
        LERRORC(
            "TextureWriter",
            std::format(
                "Cannot write 3D texture to file: '{}'. 3D textures are not supported",
                filename
            )
        );
        return;
    }

    // Make sure the directory for the file exists
    std::filesystem::create_directory(std::filesystem::path(filename).parent_path());


    const int w = texture.dimensions().x;
    const int h = texture.dimensions().y;
    const int nComponents = texture.numberOfChannels();
    std::vector<std::byte> pixels = texture.pixelData();

    extension = toLowerCase(extension);

    if (extension == "jpeg" || extension == "jpg") {
        stbi_write_jpg(filename.c_str(), w, h, nComponents, pixels.data(), 0);
    }
    else if (extension == "png") {
        stbi_write_png(filename.c_str(), w, h, nComponents, pixels.data(), 0);
    }
    else if (extension == "bmp") {
        stbi_write_bmp(filename.c_str(), w, h, nComponents, pixels.data());
    }
    else if (extension == "tga") {
        stbi_write_tga(filename.c_str(), w, h, nComponents, pixels.data());
    }
    // @TODO (2023-10-06, emmbr26) Fix implementation. This does not generate correct
    // colors. Prabably the data format is currently not correct, as the other formats
    // expect 8-bit colors while the HDR function wants 32-bit rgb(e) data. Did not
    // seem too important to fix at point of writing, as it's currently not used anywhere
    //else if (extension == "hdr") {
    //    stbi_write_hdr(
    //        filename.c_str(),
    //        w,
    //        h,
    //        nComponents,
    //        reinterpret_cast<const float*>(data)
    //    );
    //}
    else {
        throw TextureWriteException(
            filename,
            std::format("Could not write to image with file extension {}", extension)
        );
    }
}

bool isSupportedWriteExtension(const std::string& extension) {
    const std::string e = toLowerCase(extension);
    std::vector<std::string> extensions = supportedWriteExtensions();
    return std::find(extensions.begin(), extensions.end(), e) != extensions.end();
}

std::vector<std::string> supportedWriteExtensions() {
    // Taken from stb_image_writer.h
    return {
         "jpeg", "jpg",
         "png",
         "bmp",
         // "hdr",
         "tga"
    };
}

} // namespace ghoul::io::texture
