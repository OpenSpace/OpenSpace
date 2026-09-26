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

#include <ghoul/io/texture/texturereader.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/texture.h>
#include <stb_image.h>
#include <cstring>
#include <string_view>
#include <thread>
#include <utility>

namespace {
    using namespace ghoul;
    using namespace ghoul::io::texture;

    ImageInfo loadImageData(unsigned char* data, int x, int y, int n,
                            std::string_view message)
    {
        if (!data) {
            throw TextureLoadException(
                message,
                std::format("Error reading image data: {}", stbi_failure_reason())
            );
        }

        // This is weird. stb_image.h says that the first pixel loaded is the one in the
        // upper left. However, if we load the data and just use it, the images are
        // flipped in y. I assume that there is a 1-t floating around somewhere and
        // someone should hunt that down.  For now, we just flip these values as well

        // As we only need to flip in y direction, we can take entire scanlines and move
        // the ith line to the (y-i-1)th line
        std::vector<std::byte> buffer = std::vector<std::byte>(x * n);
        for (int i = 0; i < y / 2; i++) {
            // 1. Save the destination
            std::memcpy(buffer.data(), data + (i * x * n), x * n);

            // 2. Copy the source to destination
            std::memcpy(data + (i * x * n), data + (y - i - 1) * x * n, x * n);

            // 3. Copy into the source
            std::memcpy(data + (y - i - 1) * x * n, buffer.data(), x * n);
        }

        // Copy data into vector
        std::vector<std::byte> vec(
            reinterpret_cast<std::byte*>(data),
            reinterpret_cast<std::byte*>(data) + (x * y * n)
        );

        // Done with the data, let's free it
        stbi_image_free(data);

        return {
            .dimensions = glm::ivec2(x, y),
            .nChannels = n,
            .data = std::move(vec)
        };
    }
} // namespace

namespace ghoul::io::texture {

MissingReaderException::MissingReaderException(std::string extension,
                                               std::filesystem::path file_)
    : RuntimeError(
        std::format(
            "No reader found for extension '{}' with file '{}'", extension, file_
        ),
        "IO"
    )
    , fileExtension(std::move(extension))
    , file(std::move(file_))
{}

TextureLoadException::TextureLoadException(std::filesystem::path name,
                                           std::string msg)
    : RuntimeError(std::format("Error loading texture '{}'", name), "IO")
    , filename(std::move(name))
    , errorMessage(std::move(msg))
{}

InvalidLoadException::InvalidLoadException(void* memory, size_t size)
    : RuntimeError(
        std::format("Error loading texture at location {} with size {}", memory, size),
        "IO"
    )
    , _memory(memory)
    , _size(size)
{}

ImageInfo loadImage(const std::filesystem::path& filename) {
    ghoul_assert(!filename.empty(), "Filename must not be empty");

    std::string extension = std::filesystem::path(filename).extension().string();
    if (!extension.empty()) {
        extension = extension.substr(1);
    }
    ghoul_assert(!extension.empty(), "Filename must have an extension");

    if (!isSupportedReadExtension(extension)) {
        throw MissingReaderException(extension, filename);
    }

    const std::string f = filename.string();
    int x = 0;
    int y = 0;
    int n = 0;
    unsigned char* data = stbi_load(f.c_str(), &x, &y, &n, 0);

    return loadImageData(data, x, y, n, f);
}

std::future<ImageInfo> loadImageAsync(const std::filesystem::path& filename) {
    return std::async(std::launch::async, [filename]() { return loadImage(filename); });
}

ImageInfo loadImage(void* memory, size_t size, const std::string& format) {
    ghoul_assert(memory, "Memory must not be nullptr");
    ghoul_assert(size > 0, "Size must be > 0");

    if (!isSupportedReadExtension(format)) {
        throw InvalidLoadException(memory, size);
    }

    int x = 0;
    int y = 0;
    int n = 0;
    unsigned char* data = stbi_load_from_memory(
        reinterpret_cast<unsigned char*>(memory),
        static_cast<int>(size),
        &x,
        &y,
        &n,
        0
    );

    return loadImageData(data, x, y, n, "Memory");
}

std::unique_ptr<opengl::Texture> loadTexture(const ImageInfo& info, int nDimensions,
                                             opengl::Texture::SamplerInit samplerSettings)
{
    const GLenum type = [](int d) {
        switch (d) {
            case 1: return GL_TEXTURE_1D;
            case 2: return GL_TEXTURE_2D;
            case 3: return GL_TEXTURE_3D;
            default:
                throw RuntimeError(std::format("Unsupported dimensionality '{}'", d));
        }
    }(nDimensions);

    std::unique_ptr<opengl::Texture> texture =
        std::make_unique<opengl::Texture>(
            opengl::Texture::FormatInit{
                .dimensions = glm::uvec3(info.dimensions.x, info.dimensions.y, 1),
                .type = type,
                .format = opengl::Texture::formatFromNumChannels(info.nChannels),
                .dataType = GL_UNSIGNED_BYTE
            },
            samplerSettings,
            reinterpret_cast<const std::byte*>(info.data.data())
        );
    return texture;
}

std::unique_ptr<opengl::Texture> loadTexture(const std::filesystem::path& filename,
                                                                          int nDimensions,
                                             opengl::Texture::SamplerInit samplerSettings)
{
    ghoul_assert(!filename.empty(), "Filename must not be empty");
    ghoul_assert(nDimensions >= 1 && nDimensions <= 3, "nDimensions must be 1, 2, or 3");

    ImageInfo info = loadImage(filename);
    return loadTexture(info, nDimensions, std::move(samplerSettings));
}

std::unique_ptr<opengl::Texture> loadTexture(void* memory, size_t size, int nDimensions,
                                             opengl::Texture::SamplerInit samplerSettings,
                                                                const std::string& format)
{
    ImageInfo info = loadImage(memory, size, format);
    return loadTexture(info, nDimensions, std::move(samplerSettings));
}

ImageInfo imageInfo(const std::filesystem::path& filename) {
    std::string extension = std::filesystem::path(filename).extension().string();
    if (!extension.empty()) {
        extension = extension.substr(1);
    }
    ghoul_assert(!extension.empty(), "Filename must have an extension");

    if (!isSupportedReadExtension(extension)) {
        throw MissingReaderException(extension, filename);
    }

    const std::string f = filename.string();
    int x = 0;
    int y = 0;
    int n = 0;
    stbi_info(f.c_str(), &x, &y, &n);

    return {
        .dimensions = glm::ivec2(x, y),
        .nChannels = n,
        .data = {}
    };
}

bool isSupportedReadExtension(const std::string& extension) {
    const std::string e = toLowerCase(extension);
    std::vector<std::string> extensions = supportedReadExtensions();
    return std::find(extensions.begin(), extensions.end(), e) != extensions.end();
}

std::vector<std::string> supportedReadExtensions() {
    // Taken from stb_image.h
    return  {
        "jpeg", "jpg",
        "png",
        "bmp",
        "psd",  // Photoshop
        "tga",
        "gif",
        "hdr",  // Radiance RGBE format
        "pic",  // Softimage PIC
        "ppm", "pgm"
    };
}

} // namespace ghoul::io::texture
