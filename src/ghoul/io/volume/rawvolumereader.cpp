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

#include <ghoul/io/volume/rawvolumereader.h>

#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>
#include <fstream>
#include <string_view>
#include <string>
#include <utility>

namespace {
    constexpr std::string_view _loggerCat = "RawVolumeReader";
} // namespace

namespace ghoul::io {

RawVolumeReader::ReadHints::ReadHints(glm::ivec3 dimensions)
    : _dimensions(std::move(dimensions))
{}

RawVolumeReader::RawVolumeReader(ReadHints hints)
    : _hints(std::move(hints))
{}

void RawVolumeReader::setReadHints(glm::ivec3 dimension) {
    _hints._dimensions = std::move(dimension);
}

void RawVolumeReader::setReadHints(ReadHints hints) {
    _hints = std::move(hints);
}

std::unique_ptr<opengl::Texture> RawVolumeReader::read(const std::string& filename) {
    if (_hints._dimensions == glm::ivec3(0)) {
        LERROR(std::format("Volume dimensions not set for file '{}'", filename));
        return nullptr;
    }

    const unsigned int s = glm::compMul(_hints._dimensions);
    std::vector<std::byte> data = std::vector<std::byte>(s);

    std::ifstream fin = std::ifstream(filename, std::ios::in | std::ios::binary);
    if (fin.good()) {
        fin.read(reinterpret_cast<char*>(data.data()), s * sizeof(unsigned char));
    }
    else {
        LERROR(std::format("Could not open file '{}'", filename));
    }

    return std::make_unique<opengl::Texture>(
        opengl::Texture::FormatInit {
            .dimensions = glm::uvec3(_hints._dimensions),
            .type = GL_TEXTURE_3D,
            .format = _hints._format,
            .dataType = GL_UNSIGNED_BYTE,
            .internalFormat = _hints._internalFormat
        },
        opengl::Texture::SamplerInit {
            .wrapping = opengl::Texture::WrappingMode::ClampToBorder
        },
        data.data()
    );
}

} // namespace ghoul::io
