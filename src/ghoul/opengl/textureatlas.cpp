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

#include <ghoul/opengl/textureatlas.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <utility>

namespace ghoul::opengl {

TextureAtlas::InvalidRegionException::InvalidRegionException(std::string msg)
    : RuntimeError(std::move(msg), "TextureAtlas")
{}

TextureAtlas::TextureAtlas(glm::ivec3 size)
    : _size(std::move(size))
{
    // Limitations to the depth are due to the fact that the atlas is represented by a
    // single texture on the GPU (which only allows up to four channels)
    ghoul_assert(_size.x > 4, "Width has to be bigger than 4");
    ghoul_assert(_size.y > 4, "Height has to be bigger than 4");
    ghoul_assert(_size.z >= 1, "Depth has to be positive");
    ghoul_assert(_size.z <= 4, "Depth has to be smaller or equal to 4");

    // Since we are using uint16 as the index type in the handle type, we can't support
    // textures with a bigger size than 65535 (which would be a big texture *anyway*)
    ghoul_assert(_size.x <= std::numeric_limits<uint16_t>::max(), "Texture.x too large");
    ghoul_assert(_size.y <= std::numeric_limits<uint16_t>::max(), "Texture.y too large");

    _nodes.emplace_back(1, 1, _size.x - 2);
    _data.resize(_size.x * _size.y * _size.z);
    std::fill(_data.begin(), _data.end(), static_cast<std::byte>(0));
}

TextureAtlas::TextureAtlas(TextureAtlas&& rhs) noexcept
    : _nodes(std::move(rhs._nodes))
    , _handleInformation(std::move(rhs._handleInformation))
    , _size(std::move(rhs._size))
    , _nUsed(std::move(rhs._nUsed))
    , _texture(std::move(rhs._texture))
    , _data(std::move(rhs._data))
{}

TextureAtlas& TextureAtlas::operator=(TextureAtlas&& rhs) noexcept {
    if (this != &rhs) {
        _nodes = std::move(rhs._nodes);
        _texture = std::move(rhs._texture);
        _handleInformation = std::move(rhs._handleInformation);
        _size = std::move(rhs._size);
        _nUsed = std::move(rhs._nUsed);
        _data = std::move(rhs._data);
    }
    return *this;
}

void TextureAtlas::initialize() {
    const Texture::Format format = [](int z) {
        switch (z) {
            case 1: return Texture::Format::Red;
            case 2: return Texture::Format::RG;
            case 3: return Texture::Format::RGB;
            case 4: return Texture::Format::RGBA;
            default: throw RuntimeError("Wrong texture depth", "TextureAtlas");
        }
    }(_size.z);
    const GLenum dataType = GL_UNSIGNED_BYTE;

    _texture = std::make_unique<Texture>(
        Texture::FormatInit {
            .dimensions = glm::uvec3(glm::ivec2(_size), 1),
            .type = GL_TEXTURE_2D,
            .format = format,
            .dataType = dataType
        },
        Texture::SamplerInit {
            .filter = Texture::FilterMode::Nearest
        }
    );
    ghoul_assert(_texture, "Error creating Texture");
}

void TextureAtlas::deinitialize() {
    _texture = nullptr;
}

glm::ivec3 TextureAtlas::size() const {
    return _size;
}

const Texture& TextureAtlas::texture() const {
    return *_texture;
}

int TextureAtlas::spaceUsed() const {
    return _nUsed;
}

void TextureAtlas::upload() {
    _texture->setPixelData(_data.data());
}

void TextureAtlas::clear() {
    _nodes.clear();
    _nodes.emplace_back(1, 1 , _size.x - 2);

    _nUsed = 0;
    std::fill(_data.begin(), _data.end(), static_cast<std::byte>(0));
}

TextureAtlas::RegionHandle TextureAtlas::newRegion(int width, int height) {
    ghoul_assert(_texture, "TextureAtlas has not been initialized");

    // We assign an area that is one pixel bigger to allow for a margin around each region
    width += 1;
    height += 1;

    glm::ivec4 region = glm::ivec4(0, 0, width, height);

    int bestHeight = std::numeric_limits<int>::max();
    int bestWidth = std::numeric_limits<int>::max();
    int bestIndex = -1;

    for (size_t i = 0; i < _nodes.size(); i++) {
        const int y = atlasFit(i, width, height);
        if (y >= 0) {
            const glm::ivec3& node = _nodes[i];
            if (((y + height) < bestHeight) ||
                (((y + height) == bestHeight) && (node.z < bestWidth)))
            {
                bestHeight = y + height;
                bestIndex = static_cast<int>(i);
                bestWidth = node.z;
                region.x = node.x;
                region.y = y;
            }
        }
    }

    if (bestIndex == -1) {
        throw InvalidRegionException(std::format(
            "Could not fit new region of size ({},{})", width, height
        ));
    }

    _nodes.insert(_nodes.begin() + bestIndex, {region.x, region.y + height, width});

    for (size_t i = bestIndex + 1; i < _nodes.size(); i++) {
        glm::ivec3& node = _nodes[i];
        const glm::ivec3& prev = _nodes[i-1];

        if (node.x < (prev.x + prev.z)) {
            const int shrink = prev.x + prev.z - node.x;
            node.x += shrink;
            node.z -= shrink;
            if (node.z <= 0) {
                _nodes.erase(_nodes.begin() + i);
                --i;
            }
            else {
                break;
            }
        }
        else {
            break;
        }
    }
    atlasMerge();
    _nUsed += width * height;

    // The region width and height values are used elsewhere in the TextureAtlas and they
    // should not know about the added margins
    region.z -= 1;
    region.w -= 1;

    // Make the location information retrievable

    ghoul_assert(
        glm::all(glm::greaterThanEqual(region, glm::ivec4(0))),
        "Invalid region"
    );
    ghoul_assert(
        glm::all(glm::lessThan(region, glm::ivec4(std::numeric_limits<uint16_t>::max()))),
        "Invalid region"
    );

    _handleInformation.emplace_back(region);

    // Since we push_back the region, the current handle is the last valid array index
    return static_cast<RegionHandle>(_handleInformation.size() - 1);
}

void TextureAtlas::setRegionData(RegionHandle handle, void* data) {
    ghoul_assert(data, "Data must not be a nullptr");
    ghoul_assert(_texture, "TextureAtlas has not been initialized");

    const glm::u16vec4& region = _handleInformation[handle];

    const uint16_t x = region.x;
    const uint16_t y = region.y;
    const uint16_t width = region.z;
    const uint16_t height = region.w;

    ghoul_assert(x > 0, "x argument out of bounds");
    ghoul_assert(x < (_size.x - 1), "x argument out of bounds");
    ghoul_assert((x + width) <= (_size.x - 1), "x arguments out of bounds");

    ghoul_assert(y > 0, "y argument out of bounds");
    ghoul_assert(y < (_size.y - 1), "y argument out of bounds");
    ghoul_assert((y + height) <= (_size.y - 1), "y argument out of bounds");

    // As the incoming data is a single stream of bytes, we need to chop it up into chunks
    // of length 'width'. We have 'height' of these chunks that need to be copied into our
    // atlas
    for (int i = 0; i < height; i++) {
        void* dst = _data.data() + ((y + i) * _size.x + x) * sizeof(char) * _size.z;
        void* src = reinterpret_cast<unsigned char*>(data) + (i * width) * sizeof(char);
        const size_t nBytes = width * sizeof(char) * _size.z;

        if (src && dst) {
            memcpy(dst, src, nBytes);
        }
    }
}

TextureAtlas::TextureCoordinatesResult TextureAtlas::textureCoordinates(
                                                                      RegionHandle handle,
                                                        const glm::ivec4& windowing) const
{
    ghoul_assert(_texture, "TextureAtlas has not been initialized");

    const glm::u16vec4& region = _handleInformation[handle];

    // Offset the location to the pixel center
    const int topLeftX = region.x + windowing.x;
    const int topLeftY = region.y + windowing.y;

    const int bottomRightX = topLeftX + (region.z - windowing.z);
    const int bottomRightY = topLeftY + (region.w - windowing.w);

    TextureCoordinatesResult res;
    res.topLeft.x = static_cast<float>(topLeftX) / static_cast<float>(_size.x);
    res.topLeft.y = static_cast<float>(topLeftY) / static_cast<float>(_size.y);

    res.bottomRight.x = static_cast<float>(bottomRightX) / static_cast<float>(_size.x);
    res.bottomRight.y = static_cast<float>(bottomRightY) / static_cast<float>(_size.y);
    return res;
}

int TextureAtlas::atlasFit(size_t index, int width, int height) const {
    const int x = _nodes[index].x;
    int y = _nodes[index].y;

    // We first fill up the width
    if ((x + width) > (_size.x - 1)) {
        return -1;
    }
    int remainingWidth = width;
    while (remainingWidth > 0) {
        const glm::ivec3& node = _nodes[index];

        if (node.y > y) {
            y = node.y;
        }
        if ((y + height) > (_size.y - 1)) {
            return -1;
        }
        remainingWidth -= node.z;
        index++;
    }
    return y;
}

void TextureAtlas::atlasMerge() {
    for (size_t i = 0; i < _nodes.size() - 1; i++) {
        glm::ivec3& node = _nodes[i];
        const glm::ivec3& next = _nodes[i+1];
        if (node.y == next.y) {
            node.z += next.z;
            _nodes.erase(_nodes.begin() + (i + 1));
            --i;
        }
    }
}

} // namespace ghoul::opengl
