/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <catch2/catch_test_macros.hpp>

#include <modules/globebrowsing/src/rawtile.h>
#include <modules/globebrowsing/src/tiletextureinitdata.h>
#include <openspace/opengl/texture.h>
#include <array>
#include <cstring>
#include <limits>

using namespace openspace;

namespace {
    constexpr float Missing = -std::numeric_limits<float>::max();

    /**
     * Creates a 2x2 single-channel tile of type \p glType that contains the \p values.
     */
    template <typename T>
    RawTile createTile(GLenum glType, const std::array<T, 4>& values) {
        RawTile tile;
        tile.textureInitData = TileTextureInitData(
            2,
            2,
            glType,
            opengl::Texture::Format::Red,
            TileTextureInitData::ShouldAllocateDataOnCPU::Yes
        );
        tile.imageData = std::make_unique<std::byte[]>(sizeof(values));
        std::memcpy(tile.imageData.get(), values.data(), sizeof(values));
        return tile;
    }

    std::array<float, 4> floatValues(const RawTile& tile) {
        std::array<float, 4> res;
        std::memcpy(res.data(), tile.imageData.get(), sizeof(res));
        return res;
    }
} // namespace

TEST_CASE("RawTile: Signed integer to float", "[rawtile]") {
    RawTile tile = createTile<GLshort>(GL_SHORT, { -32768, -32767, 0, 32767 });
    toFloatTile(tile, -32768.f);

    REQUIRE(tile.textureInitData.has_value());
    CHECK(tile.textureInitData->glType == GL_FLOAT);
    CHECK(tile.textureInitData->textureFormat == opengl::Texture::Format::Red);
    CHECK(tile.textureInitData->totalNumBytes == 4 * sizeof(float));
    CHECK(tile.textureInitData->shouldAllocateDataOnCPU);

    const std::array<float, 4> values = floatValues(tile);
    CHECK(values[0] == Missing);
    CHECK(values[1] == -1.f);
    CHECK(values[2] == 0.f);
    CHECK(values[3] == 1.f);
}

TEST_CASE("RawTile: Signed integer clamping", "[rawtile]") {
    // The smallest value is clamped to -1 in the same way the GPU clamps it
    RawTile tile = createTile<GLshort>(GL_SHORT, { -32768, 0, 0, 100 });
    toFloatTile(tile, 0.f);

    const std::array<float, 4> values = floatValues(tile);
    CHECK(values[0] == -1.f);
    CHECK(values[1] == Missing);
    CHECK(values[2] == Missing);
    CHECK(values[3] == 100.f / 32767.f);
}

TEST_CASE("RawTile: Unsigned integer to float", "[rawtile]") {
    RawTile tile = createTile<GLushort>(GL_UNSIGNED_SHORT, { 0, 1, 65535, 65534 });
    toFloatTile(tile, 65534.f);

    CHECK(tile.textureInitData->glType == GL_FLOAT);

    const std::array<float, 4> values = floatValues(tile);
    CHECK(values[0] == 0.f);
    CHECK(values[1] == 1.f / 65535.f);
    CHECK(values[2] == 1.f);
    CHECK(values[3] == Missing);
}

TEST_CASE("RawTile: Unsupported type", "[rawtile]") {
    RawTile tile = createTile<float>(GL_FLOAT, { 0.f, 1.f, 2.f, 3.f });
    CHECK_THROWS(toFloatTile(tile, 0.f));
}
