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

#include <modules/globebrowsing/src/tiletextureinitdata.h>
#include <openspace/opengl/texture.h>
#include <array>
#include <set>

using namespace openspace;

namespace {
    using Format = opengl::Texture::Format;

    constexpr std::array<Format, 6> Formats = {
        Format::Red, Format::RG, Format::RGB, Format::BGR, Format::RGBA, Format::BGRA
    };

    constexpr std::array<GLenum, 6> Types = {
        GL_UNSIGNED_BYTE, GL_BYTE, GL_UNSIGNED_SHORT, GL_SHORT, GL_HALF_FLOAT, GL_FLOAT
    };

    constexpr std::array<size_t, 5> Sizes = { 1, 256, 512, 513, 1024 };
} // namespace

TEST_CASE("TileTextureInitData: Unique hash keys", "[tiletextureinitdata]") {
    std::set<TileTextureInitData::HashKey> keys;
    size_t nCombinations = 0;
    for (const Format format : Formats) {
        for (const GLenum type : Types) {
            for (const size_t width : Sizes) {
                for (const size_t height : Sizes) {
                    const TileTextureInitData data(width, height, type, format);
                    keys.insert(data.hashKey);
                    nCombinations++;
                }
            }
        }
    }
    CHECK(keys.size() == nCombinations);
}

TEST_CASE("TileTextureInitData: Equal hash keys", "[tiletextureinitdata]") {
    const TileTextureInitData a(512, 512, GL_UNSIGNED_BYTE, Format::Red);
    const TileTextureInitData b(512, 512, GL_UNSIGNED_BYTE, Format::Red);
    CHECK(a.hashKey == b.hashKey);
}

TEST_CASE("TileTextureInitData: Assignment", "[tiletextureinitdata]") {
    const TileTextureInitData source(
        256,
        256,
        GL_SHORT,
        Format::Red,
        TileTextureInitData::ShouldAllocateDataOnCPU::Yes
    );
    TileTextureInitData target(512, 512, GL_UNSIGNED_BYTE, Format::BGRA);
    target = source;

    CHECK(target.dimensions == source.dimensions);
    CHECK(target.glType == source.glType);
    CHECK(target.textureFormat == source.textureFormat);
    CHECK(target.nRasters == 1);
    CHECK(target.bytesPerDatum == 2);
    CHECK(target.bytesPerPixel == 2);
    CHECK(target.bytesPerLine == 256 * 2);
    CHECK(target.totalNumBytes == 256 * 256 * 2);
    CHECK(target.shouldAllocateDataOnCPU);
    CHECK(target.hashKey == source.hashKey);
}
