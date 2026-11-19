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

#include <modules/globebrowsing/src/rawtile.h>

#include <openspace/misc/assert.h>
#include <algorithm>
#include <cstring>
#include <limits>

namespace {
    using namespace openspace;

    template <typename T>
    void normalize(const std::byte* src, float* dst, size_t nValues, float noDataValue) {
        constexpr float Factor = static_cast<float>(std::numeric_limits<T>::max());
        for (size_t i = 0; i < nValues; i++) {
            // The source buffer has no alignment guarantee for T
            T value;
            std::memcpy(&value, src + i * sizeof(T), sizeof(T));
            const float v = static_cast<float>(value);

            // Signed normalized values are clamped to -1 by the GPU
            dst[i] = (v == noDataValue) ?
                -std::numeric_limits<float>::max() :
                std::max(v / Factor, -1.f);
        }
    }
} // namespace

namespace openspace {

void toFloatTile(RawTile& rawTile, float noDataValue) {
    assert_msg(rawTile.textureInitData.has_value(), "Tile must have init data");

    const TileTextureInitData& src = *rawTile.textureInitData;

    TileTextureInitData dst = TileTextureInitData(
        src.dimensions.x,
        src.dimensions.y,
        GL_FLOAT,
        src.textureFormat,
        TileTextureInitData::ShouldAllocateDataOnCPU(src.shouldAllocateDataOnCPU)
    );

    const size_t nValues = src.totalNumBytes / src.bytesPerDatum;
    std::unique_ptr<std::byte[]> data = std::make_unique<std::byte[]>(dst.totalNumBytes);
    float* values = reinterpret_cast<float*>(data.get());
    switch (src.glType) {
        case GL_SHORT:
            normalize<GLshort>(rawTile.imageData.get(), values, nValues, noDataValue);
            break;
        case GL_UNSIGNED_SHORT:
            normalize<GLushort>(rawTile.imageData.get(), values, nValues, noDataValue);
            break;
        default:
            throw MissingCaseException();
    }

    rawTile.imageData = std::move(data);
    rawTile.textureInitData = std::move(dst);
}

} // namespace openspace
