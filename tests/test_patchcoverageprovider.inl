/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include "gtest/gtest.h"

#include <modules/globebrowsing/other/patchcoverageprovider.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/glm.hpp>

class PatchCoverageProviderTest : public testing::Test {};

using namespace openspace;

TEST_F(PatchCoverageProviderTest, getTileIndexWithBigPatch) {

    // Allocate data
    GeodeticPatch patch(0,0,0,0);
    ChunkIndex ci;
    ChunkIndex ciExpected;

    // Create a provider with 1 in depth
    PatchCoverageProvider provider(
        Geodetic2(M_PI * 2, M_PI * 2), // size at level 0
        Geodetic2(- M_PI, - M_PI), // offset at level 0
        1); // depth

    // A big patch
    patch = GeodeticPatch(
        Geodetic2(0,0), // Center
        Geodetic2(M_PI / 2, M_PI / 2)); // Halfsize

    // Get its index
    ci = provider.getTileIndex(patch);
    ciExpected = {0, 0, 1};

    ASSERT_EQ(ciExpected, ci);

    // Patch positioned at the border of 1 in x index
    patch = GeodeticPatch(
        Geodetic2(0, M_PI / 2), // Center
        Geodetic2(M_PI / 2, M_PI / 2)); // Halfsize);

    // Get its index
    ci = provider.getTileIndex(patch);
    ciExpected = { 1, 0, 1 };

    ASSERT_EQ(ciExpected, ci);

    // Patch positioned at a little less than the border of 1 in x index
    patch = GeodeticPatch(
        Geodetic2(0, M_PI / 2 - 0.0001), // Center
        Geodetic2(M_PI / 2, M_PI / 2)); // Halfsize);

                                        // Get its index
    ci = provider.getTileIndex(patch);
    ciExpected = { 0, 0, 1 };

    ASSERT_EQ(ciExpected, ci);
}

TEST_F(PatchCoverageProviderTest, getTileIndexHigherDepthWithBigPatch) {

    // Allocate data
    GeodeticPatch patch(0, 0, 0, 0);
    ChunkIndex ci;
    ChunkIndex ciExpected;

    // Create a provider with 3 in depth
    // We still expect the same result since the patches are big
    PatchCoverageProvider provider(
        Geodetic2(M_PI * 2, M_PI * 2), // size at level 0
        Geodetic2(- M_PI, -M_PI), // offset at level 0
        3); // depth

    // A big patch
    patch = GeodeticPatch(
        Geodetic2(0, 0), // Center
        Geodetic2(M_PI / 2, M_PI / 2)); // Halfsize

    // Get its index
    ci = provider.getTileIndex(patch);
    ciExpected = { 0, 0, 1 };

    ASSERT_EQ(ciExpected, ci);

    // Patch positioned at the border of 1 in x index
    patch = GeodeticPatch(
        Geodetic2(0, M_PI / 2), // Center
        Geodetic2(M_PI / 2, M_PI / 2)); // Halfsize);

    // Get its index
    ci = provider.getTileIndex(patch);
    ciExpected = { 1, 0, 1 };

    ASSERT_EQ(ciExpected, ci);

    // Patch positioned at a little less than the border of 1 in x index
    patch = GeodeticPatch(
        Geodetic2(0, M_PI / 2 - 0.0001), // Center
        Geodetic2(M_PI / 2, M_PI / 2)); // Halfsize);

                                        // Get its index
    ci = provider.getTileIndex(patch);
    ciExpected = { 0, 0, 1 };

    ASSERT_EQ(ciExpected, ci);
}


TEST_F(PatchCoverageProviderTest, getTileIndexHigherDepthWithSmallPatch) {

    // Allocate data
    GeodeticPatch patch(0, 0, 0, 0);
    ChunkIndex ci;
    ChunkIndex ciExpected;

    // Create a provider with 3 in depth
    // We still expect the same result since the patches are big
    PatchCoverageProvider provider(
        Geodetic2(M_PI * 2, M_PI * 2), // size at level 0
        Geodetic2(- M_PI, - M_PI), // offset at level 0
        3); // depth

    // A small patch
    patch = GeodeticPatch(
        Geodetic2(0, 0), // Center
        Geodetic2(M_PI / 8, M_PI / 8)); // Halfsize

    // Get its index
    ci = provider.getTileIndex(patch);
    ciExpected = { 3, 3, 3 };

    ASSERT_EQ(ciExpected, ci);

    // A small patch near edge case
    patch = GeodeticPatch(
        Geodetic2(2 * M_PI / 8 + 2 * M_PI / 16 + 0.001, 2 * 2 * M_PI / 8 + 2 * M_PI / 16 - 0.001), // Center
        Geodetic2(M_PI / 8, M_PI / 8)); // Halfsize

    // Get its index
    ci = provider.getTileIndex(patch);
    ciExpected = { 5, 1, 3 };

    // A small patch near edge case other side
    patch = GeodeticPatch(
        Geodetic2(2 * M_PI / 8 + 2 * M_PI / 16 - 0.001, 2 * 2 * M_PI / 8 + 2 * M_PI / 16 + 0.001), // Center
        Geodetic2(M_PI / 8, M_PI / 8)); // Halfsize

                                        // Get its index
    ci = provider.getTileIndex(patch);
    ciExpected = { 6, 2, 3 };

    ASSERT_EQ(ciExpected, ci);
}


TEST_F(PatchCoverageProviderTest, getTileIndexWrapAround) {

    // Allocate data
    GeodeticPatch patch(0, 0, 0, 0);
    ChunkIndex ci;
    ChunkIndex ciExpected;

    // Create a provider with 2 in depth
    // We still expect the same result since the patches are big
    PatchCoverageProvider provider(
        Geodetic2(M_PI * 2, M_PI * 2), // size at level 0
        Geodetic2(-M_PI, -M_PI), // offset at level 0
        2); // depth

    // A small patch
    patch = GeodeticPatch(
        Geodetic2(0, - M_PI + M_PI / 16), // Center
        Geodetic2(M_PI / 8, M_PI / 8)); // Halfsize

    // Get its index
    // We want negative indices
    ci = provider.getTileIndex(patch);
    ciExpected = { -1, 1, 2 };

    ASSERT_EQ(ciExpected, ci);
}
