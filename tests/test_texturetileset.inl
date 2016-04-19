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

#include <modules/globebrowsing/rendering/texturetileset.h>

#define _USE_MATH_DEFINES
#include <math.h>
#include <glm/glm.hpp>

class TextureTileSetTest : public testing::Test {};

using namespace openspace;

TEST_F(TextureTileSetTest, getTileIndexLevel) {

	// Create a tile set with maximum depth 0
	TextureTileSet tileSet(LatLon(M_PI, M_PI * 2), LatLon(M_PI / 2, - M_PI), 0);

	LatLonPatch patch(LatLon(0, 0), LatLon(M_PI / 16, M_PI / 8));
	TileIndex tileIndex0 = tileSet.getTileIndex(patch);

	// Maximum level is 0
	ASSERT_EQ(tileIndex0.level, 0);


	// Create a tile set with maximum depth 10
	TextureTileSet tileSetDepth10(LatLon(M_PI, M_PI * 2), LatLon(M_PI / 2, - M_PI), 10);

	// A big tile that covers the whole latlon space
	LatLonPatch patchBig(LatLon(0, 0), LatLon(M_PI / 2, M_PI));
	tileIndex0 = tileSetDepth10.getTileIndex(patchBig);

	// Should return 0 since the tile covers the whole latlon space
	ASSERT_EQ(tileIndex0.level, 0);

	
	// An edge case tile that covers a fourth of the latlon space
	LatLonPatch patchEdgeCase(LatLon(0, 0), LatLon(M_PI / 4, M_PI / 2));
	TileIndex tileIndex1 = tileSetDepth10.getTileIndex(patchEdgeCase);

	// Now it can go up a level
	ASSERT_EQ(tileIndex1.level, 1);


	// Bigger than the edge case
	LatLonPatch patchEdgeCaseBigger(LatLon(0, 0), LatLon(M_PI / 4 + 0.001, M_PI / 2 + 0.001));
	tileIndex0 = tileSetDepth10.getTileIndex(patchEdgeCaseBigger);

	// Should return 0 again
	ASSERT_EQ(tileIndex0.level, 0);
}

TEST_F(TextureTileSetTest, getTileIndexXY) {

	// Create a tile set with maximum depth 0
	TextureTileSet tileSet(LatLon(M_PI, M_PI * 2), LatLon(M_PI / 2, - M_PI), 0);

	LatLonPatch patch(LatLon(0, 0), LatLon(M_PI / 16, M_PI / 8));
	TileIndex tileIndex0 = tileSet.getTileIndex(patch);

	// Maximum level is 0 so the x y indices should also be 0
	ASSERT_EQ(tileIndex0.x, 0);
	ASSERT_EQ(tileIndex0.y, 0);


	// Create a tile set with maximum depth 10
	TextureTileSet tileSetDepth10(LatLon(M_PI, M_PI * 2), LatLon(M_PI / 2, - M_PI), 10);

	// A big tile that covers the whole latlon space
	LatLonPatch patchBig(LatLon(0, 0), LatLon(M_PI / 2, M_PI));
	tileIndex0 = tileSetDepth10.getTileIndex(patchBig);

	// Should return 0 in x and y since the tile covers the whole latlon space
	ASSERT_EQ(tileIndex0.x, 0);
	ASSERT_EQ(tileIndex0.y, 0);


	// A tile that covers a fourth of the latlon space
	LatLonPatch patchEdgeCase(LatLon(0, 0), LatLon(M_PI / 4, M_PI / 2));
	TileIndex tileIndex1 = tileSetDepth10.getTileIndex(patchEdgeCase);

	// Now it can go up a level (1)
	// Since the position is 0, 0 it has 0, 0, in x, y index
	ASSERT_EQ(tileIndex1.x, 0);
	ASSERT_EQ(tileIndex1.y, 0);


	// A smaller edge case tile
	LatLonPatch patchEdgeCase2(LatLon(0, 0), LatLon(M_PI / 8, M_PI / 4));
	TileIndex tileIndex11 = tileSetDepth10.getTileIndex(patchEdgeCase2);

	// Now it can go up two levels (2)
	// Since the position is 0, 0 it now has 1, 1, in x, y index
	// (north west corner is in that tile)
	ASSERT_EQ(tileIndex11.x, 1);
	ASSERT_EQ(tileIndex11.y, 1);
}


TEST_F(TextureTileSetTest, getUvTransformationPatchToTile) {
	// Create a tile set with maximum depth 0
	TextureTileSet tileSet(LatLon(M_PI, M_PI * 2), LatLon(M_PI / 2, -M_PI), 0);

	// Create a patch that covers the whole latlon space
	LatLonPatch patch(LatLon(0, 0), LatLon(M_PI / 2, M_PI));

	// Should be a 1:1 mapping
	glm::mat3 patchToTileTransform =
		tileSet.getUvTransformationPatchToTile(patch, { 0, 0, 0 });

	ASSERT_EQ(patchToTileTransform, glm::mat3(1));
	
	// Create a smaller patch in the upper west side
	patch = LatLonPatch(LatLon(M_PI / 4, - M_PI / 2), LatLon(M_PI / 4, M_PI / 2));
	patchToTileTransform =
		tileSet.getUvTransformationPatchToTile(patch, { 0,0,0 });

	glm::vec2 uvPatchSpace = glm::vec2(0, 0);
	glm::vec2 uvTileSpace = glm::vec2(patchToTileTransform * glm::vec3(uvPatchSpace, 1));
}
