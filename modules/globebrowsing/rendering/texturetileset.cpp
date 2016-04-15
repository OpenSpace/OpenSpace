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

#include <modules/globebrowsing/rendering/texturetileset.h>

#include <glm/glm.hpp>

namespace openspace {
	TextureTileSet::TextureTileSet()
	{
	}

	TextureTileSet::~TextureTileSet()
	{
	}

	glm::ivec3 TextureTileSet::getTileIndex(LatLonPatch patch)
	{
		int level = log2(static_cast<int>(glm::max(
			sizeLevel0.lat / patch.halfSize().lat * 2,
			sizeLevel0.lon / patch.halfSize().lon * 2)));
		Vec2 TileSize = sizeLevel0.toLonLatVec2() / pow(2, level);
		glm::ivec2 tileIndex = -(patch.northWestCorner().toLonLatVec2() + offsetLevel0.toLonLatVec2()) / TileSize;
		return glm::ivec3(tileIndex, level);
	}

	TextureTile TextureTileSet::getTile(LatLonPatch patch)
	{
		return getTile(getTileIndex(patch));
	}

	TextureTile TextureTileSet::getTile(glm::ivec3 tileIndex)
	{
		return TextureTile();
	}

	LatLonPatch TextureTileSet::getTilePositionAndScale(glm::ivec3 tileIndex)
	{
		LatLon tileSize = LatLon(
			sizeLevel0.lat / pow(2, tileIndex.z),
			sizeLevel0.lon / pow(2, tileIndex.z));
		LatLon northWest = LatLon(
			offsetLevel0.lat + tileIndex.y * tileSize.lat,
			offsetLevel0.lon + tileIndex.x * tileSize.lon);
		
		return LatLonPatch(
			LatLon(northWest.lat + tileSize.lat / 2, northWest.lon + tileSize.lon / 2),
			LatLon(tileSize.lat / 2, tileSize.lon / 2));
	}

}  // namespace openspace
