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

#ifndef __TEXTURETILESET_H__
#define __TEXTURETILESET_H__

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

#include <modules/globebrowsing/datastructures/latlon.h>
#include <modules/globebrowsing/rendering/texturetile.h>

namespace openspace {

	using namespace ghoul::opengl;

	class TextureTileSet
	{
	public:
		TextureTileSet(LatLon sizeLevel0, LatLon offsetLevel0, int depth);
		~TextureTileSet();

		/// Returns the index of the tile at an appropriate level.
		/// Appropriate meaning that the tile should be at as high level as possible
		/// Without the tile being smaller than the patch in lat-lon space.
		/// The tile is at least as big as the patch.
		glm::ivec3 getTileIndex(LatLonPatch patch);
		std::shared_ptr<Texture> getTile(LatLonPatch patch);
		std::shared_ptr<Texture> getTile(glm::ivec3 tileIndex);
		LatLonPatch getTilePositionAndScale(glm::ivec3 tileIndex);
		glm::mat3 getUvTransformationPatchToTile(LatLonPatch patch, glm::ivec3 tileIndex);
	private:
		LatLon _sizeLevel0;
		LatLon _offsetLevel0;
		int _depth;

		std::shared_ptr<Texture> _testTexture;
	};

}  // namespace openspace

#endif  // __TEXTURETILESET_H__