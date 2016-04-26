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

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/geodetics/ellipsoid.h>
#include <modules/globebrowsing/other/twmstileprovider.h>


//////////////////////////////////////////////////////////////////////////////////////////
//									TEXTURE TILE SET									//
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {
	
	using namespace ghoul::opengl;

	class TextureTileSet
	{
	public:
		TextureTileSet(
			Geodetic2 sizeLevel0,
			Geodetic2 offsetLevel0,
			int depth);
		~TextureTileSet();

		/**
			Returns the index of the tile at an appropriate level.
			Appropriate meaning that the tile should be at as high level as possible
			Without the tile being smaller than the patch in lat-lon space.
			The tile is at least as big as the patch.
		*/
		TileIndex getTileIndex(GeodeticPatch patch);

		/**
			Returns a texture that can be used for the specified patch
		*/
		std::shared_ptr<Texture> getTile(GeodeticPatch patch);

		/**
			Returns the texture for the given tile index. The indices reaches from left
			to right and top to bottom while the texture coordinates and the latlon
			coordinates reaches from left to right and bottom to top.
		*/
		std::shared_ptr<Texture> getTile(const TileIndex& tileIndex);

		/**
			A tile can be defined with a tile index or a LatLonPatch which defines
			the position and the size of the tile.
		*/
		GeodeticPatch getTilePositionAndScale(
			const TileIndex& tileIndex,
			const Ellipsoid& ellipsoid);

		/**
			A transformation (translation and scaling) from the texture space of a patch
			to the texture space of a tile.
		*/
		glm::mat3 getUvTransformationPatchToTile( 
			GeodeticPatch patch,
			const TileIndex& tileIndex);

		/**
			Overloaded function
		*/
		glm::mat3 getUvTransformationPatchToTile(
			GeodeticPatch patch,
			GeodeticPatch tile);

	private:
		Geodetic2 _sizeLevel0;
		Geodetic2 _offsetLevel0;
		int _depth;

		std::shared_ptr<Texture> _testTexture;
	};

}  // namespace openspace

#endif  // __TEXTURETILESET_H__