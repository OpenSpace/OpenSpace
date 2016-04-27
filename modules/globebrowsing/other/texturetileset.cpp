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

#include <modules/globebrowsing/other/texturetileset.h>

#include <modules/globebrowsing/geodetics/ellipsoid.h>
#include <modules/globebrowsing/other/gdaldataconverter.h>

#include <ghoul/opengl/texturemanager.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>

#include <ghoul/logging/logmanager.h>

#include "gdal_priv.h"
#include "cpl_conv.h" // for CPLMalloc()

#include <glm/glm.hpp>

namespace {
	const std::string _loggerCat = "TextureTileSet";
}

namespace openspace {

	TextureTileSet::TextureTileSet(
		Geodetic2 sizeLevel0,
		Geodetic2 offsetLevel0,
		int depth)
		: _sizeLevel0(sizeLevel0)
		, _offsetLevel0(offsetLevel0)
		, _depth(depth)
	{
		

		// Read using GDAL

		/*
		GDALDataset  *poDataset;
		GDALAllRegister();
		poDataset = (GDALDataset *)GDALOpen(absPath("textures/earth_bluemarble.jpg").c_str(), GA_ReadOnly);
		assert(poDataset != NULL);
		GdalDataConverter conv;

		_testTexture = conv.convertToOpenGLTexture(poDataset);

		_testTexture->uploadTexture();
		_testTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		*/

		
		// Set e texture to test
		std::string fileName = "textures/earth_bluemarble.jpg";
		//std::string fileName = "../../../build/tiles/tile5_8_12.png";
		//std::string fileName = "tile5_8_12.png";
		_testTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath(fileName)));
		
		if (_testTexture) {
			LDEBUG("Loaded texture from '" << "textures/earth_bluemarble.jpg" << "'");
			_testTexture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			// TODO: AnisotropicMipMap crashes on ATI cards ---abock
			//_testTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
			_testTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		}
		
	}

	TextureTileSet::~TextureTileSet(){

	}

	TileIndex TextureTileSet::getTileIndex(GeodeticPatch patch) {
		// Calculate the level of the index depanding on the size of the incoming patch.
		// The level is as big as possible (as far down as possible) but it can't be
		// too big since at maximum four tiles should be used to cover a patch
		int level = log2(static_cast<int>(glm::max(
			_sizeLevel0.lat / (patch.size().lat),
			_sizeLevel0.lon / (patch.size().lon))));
		
		// If the depth is not big enough, the level must be clamped.
		level = glm::min(level, _depth);
		
		// Calculate the index in x y where the tile should be positioned
		Vec2 tileSize = _sizeLevel0.toLonLatVec2() / pow(2, level);
		Vec2 nw = patch.northWestCorner().toLonLatVec2();
		Vec2 offset = _offsetLevel0.toLonLatVec2();
		glm::ivec2 tileIndexXY = (nw - offset) / tileSize;

		// Flip y since indices increase from top to bottom
		tileIndexXY.y *= -1;

		// Create the tileindex
		TileIndex tileIndex = { tileIndexXY.x, tileIndexXY.y, level };
		return tileIndex;
	}

	std::shared_ptr<Texture> TextureTileSet::getTile(GeodeticPatch patch) {
		return getTile(getTileIndex(patch));
	}

	std::shared_ptr<Texture> TextureTileSet::getTile(const TileIndex& tileIndex) {
		return _testTexture;
	}

	GeodeticPatch TextureTileSet::getTilePositionAndScale(
		const TileIndex& tileIndex) {
		Geodetic2 tileSize = Geodetic2(
			_sizeLevel0.lat / pow(2, tileIndex.level),
			_sizeLevel0.lon / pow(2, tileIndex.level));
		Geodetic2 northWest = Geodetic2(
			_offsetLevel0.lat + tileIndex.y * tileSize.lat,
			_offsetLevel0.lon + tileIndex.x * tileSize.lon);
		
		return GeodeticPatch(
			Geodetic2(northWest.lat - tileSize.lat / 2, northWest.lon + tileSize.lon / 2),
			Geodetic2(tileSize.lat / 2, tileSize.lon / 2));
	}

	glm::mat3 TextureTileSet::getUvTransformationPatchToTile(
		GeodeticPatch patch,
		const TileIndex& tileIndex)
	{
		GeodeticPatch tile = getTilePositionAndScale(tileIndex);
		return getUvTransformationPatchToTile(patch, tile);
	}

	glm::mat3 TextureTileSet::getUvTransformationPatchToTile(
		GeodeticPatch patch,
		GeodeticPatch tile)
	{
		Vec2 posDiff =
			patch.southWestCorner().toLonLatVec2() -
			tile.southWestCorner().toLonLatVec2();

		glm::mat3 invTileScale = glm::mat3(
		{	1 / (tile.halfSize().lon * 2),	0,								0,
			0,								1 / (tile.halfSize().lat * 2),	0,
			0,								0,								1 });

		glm::mat3 globalTranslation = glm::mat3(
		{	1,			0,			0,
			0,			1,			0,
			posDiff.x,	posDiff.y,	1 });

		glm::mat3 patchScale = glm::mat3(
		{	(patch.halfSize().lon * 2),	0,								0,
			0,							(patch.halfSize().lat * 2),		0,
			0,							0,								1 });

		return invTileScale * globalTranslation * patchScale;
	}

}  // namespace openspace
