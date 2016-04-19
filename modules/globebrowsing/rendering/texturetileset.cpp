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

#include <ghoul/opengl/texturemanager.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>

#include <ghoul/logging/logmanager.h>


#include <glm/glm.hpp>





namespace {
	const std::string _loggerCat = "TextureTileSet";
}

namespace openspace {

	TextureTileSet::TextureTileSet(LatLon sizeLevel0, LatLon offsetLevel0, int depth)
		: _sizeLevel0(sizeLevel0)
		, _offsetLevel0(offsetLevel0)
		, _depth(depth)
	{
		
		// Set e texture to test
		_testTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath("textures/earth_bluemarble.jpg")));
		if (_testTexture) {
			LDEBUG("Loaded texture from '" << "textures/earth_bluemarble.jpg" << "'");
			_testTexture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			// TODO: AnisotropicMipMap crashes on ATI cards ---abock
			//_testTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
			_testTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		}
		/*
		int dataSize = _testTexture->width() * _testTexture->height() * _testTexture->bytesPerPixel();
		GLubyte* data = new GLubyte[dataSize];
		for (size_t i = 0; i < dataSize; i++)
		{
			data[i] = unsigned char(i / float(dataSize) * 255);
		}
		_testTexture->setPixelData(data);
		_testTexture->uploadTexture();
		*/
		
	}

	TextureTileSet::~TextureTileSet(){

	}

	TileIndex TextureTileSet::getTileIndex(LatLonPatch patch) {
		int level = log2(static_cast<int>(glm::max(
			_sizeLevel0.lat / (patch.size().lat),
			_sizeLevel0.lon / (patch.size().lon))));
		level = glm::min(level, _depth);
		Vec2 tileSize = _sizeLevel0.toLonLatVec2() / pow(2, level);
		Vec2 nw = patch.northWestCorner().toLonLatVec2();
		Vec2 offset = _offsetLevel0.toLonLatVec2();
		glm::ivec2 tileIndexXY = (nw - offset) / tileSize;

		// Flip y since indices increase from top to bottom
		tileIndexXY.y *= -1;

		TileIndex tileIndex = { tileIndexXY.x, tileIndexXY.y, level };
		return tileIndex;
	}

	std::shared_ptr<Texture> TextureTileSet::getTile(LatLonPatch patch) {
		return getTile(getTileIndex(patch));
	}

	std::shared_ptr<Texture> TextureTileSet::getTile(const TileIndex& tileIndex) {
		return _testTexture;
	}

	LatLonPatch TextureTileSet::getTilePositionAndScale(const TileIndex& tileIndex) {
		LatLon tileSize = LatLon(
			_sizeLevel0.lat / pow(2, tileIndex.level),
			_sizeLevel0.lon / pow(2, tileIndex.level));
		LatLon northWest = LatLon(
			_offsetLevel0.lat + tileIndex.y * tileSize.lat,
			_offsetLevel0.lon + tileIndex.x * tileSize.lon);
		
		return LatLonPatch(
			LatLon(northWest.lat - tileSize.lat / 2, northWest.lon + tileSize.lon / 2),
			LatLon(tileSize.lat / 2, tileSize.lon / 2));
	}

	glm::mat3 TextureTileSet::getUvTransformationPatchToTile(
		LatLonPatch patch,
		const TileIndex& tileIndex)
	{
		LatLonPatch tile = getTilePositionAndScale(tileIndex);
		Vec2 posDiff =
			patch.southWestCorner().toLonLatVec2() - 
			tile.southWestCorner().toLonLatVec2();
		
		glm::mat3 invTileScale = glm::mat3(
			{1 / (tile.halfSize().lon * 2),	0,								0,
			0,								1 / (tile.halfSize().lat * 2),	0,
			0,								0,								1});

		glm::mat3 globalTranslation = glm::mat3(
		{	1,			0,			0,
			0,			1,			0,
			posDiff.x,	posDiff.y,	1 });
		
		glm::mat3 patchScale = glm::mat3(
		{ (patch.halfSize().lon * 2),	0,								0,
			0,							(patch.halfSize().lat * 2),		0,
			0,							0,								1 });

		return invTileScale * globalTranslation * patchScale;
	}

}  // namespace openspace
