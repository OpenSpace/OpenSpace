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
	TextureTileSet::TextureTileSet()
	{		
		_testTexture = std::move(ghoul::io::TextureReader::ref().loadTexture(absPath("textures/earth_bluemarble.jpg")));
		if (_testTexture) {
			LDEBUG("Loaded texture from '" << "textures/earth_bluemarble.jpg" << "'");
			_testTexture->uploadTexture();

			// Textures of planets looks much smoother with AnisotropicMipMap rather than linear
			// TODO: AnisotropicMipMap crashes on ATI cards ---abock
			//_testTexture->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
			_testTexture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		}
		
	}

	TextureTileSet::~TextureTileSet()
	{
	}

	glm::ivec3 TextureTileSet::getTileIndex(LatLonPatch patch)
	{
		int level = log2(static_cast<int>(glm::max(
			_sizeLevel0.lat / patch.halfSize().lat * 2,
			_sizeLevel0.lon / patch.halfSize().lon * 2)));
		Vec2 TileSize = _sizeLevel0.toLonLatVec2() / pow(2, level);
		glm::ivec2 tileIndex = -(patch.northWestCorner().toLonLatVec2() + _offsetLevel0.toLonLatVec2()) / TileSize;

		return glm::ivec3(tileIndex, level);
	}

	std::shared_ptr<Texture> TextureTileSet::getTile(LatLonPatch patch)
	{
		return getTile(getTileIndex(patch));
	}

	std::shared_ptr<Texture> TextureTileSet::getTile(glm::ivec3 tileIndex)
	{
		return _testTexture;
	}

	LatLonPatch TextureTileSet::getTilePositionAndScale(glm::ivec3 tileIndex)
	{
		LatLon tileSize = LatLon(
			_sizeLevel0.lat / pow(2, tileIndex.z),
			_sizeLevel0.lon / pow(2, tileIndex.z));
		LatLon northWest = LatLon(
			_offsetLevel0.lat + tileIndex.y * tileSize.lat,
			_offsetLevel0.lon + tileIndex.x * tileSize.lon);
		
		return LatLonPatch(
			LatLon(northWest.lat + tileSize.lat / 2, northWest.lon + tileSize.lon / 2),
			LatLon(tileSize.lat / 2, tileSize.lon / 2));
	}

}  // namespace openspace
