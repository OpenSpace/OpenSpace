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

#include <openspace/engine/downloadmanager.h>

#include <ghoul/opengl/texturemanager.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>

#include <ghoul/logging/logmanager.h>


#include <glm/glm.hpp>

#include <sstream>




namespace {
	const std::string _loggerCat = "TwmsTileProvider";
}

namespace openspace {

	TwmsTileProvider::TwmsTileProvider()
	: _tileCache(500) // setting cache size
	{
		int downloadApplicationVersion = 1;
		DownloadManager::initialize("../tmp_openspace_downloads/", downloadApplicationVersion);
	}

	TwmsTileProvider::~TwmsTileProvider(){

	}


	std::shared_ptr<Texture> TwmsTileProvider::getTile(const TileIndex& tileIndex) {
		HashKey hashkey = tileIndex.hashKey();
		if (_tileCache.exist(hashkey)) {
			return _tileCache.get(hashkey);
		}
		else {
			downloadTileAndPutInCache(tileIndex);
		}
		return nullptr;
	}



	void TwmsTileProvider::downloadTileAndPutInCache(const TileIndex& tileIndex) {
		// download tile
		std::stringstream ss;
		std::string baseUrl = "https://map1c.vis.earthdata.nasa.gov/wmts-geo/wmts.cgi?TIME=2016-04-17&layer=MODIS_Terra_CorrectedReflectance_TrueColor&tilematrixset=EPSG4326_250m&Service=WMTS&Request=GetTile&Version=1.0.0&Format=image%2Fjpeg";
		ss << baseUrl;
		ss << "&TileMatrix=" << tileIndex.level;
		ss << "&TileCol=" << tileIndex.x;
		ss << "&TileRow=" << tileIndex.y;
		std::string twmsRequestUrl = ss.str();

		using ghoul::filesystem::File;

		std::string filename = "tiles/tile" + twmsRequestUrl.substr(baseUrl.length()) + ".jpg";
		File localTileFile(filename);
		bool overrideFile = true;



		struct OnTileDownloaded {
			HashKey key;
			LRUCache<HashKey, std::shared_ptr<Texture>> * tileCache;

			OnTileDownloaded(HashKey key, LRUCache<HashKey, std::shared_ptr<Texture>> * tileCache)
				: key(key)
				, tileCache(tileCache)
			{
			
			}

			void operator()(const DownloadManager::FileFuture& ff) const {
				LDEBUG("Download of tile with hashkey " << key << " done!");
				auto textureReader = ghoul::io::TextureReader::ref();
				std::shared_ptr<Texture> texture = std::move(textureReader.loadTexture(absPath(ff.filePath)));
				tileCache->put(key, texture);
				LDEBUG("Cache updated");
			}
		};


		OnTileDownloaded onTileDownloaded(tileIndex.hashKey(), &_tileCache);

		std::shared_ptr<DownloadManager::FileFuture> ff = DownloadManager::ref().downloadFile(twmsRequestUrl, localTileFile, overrideFile, onTileDownloaded);

	}


}  // namespace openspace
