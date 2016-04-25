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


#include <modules/globebrowsing/rendering/twmstileprovider.h>

#include <openspace/engine/downloadmanager.h>

#include <ghoul/io/texture/texturereader.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

#include <sstream>




namespace {
	const std::string _loggerCat = "TwmsTileProvider";
}

namespace openspace {

	TwmsTileProvider::TwmsTileProvider()
	: _tileCache(5000) // setting cache size
	, _fileFutureCache(5000) // setting cache size
	{
		int downloadApplicationVersion = 1;
		if (!DownloadManager::isInitialized()) {
			DownloadManager::initialize("../tmp_openspace_downloads/", downloadApplicationVersion);
		}
		
	}

	TwmsTileProvider::~TwmsTileProvider(){

	}


	std::shared_ptr<Texture> TwmsTileProvider::getTile(const TileIndex& tileIndex) {
		HashKey hashkey = tileIndex.hashKey();
		if (_tileCache.exist(hashkey)) {
			return _tileCache.get(hashkey);
		}
		else if (_fileFutureCache.exist(hashkey)) {
			if (_fileFutureCache.get(hashkey)->isFinished) {
				std::string fileName = _fileFutureCache.get(hashkey)->filePath;
				std::string filePath = "tiles/" + fileName;
				std::shared_ptr<Texture> texture = loadAndInitTextureDisk(filePath);
				LDEBUG("Downloaded " << fileName);
				_tileCache.put(hashkey, texture);
			}
		}
		else {
			std::shared_ptr<DownloadManager::FileFuture> fileFuture = requestTile(tileIndex);
			_fileFutureCache.put(hashkey, fileFuture);
		}
		return nullptr;
	}

	std::shared_ptr<Texture> TwmsTileProvider::loadAndInitTextureDisk(std::string filePath) {
		auto textureReader = ghoul::io::TextureReader::ref();
		std::shared_ptr<Texture> texture = std::move(textureReader.loadTexture(absPath(filePath)));

		// upload to gpu
		texture->uploadTexture();
		texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
		texture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
		return texture;
	}



	std::shared_ptr<DownloadManager::FileFuture> TwmsTileProvider::requestTile(const TileIndex& tileIndex) {
		// download tile
		std::stringstream ss;
		//std::string baseUrl = "https://map1c.vis.earthdata.nasa.gov/wmts-geo/wmts.cgi?TIME=2016-04-17&layer=MODIS_Terra_CorrectedReflectance_TrueColor&tilematrixset=EPSG4326_250m&Service=WMTS&Request=GetTile&Version=1.0.0&Format=image%2Fjpeg";
		//ss << baseUrl;
		//ss << "&TileMatrix=" << tileIndex.level;
		//ss << "&TileCol=" << tileIndex.x;
		//ss << "&TileRow=" << tileIndex.y;
		// https://map1c.vis.earthdata.nasa.gov/wmts-geo/wmts.cgi?TIME=2016-04-17&layer=MODIS_Terra_CorrectedReflectance_TrueColor&tilematrixset=EPSG4326_250m&Service=WMTS&Request=GetTile&Version=1.0.0&Format=image%2Fjpeg&TileMatrix=0&TileCol=0&TileRow=0

		std::string baseUrl = "http://mesonet.agron.iastate.edu/cache/tile.py/1.0.0/nexrad-n0q-900913";
		ss << baseUrl;
		ss << "/" << tileIndex.level;
		ss << "/" << tileIndex.x;
		ss << "/" << tileIndex.y;
		ss << ".png?1461277159335";
		// http://mesonet.agron.iastate.edu/cache/tile.py/1.0.0/nexrad-n0q-900913/5/8/12.png?
		std::string twmsRequestUrl = ss.str();
		
		ss = std::stringstream();
		ss << tileIndex.level;
		ss << "_" << tileIndex.x;
		ss << "_" << tileIndex.y;
		std::string filePath = "tiles/tile" + ss.str() + ".png";

		using ghoul::filesystem::File;
		File localTileFile(filePath);
		bool overrideFile = true;


		/*
		struct OnTileDownloaded {
			HashKey key;
			LRUCache<HashKey, std::shared_ptr<Texture>> * tileCache;

			OnTileDownloaded(HashKey key, LRUCache<HashKey, std::shared_ptr<Texture>> * tileCache)
				: key(key)
				, tileCache(tileCache)
			{
			
			}

			void operator()(const DownloadManager::FileFuture& ff) const {
				//LDEBUG("Download of tile with hashkey " << key << " done!");
				auto textureReader = ghoul::io::TextureReader::ref();
				std::string relFilePath = "tiles/" + ff.filePath;
				std::shared_ptr<Texture> texture = std::move(textureReader.loadTexture(absPath(relFilePath)));

				// upload to gpu
				texture->uploadTexture();
				texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);

				tileCache->put(key, texture);
				LDEBUG("Cache updated");
			}
		};
		OnTileDownloaded onTileDownloaded(tileIndex.hashKey(), &_tileCache);
		*/

		std::shared_ptr<DownloadManager::FileFuture> fileFuture = DownloadManager::ref().downloadFile(twmsRequestUrl, localTileFile, overrideFile);
		return fileFuture;

	}


}  // namespace openspace
