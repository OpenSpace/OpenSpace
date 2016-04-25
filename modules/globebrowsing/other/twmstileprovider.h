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

#ifndef __TWMS_TILE_PROVIDER_H__
#define __TWMS_TILE_PROVIDER_H__

#include <modules/globebrowsing/other/lrucache.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

#include <openspace/engine/downloadmanager.h>



//////////////////////////////////////////////////////////////////////////////////////
//									TILE INDEX										//
//////////////////////////////////////////////////////////////////////////////////////

namespace openspace {
	using HashKey = unsigned long;

	struct TileIndex {
		int x, y, level;

		HashKey hashKey() const {
			return x ^ (y << 16) ^ (level << 21);
		}
	};
}


//////////////////////////////////////////////////////////////////////////////////////////
//									TWMS TILE PROVIDER									//
//////////////////////////////////////////////////////////////////////////////////////////


namespace openspace {
	using namespace ghoul::opengl;

	class TwmsTileProvider
	{
	public:
		TwmsTileProvider();
		~TwmsTileProvider();

		std::shared_ptr<Texture> getTile(const TileIndex& tileIndex);


	private:
		std::shared_ptr<DownloadManager::FileFuture> requestTile(const TileIndex&);
		std::shared_ptr<Texture> loadAndInitTextureDisk(std::string filePath);

		LRUCache<HashKey, std::shared_ptr<Texture>> _tileCache;
		LRUCache<HashKey, std::shared_ptr<DownloadManager::FileFuture>> _fileFutureCache;
	};

}  // namespace openspace

#endif  // __TWMS_TILE_PROVIDER_H__