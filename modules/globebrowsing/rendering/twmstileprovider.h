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

#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/texture.h>

#include <modules/globebrowsing/datastructures/latlon.h>

#include <modules/globebrowsing/rendering/texturetile.h>



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

/*
bool operator==(const openspace::TileIndex& a, const openspace::TileIndex& b) {
	return a.x == b.x && a.y == b.y && a.level == b.level;
}

std::ostream& operator<<(std::ostream& o, const openspace::TileIndex& key) {
	return o << key.x << ", " << key.y << " at level " << key.level;
}


// custom specialization of std::hash can be injected in namespace std
namespace std {
	template<> struct hash<openspace::TileIndex> {
		unsigned long  operator()(openspace::TileIndex const& ti) const {
			return ti.x ^ (ti.y << 16) ^ (ti.level << 21);
		}
	};
}
*/

#include <modules/globebrowsing/datastructures/lrucache.h>



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
		void downloadTileAndPutInCache(const TileIndex&);

		LRUCache<HashKey, std::shared_ptr<Texture>> _tileCache;
	};

}  // namespace openspace

#endif  // __TWMS_TILE_PROVIDER_H__