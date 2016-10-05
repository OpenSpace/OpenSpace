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

#ifndef __TILE_SELECTOR_H__
#define __TILE_SELECTOR_H__


#include <modules/globebrowsing/tile/tileindex.h>
#include <modules/globebrowsing/tile/tileprovidermanager.h>

#include <vector>



namespace openspace {

    struct TileUvTransform {
        glm::vec2 uvOffset;
        glm::vec2 uvScale;
    };

    struct TileAndTransform {
        Tile tile;
        TileUvTransform uvTransform;
    };


    class TileSelector {
    public:
        static TileAndTransform getHighestResolutionTile(TileProvider* tileProvider, TileIndex tileIndex, int parents = 0);
        static TileAndTransform getHighestResolutionTile(const TileProviderGroup& tileProviderGroup, TileIndex tileIndex);
        static std::vector<TileAndTransform> getTilesSortedByHighestResolution(const TileProviderGroup&, const TileIndex& tileIndex);


        struct CompareResolution {
            bool operator() (const TileAndTransform& a, const TileAndTransform& b);
        };

        static const CompareResolution HIGHEST_RES;

    private:
        static void ascendToParent(TileIndex& tileIndex, TileUvTransform& uv);

    };

    


}  // namespace openspace


#endif  // __TILE_SELECTOR_H__