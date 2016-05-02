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

#ifndef __PATCHCOVERAGEPROVIDER_H__
#define __PATCHCOVERAGEPROVIDER_H__

#include <ghoul/logging/logmanager.h>

#include <modules/globebrowsing/geodetics/geodetic2.h>
#include <modules/globebrowsing/other/tileprovider.h>
#include <ghoul/opengl/texture.h>


//////////////////////////////////////////////////////////////////////////////////////////
//									PATCH COVERAGE PROVIDER							    //
//////////////////////////////////////////////////////////////////////////////////////////

namespace openspace {

    using namespace ghoul::opengl;

    struct PatchCoverage
    {
        using TextureTransformPair = std::pair<std::shared_ptr<Texture>, glm::mat3>;
        TextureTransformPair textureTransformPairs[4];
    };

    class PatchCoverageProvider
    {
    public:
        PatchCoverageProvider(
            std::shared_ptr<TileProvider> tileProvider,
            Geodetic2 sizeLevel0,
            Geodetic2 offsetLevel0,
            int depth);
        ~PatchCoverageProvider();

        /**
            Returns the index of the tile at an appropriate level.
            Appropriate meaning that the tile should be at as high level as possible
            Without the tile being smaller than the patch in lat-lon space.
            The tile is at least as big as the patch.
        */
        GeodeticTileIndex getTileIndex(const GeodeticPatch& patch);

        /**
            A transformation (translation and scaling) from the texture space of a patch
            to the texture space of a tile.
        */
        glm::mat3 getUvTransformationPatchToTile( 
            GeodeticPatch patch,
            const GeodeticTileIndex& tileIndex);

        /**
            Overloaded function
        */
        glm::mat3 getUvTransformationPatchToTile(
            GeodeticPatch patch,
            GeodeticPatch tile);

        PatchCoverage getCoverage(GeodeticPatch patch);

    private:

        std::shared_ptr<TileProvider> _tileProvider;
        Geodetic2 _sizeLevel0;
        Geodetic2 _offsetLevel0;
        int _depth;

        std::shared_ptr<Texture> _tempTexture;

        
    };

}  // namespace openspace

#endif  // __PATCHCOVERAGEPROVIDER_H__