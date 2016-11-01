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

#ifndef __CULLING_H__
#define __CULLING_H__

#include <memory>
#include <glm/glm.hpp>

// open space includes

#include <openspace/rendering/renderable.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#include <modules/globebrowsing/geometry/ellipsoid.h>
#include <modules/globebrowsing/geometry/aabb.h>

namespace openspace {
namespace globebrowsing {

    using namespace glm;

    class Chunk;

    class ChunkCuller {
    public:
        virtual void update() { }

        /**
         * Determine if the Chunk is cullable. That is return true if removing the
         * Chunk 'culling it' will not have any result on the final rendering. Culling
         * it will make the rendering faster.
        */
        virtual bool isCullable(const Chunk& chunk, const RenderData& renderData) = 0;
    };

    /**
     * Culls all chunks that are completely outside the view frustum.
     *
     * The frustum culling uses a 2D axis aligned bounding box for the Chunk in
     * screen space. This is calculated from a bounding polyhedron bounding the
     * Chunk. Hence the culling will not be 'perfect' but fast and good enough for
     * culling chunks outside the frustum with some margin.
    */
    class FrustumCuller : public ChunkCuller {
    public:
        /**
         * \param viewFrustum is the view space in normalized device coordinates space.
         Hence it is an axis aligned bounding box and not a real frustum.
        */
        FrustumCuller(const AABB3 viewFrustum);
        ~FrustumCuller();

        virtual bool isCullable(const Chunk& chunk, const RenderData& renderData);

    private:
        const AABB3 _viewFrustum;
    };

    /**
     * In this implementation of the horizon culling, the closer the ellipsoid is to a
     * sphere, the better this will make the culling. Using the minimum radius to
     * be safe. This means that if the ellipsoid has high difference between radii,
     * splitting might accur even though it may not be needed.
    */
    class HorizonCuller : public ChunkCuller {
    public:
        HorizonCuller();
        ~HorizonCuller();

        virtual bool isCullable(const Chunk& chunk, const RenderData& renderData);

        bool isCullable(const Vec3& cameraPosition, const Vec3& globePosition,
            const Vec3& objectPosition, Scalar objectBoundingSphereRadius,
            Scalar minimumGlobeRadius);
    };

} // namespace globebrowsing
} // namespace openspace

#endif  // __CULLING_H__