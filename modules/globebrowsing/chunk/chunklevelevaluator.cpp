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


#include <ghoul/misc/assert.h>

#include <openspace/engine/openspaceengine.h>

#include <modules/globebrowsing/chunk/chunklevelevaluator.h>
#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/chunk/chunkedlodglobe.h>
#include <modules/globebrowsing/tile/layeredtextures.h>


#include <algorithm>

namespace {
    const std::string _loggerCat = "ChunkLevelEvaluator";
}

namespace openspace {

    
    int EvaluateChunkLevelByDistance::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
        // Calculations are done in the reference frame of the globe. Hence, the camera
        // position needs to be transformed with the inverse model matrix
        glm::dmat4 inverseModelTransform = chunk.owner()->inverseModelTransform();
        ChunkedLodGlobe const * globe = chunk.owner();
        const Ellipsoid& ellipsoid = globe->ellipsoid();

        Vec3 cameraPosition =
            glm::dvec3(inverseModelTransform * glm::dvec4(data.camera.positionVec3(), 1));
        Geodetic2 pointOnPatch = chunk.surfacePatch().closestPoint(
            ellipsoid.cartesianToGeodetic2(cameraPosition));
        
        Chunk::BoundingHeights heights = chunk.getBoundingHeights();

        Vec3 patchPosition = ellipsoid.cartesianSurfacePosition(pointOnPatch);
        Vec3 cameraToChunk = patchPosition - cameraPosition;


        // Calculate desired level based on distance
        Scalar distanceToPatch = glm::length(cameraToChunk);
        Scalar distance = distanceToPatch - heights.min; // distance to actual minimum heights

        Scalar scaleFactor = globe->lodScaleFactor * ellipsoid.minimumRadius();
        Scalar projectedScaleFactor = scaleFactor / distance;
        int desiredLevel = ceil(log2(projectedScaleFactor));
        return desiredLevel;
    }

    int EvaluateChunkLevelByProjectedArea::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
        // Calculations are done in the reference frame of the globe. Hence, the camera
        // position needs to be transformed with the inverse model matrix
        glm::dmat4 inverseModelTransform = chunk.owner()->inverseModelTransform();

        ChunkedLodGlobe const * globe = chunk.owner();
        const Ellipsoid& ellipsoid = globe->ellipsoid();
        Vec3 cameraPosition =
            glm::dvec3(inverseModelTransform * glm::dvec4(data.camera.positionVec3(), 1));
        Vec3 cameraToEllipsoidCenter = - cameraPosition;

        Geodetic2 camPos = ellipsoid.cartesianToGeodetic2(cameraPosition);
        /*
        struct CornerDist {
            Geodetic2 corner;
            float dist;
        };

        struct {
            bool operator()(const CornerDist& a, const CornerDist& b) {
                return a.dist < b.dist;
            }
        } byDist;

        std::vector<CornerDist> cornerDists(4);
        for (size_t i = 0; i < 4; i++) {
            const Geodetic2& c = chunk.surfacePatch().getCorner((Quad)i);
            Geodetic2 diff = (camPos - c);
            float latDiff = fAngle::fromRadians(diff.lat).getNormalizedAround(fAngle::ZERO).asRadians();
            float lonDiff = fAngle::fromRadians(diff.lon).getNormalizedAround(fAngle::ZERO).asRadians();
            cornerDists[i].corner = c;
            cornerDists[i].dist = latDiff*latDiff + lonDiff*lonDiff;;
        }

        std::sort(cornerDists.begin(), cornerDists.end(), byDist);

        Chunk::BoundingHeights heights = chunk.getBoundingHeights();

        const Geodetic3 c0 = { cornerDists[0].corner, heights.min };
        const Geodetic3 c1 = { cornerDists[1].corner, heights.min };
        const Geodetic3 c2 = { cornerDists[2].corner, heights.max };
        const Geodetic3 c3 = { cornerDists[3].corner, heights.max };
        */

        Chunk::BoundingHeights heights = chunk.getBoundingHeights();

        const Geodetic3 c0 = { chunk.surfacePatch().getCorner((Quad)0), heights.min };
        const Geodetic3 c1 = { chunk.surfacePatch().getCorner((Quad)1), heights.min };
        const Geodetic3 c2 = { chunk.surfacePatch().getCorner((Quad)2), heights.min };
        const Geodetic3 c3 = { chunk.surfacePatch().getCorner((Quad)3), heights.min };

        Vec3 A = cameraToEllipsoidCenter + ellipsoid.cartesianPosition(c0);
        Vec3 B = cameraToEllipsoidCenter + ellipsoid.cartesianPosition(c1);
        Vec3 C = cameraToEllipsoidCenter + ellipsoid.cartesianPosition(c2);
        Vec3 D = cameraToEllipsoidCenter + ellipsoid.cartesianPosition(c3);

        // Project points onto unit sphere
        A = glm::normalize(A);
        B = glm::normalize(B);
        C = glm::normalize(C);
        D = glm::normalize(D);


        /*
        A-----____
        |         '''-----B
        |          __--'  |
        |   __--''        |
        C-----------------D
        */

        const Vec3 AB = B - A;
        const Vec3 AC = C - A;
        const Vec3 DC = C - D;
        const Vec3 DB = B - D;

        double areaTriangle1 = 0.5 * glm::length(glm::cross(AC, AB));
        double areaTriangle2 = 0.5 * glm::length(glm::cross(DC, DB));
        double projectedChunkAreaApprox = areaTriangle1 + areaTriangle2;

        double scaledArea = globe->lodScaleFactor * projectedChunkAreaApprox;
        return chunk.index().level + round(scaledArea - 1);
    }

    int EvaluateChunkLevelByAvailableTileData::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
        auto tileProvidermanager = chunk.owner()->getTileProviderManager();
        auto heightMapProviders = tileProvidermanager->getActivatedLayerCategory(LayeredTextures::HeightMaps);
        int currLevel = chunk.index().level;

        // simply check the first heigtmap
        if (heightMapProviders.size() > 0) {
            Tile::Status heightTileStatus = heightMapProviders[0]->getTileStatus(chunk.index());
            if (heightTileStatus == Tile::Status::IOError || heightTileStatus == Tile::Status::OutOfRange) {
                return currLevel-1;
            }

            return UNKNOWN_DESIRED_LEVEL;
        }

        return UNKNOWN_DESIRED_LEVEL;
    }

} // namespace openspace
