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
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/layered_rendering/layeredtextures.h>


#include <algorithm>

namespace {
    const std::string _loggerCat = "ChunkLevelEvaluator";
}

namespace openspace {
namespace globebrowsing {
    
    int EvaluateChunkLevelByDistance::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
        // Calculations are done in the reference frame of the globe. Hence, the camera
        // position needs to be transformed with the inverse model matrix
        glm::dmat4 inverseModelTransform = chunk.owner().inverseModelTransform();
        const RenderableGlobe& globe = chunk.owner();
        const Ellipsoid& ellipsoid = globe.ellipsoid();

        Vec3 cameraPosition =
            glm::dvec3(inverseModelTransform * glm::dvec4(data.camera.positionVec3(), 1));
        
        Geodetic2 pointOnPatch = chunk.surfacePatch().closestPoint(
            ellipsoid.cartesianToGeodetic2(cameraPosition));
        Vec3 patchNormal = ellipsoid.geodeticSurfaceNormal(pointOnPatch);
        Vec3 patchPosition = ellipsoid.cartesianSurfacePosition(pointOnPatch);
        
        Chunk::BoundingHeights heights = chunk.getBoundingHeights();
        Scalar heightToChunk = heights.min;

        // Offset position according to height
        patchPosition += patchNormal * heightToChunk;
    
        Vec3 cameraToChunk = patchPosition - cameraPosition;

        // Calculate desired level based on distance
        Scalar distanceToPatch = glm::length(cameraToChunk);
        Scalar distance = distanceToPatch;

        Scalar scaleFactor = globe.generalProperties().lodScaleFactor * ellipsoid.minimumRadius();
        Scalar projectedScaleFactor = scaleFactor / distance;
        int desiredLevel = ceil(log2(projectedScaleFactor));
        return desiredLevel;
    }

    int EvaluateChunkLevelByProjectedArea::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
        // Calculations are done in the reference frame of the globe. Hence, the camera
        // position needs to be transformed with the inverse model matrix
        glm::dmat4 inverseModelTransform = chunk.owner().inverseModelTransform();

        const RenderableGlobe& globe = chunk.owner();
        const Ellipsoid& ellipsoid = globe.ellipsoid();
        glm::dvec4 cameraPositionModelSpace = glm::dvec4(data.camera.positionVec3(), 1);
        Vec3 cameraPosition = glm::dvec3(inverseModelTransform * cameraPositionModelSpace);
        Vec3 cameraToEllipsoidCenter = -cameraPosition;

        Geodetic2 cameraGeodeticPos = ellipsoid.cartesianToGeodetic2(cameraPosition);       
        
        // Approach:
        // The projected area of the chunk will be calculated based on a small area that
        // is close to the camera, and the scaled up to represent the full area.
        // The advantage of doing this is that it will better handle the cases where the 
        // full patch is very curved (e.g. stretches from latitude 0 to 90 deg).
        
        const Geodetic2 center = chunk.surfacePatch().center();
        const Geodetic2 closestCorner = chunk.surfacePatch().closestCorner(cameraGeodeticPos);


        //  Camera
        //  |
        //  V
        //
        //  oo 
        // [  ]<
        //                     *geodetic space*
        //     
        //   closestCorner 
        //    +-----------------+  <-- north east corner
        //    |                 |
        //    |      center     |
        //    |                 |
        //    +-----------------+  <-- south east corner


        
        Chunk::BoundingHeights heights = chunk.getBoundingHeights();
        const Geodetic3 c = { center, heights.min };
        const Geodetic3 c1 = { Geodetic2(center.lat, closestCorner.lon), heights.min };
        const Geodetic3 c2 = { Geodetic2(closestCorner.lat, center.lon), heights.min };
        
        //  Camera
        //  |
        //  V
        //
        //  oo 
        // [  ]<
        //                     *geodetic space*
        //
        //    +--------c2-------+  <-- north east corner
        //    |                 |
        //    c1       c        |
        //    |                 |
        //    +-----------------+  <-- south east corner


        // Go from geodetic to cartesian space
        Vec3 A = cameraToEllipsoidCenter + ellipsoid.cartesianPosition(c);
        Vec3 B = cameraToEllipsoidCenter + ellipsoid.cartesianPosition(c1);
        Vec3 C = cameraToEllipsoidCenter + ellipsoid.cartesianPosition(c2);

        // Project onto unit sphere
        A = glm::normalize(A);
        B = glm::normalize(B);
        C = glm::normalize(C);

        // Camera                      *cartesian space*
        // |                    +--------+---+ 
        // V             __--''   __--''    /
        //              C-------A--------- +
        // oo          /       /          /
        //[  ]<       +-------B----------+
        //

        // If the geodetic patch is small (i.e. has small width), that means the patch in
        // cartesian space will be almost flat, and in turn, the triangle ABC will roughly 
        // correspond to 1/8 of the full area
        const Vec3 AB = B - A;
        const Vec3 AC = C - A;
        double areaABC = 0.5 * glm::length(glm::cross(AC, AB));
        double projectedChunkAreaApprox = 8 * areaABC;

        double scaledArea = globe.generalProperties().lodScaleFactor * projectedChunkAreaApprox;
        return chunk.tileIndex().level + round(scaledArea - 1);
    }

    int EvaluateChunkLevelByAvailableTileData::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
        auto tileProvidermanager = chunk.owner().chunkedLodGlobe()->getTileProviderManager();
        auto heightMapProviders = tileProvidermanager->getTileProviderGroup(LayeredTextures::HeightMaps).getActiveTileProviders();
        int currLevel = chunk.tileIndex().level;

        for (size_t i = 0; i < LayeredTextures::NUM_TEXTURE_CATEGORIES; i++) {
            auto tileProviderGroup = tileProvidermanager->getTileProviderGroup(i);
            for (auto tileProvider : tileProviderGroup.getActiveTileProviders()) {
                Tile::Status tileStatus = tileProvider->getTileStatus(chunk.tileIndex());

                if (tileStatus == Tile::Status::OK) {
                    return UNKNOWN_DESIRED_LEVEL;
                }
            }
        }

        return currLevel - 1;;
    }
} // namespace globebrowsing
} // namespace openspace
