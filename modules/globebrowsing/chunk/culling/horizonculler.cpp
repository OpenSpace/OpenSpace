/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/globebrowsing/chunk/culling/horizonculler.h>

#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <openspace/util/updatestructures.h>

namespace openspace {
namespace globebrowsing {
namespace culling {

bool HorizonCuller::isCullable(const Chunk& chunk, const RenderData& data) {
    // Calculations are done in the reference frame of the globe. Hence, the camera
    // position needs to be transformed with the inverse model matrix
    glm::dmat4 inverseModelTransform = chunk.owner().inverseModelTransform();

    const Ellipsoid& ellipsoid = chunk.owner().ellipsoid();
    const GeodeticPatch& patch = chunk.surfacePatch();
    float maxHeight = chunk.getBoundingHeights().max;
    glm::dvec3 globePosition = glm::dvec3(0,0,0); // In model space it is 0
    double minimumGlobeRadius = ellipsoid.minimumRadius();

    glm::dvec3 cameraPosition =
        glm::dvec3(inverseModelTransform * glm::dvec4(data.camera.positionVec3(), 1));

    glm::dvec3 globeToCamera = cameraPosition;

    Geodetic2 cameraPositionOnGlobe =
        ellipsoid.cartesianToGeodetic2(globeToCamera);
    Geodetic2 closestPatchPoint = patch.closestPoint(cameraPositionOnGlobe);
    glm::dvec3 objectPosition = ellipsoid.cartesianSurfacePosition(closestPatchPoint);

    // objectPosition is closest in latlon space but not guaranteed to be closest in
    // castesian coordinates. Therefore we compare it to the corners and pick the
    // real closest point,
    glm::dvec3 corners[4];
    corners[0] = ellipsoid.cartesianSurfacePosition(
        chunk.surfacePatch().getCorner(NORTH_WEST));
    corners[1] = ellipsoid.cartesianSurfacePosition(
        chunk.surfacePatch().getCorner(NORTH_EAST));
    corners[2] = ellipsoid.cartesianSurfacePosition(
        chunk.surfacePatch().getCorner(SOUTH_WEST));
    corners[3] = ellipsoid.cartesianSurfacePosition(
        chunk.surfacePatch().getCorner(SOUTH_EAST));

    for (int i = 0; i < 4; ++i) {
        float distance = glm::length(cameraPosition - corners[i]);
        if (distance < glm::length(cameraPosition - objectPosition)) {
            objectPosition = corners[i];
        }
    }
        
    return isCullable(cameraPosition, globePosition, objectPosition,
                        maxHeight, minimumGlobeRadius);
}

bool HorizonCuller::isCullable(const glm::dvec3& cameraPosition, 
                               const glm::dvec3& globePosition,
                               const glm::dvec3& objectPosition,
                               double objectBoundingSphereRadius,
                               double minimumGlobeRadius)
{
    double distanceToHorizon =
        sqrt(pow(length(cameraPosition - globePosition), 2) -
        pow(minimumGlobeRadius, 2));
    
    double minimumAllowedDistanceToObjectFromHorizon = sqrt(
        pow(length(objectPosition - globePosition), 2) -
        pow(minimumGlobeRadius - objectBoundingSphereRadius, 2));
    
    // Minimum allowed for the object to be occluded
    double minimumAllowedDistanceToObjectSquared =
        pow(distanceToHorizon + minimumAllowedDistanceToObjectFromHorizon, 2)
        + pow(objectBoundingSphereRadius, 2);
    
    double distanceToObjectSquared = pow(length(objectPosition - cameraPosition), 2);
    return distanceToObjectSquared > minimumAllowedDistanceToObjectSquared;
}

} // namespace culling
} // namespace globebrowsing
} // namespace openspace
