/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
#include <array>

namespace openspace::globebrowsing::culling {

bool HorizonCuller::isCullable(const Chunk& chunk, const RenderData& renderData) {
    // Calculations are done in the reference frame of the globe. Hence, the camera
    // position needs to be transformed with the inverse model matrix
    const glm::dmat4 inverseModelTransform = chunk.owner().inverseModelTransform();

    const Ellipsoid& ellipsoid = chunk.owner().ellipsoid();
    const GeodeticPatch& patch = chunk.surfacePatch();
    const float maxHeight = chunk.boundingHeights().max;
    const glm::dvec3 globePos = glm::dvec3(0,0,0); // In model space it is 0
    const double minimumGlobeRadius = ellipsoid.minimumRadius();

    const glm::dvec3 cameraPos = glm::dvec3(
        inverseModelTransform * glm::dvec4(renderData.camera.positionVec3(), 1)
    );

    const glm::dvec3 globeToCamera = cameraPos;

    const Geodetic2 cameraPositionOnGlobe = ellipsoid.cartesianToGeodetic2(globeToCamera);
    const Geodetic2 closestPatchPoint = patch.closestPoint(cameraPositionOnGlobe);
    glm::dvec3 objectPos = ellipsoid.cartesianSurfacePosition(closestPatchPoint);

    // objectPosition is closest in latlon space but not guaranteed to be closest in
    // castesian coordinates. Therefore we compare it to the corners and pick the
    // real closest point,
    std::array<glm::dvec3, 4> corners = {
        ellipsoid.cartesianSurfacePosition(chunk.surfacePatch().corner(NORTH_WEST)),
        ellipsoid.cartesianSurfacePosition(chunk.surfacePatch().corner(NORTH_EAST)),
        ellipsoid.cartesianSurfacePosition(chunk.surfacePatch().corner(SOUTH_WEST)),
        ellipsoid.cartesianSurfacePosition(chunk.surfacePatch().corner(SOUTH_EAST))
    };

    for (int i = 0; i < 4; ++i) {
        const double distance = glm::length(cameraPos - corners[i]);
        if (distance < glm::length(cameraPos - objectPos)) {
            objectPos = corners[i];
        }
    }

    return isCullable(cameraPos, globePos, objectPos, maxHeight, minimumGlobeRadius);
}

bool HorizonCuller::isCullable(const glm::dvec3& cameraPosition,
                               const glm::dvec3& globePosition,
                               const glm::dvec3& objectPosition,
                               double objectBoundingSphereRadius,
                               double minimumGlobeRadius)
{
    const double objectP = pow(length(objectPosition - globePosition), 2);
    const double horizonP = pow(minimumGlobeRadius - objectBoundingSphereRadius, 2);
    if (objectP < horizonP) {
        return false;
    }

    const double cameraP = pow(length(cameraPosition - globePosition), 2);
    const double minR = pow(minimumGlobeRadius, 2);
    if (cameraP < minR) {
        return false;
    }

    const double minimumAllowedDistanceToObjectFromHorizon = sqrt(objectP - horizonP);
    const double distanceToHorizon = sqrt(cameraP - minR);

    // Minimum allowed for the object to be occluded
    const double minimumAllowedDistanceToObjectSquared =
        pow(distanceToHorizon + minimumAllowedDistanceToObjectFromHorizon, 2) +
        pow(objectBoundingSphereRadius, 2);

    const double distanceToObjectSquared = pow(
        length(objectPosition - cameraPosition),
        2
    );
    return distanceToObjectSquared > minimumAllowedDistanceToObjectSquared;
}

} // namespace openspace::globebrowsing::culling
