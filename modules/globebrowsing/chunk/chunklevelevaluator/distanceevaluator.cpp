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

#include <modules/globebrowsing/chunk/chunklevelevaluator/distanceevaluator.h>

#include <modules/globebrowsing/chunk/chunk.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/rendering/layer/layermanager.h>
#include <modules/globebrowsing/tile/tileprovider/tileprovider.h>
#include <openspace/util/updatestructures.h>

namespace openspace::globebrowsing::chunklevelevaluator {
    
int Distance::getDesiredLevel(const Chunk& chunk, const RenderData& data) const {
    // Calculations are done in the reference frame of the globe
    // (model space). Hence, the camera position needs to be transformed
    // with the inverse model matrix
    glm::dmat4 inverseModelTransform = chunk.owner().inverseModelTransform();
    const RenderableGlobe& globe = chunk.owner();
    const Ellipsoid& ellipsoid = globe.ellipsoid();

    glm::dvec3 cameraPosition =
        glm::dvec3(inverseModelTransform * glm::dvec4(data.camera.positionVec3(), 1));
        
    Geodetic2 pointOnPatch = chunk.surfacePatch().closestPoint(
        ellipsoid.cartesianToGeodetic2(cameraPosition)
    );
    glm::dvec3 patchNormal = ellipsoid.geodeticSurfaceNormal(pointOnPatch);
    glm::dvec3 patchPosition = ellipsoid.cartesianSurfacePosition(pointOnPatch);
        
    Chunk::BoundingHeights heights = chunk.getBoundingHeights();
    double heightToChunk = heights.min;

    // Offset position according to height
    patchPosition += patchNormal * heightToChunk;
    
    glm::dvec3 cameraToChunk = patchPosition - cameraPosition;

    // Calculate desired level based on distance
    double distanceToPatch = glm::length(cameraToChunk);
    double distance = distanceToPatch;

    double scaleFactor =
        globe.generalProperties().lodScaleFactor * ellipsoid.minimumRadius();
    double projectedScaleFactor = scaleFactor / distance;
    int desiredLevel = static_cast<int>(ceil(log2(projectedScaleFactor)));
    return desiredLevel;
}

} // namespace openspace::globebrowsing::chunklevelevaluator
