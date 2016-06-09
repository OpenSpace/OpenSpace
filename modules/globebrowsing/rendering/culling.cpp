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


#include <modules/globebrowsing/rendering/culling.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/geodetics/ellipsoid.h>
#include <modules/globebrowsing/globes/chunk.h>
#include <modules/globebrowsing/meshes/trianglesoup.h>

namespace {
    const std::string _loggerCat = "FrustrumCuller";
}

namespace openspace {

    //////////////////////////////////////////////////////////////////////////////////////
    //							FRUSTUM CULLER											//
    //////////////////////////////////////////////////////////////////////////////////////
    FrustumCuller::FrustumCuller(const AABB3 viewFrustum)
    : _viewFrustum(viewFrustum){

    }

    FrustumCuller::~FrustumCuller() {

    }

    bool FrustumCuller::isCullable(const Chunk& chunk, const RenderData& data) {
        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();
        const GeodeticPatch& patch = chunk.surfacePatch();
        

        // Calculate the MVP matrix
        dmat4 modelTransform = translate(dmat4(1), data.position.dvec3());
        dmat4 viewTransform = dmat4(data.camera.combinedViewMatrix());
        mat4 modelViewProjectionTransform = dmat4(data.camera.projectionMatrix())
            * viewTransform * modelTransform;

        Chunk::BoundingHeights boundingHeight = chunk.getBoundingHeights();


        double patchCenterRadius = ellipsoid.maximumRadius(); // assume worst case
        
        // As the patch is curved, the maximum height offsets at the corners must be long 
        // enough to cover large enough to cover a boundingHeight.max at the center of the 
        // patch. 
        double maxCenterRadius = patchCenterRadius + boundingHeight.max;
        double maximumPatchSide = max(patch.halfSize().lat, patch.halfSize().lon);
        double maxCornerHeight = maxCenterRadius / cos(maximumPatchSide) - patchCenterRadius;

        // The minimum height offset, however, we can simply 
        double minCornerHeight = boundingHeight.min;
        

        // Create a bounding box that fits the patch corners
        AABB3 bounds; // in screen space
        int numPositiveZ = 0;
        for (size_t i = 0; i < 8; i++) {
            Quad q = (Quad)(i % 4);
            double cornerHeight = i < 4 ? minCornerHeight : maxCornerHeight;
            const Geodetic3& cornerGeodetic = { patch.getCorner(q), cornerHeight };
            dvec4 cornerModelSpace = dvec4(ellipsoid.cartesianPosition(cornerGeodetic), 1);
            vec4 cornerClippingSpace = modelViewProjectionTransform * cornerModelSpace;
            vec3 cornerScreenSpace = (1.0f / glm::abs(cornerClippingSpace.w)) * cornerClippingSpace.xyz();
            bounds.expand(cornerScreenSpace);
        }

        return !bounds.intersects(_viewFrustum);
    }


    //////////////////////////////////////////////////////////////////////////////////////
    //							HORIZON CULLER											//
    //////////////////////////////////////////////////////////////////////////////////////
    HorizonCuller::HorizonCuller() {

    }

    HorizonCuller::~HorizonCuller() {

    }

    bool HorizonCuller::isCullable(const Chunk& chunk, const RenderData& data) {
        //return !isVisible(data, chunk.surfacePatch(), chunk.owner()->ellipsoid(), chunk.owner()->chunkHeight);
        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();
        const GeodeticPatch& patch = chunk.surfacePatch();
        float maxHeight = chunk.getBoundingHeights().max;
        Vec3 globePosition = data.position.dvec3();
        Scalar minimumGlobeRadius = ellipsoid.minimumRadius();

        Vec3 cameraPosition = data.camera.positionVec3();

        Vec3 globeToCamera = cameraPosition - globePosition;

        Geodetic2 cameraPositionOnGlobe =
            ellipsoid.cartesianToGeodetic2(globeToCamera);
        Geodetic2 closestPatchPoint = patch.closestPoint(cameraPositionOnGlobe);
        const Vec3& objectPosition = ellipsoid.cartesianSurfacePosition(closestPatchPoint);

        return isCullable(cameraPosition, globePosition, objectPosition,
            maxHeight, minimumGlobeRadius);
    }

    bool HorizonCuller::isCullable(
        const Vec3& cameraPosition,
        const Vec3& globePosition,
        const Vec3& objectPosition,
        Scalar objectBoundingSphereRadius,
        Scalar minimumGlobeRadius)
    {
        Scalar distanceToHorizon =
            sqrt(pow(length(cameraPosition - globePosition), 2) - pow(minimumGlobeRadius, 2));
        Scalar minimumAllowedDistanceToObjectFromHorizon = sqrt(
            pow(length(objectPosition - globePosition), 2) -
            pow(minimumGlobeRadius - objectBoundingSphereRadius, 2));
        // Minimum allowed for the object to be occluded
        Scalar minimumAllowedDistanceToObjectSquared =
            pow(distanceToHorizon + minimumAllowedDistanceToObjectFromHorizon, 2)
            + pow(objectBoundingSphereRadius, 2);
        Scalar distanceToObjectSquared = pow(length(objectPosition - cameraPosition), 2);
        return distanceToObjectSquared > minimumAllowedDistanceToObjectSquared;
    }

}  // namespace openspace
