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

#include <modules/globebrowsing/chunk/culling.h>
#include <modules/globebrowsing/chunk/chunkedlodglobe.h>
#include <modules/globebrowsing/chunk/chunk.h>

#include <modules/globebrowsing/geometry/ellipsoid.h>
#include <modules/globebrowsing/geometry/convexhull.h>

#include <modules/globebrowsing/meshes/trianglesoup.h>

#include <modules/debugging/rendering/debugrenderer.h>

namespace {
    const std::string _loggerCat = "FrustrumCuller";
}

namespace openspace {

    //////////////////////////////////////////////////////////////////////////////////////
    //                            FRUSTUM CULLER                                            //
    //////////////////////////////////////////////////////////////////////////////////////
    FrustumCuller::FrustumCuller(const AABB3 viewFrustum)
    : _viewFrustum(viewFrustum){

    }

    FrustumCuller::~FrustumCuller() {

    }

    bool FrustumCuller::isCullable(const Chunk& chunk, const RenderData& data) {
        // Calculate the MVP matrix
        dmat4 modelTransform = chunk.owner()->modelTransform();
        dmat4 viewTransform = dmat4(data.camera.combinedViewMatrix());
        dmat4 modelViewProjectionTransform = dmat4(data.camera.projectionMatrix())
            * viewTransform * modelTransform;

        const std::vector<glm::dvec4>& corners = chunk.getBoundingPolyhedronCorners();
        

        // Create a bounding box that fits the patch corners
        AABB3 bounds; // in screen space
        std::vector<vec4> clippingSpaceCorners(8);
        for (size_t i = 0; i < 8; i++) {
            dvec4 cornerClippingSpace = modelViewProjectionTransform * corners[i];
            clippingSpaceCorners[i] = cornerClippingSpace;

            dvec3 cornerScreenSpace = (1.0f / glm::abs(cornerClippingSpace.w)) * cornerClippingSpace;
            bounds.expand(cornerScreenSpace);
        }
        
        return !_viewFrustum.intersects(bounds);
    }


    //////////////////////////////////////////////////////////////////////////////////////
    //                            HORIZON CULLER                                            //
    //////////////////////////////////////////////////////////////////////////////////////
    HorizonCuller::HorizonCuller() {

    }

    HorizonCuller::~HorizonCuller() {

    }

    bool HorizonCuller::isCullable(const Chunk& chunk, const RenderData& data) {
        //return !isVisible(data, chunk.surfacePatch(), chunk.owner()->ellipsoid(), chunk.owner()->chunkHeight);
        
        // Calculations are done in the reference frame of the globe. Hence, the camera
        // position needs to be transformed with the inverse model matrix
        glm::dmat4 inverseModelTransform = chunk.owner()->inverseModelTransform();

        const Ellipsoid& ellipsoid = chunk.owner()->ellipsoid();
        const GeodeticPatch& patch = chunk.surfacePatch();
        float maxHeight = chunk.getBoundingHeights().max;
        Vec3 globePosition = glm::dvec3(0,0,0); // In model space it is 0
        Scalar minimumGlobeRadius = ellipsoid.minimumRadius();

        Vec3 cameraPosition = 
            glm::dvec3(inverseModelTransform * glm::dvec4(data.camera.positionVec3(), 1));

        Vec3 globeToCamera = cameraPosition;

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
