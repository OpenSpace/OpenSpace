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

#include <modules/globebrowsing/geodetics/ellipsoid.h>

#include <modules/globebrowsing/meshes/trianglesoup.h>

namespace {
    const std::string _loggerCat = "FrustrumCuller";
}

namespace openspace {

    //////////////////////////////////////////////////////////////////////////////////////
    //							FRUSTUM CULLER											//
    //////////////////////////////////////////////////////////////////////////////////////
    FrustumCuller::FrustumCuller() {

    }

    FrustumCuller::~FrustumCuller() {

    }


    const AABB3 FrustumCuller::viewFrustum(dvec3(-1, -1, 0), dvec3(1, 1, 1e35));


    bool FrustumCuller::isVisible(
        const RenderData& data,
        const dvec3& point) {

        dmat4 modelTransform = translate(dmat4(1), data.position.dvec3());
        dmat4 viewTransform = dmat4(data.camera.combinedViewMatrix());
        dmat4 modelViewProjectionTransform = dmat4(data.camera.projectionMatrix())
            * viewTransform * modelTransform;

        dvec2 pointScreenSpace =
            transformToScreenSpace(point, modelViewProjectionTransform);
        return testPoint(pointScreenSpace, dvec2(0));
    }


    bool FrustumCuller::isVisible(const RenderData& data, const GeodeticPatch& patch,
        const Ellipsoid& ellipsoid) 
    {
        // An axis aligned bounding box based on the patch's minimum boudning sphere is
        // used for testnig

        //mat4 viewTransform = glm::lookAt(vec3(6378137.0 + 1000, 0, 0), vec3(0, 5e6, 1e7), vec3(0, 0, 1)); //data.camera.combinedViewMatrix
        //Vec3 cameraPosition = vec3(inverse(viewTransform) * vec4(0, 0, 0, 1));// data.camera.position().dvec3();


        // Calculate the MVP matrix
        dmat4 modelTransform = translate(mat4(1), data.position.vec3());
        dmat4 viewTransform = dmat4(data.camera.combinedViewMatrix());
        dmat4 modelViewProjectionTransform = dmat4(data.camera.projectionMatrix())
            * viewTransform * modelTransform;

        // Calculate the patch's center point in screen space
        dvec4 patchCenterModelSpace =
            dvec4(ellipsoid.cartesianSurfacePosition(patch.center()), 1);
        dvec4 patchCenterClippingSpace =
            modelViewProjectionTransform * patchCenterModelSpace;
        dvec2 pointScreenSpace =
            (1.0 / patchCenterClippingSpace.w) * patchCenterClippingSpace.xy();

        // Calculate the screen space margin that represents an axis aligned bounding 
        // box based on the patch's minimum boudning sphere
        double boundingRadius = patch.minimalBoundingRadius(ellipsoid);
        dvec4 marginClippingSpace =
            dvec4(dvec3(boundingRadius), 0) * dmat4(data.camera.projectionMatrix());
        dvec2 marginScreenSpace =
            (1.0 / patchCenterClippingSpace.w) * marginClippingSpace.xy();

        // Test the bounding box by testing the center point and the corresponding margin
        PointLocation res = testPoint(pointScreenSpace, marginScreenSpace);
        return res == PointLocation::Inside;
    }

    bool FrustumCuller::isVisible(const RenderData& data, const GeodeticPatch& patch,
        const Ellipsoid& ellipsoid, const Scalar maxHeight)
    {
        // Calculate the MVP matrix
        dmat4 modelTransform = translate(dmat4(1), data.position.dvec3());
        dmat4 viewTransform = dmat4(data.camera.combinedViewMatrix());
        dmat4 modelViewProjectionTransform = dmat4(data.camera.projectionMatrix())
            * viewTransform * modelTransform;



        double centerRadius = ellipsoid.maximumRadius();
        //double centerRadius = glm::length(ellipsoid.cartesianSurfacePosition(patch.center()));
        double maxCenterRadius = centerRadius + maxHeight;
        
        double maximumPatchSide = max(patch.halfSize().lat, patch.halfSize().lon);
        double maxHeightOffset = maxCenterRadius / cos(maximumPatchSide) - centerRadius;
        double minHeightOffset = 0; // for now


        /*
        Geodetic3 centerGeodetic = { patch.center(), 0};
        vec4 centerModelSpace = vec4(ellipsoid.cartesianPosition(centerGeodetic), 1);
        vec4 centerClippingSpace = modelViewProjectionTransform * centerModelSpace;
        vec3 centerScreenSpace = (1.0f / glm::abs(centerClippingSpace.w)) * centerClippingSpace.xyz();
        AABB3 viewFrustum(vec3(-1, -1, 0), vec3(1, 1, 1e35));
        return viewFrustum.intersects(centerScreenSpace);
        */

        // Create a bounding box that fits the patch corners
        
        AABB3 bounds; // in screen space
        int numPositiveZ = 0;
        for (size_t i = 0; i < 8; i++) {
            Quad q = (Quad) (i%4);
            double offset = i < 4 ? minHeightOffset : maxHeightOffset;
            Geodetic3 cornerGeodetic = { patch.getCorner(q), offset };
            dvec4 cornerModelSpace = dvec4(ellipsoid.cartesianPosition(cornerGeodetic), 1);
            dvec4 cornerClippingSpace = modelViewProjectionTransform * cornerModelSpace;
            dvec3 cornerScreenSpace = (1.0 / glm::abs(cornerClippingSpace.w)) * cornerClippingSpace.xyz();
            bounds.expand(cornerScreenSpace);
        }

        
        return bounds.intersects(FrustumCuller::viewFrustum);
        
        /*
        vec2 center = bounds.center();
        vec2 margin = 0.5f * bounds.size();
        return testPoint(center, margin) == PointLocation::Inside;
        */
    }


    PointLocation FrustumCuller::testPoint(const glm::dvec2& pointScreenSpace,
        const glm::dvec2& marginScreenSpace)
    {
        const vec2& p = pointScreenSpace;

        dvec2 cullBounds = dvec2(1) + marginScreenSpace;
        int x = p.x <= -cullBounds.x ? 0 : p.x < cullBounds.x ? 1 : 2;
        int y = p.y <= -cullBounds.y ? 0 : p.y < cullBounds.y ? 1 : 2;
        PointLocation res = (PointLocation) (3 * y + x);

        return res;
    }

    bool FrustumCuller::testPoint(const glm::dvec3& pointScreenSpace,
        const glm::dvec3& marginScreenSpace) 
    {

        const dvec3& p = pointScreenSpace;

        dvec3 cullBounds = dvec3(1) + marginScreenSpace;
        int x = p.x <= -cullBounds.x ? 0 : p.x < cullBounds.x ? 1 : 2;
        int y = p.y <= -cullBounds.y ? 0 : p.y < cullBounds.y ? 1 : 2;
        int z = p.z <= -cullBounds.z ? 0 : p.z < cullBounds.z ? 1 : 2;
        return x == 1 && y == 1 && z == 1;
    }

    glm::dvec2 FrustumCuller::transformToScreenSpace(const dvec3& point,
        const dmat4x4& modelViewProjection)
    {
        dvec4 pointProjectionSpace = modelViewProjection * dvec4(point, 1.0);
        dvec2 pointScreenSpace =
            (1.0f / pointProjectionSpace.w) * pointProjectionSpace.xy();
        return pointScreenSpace;
    }


    //////////////////////////////////////////////////////////////////////////////////////
    //							HORIZON CULLER											//
    //////////////////////////////////////////////////////////////////////////////////////
    HorizonCuller::HorizonCuller() {

    }

    HorizonCuller::~HorizonCuller() {

    }

    bool HorizonCuller::isVisible(
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
        return distanceToObjectSquared < minimumAllowedDistanceToObjectSquared;
    }

    bool HorizonCuller::isVisible(
        const RenderData& data,
        const GeodeticPatch& patch,
        const Ellipsoid& ellipsoid,
        float height)
    {
        Vec3 globePosition = data.position.dvec3();
        Scalar minimumGlobeRadius = ellipsoid.minimumRadius();

        Vec3 cameraPosition = data.camera.positionVec3();

        Vec3 globeToCamera = cameraPosition - globePosition;

        Geodetic2 cameraPositionOnGlobe =
            ellipsoid.cartesianToGeodetic2(globeToCamera);
        Geodetic2 closestPatchPoint = patch.closestPoint(cameraPositionOnGlobe);

        return HorizonCuller::isVisible(
            cameraPosition,
            globePosition,
            ellipsoid.cartesianSurfacePosition(closestPatchPoint),
            height,
            minimumGlobeRadius);
    }

}  // namespace openspace
