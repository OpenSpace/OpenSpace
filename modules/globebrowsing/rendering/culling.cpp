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


    bool FrustumCuller::isVisible(
        const RenderData& data,
        const vec3& point) {

        mat4 modelTransform = translate(mat4(1), data.position.vec3());
        mat4 viewTransform = data.camera.combinedViewMatrix();
        mat4 modelViewProjectionTransform = data.camera.projectionMatrix()
            * viewTransform * modelTransform;

        vec2 pointScreenSpace =
            transformToScreenSpace(point, modelViewProjectionTransform);
        return testPoint(pointScreenSpace, vec2(0));
    }

    bool FrustumCuller::isVisible(
        const RenderData& data,
        const GeodeticPatch& patch,
        const Ellipsoid& ellipsoid) {
        // An axis aligned bounding box based on the patch's minimum boudning sphere is
        // used for testnig

        //mat4 viewTransform = glm::lookAt(vec3(6378137.0 + 1000, 0, 0), vec3(0, 5e6, 1e7), vec3(0, 0, 1)); //data.camera.combinedViewMatrix
        //Vec3 cameraPosition = vec3(inverse(viewTransform) * vec4(0, 0, 0, 1));// data.camera.position().dvec3();


        // Calculate the MVP matrix
        mat4 modelTransform = translate(mat4(1), data.position.vec3());
        mat4 viewTransform = data.camera.combinedViewMatrix();
        mat4 modelViewProjectionTransform = data.camera.projectionMatrix()
            * viewTransform * modelTransform;

        // Calculate the patch's center point in screen space
        vec4 patchCenterModelSpace =
            vec4(ellipsoid.geodetic2ToCartesian(patch.center()), 1);
        vec4 patchCenterClippingSpace =
            modelViewProjectionTransform * patchCenterModelSpace;
        vec2 pointScreenSpace =
            (1.0f / patchCenterClippingSpace.w) * patchCenterClippingSpace.xy();

        // Calculate the screen space margin that represents an axis aligned bounding 
        // box based on the patch's minimum boudning sphere
        double boundingRadius = patch.minimalBoundingRadius(ellipsoid);
        vec4 marginClippingSpace =
            vec4(vec3(boundingRadius), 0) * data.camera.projectionMatrix();
        vec2 marginScreenSpace =
            (1.0f / patchCenterClippingSpace.w) * marginClippingSpace.xy();

        // Test the bounding box by testing the center point and the corresponding margin
        return testPoint(pointScreenSpace, marginScreenSpace);
    }

    bool FrustumCuller::testPoint(const glm::vec2& pointScreenSpace,
        const glm::vec2& marginScreenSpace)
    {
        const vec2& p = pointScreenSpace;

        vec2 cullBounds = vec2(1) + marginScreenSpace;
        return ((-cullBounds.x < p.x && p.x < cullBounds.x) &&
            (-cullBounds.y < p.y && p.y < cullBounds.y));
    }

    glm::vec2 FrustumCuller::transformToScreenSpace(const vec3& point,
        const mat4x4& modelViewProjection)
    {
        vec4 pointProjectionSpace = modelViewProjection * vec4(point, 1.0f);
        vec2 pointScreenSpace =
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

        Vec3 cameraPosition = data.camera.position().dvec3();

        Vec3 globeToCamera = cameraPosition - globePosition;

        Geodetic2 cameraPositionOnGlobe =
            ellipsoid.cartesianToGeodetic2(globeToCamera);
        Geodetic2 closestPatchPoint = patch.closestPoint(cameraPositionOnGlobe);

        return HorizonCuller::isVisible(
            cameraPosition,
            globePosition,
            ellipsoid.geodetic2ToCartesian(closestPatchPoint),
            height,
            minimumGlobeRadius);
    }

}  // namespace openspace
