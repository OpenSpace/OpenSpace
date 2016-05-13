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


#include <modules/globebrowsing/globes/clipmapglobe.h>

#include <modules/globebrowsing/meshes/clipmapgrid.h>

// open space includes
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/scene/scenegraphnode.h>

// ghoul includes
#include <ghoul/misc/assert.h>

#define _USE_MATH_DEFINES
#include <math.h>


namespace {
    const std::string _loggerCat = "ClipMapGlobe";
}

namespace openspace {
    ClipMapGlobe::ClipMapGlobe(
        const Ellipsoid& ellipsoid,
        std::shared_ptr<TileProviderManager> tileProviderManager)
        : _clipMapPyramid(Geodetic2(M_PI / 2, M_PI / 2))
        , _ellipsoid(ellipsoid)
    {
        // init Renderer
        auto outerPatchRenderer = new ClipMapPatchRenderer(
            shared_ptr<OuterClipMapGrid>(new OuterClipMapGrid(256)),
            tileProviderManager);
        _outerPatchRenderer.reset(outerPatchRenderer);
        auto innerPatchRenderer = new ClipMapPatchRenderer(
            shared_ptr<InnerClipMapGrid>(new InnerClipMapGrid(256)),
            tileProviderManager);
        _innerPatchRenderer.reset(innerPatchRenderer);
    }

    ClipMapGlobe::~ClipMapGlobe() {
    }
    
    const Ellipsoid& ClipMapGlobe::ellipsoid() const
    {
        return _ellipsoid;
    }

    bool ClipMapGlobe::initialize() {
        return isReady();
    }

    bool ClipMapGlobe::deinitialize() {
        return true;
    }

    bool ClipMapGlobe::isReady() const {
        bool ready = true; 
        return ready;
    }

    void ClipMapGlobe::render(const RenderData& data)
    {
        int minDepth, maxDepth;
        calculateDesiredMinAndMaxDepth(data, minDepth, maxDepth);

        // render patches
        for (size_t i = minDepth; i < maxDepth; i++)
        {
            Geodetic2 patchSize = _clipMapPyramid.getPatchSizeAtLevel(i);
            _outerPatchRenderer->renderPatch(patchSize, data, _ellipsoid);
        }
        Geodetic2 patchSize = _clipMapPyramid.getPatchSizeAtLevel(maxDepth);
        _innerPatchRenderer->renderPatch(patchSize, data, _ellipsoid);
    }

    void ClipMapGlobe::update(const UpdateData& data) {
        _innerPatchRenderer->update();
        _outerPatchRenderer->update();
    }

    void ClipMapGlobe::calculateDesiredMinAndMaxDepth(
        const RenderData& data,
        int& minDepth,
        int& maxDepth)
    {
        Scalar minimumRadius = _ellipsoid.minimumRadius();
        Vec3 cameraPosition = data.camera.position().dvec3();
        Vec3 cameraPositionOnSurface = _ellipsoid.geodeticSurfaceProjection(cameraPosition);
        Scalar h = glm::length(cameraPosition - cameraPositionOnSurface);
        Scalar cosAngleToHorizon = minimumRadius / (minimumRadius + h);
        Scalar angleToHorizon = glm::acos(cosAngleToHorizon);
        Scalar minimumPatchSize = glm::min(
            _clipMapPyramid.getPatchSizeAtLevel0().lat,
            _clipMapPyramid.getPatchSizeAtLevel0().lon);
        minDepth = log2(minimumPatchSize / 2 / angleToHorizon);

        // Calculate desired level based on distance
        Scalar scaleFactor = 1 * minimumRadius;
        Scalar projectedScaleFactor = scaleFactor / h;
        maxDepth = glm::max(static_cast<int>(log2(projectedScaleFactor)), 0);

        // Test smaller and smaller patches until one is outside of frustum
        int i;
        for (i = minDepth; i < maxDepth; i++)
        {
            Geodetic2 center = _ellipsoid.cartesianToGeodetic2(cameraPosition);
            Geodetic2 halfSize = _clipMapPyramid.getPatchSizeAtLevel(i) / 2;
            GeodeticPatch testPatch(center, halfSize);
            // Do frustrum culling
            if (FrustumCuller::isVisible(data, testPatch, _ellipsoid)) {
                break;
            }
        }
        maxDepth = i;
    }

}  // namespace openspace
