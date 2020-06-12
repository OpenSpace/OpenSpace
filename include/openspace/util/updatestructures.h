/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_CORE___UPDATESTRUCTURES___H__
#define __OPENSPACE_CORE___UPDATESTRUCTURES___H__

#include <openspace/util/camera.h>
#include <openspace/util/time.h>

namespace openspace {

class Deferredcaster;
class VolumeRaycaster;

struct InitializeData {};

struct TransformData {
    glm::dvec3 translation = glm::dvec3(0.0);
    glm::dmat3 rotation = glm::dmat3(1.0);
    glm::dvec3 scale = glm::dvec3(1.0);
};

struct UpdateData {
    TransformData modelTransform;
    const Time time;
    const Time previousFrameTime;
    const bool doPerformanceMeasurement;
};

struct RenderData {
    const Camera& camera;
    const Time time;
    bool doPerformanceMeasurement = false;
    int renderBinMask = -1;
    TransformData modelTransform;
};

struct RaycasterTask {
    VolumeRaycaster* raycaster;
    RenderData renderData;
};

struct DeferredcasterTask {
    Deferredcaster* deferredcaster;
    RenderData renderData;
};

struct RendererTasks {
    std::vector<RaycasterTask> raycasterTasks;
    std::vector<DeferredcasterTask> deferredcasterTasks;
};

struct RaycastData {
    int id = -1;
    std::string namespaceName;
};

struct DeferredcastData {
    int id = -1;
    std::string namespaceName;
};

/**
 * Defines the position of an object relative to a surface. The surface is defined as
 * a reference surface together with a height offset from that reference surface.
 */
struct SurfacePositionHandle {
    /// Vector from the center of the object to the reference surface of the object
    glm::dvec3 centerToReferenceSurface = glm::dvec3(0.0);
    /// Direction out from the reference. Can conincide with the surface normal but does
    /// not have to.
    glm::dvec3 referenceSurfaceOutDirection = glm::dvec3(0.0);
    /// Height from the reference surface out to the actual surface in the direction of
    /// the surface normal. Can be positive or negative.
    double heightToSurface = 0.0;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___UPDATESTRUCTURES___H__
