/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___CAMERAPOSE___H__
#define __OPENSPACE_CORE___CAMERAPOSE___H__

#include <openspace/scene/scenegraphnode.h>
#include <ghoul/glm.h>

namespace openspace {


struct CameraPose {
    glm::dvec3 position = glm::dvec3(0.0);
    glm::dquat rotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
};

struct CameraRotationDecomposition {
    glm::dquat localRotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
    glm::dquat globalRotation = glm::dquat(1.0, 0.0, 0.0, 0.0);
};

/**
 * Decomposes the camera's rotation in to a global and a local rotation defined by
 * CameraRotationDecomposition. The global rotation defines the rotation so that the
 * camera points towards the reference position.
 *
 * The local rotation defines the differential from the global to the current total
 * rotation so that `cameraRotation = globalRotation * localRotation`.
 */
CameraRotationDecomposition decomposeCameraRotation(const CameraPose& cameraPose,
    const glm::dvec3& reference);

/**
 * Decomposes the camera's rotation in to a global and a local rotation defined by
 * CameraRotationDecomposition. The global rotation defines the rotation so that the
 * camera points towards the reference node in the direction opposite to the direction
 * out from the surface of the object. The local rotation defines the differential
 * from the global to the current total rotation so that
 * `cameraRotation = globalRotation * localRotation`.
 */
CameraRotationDecomposition decomposeCameraRotationSurface(
    const CameraPose& cameraPose, const SceneGraphNode& reference);

/**
 * Composes a pair of global and local rotations into a quaternion that can be used as
 * the world rotation for a camera.
 */
glm::dquat composeCameraRotation(const CameraRotationDecomposition& decomposition);

} // namespace openspace

#endif // __OPENSPACE_CORE___CAMERAPOSE___H__
