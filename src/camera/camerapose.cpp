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

#include <openspace/camera/camerapose.h>

#include <openspace/camera/camera.h>
#include <openspace/util/updatestructures.h>

namespace openspace {

CameraRotationDecomposition decomposeCameraRotation(const CameraPose& cameraPose,
                                                    const glm::dvec3& reference)
{
    const glm::dvec3 cameraUp = cameraPose.rotation * glm::dvec3(0.0, 1.0, 0.0);
    const glm::dvec3 cameraViewDirection = ghoul::viewDirection(cameraPose.rotation);

    // To avoid problem with lookup in up direction we adjust it with the view direction
    const glm::dquat globalCameraRotation = ghoul::lookAtQuaternion(
        glm::dvec3(0.0),
        reference - cameraPose.position,
        normalize(cameraViewDirection + cameraUp)
    );

    const glm::dquat localCameraRotation = glm::inverse(globalCameraRotation) *
        cameraPose.rotation;

    return { localCameraRotation, globalCameraRotation };
}

CameraRotationDecomposition
decomposeCameraRotationSurface(const CameraPose& cameraPose,
                               const SceneGraphNode& reference)
{
    const glm::dvec3 cameraUp = cameraPose.rotation * Camera::UpDirectionCameraSpace;
    const glm::dvec3 cameraViewDirection = ghoul::viewDirection(cameraPose.rotation);

    glm::dmat4 modelTransform = reference.modelTransform();
    if (modelTransform[0][0] == 0.0) {
        modelTransform[0][0] = std::numeric_limits<double>::epsilon();
    }
    if (modelTransform[1][1] == 0.0) {
        modelTransform[1][1] = std::numeric_limits<double>::epsilon();
    }
    if (modelTransform[2][2] == 0.0) {
        modelTransform[2][2] = std::numeric_limits<double>::epsilon();
    }
    const glm::dmat4 inverseModelTransform = glm::inverse(modelTransform);
    const glm::dvec3 cameraPositionModelSpace = glm::dvec3(inverseModelTransform *
        glm::dvec4(cameraPose.position, 1));

    const SurfacePositionHandle posHandle =
        reference.calculateSurfacePositionHandle(cameraPositionModelSpace);

    const glm::dvec3 directionFromSurfaceToCameraModelSpace =
        posHandle.referenceSurfaceOutDirection;
    const glm::dvec3 directionFromSurfaceToCamera = glm::normalize(
        glm::dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace
    );

    // To avoid problem with lookup in up direction we adjust it with the view direction
    const glm::dquat globalCameraRotation = ghoul::lookAtQuaternion(
        glm::dvec3(0.0),
        -directionFromSurfaceToCamera,
        normalize(cameraViewDirection + cameraUp)
    );

    const glm::dquat localCameraRotation = glm::inverse(globalCameraRotation) *
        cameraPose.rotation;

    return { localCameraRotation, globalCameraRotation };
}

glm::dquat composeCameraRotation(const CameraRotationDecomposition& decomposition) {
    return decomposition.globalRotation * decomposition.localRotation;
}

} // namespace openspace
