/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

// open space includes
#include <openspace/util/camera.h>

// sgct includes
#include "sgct.h"

#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace openspace {

    
Camera::Camera()
    : _scaling(1.f, 0.f)
    , _viewRotation(glm::quat(glm::vec3(0.f, 0.f, 0.f)))
    , _cameraDirection(0.f, 0.f, 0.f)
{
}

Camera::~Camera()
{
}

void Camera::setPosition(psc pos)
{
    _position = std::move(pos);
}

const psc& Camera::position() const
{
    return _position;
}

void Camera::setViewProjectionMatrix(glm::mat4 viewProjectionMatrix)
{
    _viewProjectionMatrix = std::move(viewProjectionMatrix);
}

const glm::mat4& Camera::viewProjectionMatrix() const
{
    return _viewProjectionMatrix;
}

void Camera::setCameraDirection(glm::vec3 cameraDirection)
{
    _cameraDirection = std::move(cameraDirection);
}

glm::vec3 Camera::cameraDirection() const
{
    return _cameraDirection;
}

const glm::mat4& Camera::viewRotationMatrix() const
{
    return _viewRotationMatrix;
}

void Camera::compileViewRotationMatrix()
{
    // convert from quaternion to rotation matrix using glm
    _viewRotationMatrix = glm::mat4_cast(_viewRotation);

    // the camera matrix needs to be rotated inverse to the world
    _viewDirection = glm::rotate(glm::inverse(_viewRotation), _cameraDirection);
    _viewDirection = glm::normalize(_viewDirection);
}

void Camera::rotate(const glm::quat& rotation)
{
    _viewRotation = rotation * _viewRotation;
    _viewRotation = glm::normalize(_viewRotation);
}

void Camera::setRotation(glm::quat rotation)
{
    _viewRotation = glm::normalize(std::move(rotation));
}

const glm::quat& Camera::rotation() const
{
    return _viewRotation;
}

const glm::vec3& Camera::viewDirection() const
{
    return _viewDirection;
}

const float& Camera::maxFov() const
{
    return _maxFov;
}

const float& Camera::sinMaxFov() const
{
    return _sinMaxFov;
}

void Camera::setMaxFov(float fov)
{
    _maxFov = fov;
    _sinMaxFov = sin(_maxFov);
}

void Camera::setScaling(glm::vec2 scaling)
{
    _scaling = std::move(scaling);
}

const glm::vec2& Camera::scaling() const
{
    return _scaling;
}

void Camera::setLookUpVector(glm::vec3 lookUp)
{
    _lookUp = std::move(lookUp);
}

const glm::vec3 Camera::lookUpVector() const
{
    return _lookUp;
}


//
//Camera::Camera()
//    : _position(0.f, 0.f, 1.f, 0.f)
//    , _focus(0.f, 0.f, 0.f, 0.f)
//    , _upVector(0.f, 1.f, 0.f, 0.f)
//    , _projectionMatrix(glm::mat4(1.f))
//    , _viewMatrix(glm::mat4(1.f))
//    , _scaling(0.f)
//    , _maxFov(0.f)
//    , _viewMatrixIsDirty(false)
//{
//
//}
//
//void Camera::setPosition(psc pos)
//{
//    _position = std::move(pos);
//}
//
//const psc& Camera::position() const
//{
//    return _position;
//}
//
//void Camera::setFocus(psc focus)
//{
//    _focus = std::move(focus);
//}
//
//const psc& Camera::focus() const
//{
//    return _focus;
//}
//
//void Camera::setUpVector(psc upVector)
//{
//    _upVector = std::move(upVector);
//}
//
//const psc& Camera::upVector() const
//{
//    return _upVector;
//}
//
//void Camera::setScaling(float scaling)
//{
//    _scaling = scaling;
//}
//
//float Camera::scaling() const
//{
//    return _scaling;
//}
//
//const glm::mat4& Camera::viewMatrix() const
//{
//    
//    return _viewMatrix;
//}
//
//void Camera::setProjectionMatrix(glm::mat4 projectionMatrix)
//{
//    _projectionMatrix = std::move(projectionMatrix);
//}
//
//const glm::mat4& Camera::projectionMatrix() const
//{
//    return _projectionMatrix;
//}
//
//void Camera::setMaxFox(float fov)
//{
//    _maxFov = fov;
//}
//
//float Camera::maxFov() const
//{
//    return _maxFov;
//}
//
//psc Camera::lookVector() const
//{
//    return _focus - _position;
//}
//
//void Camera::invalidateViewMatrix() {
//    _viewMatrixIsDirty = true;
//}
//
//void Camera::updateViewMatrix() const {
//    if (_viewMatrixIsDirty) {
//        _viewMatrix = glm::lookAt(_position.getVec3f(), _focus.getVec3f(), _upVector.getVec3f());
//        _viewMatrixIsDirty = false;
//    }
//}

} // namespace openspace
