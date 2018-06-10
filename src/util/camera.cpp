/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/util/camera.h>

#include <sstream>

namespace openspace {

const glm::dvec3 Camera::ViewDirectionCameraSpace = glm::dvec3(0, 0, -1);
const glm::dvec3 Camera::LookupVectorCameraSpace = glm::dvec3(0, 1, 0);

Camera::Camera(const Camera& o)
    : sgctInternal(o.sgctInternal)
    , _position(o._position)
    , _rotation(o._rotation)
    , _scaling(o._scaling)
    , _focusPosition(o._focusPosition)
    , _maxFov(o._maxFov)
    , _cachedViewDirection(o._cachedViewDirection)
    , _cachedLookupVector(o._cachedLookupVector)
{}

void Camera::setPositionVec3(glm::dvec3 pos) {
    std::lock_guard<std::mutex> _lock(_mutex);
    _position = std::move(pos);

    _cachedCombinedViewMatrix.isDirty = true;
}

void Camera::setFocusPositionVec3(glm::dvec3 pos) {
    std::lock_guard<std::mutex> _lock(_mutex);
    _focusPosition = std::move(pos);
}

void Camera::setRotation(glm::dquat rotation) {
    std::lock_guard<std::mutex> _lock(_mutex);
    _rotation = std::move(rotation);
    _cachedViewDirection.isDirty = true;
    _cachedLookupVector.isDirty = true;
    _cachedViewRotationMatrix.isDirty = true;
    _cachedCombinedViewMatrix.isDirty = true;
}

void Camera::setScaling(float scaling) {
    std::lock_guard<std::mutex> _lock(_mutex);
    _scaling = scaling;
    _cachedViewScaleMatrix.isDirty = true;
    _cachedCombinedViewMatrix.isDirty = true;
}

void Camera::setMaxFov(float fov) {
    std::lock_guard<std::mutex> _lock(_mutex);
    _maxFov = fov;
    _cachedSinMaxFov.isDirty = true;
}

void Camera::setParent(SceneGraphNode* parent) {
    _parent = parent;
}

void Camera::rotate(glm::dquat rotation) {
    std::lock_guard<std::mutex> _lock(_mutex);
    _rotation = std::move(rotation) * static_cast<glm::dquat>(_rotation);

    _cachedViewDirection.isDirty = true;
    _cachedLookupVector.isDirty = true;
    _cachedViewRotationMatrix.isDirty = true;
    _cachedCombinedViewMatrix.isDirty = true;
}

const glm::dvec3& Camera::positionVec3() const {
    return _position;
}

glm::dvec3 Camera::eyePositionVec3() const {
    glm::dvec4 eyeInEyeSpace(0.0, 0.0, 0.0, 1.0);

    glm::dmat4 invViewMatrix = glm::inverse(sgctInternal.viewMatrix());
    glm::dmat4 invRotationMatrix = glm::mat4_cast(static_cast<glm::dquat>(_rotation));
    glm::dmat4 invTranslationMatrix = glm::translate(
        glm::dmat4(1.0),
        static_cast<glm::dvec3>(_position)
    );

    glm::dmat4 invViewScale = glm::inverse(viewScaleMatrix());

    glm::dvec4 eyeInWorldSpace = invTranslationMatrix * invRotationMatrix *
        invViewScale * invViewMatrix * eyeInEyeSpace;

    return glm::dvec3(eyeInWorldSpace.x, eyeInWorldSpace.y, eyeInWorldSpace.z);
}

const glm::dvec3& Camera::unsynchedPositionVec3() const {
    return _position;
}

const glm::dvec3& Camera::focusPositionVec3() const {
    return _focusPosition;
}

const glm::dvec3& Camera::viewDirectionWorldSpace() const {
    if (_cachedViewDirection.isDirty) {
        _cachedViewDirection.datum = glm::normalize(
            static_cast<glm::dquat>(_rotation) * ViewDirectionCameraSpace
        );
        _cachedViewDirection.isDirty = true;
    }
    return _cachedViewDirection.datum;
}

const glm::dvec3& Camera::lookUpVectorCameraSpace() const {
    return LookupVectorCameraSpace;
}

const glm::dvec3& Camera::lookUpVectorWorldSpace() const {
    if (_cachedLookupVector.isDirty) {
        _cachedLookupVector.datum = glm::normalize(
            static_cast<glm::dquat>(_rotation) * LookupVectorCameraSpace
        );
        _cachedLookupVector.isDirty = true;
    }

    return _cachedLookupVector.datum;
}

float Camera::maxFov() const {
    return _maxFov;
}

float Camera::sinMaxFov() const {
    if (_cachedSinMaxFov.isDirty) {
        _cachedSinMaxFov.datum = sin(_maxFov);
        _cachedSinMaxFov.isDirty = true;
    }
    return _cachedSinMaxFov.datum;
}

SceneGraphNode* Camera::parent() const {
    return _parent;
}

float Camera::scaling() const {
    return _scaling;
}

const glm::dmat4& Camera::viewRotationMatrix() const {
    if (_cachedViewRotationMatrix.isDirty) {
        _cachedViewRotationMatrix.datum = glm::mat4_cast(
            glm::inverse(static_cast<glm::dquat>(_rotation))
        );
    }
    return _cachedViewRotationMatrix.datum;
}

const glm::dmat4& Camera::viewScaleMatrix() const {
    if (_cachedViewScaleMatrix.isDirty) {
        _cachedViewScaleMatrix.datum = glm::scale(glm::mat4(1.f), glm::vec3(_scaling));
    }
    return _cachedViewScaleMatrix.datum;
}

const glm::dquat& Camera::rotationQuaternion() const {
    return _rotation;
}

const glm::dmat4& Camera::combinedViewMatrix() const {
    if (_cachedCombinedViewMatrix.isDirty) {
        const glm::dmat4 cameraTranslation = glm::inverse(
            glm::translate(glm::dmat4(1.0), static_cast<glm::dvec3>(_position))
        );
        _cachedCombinedViewMatrix.datum =
            glm::dmat4(sgctInternal.viewMatrix()) *
            glm::dmat4(viewScaleMatrix()) *
            glm::dmat4(viewRotationMatrix()) *
            cameraTranslation;
        _cachedCombinedViewMatrix.isDirty = true;
    }
    return _cachedCombinedViewMatrix.datum;
}

void Camera::invalidateCache() {
    _cachedViewDirection.isDirty = true;
    _cachedLookupVector.isDirty = true;
    _cachedViewRotationMatrix.isDirty = true;
    _cachedCombinedViewMatrix.isDirty = true;
}

void Camera::serialize(std::ostream& os) const {
    const glm::dvec3 p = positionVec3();
    const glm::dquat q = rotationQuaternion();
    os << p.x << " " << p.y << " " << p.z << std::endl;
    os << q.x << " " << q.y << " " << q.z << " " << q.w << std::endl;
}

void Camera::deserialize(std::istream& is) {
    glm::dvec3 p;
    glm::dquat q;
    is >> p.x >> p.y >> p.z;
    is >> q.x >> q.y >> q.z >> q.w;
    setPositionVec3(p);
    setRotation(q);
}

Camera::SgctInternal::SgctInternal(const SgctInternal& o)
    : _viewMatrix(o._viewMatrix)
    , _projectionMatrix(o._projectionMatrix)
    , _cachedViewProjectionMatrix(o._cachedViewProjectionMatrix)
{}

void Camera::SgctInternal::setSceneMatrix(glm::mat4 sceneMatrix) {
    std::lock_guard<std::mutex> _lock(_mutex);

    _sceneMatrix = std::move(sceneMatrix);
}

void Camera::SgctInternal::setViewMatrix(glm::mat4 viewMatrix) {
    std::lock_guard<std::mutex> _lock(_mutex);

    _viewMatrix = std::move(viewMatrix);
    _cachedViewProjectionMatrix.isDirty = true;
}

void Camera::SgctInternal::setProjectionMatrix(glm::mat4 projectionMatrix) {
    std::lock_guard<std::mutex> _lock(_mutex);

    _projectionMatrix = std::move(projectionMatrix);
    _cachedViewProjectionMatrix.isDirty = true;
}

const glm::mat4& Camera::SgctInternal::sceneMatrix() const {
    return _sceneMatrix;
}

const glm::mat4& Camera::SgctInternal::viewMatrix() const {
    return _viewMatrix;
}

const glm::mat4& Camera::SgctInternal::projectionMatrix() const {
    return _projectionMatrix;
}

const glm::mat4& Camera::SgctInternal::viewProjectionMatrix() const {
    if (_cachedViewProjectionMatrix.isDirty) {
        std::lock_guard<std::mutex> _lock(_mutex);
        _cachedViewProjectionMatrix.datum = _projectionMatrix * _viewMatrix;
        _cachedViewProjectionMatrix.isDirty = false;
    }
    return _cachedViewProjectionMatrix.datum;
}

// Deprecated
void Camera::setPosition(psc pos) {
    std::lock_guard<std::mutex> _lock(_mutex);
    _position = pos.dvec3();
}

void Camera::setFocusPosition(psc pos) {
    std::lock_guard<std::mutex> _lock(_mutex);
    _focusPosition = pos.dvec3();
}

psc Camera::position() const {
    return psc(static_cast<glm::dvec3>(_position));
}

psc Camera::unsynchedPosition() const {
    return psc(static_cast<glm::dvec3>(_position));
}

psc Camera::focusPosition() const {
    return psc(_focusPosition);
}

const glm::mat4& Camera::viewMatrix() const {
    return sgctInternal.viewMatrix();
}

const glm::mat4& Camera::projectionMatrix() const {
    return sgctInternal.projectionMatrix();
}

const glm::mat4& Camera::viewProjectionMatrix() const {
    return sgctInternal.viewProjectionMatrix();
}

std::vector<Syncable*> Camera::getSyncables() {
    return { &_position, &_rotation, &_scaling };
}

} // namespace openspace
