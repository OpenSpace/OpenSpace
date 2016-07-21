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

// open space includes
#include <openspace/util/camera.h>
#include <openspace/util/syncbuffer.h>
#include <openspace/query/query.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>


#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace openspace {

    //////////////////////////////////////////////////////////////////////////////////////
    //								        CAMERA	                                    //
    //////////////////////////////////////////////////////////////////////////////////////

    namespace {
        const std::string _loggerCat = "Camera";
    }

    const Camera::Vec3 Camera::_VIEW_DIRECTION_CAMERA_SPACE = Camera::Vec3(0, 0, -1);
    const Camera::Vec3 Camera::_LOOKUP_VECTOR_CAMERA_SPACE = Camera::Vec3(0, 1, 0);

    Camera::Camera()
        : _maxFov(0.f)
        , _focusPosition()
    {
        _scaling.local = glm::vec2(1.f, 0.f);
        _position.local = Vec3(1.0, 1.0, 1.0);
        Vec3 eulerAngles(1.0, 1.0, 1.0);
        _rotation.local = Quat(eulerAngles);
    }

    Camera::Camera(const Camera& o)
        : sgctInternal(o.sgctInternal)
        , _focusPosition(o._focusPosition)
        , _cachedViewDirection(o._cachedViewDirection)
        , _cachedLookupVector(o._cachedLookupVector)
        , _rotation(o._rotation)
        , _scaling(o._scaling)
        , _position(o._position)
        , _maxFov(o._maxFov)
    { }

    Camera::~Camera() { }

    // Mutators
    void Camera::setPositionVec3(Vec3 pos) {
        std::lock_guard<std::mutex> _lock(_mutex);
        _position.local = pos;
      
        _cachedCombinedViewMatrix.isDirty = true;
    }

    void Camera::setFocusPositionVec3(Vec3 pos) {
        std::lock_guard<std::mutex> _lock(_mutex);
        _focusPosition = pos;
      
        _cachedViewDirection.isDirty = true;
        _cachedLookupVector.isDirty = true;
        _cachedViewRotationMatrix.isDirty = true;
        _cachedCombinedViewMatrix.isDirty = true;
    }

    void Camera::setRotation(Quat rotation) {
        std::lock_guard<std::mutex> _lock(_mutex);
        _rotation.local = rotation;
        _cachedViewDirection.isDirty = true;
        _cachedLookupVector.isDirty = true;
        _cachedViewRotationMatrix.isDirty = true;
        _cachedCombinedViewMatrix.isDirty = true;
    }

    void Camera::setScaling(glm::vec2 scaling) {
        std::lock_guard<std::mutex> _lock(_mutex);
        _scaling.local = std::move(scaling);
    }

    void Camera::setMaxFov(float fov) {
        std::lock_guard<std::mutex> _lock(_mutex);
        _maxFov = fov;
        _cachedSinMaxFov.isDirty = true;
    }

    // Relative mutators
    void Camera::rotate(Quat rotation) {
        std::lock_guard<std::mutex> _lock(_mutex);
        _rotation.local = rotation * _rotation.local;
      
        _cachedViewDirection.isDirty = true;
        _cachedLookupVector.isDirty = true;
        _cachedViewRotationMatrix.isDirty = true;
        _cachedCombinedViewMatrix.isDirty = true;
    }

    // Accessors
    const Camera::Vec3& Camera::positionVec3() const {
        return _position.synced;
    }

    const Camera::Vec3& Camera::unsynchedPositionVec3() const {
        return _position.local;
    }

    const Camera::Vec3& Camera::focusPositionVec3() const {
        return _focusPosition;
    }

    const Camera::Vec3& Camera::viewDirectionWorldSpace() const {
        if (_cachedViewDirection.isDirty) {
            _cachedViewDirection.datum =
                _rotation.synced * Vec3(_VIEW_DIRECTION_CAMERA_SPACE);
            _cachedViewDirection.datum = glm::normalize(_cachedViewDirection.datum);
            _cachedViewDirection.isDirty = true;
        }
        return _cachedViewDirection.datum;
    }

    const Camera::Vec3& Camera::lookUpVectorCameraSpace() const {
        return _LOOKUP_VECTOR_CAMERA_SPACE;
    }

    const Camera::Vec3& Camera::lookUpVectorWorldSpace() const {
        if (_cachedLookupVector.isDirty) {
            _cachedLookupVector.datum =
                _rotation.synced * Vec3(_LOOKUP_VECTOR_CAMERA_SPACE);
            _cachedLookupVector.datum = glm::normalize(_cachedLookupVector.datum);
            _cachedLookupVector.isDirty = true;
        }
        return _cachedLookupVector.datum;
    }

    const glm::vec2& Camera::scaling() const {
        return _scaling.synced;
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

    const Camera::Mat4& Camera::viewRotationMatrix() const {
        if (_cachedViewRotationMatrix.isDirty) {
            _cachedViewRotationMatrix.datum = glm::mat4_cast(glm::inverse(_rotation.synced));
        }
        return _cachedViewRotationMatrix.datum;
    }

    const Camera::Quat& Camera::rotationQuaternion() const {
        return _rotation.synced;
    }

    const Camera::Mat4& Camera::combinedViewMatrix() const {
        if (_cachedCombinedViewMatrix.isDirty) {
            Mat4 cameraTranslation =
                glm::inverse(glm::translate(Mat4(1.0), _position.synced));
            _cachedCombinedViewMatrix.datum =
                Mat4(sgctInternal.viewMatrix()) *
                Mat4(viewRotationMatrix()) *
                cameraTranslation;
            _cachedCombinedViewMatrix.isDirty = true;
        }
        return _cachedCombinedViewMatrix.datum;
    }

    // Synchronization
    void Camera::serialize(SyncBuffer* syncBuffer) {
        std::lock_guard<std::mutex> _lock(_mutex);

        _rotation.serialize(syncBuffer);
        _position.serialize(syncBuffer);
        _scaling.serialize(syncBuffer);
    }

    void Camera::deserialize(SyncBuffer* syncBuffer) {
        std::lock_guard<std::mutex> _lock(_mutex);

        _rotation.deserialize(syncBuffer);
        _position.deserialize(syncBuffer);
        _scaling.deserialize(syncBuffer);
    }

    void Camera::postSynchronizationPreDraw() {
        std::lock_guard<std::mutex> _lock(_mutex);

        _rotation.postSynchronizationPreDraw();
        _position.postSynchronizationPreDraw();
        _scaling.postSynchronizationPreDraw();

        _cachedViewDirection.isDirty = true;
        _cachedLookupVector.isDirty = true;
        _cachedViewRotationMatrix.isDirty = true;
        _cachedCombinedViewMatrix.isDirty = true;
    }

    
    void Camera::serialize(std::ostream& os) const {
        Vec3 p = positionVec3();
        Quat q = rotationQuaternion();
        os << p.x << " " << p.y << " " << p.z << std::endl;
        os << q.x << " " << q.y << " " << q.z << " " << q.w << std::endl;
    }

    void Camera::deserialize(std::istream& is) {
        Vec3 p;
        Quat q;
        is >> p.x >> p.y >> p.z;
        is >> q.x >> q.y >> q.z >> q.w;
        setPositionVec3(p);
        setRotation(q);
    }

    void Camera::preSynchronization() {
        std::lock_guard<std::mutex> _lock(_mutex);

        _rotation.preSynchronization();
        _position.preSynchronization();
        _scaling.preSynchronization();
    }

    //////////////////////////////////////////////////////////////////////////////////////
    //								    SGCT INTERNAL	                                //
    //////////////////////////////////////////////////////////////////////////////////////
    Camera::SgctInternal::SgctInternal()
        : _viewMatrix()
        , _projectionMatrix()
    { }

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
        _position.local = pos.dvec3();
    }

    void Camera::setFocusPosition(psc pos) {
        std::lock_guard<std::mutex> _lock(_mutex);
        _focusPosition = pos.dvec3();
    }

    psc Camera::position() const {
        return psc(_position.synced);
    }

    psc Camera::unsynchedPosition() const {
        return psc(_position.local);
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
} // namespace openspace
