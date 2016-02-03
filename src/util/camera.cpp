/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace openspace {
    
Camera::Camera()
	: _maxFov(0.f)
	, _sinMaxFov(0.f)
	, _viewProjectionMatrix()
	, _modelMatrix()
	, _viewMatrix()
	, _projectionMatrix()
    , _dirtyViewProjectionMatrix(true)
	, _viewDirection(0,0,-1)
    , _cameraDirection(0.f, 0.f, 0.f)
    , _focusPosition()
	//, _viewRotation(glm::quat(glm::vec3(0.f, 0.f, 0.f)))
	, _localViewRotationMatrix(1.f)
    , _localScaling(1.f, 0.f)
    , _localPosition()
    , _sharedScaling(1.f, 0.f)
	, _sharedPosition()
	, _sharedViewRotationMatrix(1.f)
	, _syncedScaling(1.f, 0.f)
    , _syncedPosition()
	, _syncedViewRotationMatrix(1.f)
{
}

Camera::~Camera()
{
}

void Camera::setPosition(psc pos)
{
    std::lock_guard<std::mutex> _lock(_mutex);
	_localPosition = std::move(pos);
}

const psc& Camera::position() const
{
	return _syncedPosition;
}

const psc& Camera::unsynchedPosition() const{
	return _localPosition;
}

void Camera::setModelMatrix(glm::mat4 modelMatrix){
    std::lock_guard<std::mutex> _lock(_mutex);
	_modelMatrix = std::move(modelMatrix);
}

const glm::mat4& Camera::modelMatrix() const{
	return _modelMatrix;
}

void Camera::setViewMatrix(glm::mat4 viewMatrix){
    std::lock_guard<std::mutex> _lock(_mutex);
	_viewMatrix = std::move(viewMatrix);
    _dirtyViewProjectionMatrix = true;
}

const glm::mat4& Camera::viewMatrix() const{
	return _viewMatrix;
}

void Camera::setProjectionMatrix(glm::mat4 projectionMatrix){
    std::lock_guard<std::mutex> _lock(_mutex);
	_projectionMatrix = std::move(projectionMatrix);
    _dirtyViewProjectionMatrix = true;
}

const glm::mat4& Camera::projectionMatrix() const{
	return _projectionMatrix;
}
    
const glm::mat4& Camera::viewProjectionMatrix() const {
    if (_dirtyViewProjectionMatrix) {
        std::lock_guard<std::mutex> _lock(_mutex);
        _viewProjectionMatrix = _projectionMatrix * _viewMatrix;
        _dirtyViewProjectionMatrix = false;
    }
    return _viewProjectionMatrix;
}

void Camera::setCameraDirection(glm::vec3 cameraDirection)
{
    std::lock_guard<std::mutex> _lock(_mutex);
    _cameraDirection = std::move(cameraDirection);
}

glm::vec3 Camera::cameraDirection() const
{
    return _cameraDirection;
}

void Camera::setViewRotationMatrix(glm::mat4 m) {
    std::lock_guard<std::mutex> _lock(_mutex);
	_localViewRotationMatrix = m;
}

const glm::mat4& Camera::viewRotationMatrix() const
{
	//return _localViewRotationMatrix;
	return _syncedViewRotationMatrix;
}

void Camera::compileViewRotationMatrix()
{
    std::lock_guard<std::mutex> _lock(_mutex);
    // convert from quaternion to rotation matrix using glm
    //_viewRotationMatrix = glm::mat4_cast(_viewRotation);

    // the camera matrix needs to be rotated inverse to the world
   // _viewDirection = glm::rotate(glm::inverse(_viewRotation), _cameraDirection);
	//_viewDirection = (glm::inverse(_localViewRotationMatrix) * glm::vec4(_cameraDirection, 0.f)).xyz;
	_viewDirection = (glm::inverse(_localViewRotationMatrix) * glm::vec4(_cameraDirection, 0.f)).xyz();
    _viewDirection = glm::normalize(_viewDirection);
}

void Camera::rotate(const glm::quat& rotation)
{
    std::lock_guard<std::mutex> _lock(_mutex);
	glm::mat4 tmp = glm::mat4_cast(rotation);
	_localViewRotationMatrix = _localViewRotationMatrix * tmp;
    //_viewRotation = rotation * _viewRotation;
    //_viewRotation = glm::normalize(_viewRotation);
}

void Camera::setRotation(glm::quat rotation)
{
    std::lock_guard<std::mutex> _lock(_mutex);
    //_viewRotation = glm::normalize(std::move(rotation));
	_localViewRotationMatrix = glm::mat4_cast(rotation);
}

void Camera::setRotation(glm::mat4 rotation)
{
    std::lock_guard<std::mutex> _lock(_mutex);
	_localViewRotationMatrix = std::move(rotation);
}

//const glm::quat& Camera::rotation() const
//{
  //  return _viewRotation;
//}

void Camera::setFocusPosition(psc pos){
    std::lock_guard<std::mutex> _lock(_mutex);
	_focusPosition = pos;
}

const psc& Camera::focusPosition() const{
	return _focusPosition;
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
    std::lock_guard<std::mutex> _lock(_mutex);
    _maxFov = fov;
    _sinMaxFov = sin(_maxFov);
}

void Camera::setScaling(glm::vec2 scaling)
{
    std::lock_guard<std::mutex> _lock(_mutex);
	_localScaling = std::move(scaling);
}

const glm::vec2& Camera::scaling() const
{
	//return _localScaling;
	return _syncedScaling;
}

void Camera::setLookUpVector(glm::vec3 lookUp)
{
    std::lock_guard<std::mutex> _lock(_mutex);
    _lookUp = std::move(lookUp);
}

const glm::vec3& Camera::lookUpVector() const
{
    return _lookUp;
}

void Camera::serialize(SyncBuffer* syncBuffer){
	_mutex.lock();

	syncBuffer->encode(_sharedViewRotationMatrix);
	syncBuffer->encode(_sharedPosition);
	syncBuffer->encode(_sharedScaling);

	_mutex.unlock();
}

void Camera::deserialize(SyncBuffer* syncBuffer){	
	_mutex.lock();

	syncBuffer->decode(_sharedViewRotationMatrix);
	syncBuffer->decode(_sharedPosition);
	syncBuffer->decode(_sharedScaling);

	_mutex.unlock();
}

void Camera::postSynchronizationPreDraw(){
	_mutex.lock();

	_syncedViewRotationMatrix = _sharedViewRotationMatrix;
	_syncedPosition = _sharedPosition;
	_syncedScaling = _sharedScaling;

	_mutex.unlock();
}

void Camera::preSynchronization(){
	_mutex.lock();

	_sharedViewRotationMatrix = _localViewRotationMatrix;
	_sharedPosition = _localPosition;
	_sharedScaling = _localScaling;

	_mutex.unlock();
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
