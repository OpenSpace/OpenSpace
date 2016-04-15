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

#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtx/vector_angle.hpp>

namespace openspace {
	
Camera::Camera()
	: _maxFov(0.f)
	, _sinMaxFov(0.f)
	, _viewDirection(0,0,-1)
	, _viewDirectionInCameraSpace(0.f, 0.f, -1.f)
	, _focusPosition()
{
	_scaling.local = glm::vec2(1.f, 0.f);
	_viewRotationMatrix.local = glm::mat4(1.0f);
	_position.local = psc();
}

Camera::~Camera() { }



//////////////////////////////////////////////////////////////////////////////////////////
//							CAMERA MUTATORS (SETTERS)									//
//////////////////////////////////////////////////////////////////////////////////////////

void Camera::setPosition(psc pos){
	std::lock_guard<std::mutex> _lock(_mutex);
	_position.local = std::move(pos);
}

void Camera::setFocusPosition(psc pos) {
	std::lock_guard<std::mutex> _lock(_mutex);
	_focusPosition = pos;
}

void Camera::setRotation(glm::quat rotation) {
	std::lock_guard<std::mutex> _lock(_mutex);
	_viewRotationMatrix.local = glm::mat4_cast(glm::normalize(rotation));
}

void Camera::setLookUpVector(glm::vec3 lookUp) {
	std::lock_guard<std::mutex> _lock(_mutex);
	_lookUp = std::move(lookUp);
}

void Camera::setScaling(glm::vec2 scaling) {
	std::lock_guard<std::mutex> _lock(_mutex);
	_scaling.local = std::move(scaling);
}

void Camera::setMaxFov(float fov) {
	std::lock_guard<std::mutex> _lock(_mutex);
	_maxFov = fov;
	_sinMaxFov = sin(_maxFov);
}


//////////////////////////////////////////////////////////////////////////////////////////
//							CAMERA ACCESSORS (GETTERS)									//
//////////////////////////////////////////////////////////////////////////////////////////


const psc& Camera::position() const {
	return _position.synced;
}

const psc& Camera::unsynchedPosition() const {
	return _position.local;
}

const psc& Camera::focusPosition() const {
	return _focusPosition;
}

const glm::vec3& Camera::viewDirection() const {
	return _viewDirection;
}

const glm::vec3& Camera::lookUpVector() const {
	return _lookUp;
}

const glm::vec2& Camera::scaling() const {
	return _scaling.synced;
}

float Camera::maxFov() const {
	return _maxFov;
}

float Camera::sinMaxFov() const {
	return _sinMaxFov;
}

const glm::mat4& Camera::viewRotationMatrix() const {
	return _viewRotationMatrix.synced;
}

const glm::mat4& Camera::combinedViewMatrix() const {
	glm::vec3 cameraPosition = position().vec3();
	glm::mat4 viewTransform = glm::inverse(glm::translate(glm::mat4(1.0), cameraPosition));
	viewTransform = glm::mat4(viewRotationMatrix()) * viewTransform;
	return viewTransform;
}





//////////////////////////////////////////////////////////////////////////////////////////
//						DEPRECATED CAMERA ACCESSORS (GETTERS)							//
//////////////////////////////////////////////////////////////////////////////////////////

const glm::mat4& Camera::viewMatrix() const {
	return sgctInternal.viewMatrix();
}

const glm::mat4& Camera::projectionMatrix() const {
	return sgctInternal.projectionMatrix();
}

const glm::mat4& Camera::viewProjectionMatrix() const {
	return sgctInternal.viewProjectionMatrix();
}




//////////////////////////////////////////////////////////////////////////////////////////
//								CAMERA RELATICVE MUTATORS								//
//////////////////////////////////////////////////////////////////////////////////////////


void Camera::rotate(const glm::quat& rotation) {
	std::lock_guard<std::mutex> _lock(_mutex);
	glm::mat4 tmp = glm::mat4_cast(rotation);
	_viewRotationMatrix.local = _viewRotationMatrix.local * tmp;
}




//////////////////////////////////////////////////////////////////////////////////////////
//								CAMERA SYNCHRONIZATION									//
//////////////////////////////////////////////////////////////////////////////////////////

void Camera::serialize(SyncBuffer* syncBuffer){
	_mutex.lock();

	_viewRotationMatrix.serialize(syncBuffer);
	_position.serialize(syncBuffer);
	_scaling.serialize(syncBuffer);

	_mutex.unlock();
}

void Camera::deserialize(SyncBuffer* syncBuffer){	
	_mutex.lock();

	_viewRotationMatrix.deserialize(syncBuffer);
	_position.deserialize(syncBuffer);
	_scaling.deserialize(syncBuffer);

	_mutex.unlock();
}

void Camera::postSynchronizationPreDraw(){
	_mutex.lock();

	_viewRotationMatrix.postSynchronizationPreDraw();
	_position.postSynchronizationPreDraw();
	_scaling.postSynchronizationPreDraw();
	
	
	glm::vec4 localViewDir = glm::vec4(_viewDirectionInCameraSpace, 0.f);
	_viewDirection = (glm::inverse(_viewRotationMatrix.local) * localViewDir).xyz();
	_viewDirection = glm::normalize(_viewDirection);

	_mutex.unlock();
}

void Camera::preSynchronization(){
	_mutex.lock();

	_viewRotationMatrix.preSynchronization();
	_position.preSynchronization();
	_scaling.preSynchronization();

	_mutex.unlock();
}





//////////////////////////////////////////////////////////////////////////////////////////
//								SGCT NODE DEPENTENT 									//
//////////////////////////////////////////////////////////////////////////////////////////
Camera::SgctInternal::SgctInternal()
	: _viewMatrix()
	, _projectionMatrix()
	, _dirtyViewProjectionMatrix(true)
{

}

void Camera::SgctInternal::setViewMatrix(glm::mat4 viewMatrix) {
	std::lock_guard<std::mutex> _lock(_mutex);
	_viewMatrix = std::move(viewMatrix);
	_dirtyViewProjectionMatrix = true;
}


void Camera::SgctInternal::setProjectionMatrix(glm::mat4 projectionMatrix) {
	std::lock_guard<std::mutex> _lock(_mutex);
	_projectionMatrix = std::move(projectionMatrix);
	_dirtyViewProjectionMatrix = true;
}

const glm::mat4& Camera::SgctInternal::viewMatrix() const {
	return _viewMatrix;
}

const glm::mat4& Camera::SgctInternal::projectionMatrix() const {
	return _projectionMatrix;
}

const glm::mat4& Camera::SgctInternal::viewProjectionMatrix() const {
	if (_dirtyViewProjectionMatrix) {
		std::lock_guard<std::mutex> _lock(_mutex);
		_viewProjectionMatrix = _projectionMatrix * _viewMatrix;
		_dirtyViewProjectionMatrix = false;
	}
	return _viewProjectionMatrix;
}

} // namespace openspace
