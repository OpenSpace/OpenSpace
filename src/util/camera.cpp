
// open space includes
#include <openspace/util/camera.h>

// sgct includes
#include "sgct.h"

#include <glm/gtx/vector_angle.hpp>

namespace openspace {
	
Camera::Camera() {
	scaling_ = glm::vec2(1.0,0.0);
	glm::vec3 EulerAngles(0, 0, 0);
	viewRotation_ = glm::quat(EulerAngles);
}

Camera::~Camera() {

}

void Camera::setPosition(psc pos) {
    position_ = pos;
}

const psc& Camera::getPosition() const {
	return position_;
}


void Camera::setViewProjectionMatrix(const glm::mat4 &viewProjectionMatrix) {
	viewProjectionMatrix_ = viewProjectionMatrix;
}

void Camera::setCameraDirection(const glm::vec3 &cameraDirection) {
	cameraDirection_ = cameraDirection;
}

const glm::mat4 & Camera::getViewProjectionMatrix() const {
	return viewProjectionMatrix_;
}

const glm::mat4 & Camera::getViewRotationMatrix() const {
	return viewRotationMatrix_;
}

void Camera::compileViewRotationMatrix() {
	// convert from quaternion to rotationmatrix using glm
	viewRotationMatrix_ = glm::mat4_cast(viewRotation_);

	// the camera matrix needs to be rotated inverse to the world
	viewDirection_ = glm::rotate(glm::inverse(viewRotation_), cameraDirection_);
	viewDirection_ = glm::normalize(viewDirection_);
}

void Camera::rotate(glm::quat rotation) {
	viewRotation_ = rotation * viewRotation_;
	viewRotation_ = glm::normalize(viewRotation_);
}

void Camera::setRotation(glm::quat rotation) {
	viewRotation_ = glm::normalize(rotation);
}

const glm::quat & Camera::getRotation() const {
	return viewRotation_;
}

const glm::vec3 & Camera::getViewDirection() const {
	return viewDirection_;
}

const float & Camera::getMaxFov() const {
	return maxFov_;
}

const float & Camera::getSinMaxFov() const {
	return sinMaxFov_;
}

void Camera::setMaxFov(const float &fov) {
	maxFov_ = fov;
	sinMaxFov_ = sin(maxFov_);
}

void Camera::setScaling(const glm::vec2 &scaling) {
	scaling_ = scaling;
}

const glm::vec2 & Camera::getScaling() const {
	return scaling_;
}

void Camera::setLookUp(glm::vec3 lookUp) {
	lookUp_ = lookUp;
}

const glm::vec3 Camera::getLookUp() const {
	return lookUp_;
}
	
} // namespace openspace
