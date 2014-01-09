#ifndef CAMERA_H
#define CAMERA_H

// open space includes
#include "util/psc.h"

// glm includes
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtc/quaternion.hpp>

namespace openspace {

class Camera {
public:

	// constructors & destructor
	Camera();
	~Camera();

	void setPosition(psc pos);
	const psc& getPosition() const;
	
	void setViewProjectionMatrix(const glm::mat4 &viewProjectionMatrix);
	void setCameraDirection(const glm::vec3 &cameraDirection);
	
	const glm::mat4 & getViewProjectionMatrix() const;
	const glm::mat4 & getViewRotationMatrix() const;
	void compileViewRotationMatrix();
	
	void rotate(glm::quat rotation);
	void setRotation(glm::quat rotation);
	const glm::quat & getRotation() const;

	const glm::vec3 & getViewDirection() const;
	const float & getMaxFov() const;
	const float & getSinMaxFov() const;
	void setMaxFov(const float &fov);
	void setScaling(const glm::vec2 &scaling);
	const glm::vec2 & getScaling() const;
	
private:
	float maxFov_;
	float sinMaxFov_;
	psc position_;
	glm::mat4 viewProjectionMatrix_;
	glm::vec3 viewDirection_;
	glm::vec3 cameraDirection_;
	glm::vec2 scaling_;

	glm::quat viewRotation_;
	glm::mat4 viewRotationMatrix_; // compiled from the quaternion
};

} // namespace openspace

#endif