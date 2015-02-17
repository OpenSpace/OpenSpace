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

#ifndef __CAMERA_H__
#define __CAMERA_H__

#include <mutex>

// open space includes
#include <openspace/util/powerscaledcoordinate.h>

// glm includes
#include <ghoul/glm.h>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtc/quaternion.hpp>

namespace openspace {

//class Camera {
//public:
//    enum class ProjectionMode {
//        Perspective,
//        Orthographic,
//        Frustum,
//        FixedPerspective
//    };
//
//    Camera();
//
//    void setPosition(psc pos);
//    const psc& position() const;
//
//    void setFocus(psc focus);
//    const psc& focus() const;
//
//    void setUpVector(psc upVector);
//    const psc& upVector() const;
//
//    void setScaling(float scaling);
//    float scaling() const;
//
//    const glm::mat4& viewMatrix() const;
//
//    void setProjectionMatrix(glm::mat4 projectionMatrix);
//    const glm::mat4& projectionMatrix() const;
//
//    void setMaxFox(float fov);
//    float maxFov() const;
//
//
//    // derived values
//    psc lookVector() const;
//
//private:
//    void invalidateViewMatrix();
//    void updateViewMatrix() const;  // has to be constant to be called from getter methods
//
//    psc _position;
//    psc _focus;
//    psc _upVector;
//
//    glm::mat4 _projectionMatrix;
//    mutable glm::mat4 _viewMatrix;
//    float _scaling;
//
//    float _maxFov;
//
//    mutable bool _viewMatrixIsDirty;
//};

	class SyncBuffer;

class Camera {
public:
    Camera();
    ~Camera();

    void setPosition(psc pos);
    const psc& position() const;

	void setModelMatrix(glm::mat4 modelMatrix);
	const glm::mat4& modelMatrix() const;

	void setViewMatrix(glm::mat4 viewMatrix);
	const glm::mat4& viewMatrix() const;

	void setProjectionMatrix(glm::mat4 projectionMatrix);
	const glm::mat4& projectionMatrix() const;

    void setViewProjectionMatrix(glm::mat4 viewProjectionMatrix);
    const glm::mat4& viewProjectionMatrix() const;

    void setCameraDirection(glm::vec3 cameraDirection);
    glm::vec3 cameraDirection() const;

	void setFocusPosition(psc pos);
	const psc& focusPosition() const;

	void setViewRotationMatrix(glm::mat4 m);
	const glm::mat4& viewRotationMatrix() const;
    void compileViewRotationMatrix();

    void rotate(const glm::quat& rotation);
    void setRotation(glm::quat rotation);
   // const glm::quat& rotation() const;
	void setRotation(glm::mat4 rotation);

	const glm::vec3& viewDirection() const;

	const float& maxFov() const;
    const float& sinMaxFov() const;
    void setMaxFov(float fov);
    void setScaling(glm::vec2 scaling);
    const glm::vec2& scaling() const;

    void setLookUpVector(glm::vec3 lookUp);
    const glm::vec3& lookUpVector() const;

	void postSynchronizationPreDraw();
	void preSynchronization();
	void serialize(SyncBuffer* syncBuffer);
	void deserialize(SyncBuffer* syncBuffer);

private:
    float _maxFov;
    float _sinMaxFov;
    glm::mat4 _viewProjectionMatrix;
	glm::mat4 _modelMatrix;
	glm::mat4 _viewMatrix;
	glm::mat4 _projectionMatrix;
    glm::vec3 _viewDirection;
    glm::vec3 _cameraDirection;
	psc _focusPosition;
    // glm::quat _viewRotation;
    
    glm::vec3 _lookUp;

	//cluster variables
	std::mutex _syncMutex;

	//local variables
	glm::mat4 _localViewRotationMatrix;
	glm::vec2 _localScaling;
	psc _localPosition;

	//shared copies of local variables
	glm::vec2 _sharedScaling;
	psc _sharedPosition;
	glm::mat4 _sharedViewRotationMatrix;

	//synced copies of local variables
	glm::vec2 _syncedScaling;
	psc _syncedPosition;
	glm::mat4 _syncedViewRotationMatrix;
	
};

} // namespace openspace

#endif // __CAMERA_H__
