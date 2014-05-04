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

// open space includes
#include <openspace/util/psc.h>

// glm includes
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtc/quaternion.hpp>

namespace openspace {

class Camera {
public:
    Camera();
    ~Camera();

    void setPosition(psc pos);
    const psc& position() const;

    void setViewProjectionMatrix(glm::mat4 viewProjectionMatrix);
    const glm::mat4& viewProjectionMatrix() const;

    void setCameraDirection(glm::vec3 cameraDirection);
    glm::vec3 cameraDirection() const;

    const glm::mat4& viewRotationMatrix() const;
    void compileViewRotationMatrix();

    void rotate(const glm::quat& rotation);
    void setRotation(glm::quat rotation);
    const glm::quat& rotation() const;

    const glm::vec3& viewDirection() const;
    const float& maxFov() const;
    const float& sinMaxFov() const;
    void setMaxFov(float fov);
    void setScaling(glm::vec2 scaling);
    const glm::vec2& scaling() const;

    void setLookUpVector(glm::vec3 lookUp);
    const glm::vec3 lookUpVector() const;

private:
    float _maxFov;
    float _sinMaxFov;
    psc _position;
    glm::mat4 _viewProjectionMatrix;
    glm::vec3 _viewDirection;
    glm::vec3 _cameraDirection;
    glm::vec2 _scaling;

    glm::quat _viewRotation;
    glm::mat4 _viewRotationMatrix;  // compiled from the quaternion

    glm::vec3 _lookUp;
};

} // namespace openspace

#endif // __CAMERA_H__
