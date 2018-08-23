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

#ifndef __OPENSPACE_CORE___CAMERA___H__
#define __OPENSPACE_CORE___CAMERA___H__

#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/syncdata.h>
#include <ghoul/glm.h>
#include <mutex>

namespace openspace {

class SceneGraphNode;
class SyncBuffer;

/**
 * This class still needs some more love. Suggested improvements:
 * - Remove psc from the camera class interface.
 * - Accessors should return constant references to double precision class members.
 * - Remove the scaling variable (What is it used for?)
 * - Remove the maxFov and sinMaxfov variables. Redundant since the fov is embedded
 * within the perspective projection matrix.
 * - Remove focusposition, part of the integration with the scale graph. The
 * "focus position" should not be needed since we assume the camera is always
 * positioned relative to its origin. When orbiting another object (not in origin),
 * the focus position should probably be handled outside the camera class
 * (interaction handler) since it does not affect the state of the camera
 * (only how it interacts).
 * - The class might need some more reasonable accessors depending on use cases.
 * (up vector world space?)
 * - Make clear which function returns a combined view matrix (things that are
 * dependent on the separate sgct nodes).
 */
class Camera {
public:
    /**
     * Used to explicitly show which variables within the Camera class that are used
     * for caching.
     */
    template<typename T>
    struct Cached {
        Cached() { isDirty = true; }
        T datum;
        bool isDirty;
    };

    Camera() = default;
    Camera(const Camera& o);
    ~Camera() = default;

    // Mutators
    void setPositionVec3(glm::dvec3 pos);
    void setFocusPositionVec3(glm::dvec3 pos);
    void setRotation(glm::dquat rotation);
    void setScaling(float scaling);
    void setMaxFov(float fov);
    void setParent(SceneGraphNode* parent);

    // Relative mutators
    void rotate(glm::dquat rotation);

    // Accessors
    // Remove Vec3 from the name when psc is gone
    const glm::dvec3& positionVec3() const;
    glm::dvec3 eyePositionVec3() const;
    const glm::dvec3& unsynchedPositionVec3() const;
    const glm::dvec3& focusPositionVec3() const;
    const glm::dvec3& viewDirectionWorldSpace() const;
    const glm::dvec3& lookUpVectorCameraSpace() const;
    const glm::dvec3& lookUpVectorWorldSpace() const;
    const glm::dmat4& viewRotationMatrix() const;
    const glm::dmat4& viewScaleMatrix() const;
    const glm::dquat& rotationQuaternion() const;
    float maxFov() const;
    float sinMaxFov() const;
    SceneGraphNode* parent() const;
    float scaling() const;

    // @TODO this should simply be called viewMatrix!
    // Or it needs to be changed so that it actually is combined. Right now it is
    // only the view matrix that is the same for all SGCT cameras.
    // Right now this function returns the actual combined matrix which makes some
    // of the old calls to the function wrong..
    const glm::dmat4& combinedViewMatrix() const;

    void invalidateCache();

    void serialize(std::ostream& os) const;
    void deserialize(std::istream& is);

    /**
     * Handles SGCT's internal matrices. Also caches a calculated viewProjection
     * matrix. This is the data that is different for different cameras within SGCT.
     */
    class SgctInternal {
        friend class Camera;
    public:
        void setSceneMatrix(glm::mat4 sceneMatrix);
        void setViewMatrix(glm::mat4 viewMatrix);
        void setProjectionMatrix(glm::mat4 projectionMatrix);

        const glm::mat4& sceneMatrix() const;
        const glm::mat4& viewMatrix() const;
        const glm::mat4& projectionMatrix() const;
        const glm::mat4& viewProjectionMatrix() const;

    private:
        SgctInternal() = default;
        SgctInternal(const SgctInternal& o);

        glm::mat4 _sceneMatrix;
        glm::mat4 _viewMatrix;
        glm::mat4 _projectionMatrix;

        mutable Cached<glm::mat4> _cachedViewProjectionMatrix;
        mutable std::mutex _mutex;
    } sgctInternal;

    // Deprecated
    // [[deprecated("Replaced by Camera::setPositionVec3()")]]
    void setPosition(psc pos);
    // [[deprecated("Replaced by Camera::setFocusPositionVec3()")]]
    void setFocusPosition(psc pos);
    // [[deprecated("Replaced by Camera::positionVec3()")]]
    psc position() const;
    // [[deprecated("Replaced by Camera::unsynchedPositionVec3()")]]
    psc unsynchedPosition() const;
    // [[deprecated("Replaced by Camera::focusPositionVec3()")]]
    psc focusPosition() const;
    const glm::mat4& sceneMatrix() const;
    // @TODO use Camera::SgctInternal interface instead
    // [[deprecated("Replaced by Camera::SgctInternal::viewMatrix()")]]
    const glm::mat4& viewMatrix() const;
    // [[deprecated("Replaced by Camera::SgctInternal::projectionMatrix()")]]
    const glm::mat4& projectionMatrix() const;
    // [[deprecated("Replaced by Camera::SgctInternal::viewProjectionMatrix()")]]
    const glm::mat4& viewProjectionMatrix() const;

    std::vector<Syncable*> getSyncables();

private:
    // Static constants
    static const glm::dvec3 ViewDirectionCameraSpace;
    static const glm::dvec3 LookupVectorCameraSpace;

    SyncData<glm::dvec3> _position = glm::dvec3(1.0, 1.0, 1.0);
    SyncData<glm::dquat> _rotation  = glm::dquat(glm::dvec3(1.0, 1.0, 1.0));
    SyncData<float> _scaling = 1.f;
    SceneGraphNode* _parent = nullptr;

    // _focusPosition to be removed
    glm::dvec3 _focusPosition;
    float _maxFov = 0.f;

    // Cached data
    mutable Cached<glm::dvec3> _cachedViewDirection;
    mutable Cached<glm::dvec3> _cachedLookupVector;
    mutable Cached<glm::dmat4> _cachedViewRotationMatrix;
    mutable Cached<glm::dmat4> _cachedViewScaleMatrix;
    mutable Cached<glm::dmat4> _cachedCombinedViewMatrix;
    mutable Cached<float> _cachedSinMaxFov;

    mutable std::mutex _mutex;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___CAMERA___H__
