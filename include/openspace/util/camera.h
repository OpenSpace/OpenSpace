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

#ifndef __CAMERA_H__
#define __CAMERA_H__

#include <mutex>

// open space includes
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/syncbuffer.h>
#include <openspace/rendering/renderengine.h>

// glm includes
#include <ghoul/glm.h>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/type_ptr.hpp>
#include <glm/gtc/quaternion.hpp>

namespace openspace {
    /**
        This class still needs some more love. Suggested improvements:
        - Remove psc from the camera class interface.
        - Accessors should return constant references to double precision class members.
        - Remove the scaling variable (What is it used for?)
        - Remove the maxFov and sinMaxfov variables. Redundant since the fov is embedded
        within the perspective projection matrix.
        - Remove focusposition, part of the integration with the scale graph. The
        "focus position" should not be needed since we assume the camera is always
        positioned relative to its origin. When orbiting another object (not in origin),
        the focus position should probably be handled outside the camera class
        (interaction handler) since it does not affect the state of the camera
        (only how it interacts).
        - The class might need some more reasonable accessors depending on use cases.
        (up vector world space?)
        - Make clear which function returns a combined view matrix (things that are
        dependent on the separate sgct nodes).
    */
    class Camera {
        /**
            Used to explicitly show which variables within the Camera class that are used
            for caching.
        */
        template<typename T>
        struct Cached
        {
            Cached() { isDirty = true; }
            T datum;
            bool isDirty;
        };

        // now working with float precision. To be changed to double later.
        // The reason double does not work yet is because of the setUniform function
        // in ghoul::opengl
        typedef glm::quat Quat;
        typedef glm::mat4 Mat4;
        typedef glm::vec3 Vec3;

        // Static constants
        static const Vec3 _VIEW_DIRECTION_CAMERA_SPACE;
        static const Vec3 _LOOKUP_VECTOR_CAMERA_SPACE;
    public:
        Camera();
        Camera(const Camera& o);
        ~Camera();

        // Mutators
        void setPositionVec3(Vec3 pos);
        void setFocusPositionVec3(Vec3 pos);
        void setRotation(Quat rotation);
        void setScaling(glm::vec2 scaling);
        void setMaxFov(float fov);

        // Relative mutators
        void rotate(Quat rotation);

        // Accessors
        // Remove Vec3 from the name when psc is gone
        const Vec3& positionVec3() const;
        const Vec3& unsynchedPositionVec3() const;
        const Vec3& focusPositionVec3() const;
        // Should return const refs
        const Vec3& viewDirectionWorldSpace() const;
        const Vec3& lookUpVectorCameraSpace() const;
        const glm::vec2& scaling() const;
        const Mat4& viewRotationMatrix() const;
        const Quat& rotationQuaternion() const;
        float maxFov() const;
        float sinMaxFov() const;
        
        // @TODO this should simply be called viewMatrix!
        // Or it needs to be changed so that it actually is combined. Right now it is
        // only the view matrix that is the same for all SGCT cameras.
        const Mat4& combinedViewMatrix() const;

        // Synchronization
        void postSynchronizationPreDraw();
        void preSynchronization();
        void serialize(SyncBuffer* syncBuffer);
        void deserialize(SyncBuffer* syncBuffer);

        /**
            Handles SGCT's internal matrices. Also caches a calculated viewProjection
            matrix. This is the data that is different for different cameras within
            SGCT.
        */
        class SgctInternal {
            friend class Camera;
        public:
            void setViewMatrix(glm::mat4 viewMatrix);
            void setProjectionMatrix(glm::mat4 projectionMatrix);

            const glm::mat4& viewMatrix() const;
            const glm::mat4& projectionMatrix() const;
            const glm::mat4& viewProjectionMatrix() const;
        private:
            SgctInternal();
            SgctInternal(const SgctInternal& o)
                : _viewMatrix(o._viewMatrix)
                , _projectionMatrix(o._projectionMatrix)
                , _cachedViewProjectionMatrix(o._cachedViewProjectionMatrix)
            {}

            // State
            glm::mat4 _viewMatrix;
            glm::mat4 _projectionMatrix;

            // Cache
            mutable Cached<glm::mat4> _cachedViewProjectionMatrix;
            mutable std::mutex _mutex;
        } sgctInternal;

        // Deprecated
        [[deprecated("Replaced by Camera::setPositionVec3()")]]
        void setPosition(psc pos);
        [[deprecated("Replaced by Camera::setFocusPositionVec3()")]]
        void setFocusPosition(psc pos);
        [[deprecated("Replaced by Camera::positionVec3()")]]
        psc position() const;
        [[deprecated("Replaced by Camera::unsynchedPositionVec3()")]]
        psc unsynchedPosition() const;
        [[deprecated("Replaced by Camera::focusPositionVec3()")]]
        psc focusPosition() const;
        // @TODO use Camera::SgctInternal interface instead
        [[deprecated("Replaced by Camera::SgctInternal::viewMatrix()")]]
        const glm::mat4& viewMatrix() const;
        [[deprecated("Replaced by Camera::SgctInternal::projectionMatrix()")]]
        const glm::mat4& projectionMatrix() const;
        [[deprecated("Replaced by Camera::SgctInternal::viewProjectionMatrix()")]]
        const glm::mat4& viewProjectionMatrix() const;
    private:
        /**
            Class encapsulating data that needs to be synched between SGCT nodes.
            Are all three variables (i.e. local, shared, synced) really neccessary? /EB
        */
        template <typename T>
        struct SyncData {
            SyncData() {}
            SyncData(const SyncData& d)
                : local(d.local), shared(d.shared), synced(d.synced) {}

            void serialize(SyncBuffer* syncBuffer) { syncBuffer->encode(shared); }
            void deserialize(SyncBuffer* syncBuffer) { syncBuffer->decode(shared); }
            void postSynchronizationPreDraw() { synced = shared; }
            void preSynchronization() { shared = local; }

            T local;
            T shared;
            T synced;
        };

        // State of the camera
        SyncData<Vec3> _position;
        SyncData<Quat> _rotation;
        SyncData<glm::vec2> _scaling;

        // _focusPosition to be removed
        Vec3 _focusPosition;
        float _maxFov;

        // Cached data
        mutable Cached<Vec3> _cachedViewDirection;
        mutable Cached<Mat4> _cachedViewRotationMatrix;
        mutable Cached<Mat4> _cachedCombinedViewMatrix;
        mutable Cached<float> _cachedSinMaxFov;

        mutable std::mutex _mutex;
    };
} // namespace openspace

#endif // __CAMERA_H__