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

    template<typename T>
    struct CachedDatum
    {
        CachedDatum() { isDirty = true; }
        T datum;
        bool isDirty;
    };

    class Camera {
        // For testing double vs float precision
        typedef glm::dquat Quat;
        typedef glm::dmat4 Mat4;
        typedef glm::dvec3 Vec3;

        // Static constants
        static const Vec3 _VIEW_DIRECTION_CAMERA_SPACE;
        static const Vec3 _LOOKUP_VECTOR_CAMERA_SPACE;
    public:
        Camera();
        Camera(const Camera& o);
        ~Camera();

        // Mutators
        void setPosition(psc pos);
        void setFocusPosition(psc pos);
        void setRotation(Quat rotation);
        void setScaling(glm::vec2 scaling);
        void setMaxFov(float fov);

        // Relative mutators
        void rotate(Quat rotation);

        // Accessors
        const psc& position() const;
        const psc& unsynchedPosition() const;
        const psc& focusPosition() const;
        const glm::vec3 viewDirectionWorldSpace() const;
        const glm::vec3 lookUpVectorCameraSpace() const;
        const glm::vec2& scaling() const;
        const glm::mat4 viewRotationMatrix() const;
        const glm::quat rotationQuaternion() const;
        float maxFov() const;
        float sinMaxFov() const;
        
        //@TODO this should simply be called viewMatrix!
        //Rename after removing deprecated methods
        glm::mat4 combinedViewMatrix() const;

        // DEPRECATED ACCESSORS (GETTERS)
        // @TODO use Camera::SgctInternal interface instead
        [[deprecated("Replaced by Camera::SgctInternal::viewMatrix()")]]
        const glm::mat4& viewMatrix() const;

        [[deprecated("Replaced by Camera::SgctInternal::projectionMatrix()")]]
        const glm::mat4& projectionMatrix() const;

        [[deprecated("Replaced by Camera::SgctInternal::viewProjectionMatrix()")]]
        const glm::mat4& viewProjectionMatrix() const;

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
            mutable CachedDatum<glm::mat4> _cachedViewProjectionMatrix;
            mutable std::mutex _mutex;
        } sgctInternal;
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
        SyncData<Quat> _rotation;
        SyncData<glm::vec2> _scaling;
        SyncData<psc> _position;

        psc _focusPosition;

        float _maxFov;

        // Cached data
        mutable CachedDatum<Vec3> _cachedViewDirection;
        mutable CachedDatum<Mat4> _cachedViewRotationMatrix;
        mutable CachedDatum<Mat4> _cachedCombinedViewMatrix;
        mutable CachedDatum<float> _cachedSinMaxFov;

        mutable std::mutex _mutex;
    };
} // namespace openspace

#endif // __CAMERA_H__