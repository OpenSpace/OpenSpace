/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef __OPENSPACE_CORE___ORBITALINTERACTIONMODE___H__
#define __OPENSPACE_CORE___ORBITALINTERACTIONMODE___H__

#include <openspace/interaction/interactionmode.h>

#include <glm/gtx/quaternion.hpp>

#include <tuple>

namespace openspace {
namespace interaction {

class OrbitalInteractionMode : public InteractionMode
{
public:
    class MouseStates
    {
    public:
        /**
        \param sensitivity
        \param velocityScaleFactor can be set to 60 to remove the inertia of the
        interaction. Lower value will make it harder to move the camera. 
        */
        MouseStates(double sensitivity, double velocityScaleFactor);
        void updateMouseStatesFromInput(const InputState& inputState, double deltaTime);
        void setRotationalFriction(double friction);
        void setHorizontalFriction(double friction);
        void setVerticalFriction(double friction);
        void setSensitivity(double sensitivity);
        void setVelocityScaleFactor(double scaleFactor);

        glm::dvec2 synchedGlobalRotationMouseVelocity();
        glm::dvec2 synchedLocalRotationMouseVelocity();
        glm::dvec2 synchedTruckMovementMouseVelocity();
        glm::dvec2 synchedLocalRollMouseVelocity();
        glm::dvec2 synchedGlobalRollMouseVelocity();

    private:
        double _sensitivity;

        MouseState _globalRotationMouseState;
        MouseState _localRotationMouseState;
        MouseState _truckMovementMouseState;
        MouseState _localRollMouseState;
        MouseState _globalRollMouseState;
    };

    OrbitalInteractionMode(std::shared_ptr<MouseStates> mouseStates);
    ~OrbitalInteractionMode();

    virtual void updateMouseStatesFromInput(const InputState& inputState, double deltaTime);
    virtual void updateCameraStateFromMouseStates(Camera& camera, double deltaTime);
    bool followingNodeRotation() const override;

protected:
    struct CameraRotationDecomposition {
        glm::dquat localRotation;
        glm::dquat globalRotation;
    };

    std::shared_ptr<MouseStates> _mouseStates;

    CameraRotationDecomposition decomposeCameraRotation(const Camera& camera);

    void performRoll(double deltaTime, glm::dquat& localCameraRotation);
    void performLocalRotation(double deltaTime, glm::dquat& localCameraRotation);
    void interpolateLocalRotation(double deltaTime, glm::dquat& localCameraRotation);
    void performHorizontalTranslationAndRotation(
        double deltaTime,
        glm::dvec3 objectPosition,
        glm::dvec3& cameraPosition,
        glm::dquat& globalCameraRotation);
    void performVerticalTranslation(
        double deltaTime,
        double boundingSphere,
        glm::dvec3 objectPosition,
        glm::dvec3& cameraPosition);
    void performHorizontalRotation(
        double deltaTime,
        glm::dvec3 objectPosition,
        glm::dvec3& cameraPosition,
        glm::dquat& globalCameraRotation);
    void pushToSurface(
        double deltaTime,
        double boundingSphere,
        glm::dvec3 objectPosition,
        glm::dvec3& cameraPosition);
};

} // namespace interaction
} // namespace openspace

#endif // __OPENSPACE_CORE___ORBITALINTERACTIONMODE___H__
