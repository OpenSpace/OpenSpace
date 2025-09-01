/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___CAMERAINTERACTIONSTATES___H__
#define __OPENSPACE_CORE___CAMERAINTERACTIONSTATES___H__

#include <openspace/interaction/delayedvariable.h>
#include <ghoul/glm.h>

namespace openspace::interaction {

class CameraInteractionStates {
public:
    /**
     * \param sensitivity Interaction sensitivity
     * \param velocityScaleFactor Can be set to 60 to remove the inertia of the
     *        interaction. Lower value will make it harder to move the camera
     */
    CameraInteractionStates(double sensitivity, double velocityScaleFactor);
    virtual ~CameraInteractionStates() = default;

    void setRotationalFriction(double friction);
    void setHorizontalFriction(double friction);
    void setVerticalFriction(double friction);
    void setSensitivity(double sensitivity);
    void setVelocityScaleFactor(double scaleFactor);

    glm::dvec2 globalRotationVelocity() const;
    glm::dvec2 localRotationVelocity() const;
    double truckMovementVelocity() const;
    double localRollVelocity() const;
    double globalRollVelocity() const;

    void resetVelocities();

    /**
     * Returns true if any of the velocities are larger than zero, i.e. wether an
     * interaction happened.
     */
    bool hasNonZeroVelocities(bool checkOnlyMovement = false) const;

protected:
    template <typename T>
    struct InteractionState {
        InteractionState(double scaleFactor);
        void setFriction(double friction);
        void setVelocityScaleFactor(double scaleFactor);

        T previousValue = T(0.0);
        DelayedVariable<T, double> velocity;
    };

    double _sensitivity = 0.0;

    InteractionState<glm::dvec2> _globalRotationState;
    InteractionState<glm::dvec2> _localRotationState;
    InteractionState<double> _truckMovementState;
    InteractionState<double> _localRollState;
    InteractionState<double> _globalRollState;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___CAMERAINTERACTIONSTATES___H__
