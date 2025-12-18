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

#ifndef __OPENSPACE_CORE___IDLEBEHAVIOR___H__
#define __OPENSPACE_CORE___IDLEBEHAVIOR___H__

#include <openspace/properties/propertyowner.h>

#include <openspace/properties/misc/optionproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/util/interpolator.h>
#include <optional>
#include <string_view>

namespace openspace {
    class SceneGraphNode;
} // namespace openspace

namespace openspace::interaction {

class IdleBehavior : public properties::PropertyOwner {
public:
    enum class Behavior {
        Orbit = 0,
        OrbitAtConstantLat,
        OrbitAroundUp
    };

    IdleBehavior();

    void resetIdleBehaviorOnCamera();
    void tickIdleBehaviorTimer(double deltaTime);

    /**
     * Apply one time step of the currently chosen idle behavior.
     *
     * \param anchor The current anchor node
     * \param deltaTime The time step to apply
     * \param speedScale A speed scale factor provided by the caller, to adjust the speed
     *                   of the motion
     * \param position The current position of the camera. Will be changed by the function
     * \param globalRotation The current global rotation of the camera. Will be changed by
     *                       the function
     */
    void applyIdleBehavior(const SceneGraphNode* anchor, double deltaTime,
        double speedScale, glm::dvec3& position, glm::dquat& globalRotation);

    /**
     * Trigger an idle behavior based on a given string, or the default behavior if the
     * string is empty.
     *
     * \param choice A string corresponding to the idle behavior to trigger, or empty for
     *               default behavior
     */
    void triggerIdleBehavior(std::string_view choice = "");

    /**
     * Orbit the current anchor node, in a right-bound orbit, by updating the position
     * and global rotation of the camera.
     *
     * Used for IdleBehavior::Behavior::Orbit
     *
     * \param angle The rotation angle to use for the motion
     * \param position The position of the camera. Will be changed by the function
     * \param globalRotation The camera's global rotation. Will be changed by the function
     */
    static void orbitAnchor(const SceneGraphNode* anchor, double angle,
        glm::dvec3& position, glm::dquat& globalRotation);

    /**
     * Orbit the current anchor node, by adding a rotation around the given axis. For
     * example, when the axis is the north vector, the camera will stay on the current
     * latitude band. Note that this creates a rolling motion if the camera's forward
     * vector coincides with the axis, and should be used with care.
     *
     * Used for:
     *   - IdleBehavior::Behavior::OrbitAtConstantLat (axis = north = z-axis) and
     *   - IdleBehavior::Behavior::OrbitAroundUp (axis = up = y-axis)
     *
     * \param axis The axis to arbit around, given in model coordinates of the anchor
     * \param angle The rotation angle to use for the motion
     * \param position The position of the camera. Will be changed by the function
     * \param globalRotation The camera's global rotation. Will be changed by the function
     */
    static void orbitAroundAxis(const SceneGraphNode* anchor, const glm::dvec3& axis,
        double angle, glm::dvec3& position, glm::dquat& globalRotation);

private:
    properties::BoolProperty _apply;
    properties::BoolProperty _shouldTriggerWhenIdle;
    properties::FloatProperty _idleWaitTime;
    properties::BoolProperty _abortOnCameraInteraction;
    properties::BoolProperty _invert;
    properties::FloatProperty _speedScaleFactor;
    properties::FloatProperty _dampenInterpolationTime;

    properties::OptionProperty _defaultBehavior;
    std::optional<Behavior> _chosenBehavior;

    Interpolator<double> _dampenInterpolator;
    bool _invertInterpolation = false;

    float _triggerTimer = 0.f;
};

} // namespace openspace::interaction

#endif // __OPENSPACE_CORE___IDLEBEHAVIOR___H__
