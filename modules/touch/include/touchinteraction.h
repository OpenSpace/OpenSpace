/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#ifndef __OPENSPACE_MODULE_TOUCH___TOUCH_INTERACTION___H__
#define __OPENSPACE_MODULE_TOUCH___TOUCH_INTERACTION___H__

#include <openspace/properties/propertyowner.h>

#include <modules/touch/include/directinputsolver.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/doubleproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/triggerproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec4property.h>
#include <array>
#include <chrono>
#include <memory>

// #define TOUCH_DEBUG_PROPERTIES

namespace openspace {

class Camera;
class SceneGraphNode;

// Class used for keeping track of the recent average frame time
class FrameTimeAverage {
public:
    // Update the circular buffer with the most recent frame time
    void updateWithNewFrame(double sample);
    // Get the value of the most recent average frame time (seconds)
    double averageFrameTime() const;

private:
    static constexpr int TotalSamples = 10;
    int _nSamples = 0;
    double _samples[TotalSamples];
    int _index = 0;
};

class TouchInteraction : public properties::PropertyOwner {
public:
    TouchInteraction();

    enum class InteractionType {
        ROTATION = 0,
        PINCH,
        PAN,
        ROLL,
        ZOOM_OUT
    };

    // Stores the velocity in all 6 DOF
    struct VelocityStates {
        glm::dvec2 orbit = glm::dvec2(0.0);
        double zoom = 0.0;
        double roll = 0.0;
        glm::dvec2 pan = glm::dvec2(0.0);
    };

    /**
     * Main function call
     * 1 Checks if doubleTap occured
     * 2 If the node in focus is large enough and all contact points have selected it,
     * calls directControl() function for direct-manipulation
     * 3 Updates std::vector<SelectedBody> _selectedContactPoints (only if LMA successfully
     * converged, avoids interaction to snap on LMA fails)
     * 4 If directControl() wasn't called this frame, interpret the incoming
     * list and decide what type of interaction this frame should do
     * 5 Compute the new total velocities after interaction
     * 6 Evaluate if directControl should be called next frame- true if all contact points
     * select the same node and said node is larger than _nodeRadiusThreshold
     */

    void updateStateFromInput(const std::vector<TouchInputHolder>& list,
        std::vector<TouchInput>& lastProcessed);

    bool hasNonZeroVelocities() const;

    // Calculates the new camera state with velocities and time since last frame
    void step(double dt, bool directTouch = false);

    // Called each frame we have no new input, used to reset data
    void resetAfterInput();

    // Sets _tap to true, called if tap occured current frame (called from touchmodule)
    void tap();

    void setCamera(Camera* camera);

private:
    /**
     * Function that calculates the new camera state such that it minimizes the L2 error
     * in screenspace
     * between contact points and surface coordinates projected to clip space using LMA
     */
    void directControl(const std::vector<TouchInputHolder>& list);

    /**
     * Traces each contact point into the scene as a ray and find the intersection
     * points on the surface of the current anchor node, if any. Saves the input id
     * the node and surface coordinates the cursor hit
     */
    void updateNodeSurfacePoints(const std::vector<TouchInputHolder>& list);

    /**
     * Returns an enum for what interaction to be used, depending on what input was
     * received
     */
    InteractionType interpretInteraction(const std::vector<TouchInputHolder>& list,
        const std::vector<TouchInput>& lastProcessed);

    // Compute new velocity according to the interpreted action
    void computeVelocities(const std::vector<TouchInputHolder>& list,
        const std::vector<TouchInput>& lastProcessed);

    // Compute velocity based on double-tap for zooming
    double computeTapZoomDistance(double zoomGain);

    // Compute coefficient for velocity decay to be applied in decceleration
    double computeConstTimeDecayCoefficient(double velocity);

    /**
     * Decelerate the velocities. Function is called in step() but is dereferenced from
     * frame time to assure same behaviour on all systems
     */
    void decelerate(double dt);

    // Resets all properties that can be changed in the GUI to default
    void resetPropertiesToDefault();

    // Set all velocities to zero
    void resetVelocities();

    Camera* _camera = nullptr;

    // Property variables
    properties::BoolProperty _unitTest;
    properties::BoolProperty _disableZoom;
    properties::BoolProperty _disableRoll;
    properties::TriggerProperty _reset;
    properties::IntProperty _maxTapTime;
    properties::IntProperty _deceleratesPerSecond;
    properties::FloatProperty _touchScreenSize;
    properties::FloatProperty _tapZoomFactor;
    properties::FloatProperty _pinchZoomFactor;
    properties::FloatProperty _rollAngleThreshold;
    properties::FloatProperty _zoomSensitivityExponential;
    properties::FloatProperty _zoomSensitivityProportionalDist;
    properties::FloatProperty _zoomSensitivityDistanceThreshold;
    properties::FloatProperty _zoomInBoundarySphereMultiplier;
    properties::FloatProperty _zoomOutBoundarySphereMultiplier;
    properties::DoubleProperty _zoomInLimit;
    properties::DoubleProperty _zoomOutLimit;
    properties::FloatProperty _inputStillThreshold;
    properties::FloatProperty _centroidStillThreshold;
    properties::BoolProperty  _panEnabled;
    properties::FloatProperty _interpretPan;
    properties::Vec4Property _friction;
    properties::FloatProperty _constTimeDecay_secs;

    properties::BoolProperty _enableDirectManipulation;
    properties::FloatProperty _directTouchDistanceThreshold;

#ifdef TOUCH_DEBUG_PROPERTIES
    struct DebugProperties : PropertyOwner {
        DebugProperties();
        properties::StringProperty interactionMode;
        properties::IntProperty nFingers;
        properties::StringProperty interpretedInteraction;
        properties::FloatProperty normalizedCentroidDistance;
        properties::FloatProperty minDiff;
        properties::FloatProperty rollOn;
    } _debugProperties;

    int pinchConsecCt = 0;
    double pinchConsecZoomFactor = 0;
    int stepVelUpdate = 0;
#endif
    std::array<TouchInputHolder, 2> _pinchInputs;

    // Class variables
    VelocityStates _vel;
    VelocityStates _lastVel;
    VelocityStates _sensitivity;

    bool _isWithinDirectTouchDistance = false;
    double _timeSlack = 0.0;
    std::chrono::milliseconds _time;
    bool _directTouchMode = false;
    bool _wasPrevModeDirectTouch = false;
    bool _tap = false;
    bool _doubleTap = false;
    bool _zoomOutTap = false;
    std::vector<DirectInputSolver::SelectedBody> _selectedNodeSurfacePoints;
    DirectInputSolver _directInputSolver;

    glm::vec2 _centroid = glm::vec2(0.f);

    FrameTimeAverage _frameTimeAvg;

    struct ConstantTimeDecayCoefficients {
        double zoom = 0.0;
        double orbit = 0.0;
        double roll = 0.0;
        double pan = 0.0;
    };
    ConstantTimeDecayCoefficients _constTimeDecayCoeff;
};

} // openspace namespace

#endif // __OPENSPACE_MODULE_TOUCH___TOUCH_INTERACTION___H__
