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

#ifndef __OPENSPACE_MODULE_TOUCH___TOUCH_INTERACTION___H__
#define __OPENSPACE_MODULE_TOUCH___TOUCH_INTERACTION___H__

#include <openspace/properties/propertyowner.h>

#include <modules/touch/ext/levmarq.h>
#include <modules/touch/include/tuioear.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/properties/stringproperty.h>
#include <openspace/properties/vector/ivec2property.h>
#include <openspace/properties/vector/vec4property.h>

//#define TOUCH_DEBUG_PROPERTIES

namespace openspace {

class Camera;
class SceneGraphNode;

//Class used for keeping track of the recent average frame time
class FrameTimeAverage {
public:
    //Update the circular buffer with the most recent frame time
    void updateWithNewFrame(double sample);
    //Get the value of the most recent average frame time (seconds)
    double averageFrameTime() const;

private:
    static const int TotalSamples = 10;
    int _nSamples = 0;
    double _samples[TotalSamples];
    double _runningTotal = 0.0;
    int _index = 0;
};

class TouchInteraction : public properties::PropertyOwner {
public:
    using Point = std::pair<int, TUIO::TuioPoint>;

    TouchInteraction();

    // for interpretInteraction()
    enum Type { ROT = 0, PINCH, PAN, ROLL, PICK, ZOOM_OUT };

    // Stores the velocity in all 6DOF
    struct VelocityStates {
        glm::dvec2 orbit;
        double zoom;
        double roll;
        glm::dvec2 pan;
    };

    // Stores the selected node, the cursor ID as well as the surface coordinates the
    // cursor touched
    struct SelectedBody {
        long id;
        SceneGraphNode* node;
        glm::dvec3 coordinates;
    };

    // Used in the LM algorithm
    struct FunctionData {
        std::vector<glm::dvec3> selectedPoints;
        std::vector<glm::dvec2> screenPoints;
        int nDOF;
        glm::dvec2(*castToNDC)(const glm::dvec3&, Camera&, SceneGraphNode*);
        double(*distToMinimize)(double* par, int x, void* fdata, LMstat* lmstat);
        Camera* camera;
        SceneGraphNode* node;
        LMstat stats;
        double objectScreenRadius;
    };

    /* Main function call
     * 1 Checks if doubleTap occured
     * 2 Goes through the guiMode() function
     * 3 Continues if GUI isn't on
     * 4 If the node in focus is large enough and all contact points have selected it,
     * calls directControl() function for direct-manipulation
     * 5 Updates std::vector<SelectedBody> _selected (only if LMA successfully
     * converged, avoids interaction to snap on LMA fails)
     * 6 If directControl() wasn't called this frame, interpret the incoming
     * list and decide what type of interaction this frame should do
     * 7 Compute the new total velocities after interaction
     * 8 Evaluate if directControl should be called next frame- true if all contact points
     * select the same node and said node is larger than _nodeRadiusThreshold
    */
    void updateStateFromInput(const std::vector<TUIO::TuioCursor>& list,
        std::vector<Point>& lastProcessed);

    // Calculates the new camera state with velocities and time since last frame
    void step(double dt);

    // Used to save LMA data for one frame if the user chose to
    void unitTest();

    // Called each frame we have no new input, used to reset data
    void resetAfterInput();

    // Sets _tap to true, called if tap occured current frame (called from touchmodule)
    void tap();
    // Set touchactive as true from the touchmodule if incoming list isn't empty, used to
    // void mouse input
    void touchActive(bool active);

    // Get & Setters
    Camera* getCamera();
    SceneGraphNode* getFocusNode();
    void setFocusNode(SceneGraphNode* focusNode);
    void setCamera(Camera* camera);

private:
    /* Returns true if we have the GUI window open. If so, emulates the incoming touch
     * input to a mouse such that we can interact with the GUI
     */
    bool guiMode(const std::vector<TUIO::TuioCursor>& list);

    /* Function that calculates the new camera state such that it minimizes the L2 error
     * in screenspace
     * between contact points and surface coordinates projected to clip space using LMA
     */
    void directControl(const std::vector<TUIO::TuioCursor>& list);

    /* Traces each contact point into the scene as a ray
     * if the ray hits a node, save the id, node and surface coordinates the cursor hit
     * in the list _selected
     */
    void findSelectedNode(const std::vector<TUIO::TuioCursor>& list);

    /* Returns an int (ROT = 0, PINCH, PAN, ROLL, PICK) for what interaction to be used,
     * depending on what input was gotten
     */
    int interpretInteraction(const std::vector<TUIO::TuioCursor>& list,
        const std::vector<Point>& lastProcessed);

    // Compute new velocity according to the interpreted action
    void computeVelocities(const std::vector<TUIO::TuioCursor>& list,
        const std::vector<Point>& lastProcessed);

    //Compute velocity based on double-tap for zooming
    double computeTapZoomDistance(double zoomGain);

    //Compute coefficient for velocity decay to be applied in decceleration
    double computeConstTimeDecayCoefficient(double velocity);

    //Compute coefficient of decay based on current frametime; if frametime has been
    // longer than usual then multiple decay steps may be applied to keep the decay
    // relative to user time
    double computeDecayCoeffFromFrametime(double coeff, int times);

    /* Decelerate the velocities. Function is called in step() but is dereferenced from
     * frame time to assure same behaviour on all systems
     */
    void decelerate(double dt);

    // Resets all properties that can be changed in the GUI to default
    void resetToDefault();

    Camera* _camera = nullptr;
    SceneGraphNode* _focusNode = nullptr;

    // Property variables
    properties::StringProperty _origin;
    properties::BoolProperty _unitTest;
    properties::BoolProperty _touchActive;
    properties::BoolProperty _reset;
    properties::IntProperty _maxTapTime;
    properties::IntProperty _deceleratesPerSecond;
    properties::FloatProperty _touchScreenSize;
    properties::FloatProperty _tapZoomFactor;
    properties::FloatProperty _nodeRadiusThreshold;
    properties::FloatProperty _rollAngleThreshold;
    properties::FloatProperty _orbitSpeedThreshold;
    properties::FloatProperty _spinSensitivity;
    properties::FloatProperty _zoomSensitivityExponential;
    properties::FloatProperty _zoomSensitivityProportionalDist;
    properties::FloatProperty _zoomSensitivityDistanceThreshold;
    properties::FloatProperty _zoomBoundarySphereMultiplier;
    properties::FloatProperty _inputStillThreshold;
    properties::FloatProperty _centroidStillThreshold;
    properties::BoolProperty  _panEnabled;
    properties::FloatProperty _interpretPan;
    properties::FloatProperty _slerpTime;
    properties::IVec2Property _guiButton;
    properties::Vec4Property _friction;
    properties::FloatProperty _pickingRadiusMinimum;
    properties::BoolProperty _ignoreGui;
    properties::FloatProperty _constTimeDecay_secs;

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
    //int stepVelUpdate = 0;
#endif

    // Class variables
    VelocityStates _vel;
    VelocityStates _lastVel;
    VelocityStates _sensitivity;

    double _projectionScaleFactor;
    double _currentRadius;
    double _slerpdT;
    double _timeSlack;
    int _numOfTests;
    TUIO::TuioTime _time;
    bool _directTouchMode;
    bool _tap;
    bool _doubleTap;
    bool _zoomOutTap;
    bool _lmSuccess;
    bool _guiON;
    std::vector<SelectedBody> _selected;
    SceneGraphNode* _pickingSelected = nullptr;
    LMstat _lmstat;
    glm::dquat _toSlerp;
    glm::dvec3 _centroid;

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

