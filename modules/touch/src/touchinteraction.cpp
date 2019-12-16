/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/engine/globals.h>

#include <modules/touch/include/touchinteraction.h>
#include <modules/touch/include/directinputsolver.h>
#include <modules/imgui/imguimodule.h>

#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/logging/logmanager.h>
#include <openspace/util/camera.h>
#include <openspace/util/updatestructures.h>

#include <glm/gtx/quaternion.hpp>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/src/basictypes.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#endif

#ifdef OPENSPACE_MODULE_WEBBROWSER_ENABLED
#include <modules/webbrowser/webbrowsermodule.h>
#endif

#include <cmath>
#include <ghoul/fmt.h>
#include <functional>
#include <fstream>

#ifdef WIN32
#pragma warning (push)
#pragma warning (disable : 4310) // cast truncates constant value
#endif // WIN32

#include <glm/ext.hpp>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>

namespace {
    constexpr const char* _loggerCat = "TouchInteraction";

    constexpr openspace::properties::Property::PropertyInfo OriginInfo = {
        "Origin",
        "Origin",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo UnitTestInfo = {
        "UnitTest",
        "Take a unit test saving the LM data into file",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo EventsInfo = {
        "TouchEvents",
        "True if we have a touch event",
        "",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo SetDefaultInfo = {
        "SetDefault",
        "Reset all properties to default",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo MaxTapTimeInfo = {
        "MaxTapTime",
        "Max tap delay (in ms) for double tap",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo DecelatesPerSecondInfo = {
        "DeceleratesPerSecond",
        "Number of times velocity is decelerated per second",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo TouchScreenSizeInfo = {
        "TouchScreenSize",
        "Touch Screen size in inches",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo TapZoomFactorInfo = {
        "TapZoomFactor",
        "Scaling distance travelled on tap",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo DirectManipulationInfo = {
        "DirectManipulationRadius",
        "Radius a planet has to have to activate direct-manipulation",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo RollThresholdInfo = {
        "RollThreshold",
        "Threshold for min angle for roll interpret",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo OrbitSpinningThreshold = {
        "OrbitThreshold",
        "Threshold to activate orbit spinning in direct-manipulation",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo SpinningSensitivityInfo = {
        "SpinningSensitivity",
        "Sensitivity of spinning in direct-manipulation",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ZoomSensitivityExpInfo = {
        "ZoomSensitivityExp",
        "Sensitivity of exponential zooming in relation to distance from focus node",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ZoomSensitivityPropInfo = {
        "ZoomSensitivityProp",
        "Sensitivity of zooming proportional to distance from focus node",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo
    ZoomSensitivityDistanceThresholdInfo = {
        "ZoomSensitivityDistanceThreshold",
        "Threshold of distance to target node for whether or not to use exponential "
        "zooming",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo
    ZoomBoundarySphereMultiplierInfo = {
        "ZoomBoundarySphereMultiplier",
        "Multiplies a node's boundary sphere by this in order to limit zoom & prevent "
        "surface collision",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ConstantTimeDecaySecsInfo = {
        "ConstantTimeDecaySecs",
        "Time duration that a pitch/roll/zoom/pan should take to decay to zero (seconds)",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo InputSensitivityInfo = {
        "InputSensitivity",
        "Threshold for interpreting input as still",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo StationaryCentroidInfo = {
        "CentroidStationary",
        "Threshold for stationary centroid",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo PanModeInfo = {
        "PanMode",
        "Allow panning gesture",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo PanDeltaDistanceInfo = {
        "PanDeltaDistance",
        "Delta distance between fingers allowed for interpreting pan interaction",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo SlerpTimeInfo = {
        "SlerpTime",
        "Time to slerp in seconds to new orientation with new node picking",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo GuiButtonSizeInfo = {
        "GuiButtonSize",
        "GUI button size in pixels",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo FrictionInfo = {
        "Friction",
        "Friction for different interactions (orbit, zoom, roll, pan)",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo PickingRadiusInfo = {
        "PickingRadiusMinimum",
        "Minimum radius for picking in NDC coordinates",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo ZoomOutLimitInfo = {
        "ZoomOutLimit",
        "Zoom Out Limit",
        "The maximum distance you are allowed to navigate away from the anchor. "
        "This should always be larger than the zoom in value if you want to be able "
        "to zoom. Defaults to maximum allowed double."
    };

    constexpr openspace::properties::Property::PropertyInfo ZoomInLimitInfo = {
        "ZoomInLimit",
        "Zoom In Limit",
        "The minimum distance from the anchor that you are allowed to navigate to. "
        "Its purpose is to limit zooming in on a node. If this value is not set it "
        "defaults to the surface of the current anchor. "
    };
} // namespace

using namespace TUIO;

namespace openspace {

TouchInteraction::TouchInteraction()
    : properties::PropertyOwner({ "TouchInteraction" })
    , _origin(OriginInfo)
    , _unitTest(UnitTestInfo, false)
    , _touchActive(EventsInfo, false)
    , _reset(SetDefaultInfo, false)
    , _maxTapTime(MaxTapTimeInfo, 300, 10, 1000)
    , _deceleratesPerSecond(DecelatesPerSecondInfo, 240, 60, 300)
    , _touchScreenSize(TouchScreenSizeInfo, 55.0f, 5.5f, 150.0f)
    , _tapZoomFactor(TapZoomFactorInfo, 0.2f, 0.f, 0.5f)
    , _nodeRadiusThreshold(DirectManipulationInfo, 0.2f, 0.0f, 1.0f)
    , _rollAngleThreshold(RollThresholdInfo, 0.025f, 0.f, 0.05f)
    , _orbitSpeedThreshold(OrbitSpinningThreshold, 0.005f, 0.f, 0.01f)
    , _spinSensitivity(SpinningSensitivityInfo, 0.25f, 0.f, 2.f)
    , _zoomSensitivityExponential(ZoomSensitivityExpInfo, 1.03f, 1.0f, 1.1f)
    , _zoomSensitivityProportionalDist(ZoomSensitivityPropInfo, 11.f, 5.f, 50.f)
    , _zoomSensitivityDistanceThreshold(
        ZoomSensitivityDistanceThresholdInfo,
        0.05f,
        0.01f,
        0.25f
    )
    , _zoomBoundarySphereMultiplier(ZoomBoundarySphereMultiplierInfo, 1.001f, 1.f, 1.01f)
    , _zoomOutLimit(ZoomOutLimitInfo, std::numeric_limits<double>::max(), 1000.0f, std::numeric_limits<double>::max())
    , _zoomInLimit(ZoomInLimitInfo, -1.0f, 0.0f, std::numeric_limits<double>::max())
    , _inputStillThreshold(InputSensitivityInfo, 0.0005f, 0.f, 0.001f)
    // used to void wrongly interpreted roll interactions
    , _centroidStillThreshold(StationaryCentroidInfo, 0.0018f, 0.f, 0.01f)
    , _panEnabled(PanModeInfo, false)
    , _interpretPan(PanDeltaDistanceInfo, 0.015f, 0.f, 0.1f)
    , _slerpTime(SlerpTimeInfo, 3.f, 0.1f, 5.f)
    , _guiButton(
        GuiButtonSizeInfo,
        glm::ivec2(32, 64),
        glm::ivec2(8, 16),
        glm::ivec2(128, 256)
    )
    , _friction(
        FrictionInfo,
        glm::vec4(0.025f, 0.025f, 0.02f, 0.02f),
        glm::vec4(0.f),
        glm::vec4(0.2f)
    )
    , _pickingRadiusMinimum(
        { "Picking Radius", "Minimum radius for picking in NDC coordinates", "" },
        0.1f,
        0.f,
        1.f
    )
    , _ignoreGui( // @TODO Missing documentation
        { "Ignore GUI", "Disable GUI touch interaction", "" },
        false
    )
    , _vel{ glm::dvec2(0.0), 0.0, 0.0, glm::dvec2(0.0) }
    , _sensitivity{ glm::dvec2(0.08, 0.045), 12.0 /*4.0*/, 2.75, glm::dvec2(0.08, 0.045) }
    , _constTimeDecay_secs(ConstantTimeDecaySecsInfo, 1.75f, 0.1f, 4.0f)
    // calculated with two vectors with known diff in length, then
    // projDiffLength/diffLength.
    , _projectionScaleFactor(1.000004)
    , _currentRadius(1.0)
    , _slerpdT(1000)
    , _timeSlack(0.0)
    , _numOfTests(0)
    , _directTouchMode(false)
    , _wasPrevModeDirectTouch(false)
    , _tap(false)
    , _doubleTap(false)
    , _zoomOutTap(false)
    , _lmSuccess(true)
    , _guiON(false)
#ifdef TOUCH_DEBUG_PROPERTIES
    , _debugProperties()
#endif
    , _centroid(glm::dvec3(0.0))
{
    addProperty(_touchActive);
    addProperty(_unitTest);
    addProperty(_reset);
    addProperty(_maxTapTime);
    addProperty(_deceleratesPerSecond);
    addProperty(_touchScreenSize);
    addProperty(_tapZoomFactor);
    addProperty(_nodeRadiusThreshold);
    addProperty(_rollAngleThreshold);
    addProperty(_orbitSpeedThreshold);
    addProperty(_spinSensitivity);
    addProperty(_zoomSensitivityExponential);
    addProperty(_zoomSensitivityProportionalDist);
    addProperty(_zoomSensitivityDistanceThreshold);
    addProperty(_zoomBoundarySphereMultiplier);
    addProperty(_zoomInLimit);
    addProperty(_zoomOutLimit);
    addProperty(_constTimeDecay_secs);
    addProperty(_inputStillThreshold);
    addProperty(_centroidStillThreshold);
    addProperty(_panEnabled);
    addProperty(_interpretPan);
    addProperty(_slerpTime);
    addProperty(_guiButton);
    addProperty(_friction);
    addProperty(_pickingRadiusMinimum);
    addProperty(_ignoreGui);

#ifdef TOUCH_DEBUG_PROPERTIES
    addPropertySubOwner(_debugProperties);
#endif

    _origin.onChange([this]() {
        SceneGraphNode* node = sceneGraphNode(_origin.value());
        if (node) {
            setFocusNode(node);
        }
        else {
            LWARNING(fmt::format(
                "Could not find a node in scenegraph called '{}'", _origin.value()
            ));
        }
    });

    _time.initSession();
}
// Called each frame if there is any input
void TouchInteraction::updateStateFromInput(const std::vector<TuioCursor>& list,
                                            std::vector<Point>& lastProcessed)
{
#ifdef TOUCH_DEBUG_PROPERTIES
    _debugProperties.nFingers = list.size();
#endif
    if (_tap) { // check for doubletap
        if (_time.getSessionTime().getTotalMilliseconds() < _maxTapTime) {
            _doubleTap = true;
            _tap = false;
        }
        _time.initSession();
    }

    bool hasWebContent = webContent(list);

    //Code for lower-right corner double-tap to zoom-out
    glm::ivec2 res = global::windowDelegate.currentWindowSize();
    glm::dvec2 pos = glm::vec2(
        list.at(0).getScreenX(res.x),
        list.at(0).getScreenY(res.y)
    );
    const float bottomCornerSizeForZoomTap_fraction = 0.08f;
    int zoomTapThresholdX = static_cast<int>(
        res.x * (1.0f - bottomCornerSizeForZoomTap_fraction)
    );
    int zoomTapThresholdY = static_cast<int>(
        res.y * (1.0f - bottomCornerSizeForZoomTap_fraction)
    );

    bool isTapInLowerRightCorner =
        (std::abs(pos.x) > zoomTapThresholdX && std::abs(pos.y) > zoomTapThresholdY);

    if (_doubleTap && isTapInLowerRightCorner) {
        _zoomOutTap = true;
        _tap = false;
        _doubleTap = false;
    }

    if (!guiMode(list) && !hasWebContent) {
        bool isThisFrameTransitionBetweenTouchModes
            = (_wasPrevModeDirectTouch != _directTouchMode);
        if (isThisFrameTransitionBetweenTouchModes) {
            _vel.orbit = glm::dvec2(0.0, 0.0);
            _vel.zoom = 0.0;
            _vel.roll = 0.0;
            _vel.pan = glm::dvec2(0.0, 0.0);
            resetAfterInput();
            /*if( _directTouchMode )
                LINFO("Touch -> Direct-touch");
            else
                LINFO("Direct-touch -> Touch");*/
        }

        if (_directTouchMode && _selected.size() > 0 && list.size() == _selected.size()) {
#ifdef TOUCH_DEBUG_PROPERTIES
            _debugProperties.interactionMode = "Direct";
#endif
            directControl(list);
        }
        if (_lmSuccess) {
            findSelectedNode(list);
        }

        if (!_directTouchMode) {
#ifdef TOUCH_DEBUG_PROPERTIES
            _debugProperties.interactionMode = "Velocities";
#endif
            computeVelocities(list, lastProcessed);
        }

        _wasPrevModeDirectTouch = _directTouchMode;
        // evaluates if current frame is in directTouchMode (will be used next frame)
        _directTouchMode =
            (_currentRadius > _nodeRadiusThreshold && _selected.size() == list.size());
    }
}

bool TouchInteraction::webContent(const std::vector<TuioCursor>& list) {
#ifdef OPENSPACE_MODULE_WEBBROWSER_ENABLED
    glm::ivec2 res = global::windowDelegate.currentWindowSize();
    glm::dvec2 pos = glm::vec2(
        list.at(0).getScreenX(res.x),
        list.at(0).getScreenY(res.y)
    );

    WebBrowserModule& module = *(global::moduleEngine.module<WebBrowserModule>());
    return module.eventHandler().hasContentCallback(pos.x, pos.y);
#else
    return false;
#endif
}

// Activates/Deactivates gui input mode (if active it voids all other interactions)
bool TouchInteraction::guiMode(const std::vector<TuioCursor>& list) {
    if (_ignoreGui) {
        return false;
    }
    glm::ivec2 res = global::windowDelegate.currentWindowSize();
    glm::dvec2 pos = glm::vec2(
        list.at(0).getScreenX(res.x),
        list.at(0).getScreenY(res.y)
    );

    ImGUIModule& module = *(global::moduleEngine.module<ImGUIModule>());
    _guiON = module.gui.isEnabled();

    if (_tap && list.size() == 1 &&
        std::abs(pos.x) < _guiButton.value().x && std::abs(pos.y) < _guiButton.value().y)
    {
        // pressed invisible button
        _guiON = !_guiON;
        module.gui.setEnabled(_guiON);

        LINFO(fmt::format(
            "GUI mode is {}. Inside box by: ({}%, {}%)",
            _guiON ? "activated" : "deactivated",
            static_cast<int>(100 * (pos.x / _guiButton.value().x)),
            static_cast<int>(100 * (pos.y / _guiButton.value().y))
        ));
    }
    else if (_guiON) {
        module.touchInput = { _guiON, pos, 1 }; // emulate touch input as a mouse
    }

    return _guiON;
}

// Sets _vel to update _camera according to direct-manipulation (L2 error)
void TouchInteraction::directControl(const std::vector<TuioCursor>& list) {
    // Reset old velocities upon new interaction
    _vel.orbit = glm::dvec2(0.0, 0.0);
    _vel.zoom = 0.0;
    _vel.roll = 0.0;
    _vel.pan = glm::dvec2(0.0, 0.0);
#ifdef TOUCH_DEBUG_PROPERTIES
    LINFO("DirectControl");
#endif

    // finds best transform values for the new camera state and stores them in par
    std::vector<double> par(6, 0.0);
    par.at(0) = _lastVel.orbit.x; // use _lastVel for orbit
    par.at(1) = _lastVel.orbit.y;
    _lmSuccess = _solver.solve(list, _selected, &par, *_camera);
    int nDof = _solver.getNDof();

    if (_lmSuccess && !_unitTest) {
         // if good values were found set new camera state
        _vel.orbit = glm::dvec2(par.at(0), par.at(1));
        if (nDof > 2) {
            _vel.zoom = par.at(2);
            _vel.roll = par.at(3);
            if (_panEnabled && nDof > 4) {
                _vel.roll = 0.0;
                _vel.pan = glm::dvec2(par.at(4), par.at(5));
            }
        }
        step(1.0);

        // Reset velocities after setting new camera state
        _lastVel = _vel;
        _vel.orbit = glm::dvec2(0.0, 0.0);
        _vel.zoom = 0.0;
        _vel.roll = 0.0;
        _vel.pan = glm::dvec2(0.0, 0.0);
    }
    else {
        // prevents touch to infinitely be active (due to windows bridge case where event
        // doesnt get consumed sometimes when LMA fails to converge)
        global::moduleEngine.module<ImGUIModule>()->touchInput = {
            true,
            glm::dvec2(0.0, 0.0),
            1
        };
        resetAfterInput();
    }
}

// Traces the touch input into the scene and finds the surface coordinates of touched
// planets (if occuring)
void TouchInteraction::findSelectedNode(const std::vector<TuioCursor>& list) {
    //trim list to only contain visible nodes that make sense
    std::string selectables[30] = {
        "Sun", "Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus",
        "Neptune", "Pluto", "Moon", "Titan", "Rhea", "Mimas", "Iapetus", "Enceladus",
        "Dione", "Io", "Ganymede", "Europa", "Callisto", "NewHorizons", "Styx", "Nix",
        "Kerberos", "Hydra", "Charon", "Tethys", "OsirisRex", "Bennu"
    };
    std::vector<SceneGraphNode*> selectableNodes;
    for (SceneGraphNode* node : global::renderEngine.scene()->allSceneGraphNodes()) {
        for (const std::string& name : selectables) {
            if (node->identifier() == name) {
                selectableNodes.push_back(node);
            }
        }
    }

    glm::dquat camToWorldSpace = _camera->rotationQuaternion();
    glm::dvec3 camPos = _camera->positionVec3();
    std::vector<DirectInputSolver::SelectedBody> newSelected;
    
    //node & distance
    std::tuple<SceneGraphNode*, double> currentlyPicked = {
        nullptr,
        std::numeric_limits<double>::max()
    };
    

    for (const TuioCursor& c : list) {
        double xCo = 2 * (c.getX() - 0.5);
        double yCo = -2 * (c.getY() - 0.5); // normalized -1 to 1 coordinates on screen
        glm::dvec3 cursorInWorldSpace = camToWorldSpace *
            glm::dvec3(glm::inverse(_camera->projectionMatrix()) *
            glm::dvec4(xCo, yCo, -1.0, 1.0));
        glm::dvec3 raytrace = glm::normalize(cursorInWorldSpace);

        long id = c.getSessionID();

        for (SceneGraphNode* node : selectableNodes) {
            double boundingSphereSquared = static_cast<double>(node->boundingSphere()) *
                                           static_cast<double>(node->boundingSphere());
            glm::dvec3 camToSelectable = node->worldPosition() - camPos;
            double intersectionDist = 0.0;
            bool intersected = glm::intersectRaySphere(
                camPos,
                raytrace,
                node->worldPosition(),
                boundingSphereSquared,
                intersectionDist
            );
            if (intersected) {
                glm::dvec3 intersectionPos = camPos + raytrace * intersectionDist;
                glm::dvec3 pointInModelView = glm::inverse(node->worldRotationMatrix()) *
                                              (intersectionPos - node->worldPosition());

                // Add id, node and surface coordinates to the selected list
                auto oldNode = std::find_if(
                    newSelected.begin(),
                    newSelected.end(),
                    [id](const DirectInputSolver::SelectedBody& s) { return s.id == id; }
                );
                if (oldNode != newSelected.end()) {
                    double oldNodeDist = glm::length(
                        oldNode->node->worldPosition() - camPos
                    );
                    if (glm::length(camToSelectable) < oldNodeDist) {
                         // new node is closer, remove added node and add the new one
                         // instead
                        newSelected.pop_back();
                        newSelected.push_back({ id, node, pointInModelView });
                    }
                }
                else {
                    newSelected.push_back({ id, node, pointInModelView });
                }
            }

            // Compute locations in view space to perform the picking
            glm::dvec4 clip = glm::dmat4(_camera->projectionMatrix()) *
                              _camera->combinedViewMatrix() *
                              glm::vec4(node->worldPosition(), 1.0);
            glm::dvec2 ndc = clip / clip.w;

            // If the object is not in the screen, we dont want to consider it at all
            if (ndc.x >= -1.0 && ndc.x <= 1.0 && ndc.y >= -1.0 && ndc.y <= 1.0) {
                glm::dvec2 cursor = { xCo, yCo };

                double ndcDist = glm::length(ndc - cursor);
                // We either want to select the object if it's bounding sphere as been
                // touched (checked by the first part of this loop above) or if the touch
                // point is within a minimum distance of the center

                // If the user touched the planet directly, this is definitely the one
                // they are interested in  =>  minimum distance
                if (intersected) {
#ifdef TOUCH_DEBUG_NODE_PICK_MESSAGES
                    LINFOC(
                        node->identifier(),
                        "Picking candidate based on direct touch"
                    );
#endif //#ifdef TOUCH_DEBUG_NODE_PICK_MESSAGES
                    currentlyPicked = {
                        node,
                        -std::numeric_limits<double>::max()
                    };
                }
                else if (ndcDist <= _pickingRadiusMinimum) {
                    // The node was considered due to minimum picking distance radius
#ifdef TOUCH_DEBUG_NODE_PICK_MESSAGES
                    LINFOC(
                        node->identifier(),
                        "Picking candidate based on proximity"
                    );
#endif //#ifdef TOUCH_DEBUG_NODE_PICK_MESSAGES
                    double dist = length(camToSelectable);
                    if (dist < std::get<1>(currentlyPicked)) {
                        currentlyPicked = {
                            node,
                            dist
                        };
                    }
                }
            }
        }
    }

    // If an item has been picked, it's in the first position of the vector now
    if (SceneGraphNode* node = std::get<0>(currentlyPicked)) {
        _pickingSelected = node;
#ifdef TOUCH_DEBUG_NODE_PICK_MESSAGES
        LINFOC("Picking", "Picked node: " + _pickingSelected->identifier());
#endif //#ifdef TOUCH_DEBUG_NODE_PICK_MESSAGES
    }

    _selected = std::move(newSelected);
}

// Interprets the input gesture to a specific interaction
int TouchInteraction::interpretInteraction(const std::vector<TuioCursor>& list,
                                           const std::vector<Point>& lastProcessed)
{
    glm::dvec3 lastCentroid = _centroid;
    _centroid.x = std::accumulate(
        list.begin(),
        list.end(),
        0.0,
        [](double x, const TuioCursor& c) { return x + c.getX(); }
    ) / list.size();
    _centroid.y = std::accumulate(
        list.begin(),
        list.end(),
        0.0,
        [](double y, const TuioCursor& c) { return y + c.getY(); }
    ) / list.size();

    // see if the distance between fingers changed - used in pan interpretation
    double dist = 0;
    double lastDist = 0;
    TuioCursor cursor = list.at(0);
    for (const TuioCursor& c : list) {
        dist += glm::length(
            glm::dvec2(c.getX(), c.getY()) - glm::dvec2(cursor.getX(), cursor.getY())
        );
        cursor = c;
    }
    TuioPoint point = lastProcessed.at(0).second;
    for (const Point& p : lastProcessed) {
        lastDist += glm::length(glm::dvec2(p.second.getX(), p.second.getY()) -
                    glm::dvec2(point.getX(), point.getY()));
        point = p.second;
    }
    // find the slowest moving finger - used in roll interpretation
    double minDiff = 1000;
    long id = 0;
    for (const TuioCursor& c : list) {
        auto it = std::find_if(
            lastProcessed.begin(),
            lastProcessed.end(),
            [&c](const Point& p) {
                return p.first == c.getSessionID();
        });

        if (it == lastProcessed.end()) {
            continue;
        }

        TuioPoint itPoint = it->second;
        double diff = c.getX() - itPoint.getX() + c.getY() - itPoint.getY();

        if (!c.isMoving()) {
            diff = minDiff = 0.0;
            id = c.getSessionID();
        }
        else if (std::abs(diff) < std::abs(minDiff)) {
            minDiff = diff;
            id = c.getSessionID();
        }
    }
    // find if all fingers angles are high - used in roll interpretation
    double rollOn = std::accumulate(
        list.begin(),
        list.end(),
        0.0,
        [&](double diff, const TuioCursor& c) {
            TuioPoint point = std::find_if(
                lastProcessed.begin(),
                lastProcessed.end(),
                [&c](const Point& p) { return p.first == c.getSessionID(); }
            )->second;
            double res = 0.0;
            float lastAngle = point.getAngle(
                static_cast<float>(_centroid.x),
                static_cast<float>(_centroid.y)
            );
            float currentAngle = c.getAngle(
                static_cast<float>(_centroid.x),
                static_cast<float>(_centroid.y)
            );
            if (lastAngle > currentAngle + 1.5 * M_PI) {
                res = currentAngle + (2 * M_PI - lastAngle);
            }
            else if (currentAngle > lastAngle + 1.5 * M_PI) {
                res = (2 * M_PI - currentAngle) + lastAngle;
            }
            else {
                res = currentAngle - lastAngle;
            }
            if (std::abs(res) < _rollAngleThreshold) {
                return 1000.0;
            }
            else {
                return (diff + res);
            }
        }
    );

    double normalizedCentroidDistance = glm::distance(
        _centroid,
        lastCentroid
    ) / list.size();
#ifdef TOUCH_DEBUG_PROPERTIES
    _debugProperties.normalizedCentroidDistance = normalizedCentroidDistance;
    _debugProperties.rollOn = rollOn;
    _debugProperties.minDiff = minDiff;
#endif

    if (_zoomOutTap) {
        return ZOOM_OUT;
    }
    else if (_doubleTap) {
        return PICK;
    }
    else if (list.size() == 1) {
        return ROT;
    }
    else {
        float avgDistance = static_cast<float>(
            std::abs(dist - lastDist) / list.at(0).getMotionSpeed()
        );
        // if average distance between 3 fingers are constant we have panning
        if (_panEnabled && (avgDistance < _interpretPan && list.size() == 3)) {
            return PAN;
        }

        // we have roll if one finger is still, or the total roll angles around the
        // centroid is over _rollAngleThreshold (_centroidStillThreshold is used to void
        // misinterpretations)
        else if (std::abs(minDiff) < _inputStillThreshold ||
                (std::abs(rollOn) < 100.0 &&
                 normalizedCentroidDistance < _centroidStillThreshold))
        {
            return ROLL;
        }
        else {
            return PINCH;
        }
    }
}

// Calculate how much interpreted interaction (_vel) should change the camera state
void TouchInteraction::computeVelocities(const std::vector<TuioCursor>& list,
                                         const std::vector<Point>& lastProcessed)
{
    const TuioCursor& cursor = list.at(0);
    const int action = interpretInteraction(list, lastProcessed);
    const SceneGraphNode* anchor =
        global::navigationHandler.orbitalNavigator().anchorNode();
    if (!anchor) {
        return;
    }

#ifdef TOUCH_DEBUG_PROPERTIES
    const std::map<int, std::string> interactionNames = {
        { ROT, "Rotation" },
        { PINCH, "Pinch" },
        { PAN, "Pan" },
        { ROLL, "Roll" },
        { PICK, "Pick" }
    };
    _debugProperties.interpretedInteraction = interactionNames.at(action);

    if (pinchConsecCt > 0 && action != PINCH) {
        if (pinchConsecCt > 3) {
            LDEBUG(fmt::format(
                "PINCH gesture ended with {} drag distance and {} counts",
                pinchConsecZoomFactor,
                pinchConsecCt
            ));
        }
        pinchConsecCt = 0;
        pinchConsecZoomFactor = 0.0;
    }
#endif

    switch (action) {
        case ROT: { // add rotation velocity
            _vel.orbit += glm::dvec2(cursor.getXSpeed() *
                          _sensitivity.orbit.x, cursor.getYSpeed() *
                          _sensitivity.orbit.y);
            double orbitVelocityAvg = glm::distance(_vel.orbit.x, _vel.orbit.y);
            _constTimeDecayCoeff.orbit
                = computeConstTimeDecayCoefficient(orbitVelocityAvg);
            break;
        }
        case PINCH: {
             // add zooming velocity - dependant on distance difference between contact
             // points this/last frame
            double distance = std::accumulate(
                list.begin(),
                list.end(),
                0.0,
                [&](double d, const TuioCursor& c) {
                    return d + c.getDistance(
                        static_cast<float>(_centroid.x),
                        static_cast<float>(_centroid.y)
                    );
                }
            ) / list.size();
            double lastDistance = std::accumulate(
                lastProcessed.begin(),
                lastProcessed.end(),
                0.0f,
                [&](float d, const Point& p) {
                    return d + p.second.getDistance(
                        static_cast<float>(_centroid.x),
                        static_cast<float>(_centroid.y)
                    );
                }
            ) / lastProcessed.size();

            glm::dvec3 camPos = _camera->positionVec3();
            glm::dvec3 centerPos = anchor->worldPosition();
            glm::dvec3 currDistanceToFocusNode = camPos - centerPos;

            double distanceFromFocusSurface =
                length(currDistanceToFocusNode) - anchor->boundingSphere();
            double zoomFactor = (distance - lastDistance);
#ifdef TOUCH_DEBUG_PROPERTIES
            pinchConsecCt++;
            pinchConsecZoomFactor += zoomFactor;
#endif

            _constTimeDecayCoeff.zoom = computeConstTimeDecayCoefficient(_vel.zoom);
            if (distanceFromFocusSurface > 0.1) {
                double ratioOfDistanceToNodeVsSurface =
                    length(currDistanceToFocusNode) / distanceFromFocusSurface;
                if (ratioOfDistanceToNodeVsSurface > _zoomSensitivityDistanceThreshold) {
                    zoomFactor *= pow(
                std::abs(distanceFromFocusSurface),
                        static_cast<float>(_zoomSensitivityExponential)
                    );
                }
            }
            else {
                zoomFactor = 1.0;
            }
            _vel.zoom = zoomFactor * _zoomSensitivityProportionalDist *
                         std::max(_touchScreenSize.value() * 0.1, 1.0);
            break;
        }
        case ROLL: {
            // add global roll rotation velocity
            double rollFactor = std::accumulate(
                list.begin(),
                list.end(),
                0.0,
                [&](double diff, const TuioCursor& c) {
                    TuioPoint point = std::find_if(
                        lastProcessed.begin(),
                        lastProcessed.end(),
                        [&c](const Point& p) { return p.first == c.getSessionID(); }
                    )->second;
                    double res = diff;
                    double lastAngle = point.getAngle(
                        static_cast<float>(_centroid.x),
                        static_cast<float>(_centroid.y)
                    );
                    double currentAngle = c.getAngle(
                        static_cast<float>(_centroid.x),
                        static_cast<float>(_centroid.y)
                    );
                    // if's used to set angles 359 + 1 = 0 and 0 - 1 = 359
                    if (lastAngle > currentAngle + 1.5 * M_PI) {
                        res += currentAngle + (2 * M_PI - lastAngle);
                    }
                    else if (currentAngle > lastAngle + 1.5 * M_PI) {
                        res += (2 * M_PI - currentAngle) + lastAngle;
                    }
                    else {
                        res += currentAngle - lastAngle;
                    }
                    return res;
                }
            ) / list.size();

            _vel.roll += -rollFactor * _sensitivity.roll;
            _constTimeDecayCoeff.roll = computeConstTimeDecayCoefficient(_vel.roll);
            break;
        }
        case PAN: {
             // add local rotation velocity
            _vel.pan += glm::dvec2(cursor.getXSpeed() *
                        _sensitivity.pan.x, cursor.getYSpeed() * _sensitivity.pan.y);
            double panVelocityAvg = glm::distance(_vel.pan.x, _vel.pan.y);
            _constTimeDecayCoeff.pan = computeConstTimeDecayCoefficient(panVelocityAvg);
            break;
        }
        case PICK: {
             // pick something in the scene as focus node
            if (_pickingSelected) {
                setFocusNode(_pickingSelected);

                // rotate camera to look at new focus, using slerp quat
                glm::dvec3 camToFocus = _pickingSelected->worldPosition() -
                                        _camera->positionVec3();
                glm::dvec3 forward = glm::normalize(_camera->viewDirectionWorldSpace());
                double angle = glm::angle(forward, camToFocus);
                glm::dvec3 axis = glm::normalize(glm::cross(forward, camToFocus));
                _toSlerp.x = axis.x * sin(angle / 2.0);
                _toSlerp.y = axis.y * sin(angle / 2.0);
                _toSlerp.z = axis.z * sin(angle / 2.0);
                _toSlerp.w = cos(angle / 2.0);
                _slerpdT = 0.0;
            }
            else {
                 // zooms in to current if PICK interpret happened but only space was
                 // selected
                 _vel.zoom = computeTapZoomDistance(0.3);
                 _constTimeDecayCoeff.zoom = computeConstTimeDecayCoefficient(_vel.zoom);
            }
            break;
        }
        case ZOOM_OUT: {
            // zooms out from current if triple tap occurred
            _vel.zoom = computeTapZoomDistance(-1.0);
            _constTimeDecayCoeff.zoom = computeConstTimeDecayCoefficient(_vel.zoom);
        }
    }
}

double TouchInteraction::computeConstTimeDecayCoefficient(double velocity) {
    const double postDecayVelocityTarget = 1e-6;
    double stepsToDecay = _constTimeDecay_secs / _frameTimeAvg.averageFrameTime();

    if (stepsToDecay > 0.0 && std::abs(velocity) > postDecayVelocityTarget) {
        return std::pow(postDecayVelocityTarget / std::abs(velocity), 1.0 / stepsToDecay);
    }
    else {
        return 1.0;
    }
}

double TouchInteraction::computeTapZoomDistance(double zoomGain) {
    const SceneGraphNode* anchor =
        global::navigationHandler.orbitalNavigator().anchorNode();
    if (!anchor) {
        return 0.0;
    }

    double dist = glm::distance(
        _camera->positionVec3(),
        global::navigationHandler.orbitalNavigator().anchorNode()->worldPosition()
    );

    dist -= anchor->boundingSphere();

    double newVelocity = dist * _tapZoomFactor;
    newVelocity *= std::max(_touchScreenSize.value() * 0.1, 1.0);
    newVelocity *= _zoomSensitivityProportionalDist * zoomGain;

    return newVelocity;
}

// Main update call, calculates the new orientation and position for the camera depending
// on _vel and dt. Called every frame
void TouchInteraction::step(double dt) {
    using namespace glm;

    const SceneGraphNode* anchor =
        global::navigationHandler.orbitalNavigator().anchorNode();

     // since functions cant be called directly (TouchInteraction not a subclass of
     // InteractionMode)
    setFocusNode(global::navigationHandler.orbitalNavigator().anchorNode());
    if (anchor && _camera) {
        // Create variables from current state
        dvec3 camPos = _camera->positionVec3();
        dvec3 centerPos = anchor->worldPosition();

        dvec3 directionToCenter = normalize(centerPos - camPos);
        dvec3 centerToCamera = camPos - centerPos;
        dvec3 lookUp = _camera->lookUpVectorWorldSpace();
        dvec3 camDirection = _camera->viewDirectionWorldSpace();

        // Make a representation of the rotation quaternion with local and global
        // rotations
        // To avoid problem with lookup in up direction
        dmat4 lookAtMat = lookAt(
            dvec3(0, 0, 0),
            directionToCenter,
            normalize(camDirection + lookUp)
        );
        dquat globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
        dquat localCamRot = inverse(globalCamRot) * _camera->rotationQuaternion();

        double boundingSphere = anchor->boundingSphere();
        double distance = std::max(length(centerToCamera) - boundingSphere, 0.0);
        _currentRadius = boundingSphere /
                         std::max(distance * _projectionScaleFactor, 1.0);

        {
            // Roll
            dquat camRollRot = angleAxis(_vel.roll * dt, dvec3(0.0, 0.0, 1.0));
            localCamRot = localCamRot * camRollRot;
        }
        {
            // Panning (local rotation)
            dvec3 eulerAngles(_vel.pan.y * dt, _vel.pan.x * dt, 0);
            dquat rotationDiff = dquat(eulerAngles);
            localCamRot = localCamRot * rotationDiff;

            // if we have chosen a new focus node
            if (_slerpdT < _slerpTime) {
                _slerpdT += 0.1*dt;
                localCamRot = slerp(localCamRot, _toSlerp, _slerpdT / _slerpTime);
            }
        }
        {
            // Orbit (global rotation)
            dvec3 eulerAngles(_vel.orbit.y*dt, _vel.orbit.x*dt, 0);
            dquat rotationDiffCamSpace = dquat(eulerAngles);

            dquat rotationDiffWorldSpace = globalCamRot * rotationDiffCamSpace *
                                           inverse(globalCamRot);
            dvec3 rotationDiffVec3 = centerToCamera * rotationDiffWorldSpace -
                                     centerToCamera;
            camPos += rotationDiffVec3;

            dvec3 centerToCam = camPos - centerPos;
            directionToCenter = normalize(-centerToCam);
            dvec3 lookUpWhenFacingCenter = globalCamRot *
                                           dvec3(_camera->lookUpVectorCameraSpace());
            dmat4 lookAtMatrix = lookAt(
                dvec3(0, 0, 0),
                directionToCenter,
                lookUpWhenFacingCenter);
            globalCamRot = normalize(quat_cast(inverse(lookAtMatrix)));
        }
        { // Zooming

            // This is a rough estimate of the node surface
            double zoomInBounds = boundingSphere * _zoomBoundarySphereMultiplier;

            // If nobody has set another zoom in limit, use the default zoom in bounds
            if (_zoomInLimit.value() < 0) {
                _zoomInLimit.setValue(zoomInBounds);
            }
            else if (_zoomInLimit.value() < zoomInBounds) {
                // If zoom in limit is less than the estimated node radius we need to 
                // make sure we do not get too close to possible height maps
                SurfacePositionHandle posHandle = anchor->calculateSurfacePositionHandle(
                    camPos
                );
                glm::dvec3 centerToActualSurfaceModelSpace =
                    posHandle.centerToReferenceSurface +
                    posHandle.referenceSurfaceOutDirection * posHandle.heightToSurface;
                glm::dvec3 centerToActualSurface = glm::dmat3(anchor->modelTransform()) *
                    centerToActualSurfaceModelSpace;
                double nodeRadius = length(centerToActualSurface);

                // Because of heightmaps we should make sure we do not go through the surface
                if (_zoomInLimit.value() < nodeRadius) {
#ifdef TOUCH_DEBUG_PROPERTIES
                    LINFO(fmt::format("{}: Zoom In limit should be larger than anchor "
                        "center to surface, setting it to {}", _loggerCat, zoomInBounds));
#endif
                    _zoomInLimit.setValue(zoomInBounds);
                }     
            }
           
            // Make sure zoom in limit is not larger than zoom out limit
            if (_zoomInLimit.value() > _zoomOutLimit.value()) {
               LWARNING(fmt::format(
                   "{}: Zoom In Limit should be smaller than Zoom Out Limit", 
                    _loggerCat, _zoomOutLimit.value()
               ));
            }

            //Apply the velocity to update camera position
            glm::dvec3 zoomDistanceIncrement = directionToCenter * _vel.zoom * dt;
            double newPosDistance = length(centerToCamera + zoomDistanceIncrement);
            double currentPosDistance = length(centerToCamera);

            // Possible with other navigations performed outside touch interaction
            bool currentPosViolatingZoomOutLimit =
                (currentPosDistance >= _zoomOutLimit.value());
            bool willNewPositionViolateZoomOutLimit =
                (newPosDistance >= _zoomOutLimit.value());
            bool willNewPositionViolateZoomInLimit =
                (newPosDistance < _zoomInLimit.value());

            if (!willNewPositionViolateZoomInLimit && !willNewPositionViolateZoomOutLimit){
                camPos += zoomDistanceIncrement;
            }
            else if (currentPosViolatingZoomOutLimit) {
#ifdef TOUCH_DEBUG_PROPERTIES
                LINFO(fmt::format(
                    "{}: You are outside zoom out {} limit, only zoom in allowed",
                    _loggerCat, _zoomOutLimit.value());
#endif
                // Only allow zooming in if you are outside the zoom out limit
                if (newPosDistance < currentPosDistance)
                {
                    camPos += zoomDistanceIncrement;
                }
            }
            else {
#ifdef TOUCH_DEBUG_PROPERTIES
                LINFO("Zero the zoom velocity close to surface.");
#endif
                _vel.zoom = 0.0;
            }
        }

        decelerate(dt);
        // Update the camera state
        _camera->setPositionVec3(camPos);
        _camera->setRotation(globalCamRot * localCamRot);

#ifdef TOUCH_DEBUG_PROPERTIES
        //Show velocity status every N frames
        if (++stepVelUpdate >= 60) {
            stepVelUpdate = 0;
            LINFO(fmt::format(
                "DistToFocusNode {} stepZoomVelUpdate {}",
                length(centerToCamera),
                _vel.zoom
            ));
        }
#endif

        _tap = false;
        _doubleTap = false;
        _zoomOutTap = false;
        if (_reset) {
            resetToDefault();
        }
    }
}

void TouchInteraction::unitTest() {
    if (_unitTest) {
        _solver.setLevMarqVerbosity(true);

        // set _selected pos and new pos (on screen)
        std::vector<TuioCursor> lastFrame = {
            { TuioCursor(0, 10, 0.45f, 0.4f) }, // session id, cursor id, x, y
            { TuioCursor(1, 11, 0.55f, 0.6f) }
        };
        std::vector<TuioCursor> currFrame = {
            { TuioCursor(0, 10, 0.2f, 0.6f) }, // (-0.6,-0.2)
            { TuioCursor(1, 11, 0.8f, 0.4f) } // (0.6, 0.2)
        };

        // call update
        findSelectedNode(lastFrame);
        directControl(currFrame);

        // save lmstats.data into a file and clear it
        char buffer[32];
        snprintf(buffer, sizeof(char) * 32, "lmdata%i.csv", _numOfTests);
        _numOfTests++;
        std::ofstream file(buffer);
        file << _solver.getLevMarqStat().data;

        // clear everything
        _selected.clear();
        _pickingSelected = nullptr;
        _vel.orbit = glm::dvec2(0.0, 0.0);
        _vel.zoom = 0.0;
        _vel.roll = 0.0;
        _vel.pan = glm::dvec2(0.0, 0.0);
        _lastVel = _vel;
        _unitTest = false;

        _solver.setLevMarqVerbosity(false);
        // could be the camera copy in func
    }
}

// Decelerate velocities, called a set number of times per second to dereference it from
// frame time
// Example:
// Assume: frequency = 0.01, dt = 0.05 (200 fps), _timeSlack = 0.0001
// times = floor((0.05 + 0.0001) / 0.01) = 5
// _timeSlack = 0.0501 % 0.01 = 0.01
void TouchInteraction::decelerate(double dt) {
    _frameTimeAvg.updateWithNewFrame(dt);
    double expectedFrameTime = _frameTimeAvg.averageFrameTime();

    // Number of times velocities should decelerate, depending on chosen frequency and
    // time slack over from last frame
    int times = static_cast<int>((dt + _timeSlack) / expectedFrameTime);
    // Save the new time slack for the next frame
    _timeSlack = fmod((dt + _timeSlack), expectedFrameTime) * expectedFrameTime;

    //Ensure the number of times to apply the decay coefficient is valid
    times = std::min(times, 1);

    _vel.orbit *= computeDecayCoeffFromFrametime(_constTimeDecayCoeff.orbit, times);
    _vel.roll  *= computeDecayCoeffFromFrametime(_constTimeDecayCoeff.roll,  times);
    _vel.pan   *= computeDecayCoeffFromFrametime(_constTimeDecayCoeff.pan,   times);
    _vel.zoom  *= computeDecayCoeffFromFrametime(_constTimeDecayCoeff.zoom,  times);
}

double TouchInteraction::computeDecayCoeffFromFrametime(double coeff, int times) {
    if (coeff > 0.00001) {
        return std::pow(coeff, times);
    }
    else {
        return 0.0;
    }
}

// Called if all fingers are off the screen
void TouchInteraction::resetAfterInput() {
#ifdef TOUCH_DEBUG_PROPERTIES
    _debugProperties.nFingers = 0;
    _debugProperties.interactionMode = "None";
#endif
    if (_directTouchMode && !_selected.empty() && _lmSuccess) {
        double spinDelta = _spinSensitivity / global::windowDelegate.averageDeltaTime();
        if (glm::length(_lastVel.orbit) > _orbitSpeedThreshold) {
             // allow node to start "spinning" after direct-manipulation finger is let go
            _vel.orbit = _lastVel.orbit * spinDelta;
        }
    }
    // Reset emulated mouse values
    ImGUIModule& module = *(global::moduleEngine.module<ImGUIModule>());
    if (_guiON) {
        bool activeLastFrame = module.touchInput.action;
        module.touchInput.active = false;
        if (activeLastFrame) {
            module.touchInput.active = true;
            module.touchInput.action = 0;
        }
    }
    else {
        module.touchInput.active = false;
        module.touchInput.action = 0;
    }

    _lmSuccess = true;
    // Ensure that _guiON is consistent with properties in OnScreenGUI and
    _guiON = module.gui.isEnabled();

    // Reset variables
    _lastVel.orbit = glm::dvec2(0.0, 0.0);
    _lastVel.zoom = 0.0;
    _lastVel.roll = 0.0;
    _lastVel.pan = glm::dvec2(0.0, 0.0);
    _selected.clear();
    _pickingSelected = nullptr;
}

// Reset all property values to default
void TouchInteraction::resetToDefault() {
    _unitTest.set(false);
    _reset.set(false);
    _maxTapTime.set(300);
    _deceleratesPerSecond.set(240);
    _touchScreenSize.set(55.0f);
    _tapZoomFactor.set(0.2f);
    _nodeRadiusThreshold.set(0.2f);
    _rollAngleThreshold.set(0.025f);
    _orbitSpeedThreshold.set(0.005f);
    _spinSensitivity.set(1.0f);
    _zoomSensitivityExponential.set(1.025f);
    _inputStillThreshold.set(0.0005f);
    _centroidStillThreshold.set(0.0018f);
    _interpretPan.set(0.015f);
    _slerpTime.set(3.0f);
    _guiButton.set(glm::ivec2(32, 64));
    _friction.set(glm::vec4(0.025, 0.025, 0.02, 0.02));
}

void TouchInteraction::tap() {
    _tap = true;
}

void TouchInteraction::touchActive(bool active) {
    _touchActive = active;
}

// Get & Setters
Camera* TouchInteraction::getCamera() {
    return _camera;
}

const SceneGraphNode* TouchInteraction::getFocusNode() {
    return global::navigationHandler.orbitalNavigator().anchorNode();
}
void TouchInteraction::setCamera(Camera* camera) {
    _camera = camera;
}
void TouchInteraction::setFocusNode(const SceneGraphNode* focusNode) {
    if (focusNode) {
        global::navigationHandler.orbitalNavigator().setAnchorNode(
            focusNode->identifier()
        );
    }
    else {
        global::navigationHandler.orbitalNavigator().setAnchorNode("");
    }
}

void FrameTimeAverage::updateWithNewFrame(double sample) {
    if (sample > 0.0005) {
        _samples[_index++] = sample;
        if (_index >= TotalSamples) {
            _index = 0;
        }
        if (_nSamples < TotalSamples) {
            _nSamples++;
        }
    }
}

double FrameTimeAverage::averageFrameTime() const {
    double ft;
    if (_nSamples == 0)
        ft = 1.0 / 60.0; //Just guess at 60fps if no data is available yet
    else
        ft = std::accumulate(_samples, _samples + _nSamples, 0.0) / (double)(_nSamples);
    return ft;
}

#ifdef TOUCH_DEBUG_PROPERTIES
TouchInteraction::DebugProperties::DebugProperties()
    : properties::PropertyOwner({ "TouchDebugProperties" })
    , interactionMode(
        { "interactionMode", "Current interaction mode", "" },
        "Unknown"
    )
    , nFingers(
        {"nFingers", "Number of fingers", ""},
        0, 0, 20
    )
    , interpretedInteraction(
        { "interpretedInteraction", "Interpreted interaction", "" },
        "Unknown"
    )
    , normalizedCentroidDistance(
        { "normalizedCentroidDistance", "Normalized Centroid Distance", "" },
        0.f, 0.f, 0.01f
    )
    , minDiff(
        { "minDiff", "Movement of slowest moving finger", "" },
        0.f, 0.f, 100.f
    )
    , rollOn(
        { "rollOn", "Roll On", "" },
        0.f, 0.f, 100.f
    )
{
    addProperty(interactionMode);
    addProperty(nFingers);
    addProperty(interpretedInteraction);
    addProperty(normalizedCentroidDistance);
    addProperty(minDiff);
    addProperty(rollOn);
}
#endif

} // openspace namespace
