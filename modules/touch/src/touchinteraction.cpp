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

#include <modules/touch/include/touchinteraction.h>
#include <modules/imgui/imguimodule.h>

#include <openspace/interaction/orbitalnavigator.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#endif

#include <cmath>
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


#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/interaction/navigationhandler.h>

namespace {
    const char* _loggerCat = "TouchInteraction";

    static const openspace::properties::Property::PropertyInfo OriginInfo = {
        "Origin",
        "Origin",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo UnitTestInfo = {
        "UnitTest",
        "Take a unit test saving the LM data into file",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo EventsInfo = {
        "TouchEvents",
        "True if we have a touch event",
        "",
        openspace::properties::Property::Visibility::Hidden
    };

    static const openspace::properties::Property::PropertyInfo SetDefaultInfo = {
        "SetDefault",
        "Reset all properties to default",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo MaxTapTimeInfo = {
        "MaxTapTime",
        "Max tap delay (in ms) for double tap",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo DecelatesPerSecondInfo = {
        "DeceleratesPerSecond",
        "Number of times velocity is decelerated per second",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TouchScreenSizeInfo = {
        "TouchScreenSize",
        "Touch Screen size in inches",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo TapZoomFactorInfo = {
        "TapZoomFactor",
        "Scaling distance travelled on tap",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo DirectManipulationInfo = {
        "DirectManipulationRadius",
        "Radius a planet has to have to activate direct-manipulation",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo RollThresholdInfo = {
        "RollThreshold",
        "Threshold for min angle for roll interpret",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo OrbitSpinningThreshold = {
        "OrbitThreshold",
        "Threshold to activate orbit spinning in direct-manipulation",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo SpinningSensitivityInfo = {
        "SpinningSensitivity",
        "Sensitivity of spinning in direct-manipulation",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo InputSensitivityInfo = {
        "InputSensitivity",
        "Threshold for interpreting input as still",
        ""
    };

    static const openspace::properties::Property::PropertyInfo StationaryCentroidInfo = {
        "CentroidStationary",
        "Threshold for stationary centroid",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo PanDeltaDistanceInfo = {
        "PanDeltaDistance",
        "Delta distance between fingers allowed for interpreting pan interaction",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo SlerpTimeInfo = {
        "SlerpTime",
        "Time to slerp in seconds to new orientation with new node picking",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo GuiButtonSizeInfo = {
        "GuiButtonSize",
        "GUI button size in pixels",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo FrictionInfo = {
        "Friction",
        "Friction for different interactions (orbit, zoom, roll, pan)",
        "" // @TODO Missing documentation
    };

    static const openspace::properties::Property::PropertyInfo PickingRadiusInfo = {
        "PickingRadiusMinimum",
        "Minimum radius for picking in NDC coordinates",
        "" // @TODO Missing documentation
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
    , _spinSensitivity(SpinningSensitivityInfo, 1.f, 0.f, 2.f)
    , _inputStillThreshold(InputSensitivityInfo, 0.0005f, 0.f, 0.001f)
    // used to void wrongly interpreted roll interactions
    , _centroidStillThreshold(StationaryCentroidInfo, 0.0018f, 0.f, 0.01f)
    , _interpretPan(PanDeltaDistanceInfo, 0.015f, 0.f, 0.1f)
    , _slerpTime(SlerpTimeInfo, 3.f, 0.f, 5.f)
    , _guiButton(
        GuiButtonSizeInfo,
        glm::ivec2(32, 64),
        glm::ivec2(8, 16),
        glm::ivec2(128, 256)
    )
    , _friction(
        FrictionInfo,
        glm::vec4(0.01f, 0.025f, 0.02f, 0.02f),
        glm::vec4(0.f),
        glm::vec4(0.2f)
    )
    , _pickingRadiusMinimum(PickingRadiusInfo, 0.1f, 0.f, 1.f)
    , _vel{ glm::dvec2(0.0), 0.0, 0.0, glm::dvec2(0.0) }
    , _sensitivity{ glm::dvec2(0.08, 0.045), 4.0, 2.75, glm::dvec2(0.08, 0.045) }
    // calculated with two vectors with known diff in length, then
    // projDiffLength/diffLength.
    , _projectionScaleFactor(1.000004)
    , _currentRadius(1.0)
    , _slerpdT(1000)
    , _timeSlack(0.0)
    , _numOfTests(0)
    , _directTouchMode(false)
    , _tap(false)
    , _doubleTap(false)
    , _lmSuccess(true)
    , _guiON(false)
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
    addProperty(_inputStillThreshold);
    addProperty(_centroidStillThreshold);
    addProperty(_interpretPan);
    addProperty(_slerpTime);
    addProperty(_guiButton);
    addProperty(_friction);
    addProperty(_pickingRadiusMinimum);

    _origin.onChange([this]() {
        SceneGraphNode* node = sceneGraphNode(_origin.value());
        if (!node) {
            LWARNING(
                "Could not find a node in scenegraph called '" << _origin.value() << "'"
            );
            return;
        }
        setFocusNode(node);
    });

    levmarq_init(&_lmstat);


    _time.initSession();
}

// Called each frame if there is any input
void TouchInteraction::updateStateFromInput(const std::vector<TuioCursor>& list,
                                            std::vector<Point>& lastProcessed)
{
    if (_tap) { // check for doubletap
        if (_time.getSessionTime().getTotalMilliseconds() < _maxTapTime) {
            _doubleTap = true;
            _tap = false;
        }
        _time.initSession();
    }

    if (!guiMode(list)) {
        if (_directTouchMode && _selected.size() > 0 && list.size() == _selected.size()) {
            directControl(list);
        }
        if (_lmSuccess) {
            findSelectedNode(list);
        }
        if (!_directTouchMode) {
            computeVelocities(list, lastProcessed);
        }

        // evaluates if current frame is in directTouchMode (will be used next frame)
        _directTouchMode =
            (_currentRadius > _nodeRadiusThreshold && _selected.size() == list.size());
    }
}

// Activates/Deactivates gui input mode (if active it voids all other interactions)
bool TouchInteraction::guiMode(const std::vector<TuioCursor>& list) {
    WindowWrapper& wrapper = OsEng.windowWrapper();
    glm::ivec2 res = wrapper.currentWindowSize();
    glm::dvec2 pos = glm::vec2(
        list.at(0).getScreenX(res.x),
        list.at(0).getScreenY(res.y)
    );

    ImGUIModule& module = *(OsEng.moduleEngine().module<ImGUIModule>());
    _guiON = module.gui.isEnabled();

    if (_tap && list.size() == 1 &&
        std::abs(pos.x) < _guiButton.value().x && std::abs(pos.y) < _guiButton.value().y)
    {
        // pressed invisible button
        _guiON = !_guiON;
        module.gui.setEnabled(_guiON);

        std::string mode = (_guiON) ? "" : "de";
        LINFO(
            "GUI mode is " << mode << "activated. Inside box by: (" <<
            static_cast<int>(100 * (pos.x / _guiButton.value().x)) << "%, " <<
            static_cast<int>(100 * (pos.y / _guiButton.value().y)) << "%)\n"
        );
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

    // Returns the screen point s(xi,par) dependent the transform M(par) and object
    // point xi
    auto distToMinimize = [](double* par, int x, void* fdata, LMstat* lmstat) {
        FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);

        // Apply transform to camera and find the new screen point of the updated camera
        // state

        // { vec2 globalRot, zoom, roll, vec2 localRot }
        double q[6] = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };
        for (int i = 0; i < ptr->nDOF; ++i) {
            q[i] = par[i];
        }

        using namespace glm;
        // Create variables from current state
        dvec3 camPos = ptr->camera->positionVec3();
        dvec3 centerPos = ptr->node->worldPosition();

        dvec3 directionToCenter = normalize(centerPos - camPos);
        dvec3 lookUp = ptr->camera->lookUpVectorWorldSpace();
        dvec3 camDirection = ptr->camera->viewDirectionWorldSpace();

        // Make a representation of the rotation quaternion with local and global
        // rotations
        dmat4 lookAtMat = lookAt(
            dvec3(0, 0, 0),
            directionToCenter,
            // To avoid problem with lookup in up direction
            normalize(camDirection + lookUp));
        dquat globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
        dquat localCamRot = inverse(globalCamRot) * ptr->camera->rotationQuaternion();

        { // Roll
            dquat rollRot = angleAxis(q[3], dvec3(0.0, 0.0, 1.0));
            localCamRot = localCamRot * rollRot;
        }
        { // Panning (local rotation)
            dvec3 eulerAngles(q[5], q[4], 0);
            dquat panRot = dquat(eulerAngles);
            localCamRot = localCamRot * panRot;
        }
        { // Orbit (global rotation)
            dvec3 eulerAngles(q[1], q[0], 0);
            dquat rotationDiffCamSpace = dquat(eulerAngles);
            
            dvec3 centerToCamera = camPos - centerPos;

            dquat rotationDiffWorldSpace =
                globalCamRot * rotationDiffCamSpace * inverse(globalCamRot);
            dvec3 rotationDiffVec3 =
                centerToCamera * rotationDiffWorldSpace - centerToCamera;
            camPos += rotationDiffVec3;

            centerToCamera = camPos - centerPos;
            directionToCenter = normalize(-centerToCamera);
            dvec3 lookUpWhenFacingCenter =
                globalCamRot * dvec3(ptr->camera->lookUpVectorCameraSpace());
            lookAtMat = lookAt(
                dvec3(0, 0, 0),
                directionToCenter,
                lookUpWhenFacingCenter);
            globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
        }
        { // Zooming
            camPos += directionToCenter * q[2];
        }
        // Update the camera state
        Camera cam = *(ptr->camera);
        cam.setPositionVec3(camPos);
        cam.setRotation(globalCamRot * localCamRot);

        // we now have a new position and orientation of camera, project surfacePoint to
        // the new screen to get distance to minimize
        glm::dvec2 newScreenPoint = ptr->castToNDC(
            ptr->selectedPoints.at(x),
            cam,
            ptr->node
        );
        lmstat->pos.push_back(newScreenPoint);
        return glm::length(ptr->screenPoints.at(x) - newScreenPoint);
    };
    // Gradient of distToMinimize w.r.t par (using forward difference)
    auto gradient = [](double* g, double* par, int x, void* fdata, LMstat* lmstat) {
        FunctionData* ptr = reinterpret_cast<FunctionData*>(fdata);
        double h, lastG, f1, f0 = ptr->distToMinimize(par, x, fdata, lmstat);
         // scale value to find minimum step size h, dependant on planet size
        double scale = log10(ptr->node->boundingSphere());
        std::vector<double> dPar(ptr->nDOF, 0.0);
        dPar.assign(par, par + ptr->nDOF);

        for (int i = 0; i < ptr->nDOF; ++i) {
            // Initial values
            h = 1e-8;
            lastG = 1;
            dPar.at(i) += h;
            f1 = ptr->distToMinimize(dPar.data(), x, fdata, lmstat);
            dPar.at(i) = par[i];
            // Iterative process to find the minimum step h that gives a good gradient
            for (int j = 0; j < 100; ++j) {
                if ((f1 - f0) != 0 && lastG == 0) { // found minimum step size h
                    // scale up to get a good initial guess value
                    h *= scale * scale * scale;

                    // clamp min step size to a fraction of the incoming parameter
                    if (i == 2) {
                        double epsilon = 1e-3;
                        // make sure incoming parameter is larger than 0
                        h = std::max(std::max(std::abs(dPar.at(i)), epsilon) * 0.001, h);
                    }
                    else if (ptr->nDOF == 2) {
                        h = std::max(std::abs(dPar.at(i)) * 0.001, h);
                    }

                    // calculate f1 with good h for finite difference
                    dPar.at(i) += h;
                    f1 = ptr->distToMinimize(dPar.data(), x, fdata, lmstat);
                    dPar.at(i) = par[i];
                    break;
                }
                else if ((f1 - f0) != 0 && lastG != 0) { // h too big
                    h /= scale;
                }
                else if ((f1 - f0) == 0) { // h too small
                    h *= scale;
                }
                lastG = f1 - f0;
                dPar.at(i) += h;
                f1 = ptr->distToMinimize(dPar.data(), x, fdata, lmstat);
                dPar.at(i) = par[i];
            }
            g[i] = (f1 - f0) / h;
        }
        if (ptr->nDOF == 2) {
             // normalize on 1 finger case to allow for horizontal/vertical movement
            for (int i = 0; i < 2; ++i) {
                g[i] = g[i]/std::abs(g[i]);
            }
        }
        else if (ptr->nDOF == 6) {
            for (int i = 0; i < ptr->nDOF; ++i) {
                 // lock to only pan and zoom on 3 finger case, no roll/orbit
                g[i] = (i == 2) ? g[i] : g[i] / std::abs(g[i]);
            }
        }
    };

    // project back a 3D point in model view to clip space [-1,1] coordinates on the view
    // plane
    auto castToNDC = [](const glm::dvec3& vec, Camera& camera, SceneGraphNode* node) {
        glm::dvec3 posInCamSpace = glm::inverse(camera.rotationQuaternion()) *
            (node->rotationMatrix() * vec +
            (node->worldPosition() - camera.positionVec3()));

        glm::dvec4 clipspace = camera.projectionMatrix() * glm::dvec4(posInCamSpace, 1.0);
        return (glm::dvec2(clipspace) / clipspace.w);
    };

    // only send in first three fingers (to make it easier for LMA to converge on 3+
    // finger case with only zoom/pan)
    int nFingers = std::min(static_cast<int>(list.size()), 3);
    int nDOF = std::min(nFingers * 2, 6);
    std::vector<double> par(nDOF, 0.0);
    par.at(0) = _lastVel.orbit.x; // use _lastVel for orbit
    par.at(1) = _lastVel.orbit.y;

    // Parse input data to be used in the LM algorithm
    std::vector<glm::dvec3> selectedPoints;
    std::vector<glm::dvec2> screenPoints;
    for (int i = 0; i < nFingers; ++i) {
        const SelectedBody& sb = _selected.at(i);
        selectedPoints.push_back(sb.coordinates);

        std::vector<TuioCursor>::const_iterator c = std::find_if(
            list.begin(),
            list.end(),
            [&sb](const TuioCursor& c) { return c.getSessionID() == sb.id; }
        );
        // normalized -1 to 1 coordinates on screen
        screenPoints.push_back(
            glm::dvec2(2 * (c->getX() - 0.5), -2 * (c->getY() - 0.5))
        );
    }
    FunctionData fData = {
        selectedPoints,
        screenPoints,
        nDOF,
        castToNDC,
        distToMinimize,
        _camera,
        _selected.at(0).node,
        _lmstat,
        _currentRadius
    };
    void* dataPtr = reinterpret_cast<void*>(&fData);

    // finds best transform values for the new camera state and stores them in par
    _lmSuccess = levmarq(
        nDOF,
        par.data(),
        nFingers,
        nullptr,
        distToMinimize,
        gradient,
        dataPtr,
        &_lmstat
    );

    if (_lmSuccess && !_unitTest) {
         // if good values were found set new camera state
        _vel.orbit = glm::dvec2(par.at(0), par.at(1));
        if (nDOF > 2) {
            _vel.zoom = par.at(2);
            _vel.roll = par.at(3);
            if (nDOF > 4) {
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
        OsEng.moduleEngine().module<ImGUIModule>()->touchInput = {
            1,
            glm::dvec2(0.0, 0.0), 1
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
    for (SceneGraphNode* node : OsEng.renderEngine().scene()->allSceneGraphNodes())
        for (std::string name : selectables)
            if (node->name() == name)
                selectableNodes.push_back(node);

    glm::dquat camToWorldSpace = _camera->rotationQuaternion();
    glm::dvec3 camPos = _camera->positionVec3();
    std::vector<SelectedBody> newSelected;

    struct PickingInfo {
        SceneGraphNode* node;
        double pickingDistanceNDC;
        double pickingDistanceWorld;
    };
    std::vector<PickingInfo> pickingInfo;


    for (const TuioCursor& c : list) {
        double xCo = 2 * (c.getX() - 0.5);
        double yCo = -2 * (c.getY() - 0.5); // normalized -1 to 1 coordinates on screen
         // vec3(projectionmatrix * clipspace), divide with w?
        glm::dvec3 cursorInWorldSpace = camToWorldSpace *
            glm::dvec3(glm::inverse(_camera->projectionMatrix()) *
            glm::dvec4(xCo, yCo, -1.0, 1.0));
        glm::dvec3 raytrace = glm::normalize(cursorInWorldSpace);

        int id = c.getSessionID();

       for (SceneGraphNode* node : selectableNodes) {
            double boundingSphere = node->boundingSphere();
            glm::dvec3 camToSelectable = node->worldPosition() - camPos;
            double dist = length(glm::cross(cursorInWorldSpace, camToSelectable)) /
                          glm::length(cursorInWorldSpace) - boundingSphere;
            if (dist <= 0.0) {
                // finds intersection closest point between boundingsphere and line in
                // world coordinates, assumes line direction is normalized
                double d = glm::dot(raytrace, camToSelectable);
                double root = boundingSphere * boundingSphere -
                              glm::dot(camToSelectable, camToSelectable) + d * d;
                if (root > 0) { // two intersection points (take the closest one)
                    d -= sqrt(root);
                }
                glm::dvec3 intersectionPoint = camPos + d * raytrace;
                glm::dvec3 pointInModelView = glm::inverse(node->rotationMatrix()) *
                                              (intersectionPoint - node->worldPosition());

                // Add id, node and surface coordinates to the selected list
                std::vector<SelectedBody>::iterator oldNode = std::find_if(
                    newSelected.begin(),
                    newSelected.end(),
                    [id](SelectedBody s) { return s.id == id; }
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
                if (dist <= 0.0 || (ndcDist <= _pickingRadiusMinimum)) {

                    // If the user touched the planet directly, this is definitely the one
                    // they are interested in  =>  minimum distance
                    if (dist <= 0.0) {
                        LINFOC(node->name(), "Picking candidate based on direct touch");
                        pickingInfo.push_back({
                            node,
                            -std::numeric_limits<double>::max(),
                            -std::numeric_limits<double>::max()
                        });
                    }
                    else {
                        // The node was considered due to minimum picking distance radius
                        LINFOC(node->name(), "Picking candidate based on proximity");
                        pickingInfo.push_back({
                            node,
                            ndcDist,
                            dist
                        });
                    }
                }
            }
        }
    }


    // After we are done with all of the nodes, we can sort the picking list and pick the
    // one that fits best (= is closest or was touched directly)
    std::sort(
        pickingInfo.begin(),
        pickingInfo.end(),
        [](const PickingInfo& lhs, const PickingInfo& rhs) {
            return lhs.pickingDistanceWorld < rhs.pickingDistanceWorld;
        }
    );

    // If an item has been picked, it's in the first position of the vector now
    if (!pickingInfo.empty()) {
        _pickingSelected = pickingInfo.begin()->node;
        LINFOC("Picking", "Picked node: " + _pickingSelected->name());
    }

    LINFOC("Picking", "============");

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
    int id = 0;
    for (const TuioCursor& c : list) {
        TuioPoint itPoint = std::find_if(
            lastProcessed.begin(),
            lastProcessed.end(),
            [&c](const Point& p) { return p.first == c.getSessionID(); }
        )->second;
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

    if (_doubleTap) {
        return PICK;
    }
    else  if (list.size() == 1) {
        return ROT;
    }
    else {
        float avgDistance = static_cast<float>(
            std::abs(dist - lastDist) / list.at(0).getMotionSpeed()
        );
        // if average distance between 3 fingers are constant we have panning
        if (avgDistance < _interpretPan && list.size() == 3) {
            return PAN;
        }
        // we have roll if one finger is still, or the total roll angles around the
        // centroid is over _rollAngleThreshold (_centroidStillThreshold is used to void
        // misinterpretations)
        else if (std::abs(minDiff) < _inputStillThreshold ||
                (std::abs(rollOn) < 100.0 &&
                glm::distance(_centroid, lastCentroid) / list.size()
                    < _centroidStillThreshold))
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
    TuioCursor cursor = list.at(0);
    int action = interpretInteraction(list, lastProcessed);

    switch (action) {
        case ROT: { // add rotation velocity
            _vel.orbit += glm::dvec2(cursor.getXSpeed() *
                          _sensitivity.orbit.x, cursor.getYSpeed() *
                          _sensitivity.orbit.y);
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

            double zoomFactor = (distance - lastDistance) *
                (glm::distance(_camera->positionVec3(), _camera->focusPositionVec3()) -
                _focusNode->boundingSphere());
            _vel.zoom += zoomFactor * _sensitivity.zoom *
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
            break;
        }
        case PAN: {
             // add local rotation velocity
            _vel.pan += glm::dvec2(cursor.getXSpeed() *
                        _sensitivity.pan.x, cursor.getYSpeed() * _sensitivity.pan.y);
            break;
        }
        case PICK: {
             // pick something in the scene as focus node
            if (_pickingSelected) {
                setFocusNode(_pickingSelected);
                 // cant do setFocusNode() since TouchInteraction is not subclass of
                 // InteractionMode
                OsEng.navigationHandler().setFocusNode(_focusNode);

                // rotate camera to look at new focus, using slerp quat
                glm::dvec3 camToFocus = _focusNode->worldPosition() -
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
                double dist = glm::distance(
                    _camera->positionVec3(), _camera->focusPositionVec3()
                ) - _focusNode->boundingSphere();
                _vel.zoom = _sensitivity.zoom *
                            std::max(_touchScreenSize.value() * 0.1, 1.0) *
                            _tapZoomFactor * dist;
            }
            break;
        }
    }
}

// Main update call, calculates the new orientation and position for the camera depending
// on _vel and dt. Called every frame
void TouchInteraction::step(double dt) {
    using namespace glm;

     // since functions cant be called directly (TouchInteraction not a subclass of
     // InteractionMode)
    setFocusNode(OsEng.navigationHandler().focusNode());
    if (_focusNode && _camera) {
        // Create variables from current state
        dvec3 camPos = _camera->positionVec3();
        dvec3 centerPos = _focusNode->worldPosition();

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

        double boundingSphere = _focusNode->boundingSphere();
        dvec3 centerToBoundingSphere;
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
            dmat4 lookAtMat = lookAt(
                dvec3(0, 0, 0),
                directionToCenter,
                lookUpWhenFacingCenter);
            globalCamRot = normalize(quat_cast(inverse(lookAtMat)));
        }
        { // Zooming
            centerToBoundingSphere = -directionToCenter * boundingSphere;
            dvec3 centerToCam = camPos - centerPos;
            double distToSurface = length(centerToCam - centerToBoundingSphere);

            if (length(_vel.zoom * dt) < distToSurface &&
                length(centerToCam + directionToCenter*_vel.zoom * dt) >
                    length(centerToBoundingSphere))
            {
                camPos += directionToCenter * _vel.zoom * dt;
            }
            else {
                _vel.zoom = 0.0;
            }
        }

        decelerate(dt);
        // Update the camera state
        _camera->setPositionVec3(camPos);
        _camera->setRotation(globalCamRot * localCamRot);

        _tap = false;
        _doubleTap = false;
        if (_reset) {
            resetToDefault();
        }
    }
}

void TouchInteraction::unitTest() {
    if (_unitTest) {
        _lmstat.verbose = true;

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
        file << _lmstat.data;

        // clear everything
        _selected.clear();
        _pickingSelected = nullptr;
        _vel.orbit = glm::dvec2(0.0, 0.0);
        _vel.zoom = 0.0;
        _vel.roll = 0.0;
        _vel.pan = glm::dvec2(0.0, 0.0);
        _lastVel = _vel;
        _unitTest = false;

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
    double frequency = 1.0 / _deceleratesPerSecond;
    // Number of times velocities should decelerate, depending on chosen frequency and
    // time slack over from last frame
    int times = static_cast<int>((dt + _timeSlack) / frequency);
    // Save the new time slack for the next frame
    _timeSlack = fmod((dt + _timeSlack), frequency);

    // Decelerate zoom velocity quicker if we're close enough to use direct-manipulation
    if (!_directTouchMode && _currentRadius > _nodeRadiusThreshold &&
        _vel.zoom > _focusNode->boundingSphere())
    {
        _vel.zoom *= std::pow(1 - 2 * _friction.value().y, times);
    }
    _vel.orbit *= std::pow(1 - _friction.value().x, times);
    _vel.zoom *= std::pow(1 - _friction.value().y, times);
    _vel.roll *= std::pow(1 - _friction.value().z, times);
    _vel.pan *= std::pow(1 - _friction.value().w, times);
}

// Called if all fingers are off the screen
void TouchInteraction::resetAfterInput() {
    if (_directTouchMode && _selected.size() > 0 && _lmSuccess) {
        double spinDelta = _spinSensitivity / OsEng.windowWrapper().averageDeltaTime();
        if (glm::length(_lastVel.orbit) > _orbitSpeedThreshold) {
             // allow node to start "spinning" after direct-manipulation finger is let go
            _vel.orbit = _lastVel.orbit * spinDelta;
        }
    }
    // Reset emulated mouse values
    ImGUIModule& module = *(OsEng.moduleEngine().module<ImGUIModule>());
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
    _inputStillThreshold.set(0.0005f);
    _centroidStillThreshold.set(0.0018f);
    _interpretPan.set(0.015f);
    _slerpTime.set(3.0f);
    _guiButton.set(glm::ivec2(32, 64));
    _friction.set(glm::vec4(0.01, 0.025, 0.02, 0.02));
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

SceneGraphNode* TouchInteraction::getFocusNode() {
    return _focusNode;
}
void TouchInteraction::setCamera(Camera* camera) {
    _camera = camera;
}
void TouchInteraction::setFocusNode(SceneGraphNode* focusNode) {
    _focusNode = focusNode;
}

} // openspace namespace
