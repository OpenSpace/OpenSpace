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

#include <openspace/interaction/interactionmode.h>
#include <openspace/interaction/interactionhandler.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
#include <modules/globebrowsing/globes/renderableglobe.h>
#include <modules/globebrowsing/globes/chunkedlodglobe.h>
#include <modules/globebrowsing/geometry/geodetic2.h>
#endif


namespace {
    const std::string _loggerCat = "InteractionMode";
}

namespace openspace {
namespace interaction {

    // InputState
    InputState::InputState() {

    }

    InputState::~InputState() {

    }

    const std::vector<datamessagestructures::CameraKeyframe>& InputState::keyframes() const {
        return _keyframes;
    }

    void InputState::addKeyframe(const datamessagestructures::CameraKeyframe &kf) {
        clearOldKeyframes();
        if (kf._timestamp < OsEng.runTime()) {
            return;
        }
        _keyframes.insert(std::upper_bound(_keyframes.begin(), _keyframes.end(), kf, &InputState::compareKeyframeTimes), kf);
    }

    void InputState::removeKeyframesAfter(double timestamp) {
        datamessagestructures::CameraKeyframe kf;
        kf._timestamp = timestamp;
        // Remove keyframes after the inserted keyframe.
        _keyframes.erase(std::upper_bound(_keyframes.begin(), _keyframes.end(), kf, &InputState::compareKeyframeTimes), _keyframes.end());
    }

    bool InputState::compareKeyframeTimes(const datamessagestructures::CameraKeyframe& a, const datamessagestructures::CameraKeyframe& b) {
        return a._timestamp < b._timestamp;
    }

    void InputState::clearOldKeyframes() {
        double now = OsEng.runTime();
        auto isLater = [now](const datamessagestructures::CameraKeyframe kf) {
            return kf._timestamp > now;
        };

        // Remote keyframes with earlier timestamps than the current time.
        auto nextKeyframe = std::find_if(_keyframes.begin(), _keyframes.end(), isLater);
        if (nextKeyframe != _keyframes.begin()) {
            _keyframes.erase(_keyframes.begin(), nextKeyframe - 1);
        }
    }

    void InputState::clearKeyframes() {
        _keyframes.clear();
    }

    void InputState::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
        if (action == KeyAction::Press) {
            _keysDown.push_back(std::pair<Key, KeyModifier>(key, modifier));
        }
        else if (action == KeyAction::Release) {
            // Remove all key pressings for 'key'
            _keysDown.remove_if([key](std::pair<Key, KeyModifier> keyModPair)
            { return keyModPair.first == key; });
        }
    }

    void InputState::mouseButtonCallback(MouseButton button, MouseAction action) {
        if (action == MouseAction::Press) {
            _mouseButtonsDown.push_back(button);
        }
        else if (action == MouseAction::Release) {
            // Remove all key pressings for 'button'
            _mouseButtonsDown.remove_if([button](MouseButton buttonInList)
            { return button == buttonInList; });
        }
    }

    void InputState::mousePositionCallback(double mouseX, double mouseY) {
        _mousePosition = glm::dvec2(mouseX, mouseY);
    }

    void InputState::mouseScrollWheelCallback(double mouseScrollDelta) {
        _mouseScrollDelta = mouseScrollDelta;
    }

    const std::list<std::pair<Key, KeyModifier> >& InputState::getPressedKeys() const {
        return _keysDown;
    }

    const std::list<MouseButton>& InputState::getPressedMouseButtons() const {
        return _mouseButtonsDown;
    }

    glm::dvec2 InputState::getMousePosition() const {
        return _mousePosition;
    }

    double InputState::getMouseScrollDelta() const {
        return _mouseScrollDelta;
    }

    bool InputState::isKeyPressed(std::pair<Key, KeyModifier> keyModPair) const {
        for (auto it = _keysDown.begin(); it != _keysDown.end(); it++) {
            if (*it == keyModPair) {
                return true;
            }
        }
        return false;
    }
    
    bool InputState::isKeyPressed(Key key) const {
        for (auto it = _keysDown.begin(); it != _keysDown.end(); it++) {
            if ((*it).first == key) {
                return true;
            }
        }
        return false;
    }

    bool InputState::isMouseButtonPressed(MouseButton mouseButton) const {
        for (auto it = _mouseButtonsDown.begin(); it != _mouseButtonsDown.end(); it++) {
            if (*it == mouseButton) {
                return true;
            }
        }
        return false;
    }



InteractionMode::InteractionMode()
    : _rotateToFocusNodeInterpolator(Interpolator<double>([](double t){
        return pow(t, 2);
    })) {
}


InteractionMode::~InteractionMode() {

}

void InteractionMode::setFocusNode(SceneGraphNode* focusNode) {
    _focusNode = focusNode;

    if (_focusNode != nullptr) {
        _previousFocusNodePosition = _focusNode->worldPosition();
        _previousFocusNodeRotation = glm::quat_cast(_focusNode->worldRotationMatrix());
    }
}

SceneGraphNode* InteractionMode::focusNode() {
    return _focusNode;
}

Interpolator<double>& InteractionMode::rotateToFocusNodeInterpolator() {
    return _rotateToFocusNodeInterpolator;
};
    
    
// KeyframeInteractionMode
KeyframeInteractionMode::KeyframeInteractionMode(){

}

KeyframeInteractionMode::~KeyframeInteractionMode() {

}

void KeyframeInteractionMode::updateMouseStatesFromInput(const InputState& inputState, double) {
    _keyframes = inputState.keyframes();
}

void KeyframeInteractionMode::updateCameraStateFromMouseStates(Camera& camera, double) {
    if (_keyframes.size() == 0) {
        return;
    }

    double now = OsEng.runTime();
    auto isLater = [now](const datamessagestructures::CameraKeyframe kf) {
        return kf._timestamp > now;
    };

    auto nextKeyframe = std::find_if(_keyframes.begin(), _keyframes.end(), isLater);
    if (nextKeyframe == _keyframes.end()) {
        return;
    }

    if (nextKeyframe == _keyframes.begin()) {
        camera.setPositionVec3(_keyframes[0]._position);
        camera.setRotation(_keyframes[0]._rotation);
        return;
    }
    auto prevKeyframe = nextKeyframe - 1;

    double prevTime = prevKeyframe->_timestamp;
    double nextTime = nextKeyframe->_timestamp;

    double t = (now - prevTime) / (nextTime - prevTime);

    Scene* scene = camera.parent()->scene();
    SceneGraphNode* prevFocusNode = scene->sceneGraphNode(prevKeyframe->_focusNode);
    SceneGraphNode* nextFocusNode = scene->sceneGraphNode(nextKeyframe->_focusNode);

    if (!prevFocusNode || !nextFocusNode) {
        return;
    }

    glm::dvec3 prevKeyframeCameraPosition = prevKeyframe->_position;
    glm::dvec3 nextKeyframeCameraPosition = nextKeyframe->_position;
    glm::dquat prevKeyframeCameraRotation = prevKeyframe->_rotation;
    glm::dquat nextKeyframeCameraRotation = nextKeyframe->_rotation;

    // Transform position and rotation based on focus node rotation (if following rotation)
    if (prevKeyframe->_followNodeRotation) {
        prevKeyframeCameraRotation = prevFocusNode->worldRotationMatrix() * glm::dmat3(prevKeyframe->_rotation);
        prevKeyframeCameraPosition = prevFocusNode->worldRotationMatrix() * prevKeyframeCameraPosition;
    }
    if (nextKeyframe->_followNodeRotation) {
        nextKeyframeCameraRotation = nextFocusNode->worldRotationMatrix() * glm::dmat3(nextKeyframe->_rotation);
        nextKeyframeCameraPosition = nextFocusNode->worldRotationMatrix() * nextKeyframeCameraPosition;
    }

    // Transform position based on focus node position
    prevKeyframeCameraPosition += prevFocusNode->worldPosition();
    nextKeyframeCameraPosition += nextFocusNode->worldPosition();

    // Linear interpolation
    camera.setPositionVec3(prevKeyframeCameraPosition * (1 - t) + nextKeyframeCameraPosition * t);
    camera.setRotation(glm::slerp(prevKeyframeCameraRotation, nextKeyframeCameraRotation, t));
}

bool KeyframeInteractionMode::followingNodeRotation() const {
    return false;
}

// OrbitalInteractionMode
OrbitalInteractionMode::MouseStates::MouseStates(double sensitivity, double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationMouseState(velocityScaleFactor)
    , _localRotationMouseState(velocityScaleFactor)
    , _truckMovementMouseState(velocityScaleFactor)
    , _localRollMouseState(velocityScaleFactor)
    , _globalRollMouseState(velocityScaleFactor)
{}

void OrbitalInteractionMode::MouseStates::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {
    glm::dvec2 mousePosition = inputState.getMousePosition();

    bool button1Pressed = inputState.isMouseButtonPressed(MouseButton::Button1);
    bool button2Pressed = inputState.isMouseButtonPressed(MouseButton::Button2);
    bool button3Pressed = inputState.isMouseButtonPressed(MouseButton::Button3);
    bool keyCtrlPressed = inputState.isKeyPressed(Key::LeftControl);
    bool keyShiftPressed = inputState.isKeyPressed(Key::LeftShift);
    
    // Update the mouse states
    if (button1Pressed && !keyShiftPressed) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta =
                _localRotationMouseState.previousPosition - mousePosition;
            _localRotationMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);

            _globalRotationMouseState.previousPosition = mousePosition;
            _globalRotationMouseState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta =
                _globalRotationMouseState.previousPosition - mousePosition;
            _globalRotationMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);

            _localRotationMouseState.previousPosition = mousePosition;
            _localRotationMouseState.velocity.decelerate(deltaTime);
        }
    }
    else { // !button1Pressed
        _localRotationMouseState.previousPosition = mousePosition;
        _localRotationMouseState.velocity.decelerate(deltaTime);

        _globalRotationMouseState.previousPosition = mousePosition;
        _globalRotationMouseState.velocity.decelerate(deltaTime);
    }
    if (button2Pressed) {
        glm::dvec2 mousePositionDelta =
            _truckMovementMouseState.previousPosition - mousePosition;
        _truckMovementMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);
    }
    else { // !button2Pressed
        _truckMovementMouseState.previousPosition = mousePosition;
        _truckMovementMouseState.velocity.decelerate(deltaTime);
    }
    if (button3Pressed || (keyShiftPressed && button1Pressed)) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta =
                _localRollMouseState.previousPosition - mousePosition;
            _localRollMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);

            _globalRollMouseState.previousPosition = mousePosition;
            _globalRollMouseState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta =
                _globalRollMouseState.previousPosition - mousePosition;
            _globalRollMouseState.velocity.set(mousePositionDelta * _sensitivity, deltaTime);

            _localRollMouseState.previousPosition = mousePosition;
            _localRollMouseState.velocity.decelerate(deltaTime);
        }
    }
    else { // !button3Pressed
        _globalRollMouseState.previousPosition = mousePosition;
        _globalRollMouseState.velocity.decelerate(deltaTime);

        _localRollMouseState.previousPosition = mousePosition;
        _localRollMouseState.velocity.decelerate(deltaTime);
    }
}

void OrbitalInteractionMode::MouseStates::setRotationalFriction(double friction) {
    _localRotationMouseState.setFriction(friction);
    _localRollMouseState.setFriction(friction);
    _globalRollMouseState.setFriction(friction);
}

void OrbitalInteractionMode::MouseStates::setHorizontalFriction(double friction) {
    _globalRotationMouseState.setFriction(friction);
}

void OrbitalInteractionMode::MouseStates::setVerticalFriction(double friction) {
    _truckMovementMouseState.setFriction(friction);
}

void OrbitalInteractionMode::MouseStates::setSensitivity(double sensitivity) {
    _sensitivity = sensitivity;
}

void OrbitalInteractionMode::MouseStates::setVelocityScaleFactor(double scaleFactor) {
    _globalRotationMouseState.setVelocityScaleFactor(scaleFactor);
    _localRotationMouseState.setVelocityScaleFactor(scaleFactor);
    _truckMovementMouseState.setVelocityScaleFactor(scaleFactor);
    _localRollMouseState.setVelocityScaleFactor(scaleFactor);
    _globalRollMouseState.setVelocityScaleFactor(scaleFactor);
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedGlobalRotationMouseVelocity() {
    return _globalRotationMouseState.velocity.get();
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedLocalRotationMouseVelocity() {
    return _localRotationMouseState.velocity.get();
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedTruckMovementMouseVelocity() {
    return _truckMovementMouseState.velocity.get();
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedLocalRollMouseVelocity() {
    return _localRollMouseState.velocity.get();
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedGlobalRollMouseVelocity() {
    return _globalRollMouseState.velocity.get();
}

OrbitalInteractionMode::OrbitalInteractionMode(
    std::shared_ptr<MouseStates> mouseStates)
    : InteractionMode()
    , _mouseStates(mouseStates) {
        
}

OrbitalInteractionMode::~OrbitalInteractionMode() {

}

void OrbitalInteractionMode::updateCameraStateFromMouseStates(Camera& camera, double deltaTime) {
    // Update synched data

    using namespace glm;
    if (_focusNode) {
        // Read the current state of the camera and focus node
        dvec3 camPos = camera.positionVec3();
        
        // Follow focus nodes movement
        dvec3 centerPos = _focusNode->worldPosition();
        dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
        _previousFocusNodePosition = centerPos;
        camPos += focusNodeDiff;

        dquat totalRotation = camera.rotationQuaternion();
        dvec3 directionToCenter = normalize(centerPos - camPos);
        dvec3 lookUp = camera.lookUpVectorWorldSpace();
        double boundingSphere = _focusNode->boundingSphere();
        dvec3 camDirection = camera.viewDirectionWorldSpace();

        // Declare other variables used in interaction calculations
        double minHeightAboveBoundingSphere = 1;
        dvec3 centerToCamera = camPos - centerPos;
        dvec3 centerToBoundingSphere;

        // Create the internal representation of the local and global camera rotations
        dmat4 lookAtMat = lookAt(
            dvec3(0, 0, 0),
            directionToCenter,
            normalize(camDirection + lookUp)); // To avoid problem with lookup in up direction
        dquat globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
        dquat localCameraRotation = inverse(globalCameraRotation) * totalRotation;

        { // Do local roll
            glm::dquat cameraRollRotation =
                glm::angleAxis(_mouseStates->synchedLocalRollMouseVelocity().x, dvec3(0, 0, 1));
            localCameraRotation = localCameraRotation * cameraRollRotation;
        }
        if (!_rotateToFocusNodeInterpolator.isInterpolating())
        { // Do local rotation
            dvec3 eulerAngles(_mouseStates->synchedLocalRotationMouseVelocity().y, _mouseStates->synchedLocalRotationMouseVelocity().x, 0);
            dquat rotationDiff = dquat(eulerAngles);

            localCameraRotation = localCameraRotation * rotationDiff;
        }
        else
        { // Interpolate local rotation to focus node
            double t = _rotateToFocusNodeInterpolator.value();
            localCameraRotation = slerp(localCameraRotation, dquat(dvec3(0.0)), t);
            _rotateToFocusNodeInterpolator.step(deltaTime * 0.1); // Should probably depend on dt
            // This is a fast but ugly solution to slow regaining of control...
            if (t > 0.18) {
                _rotateToFocusNodeInterpolator.end();
            }
        }
        { // Do global rotation
            dvec2 smoothMouseVelocity = _mouseStates->synchedGlobalRotationMouseVelocity();
            dvec3 eulerAngles(-smoothMouseVelocity.y, -smoothMouseVelocity.x, 0);
            dquat rotationDiffCamSpace = dquat(eulerAngles);

            dquat newRotationCamspace = globalCameraRotation * rotationDiffCamSpace;
            dquat rotationDiffWorldSpace = newRotationCamspace * inverse(globalCameraRotation); 
            dvec3 rotationDiffVec3 = centerToCamera * rotationDiffWorldSpace - centerToCamera;

            camPos += rotationDiffVec3;
            dvec3 centerToCamera = camPos - centerPos;
            directionToCenter = normalize(-centerToCamera);

            dvec3 lookUpWhenFacingCenter =
                globalCameraRotation * dvec3(camera.lookUpVectorCameraSpace());
            dmat4 lookAtMat = lookAt(
                dvec3(0, 0, 0),
                directionToCenter,
                lookUpWhenFacingCenter);
            globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
        }
        { // Move position towards or away from focus node
            centerToBoundingSphere =
                -directionToCenter *
                boundingSphere;
            camPos += -(centerToCamera - centerToBoundingSphere) *
                _mouseStates->synchedTruckMovementMouseVelocity().y;
        }
        { // Roll around sphere normal
            dquat cameraRollRotation =
                angleAxis(_mouseStates->synchedGlobalRollMouseVelocity().x, -directionToCenter);
            globalCameraRotation = cameraRollRotation * globalCameraRotation;
        }
        { // Push up to surface
            dvec3 sphereSurfaceToCamera = camPos - (centerPos + centerToBoundingSphere);

            double distFromSphereSurfaceToCamera = length(sphereSurfaceToCamera);
            camPos += -directionToCenter *
                max(minHeightAboveBoundingSphere - distFromSphereSurfaceToCamera, 0.0);
        }
      
        // Update the camera state
        camera.setPositionVec3(camPos);
        camera.setRotation(globalCameraRotation * localCameraRotation);
    }
}

bool OrbitalInteractionMode::followingNodeRotation() const {
    return false;
}

void OrbitalInteractionMode::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {
    _mouseStates->updateMouseStatesFromInput(inputState, deltaTime);
}

GlobeBrowsingInteractionMode::GlobeBrowsingInteractionMode(std::shared_ptr<MouseStates> mouseStates)
    : OrbitalInteractionMode(mouseStates) 
{
     
}

GlobeBrowsingInteractionMode::~GlobeBrowsingInteractionMode() {

}

void GlobeBrowsingInteractionMode::setFocusNode(SceneGraphNode* focusNode) {
    InteractionMode::setFocusNode(focusNode);

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    Renderable* baseRenderable = _focusNode->renderable();
    if (globebrowsing::RenderableGlobe* globe = dynamic_cast<globebrowsing::RenderableGlobe*>(baseRenderable)) {
        _globe = globe;
    }
    else {
        LWARNING("Focus node is not a renderable globe. GlobeBrowsingInteraction is not possible");
        _globe = nullptr;
    }
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
}

void GlobeBrowsingInteractionMode::updateCameraStateFromMouseStates(Camera& camera, double deltaTime) {
    
#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
    using namespace glm;
    if (_focusNode && _globe) {
        // Declare variables to use in interaction calculations
        // Shrink interaction ellipsoid to enable interaction below height = 0
        double ellipsoidShrinkTerm = _globe->interactionDepthBelowEllipsoid();
        double minHeightAboveGround = _globe->generalProperties().cameraMinHeight;

        // Read the current state of the camera and focusnode
        dvec3 camPos = camera.positionVec3();
        dvec3 centerPos = _focusNode->worldPosition();

        // Follow focus nodes movement
        dvec3 focusNodeDiff = centerPos - _previousFocusNodePosition;
        _previousFocusNodePosition = centerPos;
        camPos += focusNodeDiff;

        dquat totalRotation = camera.rotationQuaternion();
        dvec3 lookUp = camera.lookUpVectorWorldSpace();
        dvec3 camDirection = camera.viewDirectionWorldSpace();

        // Sampling of height is done in the reference frame of the globe.
        // Hence, the camera position needs to be transformed with the inverse model matrix
        glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
        glm::dmat4 modelTransform = _globe->modelTransform();
        glm::dvec3 cameraPositionModelSpace =
            glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

        dvec3 posDiff = camPos - centerPos;

        dvec3 directionFromSurfaceToCameraModelSpace =
           _globe->ellipsoid().geodeticSurfaceNormal(
                _globe->ellipsoid().cartesianToGeodetic2(cameraPositionModelSpace));
        dvec3 directionFromSurfaceToCamera =
            normalize(dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);
        dvec3 centerToEllipsoidSurface = dmat3(modelTransform)  * (_globe->projectOnEllipsoid(cameraPositionModelSpace) -
            directionFromSurfaceToCameraModelSpace * ellipsoidShrinkTerm);
        dvec3 ellipsoidSurfaceToCamera = camPos - (centerPos + centerToEllipsoidSurface);

        double heightToSurface =
            _globe->getHeight(cameraPositionModelSpace) + ellipsoidShrinkTerm;
        
        double distFromCenterToSurface =
            length(centerToEllipsoidSurface);
        double distFromEllipsoidSurfaceToCamera = length(ellipsoidSurfaceToCamera);
        double distFromCenterToCamera = length(posDiff);
        double distFromSurfaceToCamera =
            distFromEllipsoidSurfaceToCamera - heightToSurface;

        // Create the internal representation of the local and global camera rotations
        dmat4 lookAtMat = lookAt(
            dvec3(0.0, 0.0, 0.0),
            -directionFromSurfaceToCamera,
            normalize(camDirection + lookUp)); // To avoid problem with lookup in up direction
        dquat globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
        dquat localCameraRotation = inverse(globalCameraRotation) * totalRotation;

        // Rotate with the globe
        dmat3 globeStateMatrix = _focusNode->worldRotationMatrix();
        dquat globeRotation = quat_cast(globeStateMatrix);
        dquat focusNodeRotationDiff = _previousFocusNodeRotation * inverse(globeRotation);
        _previousFocusNodeRotation = globeRotation;

        { // Do local roll
            glm::dquat cameraRollRotation =
                glm::angleAxis(_mouseStates->synchedLocalRollMouseVelocity().x, dvec3(0, 0, 1));
            localCameraRotation = localCameraRotation * cameraRollRotation;
        }
        if(!_rotateToFocusNodeInterpolator.isInterpolating())
        { // Do local rotation
            glm::dvec3 eulerAngles(_mouseStates->synchedLocalRotationMouseVelocity().y, _mouseStates->synchedLocalRotationMouseVelocity().x, 0);
            glm::dquat rotationDiff = glm::dquat(eulerAngles);

            localCameraRotation = localCameraRotation * rotationDiff;
        }
        else
        { // Interpolate local rotation to focus node
            double t = _rotateToFocusNodeInterpolator.value();
            localCameraRotation = slerp(localCameraRotation, dquat(dvec3(0.0)), t);
            _rotateToFocusNodeInterpolator.step(0.002); // Should probably depend on dt
            // This is a fast but ugly solution to slow regaining of control...
            if (t > 0.2) {
                _rotateToFocusNodeInterpolator.end();
            }
        }
        { // Do global rotation (horizontal movement)
            glm::dvec3 eulerAngles = glm::dvec3(
                -_mouseStates->synchedGlobalRotationMouseVelocity().y,
                -_mouseStates->synchedGlobalRotationMouseVelocity().x,
                0) * glm::clamp(distFromSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0);
            glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);

            glm::dquat rotationDiffWorldSpace =
                globalCameraRotation *
                rotationDiffCamSpace *
                glm::inverse(globalCameraRotation);

            glm::dvec3 rotationDiffVec3 =
                (distFromCenterToCamera * directionFromSurfaceToCamera)
                 * rotationDiffWorldSpace
                - (distFromCenterToCamera * directionFromSurfaceToCamera);

            camPos += rotationDiffVec3;

            posDiff = camPos - centerPos;
            glm::dvec3 rotationDiffVec3AroundCenter =
                posDiff
                * focusNodeRotationDiff
                - (posDiff);
            camPos += rotationDiffVec3AroundCenter;
            
            
            


            cameraPositionModelSpace =
                glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

            directionFromSurfaceToCameraModelSpace =
                _globe->ellipsoid().geodeticSurfaceNormal(
                    _globe->ellipsoid().cartesianToGeodetic2(cameraPositionModelSpace));
            directionFromSurfaceToCamera =
              normalize(dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace);
            centerToEllipsoidSurface = dmat3(modelTransform) * (_globe->projectOnEllipsoid(cameraPositionModelSpace) -
                directionFromSurfaceToCameraModelSpace * ellipsoidShrinkTerm);
            ellipsoidSurfaceToCamera = camPos - (centerPos + centerToEllipsoidSurface);


            glm::dvec3 lookUpWhenFacingSurface =
                inverse(focusNodeRotationDiff) * globalCameraRotation * glm::dvec3(camera.lookUpVectorCameraSpace());
            glm::dmat4 lookAtMat = glm::lookAt(
                glm::dvec3(0, 0, 0),
                -directionFromSurfaceToCamera,
                lookUpWhenFacingSurface);
            globalCameraRotation =
                glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
        }
        { // Move position towards or away from focus node
            camPos += -directionFromSurfaceToCamera * distFromSurfaceToCamera *
                _mouseStates->synchedTruckMovementMouseVelocity().y;
        }
        { // Roll around ellipsoid normal
            glm::dquat cameraRollRotation =
                glm::angleAxis(_mouseStates->synchedGlobalRollMouseVelocity().x, directionFromSurfaceToCamera);
            globalCameraRotation = cameraRollRotation * globalCameraRotation;
        }
        { // Push up to surface
            ellipsoidSurfaceToCamera = camPos - (centerPos + centerToEllipsoidSurface);

            // Sampling of height is done in the reference frame of the globe.
            // Hence, the camera position needs to be transformed with the inverse model matrix
            glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
            glm::dvec3 cameraPositionModelSpace =
                glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

            distFromEllipsoidSurfaceToCamera = glm::length(ellipsoidSurfaceToCamera);
            double heightToSurface =
                _globe->getHeight(cameraPositionModelSpace) + ellipsoidShrinkTerm;
            double heightToSurfaceAndPadding = heightToSurface + minHeightAboveGround;
            camPos += directionFromSurfaceToCamera *
                glm::max(heightToSurfaceAndPadding - distFromEllipsoidSurfaceToCamera, 0.0);
        }
        
        // Update the camera state
        camera.setPositionVec3(camPos); 
        camera.setRotation(globalCameraRotation * localCameraRotation);
    }
#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
}

bool GlobeBrowsingInteractionMode::followingNodeRotation() const {
    return true;
}

#ifdef OPENSPACE_MODULE_GLOBEBROWSING_ENABLED
void GlobeBrowsingInteractionMode::goToChunk(Camera& camera,
    globebrowsing::TileIndex ti, glm::vec2 uv, bool resetCameraDirection) {
    using namespace globebrowsing;
    
    // Camera position in model space
    glm::dvec3 camPos = camera.positionVec3();
    glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
    glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));
    
    GeodeticPatch patch(ti);
    Geodetic2 corner = patch.getCorner(SOUTH_WEST);
    Geodetic2 positionOnPatch = patch.getSize();
    positionOnPatch.lat *= uv.y;
    positionOnPatch.lon *= uv.x;
    Geodetic2 pointPosition = corner + positionOnPatch;
    
    glm::dvec3 positionOnEllipsoid =
        _globe->ellipsoid().geodeticSurfaceProjection(cameraPositionModelSpace);
    double altitude = glm::length(cameraPositionModelSpace - positionOnEllipsoid);
    
    goToGeodetic3(camera, {pointPosition, altitude});
    
    if (resetCameraDirection) {
        this->resetCameraDirection(camera, pointPosition);
    }
}

void GlobeBrowsingInteractionMode::goToGeodetic2(Camera& camera,
    globebrowsing::Geodetic2 geo2, bool resetCameraDirection) {
    using namespace globebrowsing;
    
    // Camera position in model space
    glm::dvec3 camPos = camera.positionVec3();
    glm::dmat4 inverseModelTransform = _globe->inverseModelTransform();
    glm::dvec3 cameraPositionModelSpace =
    glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));
        
    glm::dvec3 positionOnEllipsoid =
    _globe->ellipsoid().geodeticSurfaceProjection(cameraPositionModelSpace);
    double altitude = glm::length(cameraPositionModelSpace - positionOnEllipsoid);
        
    goToGeodetic3(camera, {geo2, altitude});
        
    if (resetCameraDirection) {
        this->resetCameraDirection(camera, geo2);
    }
}
    
void GlobeBrowsingInteractionMode::goToGeodetic3(Camera& camera, globebrowsing::Geodetic3 geo3) {
    glm::dvec3 positionModelSpace = _globe->ellipsoid().cartesianPosition(geo3);
    glm::dmat4 modelTransform = _globe->modelTransform();
    glm::dvec3 positionWorldSpace = modelTransform * glm::dvec4(positionModelSpace, 1.0);
    camera.setPositionVec3(positionWorldSpace);
}
    
    void GlobeBrowsingInteractionMode::resetCameraDirection(Camera& camera,  globebrowsing::Geodetic2 geo2) {
        using namespace globebrowsing;
        
        // Camera is described in world space
        glm::dmat4 modelTransform = _globe->modelTransform();
        
        // Lookup vector
        glm::dvec3 positionModelSpace = _globe->ellipsoid().cartesianSurfacePosition(geo2);
        glm::dvec3 slightlyNorth = _globe->ellipsoid().cartesianSurfacePosition(
            Geodetic2(geo2.lat + 0.001, geo2.lon));
        glm::dvec3 lookUpModelSpace = glm::normalize(slightlyNorth - positionModelSpace);
        glm::dvec3 lookUpWorldSpace = glm::dmat3(modelTransform) * lookUpModelSpace;
        
        // Lookat vector
        glm::dvec3 lookAtWorldSpace = modelTransform * glm::dvec4(positionModelSpace, 1.0);

        // Eye position
        glm::dvec3 eye = camera.positionVec3();
        
        // Matrix
        glm::dmat4 lookAtMatrix = glm::lookAt(
                        eye, lookAtWorldSpace, lookUpWorldSpace);
        
        // Set rotation
        glm::dquat rotation = glm::quat_cast(inverse(lookAtMatrix));
        camera.setRotation(rotation);
    }

#endif // OPENSPACE_MODULE_GLOBEBROWSING_ENABLED

} // namespace interaction
} // namespace openspace
