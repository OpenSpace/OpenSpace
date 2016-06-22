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

#include <openspace/interaction/interactionmode.h>
#include <openspace/interaction/interactionhandler.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/logging/logmanager.h>

#include <glm/gtx/quaternion.hpp>



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

    void InputState::addKeyframe(const network::datamessagestructures::PositionKeyframe &kf) {
        _keyframeMutex.lock();

        //save a maximum of 10 samples (1 seconds of buffer)
        if (_keyframes.size() >= 10) {
            _keyframes.erase(_keyframes.begin());
        }
        _keyframes.push_back(kf);

        _keyframeMutex.unlock();
    }

    void InputState::clearKeyframes() {
        _keyframeMutex.lock();
        _keyframes.clear();
        _keyframeMutex.unlock();
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

    bool InputState::isMouseButtonPressed(MouseButton mouseButton) const {
        for (auto it = _mouseButtonsDown.begin(); it != _mouseButtonsDown.end(); it++) {
            if (*it == mouseButton) {
                return true;
            }
        }
        return false;
    }






InteractionMode::InteractionMode() {

}


InteractionMode::~InteractionMode() {

}

void InteractionMode::setFocusNode(SceneGraphNode* focusNode) {
    _focusNode = focusNode;
}

SceneGraphNode* InteractionMode::focusNode() {
    return _focusNode;
}

// KeyframeInteractionMode
KeyframeInteractionMode::KeyframeInteractionMode(){

}

KeyframeInteractionMode::~KeyframeInteractionMode() {

}

void KeyframeInteractionMode::update(double deltaTime) {
    // TODO : Get keyframes from input state and use them to position and rotate the camera
}
void KeyframeInteractionMode::initialize(const Camera& camera) {
    LWARNING("KeyframeInteractionMode::initialize not implemented yet");
}


// OrbitalInteractionMode
OrbitalInteractionMode::OrbitalInteractionMode(double sensitivity, double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationMouseState(velocityScaleFactor)
    , _localRotationMouseState(velocityScaleFactor)
    , _truckMovementMouseState(velocityScaleFactor)
    , _rollMouseState(velocityScaleFactor) {

}

OrbitalInteractionMode::~OrbitalInteractionMode() {

}

void OrbitalInteractionMode::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {
    glm::dvec2 mousePosition = inputState.getMousePosition();

    bool button1Pressed = inputState.isMouseButtonPressed(MouseButton::Button1);
    bool button2Pressed = inputState.isMouseButtonPressed(MouseButton::Button2);
    bool button3Pressed = inputState.isMouseButtonPressed(MouseButton::Button3);
    bool keyCtrlPressed = inputState.isKeyPressed(
        std::pair<Key, KeyModifier>(Key::LeftControl, KeyModifier::Control));

    // Update the mouse states
    if (button1Pressed) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta =
                _localRotationMouseState.previousPosition - mousePosition;
            _localRotationMouseState.velocity.set(mousePositionDelta * deltaTime * _sensitivity, deltaTime);

            _globalRotationMouseState.previousPosition = mousePosition;
            _globalRotationMouseState.velocity.set(glm::dvec2(0, 0), deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta =
                _globalRotationMouseState.previousPosition - mousePosition;
            _globalRotationMouseState.velocity.set(mousePositionDelta * deltaTime * _sensitivity, deltaTime);

            _localRotationMouseState.previousPosition = mousePosition;
            _localRotationMouseState.velocity.set(glm::dvec2(0, 0), deltaTime);
        }
    }
    else { // !button1Pressed
        _localRotationMouseState.previousPosition = mousePosition;
        _localRotationMouseState.velocity.set(glm::dvec2(0, 0), deltaTime);

        _globalRotationMouseState.previousPosition = mousePosition;
        _globalRotationMouseState.velocity.set(glm::dvec2(0, 0), deltaTime);
    }
    if (button2Pressed) {
        glm::dvec2 mousePositionDelta =
            _truckMovementMouseState.previousPosition - mousePosition;
        _truckMovementMouseState.velocity.set(mousePositionDelta * deltaTime * _sensitivity, deltaTime);
    }
    else { // !button2Pressed
        _truckMovementMouseState.previousPosition = mousePosition;
        _truckMovementMouseState.velocity.set(glm::dvec2(0, 0), deltaTime);
    }
    if (button3Pressed) {
        glm::dvec2 mousePositionDelta =
            _rollMouseState.previousPosition - mousePosition;
        _rollMouseState.velocity.set(mousePositionDelta * deltaTime * _sensitivity, deltaTime);
    }
    else { // !button3Pressed
        _rollMouseState.previousPosition = mousePosition;
        _rollMouseState.velocity.set(glm::dvec2(0, 0), deltaTime);
    }
}

void OrbitalInteractionMode::updateCameraStateFromMouseStates(Camera& camera) {
    using namespace glm;
    if (_focusNode) {
        // Declare variables to use in interaction calculations
        dvec3 centerPos = _focusNode->worldPosition().dvec3();
        dvec3 camPos = camera.positionVec3();
        dvec3 posDiff = camPos - centerPos;
        dvec3 newPosition = camPos;
        
        { // Do local rotation
            dvec3 eulerAngles(_localRotationMouseState.velocity.get().y, 0, 0);
            dquat rotationDiff = dquat(eulerAngles);

            _localCameraRotation = _localCameraRotation * rotationDiff;
        }
        { // Do global rotation
            dvec2 smoothMouseVelocity = _globalRotationMouseState.velocity.get();
            dvec3 eulerAngles(smoothMouseVelocity.x, smoothMouseVelocity.y, 0);
            dquat rotationDiffCamSpace = dquat(eulerAngles);

            dquat newRotationCamspace = _globalCameraRotation * rotationDiffCamSpace;
            dquat rotationDiffWorldSpace = newRotationCamspace * inverse(_globalCameraRotation); 
            dvec3 rotationDiffVec3 = posDiff * rotationDiffWorldSpace - posDiff;

            newPosition = camPos + rotationDiffVec3;

            dvec3 directionToCenter = normalize(centerPos - newPosition);

            dvec3 lookUpWhenFacingCenter =
                _globalCameraRotation * dvec3(camera.lookUpVectorCameraSpace());
            dmat4 lookAtMat = lookAt(
                dvec3(0, 0, 0),
                directionToCenter,
                lookUpWhenFacingCenter);
            _globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
        }
        { // Move position towards or away from focus node
            double boundingSphere = _focusNode->boundingSphere().lengthf();
            dvec3 centerToBoundingSphere =
                normalize(posDiff) *
                static_cast<double>(_focusNode->boundingSphere().lengthf());
            newPosition += -(posDiff - centerToBoundingSphere) *
                _truckMovementMouseState.velocity.get().y;
        }
        { // Do roll
            dquat cameraRollRotation =
                angleAxis(_rollMouseState.velocity.get().x, normalize(posDiff));
            _globalCameraRotation = cameraRollRotation * _globalCameraRotation;
        }

        // Update the camera state
        camera.setRotation(_globalCameraRotation * _localCameraRotation);
        camera.setPositionVec3(newPosition);
    }
}

void OrbitalInteractionMode::update(Camera& camera, const InputState& inputState, double deltaTime) {
    updateMouseStatesFromInput(inputState, deltaTime);
    updateCameraStateFromMouseStates(camera);
}

void OrbitalInteractionMode::initialize(const Camera& camera) {
    LWARNING("Interaction has not implemented sync to camera. I.e. interaction \
        mode may contain staate that is not transferred when reading camera state from disk");

    // @Todo : extract _localCameraRotation and _globalCameraRotation from camera state

}

GlobeBrowsingInteractionMode::GlobeBrowsingInteractionMode(double sensitivity, double velocityScaleFactor)
    : OrbitalInteractionMode(sensitivity, velocityScaleFactor) 
{

}

GlobeBrowsingInteractionMode::~GlobeBrowsingInteractionMode() {

}

void GlobeBrowsingInteractionMode::setFocusNode(SceneGraphNode* focusNode) {
    _focusNode = focusNode;

    Renderable* baseRenderable = _focusNode->renderable();
    if (RenderableGlobe* globe = dynamic_cast<RenderableGlobe*>(baseRenderable)) {
        _globe = globe;
    }
    else {
        LWARNING("Focus node is not a renderable globe. GlobeBrowsingInteraction is not possible");
        _globe = nullptr;
    }

}


void GlobeBrowsingInteractionMode::updateCameraStateFromMouseStates(Camera& camera) {
    using namespace glm;

    if (_focusNode && _globe) {
        // Declare variables to use in interaction calculations
        dvec3 centerPos = _focusNode->worldPosition().dvec3();
        dvec3 camPos = camera.positionVec3();
        dvec3 posDiff = camPos - centerPos;
        dvec3 newPosition = camPos;

        dvec3 centerToSurface = _globe->geodeticSurfaceProjection(camPos);
        dvec3 surfaceToCamera = camPos - (centerPos + centerToSurface);
        dvec3 directionFromSurfaceToCamera = normalize(surfaceToCamera);
        double distFromCenterToSurface = length(centerToSurface);
        double distFromSurfaceToCamera = length(surfaceToCamera);
        double distFromCenterToCamera = length(posDiff);

        { // Do local rotation
            dvec3 eulerAngles(_localRotationMouseState.velocity.get().y, 0, 0);
            dquat rotationDiff = dquat(eulerAngles);

            _localCameraRotation = _localCameraRotation * rotationDiff;
        }
        { // Do global rotation
            dvec2 smoothMouseVel = _globalRotationMouseState.velocity.get();
            dvec3 eulerAngles(smoothMouseVel.y, -smoothMouseVel.x, 0);
            dquat rotationDiffCamSpace = dquat(eulerAngles);


            dquat rotationDiffWorldSpace = _globalCameraRotation * rotationDiffCamSpace 
                * inverse(_globalCameraRotation);

            dvec3 rotationDiffVec3 = (distFromCenterToCamera * directionFromSurfaceToCamera)
                * rotationDiffWorldSpace - (distFromCenterToCamera * directionFromSurfaceToCamera);
            rotationDiffVec3 *= clamp(distFromSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0);

            newPosition = camPos + rotationDiffVec3;

            dvec3 newCenterToSurface = _globe->geodeticSurfaceProjection(newPosition);
            dvec3 newSurfaceToCamera = newPosition - (centerPos + newCenterToSurface);
            dvec3 newDirectionFromSurfaceToCamera = normalize(newSurfaceToCamera);

            dvec3 lookUpWhenFacingSurface = _globalCameraRotation * dvec3(camera.lookUpVectorCameraSpace());
            dmat4 lookAtMat = lookAt(dvec3(0, 0, 0), -newDirectionFromSurfaceToCamera, lookUpWhenFacingSurface);
            _globalCameraRotation = normalize(quat_cast(inverse(lookAtMat)));
        }
        { // Move position towards or away from focus node
            newPosition += -(posDiff - centerToSurface) * _truckMovementMouseState.velocity.get().y;
        }
        { // Do roll
            dquat cameraRollRotation =
                angleAxis(_rollMouseState.velocity.get().x, directionFromSurfaceToCamera);
            _globalCameraRotation = cameraRollRotation * _globalCameraRotation;
        }

        // Update the camera state
        camera.setRotation(_globalCameraRotation * _localCameraRotation);
        camera.setPositionVec3(newPosition);
    }
}

void GlobeBrowsingInteractionMode::update(Camera& camera, const InputState& inputState, double deltaTime) {
    updateMouseStatesFromInput(inputState, deltaTime);
    updateCameraStateFromMouseStates(camera);
}


} // namespace interaction
} // namespace openspace
