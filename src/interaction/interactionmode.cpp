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

#include <modules/globebrowsing/globes/renderableglobe.h>


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

    if (_focusNode != nullptr) {
        _previousFocusNodePosition = _focusNode->worldPosition();
        _previousFocusNodeRotation = glm::quat_cast(_focusNode->worldRotationMatrix());
    }
}

SceneGraphNode* InteractionMode::focusNode() {
    return _focusNode;
}

// KeyframeInteractionMode
KeyframeInteractionMode::KeyframeInteractionMode(){

}

KeyframeInteractionMode::~KeyframeInteractionMode() {

}

void KeyframeInteractionMode::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {

}

void KeyframeInteractionMode::updateCameraStateFromMouseStates(Camera& camera) {

}

// OrbitalInteractionMode
OrbitalInteractionMode::MouseStates::MouseStates(double sensitivity, double velocityScaleFactor)
    : _sensitivity(sensitivity)
    , _globalRotationMouseState(velocityScaleFactor)
    , _localRotationMouseState(velocityScaleFactor)
    , _truckMovementMouseState(velocityScaleFactor)
    , _globalRollMouseState(velocityScaleFactor)
    , _localRollMouseState(velocityScaleFactor) {

}

void OrbitalInteractionMode::MouseStates::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {
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
            _globalRotationMouseState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta =
                _globalRotationMouseState.previousPosition - mousePosition;
            _globalRotationMouseState.velocity.set(mousePositionDelta * deltaTime * _sensitivity, deltaTime);

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
        _truckMovementMouseState.velocity.set(mousePositionDelta * deltaTime * _sensitivity, deltaTime);
    }
    else { // !button2Pressed
        _truckMovementMouseState.previousPosition = mousePosition;
        _truckMovementMouseState.velocity.decelerate(deltaTime);
    }
    if (button3Pressed) {
        if (keyCtrlPressed) {
            glm::dvec2 mousePositionDelta =
                _localRollMouseState.previousPosition - mousePosition;
            _localRollMouseState.velocity.set(mousePositionDelta * deltaTime * _sensitivity, deltaTime);

            _globalRollMouseState.previousPosition = mousePosition;
            _globalRollMouseState.velocity.decelerate(deltaTime);
        }
        else {
            glm::dvec2 mousePositionDelta =
                _globalRollMouseState.previousPosition - mousePosition;
            _globalRollMouseState.velocity.set(mousePositionDelta * deltaTime * _sensitivity, deltaTime);

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
    return _synchedGlobalRotationMouseVelocity;
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedLocalRotationMouseVelocity() {
    return _synchedLocalRotationMouseVelocity;
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedTruckMovementMouseVelocity() {
    return _synchedTruckMovementMouseVelocity;
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedLocalRollMouseVelocity() {
    return _synchedLocalRollMouseVelocity;
}

glm::dvec2 OrbitalInteractionMode::MouseStates::synchedGlobalRollMouseVelocity() {
    return _synchedGlobalRollMouseVelocity;
}

void OrbitalInteractionMode::MouseStates::preSynchronization() {
    _sharedGlobalRotationMouseVelocity = _globalRotationMouseState.velocity.get();
    _sharedLocalRotationMouseVelocity = _localRotationMouseState.velocity.get();
    _sharedTruckMovementMouseVelocity = _truckMovementMouseState.velocity.get();
    _sharedLocalRollMouseVelocity = _localRollMouseState.velocity.get();
    _sharedGlobalRollMouseVelocity = _globalRollMouseState.velocity.get();
}

void OrbitalInteractionMode::MouseStates::postSynchronizationPreDraw() {
    _synchedGlobalRotationMouseVelocity = _sharedGlobalRotationMouseVelocity;
    _synchedLocalRotationMouseVelocity = _sharedLocalRotationMouseVelocity;
    _synchedTruckMovementMouseVelocity = _sharedTruckMovementMouseVelocity;
    _synchedLocalRollMouseVelocity = _sharedLocalRollMouseVelocity;
    _synchedGlobalRollMouseVelocity = _sharedGlobalRollMouseVelocity;
}

void OrbitalInteractionMode::MouseStates::serialize(SyncBuffer* syncBuffer) {
    syncBuffer->encode(_sharedGlobalRotationMouseVelocity);
    syncBuffer->encode(_sharedLocalRotationMouseVelocity);
    syncBuffer->encode(_sharedTruckMovementMouseVelocity);
    syncBuffer->encode(_sharedLocalRollMouseVelocity);
    syncBuffer->encode(_sharedGlobalRollMouseVelocity);
}

void OrbitalInteractionMode::MouseStates::deserialize(SyncBuffer* syncBuffer) {
    syncBuffer->decode(_sharedGlobalRotationMouseVelocity);
    syncBuffer->decode(_sharedLocalRotationMouseVelocity);
    syncBuffer->decode(_sharedTruckMovementMouseVelocity);
    syncBuffer->decode(_sharedLocalRollMouseVelocity);
    syncBuffer->decode(_sharedGlobalRollMouseVelocity);
}

OrbitalInteractionMode::OrbitalInteractionMode(std::shared_ptr<MouseStates> mouseStates)
    : InteractionMode()
    , _mouseStates(mouseStates){

}

OrbitalInteractionMode::~OrbitalInteractionMode() {

}

void OrbitalInteractionMode::updateCameraStateFromMouseStates(Camera& camera) {
    // Update synched data
    _mouseStates->postSynchronizationPreDraw();

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
        double boundingSphere = _focusNode->boundingSphere().lengthf();
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
        { // Do local rotation
            dvec3 eulerAngles(_mouseStates->synchedLocalRotationMouseVelocity().y, _mouseStates->synchedLocalRotationMouseVelocity().x, 0);
            dquat rotationDiff = dquat(eulerAngles);

            localCameraRotation = localCameraRotation * rotationDiff;
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

void OrbitalInteractionMode::updateMouseStatesFromInput(const InputState& inputState, double deltaTime) {
    _mouseStates->updateMouseStatesFromInput(inputState, deltaTime);
    _mouseStates->preSynchronization();
}

void OrbitalInteractionMode::serialize(SyncBuffer* syncBuffer) {
    _mouseStates->serialize(syncBuffer);
}

void OrbitalInteractionMode::deserialize(SyncBuffer* syncBuffer) {
    _mouseStates->deserialize(syncBuffer);
}

GlobeBrowsingInteractionMode::GlobeBrowsingInteractionMode(std::shared_ptr<MouseStates> mouseStates)
    : OrbitalInteractionMode(mouseStates) 
{
     
}

GlobeBrowsingInteractionMode::~GlobeBrowsingInteractionMode() {

}

void GlobeBrowsingInteractionMode::setFocusNode(SceneGraphNode* focusNode) {
    InteractionMode::setFocusNode(focusNode);

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
    // Update synched data
    _mouseStates->postSynchronizationPreDraw();

    using namespace glm;
    if (_focusNode && _globe) {
        // Declare variables to use in interaction calculations
        // Shrink interaction ellipsoid to enable interaction below height = 0
        double ellipsoidShrinkTerm = _globe->interactionDepthBelowEllipsoid();
        double minHeightAboveGround = _globe->cameraMinHeight();

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
        glm::dmat4 inverseModelTransform = _globe->chunkedLodGlobe()->inverseModelTransform();
        glm::dmat4 modelTransform = _globe->chunkedLodGlobe()->modelTransform();
        glm::dvec3 cameraPositionModelSpace =
            glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

        dvec3 posDiff = camPos - centerPos;

        dvec3 directionFromSurfaceToCameraModelSpace =
           _globe->ellipsoid().geodeticSurfaceNormal(
                _globe->ellipsoid().cartesianToGeodetic2(cameraPositionModelSpace));
        dvec3 directionFromSurfaceToCamera =
            dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace;
        dvec3 centerToEllipsoidSurface = dmat3(modelTransform)  * (_globe->projectOnEllipsoid(cameraPositionModelSpace) -
            directionFromSurfaceToCameraModelSpace * ellipsoidShrinkTerm);
        dvec3 ellipsoidSurfaceToCamera = camPos - (centerPos + centerToEllipsoidSurface);

        double distFromCenterToSurface =
            length(centerToEllipsoidSurface);
        double distFromEllipsoidSurfaceToCamera = length(ellipsoidSurfaceToCamera);
        double distFromCenterToCamera = length(posDiff);

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
        { // Do local rotation
            glm::dvec3 eulerAngles(_mouseStates->synchedLocalRotationMouseVelocity().y, _mouseStates->synchedLocalRotationMouseVelocity().x, 0);
            glm::dquat rotationDiff = glm::dquat(eulerAngles);

            localCameraRotation = localCameraRotation * rotationDiff;
        }
        { // Do global rotation (horizontal movement)
            glm::dvec3 eulerAngles = glm::dvec3(
                -_mouseStates->synchedGlobalRotationMouseVelocity().y,
                -_mouseStates->synchedGlobalRotationMouseVelocity().x,
                0) * glm::clamp(distFromEllipsoidSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0);
            glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);

            glm::dquat rotationDiffWorldSpace =
                globalCameraRotation *
                rotationDiffCamSpace *
                glm::inverse(globalCameraRotation);

            glm::dvec3 rotationDiffVec3 =
                (distFromCenterToCamera * directionFromSurfaceToCamera)
                 * rotationDiffWorldSpace
                - (distFromCenterToCamera * directionFromSurfaceToCamera);

            glm::dvec3 rotationDiffVec3AroundCenter = 
                posDiff
                 * focusNodeRotationDiff
                - (posDiff);

            camPos += rotationDiffVec3 + rotationDiffVec3AroundCenter;
            dvec3 posDiff = camPos - centerPos;



            cameraPositionModelSpace =
                glm::dvec3(inverseModelTransform * glm::dvec4(camPos, 1));

            directionFromSurfaceToCameraModelSpace =
                _globe->ellipsoid().geodeticSurfaceNormal(
                    _globe->ellipsoid().cartesianToGeodetic2(cameraPositionModelSpace));
            directionFromSurfaceToCamera =
                dmat3(modelTransform) * directionFromSurfaceToCameraModelSpace;
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
            distFromEllipsoidSurfaceToCamera = glm::length(ellipsoidSurfaceToCamera);
            camPos += -directionFromSurfaceToCamera * distFromEllipsoidSurfaceToCamera *
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
            glm::dmat4 inverseModelTransform = _globe->chunkedLodGlobe()->inverseModelTransform();
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
}

} // namespace interaction
} // namespace openspace
