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

#include <openspace/interaction/interactionhandler.h>
//
#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>

#include <glm/gtx/quaternion.hpp>

namespace {
    const std::string _loggerCat = "InteractionHandler";
}

#include "interactionhandler_lua.inl"

namespace openspace {
namespace interaction {

#ifdef USE_OLD_INTERACTIONHANDLER


InteractionHandler::InteractionHandler()
    : properties::PropertyOwner()
    , _camera(nullptr)
    , _focusNode(nullptr)
    , _deltaTime(0.0)
    //, _validKeyLua(false)
    , _controllerSensitivity(1.f)
    , _invertRoll(true)
    , _invertRotation(false)
    , _keyboardController(nullptr)
    , _mouseController(nullptr)
    , _origin("origin", "Origin", "")
    , _coordinateSystem("coordinateSystem", "Coordinate System", "")
    , _currentKeyframeTime(-1.0)
{
    setName("Interaction");
    
    _origin.onChange([this](){
        SceneGraphNode* node = sceneGraphNode(_origin.value());
        if (!node) {
            LWARNING("Could not find a node in scenegraph called '" << _origin.value() <<"'");
            return;
        }
        setFocusNode(node);
    });
    addProperty(_origin);
    
    _coordinateSystem.onChange([this](){
        OsEng.renderEngine().changeViewPoint(_coordinateSystem.value());
    });
    addProperty(_coordinateSystem);
}

InteractionHandler::~InteractionHandler() {
    delete _keyboardController;
    delete _mouseController;
    for (size_t i = 0; i < _controllers.size(); ++i)
        delete _controllers[i];
}

void InteractionHandler::setKeyboardController(KeyboardController* controller) {
    assert(controller);
    delete _keyboardController;
    _keyboardController = controller;
    _keyboardController->setHandler(this);
}

void InteractionHandler::setMouseController(MouseController* controller) {
    assert(controller);
    delete _mouseController;
    _mouseController = controller;
    _mouseController->setHandler(this);
}

void InteractionHandler::addController(Controller* controller) {
    assert(controller);
    _controllers.push_back(controller);
    controller->setHandler(this);
}

void InteractionHandler::lockControls() {
    _mutex.lock();
}

void InteractionHandler::unlockControls() {
    _mutex.unlock();
}

void InteractionHandler::update(double deltaTime) {
    _deltaTime = deltaTime;
    _mouseController->update(deltaTime);
    
    bool hasKeys = false;
    psc pos;
    glm::quat q;
    
    _keyframeMutex.lock();

    if (_keyframes.size() > 4){    //wait until enough samples are buffered
        hasKeys = true;
        
        openspace::network::datamessagestructures::PositionKeyframe p0, p1, p2, p3;
        
        p0 = _keyframes[0];
        p1 = _keyframes[1];
        p2 = _keyframes[2];
        p3 = _keyframes[3];

        //interval check
        if (_currentKeyframeTime < p1._timeStamp){
            _currentKeyframeTime = p1._timeStamp;
        }

        double t0 = p1._timeStamp;
        double t1 = p2._timeStamp;
        double fact = (_currentKeyframeTime - t0) / (t1 - t0);

        
        
        //glm::dvec4 v = positionInterpCR.interpolate(fact, _keyframes[0]._position.dvec4(), _keyframes[1]._position.dvec4(), _keyframes[2]._position.dvec4(), _keyframes[3]._position.dvec4());
        glm::dvec4 v = ghoul::interpolateLinear(fact, p1._position.dvec4(), p2._position.dvec4());
        
        pos = psc(v.x, v.y, v.z, v.w);
        q = ghoul::interpolateLinear(fact, p1._viewRotationQuat, p2._viewRotationQuat);
        
        //we're done with this sample interval
        if (_currentKeyframeTime >= p2._timeStamp){
            _keyframes.erase(_keyframes.begin());
            _currentKeyframeTime = p1._timeStamp;
        }
        
        _currentKeyframeTime += deltaTime;
        
    }
    
    _keyframeMutex.unlock();
    
    if (hasKeys) {
        _camera->setPosition(pos);
        _camera->setRotation(q);
    }

        


    
}

void InteractionHandler::setFocusNode(SceneGraphNode* node) {
    
    if (_focusNode == node){
        return;
    }

    _focusNode = node;

    //orient the camera to the new node
    psc focusPos = node->worldPosition();
    psc camToFocus = focusPos - _camera->position();
    glm::vec3 viewDir = glm::normalize(camToFocus.vec3());
    glm::vec3 cameraView = glm::normalize(_camera->viewDirectionWorldSpace());
    //set new focus position
    _camera->setFocusPosition(node->worldPosition());
    float dot = glm::dot(viewDir, cameraView);

    //static const float Epsilon = 0.001f;
    if (dot < 1.f && dot > -1.f) {
    //if (glm::length(viewDir - cameraView) < 0.001) {
    //if (viewDir != cameraView) {
        glm::vec3 rotAxis = glm::normalize(glm::cross(viewDir, cameraView));
        float angle = glm::angle(viewDir, cameraView);
        glm::quat q = glm::angleAxis(angle, rotAxis);

        //rotate view to target new focus
        _camera->rotate(q);
    }
}

const SceneGraphNode* const InteractionHandler::focusNode() const {
    return _focusNode;
}

void InteractionHandler::setCamera(Camera* camera) {
    assert(camera);
    _camera = camera;
}
const Camera* const InteractionHandler::camera() const {
    return _camera;
}

//void InteractionHandler::keyboardCallback(int key, int action) {
//    if (_keyboardController) {
//        auto start = ghoul::HighResClock::now();
//        _keyboardController->keyPressed(KeyAction(action), Key(key), KeyModifier::None);
//        auto end = ghoul::HighResClock::now();
//        LINFO("Keyboard timing: " << std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count() << "ns");
//    }
//}

void InteractionHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    if (_mouseController)
        _mouseController->button(button, action);
}

void InteractionHandler::mousePositionCallback(double x, double y) {
    if (_mouseController)
        // TODO Remap screen coordinates to [0,1]
        _mouseController->move(static_cast<float>(x), static_cast<float>(y));
}

void InteractionHandler::mouseScrollWheelCallback(double pos) {
    if (_mouseController)
        _mouseController->scrollWheel(static_cast<int>(pos));
}

void InteractionHandler::orbit(const float &dx, const float &dy, const float &dz, const float &dist){

    lockControls();
    
    glm::vec3 cameraUp = glm::normalize((glm::inverse(_camera->viewRotationMatrix()) * glm::vec4(_camera->lookUpVectorCameraSpace(), 0))).xyz();
    glm::vec3 cameraRight = glm::cross(glm::vec3(_camera->viewDirectionWorldSpace()), cameraUp);

    glm::mat4 transform;
    transform = glm::rotate(glm::radians(dx * 100.f), cameraUp) * transform;
    transform = glm::rotate(glm::radians(dy * 100.f), cameraRight) * transform;
    transform = glm::rotate(glm::radians(dz * 100.f), glm::vec3(_camera->viewDirectionWorldSpace())) * transform;

    //get "old" focus position 
        
    psc camPos = _camera->position();
    psc focusPos = _camera->focusPosition();
    float distToFocusNodeCenter = (focusPos - camPos).length().lengthf();
    float focusNodeBounds = _focusNode ? _focusNode->boundingSphere().lengthf() : 0.f;

    float speedFactor = distToFocusNodeCenter - 0.098*focusNodeBounds;
        
    float rotationSpeed = glm::min(0.00001f * speedFactor, 100.0f);
    float zoomSpeed = glm::min(0.0000001f * speedFactor, 1.0f);
    float rollSpeed = 100.0f;
    

	glm::mat4 transform;
	transform = glm::rotate(glm::radians(dx * rotationSpeed), cameraUp) * transform;
	transform = glm::rotate(glm::radians(dy * rotationSpeed), cameraRight) * transform;
	transform = glm::rotate(glm::radians(dz * rollSpeed), _camera->viewDirection()) * transform;

	
	
	
	//// get camera position 
	//psc relative = _camera->position();

	// get camera position (UNSYNCHRONIZED)
	psc relative = _camera->unsynchedPosition();

	//get relative vector
	psc relative_focus_coordinate = relative - focusPos;
	//rotate relative vector
	relative_focus_coordinate = glm::inverse(transform) * relative_focus_coordinate.vec4();
	
	//get new new position of focus node
	psc origin;
	if (_focusNode) {
		origin = _focusNode->worldPosition();
	}

	//new camera position
	relative = origin + relative_focus_coordinate; 	


	psc target = relative + relative_focus_coordinate * dist * zoomSpeed;

	//don't fly into objects
	if ((target - origin).length() < focusNodeBounds){
		//target = relative;
	}

	unlockControls();

    
    _camera->setFocusPosition(origin);
    _camera->setPosition(target);
    _camera->rotate(glm::quat_cast(transform));
    
}

//void InteractionHandler::distance(const float &d){
//
//    lockControls();
//
//    psc relative = _camera->position();
//    const psc origin = (_focusNode) ? _focusNode->worldPosition() : psc();
//    psc relative_origin_coordinate = relative - origin;
//    // addition 100% of bounds (fix later to something node specific?)
//    float bounds = 2.f * (_focusNode ? _focusNode->boundingSphere().lengthf() : 0.f);
//
//    psc target = relative + relative_origin_coordinate * d;// *fmaxf(bounds, (1.f - d));
//    //don't fly into objects
//    if ((target - origin).length() < bounds){
//        target = relative;
//    }
//    _camera->setPosition(target);
//    
//    unlockControls();
//}

void InteractionHandler::orbitDelta(const glm::quat& rotation)
{
    lockControls();

    // the camera position
    psc relative = _camera->position();

    // should be changed to something more dynamic =)
    psc origin;
    if (_focusNode) {
        origin = _focusNode->worldPosition();
    }

    psc relative_origin_coordinate = relative - origin;
    //glm::mat4 rotation_matrix = glm::mat4_cast(glm::inverse(rotation));
    //relative_origin_coordinate = relative_origin_coordinate.vec4() * glm::inverse(rotation);
    relative_origin_coordinate = glm::inverse(rotation) * relative_origin_coordinate.vec4();
    relative = relative_origin_coordinate + origin;
    glm::mat4 la = glm::lookAt(_camera->position().vec3(), origin.vec3(), glm::rotate(rotation, glm::vec3(_camera->lookUpVectorCameraSpace())));
    
    unlockControls();
    
    _camera->setPosition(relative);
    //camera_->rotate(rotation);
    //camera_->setRotation(glm::mat4_cast(rotation));

    
    _camera->setRotation(glm::quat_cast(la));
    //camera_->setLookUpVector();

    
}

//<<<<<<< HEAD
//void InteractionHandler::distance(const PowerScaledScalar &dist, size_t iterations) {
//    if (iterations > 5)
//        return;
//    //assert(this_);
//    lockControls();
//    
//    psc relative = _camera->position();
//    const psc origin = (_node) ? _node->worldPosition() : psc();
//
//    psc relative_origin_coordinate = relative - origin;
//    const glm::vec3 dir(relative_origin_coordinate.direction());
//    glm::vec3 newdir = dir * dist[0];
//=======
void InteractionHandler::rotateDelta(const glm::quat& rotation)
{
    _camera->rotate(rotation);
}

void InteractionHandler::distanceDelta(const PowerScaledScalar& distance, size_t iterations)
{
    if (iterations > 5)
        return;
    //assert(this_);
    lockControls();
        
    psc relative = _camera->position();
    const psc origin = (_focusNode) ? _focusNode->worldPosition() : psc();
    
    unlockControls();

    psc relative_origin_coordinate = relative - origin;
    const glm::vec3 dir(relative_origin_coordinate.direction());
    glm::vec3 newdir = dir * distance[0];

    relative_origin_coordinate = newdir;
    relative_origin_coordinate[3] = distance[1];
    relative = relative + relative_origin_coordinate;

    relative_origin_coordinate = relative - origin;
    if (relative_origin_coordinate.vec4().x == 0.f && relative_origin_coordinate.vec4().y == 0.f && relative_origin_coordinate.vec4().z == 0.f)
        // TODO: this shouldn't be allowed to happen; a mechanism to prevent the camera to coincide with the origin is necessary (ab)
        return;

    newdir = relative_origin_coordinate.direction();

    // update only if on the same side of the origin
    if (glm::angle(newdir, dir) < 90.0f) {
        _camera->setPosition(relative);
    }
    else {
        PowerScaledScalar d2 = distance;
        d2[0] *= 0.75f;
        d2[1] *= 0.85f;
        distanceDelta(d2, iterations + 1);
    }
}

void InteractionHandler::lookAt(const glm::quat& rotation)
{
}

void InteractionHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    // TODO package in script
    const float speed = _controllerSensitivity;
    const float dt = static_cast<float>(_deltaTime);
    if (action == KeyAction::Press || action == KeyAction::Repeat) {
        if ((key == Key::Right) && (modifier == KeyModifier::NoModifier)) {
            glm::vec3 euler(0.0, speed * dt*0.4, 0.0);
            glm::quat rot = glm::quat(euler);
            rotateDelta(rot);
        }
        if ((key == Key::Left) && (modifier == KeyModifier::NoModifier)) {
            glm::vec3 euler(0.0, -speed * dt*0.4, 0.0);
            glm::quat rot = glm::quat(euler);
            rotateDelta(rot);
        }
        if ((key == Key::Down) && (modifier == KeyModifier::NoModifier)) {
            glm::vec3 euler(speed * dt*0.4, 0.0, 0.0);
            glm::quat rot = glm::quat(euler);
            rotateDelta(rot);
        }
        if ((key == Key::Up) && (modifier == KeyModifier::NoModifier)) {
            glm::vec3 euler(-speed * dt*0.4, 0.0, 0.0);
            glm::quat rot = glm::quat(euler);
            rotateDelta(rot);
        }
        if ((key == Key::KeypadSubtract) && (modifier == KeyModifier::NoModifier)) {
            glm::vec2 s = OsEng.renderEngine().camera()->scaling();
            s[1] -= 0.5f;
            OsEng.renderEngine().camera()->setScaling(s);
        }
        if ((key == Key::KeypadAdd) && (modifier == KeyModifier::NoModifier)) {
            glm::vec2 s = OsEng.renderEngine().camera()->scaling();
            s[1] += 0.5f;
            OsEng.renderEngine().camera()->setScaling(s);
        }

        // iterate over key bindings
        //_validKeyLua = true;
        auto ret = _keyLua.equal_range({ key, modifier });
        for (auto it = ret.first; it != ret.second; ++it) {
            //OsEng.scriptEngine()->runScript(it->second);
            OsEng.scriptEngine().queueScript(it->second);
            //if (!_validKeyLua) {
            //    break;
            //}
        }
    }
}

void InteractionHandler::resetKeyBindings() {
    _keyLua.clear();
    //_validKeyLua = false;
}

void InteractionHandler::bindKey(Key key, KeyModifier modifier, std::string lua) {
    _keyLua.insert({
        {key, modifier},
        lua
    });
}

scripting::ScriptEngine::LuaLibrary InteractionHandler::luaLibrary() {
    return {
        "",
        {
            {
                "clearKeys",
                &luascriptfunctions::clearKeys,
                "",
                "Clear all key bindings"
            },
            {
                "bindKey",
                &luascriptfunctions::bindKey,
                "string, string",
                "Binds a key by name to a lua string command"
            },
            {
                "dt",
                &luascriptfunctions::dt,
                "",
                "Get current frame time"
            },
            {
                "distance",
                &luascriptfunctions::distance,
                "number",
                "Change distance to origin",
                true
            },
            {
                "setInteractionSensitivity",
                &luascriptfunctions::setInteractionSensitivity,
                "number",
                "Sets the global interaction sensitivity"
            },
            {
                "interactionSensitivity",
                &luascriptfunctions::interactionSensitivity,
                "",
                "Gets the current global interaction sensitivity"
            },
            {
                "setInvertRoll",
                &luascriptfunctions::setInvertRoll,
                "bool",
                "Sets the setting if roll movements are inverted"
            },
            {
                "invertRoll",
                &luascriptfunctions::invertRoll,
                "",
                "Returns the status of roll movement inversion"
            },
            {
                "setInvertRotation",
                &luascriptfunctions::setInvertRotation,
                "bool",
                "Sets the setting if rotation movements are inverted"
            },
            {
                "invertRotation",
                &luascriptfunctions::invertRotation,
                "",
                "Returns the status of rotation movement inversion"
            }
            
        }
    };
}

void InteractionHandler::setRotation(const glm::quat& rotation)
{
    _camera->setRotation(rotation);
}

double InteractionHandler::deltaTime() const {
    return _deltaTime;
}

void InteractionHandler::setInteractionSensitivity(float sensitivity) {
    _controllerSensitivity = sensitivity;
}

float InteractionHandler::interactionSensitivity() const {
    return _controllerSensitivity;
}

void InteractionHandler::setInvertRoll(bool invert) {
    _invertRoll = invert;
}

bool InteractionHandler::invertRoll() const {
    return _invertRoll;
}

void InteractionHandler::setInvertRotation(bool invert) {
    _invertRotation = invert;
}

bool InteractionHandler::invertRotation() const {
    return _invertRotation;
}

    void InteractionHandler::addKeyframe(const network::datamessagestructures::PositionKeyframe &kf){
    _keyframeMutex.lock();

    //save a maximum of 10 samples (1 seconds of buffer)
    if (_keyframes.size() >= 10){
        _keyframes.erase(_keyframes.begin());
    }
    _keyframes.push_back(kf);

    _keyframeMutex.unlock();
}

void InteractionHandler::clearKeyframes(){
    _keyframeMutex.lock();
    _keyframes.clear();
    _keyframeMutex.unlock();
}

#else // USE_OLD_INTERACTIONHANDLER

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

const std::list<std::pair<Key, KeyModifier> >& InputState::getPressedKeys() {
    return _keysDown;
}

const std::list<MouseButton>& InputState::getPressedMouseButtons() {
    return _mouseButtonsDown;
}

glm::dvec2 InputState::getMousePosition() {
    return _mousePosition;
}

double InputState::getMouseScrollDelta() {
    return _mouseScrollDelta;
}

bool InputState::isKeyPressed(std::pair<Key, KeyModifier> keyModPair) {
    for (auto it = _keysDown.begin(); it != _keysDown.end(); it++) {
        if (*it == keyModPair) {
            return true;
        }
    }
    return false;
}

bool InputState::isMouseButtonPressed(MouseButton mouseButton) {
    for (auto it = _mouseButtonsDown.begin(); it != _mouseButtonsDown.end(); it++) {
        if (*it == mouseButton) {
            return true;
        }
    }
    return false;
}

// InteractionMode
InteractionMode::InteractionMode(std::shared_ptr<InputState> inputState)
    : _inputState(inputState)
    , _focusNode(nullptr)
    , _camera(nullptr) {

}

InteractionMode::~InteractionMode() {

}

void InteractionMode::setFocusNode(SceneGraphNode* focusNode) {
    _focusNode = focusNode;
}

void InteractionMode::setCamera(Camera* camera) {
    _camera = camera;
}

SceneGraphNode* InteractionMode::focusNode() {
    return _focusNode;
}

Camera* InteractionMode::camera() {
    return _camera;
}

// KeyframeInteractionMode
KeyframeInteractionMode::KeyframeInteractionMode(std::shared_ptr<InputState> inputState)
    : InteractionMode(inputState) {

}

KeyframeInteractionMode::~KeyframeInteractionMode() {

}

void KeyframeInteractionMode::update(double deltaTime) {
    // TODO : Get keyframes from input state and use them to position and rotate the camera
}

// OrbitalInteractionMode
OrbitalInteractionMode::OrbitalInteractionMode(
    std::shared_ptr<InputState> inputState,
    double sensitivity,
    double velocityScaleFactor)
    : InteractionMode(inputState)
    , _sensitivity(sensitivity)
    , _globalRotationMouseState(velocityScaleFactor)
    , _localRotationMouseState(velocityScaleFactor)
    , _truckMovementMouseState(velocityScaleFactor)
    , _rollMouseState(velocityScaleFactor) {

}

OrbitalInteractionMode::~OrbitalInteractionMode() {

}

void OrbitalInteractionMode::updateMouseStatesFromInput(double deltaTime) {
    glm::dvec2 mousePosition = _inputState->getMousePosition();

    bool button1Pressed = _inputState->isMouseButtonPressed(MouseButton::Button1);
    bool button2Pressed = _inputState->isMouseButtonPressed(MouseButton::Button2);
    bool button3Pressed = _inputState->isMouseButtonPressed(MouseButton::Button3);
    bool keyCtrlPressed = _inputState->isKeyPressed(
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

void OrbitalInteractionMode::updateCameraStateFromMouseStates() {
    if (_focusNode) {
        // Declare variables to use in interaction calculations
        glm::dvec3 centerPos = _focusNode->worldPosition().dvec3();
        glm::dvec3 camPos = _camera->positionVec3();
        glm::dvec3 posDiff = camPos - centerPos;
        glm::dvec3 newPosition = camPos;

        { // Do local rotation
            glm::dvec3 eulerAngles(_localRotationMouseState.velocity.get().y, 0, 0);
            glm::dquat rotationDiff = glm::dquat(eulerAngles);

            _localCameraRotation = _localCameraRotation * rotationDiff;
        }
        { // Do global rotation
            glm::dvec3 eulerAngles(
                -_globalRotationMouseState.velocity.get().y,
                -_globalRotationMouseState.velocity.get().x,
                0);
            glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);

            glm::dquat newRotationCamspace =
                _globalCameraRotation * rotationDiffCamSpace;
            glm::dquat rotationDiffWorldSpace =
                newRotationCamspace * glm::inverse(_globalCameraRotation);

            glm::dvec3 rotationDiffVec3 = posDiff * rotationDiffWorldSpace - posDiff;

            newPosition = camPos + rotationDiffVec3;

            glm::dvec3 directionToCenter = glm::normalize(centerPos - newPosition);

            glm::dvec3 lookUpWhenFacingCenter =
                _globalCameraRotation * glm::dvec3(_camera->lookUpVectorCameraSpace());
            glm::dmat4 lookAtMat = glm::lookAt(
                glm::dvec3(0, 0, 0),
                directionToCenter,
                lookUpWhenFacingCenter);
            _globalCameraRotation =
                glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
        }
        { // Move position towards or away from focus node
            double boundingSphere = _focusNode->boundingSphere().lengthf();
            glm::dvec3 centerToBoundingSphere =
                glm::normalize(posDiff) *
                static_cast<double>(_focusNode->boundingSphere().lengthf());
            newPosition += -(posDiff - centerToBoundingSphere) *
                _truckMovementMouseState.velocity.get().y;
        }
        { // Do roll
            glm::dquat cameraRollRotation =
                glm::angleAxis(_rollMouseState.velocity.get().x, glm::normalize(posDiff));
            _globalCameraRotation = cameraRollRotation * _globalCameraRotation;
        }

        // Update the camera state
        _camera->setRotation(_globalCameraRotation * _localCameraRotation);
        _camera->setPositionVec3(newPosition);
    }
}

void OrbitalInteractionMode::update(double deltaTime) {
    updateMouseStatesFromInput(deltaTime);
    updateCameraStateFromMouseStates();
}

GlobeBrowsingInteractionMode::GlobeBrowsingInteractionMode(
    std::shared_ptr<InputState> inputState,
    double sensitivity,
    double velocityScaleFactor)
    : OrbitalInteractionMode(inputState, sensitivity, velocityScaleFactor) {

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


void GlobeBrowsingInteractionMode::updateCameraStateFromMouseStates() {
    if (_focusNode && _globe) {
        // Declare variables to use in interaction calculations
        glm::dvec3 centerPos = _focusNode->worldPosition().dvec3();
        glm::dvec3 camPos = _camera->positionVec3();
        glm::dvec3 posDiff = camPos - centerPos;
        glm::dvec3 newPosition = camPos;

        glm::dvec3 centerToSurface = _globe->geodeticSurfaceProjection(camPos);
        glm::dvec3 surfaceToCamera = camPos - (centerPos + centerToSurface);
        glm::dvec3 directionFromSurfaceToCamera = glm::normalize(surfaceToCamera);
        double distFromCenterToSurface =
            glm::length(centerToSurface);
        double distFromSurfaceToCamera = glm::length(surfaceToCamera);
        double distFromCenterToCamera = glm::length(posDiff);

        { // Do local rotation
            glm::dvec3 eulerAngles(_localRotationMouseState.velocity.get().y, 0, 0);
            glm::dquat rotationDiff = glm::dquat(eulerAngles);

            _localCameraRotation = _localCameraRotation * rotationDiff;
        }
        { // Do global rotation
            glm::dvec3 eulerAngles(
                -_globalRotationMouseState.velocity.get().y,
                -_globalRotationMouseState.velocity.get().x,
                0);
            glm::dquat rotationDiffCamSpace = glm::dquat(eulerAngles);


            glm::dquat rotationDiffWorldSpace =
                _globalCameraRotation *
                rotationDiffCamSpace *
                glm::inverse(_globalCameraRotation);

            glm::dvec3 rotationDiffVec3 =
                (distFromCenterToCamera * directionFromSurfaceToCamera)
                * rotationDiffWorldSpace
                - (distFromCenterToCamera * directionFromSurfaceToCamera);
            rotationDiffVec3 *= glm::clamp(distFromSurfaceToCamera / distFromCenterToSurface, 0.0, 1.0);

            newPosition = camPos + rotationDiffVec3;

            glm::dvec3 newCenterToSurface = _globe->geodeticSurfaceProjection(newPosition);
            glm::dvec3 newSurfaceToCamera = newPosition - (centerPos + newCenterToSurface);
            glm::dvec3 newDirectionFromSurfaceToCamera = glm::normalize(newSurfaceToCamera);

            glm::dvec3 lookUpWhenFacingSurface =
                _globalCameraRotation * glm::dvec3(_camera->lookUpVectorCameraSpace());
            glm::dmat4 lookAtMat = glm::lookAt(
                glm::dvec3(0, 0, 0),
                -newDirectionFromSurfaceToCamera,
                lookUpWhenFacingSurface);
            _globalCameraRotation =
                glm::normalize(glm::quat_cast(glm::inverse(lookAtMat)));
        }
        { // Move position towards or away from focus node
            newPosition += -(posDiff - centerToSurface) *
                _truckMovementMouseState.velocity.get().y;
        }
        { // Do roll
            glm::dquat cameraRollRotation =
                glm::angleAxis(_rollMouseState.velocity.get().x, directionFromSurfaceToCamera);
            _globalCameraRotation = cameraRollRotation * _globalCameraRotation;
        }

        // Update the camera state
        _camera->setRotation(_globalCameraRotation * _localCameraRotation);
        _camera->setPositionVec3(newPosition);
    }
}

void GlobeBrowsingInteractionMode::update(double deltaTime) {
    updateMouseStatesFromInput(deltaTime);
    updateCameraStateFromMouseStates();
}

// InteractionHandler
InteractionHandler::InteractionHandler()
    : _origin("origin", "Origin", "")
    , _coordinateSystem("coordinateSystem", "Coordinate System", "") {
    setName("Interaction");

    _origin.onChange([this]() {
        SceneGraphNode* node = sceneGraphNode(_origin.value());
        if (!node) {
            LWARNING("Could not find a node in scenegraph called '" << _origin.value() << "'");
            return;
        }
        setFocusNode(node);
    });
    addProperty(_origin);

    _coordinateSystem.onChange([this]() {
        OsEng.renderEngine().changeViewPoint(_coordinateSystem.value());
    });
    addProperty(_coordinateSystem);

    // Create the interactionModes
    _inputState = std::shared_ptr<InputState>(new InputState());
    _orbitalInteractionMode = std::shared_ptr<OrbitalInteractionMode>(
        new OrbitalInteractionMode(_inputState, 0.002, 1));
    _globebrowsingInteractionMode = std::shared_ptr<GlobeBrowsingInteractionMode>(
        new GlobeBrowsingInteractionMode(_inputState, 0.002, 1));

    // Set the interactionMode
    _currentInteractionMode = _orbitalInteractionMode;
}

InteractionHandler::~InteractionHandler() {

}

void InteractionHandler::setFocusNode(SceneGraphNode* node) {
    _currentInteractionMode->setFocusNode(node);
}

void InteractionHandler::setCamera(Camera* camera) {
    _currentInteractionMode->setCamera(camera);
}

void InteractionHandler::setInteractionMode(std::shared_ptr<InteractionMode> interactionMode) {
    // Camera and focus node is passed over from the previous interaction mode
    Camera* camera = _currentInteractionMode->camera();
    SceneGraphNode* focusNode = _currentInteractionMode->focusNode();

    // Set the interaction mode
    _currentInteractionMode = interactionMode;

    // Update the camera and focusnode for the new interaction mode
    _currentInteractionMode->setCamera(camera);
    _currentInteractionMode->setFocusNode(focusNode);
}

void InteractionHandler::setInteractionModeToOrbital() {
    setInteractionMode(_orbitalInteractionMode);
}

void InteractionHandler::setInteractionModeToGlobeBrowsing() {
    setInteractionMode(_globebrowsingInteractionMode);
}

void InteractionHandler::lockControls() {

}

void InteractionHandler::unlockControls() {

}

void InteractionHandler::update(double deltaTime) {
    _currentInteractionMode->update(deltaTime);
}

SceneGraphNode* const InteractionHandler::focusNode() const {
    return _currentInteractionMode->focusNode();
}

Camera* const InteractionHandler::camera() const {
    return _currentInteractionMode->camera();
}

std::shared_ptr<InputState> InteractionHandler::inputState() const {
    return _inputState;
}


void InteractionHandler::mouseButtonCallback(MouseButton button, MouseAction action) {
    _inputState->mouseButtonCallback(button, action);
}

void InteractionHandler::mousePositionCallback(double x, double y) {
    _inputState->mousePositionCallback(x, y);
}

void InteractionHandler::mouseScrollWheelCallback(double pos) {
    _inputState->mouseScrollWheelCallback(pos);
}

void InteractionHandler::keyboardCallback(Key key, KeyModifier modifier, KeyAction action) {
    _inputState->keyboardCallback(key, modifier, action);

    if (action == KeyAction::Press || action == KeyAction::Repeat) {
        // iterate over key bindings
        auto ret = _keyLua.equal_range({ key, modifier });
        for (auto it = ret.first; it != ret.second; ++it) {
            //OsEng.scriptEngine()->runScript(it->second);
            OsEng.scriptEngine().queueScript(it->second);
        }
    }
}

void InteractionHandler::resetKeyBindings() {
    _keyLua.clear();
}

void InteractionHandler::bindKey(Key key, KeyModifier modifier, std::string lua) {
    _keyLua.insert({
        { key, modifier },
        lua
    });
}

scripting::ScriptEngine::LuaLibrary InteractionHandler::luaLibrary() {
    return{
        "",
        {
            {
                "clearKeys",
                &luascriptfunctions::clearKeys,
                "",
                "Clear all key bindings"
            },
            {
                "bindKey",
                &luascriptfunctions::bindKey,
                "string, string",
                "Binds a key by name to a lua string command"
            },
            {
                "setInteractionMode",
                &luascriptfunctions::setInteractionMode,
                "string",
                "Set the interaction mode for the camera"
            }
        }
    };
}

void InteractionHandler::addKeyframe(const network::datamessagestructures::PositionKeyframe &kf) {
    _inputState->addKeyframe(kf);
}

void InteractionHandler::clearKeyframes() {
    _inputState->clearKeyframes();
}

#endif // USE_OLD_INTERACTIONHANDLER

} // namespace interaction
} // namespace openspace
