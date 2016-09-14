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

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util/keys.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/interpolator.h>

#include <glm/gtx/quaternion.hpp>

//#include <modules/globebrowsing/globes/renderableglobe.h>

#include <ghoul/glm.h>

#include <fstream>

namespace {
    const std::string _loggerCat = "InteractionHandler";

    const std::string KeyFocus = "Focus";
    const std::string KeyPosition = "Position";
    const std::string KeyRotation = "Rotation";
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

scripting::LuaLibrary InteractionHandler::luaLibrary() {
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

// InteractionHandler
InteractionHandler::InteractionHandler()
    : _origin("origin", "Origin", "")
    , _coordinateSystem("coordinateSystem", "Coordinate System", "")
    , _rotationalFriction("rotationalFriction", "Rotational Friction", true)
    , _horizontalFriction("horizontalFriction", "Horizontal Friction", true)
    , _verticalFriction("verticalFriction", "Vertical Friction", true)
    , _sensitivity("sensitivity", "Sensitivity", 0.002, 0.0001, 0.02)
    , _rapidness("rapidness", "Rapidness", 1, 0.1, 60)
{
    setName("Interaction");

    _origin.onChange([this]() {
        SceneGraphNode* node = sceneGraphNode(_origin.value());
        if (!node) {
            LWARNING("Could not find a node in scenegraph called '" << _origin.value() << "'");
            return;
        }
        setFocusNode(node);
        resetCameraDirection();
    });

    _coordinateSystem.onChange([this]() {
        OsEng.renderEngine().changeViewPoint(_coordinateSystem.value());
    });

    // Create the interactionModes
    _inputState = std::make_unique<InputState>();
    // Inject the same mouse states to both orbital and global interaction mode
    _mouseStates = std::make_unique<OrbitalInteractionMode::MouseStates>(0.002, 1);
    _interactionModes.insert(
        std::pair<std::string, std::shared_ptr<InteractionMode>>(
            "Orbital",
            std::make_shared<OrbitalInteractionMode>(_mouseStates)
            ));
    _interactionModes.insert(
        std::pair<std::string, std::shared_ptr<InteractionMode>>(
            "GlobeBrowsing",
            std::make_shared<GlobeBrowsingInteractionMode>(_mouseStates)
            ));

    // Set the interactionMode
    _currentInteractionMode = _interactionModes["Orbital"];

    // Define lambda functions for changed properties
    _rotationalFriction.onChange([&]() {
        _mouseStates->setRotationalFriction(_rotationalFriction);
    });
    _horizontalFriction.onChange([&]() {
        _mouseStates->setHorizontalFriction(_horizontalFriction);
    });
    _verticalFriction.onChange([&]() {
        _mouseStates->setVerticalFriction(_verticalFriction);
    });
    _sensitivity.onChange([&]() {
        _mouseStates->setSensitivity(_sensitivity);
    });
    _rapidness.onChange([&]() {
        _mouseStates->setVelocityScaleFactor(_rapidness);
    });

    // Add the properties
    addProperty(_origin);
    addProperty(_coordinateSystem);

    addProperty(_rotationalFriction);
    addProperty(_horizontalFriction);
    addProperty(_verticalFriction);
    addProperty(_sensitivity);
    addProperty(_rapidness);
}

InteractionHandler::~InteractionHandler() {

}

void InteractionHandler::setFocusNode(SceneGraphNode* node) {
    _currentInteractionMode->setFocusNode(node);
}

void InteractionHandler::setCamera(Camera* camera) {
    _camera = camera;
}

void InteractionHandler::resetCameraDirection() {
    LINFO("Setting camera direction to point at focus node.");

    glm::dquat rotation = _camera->rotationQuaternion();
    glm::dvec3 focusPosition = focusNode()->worldPosition();
    glm::dvec3 cameraPosition = _camera->positionVec3();
    glm::dvec3 lookUpVector = _camera->lookUpVectorWorldSpace();

    glm::dvec3 directionToFocusNode = glm::normalize(focusPosition - cameraPosition);

    // To make sure the lookAt function won't fail
    static const double epsilon = 0.000001;
    if (glm::dot(lookUpVector, directionToFocusNode) > 1.0 - epsilon) {
        // Change the look up vector a little bit
        lookUpVector = glm::normalize(lookUpVector + glm::dvec3(epsilon));
    }

    // Create the rotation to look at  focus node
    glm::dmat4 lookAtMat = glm::lookAt(
        glm::dvec3(0, 0, 0),
        directionToFocusNode,
        lookUpVector);
    glm::dquat rotationLookAtFocusNode = normalize(quat_cast(inverse(lookAtMat)));

    // Update camera Rotation
    _camera->setRotation(rotationLookAtFocusNode);

    // Explicitly synch
    _camera->preSynchronization();
    _camera->postSynchronizationPreDraw();
}

void InteractionHandler::setInteractionMode(std::shared_ptr<InteractionMode> interactionMode) {
    // Focus node is passed over from the previous interaction mode
    SceneGraphNode* focusNode = _currentInteractionMode->focusNode();

    // Set the interaction mode
    _currentInteractionMode = interactionMode;

    // Update the focusnode for the new interaction mode
    _currentInteractionMode->setFocusNode(focusNode);
}

void InteractionHandler::setInteractionMode(const std::string& interactionModeKey) {
    if (_interactionModes.find(interactionModeKey) != _interactionModes.end()) {
        setInteractionMode(_interactionModes[interactionModeKey]);
        LINFO("Interaction mode set to '" << interactionModeKey << "'");
    }
    else {
        std::string listInteractionModes("");
        for (auto pair : _interactionModes) {
            listInteractionModes += "'" + pair.first + "', ";
        }
        LWARNING("'" << interactionModeKey <<
            "' is not a valid interaction mode. Candidates are " << listInteractionModes);
    }
}

void InteractionHandler::lockControls() {

}

void InteractionHandler::unlockControls() {

}

void InteractionHandler::preSynchronization(double deltaTime) {
    ghoul_assert(_inputState != nullptr, "InputState cannot be null!");
    ghoul_assert(_camera != nullptr, "Camera cannot be null!");

    if (_cameraUpdatedFromScript) {
        _cameraUpdatedFromScript = false;
    }
    else {
        _currentInteractionMode->updateMouseStatesFromInput(*_inputState, deltaTime);
    }
}

void InteractionHandler::postSynchronizationPreDraw() {
    ghoul_assert(_inputState != nullptr, "InputState cannot be null!");
    ghoul_assert(_camera != nullptr, "Camera cannot be null!");

    if (_cameraUpdatedFromScript) {
        _cameraUpdatedFromScript = false;
    }
    else {
        _currentInteractionMode->updateCameraStateFromMouseStates(*_camera);
        _camera->setFocusPositionVec3(focusNode()->worldPosition());
    }
}


SceneGraphNode* const InteractionHandler::focusNode() const {
    return _currentInteractionMode->focusNode();
}

Camera* const InteractionHandler::camera() const {
    return _camera;
}

const InputState& InteractionHandler::inputState() const {
    return *_inputState;
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

void InteractionHandler::setCameraStateFromDictionary(const ghoul::Dictionary& cameraDict) {
    bool readSuccessful = true;

    std::string focus;
    glm::dvec3 cameraPosition;
    glm::dvec4 cameraRotation; // Need to read the quaternion as a vector first.

    readSuccessful &= cameraDict.getValue(KeyFocus, focus);
    readSuccessful &= cameraDict.getValue(KeyPosition, cameraPosition);
    readSuccessful &= cameraDict.getValue(KeyRotation, cameraRotation);

    if (!readSuccessful) {
        throw ghoul::RuntimeError(
            "Position, Rotation and Focus need to be defined for camera dictionary.");
    }

    SceneGraphNode* node = sceneGraphNode(focus);
    if (!node) {
        throw ghoul::RuntimeError(
            "Could not find a node in scenegraph called '" + focus + "'");
    }

    // Set state
    setFocusNode(node);
    _camera->setPositionVec3(cameraPosition);
    _camera->setRotation(glm::dquat(
        cameraRotation.x, cameraRotation.y, cameraRotation.z, cameraRotation.w));

    // Explicitly synch
    _camera->preSynchronization();
    _camera->postSynchronizationPreDraw();
}

ghoul::Dictionary InteractionHandler::getCameraStateDictionary() {
    glm::dvec3 cameraPosition;
    glm::dquat quat;
    glm::dvec4 cameraRotation;

    cameraPosition = _camera->positionVec3();
    quat = _camera->rotationQuaternion();
    cameraRotation = glm::dvec4(quat.w, quat.x, quat.y, quat.z);

    ghoul::Dictionary cameraDict;
    cameraDict.setValue(KeyPosition, cameraPosition);
    cameraDict.setValue(KeyRotation, cameraRotation);
    cameraDict.setValue(KeyFocus, focusNode()->name());

    return cameraDict;
}

void InteractionHandler::saveCameraStateToFile(const std::string& filepath) {
    if (!filepath.empty()) {
        auto fullpath = absPath(filepath);
        LINFO("Saving camera position: " << filepath);

        ghoul::Dictionary cameraDict = getCameraStateDictionary();

        // TODO : Should get the camera state as a dictionary and save the dictionary to
        // a file in form of a lua state and not use ofstreams here.
        
        std::ofstream ofs(fullpath.c_str());
        
        glm::dvec3 p = _camera->positionVec3();
        glm::dquat q = _camera->rotationQuaternion();

        ofs << "return {" << std::endl;
        ofs << "    " << KeyFocus << " = " << "\"" << focusNode()->name() << "\"" << "," << std::endl;
        ofs << "    " << KeyPosition << " = {"
            << std::to_string(p.x) << ", "
            << std::to_string(p.y) << ", "
            << std::to_string(p.z) << "}," << std::endl;
        ofs << "    " << KeyRotation << " = {"
            << std::to_string(q.w) << ", "
            << std::to_string(q.x) << ", "
            << std::to_string(q.y) << ", "
            << std::to_string(q.z) << "}," << std::endl;
        ofs << "}"<< std::endl;

        ofs.close();
    }
}

void InteractionHandler::restoreCameraStateFromFile(const std::string& filepath) {
    LINFO("Reading camera state from file: " << filepath);
    if (!FileSys.fileExists(filepath))
        throw ghoul::FileNotFoundError(filepath, "CameraFilePath");

    ghoul::Dictionary cameraDict;
    try {
        ghoul::lua::loadDictionaryFromFile(filepath, cameraDict);
        setCameraStateFromDictionary(cameraDict);
        _cameraUpdatedFromScript = true;
    }
    catch (ghoul::RuntimeError& e) {
        LWARNING("Unable to set camera position");
        LWARNING(e.message);
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
    
void InteractionHandler::writeKeyboardDocumentation(const std::string& type, const std::string& file)
{
    if (type == "text") {
        std::ofstream f;
        f.exceptions(~std::ofstream::goodbit);
        f.open(file);
        
        for (const auto& p : _keyLua) {
            f << std::to_string(p.first) << ": " <<
                p.second << std::endl;
        }
    }
    else if (type == "html") {
        std::ofstream f;
        f.exceptions(~std::ofstream::goodbit);
        f.open(absPath(file));

        std::stringstream json;
        json << "[";
        for (const auto& p : _keyLua) {
            json << "{";
            json << "\"key\": \"" << std::to_string(p.first) << "\",";
            json << "\"script\": \"" << p.second << "\",";
            json << "},";
        }
        json << "]";

        std::string jsonText = json.str();

    }
    else {
        throw ghoul::RuntimeError(
            "Unsupported keyboard documentation type '" + type + "'",
            "InteractionHandler"
        );
    }
}

scripting::LuaLibrary InteractionHandler::luaLibrary() {
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
            },
            {
                "saveCameraStateToFile",
                &luascriptfunctions::saveCameraStateToFile,
                "string",
                "Save the current camera state to file"
            },
            {
                "restoreCameraStateFromFile",
                &luascriptfunctions::restoreCameraStateFromFile,
                "string",
                "Restore the camera state from file"
            },
            {
                "resetCameraDirection",
                &luascriptfunctions::resetCameraDirection,
                "void",
                "Reset the camera direction to point at the focus node"
            },
        }
    };
}

void InteractionHandler::addKeyframe(const network::datamessagestructures::PositionKeyframe &kf) {
    _inputState->addKeyframe(kf);
}

void InteractionHandler::clearKeyframes() {
    _inputState->clearKeyframes();
}

void InteractionHandler::serialize(SyncBuffer* syncBuffer) {
    for (auto var : _interactionModes)
    {
        var.second->serialize(syncBuffer);
    }
}

void InteractionHandler::deserialize(SyncBuffer* syncBuffer) {
    for (auto var : _interactionModes)
    {
        var.second->deserialize(syncBuffer);
    }
}

#endif // USE_OLD_INTERACTIONHANDLER

} // namespace interaction
} // namespace openspace
