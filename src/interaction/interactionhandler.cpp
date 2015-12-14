/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

namespace {
	const std::string _loggerCat = "InteractionHandler";

    openspace::Key stringToKey(std::string s) {
		// key only uppercase
		std::transform(s.begin(), s.end(), s.begin(), ::toupper);

		// default is unknown
        auto it = openspace::KeyMapping.find(s);
        if (it != openspace::KeyMapping.end())
            return it->second;
        else
            return openspace::Key::Unknown;
	}
}

#include "interactionhandler_lua.inl"

namespace openspace {

//InteractionHandler::InteractionHandler() {
//	// initiate pointers
//	_camera = nullptr;
//	_enabled = true;
//	_node = nullptr;
//	_dt = 0.0;
//	_lastTrackballPos = glm::vec3(0.0, 0.0, 0.5);
//	_leftMouseButtonDown = false;
//	_isMouseBeingPressedAndHeld = false;
//}
//
//InteractionHandler::~InteractionHandler() {
//	for (size_t i = 0; i < _controllers.size(); ++i) {
//		delete _controllers[i];
//	}
//}
//
////void InteractionHandler::init() {
////	assert( ! this_);
////	 this_ = new InteractionHandler();
////}
////
////void InteractionHandler::deinit() {
////	assert(this_);
////	delete this_;
////	this_ = nullptr;
////}
////
////InteractionHandler& InteractionHandler::ref() {
////	assert(this_);
////    return *this_;
////}
//
////bool InteractionHandler::isInitialized() {
////	return this_ != nullptr;
////}	
//
//void InteractionHandler::enable() {
//	//assert(this_);
//	_enabled = true;
//}
//
//void InteractionHandler::disable() {
//	//assert(this_);
//	_enabled = false;
//}
//
//const bool InteractionHandler::isEnabled() const {
//	//assert(this_);
//	if (_camera)
//		return false;
//	return _enabled;
//=======

//namespace openspace {
namespace interaction {

InteractionHandler::InteractionHandler()
    : properties::PropertyOwner()
	, _camera(nullptr)
	, _focusNode(nullptr)
    , _deltaTime(0.0)
    , _validKeyLua(false)
    , _controllerSensitivity(1.f)
    , _invertRoll(false)
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

//<<<<<<< HEAD
//void InteractionHandler::addExternalControl(ExternalControl* controller) {
//	//assert(this_);
//	if (controller != nullptr) {
//		_controllers.push_back(controller);
//	}
//}
//
//void InteractionHandler::setCamera(Camera *camera) {
//	//assert(this_);
//	_camera = camera;
//}
//
//void InteractionHandler::setOrigin(SceneGraphNode* node) {
//	if (node)
//		_node = node;
//}
//
//Camera * InteractionHandler::getCamera() const {
//	//assert(this_);
//	if (_enabled) {
//		return _camera;
//	}
//	return nullptr;
//}
//
//const psc InteractionHandler::getOrigin() const {
//	if (_node)
//		return _node->worldPosition();
//	return psc();
//}
//
//void InteractionHandler::lockControls() {
//	//assert(this_);
//	_cameraGuard.lock();
//}
//
//void InteractionHandler::unlockControls() {
//	//assert(this_);
//	_cameraGuard.unlock();
//}
//
//void InteractionHandler::setFocusNode(SceneGraphNode *node) {
//	//assert(this_);
//	_node = node;
//}
//
//void InteractionHandler::rotate(const glm::quat &rotation) {
//	//assert(this_);
//	lockControls();
//	_camera->rotate(rotation);
//	unlockControls();
//=======
void InteractionHandler::update(double deltaTime) {
	_deltaTime = deltaTime;
	_mouseController->update(deltaTime);
    
    bool hasKeys = false;
    psc pos;
    glm::quat q;
    
	_keyframeMutex.lock();

	if (_keyframes.size() > 4){	//wait until enough samples are buffered
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
    
    if(hasKeys){
		_camera->setPosition(pos);
		_camera->setViewRotationMatrix(glm::mat4_cast(q));
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
	glm::vec3 cameraView = glm::normalize(_camera->viewDirection());
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
//	if (_keyboardController) {
//		auto start = ghoul::HighResClock::now();
//		_keyboardController->keyPressed(KeyAction(action), Key(key), KeyModifier::None);
//		auto end = ghoul::HighResClock::now();
//		LINFO("Keyboard timing: " << std::chrono::duration_cast<std::chrono::nanoseconds>(end - start).count() << "ns");
//	}
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
	
	glm::vec3 cameraUp = glm::normalize((glm::inverse(_camera->viewRotationMatrix()) * glm::vec4(_camera->lookUpVector(), 0))).xyz();
	glm::vec3 cameraRight = glm::cross(_camera->viewDirection(), cameraUp);

	glm::mat4 transform;
	transform = glm::rotate(dx * 10.f, cameraUp) * transform;
	transform = glm::rotate(dy * 10.f, cameraRight) * transform;
	transform = glm::rotate(dz * 10.f, _camera->viewDirection()) * transform;

	
	//get "old" focus position 
	psc focus = _camera->focusPosition();
	
	//// get camera position 
	//psc relative = _camera->position();

	// get camera position (UNSYNCHRONIZED)
	psc relative = _camera->unsynchedPosition();

	//get relative vector
	psc relative_focus_coordinate = relative - focus;
	//rotate relative vector
	relative_focus_coordinate = glm::inverse(transform) * relative_focus_coordinate.vec4();
	
	//get new new position of focus node
	psc origin;
	if (_focusNode) {
		origin = _focusNode->worldPosition();
	}

	//new camera position
	relative = origin + relative_focus_coordinate; 	

	float bounds = 2.f * (_focusNode ? _focusNode->boundingSphere().lengthf() : 0.f);

	psc target = relative + relative_focus_coordinate * dist;
	//don't fly into objects
	if ((target - origin).length() < bounds){
		target = relative;
	}

	unlockControls();
    
    _camera->setFocusPosition(origin);
	_camera->setPosition(target);
	_camera->rotate(glm::quat_cast(transform));
	
}

//void InteractionHandler::distance(const float &d){
//
//	lockControls();
//
//	psc relative = _camera->position();
//	const psc origin = (_focusNode) ? _focusNode->worldPosition() : psc();
//	psc relative_origin_coordinate = relative - origin;
//	// addition 100% of bounds (fix later to something node specific?)
//	float bounds = 2.f * (_focusNode ? _focusNode->boundingSphere().lengthf() : 0.f);
//
//	psc target = relative + relative_origin_coordinate * d;// *fmaxf(bounds, (1.f - d));
//	//don't fly into objects
//	if ((target - origin).length() < bounds){
//		target = relative;
//	}
//	_camera->setPosition(target);
//	
//	unlockControls();
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
    glm::mat4 la = glm::lookAt(_camera->position().vec3(), origin.vec3(), glm::rotate(rotation, _camera->lookUpVector()));
    
    unlockControls();
    
	_camera->setPosition(relative);
	//camera_->rotate(rotation);
	//camera_->setRotation(glm::mat4_cast(rotation));

	
	_camera->setRotation(la);
	//camera_->setLookUpVector();

	
}

//<<<<<<< HEAD
//void InteractionHandler::distance(const PowerScaledScalar &dist, size_t iterations) {
//	if (iterations > 5)
//		return;
//	//assert(this_);
//	lockControls();
//	
//	psc relative = _camera->position();
//	const psc origin = (_node) ? _node->worldPosition() : psc();
//
//	psc relative_origin_coordinate = relative - origin;
//	const glm::vec3 dir(relative_origin_coordinate.direction());
//	glm::vec3 newdir = dir * dist[0];
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
		_validKeyLua = true;
		auto ret = _keyLua.equal_range(key);
		for (auto it = ret.first; it != ret.second; ++it) {
			//OsEng.scriptEngine()->runScript(it->second);
			OsEng.scriptEngine().queueScript(it->second);
			if (!_validKeyLua) {
				break;
			}
		}
	}
}
//
//void InteractionHandler::mouseButtonCallback(int key, int action) {
//    //if(mouseControl_ != nullptr) {
//    //	mouseControl_->mouseButtonCallback(key,action);
//    //}
//	if (key == SGCT_MOUSE_BUTTON_LEFT && action == SGCT_PRESS)
//		_leftMouseButtonDown = true;
//	else if (key == SGCT_MOUSE_BUTTON_LEFT && action == SGCT_RELEASE) {
//		_leftMouseButtonDown = false;
//		_isMouseBeingPressedAndHeld = false;
//	}
//}
//
//void InteractionHandler::mousePositionCallback(int x, int y) {
//	if (_leftMouseButtonDown)
//		trackballRotate(x,y);
//
//    //if(mouseControl_ != nullptr) {
//    //	mouseControl_->mousePosCallback(x,y);
//    //}
//}
//
//void InteractionHandler::mouseScrollWheelCallback(int pos) {
//    //if(mouseControl_ != nullptr) {
//    //	mouseControl_->mouseScrollCallback(pos);
//    //}
//    const float speed = 4.75f;
//	const float dt = static_cast<float>(_dt);
//    if(pos < 0) {
//	    PowerScaledScalar dist(speed * dt, 0.0f);
//	    distance(dist);
//    } else if(pos > 0) {
//	    PowerScaledScalar dist(-speed * dt, 0.0f);
//	    distance(dist);
//    }
//}
//
//
void InteractionHandler::resetKeyBindings() {
	_keyLua.clear();
	_validKeyLua = false;
}

void InteractionHandler::bindKey(Key key, std::string lua) {
	_keyLua.insert(std::make_pair(key, lua));
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

//=======
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

} // namespace interaction
} // namespace openspace
