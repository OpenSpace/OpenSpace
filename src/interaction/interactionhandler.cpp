/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/highresclock.h>

namespace {
	const std::string _loggerCat = "InteractionHandler";

	int stringToKey(std::string s) {

		static std::map<std::string, int> m = {
			{ "SPACE", SGCT_KEY_SPACE },
			{ "APOSTROPHE", SGCT_KEY_APOSTROPHE },
			{ "COMMA", SGCT_KEY_COMMA },
			{ "MINUS", SGCT_KEY_MINUS },
			{ "PERIOD", SGCT_KEY_PERIOD },
			{ "APOSTROPHE", SGCT_KEY_SLASH },
			{ "0", SGCT_KEY_0 },
			{ "1", SGCT_KEY_1 },
			{ "2", SGCT_KEY_2 },
			{ "3", SGCT_KEY_3 },
			{ "4", SGCT_KEY_4 },
			{ "5", SGCT_KEY_5 },
			{ "6", SGCT_KEY_6 },
			{ "7", SGCT_KEY_7 },
			{ "8", SGCT_KEY_8 },
			{ "9", SGCT_KEY_9 },
			{ "SEMICOLON", SGCT_KEY_SEMICOLON },
			{ "EQUAL", SGCT_KEY_EQUAL },
			{ "A", SGCT_KEY_A },
			{ "B", SGCT_KEY_B },
			{ "C", SGCT_KEY_C },
			{ "D", SGCT_KEY_D },
			{ "E", SGCT_KEY_E },
			{ "F", SGCT_KEY_F },
			{ "G", SGCT_KEY_G },
			{ "H", SGCT_KEY_H },
			{ "I", SGCT_KEY_I },
			{ "J", SGCT_KEY_J },
			{ "K", SGCT_KEY_K },
			{ "L", SGCT_KEY_L },
			{ "M", SGCT_KEY_M },
			{ "N", SGCT_KEY_N },
			{ "O", SGCT_KEY_O },
			{ "P", SGCT_KEY_P },
			{ "Q", SGCT_KEY_Q },
			{ "R", SGCT_KEY_R },
			{ "S", SGCT_KEY_S },
			{ "T", SGCT_KEY_T },
			{ "U", SGCT_KEY_U },
			{ "V", SGCT_KEY_V },
			{ "W", SGCT_KEY_W },
			{ "X", SGCT_KEY_X },
			{ "Y", SGCT_KEY_Y },
			{ "Z", SGCT_KEY_Z },
			{ "LEFT_BRACKET", SGCT_KEY_LEFT_BRACKET },
			{ "BACKSLASH", SGCT_KEY_BACKSLASH },
			{ "RIGHT_BRACKET", SGCT_KEY_RIGHT_BRACKET },
			{ "GRAVE_ACCENT", SGCT_KEY_GRAVE_ACCENT },
			{ "WORLD_1", SGCT_KEY_WORLD_1 },
			{ "WORLD_2", SGCT_KEY_WORLD_2 },
			{ "ESC", SGCT_KEY_ESC },
			{ "ESCAPE", SGCT_KEY_ESCAPE },
			{ "ENTER", SGCT_KEY_ENTER },
			{ "TAB", SGCT_KEY_TAB },
			{ "BACKSPACE", SGCT_KEY_BACKSPACE },
			{ "INSERT", SGCT_KEY_INSERT },
			{ "DEL", SGCT_KEY_DEL },
			{ "DELETE", SGCT_KEY_DELETE },
			{ "RIGHT", SGCT_KEY_RIGHT },
			{ "LEFT", SGCT_KEY_LEFT },
			{ "DOWN", SGCT_KEY_DOWN },
			{ "UP", SGCT_KEY_UP },
			{ "PAGEUP", SGCT_KEY_PAGEUP },
			{ "PAGEDOWN", SGCT_KEY_PAGEDOWN },
			{ "PAGE_UP", SGCT_KEY_PAGE_UP },
			{ "PAGE_DOWN", SGCT_KEY_PAGE_DOWN },
			{ "HOME", SGCT_KEY_HOME },
			{ "END", SGCT_KEY_END },
			{ "CAPS_LOCK", SGCT_KEY_CAPS_LOCK },
			{ "SCROLL_LOCK", SGCT_KEY_SCROLL_LOCK },
			{ "NUM_LOCK", SGCT_KEY_NUM_LOCK },
			{ "PRINT_SCREEN", SGCT_KEY_PRINT_SCREEN },
			{ "PAUSE", SGCT_KEY_PAUSE },
			{ "F1", SGCT_KEY_F1 },
			{ "F2", SGCT_KEY_F2 },
			{ "F3", SGCT_KEY_F3 },
			{ "F4", SGCT_KEY_F4 },
			{ "F5", SGCT_KEY_F5 },
			{ "F6", SGCT_KEY_F6 },
			{ "F7", SGCT_KEY_F7 },
			{ "F8", SGCT_KEY_F8 },
			{ "F9", SGCT_KEY_F9 },
			{ "F10", SGCT_KEY_F10 },
			{ "F11", SGCT_KEY_F11 },
			{ "F12", SGCT_KEY_F12 },
			{ "F13", SGCT_KEY_F13 },
			{ "F14", SGCT_KEY_F14 },
			{ "F15", SGCT_KEY_F15 },
			{ "F16", SGCT_KEY_F16 },
			{ "F17", SGCT_KEY_F17 },
			{ "F18", SGCT_KEY_F18 },
			{ "F19", SGCT_KEY_F19 },
			{ "F20", SGCT_KEY_F20 },
			{ "F21", SGCT_KEY_F21 },
			{ "F22", SGCT_KEY_F22 },
			{ "F23", SGCT_KEY_F23 },
			{ "F24", SGCT_KEY_F24 },
			{ "F25", SGCT_KEY_F25 },
			{ "KP_0", SGCT_KEY_KP_0 },
			{ "KP_1", SGCT_KEY_KP_1 },
			{ "KP_2", SGCT_KEY_KP_2 },
			{ "KP_3", SGCT_KEY_KP_3 },
			{ "KP_4", SGCT_KEY_KP_4 },
			{ "KP_5", SGCT_KEY_KP_5 },
			{ "KP_6", SGCT_KEY_KP_6 },
			{ "KP_7", SGCT_KEY_KP_7 },
			{ "KP_8", SGCT_KEY_KP_8 },
			{ "KP_9", SGCT_KEY_KP_9 },
			{ "KP_DECIMAL", SGCT_KEY_KP_DECIMAL },
			{ "KP_DIVIDE", SGCT_KEY_KP_DIVIDE },
			{ "KP_MULTIPLY", SGCT_KEY_KP_MULTIPLY },
			{ "KP_SUBTRACT", SGCT_KEY_KP_SUBTRACT },
			{ "KP_ADD", SGCT_KEY_KP_ADD },
			{ "KP_ENTER", SGCT_KEY_KP_ENTER },
			{ "KP_EQUAL", SGCT_KEY_KP_EQUAL },
			{ "LSHIFT", SGCT_KEY_LSHIFT },
			{ "LEFT_SHIFT", SGCT_KEY_LEFT_SHIFT },
			{ "LCTRL", SGCT_KEY_LCTRL },
			{ "LEFT_CONTROL", SGCT_KEY_LEFT_CONTROL },
			{ "LALT", SGCT_KEY_LALT },
			{ "LEFT_ALT", SGCT_KEY_LEFT_ALT },
			{ "LEFT_SUPER", SGCT_KEY_LEFT_SUPER },
			{ "RSHIFT", SGCT_KEY_RSHIFT },
			{ "RIGHT_SHIFT", SGCT_KEY_RIGHT_SHIFT },
			{ "RCTRL", SGCT_KEY_RCTRL },
			{ "RIGHT_CONTROL", SGCT_KEY_RIGHT_CONTROL },
			{ "RALT", SGCT_KEY_RALT },
			{ "RIGHT_ALT", SGCT_KEY_RIGHT_ALT },
			{ "RIGHT_SUPER", SGCT_KEY_RIGHT_SUPER },
			{ "MENU", SGCT_KEY_MENU }
		};
		// key only uppercase
		std::transform(s.begin(), s.end(), s.begin(), ::toupper);

		// default is unknown
		int key = SGCT_KEY_UNKNOWN;
		auto it = m.find(s);
		if (it != m.end())
			key = m[s];
		return key;
	}
}

namespace openspace {

namespace luascriptfunctions {

/**
 * \ingroup LuaScripts
 * setOrigin():
 * Set the origin of the camera
 */
int setOrigin(lua_State* L) {
	using ghoul::lua::luaTypeToString;
	const std::string _loggerCat = "LuaInteractionHandler";

	int nArguments = lua_gettop(L);
	if (nArguments != 1)
		return luaL_error(L, "Expected %i arguments, got %i", 1, nArguments);

	const int type = lua_type(L, -1);
	if (type != LUA_TSTRING)
		return luaL_error(L, "Expected string, got %i", type);

	std::string s = luaL_checkstring(L, -1);

	SceneGraphNode* node = sceneGraphNode(s);
	if (!node) {
		LWARNING("Could not find a node in scenegraph called '" << s <<"'");
		return 0;
	}

	OsEng.interactionHandler()->setFocusNode(node);

	return 0;
}

/**
* \ingroup LuaScripts
* bindKey():
* Binds a key to Lua command
*/
int bindKey(lua_State* L) {
	using ghoul::lua::luaTypeToString;
	const std::string _loggerCat = "LuaInteractionHandler";

	int nArguments = lua_gettop(L);
	if (nArguments != 2)
		return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);


	std::string key = luaL_checkstring(L, -2);
	std::string command = luaL_checkstring(L, -1);

	if (command.empty())
		return luaL_error(L, "Command string is empty");

	int iKey = stringToKey(key);

	if (iKey == SGCT_KEY_UNKNOWN) {
		LERROR("Could not find key '"<< key <<"'");
		return 0;
	}


	OsEng.interactionHandler()->bindKey(iKey, command);

	return 0;
}

/**
* \ingroup LuaScripts
* clearKeys():
* Clears all key bindings
*/
int clearKeys(lua_State* L) {
	using ghoul::lua::luaTypeToString;
	const std::string _loggerCat = "LuaInteractionHandler";

	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	OsEng.interactionHandler()->resetKeyBindings();

	return 0;
}

/**
* \ingroup LuaScripts
* dt(bool):
* Get current frame time
*/
int dt(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 0)
		return luaL_error(L, "Expected %i arguments, got %i", 0, nArguments);

	lua_pushnumber(L,OsEng.interactionHandler()->deltaTime());
	return 1;
}

/**
* \ingroup LuaScripts
* distance(double, double):
* Change distance to origin
*/
int distance(lua_State* L) {
	int nArguments = lua_gettop(L);
	if (nArguments != 2)
		return luaL_error(L, "Expected %i arguments, got %i", 2, nArguments);

	double d1 = luaL_checknumber(L, -2);
	double d2 = luaL_checknumber(L, -1);
	PowerScaledScalar dist(static_cast<float>(d1), static_cast<float>(d2));
	OsEng.interactionHandler()->distanceDelta(dist);
	return 0;
}

} // namespace luascriptfunctions

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
	: _camera(nullptr)
	, _focusNode(nullptr)
	, _keyboardController(nullptr)
	, _mouseController(nullptr)
{
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
	glm::vec3 rotAxis = glm::normalize(glm::cross(viewDir, cameraView));
	float angle = glm::angle(viewDir, cameraView);
	glm::quat q = glm::angleAxis(angle, rotAxis);

	psc camPos = _camera->position();
	//set new focus position
	_camera->setFocusPosition(node->worldPosition());
	//rotate view to target new focus
	_camera->rotate(q);

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

void InteractionHandler::mouseButtonCallback(int button, int action) {
	if (_mouseController)
		_mouseController->button(MouseAction(action), MouseButton(button));
}

void InteractionHandler::mousePositionCallback(int x, int y) {
	if (_mouseController)
		// TODO Remap screen coordinates to [0,1]
		_mouseController->move(float(x), float(y));
}

void InteractionHandler::mouseScrollWheelCallback(int pos) {
	if (_mouseController)
		_mouseController->scrollWheel(pos);
}

void InteractionHandler::orbit(const float &dx, const float &dy, const float &dz, const float &dist){

	lockControls();
	
	glm::vec3 cameraUp = glm::normalize((glm::inverse(_camera->viewRotationMatrix()) * glm::vec4(_camera->lookUpVector(), 0))).xyz;
	glm::vec3 cameraRight = glm::cross(_camera->viewDirection(), cameraUp);

	glm::mat4 transform;
	transform = glm::rotate(dx * 10, cameraUp) * transform;
	transform = glm::rotate(dy * 10, cameraRight) * transform;
	transform = glm::rotate(dz * 10, _camera->viewDirection()) * transform;

	
	//get "old" focus position 
	psc focus = _camera->focusPosition();
	
	// get camera position
	psc relative = _camera->position();
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

	_camera->setFocusPosition(origin);
	_camera->setPosition(target);
	_camera->rotate(glm::quat_cast(transform));

	unlockControls();
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

	_camera->setPosition(relative);
	//camera_->rotate(rotation);
	//camera_->setRotation(glm::mat4_cast(rotation));

	glm::mat4 la = glm::lookAt(_camera->position().vec3(), origin.vec3(), glm::rotate(rotation, _camera->lookUpVector()));
	_camera->setRotation(la);
	//camera_->setLookUpVector();

	unlockControls();
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
	lockControls();
	_camera->rotate(rotation);
	unlockControls();
}

void InteractionHandler::distanceDelta(const PowerScaledScalar& distance, size_t iterations)
{
	if (iterations > 5)
		return;
	//assert(this_);
	lockControls();
		
	psc relative = _camera->position();
	const psc origin = (_focusNode) ? _focusNode->worldPosition() : psc();
	
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
		unlockControls();

	}
	else {
		unlockControls();
		PowerScaledScalar d2 = distance;
		d2[0] *= 0.75f;
		d2[1] *= 0.85f;
		distanceDelta(d2, iterations + 1);
	}
//	
//}
//
//void InteractionHandler::lookAt(const glm::quat &rotation) {
//	//assert(this_);
//	lockControls();
//=======
	//if (glm::angle(newdir, dir) < 90.0f)
		//_camera->setPosition(relative);
//>>>>>>> feature/interactionhandler

	//unlockControls();
}

void InteractionHandler::lookAt(const glm::quat& rotation)
{
}

//<<<<<<< HEAD
//void InteractionHandler::update(const double dt) {
//	//assert(this_);
//	// setting dt_ for use in callbacks
//	_dt = dt;
//	if (_enabled && _camera) {
//		// fetch data from joysticks
//		DeviceIdentifier::ref().update();
//
//		// update all controllers
//		for (size_t i = 0; i < _controllers.size(); ++i) {
//			_controllers[i]->update();
//		}
//	}
//}
//
//double InteractionHandler::dt() {
//	//assert(this_);
//	return _dt;
//}
//
//glm::vec3 InteractionHandler::mapToTrackball(glm::vec2 mousePos) {
//	const float RADIUS = 0.5; // Sphere radius
//	glm::vec3 out = glm::vec3(mousePos.x-0.5, -1.0*(mousePos.y-0.5), 0);
//
//	// Mapping according to Holroyds trackball
//	// Piece-wise sphere + hyperbolic sheet
//	if (out.x*out.x + out.y*out.y <= RADIUS*RADIUS/2.0) {
//		//Spherical Region
//		out.z = RADIUS*RADIUS - (out.x*out.x + out.y*out.y);
//		out.z = out.z > 0.0f ? sqrtf(out.z) : 0.0f;
//	} else { //Hyperbolic Region - for smooth z values
//		out.z = (RADIUS*RADIUS)/(2.0f*sqrt(out.x*out.x + out.y*out.y));
//	}
//
//	return glm::normalize(out);
//}
//
//glm::vec3 InteractionHandler::mapToCamera(glm::vec3 trackballPos) {
////	return glm::vec3((sgct::Engine::instance()->getActiveViewMatrix() * glm::vec4(trackballPos,0)));
//
//    //Get x,y,z axis vectors of current camera view
//	glm::vec3 currentViewYaxis = glm::normalize(_camera->lookUpVector());
//	psc viewDir = _camera->position() - _node->worldPosition();
//	glm::vec3 currentViewZaxis = glm::normalize(viewDir.vec3());
//	glm::vec3 currentViewXaxis = glm::normalize(glm::cross(currentViewYaxis, currentViewZaxis));
//
//    //mapping to camera co-ordinate
//    currentViewXaxis*=trackballPos.x;
//    currentViewYaxis*=trackballPos.y;
//    currentViewZaxis*=trackballPos.z;
//    return (currentViewXaxis + currentViewYaxis + currentViewZaxis);
//}
//
//void InteractionHandler::trackballRotate(int x, int y) {
//	// Normalize mouse coordinates to [0,1]
//	float width = static_cast<float>(sgct::Engine::instance()->getActiveXResolution());
//	float height = static_cast<float>(sgct::Engine::instance()->getActiveYResolution());
//	glm::vec2 mousePos = glm::vec2((float)x/width, (float)y/height);
//
//	mousePos = glm::clamp(mousePos, -0.5, 1.5); // Ugly fix #1: Camera position becomes NaN on mouse values outside [-0.5, 1.5]
//    //mousePos[1] = 0.5; 							// Ugly fix #2: Tempoarily only allow rotation around y
//
//	glm::vec3 curTrackballPos = mapToTrackball(mousePos);
////	LDEBUG(mousePos.x << ", " << mousePos.y << " = " << curTrackballPos.x << ", " << curTrackballPos.y << ", " << curTrackballPos.z);
//
//	// Disable movement on the first click for extra smoothness
//	if (!_isMouseBeingPressedAndHeld) {
//		_lastTrackballPos = curTrackballPos;
//		_isMouseBeingPressedAndHeld = true;
//	}
//
//	if (curTrackballPos != _lastTrackballPos) {
//		// calculate rotation angle (in radians)
//		float rotationAngle = glm::angle(curTrackballPos, _lastTrackballPos);
//		rotationAngle *= static_cast<float>(dt())*100.0f;
//
//        // Map trackballpos to camera
////		glm::vec3 trackballMappedToCamera = mapToCamera(_lastTrackballPos - curTrackballPos);
////		psc currentCamPos = camera_->getPosition();
////		glm::vec3 nextCamPos = currentCamPos.getVec3f() + trackballMappedToCamera;
////		glm::vec3 rotationAxis = glm::cross(currentCamPos.getVec3f(), nextCamPos);
//
//		glm::vec3 rotationAxis = glm::cross(_lastTrackballPos, curTrackballPos);
//		rotationAxis = glm::normalize(rotationAxis);
//		glm::quat quaternion = glm::angleAxis(rotationAngle, rotationAxis);
//
//		// Apply quaternion to camera
//		orbit(quaternion);
//
//		_lastTrackballPos = curTrackballPos;
//	}
//}
//
void InteractionHandler::keyboardCallback(int key, int action) {
    // TODO package in script
    const float speed = 2.75f;
	const float dt = static_cast<float>(_deltaTime);
	if (action == SGCT_PRESS || action == SGCT_REPEAT) {
		
	    if (key == SGCT_KEY_S) {
	        glm::vec3 euler(speed * dt, 0.0, 0.0);
	        glm::quat rot = glm::quat(euler);
	        orbitDelta(rot);
	    }
	    if (key == SGCT_KEY_W) {
	        glm::vec3 euler(-speed * dt, 0.0, 0.0);
	        glm::quat rot = glm::quat(euler);
	        orbitDelta(rot);
	    }
	    if (key == SGCT_KEY_A) {
	        glm::vec3 euler(0.0, -speed * dt, 0.0);
	        glm::quat rot = glm::quat(euler);
	        orbitDelta(rot);
	    }
	    if (key == SGCT_KEY_D) {
	        glm::vec3 euler(0.0, speed * dt, 0.0);
	        glm::quat rot = glm::quat(euler);
	        orbitDelta(rot);
	    }
		if (key == SGCT_KEY_Z) {
			Time::ref().advanceTime(sgct::Engine::instance()->getDt());
		}
		if (key == SGCT_KEY_X) {
			Time::ref().retreatTime(sgct::Engine::instance()->getDt());
		}
	    if (key == 262) {
	        glm::vec3 euler(0.0, speed * dt, 0.0);
	        glm::quat rot = glm::quat(euler);
	        rotateDelta(rot);
	    }
	    if (key == 263) {
	        glm::vec3 euler(0.0, -speed * dt, 0.0);
	        glm::quat rot = glm::quat(euler);
	        rotateDelta(rot);
	    }
	    if (key == 264) {
	        glm::vec3 euler(speed * dt, 0.0, 0.0);
	        glm::quat rot = glm::quat(euler);
	        rotateDelta(rot);
	    }
	    if (key == 265) {
	        glm::vec3 euler(-speed * dt, 0.0, 0.0);
	        glm::quat rot = glm::quat(euler);
	        rotateDelta(rot);
	    }
		if (key == SGCT_KEY_KP_SUBTRACT) {
			glm::vec2 s = OsEng.renderEngine()->camera()->scaling();
			s[1] -= 0.5f;
			OsEng.renderEngine()->camera()->setScaling(s);
		}
		if (key == SGCT_KEY_KP_ADD) {
			glm::vec2 s = OsEng.renderEngine()->camera()->scaling();
			s[1] += 0.5f;
			OsEng.renderEngine()->camera()->setScaling(s);
		}

		// iterate over key bindings
		_validKeyLua = true;
		auto ret = _keyLua.equal_range(key);
		for (auto it = ret.first; it != ret.second; ++it) {
			OsEng.scriptEngine()->runScript(it->second);
			//OsEng.scriptEngine().runScript(it->second);
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

void InteractionHandler::bindKey(int key, const std::string& lua) {
	_keyLua.insert(std::make_pair(key, lua));
}

scripting::ScriptEngine::LuaLibrary InteractionHandler::luaLibrary() {
	return {
		"",
		{
			{
				"setOrigin",
				&luascriptfunctions::setOrigin,
				"string",
				"Set the camera origin node by name"
			},
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
				"Change distance to origin"
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

} // namespace interaction
//>>>>>>> feature/interactionhandler
} // namespace openspace

//
//// open space includes
//#include <ghoul/logging/logmanager.h>
//#include <openspace/interaction/interactionhandler.h>
//#include <openspace/interaction/deviceidentifier.h>
////#include <openspace/interaction/externalcontrol/randomexternalcontrol.h>
////#include <openspace/interaction/externalcontrol/joystickexternalcontrol.h>
//#include <openspace/query/query.h>
//#include <openspace/engine/openspaceengine.h>
//#include <openspace/util/powerscaledcoordinate.h>
//#include <glm/gtx/vector_angle.hpp>
//
//#include <openspace/util/time.h>
//
//#include <cassert>
//
//namespace {
//	const std::string _loggerCat = "InteractionHandler";
//}
//
//namespace openspace {
//namespace interaction {
//
//InteractionHandler::InteractionHandler()
//	: _camera(nullptr)
//	, _node(nullptr)
//	, _dt(0.0)
//	, _lastTrackballPos(0.f)
//	, _leftMouseButtonDown(false)
//	, _isMouseBeingPressedAndHeld(false)
//{
//}
//
//InteractionHandler::~InteractionHandler() {
//	//for (size_t i = 0; i < _controllers.size(); ++i) {
//	//	delete _controllers[i];
//	//}
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
////void InteractionHandler::connectDevices() {
////	//assert(this_);
////	assert(DeviceIdentifier::ref().isInitialized());
////
////	// for each device found
////	for(int i = 0; i < DeviceIdentifier::ref().numberOfDevices(); ++i) {
////
////        // TODO
////		//if(DeviceIdentifier::ref().type(i) == InputDevice::XBOX) {
////
////		//	// found xbox, use xbox python controller
////		//	JoystickExternalControl *joystickexcontrol = new JoystickExternalControl(RELATIVE_PATH"pyinput/Xbox.py");
////		//	joystickexcontrol->setInputDevice(i);
////		//	addExternalControl(joystickexcontrol);
////
////		//} else if(DeviceIdentifier::ref().type(i) == InputDevice::SPACENAVIGATOR) {
////
////		//	// found SpaceNavigator, use SpaceNavigator python controller
////		//	JoystickExternalControl *joystickexcontrol = new JoystickExternalControl(RELATIVE_PATH"pyinput/SpaceNavigator.py");
////		//	joystickexcontrol->setInputDevice(i);
////		//	addExternalControl(joystickexcontrol);
////		//}
////		
////	}
////}
////
////void InteractionHandler::addExternalControl(ExternalControl* controller) {
////	//assert(this_);
////	if (controller != nullptr) {
////		_controllers.push_back(controller);
////	}
////}
//
//void InteractionHandler::setCamera(Camera *camera) {
//	//assert(this_);
//	_camera = camera;
//}
//
//Camera * InteractionHandler::getCamera() const {
//	return _camera;
//}
//
//const psc InteractionHandler::getOrigin() const {
//	if(_node)
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
//}
//
//void InteractionHandler::orbit(const glm::quat &rotation) {
//	//assert(this_);
//	lockControls();
//	
//	// the camera position
//	psc relative = _camera->position();
//
//	// should be changed to something more dynamic =)
//	psc origin;
//	if (_node) {
//		origin = _node->worldPosition();
//	}
//
//	psc relative_origin_coordinate = relative - origin;
//	//glm::mat4 rotation_matrix = glm::mat4_cast(glm::inverse(rotation));
//	//relative_origin_coordinate = relative_origin_coordinate.vec4() * glm::inverse(rotation);
//	relative_origin_coordinate = glm::inverse(rotation) * relative_origin_coordinate.vec4();
//	relative = relative_origin_coordinate + origin;
//
//	_camera->setPosition(relative);
//	//camera_->rotate(rotation);
//	//camera_->setRotation(glm::mat4_cast(rotation));
//
//	glm::mat4 la = glm::lookAt(_camera->position().vec3(), origin.vec3(), glm::rotate(rotation, _camera->lookUpVector()));
//	_camera->setRotation(la);
//	//camera_->setLookUpVector();
//
//	unlockControls();
//}
//
//void InteractionHandler::distance(const PowerScaledScalar &distance) {
//	//assert(this_);
//	lockControls();
//	
//	psc relative = _camera->position();
//	const psc origin = (_node) ? _node->worldPosition() : psc();
//
//	psc relative_origin_coordinate = relative - origin;
//	const glm::vec3 dir(relative_origin_coordinate.direction());
//	glm:: vec3 newdir = dir * distance[0];
//	relative_origin_coordinate = newdir;
//	relative_origin_coordinate[3] = distance[1];
//	relative = relative + relative_origin_coordinate;
//
//	relative_origin_coordinate = relative - origin;
//	newdir = relative_origin_coordinate.direction();
//
//	// update only if on the same side of the origin
//	if(glm::angle(newdir, dir) < 90.0f)
//		_camera->setPosition(relative);
//	
//	unlockControls();
//}
//
//void InteractionHandler::lookAt(const glm::quat &rotation) {
//	//assert(this_);
//	lockControls();
//
//	unlockControls();
//}
//
//void InteractionHandler::setRotation(const glm::quat &rotation) {
//	//assert(this_);
//	lockControls();
//
//	unlockControls();
//}
//
//void InteractionHandler::update(const double dt) {
//	//assert(this_);
//	// setting dt_ for use in callbacks
//	_dt = dt;
//	if (_camera) {
//		// fetch data from joysticks
//		DeviceIdentifier::ref().update();
//
//		//// update all controllers
//		//for (size_t i = 0; i < _controllers.size(); ++i) {
//		//	_controllers[i]->update();
//		//}
//	}
//}
//
//double InteractionHandler::getDt() {
//	//assert(this_);
//	return _dt;
//}
//
//glm::vec3 InteractionHandler::mapToTrackball(glm::vec2 mousePos) {
//	const float RADIUS = 0.5; // Sphere radius
//	glm::vec3 out = glm::vec3(mousePos.x-0.5, -1.0*(mousePos.y-0.5), 0);
//
//	// Mapping according to Holroyds trackball
//	// Piece-wise sphere + hyperbolic sheet
//	if (out.x*out.x + out.y*out.y <= RADIUS*RADIUS/2.0) {
//		//Spherical Region
//		out.z = RADIUS*RADIUS - (out.x*out.x + out.y*out.y);
//		out.z = out.z > 0.0 ? sqrtf(out.z) : 0.0;
//	} else { //Hyperbolic Region - for smooth z values
//		out.z = (RADIUS*RADIUS)/(2.0*sqrt(out.x*out.x + out.y*out.y));
//	}
//
//	return glm::normalize(out);
//}
//
//glm::vec3 InteractionHandler::mapToCamera(glm::vec3 trackballPos) {
////	return glm::vec3((sgct::Engine::instance()->getActiveViewMatrix() * glm::vec4(trackballPos,0)));
//
//    //Get x,y,z axis vectors of current camera view
//	glm::vec3 currentViewYaxis = glm::normalize(_camera->lookUpVector());
//	psc viewDir = _camera->position() - _node->worldPosition();
//	glm::vec3 currentViewZaxis = glm::normalize(viewDir.vec3());
//	glm::vec3 currentViewXaxis = glm::normalize(glm::cross(currentViewYaxis, currentViewZaxis));
//
//    //mapping to camera co-ordinate
//    currentViewXaxis*=trackballPos.x;
//    currentViewYaxis*=trackballPos.y;
//    currentViewZaxis*=trackballPos.z;
//    return (currentViewXaxis + currentViewYaxis + currentViewZaxis);
//}
//
//void InteractionHandler::trackballRotate(int x, int y) {
//	// Normalize mouse coordinates to [0,1]
//	float width = sgct::Engine::instance()->getActiveXResolution();
//	float height = sgct::Engine::instance()->getActiveYResolution();
//	glm::vec2 mousePos = glm::vec2((float)x/width, (float)y/height);
//
//	mousePos = glm::clamp(mousePos, -0.5, 1.5); // Ugly fix #1: Camera position becomes NaN on mouse values outside [-0.5, 1.5]
//    //mousePos[1] = 0.5; 							// Ugly fix #2: Tempoarily only allow rotation around y
//
//	glm::vec3 curTrackballPos = mapToTrackball(mousePos);
////	LDEBUG(mousePos.x << ", " << mousePos.y << " = " << curTrackballPos.x << ", " << curTrackballPos.y << ", " << curTrackballPos.z);
//
//	// Disable movement on the first click for extra smoothness
//	if (!_isMouseBeingPressedAndHeld) {
//		_lastTrackballPos = curTrackballPos;
//		_isMouseBeingPressedAndHeld = true;
//	}
//
//	if (curTrackballPos != _lastTrackballPos) {
//		// calculate rotation angle (in radians)
//		float rotationAngle = glm::angle(curTrackballPos, _lastTrackballPos);
//		rotationAngle *= getDt()*100.0f;
//
//        // Map trackballpos to camera
////		glm::vec3 trackballMappedToCamera = mapToCamera(_lastTrackballPos - curTrackballPos);
////		psc currentCamPos = camera_->getPosition();
////		glm::vec3 nextCamPos = currentCamPos.getVec3f() + trackballMappedToCamera;
////		glm::vec3 rotationAxis = glm::cross(currentCamPos.getVec3f(), nextCamPos);
//
//		glm::vec3 rotationAxis = glm::cross(_lastTrackballPos, curTrackballPos);
//		rotationAxis = glm::normalize(rotationAxis);
//		glm::quat quaternion = glm::angleAxis(rotationAngle, rotationAxis);
//
//		// Apply quaternion to camera
//		orbit(quaternion);
//
//		_lastTrackballPos = curTrackballPos;
//	}
//}
//double acc = 1;
//
//void InteractionHandler::keyboardCallback(int key, int action) {
//    // TODO package in script
//    const double speed = 2.75;
//    const double dt = getDt();
//    if(action == SGCT_PRESS || action == SGCT_REPEAT) {
//	    if (key == SGCT_KEY_S) {
//	        glm::vec3 euler(speed * dt, 0.0, 0.0);
//	        glm::quat rot = glm::quat(euler);
//	        orbit(rot);
//	    }
//	    if (key == SGCT_KEY_W) {
//	        glm::vec3 euler(-speed * dt, 0.0, 0.0);
//	        glm::quat rot = glm::quat(euler);
//	        orbit(rot);
//	    }
//	    if (key == SGCT_KEY_A) {
//	        glm::vec3 euler(0.0, -speed * dt, 0.0);
//	        glm::quat rot = glm::quat(euler);
//	        orbit(rot);
//	    }
//	    if (key == SGCT_KEY_D) {
//	        glm::vec3 euler(0.0, speed * dt, 0.0);
//	        glm::quat rot = glm::quat(euler);
//	        orbit(rot);
//	    }
//		if (key == SGCT_KEY_Q) {
//			Time::ref().advanceTime(dt);
//		}
//	    if (key == 262) {
//	        glm::vec3 euler(0.0, speed * dt, 0.0);
//	        glm::quat rot = glm::quat(euler);
//	        rotate(rot);
//	    }
//	    if (key == 263) {
//	        glm::vec3 euler(0.0, -speed * dt, 0.0);
//	        glm::quat rot = glm::quat(euler);
//	        rotate(rot);
//	    }
//	    if (key == 264) {
//	        glm::vec3 euler(speed * dt, 0.0, 0.0);
//	        glm::quat rot = glm::quat(euler);
//	        rotate(rot);
//	    }
//	    if (key == 265) {
//	        glm::vec3 euler(-speed * dt, 0.0, 0.0);
//	        glm::quat rot = glm::quat(euler);
//	        rotate(rot);
//	    }
//	    if (key == SGCT_KEY_R) {
//	        PowerScaledScalar dist(-speed * dt, 0.0);
//	        distance(dist);
//	    }
//	    if (key == SGCT_KEY_F) {
//	        PowerScaledScalar dist(speed * dt, 0.0);
//	        distance(dist);
//		}
//		if (key == SGCT_KEY_T) {
//			PowerScaledScalar dist(-speed * pow(10, 11) * dt, 0.0);
//			distance(dist);
//		}
//		if (key == SGCT_KEY_G) {
//			acc += 0.001;
//			PowerScaledScalar dist(speed * pow(10, 8 * acc) * dt, 0.0);
//			distance(dist);
//		}
//		if (key == SGCT_KEY_Y) {
//			PowerScaledScalar dist(-speed * 100.0 * dt, 6.0);
//			distance(dist);
//		}
//		if (key == SGCT_KEY_H) {
//			PowerScaledScalar dist(speed * 100.0 * dt, 6.0);
//			distance(dist);
//		}
//
//		if (key == SGCT_KEY_KP_SUBTRACT) {
//			glm::vec2 s = OsEng.renderEngine().camera()->scaling();
//			s[1] -= 0.5;
//			OsEng.renderEngine().camera()->setScaling(s);
//		}
//		if (key == SGCT_KEY_KP_ADD) {
//			glm::vec2 s = OsEng.renderEngine().camera()->scaling();
//			s[1] += 0.5;
//			OsEng.renderEngine().camera()->setScaling(s);
//		}
//	}
//    /*
//    if (key == '1') {
//        SceneGraphNode* node = getSceneGraphNode("sun");
//
//        setFocusNode(node);
//        getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 0.5, 10.0));
//        getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
//    }
//
//    if (key == '2') {
//        SceneGraphNode* node = getSceneGraphNode("earth");
//
//        setFocusNode(node);
//        getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 1.0, 8.0));
//        getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
//    }
//
//
//    if (key == '3') {
//        SceneGraphNode* node = getSceneGraphNode("moon");
//
//        setFocusNode(node);
//        getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 0.5, 8.0));
//        getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
//    }
//*/
//    // std::pair <std::multimap<int,std::function<void(void)> >::iterator, std::multimap<int , std::function<void(void)> >::iterator> ret;
//    if(action == SGCT_PRESS) {
//    	auto ret = _keyCallbacks.equal_range(key);
//	    for (auto it=ret.first; it!=ret.second; ++it)
//	    	it->second();
//    }
//    
//
//}
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
//    const double speed = 4.75;
//    const double dt = getDt();
//    if(pos < 0) {
//	    PowerScaledScalar dist(speed * dt, 0.0);
//	    distance(dist);
//    } else if(pos > 0) {
//	    PowerScaledScalar dist(-speed * dt, 0.0);
//	    distance(dist);
//    }
//}
//
////void InteractionHandler::addKeyCallback(int key, std::function<void(void)> f) {
////	//std::map<int, std::vector<std::function<void(void)> > > _keyCallbacks;
////
////	_keyCallbacks.insert(std::make_pair(key, f));
////}
//
//} // namespace interaction
//} // namespace openspace
