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

// open space includes
#include <ghoul/logging/logmanager.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/deviceidentifier.h>
#include <openspace/interaction/externalcontrol/randomexternalcontrol.h>
#include <openspace/interaction/externalcontrol/joystickexternalcontrol.h>
#include <openspace/query/query.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <glm/gtx/vector_angle.hpp>

#include <openspace/util/time.h>

// std includes
#include <cassert>

std::string _loggerCat = "InteractionHandler";

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

	OsEng.interactionHandler().setOrigin(node);

	return 0;
}

} // namespace luascriptfunctions

InteractionHandler::InteractionHandler() {
	// initiate pointers
	camera_ = nullptr;
	enabled_ = true;
	node_ = nullptr;
	dt_ = 0.0;
	_lastTrackballPos = glm::vec3(0.0, 0.0, 0.5);
	_leftMouseButtonDown = false;
	_isMouseBeingPressedAndHeld = false;
}

InteractionHandler::~InteractionHandler() {
	for (size_t i = 0; i < controllers_.size(); ++i) {
		delete controllers_[i];
	}
}

//void InteractionHandler::init() {
//	assert( ! this_);
//	 this_ = new InteractionHandler();
//}
//
//void InteractionHandler::deinit() {
//	assert(this_);
//	delete this_;
//	this_ = nullptr;
//}
//
//InteractionHandler& InteractionHandler::ref() {
//	assert(this_);
//    return *this_;
//}

//bool InteractionHandler::isInitialized() {
//	return this_ != nullptr;
//}	

void InteractionHandler::enable() {
	//assert(this_);
	enabled_ = true;
}

void InteractionHandler::disable() {
	//assert(this_);
	enabled_ = false;
}

const bool InteractionHandler::isEnabled() const {
	//assert(this_);
	if (camera_)
		return false;
	return enabled_;
}

void InteractionHandler::connectDevices() {
	//assert(this_);
	assert(DeviceIdentifier::ref().isInitialized());

	// for each device found
	for(int i = 0; i < DeviceIdentifier::ref().numberOfDevices(); ++i) {

        // TODO
		//if(DeviceIdentifier::ref().type(i) == InputDevice::XBOX) {

		//	// found xbox, use xbox python controller
		//	JoystickExternalControl *joystickexcontrol = new JoystickExternalControl(RELATIVE_PATH"pyinput/Xbox.py");
		//	joystickexcontrol->setInputDevice(i);
		//	addExternalControl(joystickexcontrol);

		//} else if(DeviceIdentifier::ref().type(i) == InputDevice::SPACENAVIGATOR) {

		//	// found SpaceNavigator, use SpaceNavigator python controller
		//	JoystickExternalControl *joystickexcontrol = new JoystickExternalControl(RELATIVE_PATH"pyinput/SpaceNavigator.py");
		//	joystickexcontrol->setInputDevice(i);
		//	addExternalControl(joystickexcontrol);
		//}
		
	}
}

void InteractionHandler::addExternalControl(ExternalControl* controller) {
	//assert(this_);
	if (controller != nullptr) {
		controllers_.push_back(controller);
	}
}

void InteractionHandler::setCamera(Camera *camera) {
	//assert(this_);
	camera_ = camera;
}

void InteractionHandler::setOrigin(SceneGraphNode* node) {
	if (node)
		node_ = node;
}

Camera * InteractionHandler::getCamera() const {
	//assert(this_);
	if (enabled_) {
		return camera_;
	}
	return nullptr;
}

const psc InteractionHandler::getOrigin() const {
	if(node_)
		return node_->worldPosition();
	return psc();
}

void InteractionHandler::lockControls() {
	//assert(this_);
	cameraGuard_.lock();
}

void InteractionHandler::unlockControls() {
	//assert(this_);
	cameraGuard_.unlock();
}

void InteractionHandler::setFocusNode(SceneGraphNode *node) {
	//assert(this_);
	node_ = node;
}

void InteractionHandler::rotate(const glm::quat &rotation) {
	//assert(this_);
	lockControls();
	camera_->rotate(rotation);
	unlockControls();
}

void InteractionHandler::orbit(const glm::quat &rotation) {
	//assert(this_);
	lockControls();
	
	// the camera position
	psc relative = camera_->position();

	// should be changed to something more dynamic =)
	psc origin;
	if (node_) {
		origin = node_->worldPosition();
	}

	psc relative_origin_coordinate = relative - origin;
	//glm::mat4 rotation_matrix = glm::mat4_cast(glm::inverse(rotation));
	//relative_origin_coordinate = relative_origin_coordinate.vec4() * glm::inverse(rotation);
	relative_origin_coordinate = glm::inverse(rotation) * relative_origin_coordinate.vec4();
	relative = relative_origin_coordinate + origin;

	camera_->setPosition(relative);
	//camera_->rotate(rotation);
	//camera_->setRotation(glm::mat4_cast(rotation));

	glm::mat4 la = glm::lookAt(camera_->position().vec3(), origin.vec3(), glm::rotate(rotation, camera_->lookUpVector()));
	camera_->setRotation(la);
	//camera_->setLookUpVector();

	unlockControls();
}

void InteractionHandler::distance(const PowerScaledScalar &dist, size_t iterations) {
	if (iterations > 5)
		return;
	//assert(this_);
	lockControls();
	
	psc relative = camera_->position();
	const psc origin = (node_) ? node_->worldPosition() : psc();

	psc relative_origin_coordinate = relative - origin;
	const glm::vec3 dir(relative_origin_coordinate.direction());
	glm::vec3 newdir = dir * dist[0];
	relative_origin_coordinate = newdir;
	relative_origin_coordinate[3] = dist[1];
	relative = relative + relative_origin_coordinate;

	relative_origin_coordinate = relative - origin;
	newdir = relative_origin_coordinate.direction();

	// update only if on the same side of the origin
	if (glm::angle(newdir, dir) < 90.0f) {
		camera_->setPosition(relative);
		unlockControls();

	}
	else {
		unlockControls();
		PowerScaledScalar d2 = dist;
		d2[0] *= 0.75;
		d2[1] *= 0.85;
		distance(d2, iterations + 1);
	}
	
}

void InteractionHandler::lookAt(const glm::quat &rotation) {
	//assert(this_);
	lockControls();

	unlockControls();
}

void InteractionHandler::setRotation(const glm::quat &rotation) {
	//assert(this_);
	lockControls();

	unlockControls();
}

void InteractionHandler::update(const double dt) {
	//assert(this_);
	// setting dt_ for use in callbacks
	dt_ = dt;
	if (enabled_ && camera_) {
		// fetch data from joysticks
		DeviceIdentifier::ref().update();

		// update all controllers
		for (size_t i = 0; i < controllers_.size(); ++i) {
			controllers_[i]->update();
		}
	}
}

double InteractionHandler::getDt() {
	//assert(this_);
	return dt_;
}

glm::vec3 InteractionHandler::mapToTrackball(glm::vec2 mousePos) {
	const float RADIUS = 0.5; // Sphere radius
	glm::vec3 out = glm::vec3(mousePos.x-0.5, -1.0*(mousePos.y-0.5), 0);

	// Mapping according to Holroyds trackball
	// Piece-wise sphere + hyperbolic sheet
	if (out.x*out.x + out.y*out.y <= RADIUS*RADIUS/2.0) {
		//Spherical Region
		out.z = RADIUS*RADIUS - (out.x*out.x + out.y*out.y);
		out.z = out.z > 0.0 ? sqrtf(out.z) : 0.0;
	} else { //Hyperbolic Region - for smooth z values
		out.z = (RADIUS*RADIUS)/(2.0*sqrt(out.x*out.x + out.y*out.y));
	}

	return glm::normalize(out);
}

glm::vec3 InteractionHandler::mapToCamera(glm::vec3 trackballPos) {
//	return glm::vec3((sgct::Engine::instance()->getActiveViewMatrix() * glm::vec4(trackballPos,0)));

    //Get x,y,z axis vectors of current camera view
	glm::vec3 currentViewYaxis = glm::normalize(camera_->lookUpVector());
	psc viewDir = camera_->position() - node_->worldPosition();
	glm::vec3 currentViewZaxis = glm::normalize(viewDir.vec3());
	glm::vec3 currentViewXaxis = glm::normalize(glm::cross(currentViewYaxis, currentViewZaxis));

    //mapping to camera co-ordinate
    currentViewXaxis*=trackballPos.x;
    currentViewYaxis*=trackballPos.y;
    currentViewZaxis*=trackballPos.z;
    return (currentViewXaxis + currentViewYaxis + currentViewZaxis);
}

void InteractionHandler::trackballRotate(int x, int y) {
	// Normalize mouse coordinates to [0,1]
	float width = sgct::Engine::instance()->getActiveXResolution();
	float height = sgct::Engine::instance()->getActiveYResolution();
	glm::vec2 mousePos = glm::vec2((float)x/width, (float)y/height);

	mousePos = glm::clamp(mousePos, -0.5, 1.5); // Ugly fix #1: Camera position becomes NaN on mouse values outside [-0.5, 1.5]
    //mousePos[1] = 0.5; 							// Ugly fix #2: Tempoarily only allow rotation around y

	glm::vec3 curTrackballPos = mapToTrackball(mousePos);
//	LDEBUG(mousePos.x << ", " << mousePos.y << " = " << curTrackballPos.x << ", " << curTrackballPos.y << ", " << curTrackballPos.z);

	// Disable movement on the first click for extra smoothness
	if (!_isMouseBeingPressedAndHeld) {
		_lastTrackballPos = curTrackballPos;
		_isMouseBeingPressedAndHeld = true;
	}

	if (curTrackballPos != _lastTrackballPos) {
		// calculate rotation angle (in radians)
		float rotationAngle = glm::angle(curTrackballPos, _lastTrackballPos);
		rotationAngle *= getDt()*100.0f;

        // Map trackballpos to camera
//		glm::vec3 trackballMappedToCamera = mapToCamera(_lastTrackballPos - curTrackballPos);
//		psc currentCamPos = camera_->getPosition();
//		glm::vec3 nextCamPos = currentCamPos.getVec3f() + trackballMappedToCamera;
//		glm::vec3 rotationAxis = glm::cross(currentCamPos.getVec3f(), nextCamPos);

		glm::vec3 rotationAxis = glm::cross(_lastTrackballPos, curTrackballPos);
		rotationAxis = glm::normalize(rotationAxis);
		glm::quat quaternion = glm::angleAxis(rotationAngle, rotationAxis);

		// Apply quaternion to camera
		orbit(quaternion);

		_lastTrackballPos = curTrackballPos;
	}
}

void InteractionHandler::keyboardCallback(int key, int action) {
    // TODO package in script
    const double speed = 2.75;
    const double dt = getDt();
	if (action == SGCT_PRESS || action == SGCT_REPEAT) {
	    if (key == SGCT_KEY_S) {
	        glm::vec3 euler(speed * dt, 0.0, 0.0);
	        glm::quat rot = glm::quat(euler);
	        orbit(rot);
	    }
	    if (key == SGCT_KEY_W) {
	        glm::vec3 euler(-speed * dt, 0.0, 0.0);
	        glm::quat rot = glm::quat(euler);
	        orbit(rot);
	    }
	    if (key == SGCT_KEY_A) {
	        glm::vec3 euler(0.0, -speed * dt, 0.0);
	        glm::quat rot = glm::quat(euler);
	        orbit(rot);
	    }
	    if (key == SGCT_KEY_D) {
	        glm::vec3 euler(0.0, speed * dt, 0.0);
	        glm::quat rot = glm::quat(euler);
	        orbit(rot);
	    }
		if (key == SGCT_KEY_Q) {
			Time::ref().advanceTime(dt);
		}
	    if (key == 262) {
	        glm::vec3 euler(0.0, speed * dt, 0.0);
	        glm::quat rot = glm::quat(euler);
	        rotate(rot);
	    }
	    if (key == 263) {
	        glm::vec3 euler(0.0, -speed * dt, 0.0);
	        glm::quat rot = glm::quat(euler);
	        rotate(rot);
	    }
	    if (key == 264) {
	        glm::vec3 euler(speed * dt, 0.0, 0.0);
	        glm::quat rot = glm::quat(euler);
	        rotate(rot);
	    }
	    if (key == 265) {
	        glm::vec3 euler(-speed * dt, 0.0, 0.0);
	        glm::quat rot = glm::quat(euler);
	        rotate(rot);
	    }
	    if (key == SGCT_KEY_R) {
	        PowerScaledScalar dist(-speed * dt, 5.0);
	        distance(dist);
	    }
	    if (key == SGCT_KEY_F) {
	        PowerScaledScalar dist(speed * dt, 5.0);
	        distance(dist);
		}
		if (key == SGCT_KEY_T) {
			PowerScaledScalar dist(-speed  * dt, 10.0);
			distance(dist);
		}
		if (key == SGCT_KEY_G) {
			PowerScaledScalar dist(speed * dt, 10.0);
			distance(dist);
		}
		if (key == SGCT_KEY_Y) {
			PowerScaledScalar dist(-speed * dt, 11.5);
			distance(dist);
		}
		if (key == SGCT_KEY_H) {
			PowerScaledScalar dist(speed  * dt, 11.5);
			distance(dist);
		}
		if (key == SGCT_KEY_U) {
			PowerScaledScalar dist(-speed * dt, 13.0);
			distance(dist);
		}
		if (key == SGCT_KEY_J) {
			PowerScaledScalar dist(speed  * dt, 13.0);
			distance(dist);
		}

		if (key == SGCT_KEY_KP_SUBTRACT) {
			glm::vec2 s = OsEng.renderEngine().camera()->scaling();
			s[1] -= 0.5;
			OsEng.renderEngine().camera()->setScaling(s);
		}
		if (key == SGCT_KEY_KP_ADD) {
			glm::vec2 s = OsEng.renderEngine().camera()->scaling();
			s[1] += 0.5;
			OsEng.renderEngine().camera()->setScaling(s);
		}
	}

	// Screenshot
	if (action == SGCT_PRESS && key == SGCT_KEY_PRINT_SCREEN) {
		OsEng.renderEngine().takeScreenshot();
	}

	
    /*
    if (key == '1') {
        SceneGraphNode* node = getSceneGraphNode("sun");

        setFocusNode(node);
        getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 0.5, 10.0));
        getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
    }

    if (key == '2') {
        SceneGraphNode* node = getSceneGraphNode("earth");

        setFocusNode(node);
        getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 1.0, 8.0));
        getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
    }


    if (key == '3') {
        SceneGraphNode* node = getSceneGraphNode("moon");

        setFocusNode(node);
        getCamera()->setPosition(node->getWorldPosition() + psc(0.0, 0.0, 0.5, 8.0));
        getCamera()->setCameraDirection(glm::vec3(0.0, 0.0, -1.0));
    }
*/
    // std::pair <std::multimap<int,std::function<void(void)> >::iterator, std::multimap<int , std::function<void(void)> >::iterator> ret;
    if(action == SGCT_PRESS) {
    	auto ret = _keyCallbacks.equal_range(key);
	    for (auto it=ret.first; it!=ret.second; ++it)
	    	it->second();
    }
    

}

void InteractionHandler::mouseButtonCallback(int key, int action) {
    //if(mouseControl_ != nullptr) {
    //	mouseControl_->mouseButtonCallback(key,action);
    //}
	if (key == SGCT_MOUSE_BUTTON_LEFT && action == SGCT_PRESS)
		_leftMouseButtonDown = true;
	else if (key == SGCT_MOUSE_BUTTON_LEFT && action == SGCT_RELEASE) {
		_leftMouseButtonDown = false;
		_isMouseBeingPressedAndHeld = false;
	}
}

void InteractionHandler::mousePositionCallback(int x, int y) {
	if (_leftMouseButtonDown)
		trackballRotate(x,y);

    //if(mouseControl_ != nullptr) {
    //	mouseControl_->mousePosCallback(x,y);
    //}
}

void InteractionHandler::mouseScrollWheelCallback(int pos) {
    //if(mouseControl_ != nullptr) {
    //	mouseControl_->mouseScrollCallback(pos);
    //}
    const double speed = 4.75;
    const double dt = getDt();
    if(pos < 0) {
	    PowerScaledScalar dist(speed * dt, 0.0);
	    distance(dist);
    } else if(pos > 0) {
	    PowerScaledScalar dist(-speed * dt, 0.0);
	    distance(dist);
    }
}

void InteractionHandler::addKeyCallback(int key, std::function<void(void)> f) {
	//std::map<int, std::vector<std::function<void(void)> > > _keyCallbacks;

	_keyCallbacks.insert(std::make_pair(key, f));
}

scripting::ScriptEngine::LuaLibrary InteractionHandler::luaLibrary() {
	return{
		"",
		{
			{
				"setOrigin",
				&luascriptfunctions::setOrigin,
				"setOrigin(): set the camera origin node by name"
			}
		}
	};

}

} // namespace openspace
