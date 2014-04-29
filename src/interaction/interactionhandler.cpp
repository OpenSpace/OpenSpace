
// open space includes
#include <ghoul/logging/logmanager.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/interaction/deviceidentifier.h>
#include <openspace/interaction/externalcontrol/randomexternalcontrol.h>
#include <openspace/interaction/externalcontrol/joystickexternalcontrol.h>
#include <openspace/query/query.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/util/psc.h>
#include <glm/gtx/vector_angle.hpp>

// std includes
#include <cassert>

std::string _loggerCat = "InteractionHandler";

namespace openspace {

InteractionHandler::InteractionHandler() {

	// initiate pointers
	camera_ = nullptr;
	enabled_ = true;
	node_ = nullptr;
	dt_ = 0.0;
	_lastTrackballPos = glm::vec3(0.5);
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
	if (controller != nullptr)
	{
		controllers_.push_back(controller);
	}
}

void InteractionHandler::setCamera(Camera *camera) {
	//assert(this_);
	camera_ = camera;
}

Camera * InteractionHandler::getCamera() const {
	//assert(this_);
	if (enabled_)
	{
		return camera_;
	}
	return nullptr;
}

const psc InteractionHandler::getOrigin() const {
	if(node_)
		return node_->getWorldPosition();
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
	psc relative = camera_->getPosition();

	// should be changed to something more dynamic =)
	psc origin;
	if(node_) {
		origin = node_->getWorldPosition();
	}

	psc relative_origin_coordinate = relative - origin;
	glm::mat4 rotation_matrix = glm::mat4_cast(rotation);
	relative_origin_coordinate = relative_origin_coordinate.mul(rotation_matrix);
	relative = relative_origin_coordinate + origin;

	camera_->setPosition(relative);
	
	unlockControls();
}

void InteractionHandler::distance(const pss &distance) {
	//assert(this_);
	lockControls();

	
	psc relative = camera_->getPosition();
	psc origin;
	if(node_) {
		origin = node_->getWorldPosition();
	}

	psc relative_origin_coordinate = relative - origin;
	glm::dvec3 dir = relative_origin_coordinate.getDirection();
	dir = dir * distance[0];
	relative_origin_coordinate = dir;
	relative_origin_coordinate[3] = distance[1];
	relative = relative + relative_origin_coordinate;

	camera_->setPosition(relative);
	
	unlockControls();
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

void InteractionHandler::trackballRotate(int x, int y) {
	// Normalize mouse coordinates to [0,1]
	float width = sgct::Engine::instance()->getActiveXResolution();
	float height = sgct::Engine::instance()->getActiveYResolution();
	glm::vec2 mousePos = glm::vec2((float)x/width, (float)y/height);
    mousePos[1] = 0.5;

	glm::vec3 curTrackballPos = mapToTrackball(mousePos);
//	LDEBUG(mousePos.x << ", " << mousePos.y << " = " << curTrackballPos.x << ", " << curTrackballPos.y << ", " << curTrackballPos.z);

	// Disable movement on the first click for extra smoothness
	if (!_isMouseBeingPressedAndHeld) {
		_lastTrackballPos = curTrackballPos;
		_isMouseBeingPressedAndHeld = true;
	}

	if (curTrackballPos != _lastTrackballPos) {
		// calculate rotation angle (in radians), rotation axis and quaternion
		float rotationAngle = glm::angle(curTrackballPos, _lastTrackballPos);
        rotationAngle *= getDt()*100.0f;
		glm::vec3 rotationAxis = glm::cross(_lastTrackballPos, curTrackballPos);
		rotationAxis = glm::normalize(rotationAxis);
		glm::quat quaternion = glm::angleAxis(rotationAngle, rotationAxis);

		// -----------------------------------------------------------------

		orbit(glm::inverse(quaternion));
		camera_->rotate(quaternion);
//		camera_->compileViewRotationMatrix();
/*
		psc nodePos = node_->getWorldPosition();
		psc camPos = camera_->getPosition();
		glm::mat4 transMatrix = glm::translate(glm::mat4(1.0), nodePos.getVec3f()-camPos.getVec3f());
		glm::mat4 transBackMatrix = glm::translate(glm::mat4(1.0), camPos.getVec3f()-nodePos.getVec3f());
		glm::vec4 translated = transMatrix*glm::vec4(camPos.getVec3f(), 1.0);
		glm::vec4 postRot = glm::rotate(quaternion, translated);
		glm::vec4 newCamPos = transBackMatrix*glm::vec4(postRot.x, postRot.y, postRot.z, 1.0);
		camera_->setPosition(psc::CreatePSC(newCamPos.x, newCamPos.y, newCamPos.z));
		camera_->rotate(glm::inverse(quaternion));
		camera_->compileViewRotationMatrix();
        */
//		node_->calculateBoundingSphere();

//		glm::vec3 camPos = *sgct::Engine::instance()->getUserPtr()->getPosPtr();
//		sgct::Engine::instance()->getUserPtr()->setOrientation(quaternion.w, quaternion.x, quaternion.y, quaternion.z);
//		sgct::Engine::instance()->getUserPtr()->setPos(glm::rotate(quaternion, camPos));


		_lastTrackballPos = curTrackballPos;
	}
}

void InteractionHandler::keyboardCallback(int key, int action) {
    // TODO package in script
    const double speed = 2.75;
    const double dt = getDt();
    if (key == 'S') {
        glm::vec3 euler(speed * dt, 0.0, 0.0);
        glm::quat rot = glm::quat(euler);
        orbit(rot);
    }
    if (key == 'W') {
        glm::vec3 euler(-speed * dt, 0.0, 0.0);
        glm::quat rot = glm::quat(euler);
        orbit(rot);
    }
    if (key == 'A') {
        glm::vec3 euler(0.0, -speed * dt, 0.0);
        glm::quat rot = glm::quat(euler);
        orbit(rot);
    }
    if (key == 'D') {
        glm::vec3 euler(0.0, speed * dt, 0.0);
        glm::quat rot = glm::quat(euler);
        orbit(rot);
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
    if (key == 'R') {
        pss dist(-speed * dt, 0.0);
        distance(dist);
    }
    if (key == 'F') {
        pss dist(speed * dt, 0.0);
        distance(dist);
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
}

void InteractionHandler::mouseButtonCallback(int key, int action) {
    //if(mouseControl_ != nullptr) {
    //	mouseControl_->mouseButtonCallback(key,action);
    //}
	if (key == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_PRESS)
		_leftMouseButtonDown = true;
	else if (key == GLFW_MOUSE_BUTTON_LEFT && action == GLFW_RELEASE) {
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
}


} // namespace openspace
