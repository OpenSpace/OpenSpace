
// open space includes
#include "interactionhandler.h"
#include "deviceidentifier.h"
#include "externalcontrol/randomexternalcontrol.h"
#include "externalcontrol/joystickexternalcontrol.h"

// std includes
#include <cassert>

namespace openspace {

InteractionHandler* InteractionHandler::this_ = nullptr;

InteractionHandler::InteractionHandler() {

	// initiate pointers
	camera_ = nullptr;
	enabled_ = true;
	node_ = nullptr;
	dt_ = 0.0;
}

InteractionHandler::~InteractionHandler() {
	for (size_t i = 0; i < controllers_.size(); ++i)
	{
		delete controllers_[i];
	}
}

void InteractionHandler::init() {
	assert( ! this_);
	 this_ = new InteractionHandler();
}

void InteractionHandler::deinit() {
	assert(this_);
	delete this_;
	this_ = nullptr;
}

InteractionHandler& InteractionHandler::ref() {
	assert(this_);
    return *this_;
}

bool InteractionHandler::isInitialized() {
	return this_ != nullptr;
}	

void InteractionHandler::enable() {
	assert(this_);
	enabled_ = true;
}

void InteractionHandler::disable() {
	assert(this_);
	enabled_ = false;
}

const bool InteractionHandler::isEnabled() const {
	assert(this_);
	if (camera_)
		return false;
	return enabled_;
}

void InteractionHandler::connectDevices() {
	assert(this_);
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
	assert(this_);
	if (controller != nullptr)
	{
		controllers_.push_back(controller);
	}
}

void InteractionHandler::setCamera(Camera *camera) {
	assert(this_);
	camera_ = camera;
}

Camera * InteractionHandler::getCamera() const {
	assert(this_);
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
	assert(this_);
	cameraGuard_.lock();
}

void InteractionHandler::unlockControls() {
	assert(this_);
	cameraGuard_.unlock();
}

void InteractionHandler::setFocusNode(SceneGraphNode *node) {
	assert(this_);
	node_ = node;
}

void InteractionHandler::rotate(const glm::quat &rotation) {
	assert(this_);
	lockControls();
	camera_->rotate(rotation);
	unlockControls();
}

void InteractionHandler::orbit(const glm::quat &rotation) {
	assert(this_);
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
	assert(this_);
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
	assert(this_);
	lockControls();

	unlockControls();
}

void InteractionHandler::setRotation(const glm::quat &rotation) {
	assert(this_);
	lockControls();

	unlockControls();
}

void InteractionHandler::update(const double dt) {
	assert(this_);

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
	assert(this_);
	return dt_;
}

} // namespace openspace