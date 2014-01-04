#include "externalcontrol/externalcontrol.h"
#include "interactionhandler.h"
#include <cstdio>

namespace openspace {
	
ExternalControl::ExternalControl() {

}

ExternalControl::~ExternalControl() {
}

void ExternalControl::update() {

}

void ExternalControl::rotate(const glm::quat &rotation) {
	InteractionHandler::ref().rotate(rotation);
}

void ExternalControl::orbit(const glm::quat &rotation) {
	InteractionHandler::ref().orbit(rotation);
}

void ExternalControl::distance(const pss &distance) {
	InteractionHandler::ref().distance(distance);
}


} // namespace openspace

