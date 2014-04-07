#include <openspace/interaction/externalcontrol/externalcontrol.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/engine/openspaceengine.h>
#include <cstdio>

namespace openspace {
	
ExternalControl::ExternalControl() {

}

ExternalControl::~ExternalControl() {
}

void ExternalControl::update() {

}

void ExternalControl::rotate(const glm::quat &rotation) {
	OsEng.interactionHandler().rotate(rotation);
}

void ExternalControl::orbit(const glm::quat &rotation) {
	OsEng.interactionHandler().orbit(rotation);
}

void ExternalControl::distance(const pss &distance) {
	OsEng.interactionHandler().distance(distance);
}


} // namespace openspace

