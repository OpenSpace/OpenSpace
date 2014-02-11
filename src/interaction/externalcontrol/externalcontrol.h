#ifndef EXTERNALCONTROL_H
#define EXTERNALCONTROL_H

#include "util/pss.h"
#include <glm/glm.hpp>
#include <glm/gtc/quaternion.hpp>

namespace openspace {

class ExternalControl {
public:

	// constructors & destructor
	ExternalControl();
	virtual ~ExternalControl();
	
	virtual void update();
	
	void rotate(const glm::quat &rotation);
	void orbit(const glm::quat &rotation);
	void distance(const pss &distance);

	
protected:
};

} // namespace openspace

#endif