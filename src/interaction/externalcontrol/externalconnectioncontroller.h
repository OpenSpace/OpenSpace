#ifndef EXTERNALCONNECTIONCONTROLLER_H
#define EXTERNALCONNECTIONCONTROLLER_H

#include "interaction/externalcontrol/externalcontrol.h"
#include <vector>

namespace openspace {

class ExternalConnectionController: public ExternalControl {
public:

	// constructors & destructor
	ExternalConnectionController();
	~ExternalConnectionController();
	
private:

	std::vector<ExternalControl*> controllers;
	
};

} // namespace openspace

#endif