#ifndef RANDOMEXTERNALCONTROL_H
#define RANDOMEXTERNALCONTROL_H

#include <thread>
#include <mutex>

#include "interaction/externalcontrol/externalcontrol.h"

namespace openspace {

class RandomExternalControl: public ExternalControl {
public:

	// constructors & destructor
	RandomExternalControl();
	~RandomExternalControl();
	
private:
	std::mutex inputGuard;
	bool *keepGoing_;
	std::thread *backgroundThread;
};

} // namespace openspace

#endif