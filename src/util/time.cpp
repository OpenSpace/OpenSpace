
// open space includes
#include "util/Time.h"
#include "InteractionHandler.h"

// std includes
#include <cassert>

// spice includes
#include "SpiceUsr.h"
#include <ghoul/filesystem/filesystem.h>

namespace openspace {

Time* Time::this_ = nullptr;

Time::Time() {
	time_ = 0.0;

	// load spice time kernel
	furnsh_c (p("${BASE_PATH}/data/spice/naif0010.tls").c_str());

	// convert UTC to ET 
	str2et_c ( "2006 JAN 31 01:00", &time_ );
}

Time::~Time() {

}

void Time::init() {
	assert( ! this_);
	 this_ = new Time();
}

void Time::deinit() {
	assert(this_);
	delete this_;
	this_ = nullptr;
}

Time& Time::ref() {
	assert(this_);
    return *this_;
}

bool Time::isInitialized() {
	return this_ != nullptr;
}

void Time::setTime(const char* stringTime) {
	assert(this_);
	// convert UTC to ET 
	str2et_c ( stringTime, &time_ );
}

double Time::getTime() {
	assert(this_);
	return time_;
}

} // namespace openspace
