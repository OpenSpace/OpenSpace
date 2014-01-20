
// openspace stuff
#include "util/Spice.h"
#include "util/Time.h"
#include "util/Psc.h"
#include "interaction/interactionHandler.h"

// spice
#include "SpiceUsr.h"

// other
#include <cstdio>
#include <cassert>
#include "ghoul/logging/logmanager.h"
#include "ghoul/logging/consolelog.h"
#include <ghoul/filesystem/filesystem.h>

namespace openspace {

#define lenout 81
#define shrtms_len 81

Spice* Spice::this_ = nullptr;

Spice::Spice() {

	// logger string
	std::string _loggerCat = "Spice::Spice()";

	// print spice toolkit version
	ConstSpiceChar  * versn;
	versn = tkvrsn_c( "TOOLKIT" );
	LINFO("Spice Toolkit version: " << versn);	

	// make the spice framework not exit on error
	erract_c (const_cast<char*>("SET"), lenout, const_cast<char*>("RETURN"));

	// make spice print the errors to a log file
	errdev_c (const_cast<char*>("SET"), lenout, const_cast<char*>("NULL") );

}

Spice::~Spice() {

}

void Spice::init() {
	assert( ! this_);
	this_ = new Spice();
}

void Spice::deinit() {
	assert(this_);
	delete this_;
	this_ = nullptr;
}

Spice& Spice::ref() {
	assert(this_);
    return *this_;
}

bool Spice::isInitialized() {
	return this_ != nullptr;
}

void Spice::loadDefaultKernels() {
	assert(this_);

	// load
	loadKernel(absPath("${BASE_PATH}/data/spice/de430_1850-2150.bsp"));
	//Summary for: de430_1850-2150.bsp
	//Bodies: MERCURY BARYCENTER (1)  SATURN BARYCENTER (6)   MERCURY (199)
	//        VENUS BARYCENTER (2)    URANUS BARYCENTER (7)   VENUS (299)
	//        EARTH BARYCENTER (3)    NEPTUNE BARYCENTER (8)  MOON (301)
	//        MARS BARYCENTER (4)     PLUTO BARYCENTER (9)    EARTH (399)
	//        JUPITER BARYCENTER (5)  SUN (10)
	//        Start of Interval (ET)              End of Interval (ET)
	//        -----------------------------       -----------------------------
	//        1849 DEC 26 00:00:00.000            2150 JAN 22 00:00:00.000
	
	loadKernel(absPath("${BASE_PATH}/data/spice/pck00010.tpc"));
}

bool Spice::loadKernel(const std::string &path) {
	assert(this_);

	// ghoul logging
	std::string _loggerCat = "Spice::loadKernel";
	
	furnsh_c ( path.c_str() );
	int failed = failed_c();
	if(failed) {
		char shrtms[shrtms_len];
		getmsg_c ( "SHORT", shrtms_len, shrtms );
		LERROR("Error when loading kernel with path: " << path);
		LERROR("Spice reported: " << shrtms);
		reset_c();
	}
	return ( ! failed);
}

void Spice::bod_NameToInt(const std::string & name, int *id, int *success) {
	assert(this_);
	bodn2c_c (name.c_str(), id, success);
}

bool Spice::getRadii(const std::string & name, double radii[3], int *n) {
	assert(this_);
	
	// ghoul logging
	std::string _loggerCat = "Spice::getRadii";

	bodvrd_c (name.c_str(), "RADII", 3, n, radii );
	int failed = failed_c();
	if(failed) {
		char shrtms[shrtms_len];
		getmsg_c ( "SHORT", shrtms_len, shrtms );
		LERROR("Error when fetching radii");
		LERROR("Spice reported: " << shrtms);
		reset_c();
	}
	return ( ! failed);
}

// has error checking
bool Spice::spk_getPosition(const std::string &target, const std::string &origin, double state[3]) {
	assert(this_);
	assert(Time::ref().isInitialized());

	SpiceDouble   et = Time::ref().getTime();
	SpiceDouble   lt;
	spkpos_c(target.c_str(), et, "J2000", "LT+S", origin.c_str(), state,  &lt);
	int failed = failed_c();
	if(failed) {
		reset_c();
	}
	return ( ! failed);
}

// no error checking
void Spice::spk_getPosition(int target, int origin, double state[3]) {
	assert(this_);
	assert(Time::ref().isInitialized());

	SpiceDouble   et = Time::ref().getTime();
	SpiceDouble   lt;
	spkezp_c  (target, et, "J2000", "NONE", origin, state,  &lt);
}

// has error checking
bool Spice::spk_getOrientation(const std::string &target, double state[3][3]) {
	assert(this_);
	assert(Time::ref().isInitialized());
	
	// ghoul logging
	std::string _loggerCat = "Spice::spk_getOrientation";

	SpiceDouble et = Time::ref().getTime();
	std::string newname = "IAU_";
	newname += target;
	pxform_c ( "J2000", newname.c_str(), et, state );
	int failed = failed_c();
	if(failed) {
		char shrtms[shrtms_len];
		getmsg_c ( "SHORT", shrtms_len, shrtms );
		LERROR("Error when fetching orientation");
		LERROR("Spice reported: " << shrtms);
		reset_c();
	}
	return ( ! failed);
}

/*
The following example retrieves radii for Mars and computes
   orientation of the Mars body-fixed frame:

      SpiceDouble   et;
      SpiceDouble   mat[3][3];
      SpiceDouble   radii[3];
      SpiceInt      n;

      furnsh_c ( "naif0008.tls" );
      furnsh_c ( "pck00008.tpc" );

     //    retrieve Mars radii 
      bodvrd_c ( "MARS", "RADII", 3, &n, radii );

       //  convert UTC to ET 
      str2et_c ( "2005-DEC-28 12:00", &et );

       //  compute Mars orientation relative to the J2000 frame 
      pxform_c ( "J2000", "IAU_MARS", et, mat );
*/

} // namespace openspace
