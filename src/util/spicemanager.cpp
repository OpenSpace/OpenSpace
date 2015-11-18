/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/util/spicemanager.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>

#include <glm/gtc/type_ptr.hpp>

#include <algorithm>
#include <format.h>

namespace {
	const std::string _loggerCat = "SpiceManager";
    
    // The value comes from
    // http://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getmsg_c.html
    // as the maximum message length
    const unsigned SpiceErrorBufferSize = 1841;
    
    // This method checks if one of the previous SPICE methods has failed. If it has, an
    // exception with the SPICE error message is thrown
    void throwOnSpiceError(std::string errorMessage) {
        SpiceBoolean failed = failed_c();
        if (failed) {
            char buffer[SpiceErrorBufferSize];
            getmsg_c("LONG", SpiceErrorBufferSize, buffer);
            reset_c();
            throw openspace::SpiceManager::SpiceKernelException(
                errorMessage + ": " + buffer
            );
        }
    }
}

using fmt::format;
using std::string;

namespace openspace {
    
SpiceManager::SpiceKernelException::SpiceKernelException(const string& msg)
    : ghoul::RuntimeError(msg, "Spice")
{}

SpiceManager::SpiceManager() {
    // Set the SPICE library to not exit the program if an error occurs
    erract_c("SET", 0, const_cast<char*>("REPORT"));
    // But we do not want SPICE to print the errors, we will fetch them ourselves
    errprt_c("SET", 0, const_cast<char*>("NONE"));
}

SpiceManager::~SpiceManager() {
    for (const KernelInformation& i : _loadedKernels)
        unload_c(i.path.c_str());

    // Set values back to default
    erract_c("SET", 0, const_cast<char*>("DEFAULT"));
    errprt_c("SET", 0, const_cast<char*>("DEFAULT"));
}


SpiceManager::KernelHandle SpiceManager::loadKernel(string filePath) {
    ghoul_assert(!filePath.empty(), "Empty file path");
    ghoul_assert(
        FileSys.fileExists(filePath),
        format(
            "File '{}' ('{}') does not exist",
            filePath,
            absPath(filePath)
        )
    );
    ghoul_assert(
        FileSys.directoryExists(ghoul::filesystem::File(filePath).directoryName()),
        format(
            "File '{}' exists, but directory '{}' doesn't",
            absPath(filePath),
            ghoul::filesystem::File(filePath).directoryName()
        )
    );
    
	string path = absPath(filePath);
    auto it = std::find_if(
        _loadedKernels.begin(),
        _loadedKernels.end(),
        [path](const KernelInformation& info) { return info.path == path; });

    if (it != _loadedKernels.end()) {
        it->refCount++;
        LDEBUG(
            format(
                "Kernel '{}' was already loaded. New reference count: {}",
                path,
                it->refCount
            )
        );
        return it->id;
    }

    // We need to set the current directory as meta-kernels are usually defined relative
    // to the directory they reside in. The directory change is not necessary for regular
    // kernels
    ghoul::filesystem::Directory currentDirectory = FileSys.currentDirectory();
    string fileDirectory = ghoul::filesystem::File(path, true).directoryName();
	FileSys.setCurrentDirectory(fileDirectory);

    LINFO("Loading SPICE kernel '" << path << "'");
    // Load the kernel
	furnsh_c(path.c_str());

    // Reset the current directory to the previous one
    FileSys.setCurrentDirectory(currentDirectory);

    throwOnSpiceError("Kernel loading");
    
    string fileExtension = ghoul::filesystem::File(path, true).fileExtension();
    if (fileExtension == ".bc" || fileExtension == ".BC")
		findCkCoverage(path); // binary ck kernel
	else if (fileExtension == "bsp" || fileExtension == "BSP")
		findSpkCoverage(path); // binary spk kernel

    KernelHandle kernelId = ++_lastAssignedKernel;
    ghoul_assert(kernelId != 0, fmt::format("Kernel Handle wrapped around to 0"));
    _loadedKernels.push_back({std::move(path), kernelId, 1});
    return kernelId;
}

void SpiceManager::unloadKernel(KernelHandle kernelId) {
    ghoul_assert(kernelId <= _lastAssignedKernel, "Invalid unassigned kernel");
    ghoul_assert(kernelId != KernelHandle(0), "Invalid zero handle");
    
	auto it = std::find_if(_loadedKernels.begin(), _loadedKernels.end(),
		[&kernelId](const KernelInformation& info) { return info.id == kernelId; });

	if (it != _loadedKernels.end()) {
        // If there was only one part interested in the kernel, we can unload it
        if (it->refCount == 1) {
		    // No need to check for errors as we do not allow empty path names
            LINFO(format("Unloading SPICE kernel '{}'", it->path));
		    unload_c(it->path.c_str());
		    _loadedKernels.erase(it);
        }
        // Otherwise, we hold on to it, but reduce the reference counter by 1
        else {
            it->refCount--;
            LDEBUG(format("Reducing reference counter to: {}", it->refCount));
        }
	}
}

void SpiceManager::unloadKernel(string filePath) {
    ghoul_assert(!filePath.empty(), "Empty filename");

    string path = absPath(filePath);

	auto it = std::find_if(_loadedKernels.begin(), _loadedKernels.end(),
		[&path](const KernelInformation& info) { return info.path == path; });
    
    if (it == _loadedKernels.end())
        throw SpiceKernelException("Kernel unloading failed");
	else {
        // If there was only one part interested in the kernel, we can unload it
        if (it->refCount == 1) {
            LINFO(format("Unloading SPICE kernel '{}'", path));
            unload_c(path.c_str());
            _loadedKernels.erase(it);
        }
        else {
            // Otherwise, we hold on to it, but reduce the reference counter by 1
            it->refCount--;
            LDEBUG(format("Reducing reference counter to: {}", it->refCount));
        }
	}
}

bool SpiceManager::hasSpkCoverage(const string& target, double et) const {
    ghoul_assert(!target.empty(), "Empty target");
    
    int id = naifId(target);
    auto it = _spkIntervals.find(id);
    if (it != _spkIntervals.end()) {
        std::vector<std::pair<double, double>> intervalVector = it->second;
        for (const auto& vecElement : intervalVector) {
            if (vecElement.first < et && vecElement.second > et)
                return true;
        }
    }
    return false;
}

bool SpiceManager::hasCkCoverage(const string& frame, double et) const {
    ghoul_assert(!frame.empty(), "Empty target");
    
    int id = frameId(frame);
    
    auto it = _ckIntervals.find(id);
    if (it != _ckIntervals.end()) {
        std::vector<std::pair<double, double>> intervalVector = it->second;
        for (const auto& i : intervalVector) {
            if (i.first < et && i.second > et)
                return true;
        }
    }
    return false;
}

bool SpiceManager::hasValue(int naifId, const string& item) const {
	return bodfnd_c(naifId, item.c_str());
}

bool SpiceManager::hasValue(const string& body, const string& item) const {
    ghoul_assert(!body.empty(), "Empty body");
    ghoul_assert(!item.empty(), "Empty item");
    
	int id = naifId(body);
    return hasValue(id, item);
}

int SpiceManager::naifId(const string& body) const {
    ghoul_assert(!body.empty(), "Empty body");
    
    SpiceBoolean success;
    int id;
    bods2c_c(body.c_str(), &id, &success);
    if (!success)
        throw SpiceKernelException(format("Could not find NAIF ID of body '{}'", body));
    return id;
}
    
bool SpiceManager::hasNaifId(const string& body) const {
    ghoul_assert(!body.empty(), "Empty body");
    
    SpiceBoolean success;
    int id;
    bods2c_c(body.c_str(), &id, &success);
    return success;
}

int SpiceManager::frameId(const string& frame) const {
    ghoul_assert(!frame.empty(), "Empty frame");
    
    int id;
    namfrm_c(frame.c_str(), &id);
    if (id == 0)
        throw SpiceKernelException(format("Could not find NAIF ID of frame '{}'", frame));
    return id;
}

bool SpiceManager::hasFrameId(const string& frame) const {
    ghoul_assert(!frame.empty(), "Empty frame");
    
    int id;
    namfrm_c(frame.c_str(), &id);
    return id != 0;
}
    
void getValueInternal(const string& body, const string& value, int size,
	double* v)
{
    ghoul_assert(!body.empty(), "Empty body");
    ghoul_assert(!value.empty(), "Empty value");
    ghoul_assert(v != nullptr, "Empty value pointer");

	SpiceInt n;
	bodvrd_c(body.c_str(), value.c_str(), size, &n, v);

    throwOnSpiceError("Error getting value '" + value +
                                                "' for body '" + body + "'");
}

void SpiceManager::getValue(const string& body, const string& value, double& v) const {
	getValueInternal(body, value, 1, &v);
}

void SpiceManager::getValue(const string& body, const string& value,
                            glm::dvec3& v) const
{
	getValueInternal(body, value, 3, glm::value_ptr(v));
}

void SpiceManager::getValue(const string& body, const string& value,
	glm::dvec4& v) const
{
	getValueInternal(body, value, 4, glm::value_ptr(v));
}

void SpiceManager::getValue(const string& body, const string& value,
	std::vector<double>& v) const 
{
    ghoul_assert(!v.empty(), "Array for values has to be preallocaed");
    getValueInternal(body, value, v.size(), v.data());
}

double SpiceManager::spacecraftClockToET(const string& craft, double craftTicks) {
    ghoul_assert(!craft.empty(), "Empty craft");
    int craftId = naifId(craft);
    double et;
	sct2e_c(craftId, craftTicks, &et);
    throwOnSpiceError(format(
        "Error transforming spacecraft clock of '{}' at time {}",
        craft, craftTicks)
    );
	return et;
}

bool SpiceManager::getETfromDate(const std::string& timeString,
                                 double& ephemerisTime) const
{
    if (timeString.empty()) {
        LERROR("No time string was provided");
        return false;
    }
    
	str2et_c(timeString.c_str(), &ephemerisTime);
    throwOnSpiceError("Error converting date '" + timeString + "'");
    return true;
}

bool SpiceManager::getDateFromET(double ephemerisTime, std::string& date,
	const std::string& format) const
{
	static const int BufferSize = 256;
    
    if (format.empty()) {
        LERROR("No format string was provided for the date conversion");
        return false;
    }
	SpiceChar buffer[BufferSize];

	timout_c(ephemerisTime, format.c_str(), BufferSize - 1, buffer);
    throwOnSpiceError("Error converting ephemeris time '" +
                      std::to_string(ephemerisTime) +
                      "' to date with format '" + format + "'"
    );

    date = std::string(buffer);
    return true;
}

bool SpiceManager::getTargetPosition(const std::string& target,
	                                 const std::string& observer,
	                                 const std::string& referenceFrame,
	                                 const std::string& aberrationCorrection,
	                                 double ephemerisTime,
	                                 glm::dvec3& position,
	                                 double& lightTime) const
{
	psc pscPosition = PowerScaledCoordinate::CreatePowerScaledCoordinate(position[0], position[1], position[2]);

	bool targetHasCoverage = hasSpkCoverage(target, ephemerisTime);
	bool observerHasCoverage = hasSpkCoverage(observer, ephemerisTime);
	if (!targetHasCoverage && !observerHasCoverage){
		return false;
	}
	else if (targetHasCoverage && observerHasCoverage){
		spkpos_c(target.c_str(), ephemerisTime, referenceFrame.c_str(),
			aberrationCorrection.c_str(), observer.c_str(), glm::value_ptr(position),
			&lightTime);
	}
	else if (targetHasCoverage) {// observer has no coverage
		getEstimatedPosition(ephemerisTime, observer, target, pscPosition);
		pscPosition = pscPosition*(-1.f); // inverse estimated position because the observer is "target" argument in funciton
		position = pscPosition.vec3();
	}
	else { // target has no coverage
		getEstimatedPosition(ephemerisTime, target, observer, pscPosition);
		position = pscPosition.vec3();
	}
	
	if (position[0] == 0.0 || position[1] == 0.0 || position[2] == 0.0)
		return false;

	return targetHasCoverage && observerHasCoverage;
}

bool SpiceManager::getTargetPosition(const std::string& target,
						   const std::string& observer,
		                   const std::string& referenceFrame, 
						   const std::string& aberrationCorrection,
		                   double ephemerisTime,
						   psc& position, 
						   double& lightTime) const
{
	double pos[3] = { 0.0, 0.0, 0.0};

	bool targetHasCoverage = hasSpkCoverage(target, ephemerisTime);
	bool observerHasCoverage = hasSpkCoverage(observer, ephemerisTime);
	if (!targetHasCoverage && !observerHasCoverage){
		position = PowerScaledCoordinate::CreatePowerScaledCoordinate(pos[0], pos[1], pos[2]);
		return false;
	}
	else if (targetHasCoverage && observerHasCoverage){
		spkpos_c(target.c_str(), ephemerisTime, referenceFrame.c_str(),
			aberrationCorrection.c_str(), observer.c_str(), pos, &lightTime);
		position = PowerScaledCoordinate::CreatePowerScaledCoordinate(pos[0], pos[1], pos[2]);
	}
	else if (targetHasCoverage) {// observer has no coverage
		getEstimatedPosition(ephemerisTime, observer, target, position);
		position = position*(-1.f); // inverse estimated position because the observer is "target" argument in funciton
	}
	else {// target has no coverage
		getEstimatedPosition(ephemerisTime, target, observer, position);
	}

	return targetHasCoverage && observerHasCoverage;
}

bool SpiceManager::getEstimatedPosition(const double time, const std::string target,
										const std::string observer, psc& targetPosition) const 
{
	
	int idTarget, idObserver;
    bool targetFound = hasNaifId(target);
    idTarget = naifId(target);
	if (idTarget == 0) { //SOLAR SYSTEM BARYCENTER special case, no def. in kernels
		targetPosition[0] = 0.f;
		targetPosition[1] = 0.f;
		targetPosition[2] = 0.f;
		targetPosition[3] = 0.f;
		return true;
	}
	
	double pos[3] = { 0.0, 0.0, 0.0 };

    bool observerFound = hasNaifId(observer);
    idObserver = naifId(observer);
	if (!targetFound || !observerFound || (idTarget == idObserver)) {
		return false;
	}
	double lt, earlier, later, difference, quote;
	double pos_earlier[3] = { 0.0, 0.0, 0.0 };
	double pos_later[3] = { 0.0, 0.0, 0.0 };
	if (_spkCoverageTimes.find(idTarget) == _spkCoverageTimes.end()){ // no coverage
		LWARNING("No position for " + target + " at any time, was the correct SPK Kernels loaded?");
		targetPosition = PowerScaledCoordinate::CreatePowerScaledCoordinate(pos[0], pos[1], pos[2]);
		return false;
	}

	std::set<double> coveredTimes = _spkCoverageTimes.find(idTarget)->second;
	
	std::set<double>::iterator first = coveredTimes.begin();
	std::set<double>::iterator last = coveredTimes.end();
	
	if (coveredTimes.lower_bound(time) == first) { // coverage later, fetch first position
		spkpos_c(target.c_str(), (*first), "GALACTIC",
			"NONE", observer.c_str(), pos, &lt);
	}
	else if (coveredTimes.upper_bound(time) == last) { // coverage earlier, fetch last position
		spkpos_c(target.c_str(), *(coveredTimes.rbegin()), "GALACTIC",
			"NONE", observer.c_str(), pos, &lt);

	}
	else { // coverage both earlier and later, interpolate these positions
		earlier = *std::prev((coveredTimes.lower_bound(time)));
		later = *(coveredTimes.upper_bound(time));

		spkpos_c(target.c_str(), earlier, "GALACTIC",
			"NONE", observer.c_str(), pos_earlier, &lt);
		spkpos_c(target.c_str(), later, "GALACTIC",
			"NONE", observer.c_str(), pos_later, &lt);
		
		//linear interpolation
		difference = later - earlier;
		quote = (time - earlier) / difference;
		pos[0] = (pos_earlier[0]*(1-quote) + pos_later[0]*quote);
		pos[1] = (pos_earlier[1]*(1-quote) + pos_later[1]*quote);
		pos[2] = (pos_earlier[2]*(1-quote) + pos_later[2]*quote);
	}


	targetPosition = PowerScaledCoordinate::CreatePowerScaledCoordinate(pos[0], pos[1], pos[2]);
	throwOnSpiceError("Error estimating positin for target: " + target + ", or observer: " + observer);

	return targetFound && observerFound;
}

// do NOT remove this method. 
bool SpiceManager::frameConversion(glm::dvec3& v, const std::string& from, const std::string& to, double ephemerisTime) const{
	glm::dmat3 transform;
	if (from == to)
		return true;
	// get rotation matrix from frame A - frame B
	pxform_c(from.c_str(), to.c_str(), ephemerisTime, (double(*)[3])glm::value_ptr(transform));
	
    throwOnSpiceError("Error converting from frame '" + from +
        "' to frame '" + to + "' at time " + std::to_string(ephemerisTime));
	// re-express vector in new frame
	mxv_c((double(*)[3])glm::value_ptr(transform), glm::value_ptr(v), glm::value_ptr(v));
    return true;
}

glm::dvec3 SpiceManager::orthogonalProjection(glm::dvec3& v1, glm::dvec3& v2){
	glm::dvec3 projected;
	vproj_c(glm::value_ptr(v1), glm::value_ptr(v2), glm::value_ptr(projected));
	return projected;
}

bool SpiceManager::targetWithinFieldOfView(const std::string& instrument,
	const std::string& target,
	const std::string& observer,
	const std::string& method,
	const std::string& referenceFrame,
	const std::string& aberrationCorrection,
	double& targetEpoch,
    bool& isVisible
    ) const
{
	int visible;
	fovtrg_c(instrument.c_str(),
		     target.c_str(),
		     method.c_str(),
		     referenceFrame.c_str(),
		     aberrationCorrection.c_str(),
		     observer.c_str(),
		     &targetEpoch,
		     &visible);
    isVisible = (visible == SPICETRUE);
    
    throwOnSpiceError("Checking if target '" + target +
        "' is in view of instrument '" + instrument + "' failed");

	return true;
}

bool SpiceManager::targetWithinFieldOfView(const std::string& instrument,
	const std::string& target,
	const std::string& observer,
	const std::string& method,
	const std::string& aberrationCorrection,
	double& targetEpoch,
    bool& isVisible
    ) const{

	int visible;

	std::string frame = frameFromBody(target);

	fovtrg_c(instrument.c_str(),
		target.c_str(),
		method.c_str(),
		frame.c_str(),
		aberrationCorrection.c_str(),
		observer.c_str(),
		&targetEpoch,
		&visible);
    isVisible = (visible == SPICETRUE);

    throwOnSpiceError("Checking if target '" + target +
                "' is in view of instrument '" + instrument + "' failed");

	return true;
}


bool SpiceManager::getSurfaceIntercept(const std::string& target,
									   const std::string& observer,
									   const std::string& fovInstrument,
									   const std::string& referenceFrame,
									   const std::string& method,
									   const std::string& aberrationCorrection,
									   double ephemerisTime,
									   double& targetEpoch,
									   glm::dvec3& directionVector,
									   glm::dvec3& surfaceIntercept,
									   glm::dvec3& surfaceVector,
                                       bool& isVisible
                                       ) const
{
	// make pretty latr
	double dvec[3], spoint[3], et;
	glm::dvec3 srfvec;
	int found;
	bool convert;

	dvec[0] = directionVector[0];
	dvec[1] = directionVector[1];
	dvec[2] = directionVector[2];

	// allow client specify non-inertial frame. 
	std::string bodyfixed = "IAU_";
	convert = (referenceFrame.find(bodyfixed) == std::string::npos);
	if (convert) {
		bodyfixed = frameFromBody(target);
	} else {
		bodyfixed = referenceFrame;
	}

	sincpt_c(method.c_str(), 
		     target.c_str(), 
			 ephemerisTime, 
			 bodyfixed.c_str(), 
			 aberrationCorrection.c_str(), 
			 observer.c_str(),
		     fovInstrument.c_str(), 
			 dvec, 
			 spoint, 
			 &et, 
			 glm::value_ptr(srfvec), 
			 &found);

    isVisible = (found == SPICETRUE);

	throwOnSpiceError("Error retrieving surface intercept on target '" + target + "'" +
								  "viewed from observer '" + observer + "' in " +
								  "reference frame '" + bodyfixed + "' at time '" +
								  std::to_string(ephemerisTime) + "'");

	if (convert)
        frameConversion(srfvec, bodyfixed, referenceFrame, ephemerisTime);

	if (found){
		memcpy(glm::value_ptr(directionVector), dvec, sizeof(dvec));
		memcpy(glm::value_ptr(surfaceIntercept), spoint, sizeof(spoint));
		surfaceVector = srfvec;
	}
	return true;
}

// Not called at the moment @AA
bool SpiceManager::getTargetState(const std::string& target,
	                              const std::string& observer,
	                              const std::string& referenceFrame,
	                              const std::string& aberrationCorrection,
	                              double ephemerisTime,
	                              glm::dvec3& targetPosition,
	                              glm::dvec3& targetVelocity,
	                              double& lightTime) const
{
    double buffer[6];
    
	spkezr_c(target.c_str(), ephemerisTime, referenceFrame.c_str(),
	    	aberrationCorrection.c_str(), observer.c_str(), buffer, &lightTime);

    throwOnSpiceError("Error retrieving state of target '" + target + "'" +
                                  "viewed from observer '" + observer + "' in " +
                                  "reference frame '" + referenceFrame + "' at time '" +
                                  std::to_string(ephemerisTime) + "'");
    memmove(glm::value_ptr(targetPosition), buffer, sizeof(double) * 3);
    memmove(glm::value_ptr(targetVelocity), buffer + 3, sizeof(double) * 3);
    return true;
}

// Not called at the moment @AA
bool SpiceManager::getTargetState(const std::string& target,
									 const std::string& observer,
									 const std::string& referenceFrame,
									 const std::string& aberrationCorrection,
									 double ephemerisTime,
									 PowerScaledCoordinate& position,
									 PowerScaledCoordinate& velocity,
									 double& lightTime) const
{
	double state[6];
	std::fill_n(state, 6, NULL);

	spkezr_c(target.c_str(), ephemerisTime, referenceFrame.c_str(),
		aberrationCorrection.c_str(), observer.c_str(), state, &lightTime);

	throwOnSpiceError("Error retrieving state of target '" + target + "'" +
		"viewed from observer '" + observer + "' in " +
		"reference frame '" + referenceFrame + "' at time '" +
		std::to_string(ephemerisTime) + "'");

    position = PowerScaledCoordinate::CreatePowerScaledCoordinate(state[0], state[1], state[2]);
    velocity = PowerScaledCoordinate::CreatePowerScaledCoordinate(state[3], state[4], state[5]);

	return true;
}

// Not called at the moment @AA
bool SpiceManager::getStateTransformMatrix(const std::string& fromFrame,
							const std::string& toFrame,
							double ephemerisTime,
							TransformMatrix& stateMatrix) const
{
	sxform_c(fromFrame.c_str(), toFrame.c_str(), 
		     ephemerisTime, (double(*)[6])stateMatrix.data());
    
    throwOnSpiceError("Error retrieved state transform matrix from frame '" +
                                  fromFrame + "' to frame '" + toFrame + "' at time '" +
                                  std::to_string(ephemerisTime) + "'");
	return true;
}

bool SpiceManager::getPositionTransformMatrix(const std::string& fromFrame,
												 const std::string& toFrame,
												 double ephemerisTime,
												 glm::dmat3& positionMatrix) const 
{
	bool estimated = false;
	pxform_c(fromFrame.c_str(), toFrame.c_str(),
			ephemerisTime, (double(*)[3])glm::value_ptr(positionMatrix));

	SpiceBoolean success = !(failed_c());
    reset_c();
	if (!success) {
		estimated = getEstimatedTransformMatrix(ephemerisTime, fromFrame, toFrame, positionMatrix);
	}

    positionMatrix = glm::transpose(positionMatrix);
    
	return estimated || success;
}

// Not called at the moment @AA
bool SpiceManager::getPositionTransformMatrix(const std::string& fromFrame,
	const std::string& toFrame,
	double ephemerisTimeFrom,
	double ephemerisTimeTo,
	glm::dmat3& positionMatrix) const{

	pxfrm2_c(fromFrame.c_str(), toFrame.c_str(), ephemerisTimeFrom, ephemerisTimeTo, (double(*)[3])glm::value_ptr(positionMatrix));

	throwOnSpiceError("Error retrieving position transform matrix from "
		"frame '" + fromFrame + "' to frame '" + toFrame +
		"' from time '" + std::to_string(ephemerisTimeFrom) + " to time '"
		+ std::to_string(ephemerisTimeTo) + "'");
	positionMatrix = glm::transpose(positionMatrix);

	return true;
}


bool SpiceManager::getEstimatedTransformMatrix(const double time, const std::string fromFrame,
	const std::string toFrame, glm::dmat3& positionMatrix) const
{
	int idFrame;

    bool frameFound = hasFrameId(fromFrame);
	if (!frameFound) {
		return false;
	}
    idFrame = frameId(fromFrame);
    
    if (_ckCoverageTimes.find(idFrame) == _ckCoverageTimes.end()){ // no coverage
		return false;
	}

	double earlier, later, difference, quote;

	std::set<double> coveredTimes = _ckCoverageTimes.find(idFrame)->second;

	std::set<double>::iterator first = coveredTimes.begin();
	std::set<double>::iterator last = coveredTimes.end();

	if (coveredTimes.lower_bound(time) == first) { // coverage later, fetch first transform
		pxform_c(fromFrame.c_str(), toFrame.c_str(),
			*first, (double(*)[3])glm::value_ptr(positionMatrix));
	}
	else if (coveredTimes.upper_bound(time) == last) { // coverage earlier, fetch last transform
		pxform_c(fromFrame.c_str(), toFrame.c_str(),
			*(coveredTimes.rbegin()), (double(*)[3])glm::value_ptr(positionMatrix));
	}
	else { // coverage both earlier and later, interpolate these transformations
		earlier = *std::prev((coveredTimes.lower_bound(time)));
		later = *(coveredTimes.upper_bound(time));

		glm::dmat3 earlierTransform, laterTransform;

		pxform_c(fromFrame.c_str(), toFrame.c_str(),
			earlier, (double(*)[3])glm::value_ptr(earlierTransform));
		pxform_c(fromFrame.c_str(), toFrame.c_str(),
			later, (double(*)[3])glm::value_ptr(laterTransform));

		difference = later - earlier;
		quote = (time - earlier) / difference;

		for (int i = 0; i < 3; ++i) {
			for (int j = 0; j < 3; ++j) {
				positionMatrix[i][j] = (earlierTransform[i][j] * (1 - quote) + laterTransform[i][j] * quote);
			}
		}
	}
	throwOnSpiceError("Error estimating transform matrix from frame: "
		+ fromFrame + ", to frame: " + toFrame);

	return true;
}


bool SpiceManager::getFieldOfView(const std::string& instrument, std::string& fovShape,
	std::string& frameName, glm::dvec3& boresightVector,
	std::vector<glm::dvec3>& bounds) const
{
	int id;
    bool success = hasNaifId(instrument);
	if (!success)
		return false;
	else
        id = naifId(instrument);
		return getFieldOfView(id, fovShape, frameName, boresightVector, bounds);
}


bool SpiceManager::getFieldOfView(int instrument,
	                              std::string& fovShape,
	                              std::string& frameName,
	                              glm::dvec3& boresightVector,
	                              std::vector<glm::dvec3>& bounds) const
{
	static const int maxBoundsSize = 20;
	static const int bufferSize = 128;
	static char fovShapeBuffer[bufferSize];
	static char frameNameBuffer[bufferSize];

	SpiceInt nrReturned;
	double boundsArr[maxBoundsSize][3];

	getfov_c(instrument,  // instrument id
		maxBoundsSize,    // maximum size for the bounds vector
		bufferSize,       // maximum size for the fov shape buffer
		bufferSize,       // maximum size for the frame name buffer
		fovShapeBuffer,   // the fov shape buffer
		frameNameBuffer,  // the frame name buffer
		glm::value_ptr(boresightVector), // the boresight vector
		&nrReturned,      // the number of array values returned for the bounds
		(double(*)[3])boundsArr // the bounds
		);

	throwOnSpiceError("Error getting Field-of-View parameters for "
		"instrument '" + std::to_string(instrument) + "'");

	bounds.resize(nrReturned);
	for (int i = 0; i < nrReturned; ++i) {
		bounds[i] = glm::dvec3(boundsArr[i][0],
							   boundsArr[i][1],
							   boundsArr[i][2]);
	}

	fovShape = std::string(fovShapeBuffer);
	frameName = std::string(frameNameBuffer);

	return true;
}
bool SpiceManager::getTerminatorEllipse(const int numberOfPoints,
										const std::string terminatorType,
										const std::string lightSource,
										const std::string observer,
										const std::string target,
										const std::string frame,
										const std::string aberrationCorrection,
										double ephemerisTime,
										double& targetEpoch,
										glm::dvec3& observerPosition,
										std::vector<psc>& terminatorPoints)
{
    std::vector<std::array<double, 3>> tpoints(numberOfPoints);

	edterm_c(terminatorType.c_str(), 
		     lightSource.c_str(), 
			 target.c_str(), 
			 ephemerisTime, 
			 frame.c_str(), 
			 aberrationCorrection.c_str(), 
			 observer.c_str(),
		     numberOfPoints, 
			 &targetEpoch, 
			 glm::value_ptr(observerPosition), 
			 (double(*)[3])tpoints.data() );

	throwOnSpiceError("Error getting " + terminatorType +
		"terminator for'" + target + "'");

	for (int i = 0; i < numberOfPoints; i++){
		psc point = psc::CreatePowerScaledCoordinate(tpoints[i][0], tpoints[i][1], tpoints[i][2]);
		point[3] += 3;
		terminatorPoints.push_back(point);
	}
    
	return true;
}

std::string SpiceManager::frameFromBody(const std::string body) const {
	for (auto pair : _frameByBody) {
		if (pair.first == body) {
			return pair.second;
		}
	}

	std::string unionPrefix = "IAU_";
	std::string frame = "";

	if (body.find(unionPrefix) == std::string::npos)
		frame = unionPrefix + body;
	else
		frame = body;

	return frame;
}

bool SpiceManager::addFrame(const std::string body, const std::string frame) {
	if (body == "" || frame == "")
		return false;
	else {
		_frameByBody.push_back(std::make_pair(body, frame));
		return true;
	}
}

bool SpiceManager::getPlanetEllipsoid(std::string planetName, float &a, float &b, float &c) {
	SpiceDouble radii[3];
	SpiceInt n = -1;
	int id = -1;

	id = naifId(planetName);
	if (bodfnd_c(id, "RADII")) {
		bodvrd_c(planetName.c_str(), "RADII", 3, &n, radii);
		a = static_cast<float>(radii[0]);
		b = static_cast<float>(radii[1]);
		c = static_cast<float>(radii[2]);
	}
	else {
		LWARNING("Could not find SPICE data for the shape of " + planetName + ", using modfile value");
		a = 1.f;
		b = 1.f;
		c = 1.f;
	}

	throwOnSpiceError("Error retrieving planet radii of " + planetName);
	return true;
}
    
void SpiceManager::findCkCoverage(const std::string& path) {
    ghoul_assert(!path.empty(), "Empty file path");
    ghoul_assert(FileSys.fileExists(path), format("File '{}' does not exist", path));
    
    const unsigned int MaxObj = 64;
    const unsigned int WinSiz = 10000;
    
    SPICEINT_CELL(ids, MaxObj);
    SPICEDOUBLE_CELL(cover, WinSiz);
    
    ckobj_c(path.c_str(), &ids);
    throwOnSpiceError("Error finding Ck Converage");
    
    for (SpiceInt i = 0; i < card_c(&ids); ++i) {
        SpiceInt frame = SPICE_CELL_ELEM_I(&ids, i);
        
        scard_c(0, &cover);
        ckcov_c(path.c_str(), frame, SPICEFALSE, "SEGMENT", 0.0, "TDB", &cover);
        throwOnSpiceError("Error finding Ck Converage");
        
        //Get the number of intervals in the coverage window.
        SpiceInt numberOfIntervals = wncard_c(&cover);
        
        for (SpiceInt j = 0; j < numberOfIntervals; ++j) {
            //Get the endpoints of the jth interval.
            SpiceDouble b, e;
            wnfetd_c(&cover, j, &b, &e);
            throwOnSpiceError("Error finding Ck Converage");
            
            _ckCoverageTimes[frame].insert(e);
            _ckCoverageTimes[frame].insert(b);
            _ckIntervals[frame].emplace_back(b, e);
        }
    }
}

void SpiceManager::findSpkCoverage(const std::string& path) {
    ghoul_assert(!path.empty(), "Empty file path");
    ghoul_assert(FileSys.fileExists(path), format("File '{}' does not exist", path));
    
    const unsigned int MaxObj = 64;
    const unsigned int WinSiz = 10000;
    
    SPICEINT_CELL(ids, MaxObj);
    SPICEDOUBLE_CELL(cover, WinSiz);
    
    spkobj_c(path.c_str(), &ids);
    throwOnSpiceError("Error finding Spk Converage");
    
    for (SpiceInt i = 0; i < card_c(&ids); ++i) {
        SpiceInt obj = SPICE_CELL_ELEM_I(&ids, i);
        
        scard_c(0, &cover);
        spkcov_c(path.c_str(), obj, &cover);
        throwOnSpiceError("Error finding Spk Converage");
        
        //Get the number of intervals in the coverage window.
        SpiceInt numberOfIntervals = wncard_c(&cover);
        
        for (SpiceInt j = 0; j < numberOfIntervals; ++j) {
            //Get the endpoints of the jth interval.
            SpiceDouble b, e;
            wnfetd_c(&cover, j, &b, &e);
            throwOnSpiceError("Error finding Spk Converage");
            
            //insert all into coverage time set, the windows could be merged @AA
            _spkCoverageTimes[obj].insert(e);
            _spkCoverageTimes[obj].insert(b);
            _spkIntervals[obj].emplace_back(b, e);
        }		
    }
}


}
