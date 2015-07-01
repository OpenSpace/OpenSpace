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

#include <glm/gtc/type_ptr.hpp>

#include <algorithm>

#define  MAXOBJ 64
#define  WINSIZ 10000

namespace {
	const std::string _loggerCat = "SpiceManager";
}

namespace openspace {

SpiceManager::SpiceManager() 
    : _lastAssignedKernel(0)
{
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


SpiceManager::KernelIdentifier SpiceManager::loadKernel(const std::string& filePath) {
	if (filePath.empty()) {
		LERROR("No filename provided");
		return KernelFailed;
	}

	std::string&& path = absPath(filePath);

	if (!FileSys.fileExists(path)) {
		LERROR("Kernel file '" << path << "' does not exist");
		return KernelFailed;
	}

	// We need to set the current directory as meta-kernels are usually defined relative
	// to the directory they reside in. The directory change is not necessary for regular
	// kernels

	ghoul::filesystem::Directory currentDirectory = FileSys.currentDirectory();
	std::string&& fileDirectory = ghoul::filesystem::File(path).directoryName();

	if (!FileSys.directoryExists(fileDirectory)) {
		LERROR("Could not find directory for kernel '" << path << "'");
		return KernelFailed;
	}

    auto it = std::find_if(
        _loadedKernels.begin(),
        _loadedKernels.end(),
        [path](const KernelInformation& info) { return info.path == path; });

    if (it != _loadedKernels.end())
    {
        it->refCount++;
        LDEBUG("Kernel '" << path << "' was already loaded. "
            "New reference count: " << it->refCount);
        return it->id;
    }

    KernelIdentifier kernelId = ++_lastAssignedKernel;

	FileSys.setCurrentDirectory(fileDirectory);

    LINFO("Loading SPICE kernel '" << path << "'");
	// Load the kernel
	furnsh_c(path.c_str());

	std::string fileExtension = path.substr(path.size() - 3);
	if (fileExtension == ".bc" || fileExtension == ".BC") { // binary ck kernel
		findCkCoverage(path);
	}
	else if (fileExtension == "bsp" || fileExtension == "BSP") { // binary spk kernel
		findSpkCoverage(path);
	}


	// Reset the current directory to the previous one
	FileSys.setCurrentDirectory(currentDirectory);
	int failed = failed_c();
    if (failed) {
        char msg[1024];
        getmsg_c ( "LONG", 1024, msg );
        LERROR("Error loading kernel '" + path + "'");
        LERROR("Spice reported: " + std::string(msg));
        reset_c();
        return KernelFailed;
    }

	bool hasError = checkForError("Error loading kernel '" + path + "'");
	if (hasError)
		return KernelFailed;
	else {
		KernelInformation&& info = { path, std::move(kernelId), 1 };
		_loadedKernels.push_back(info);
		return kernelId;
	}
}

bool SpiceManager::findCkCoverage(const std::string& path) {
	SpiceInt frame, numberOfIntervals;
	SpiceDouble b, e;
	std::pair <double, double> tempInterval;
	SPICEINT_CELL(ids, MAXOBJ);
	SPICEDOUBLE_CELL(cover, WINSIZ);
	
	ckobj_c(path.c_str(), &ids);

	for (SpiceInt i = 0; i < card_c(&ids); ++i) {
		frame = SPICE_CELL_ELEM_I(&ids, i);

		scard_c(0, &cover);
		ckcov_c(path.c_str(), frame, SPICEFALSE, "SEGMENT", 0.0, "TDB", &cover);
		
		//Get the number of intervals in the coverage window.
		numberOfIntervals = wncard_c(&cover);
		
		for (SpiceInt j = 0; j < numberOfIntervals; ++j) {
			//Get the endpoints of the jth interval.
			wnfetd_c(&cover, j, &b, &e);
			tempInterval = std::make_pair(b, e);

			_ckCoverageTimes[frame].insert(e);
			_ckCoverageTimes[frame].insert(b);
			_ckIntervals[frame].push_back(tempInterval);
		}
	}
	return true;
}

bool SpiceManager::findSpkCoverage(const std::string& path) {
	SpiceInt obj, numberOfIntervals;
	SpiceDouble b, e;
	std::pair <double, double> tempInterval;
	SPICEINT_CELL(ids, MAXOBJ);
	SPICEDOUBLE_CELL(cover, WINSIZ);
	
	spkobj_c(path.c_str(), &ids);
	for (SpiceInt i = 0; i < card_c(&ids); ++i) {
		obj = SPICE_CELL_ELEM_I(&ids, i);

		scard_c(0, &cover);
		spkcov_c(path.c_str(), obj, &cover);
		//Get the number of intervals in the coverage window.
		numberOfIntervals = wncard_c(&cover);

		for (SpiceInt j = 0; j < numberOfIntervals; ++j) {
			//Get the endpoints of the jth interval.
			wnfetd_c(&cover, j, &b, &e);
			tempInterval = std::make_pair(b, e);
			//insert all into coverage time set, the windows could be merged @AA
			_spkCoverageTimes[obj].insert(e);
			_spkCoverageTimes[obj].insert(b);
			_spkIntervals[obj].push_back(tempInterval);
		}		
	}
	return true;
}

bool SpiceManager::hasSpkCoverage(std::string target, double& et) const
{
	int id;
	bool idSuccess = getNaifId(target, id);
	bool hasCoverage = false;

	std::vector< std::pair<double, double> > intervalVector;
	if (_spkIntervals.find(id) == _spkIntervals.end())
		return false;
	else
		intervalVector = _spkIntervals.find(id)->second;

	for (auto vecElement : intervalVector) {
		if (vecElement.first < et && vecElement.second > et) {
			hasCoverage = true;
			return idSuccess && hasCoverage;
		}
	}

	return idSuccess && hasCoverage;
}

bool SpiceManager::hasCkCoverage(std::string frame, double& et) const
{
	int id;
	bool idSuccess = getFrameId(frame, id);
	bool hasCoverage = false;

	std::vector< std::pair<double, double> > intervalVector;
	if (_ckIntervals.find(id) == _ckIntervals.end())
		return false;
	else
		intervalVector = _ckIntervals.find(id)->second;

	for (auto vecElement : intervalVector) {
		if (vecElement.first < et && vecElement.second > et) {
			hasCoverage = true;
			return idSuccess && hasCoverage;
		}
	}

	return idSuccess && hasCoverage;
}

void SpiceManager::unloadKernel(KernelIdentifier kernelId) {
	auto it = std::find_if(_loadedKernels.begin(), _loadedKernels.end(),
		[&kernelId](const KernelInformation& info) { return info.id == kernelId ; });

	if (it != _loadedKernels.end()) {
        // If there is only one part interested in the kernel, we can unload it
        if (it->refCount == 1) {
		    // No need to check for errors as we do not allow empty path names
            LINFO("Unloading SPICE kernel '" << it->path << "'");
		    unload_c(it->path.c_str());
		    _loadedKernels.erase(it);
        }
        else {
            // Otherwise, we hold on to it, but reduce the reference counter by 1
            it->refCount--;
            LDEBUG("Reducing reference counter. New reference count: " << it->refCount);
        }
	}
}

void SpiceManager::unloadKernel(const std::string& filePath) {
	if (filePath.empty()) {
		LERROR("No file path provided");
		return;
	}
	std::string&& path = absPath(filePath);

	auto it = std::find_if(_loadedKernels.begin(), _loadedKernels.end(),
		[&path](const KernelInformation& info) { return info.path == path; });

	if (it != _loadedKernels.end()) {
        // If there is only one part interested in the kernel, we can unload it
        if (it->refCount == 1) {
            LINFO("Unloading SPICE kernel '" << path << "'");
            unload_c(path.c_str());
            _loadedKernels.erase(it);
        }
        else {
            // Otherwise, we hold on to it, but reduce the reference counter by 1
            it->refCount--;
            LDEBUG("Reducing reference counter. New reference count: " << it->refCount);
        }
	}
}

bool SpiceManager::hasValue(int naifId, const std::string& item) const {
	return (bodfnd_c(naifId, item.c_str()) == SPICETRUE);
}

bool SpiceManager::hasValue(const std::string& body, const std::string& item) const {
	int id;
	bool success = getNaifId(body, id);
	if (success)
		return hasValue(id, item);
	else
		return false;
}

bool SpiceManager::getNaifId(const std::string& body, int& id) const {
	if (body.empty()) {
		LERROR("No body was provided");
		return false;
	}
	else {
		SpiceBoolean success;
//		SpiceInt sid = id;
		bods2c_c(body.c_str(), &id, &success);
//		id = sid;
        if (success == SPICEFALSE)
            LERROR("Could not find NAIF ID of body '" + body + "'");
		return (success == SPICETRUE);
	}
}

bool SpiceManager::getFrameId(const std::string& frame, int& id) const {
	if (frame.empty()) {
		LERROR("No frame was provided");
		return false;
	}
	else {
		namfrm_c(frame.c_str(), &id);
		bool hasError = SpiceManager::checkForError("Error getting id for frame '" + frame + "'");
		return !hasError;
	}
}

bool getValueInternal(const std::string& body, const std::string& value, int S,
	double* v)
{

	if (body.empty()) {
		LERROR("No body was provided");
		return false;
	}
	if (value.empty()) {
		LERROR("No value was provided");
		return false;
	}

	SpiceInt n;
	bodvrd_c(body.c_str(), value.c_str(), S, &n, v);

    bool hasError = SpiceManager::checkForError("Error getting value '" + value +
                                                "' for body '" + body + "'");
	return !hasError;
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	double& v) const
{
	return getValueInternal(body, value, 1, &v);
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	glm::dvec3& v) const
{
	return getValueInternal(body, value, 3, glm::value_ptr(v));
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	glm::dvec4& v) const
{
	return getValueInternal(body, value, 4, glm::value_ptr(v));
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	std::vector<double>& v) const 
{
	if (body.empty()) {
		LERROR("No body was provided");
		return false;
	}
	if (value.empty()) {
		LERROR("No value was provided");
		return false;
	}
	if (v.size() == 0) {
		LERROR("Array for values has to be preallocaed");
		return false;
	}

	SpiceInt n;
	bodvrd_c(body.c_str(), value.c_str(), static_cast<SpiceInt>(v.size()), &n, &v[0]);

	bool hasError = checkForError("Error getting value '" + value + "' for body '" + 
		body + "'");
	return !hasError;
}

bool SpiceManager::spacecraftClockToET(const std::string& craftIdCode, double& craftTicks, double& et){
	int craftID = -1;
	getNaifId(craftIdCode, craftID);
	sct2e_c(craftID, craftTicks, &et);
    bool hasError = checkForError("Error transforming spacecraft clock of '" + craftIdCode + "' at time " + std::to_string(craftTicks));
	return !hasError;
}

bool SpiceManager::getETfromDate(const std::string& timeString,
                                 double& ephemerisTime) const
{
    if (timeString.empty()) {
        LERROR("No time string was provided");
        return false;
    }
    
	str2et_c(timeString.c_str(), &ephemerisTime);
    bool hasError = checkForError("Error converting date '" + timeString + "'");
    return !hasError;
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
    bool hasError = checkForError(
                                  "Error converting ephemeris time '" +
                                  std::to_string(ephemerisTime) +
                                  "' to date with format '" + format + "'");
    if (!hasError)
        date = std::string(buffer);
    return !hasError;
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
	bool targetFound = getNaifId(target, idTarget);
	if (idTarget == 0) { //SOLAR SYSTEM BARYCENTER special case, no def. in kernels
		targetPosition[0] = 0.f;
		targetPosition[1] = 0.f;
		targetPosition[2] = 0.f;
		targetPosition[3] = 0.f;
		return true;
	}
	
	double pos[3] = { 0.0, 0.0, 0.0 };

	bool observerFound = getNaifId(observer, idObserver);
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
	checkForError("Error estimating positin for target: " + target + ", or observer: " + observer);

	return targetFound && observerFound;
}

// do NOT remove this method. 
bool SpiceManager::frameConversion(glm::dvec3& v, const std::string& from, const std::string& to, double ephemerisTime) const{
	glm::dmat3 transform;
	if (from == to)
		return true;
	// get rotation matrix from frame A - frame B
	pxform_c(from.c_str(), to.c_str(), ephemerisTime, (double(*)[3])glm::value_ptr(transform));
	
    bool hasError = checkForError("Error converting from frame '" + from +
        "' to frame '" + to + "' at time " + std::to_string(ephemerisTime));
    if (hasError)
        return false;
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
    
    bool hasError = checkForError("Checking if target '" + target +
        "' is in view of instrument '" + instrument + "' failed");

	return !hasError;
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

    bool hasError = checkForError("Checking if target '" + target +
                "' is in view of instrument '" + instrument + "' failed");

	return !hasError;
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

	bool hasError = checkForError("Error retrieving surface intercept on target '" + target + "'" +
								  "viewed from observer '" + observer + "' in " +
								  "reference frame '" + bodyfixed + "' at time '" +
								  std::to_string(ephemerisTime) + "'");

    if (hasError)
        return false;

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

    bool hasError = checkForError("Error retrieving state of target '" + target + "'" +
                                  "viewed from observer '" + observer + "' in " +
                                  "reference frame '" + referenceFrame + "' at time '" +
                                  std::to_string(ephemerisTime) + "'");
    if (!hasError) {
        memmove(glm::value_ptr(targetPosition), buffer, sizeof(double) * 3);
        memmove(glm::value_ptr(targetVelocity), buffer + 3, sizeof(double) * 3);
    }
    return !hasError;
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

	bool hasError = checkForError("Error retrieving state of target '" + target + "'" +
		"viewed from observer '" + observer + "' in " +
		"reference frame '" + referenceFrame + "' at time '" +
		std::to_string(ephemerisTime) + "'");

	if (!hasError) {
		position = PowerScaledCoordinate::CreatePowerScaledCoordinate(state[0], state[1], state[2]);
		velocity = PowerScaledCoordinate::CreatePowerScaledCoordinate(state[3], state[4], state[5]);
	}
	else
	{
		position = PowerScaledCoordinate::CreatePowerScaledCoordinate(0.0, 0.0, 0.0);
		velocity = PowerScaledCoordinate::CreatePowerScaledCoordinate(0, 0, 0);
	}

	return !hasError;
}

// Not called at the moment @AA
bool SpiceManager::getStateTransformMatrix(const std::string& fromFrame,
							const std::string& toFrame,
							double ephemerisTime,
							TransformMatrix& stateMatrix) const
{
	sxform_c(fromFrame.c_str(), toFrame.c_str(), 
		     ephemerisTime, (double(*)[6])stateMatrix.data());
    
    bool hasError = checkForError("Error retrieved state transform matrix from frame '" +
                                  fromFrame + "' to frame '" + toFrame + "' at time '" +
                                  std::to_string(ephemerisTime) + "'");
	return !hasError;
}

bool SpiceManager::getPositionTransformMatrix(const std::string& fromFrame,
												 const std::string& toFrame,
												 double ephemerisTime,
												 glm::dmat3& positionMatrix) const 
{
	bool success = false, estimated = false;
	pxform_c(fromFrame.c_str(), toFrame.c_str(),
			ephemerisTime, (double(*)[3])glm::value_ptr(positionMatrix));

	success = !(failed_c());
	if (!success) {
		reset_c();
		estimated = getEstimatedTransformMatrix(ephemerisTime, fromFrame, toFrame, positionMatrix);		
	}
	if (_showErrors) {
		bool hasError = checkForError("Error retrieving position transform matrix from "
			"frame '" + fromFrame + "' to frame '" + toFrame +
			"' at time '" + std::to_string(ephemerisTime));
        if (hasError)
            return false;
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

	bool hasError = checkForError("Error retrieving position transform matrix from "
		"frame '" + fromFrame + "' to frame '" + toFrame +
		"' from time '" + std::to_string(ephemerisTimeFrom) + " to time '"
		+ std::to_string(ephemerisTimeTo) + "'");
	positionMatrix = glm::transpose(positionMatrix);

	return !hasError;
}


bool SpiceManager::getEstimatedTransformMatrix(const double time, const std::string fromFrame,
	const std::string toFrame, glm::dmat3& positionMatrix) const
{
	int idFrame;

	bool frameFound = getFrameId(fromFrame, idFrame);
	if (!frameFound) {
		return false;
	}
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
	bool hasError = checkForError("Error estimating transform matrix from frame: "
		+ fromFrame + ", to frame: " + toFrame);

	return !hasError;
}


bool SpiceManager::getFieldOfView(const std::string& instrument, std::string& fovShape,
	std::string& frameName, glm::dvec3& boresightVector,
	std::vector<glm::dvec3>& bounds) const
{
	int id;
	bool success = getNaifId(instrument, id);
	if (!success)
		return false;
	else
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

	bool hasError = checkForError("Error getting Field-of-View parameters for "
		"instrument '" + std::to_string(instrument) + "'");
	if (hasError)
		return false;

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
										std::vector<psc>& terminatorPoints){

	double(*tpoints)[3] = new double[numberOfPoints][3];

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
			 (double(*)[3])tpoints );

	bool hasError = checkForError("Error getting " + terminatorType + 
		"terminator for'" + target + "'");
	if (hasError)
		return false;

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

bool SpiceManager::checkForError(std::string errorMessage) {

	int failed = failed_c();
	if (failed  && _showErrors) {
        static char msg[1024];
		if (!errorMessage.empty()) {
			getmsg_c("LONG", 1024, msg);
			LWARNING(errorMessage);
			LWARNING("Spice reported: " + std::string(msg));
		}
        reset_c();
        return true;
    }
	else if (failed) {
		reset_c();
		return false;
	}
	else
	return false;
}

bool SpiceManager::getPlanetEllipsoid(std::string planetName, float &a, float &b, float &c) {

	SpiceDouble radii[3];
	SpiceInt n = -1;
	int id = -1;

	getNaifId(planetName, id);
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

	bool hasError = checkForError("Error retrieving planet radii of " + planetName);
	return !hasError;
}

}
