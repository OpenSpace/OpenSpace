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

    
    
SpiceManager::AberrationCorrection::AberrationCorrection(Type t, Direction d)
    : type(t)
    , direction(d)
{}
    
SpiceManager::AberrationCorrection::AberrationCorrection(const std::string& identifier) {
    const static std::map<std::string, std::pair<Type, Direction>> Mapping =  {
        { "NONE"  , { Type::None, Direction::Reception } },
        { "LT"    , { Type::LightTime, Direction::Reception } },
        { "LT+S"  , { Type::LightTimeStellar, Direction::Reception } },
        { "CN"    , { Type::ConvergedNewtonian, Direction::Reception } },
        { "CN+S"  , { Type::ConvergedNewtonianStellar, Direction::Reception } },
        { "XLT"   , { Type::LightTime, Direction::Transmission } },
        { "XLT+S" , { Type::LightTimeStellar, Direction::Transmission } },
        { "XCN"   , { Type::ConvergedNewtonian, Direction::Transmission } },
        { "XCN+S" , { Type::ConvergedNewtonianStellar, Direction::Transmission } }
    };

    auto it = Mapping.find(identifier);
    
    ghoul_assert(!identifier.empty(), "Identifier may not be empty");
    ghoul_assert(it != Mapping.end(), format("Invalid identifer '{}'", identifier));
    
    type = it->second.first;
    direction = it->second.second;
}
    
SpiceManager::AberrationCorrection::operator const char*() const {
    switch (type) {
        case Type::None:
            return "NONE";
        case Type::LightTime:
            return (direction == Direction::Reception) ? "LT" : "XLT";
        case Type::LightTimeStellar:
            return (direction == Direction::Reception) ? "LT+S" : "XLT+S";
        case Type::ConvergedNewtonian:
            return (direction == Direction::Reception) ? "CN" : "XCN";
        case Type::ConvergedNewtonianStellar:
            return (direction == Direction::Reception) ? "CN+S" : "XCN+S";
    }
}

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
    
    if (it == _loadedKernels.end()) {
        throw SpiceKernelException(
            format("'{}' did not correspond to a loaded kernel", path)
        );
    }
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
    reset_c();
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

    throwOnSpiceError(
        format("Error getting value '{}' for body '{}'",
               value,
               body
        )
    );
}

void SpiceManager::getValue(const string& body, const string& value, double& v) const {
	getValueInternal(body, value, 1, &v);
}

void SpiceManager::getValue(const string& body, const string& value,
                            glm::dvec2& v) const
{
    getValueInternal(body, value, 2, glm::value_ptr(v));
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

double SpiceManager::ephemerisTimeFromDate(const std::string& timeString) const {
    ghoul_assert(!timeString.empty(), "Empty timeString");

    double et;
	str2et_c(timeString.c_str(), &et);
    throwOnSpiceError(format("Error converting date '{}'", timeString));
    return et;
}

string SpiceManager::dateFromEphemerisTime(double ephemerisTime,
                                           const string& formatString) const
{
    ghoul_assert(!formatString.empty(), "Format is empty");
    
    static const int BufferSize = 256;
    SpiceChar buffer[BufferSize];
    timout_c(ephemerisTime, formatString.c_str(), BufferSize - 1, buffer);
    throwOnSpiceError(
        format("Error converting ephemeris time '{}' to date with format '{}'",
               ephemerisTime,
               formatString
        )
    );
    
    return std::string(buffer);
    
}

glm::dvec3 SpiceManager::targetPosition(const std::string& target,
	                                    const std::string& observer,
	                                    const std::string& referenceFrame,
                                        AberrationCorrection aberrationCorrection,
	                                    double ephemerisTime,
	                                    double& lightTime) const
{
    ghoul_assert(!target.empty(), "Target is not empty");
    ghoul_assert(!observer.empty(), "Observer is not empty");
    ghoul_assert(!referenceFrame.empty(), "Reference frame is not empty");
    
	bool targetHasCoverage = hasSpkCoverage(target, ephemerisTime);
	bool observerHasCoverage = hasSpkCoverage(observer, ephemerisTime);
	if (!targetHasCoverage && !observerHasCoverage){
        throw SpiceKernelException(
            format("Neither the target '{}' nor observer '{}' has SPK coverage",
                   target,
                   observer
            )
        );
	}
	else if (targetHasCoverage && observerHasCoverage) {
        glm::dvec3 position;
		spkpos_c(
            target.c_str(),
            ephemerisTime,
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(position),
			&lightTime
        );
        throwOnSpiceError(format(
            "Error getting target position from '{}' to '{}' in reference frame '{}",
            target,
            observer,
            referenceFrame
        ));
        return position;
	}
        else if (targetHasCoverage) {
            // observer has no coverage
            return getEstimatedPosition(
                observer,
                target,
                referenceFrame,
                aberrationCorrection,
                ephemerisTime,
                lightTime
            ) * -1.0;
        }
        else {
            // target has no coverage
            return getEstimatedPosition(
                target,
                observer,
                referenceFrame,
                aberrationCorrection,
                ephemerisTime,
                lightTime
            );
        }
}

glm::dmat3 SpiceManager::frameTransformationMatrix(const std::string& from,
                                         const std::string& to,
                                         double ephemerisTime) const
{
    ghoul_assert(!from.empty(), "From must not be empty");
    ghoul_assert(!to.empty(), "To must not be empty");
    
    // get rotation matrix from frame A - frame B
    glm::dmat3 transform;
	pxform_c(
        from.c_str(),
        to.c_str(),
        ephemerisTime,
        reinterpret_cast<double(*)[3]>(glm::value_ptr(transform))
    );
	
    throwOnSpiceError(
        format("Error converting from frame '{}' to frame '{}' at time '{}'",
               from, to, ephemerisTime
        )
    );

    // The rox-major, column-major order are switched in GLM and SPICE, so we have to
    // transpose the matrix before we can return it
    return glm::transpose(transform);
}

bool SpiceManager::getSurfaceIntercept(const std::string& target,
                                       const std::string& observer,
                                       const std::string& fovFrame,
                                       const std::string& referenceFrame,
                                       AberrationCorrection aberrationCorrection,
                                       double ephemerisTime,
                                       glm::dvec3& directionVector,
                                       glm::dvec3& surfaceIntercept,
                                       glm::dvec3& surfaceVector,
                                       bool& isVisible
                                       ) const
{
    const std::string ComputationMethod = "ELLIPSOID";
    
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
    
    sincpt_c(ComputationMethod.c_str(),
             target.c_str(),
             ephemerisTime,
             bodyfixed.c_str(),
             aberrationCorrection,
             observer.c_str(),
             fovFrame.c_str(),
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
        srfvec = SpiceManager::ref().frameTransformationMatrix(bodyfixed, referenceFrame, ephemerisTime) * srfvec;
    
    if (found){
        memcpy(glm::value_ptr(directionVector), dvec, sizeof(dvec));
        memcpy(glm::value_ptr(surfaceIntercept), spoint, sizeof(spoint));
        surfaceVector = srfvec;
    }
    return true;
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
    
glm::dvec3 SpiceManager::getEstimatedPosition(const std::string& target,
                                              const std::string& observer,
                                              const std::string& referenceFrame,
                                              AberrationCorrection aberrationCorrection,
                                              double ephemerisTime,
                                              double& lightTime) const
{
    ghoul_assert(!target.empty(), "Target must not be empty");
    ghoul_assert(!observer.empty(), "Observer must not be empty");
    ghoul_assert(!referenceFrame.empty(), "Reference frame must not be empty");
    ghoul_assert(target != observer, "Target and observer must be different");
    
    int targetId = naifId(target);
    
    if (targetId == 0) {
        // SOLAR SYSTEM BARYCENTER special case, no definition in kernels
        return glm::dvec3(0.0);
    }
    
    if (_spkCoverageTimes.find(targetId) == _spkCoverageTimes.end()) {
        // no coverage
        throw SpiceKernelException(format("No position for '{}' at any time", target));
    }
    
    
    int observerId = naifId(observer);

    std::set<double> coveredTimes = _spkCoverageTimes.find(targetId)->second;
    
    glm::dvec3 pos;
    if (coveredTimes.lower_bound(ephemerisTime) == coveredTimes.begin()) {
        // coverage later, fetch first position
        spkpos_c(
            target.c_str(),
            *(coveredTimes.begin()),
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(pos),
            &lightTime
        );
        throwOnSpiceError(format(
            "Error estimating position for target '{}' with observer '{}' in frame '{}'",
            target, observer, referenceFrame
        ));
    }
    else if (coveredTimes.upper_bound(ephemerisTime) == coveredTimes.end()) {
        // coverage earlier, fetch last position
        spkpos_c(
            target.c_str(),
            *(coveredTimes.rbegin()),
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(pos),
            &lightTime
        );
        throwOnSpiceError(format(
            "Error estimating position for target '{}' with observer '{}' in frame '{}'",
            target, observer, referenceFrame
        ));
    }
    else {
        // coverage both earlier and later, interpolate these positions
        glm::dvec3 posEarlier;
        double ltEarlier;
        double timeEarlier = *std::prev((coveredTimes.lower_bound(ephemerisTime)));
        spkpos_c(
            target.c_str(),
            timeEarlier,
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(posEarlier),
            &ltEarlier
        );
        
        glm::dvec3 posLater;
        double ltLater;
        double timeLater = *(coveredTimes.upper_bound(ephemerisTime));
        spkpos_c(
            target.c_str(),
            timeLater,
            referenceFrame.c_str(),
            aberrationCorrection,
            observer.c_str(),
            glm::value_ptr(posLater),
            &ltLater
        );

        throwOnSpiceError(format(
            "Error estimating position for target '{}' with observer '{}' in frame '{}'",
            target, observer, referenceFrame
        ));

        // linear interpolation
        double timeDifference = timeLater - timeEarlier;
        double t = (ephemerisTime - timeEarlier) / timeDifference;
        pos = posEarlier * (1.0 - t) + posLater * t;
        lightTime = ltEarlier * (1.0 - t) + ltLater * t;
    }

    return pos;
}

} // namespace openspace
