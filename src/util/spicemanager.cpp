/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014                                                                    *
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


namespace {
	const std::string _loggerCat = "SpiceManager";
}

namespace openspace {

SpiceManager* SpiceManager::_manager = nullptr;

void SpiceManager::initialize() {
	assert(_manager == nullptr);
	_manager = new SpiceManager;
	_manager->_lastAssignedKernel = 0;

	char REPORT[]="REPORT";
	char NONE[]="NONE";

	// Set the SPICE library to not exit the program if an error occurs
	erract_c("SET", 0, REPORT);
	// But we do not want SPICE to print the errors, we will fetch them ourselves
	errprt_c("SET", 0, NONE);
}

void SpiceManager::deinitialize() {
	for (const KernelInformation& i : _manager->_loadedKernels)
		unload_c(i.path.c_str());

	delete _manager;
	_manager = nullptr;
	char DEFAULT[]="DEFAULT";

	// Set values back to default
	erract_c("SET", 0, DEFAULT);
	errprt_c("SET", 0, DEFAULT);
}

SpiceManager& SpiceManager::ref() {
	assert(_manager != nullptr);
	return *_manager;
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

	KernelIdentifier kernelId = ++_lastAssignedKernel;

	// We need to set the current directory as meta-kernels are usually defined relative
	// to the directory they reside in. The directory change is not necessary for regular
	// kernels

	ghoul::filesystem::Directory currentDirectory = FileSys.currentDirectory();
	std::string&& fileDirectory = ghoul::filesystem::File(path).directoryName();

	if (!FileSys.directoryExists(fileDirectory)) {
		LERROR("Could not find directory for kernel '" << path << "'");
		return KernelFailed;
	}
	FileSys.setCurrentDirectory(fileDirectory);

	// Load the kernel
	furnsh_c(path.c_str());
	
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
		KernelInformation&& info = { path, std::move(kernelId) };
		_loadedKernels.push_back(info);
		return kernelId;
	}
}

void SpiceManager::unloadKernel(KernelIdentifier kernelId) {
	auto it = std::find_if(_loadedKernels.begin(), _loadedKernels.end(),
		[&kernelId](const KernelInformation& info) { return info.id == kernelId ; });

	if (it != _loadedKernels.end()) {
		// No need to check for errors as we do not allow empty path names
		unload_c(it->path.c_str());
		_loadedKernels.erase(it);
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
		unload_c(path.c_str());
		_loadedKernels.erase(it);
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
		bods2c_c(body.c_str(), &id, &success);
        if (success == SPICEFALSE)
            LERROR("Could not find NAIF ID of body '" + body + "'");
		return (success == SPICETRUE);
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

	int n;
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

	int n;
	bodvrd_c(body.c_str(), value.c_str(), static_cast<SpiceInt>(v.size()), &n, &v[0]);

	bool hasError = checkForError("Error getting value '" + value + "' for body '" + 
		body + "'");
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
	const std::string& format)
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
    spkpos_c(target.c_str(), ephemerisTime, referenceFrame.c_str(),
             aberrationCorrection.c_str(), observer.c_str(), glm::value_ptr(position),
             &lightTime);

    bool hasError = checkForError("Error retrieving position of target '" + target + "'" +
                                  " viewed from observer '" + observer + "' in reference"+
                                  " frame '" + referenceFrame + "' at time '" +
                                  std::to_string(ephemerisTime) + "'");
    return !hasError;
}

bool SpiceManager::getTargetPosition(const std::string& target,
						   const std::string& observer,
		                   const std::string& referenceFrame, 
						   const std::string& aberrationCorrection,
		                   double ephemerisTime,
						   psc& position, 
						   double& lightTime) const
{
	glm::dvec3 pos;
	bool success = getTargetPosition(target, observer, referenceFrame,
		aberrationCorrection, ephemerisTime, pos, lightTime);
	
	if(!success)
		return false;

	position = PowerScaledCoordinate::CreatePowerScaledCoordinate(pos[0], pos[1], pos[2]);

	return true;
}

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

	position = PowerScaledCoordinate::CreatePowerScaledCoordinate(state[0], state[1], state[2]);
	velocity = PowerScaledCoordinate::CreatePowerScaledCoordinate(state[3], state[4], state[5]);

	return true;
}

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

bool SpiceManager::getPositionPrimeMeridian(const std::string& fromFrame,
	const std::string& body,
	double ephemerisTime,
	glm::dmat3& positionMatrix) const{

	int id;
	getNaifId(body.c_str(), id);
	tipbod_c(fromFrame.c_str(), id, ephemerisTime, (double(*)[3])glm::value_ptr(positionMatrix));

	bool hasError = checkForError("Error retrieving position transform matrix from "
		"frame '" + fromFrame + "' to frame '" + body +
		"at time '" + std::to_string(ephemerisTime) + "'");
	positionMatrix = glm::transpose(positionMatrix);

	return !hasError;
}

bool SpiceManager::getPositionTransformMatrix(const std::string& fromFrame,
												 const std::string& toFrame,
												 double ephemerisTime,
												 glm::dmat3& positionMatrix) const{
	pxform_c(fromFrame.c_str(), toFrame.c_str(),
		ephemerisTime, (double(*)[3])glm::value_ptr(positionMatrix));
    
    bool hasError = checkForError("Error retrieving position transform matrix from "
                                  "frame '" + fromFrame + "' to frame '" + toFrame +
                                  "' at time '" + std::to_string(ephemerisTime) + "'");
    positionMatrix = glm::transpose(positionMatrix);
    
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

	int nrReturned;
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

bool SpiceManager::geographicToRectangular(const std::string& body,
											   double longitude,
											   double latitude,
											   glm::dvec3& coordinates) const
{
	int id;
	bool success = getNaifId(body, id);
	if (!success)
		return false;
	else
		return geographicToRectangular(id, longitude, latitude, coordinates);
}

bool SpiceManager::geographicToRectangular(int id, double longitude, double latitude,
								               glm::dvec3& coordinates) const
{
	srfrec_c(id, longitude*rpd_c(), latitude*rpd_c(), glm::value_ptr(coordinates));
	bool hasError = checkForError("Error transforming geographic coordinates for '" +
		std::to_string(id) + "'");
	return !hasError;
}

bool SpiceManager::getSubObserverPoint(const std::string& target,
	                                   const std::string& observer,
									   const std::string& computationMethod,
	                                   const std::string& bodyFixedFrame,
	                                   const std::string& aberrationCorrection,
	                                   double      ephemerisTime,
	                                   glm::dvec3& subObserverPoint,
	                                   double&     targetEphemerisTime,
	                                   glm::dvec3& vectorToSurfacePoint) const
{
	if (target.empty()) {
		LERROR("No target was provided");
		return false;
	}
	if (observer.empty()) {
		LERROR("No observer was provided");
		return false;
	}
	if (computationMethod.empty()) {
		LERROR("No computation method was provided");
		return false;
	}
	if (bodyFixedFrame.empty()) {
		LERROR("No body fixed frame was provided");
		return false;
	}
	if (aberrationCorrection.empty()) {
		LERROR("No aberration correction was provided");
		return false;
	}
	
	subpnt_c(computationMethod.c_str(), 
		     target.c_str(), 
			 ephemerisTime,
			 bodyFixedFrame.c_str(), 
			 aberrationCorrection.c_str(), 
			 observer.c_str(),
			 glm::value_ptr(subObserverPoint),
			 &targetEphemerisTime,
			 glm::value_ptr(vectorToSurfacePoint)
			 );

	bool hasError = checkForError("Error retrieving subobserver point on target '" +
		target + "' of observer '" + observer + "' for computation method '" +
		computationMethod + "' and body fixed frame '" + bodyFixedFrame + "'");
	return !hasError;
}

void SpiceManager::applyTransformationMatrix(glm::dvec3& position,
											 glm::dvec3& velocity,
											 const TransformMatrix& transformationMatrix)
{
	double input[6];
	double output[6];
	memmove(input, glm::value_ptr(position), 3 * sizeof(glm::dvec3::value_type));
	memmove(input + 3, glm::value_ptr(velocity), 3 * sizeof(glm::dvec3::value_type));
	mxvg_c(transformationMatrix.data(), input, 6, 6, output);
	memmove(glm::value_ptr(position), output, 3 * sizeof(glm::dvec3::value_type));
	memmove(glm::value_ptr(velocity), output + 3, 3 * sizeof(glm::dvec3::value_type));
}

bool SpiceManager::checkForError(std::string errorMessage) {
	static char msg[1024];

	int failed = failed_c();
    if (failed) {
		if (!errorMessage.empty()) {
			getmsg_c("LONG", 1024, msg);
			LERROR(errorMessage);
			LERROR("Spice reported: " + std::string(msg));
		}
        reset_c();
        return true;
    }
	return false;
}

//bool SpiceManager::getSubSolarPoint(std::string computationMethod,
//	                                std::string target,
//	                                double      ephemeris,
//	                                std::string bodyFixedFrame,
//	                                std::string aberrationCorrection,
//	                                std::string observer,
//									glm::dvec3& subSolarPoint,
//	                                double&     targetEpoch,
//	                                glm::dvec3& vectorToSurfacePoint) const{
//	double subPoint[3], vecToSurf[3];
//
//	subslr_c(computationMethod.c_str(),
//		     target.c_str(),
//		     ephemeris,
//		     bodyFixedFrame.c_str(),
//		     aberrationCorrection.c_str(),
//		     observer.c_str(), subPoint, &targetEpoch, vecToSurf);
//
//	memcpy(&subSolarPoint, subPoint, sizeof(double)* 3);
//	memcpy(&vectorToSurfacePoint, vecToSurf, sizeof(double)* 3);
//
//	return true;
//}

}
