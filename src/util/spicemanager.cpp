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

	// Set the SPICE library to not exit the program if an error occurs
	erract_c("SET", 0, "REPORT");
	// But we do not want SPICE to print the errors, we will fetch them ourselves
	errprt_c("SET", 0, "NONE");
}

void SpiceManager::deinitialize() {
	for (const KernelInformation& i : _manager->_loadedKernels)
		unload_c(i.path.c_str());

	delete _manager;
	_manager = nullptr;
	_manager->_lastAssignedKernel = 0;

	// Set values back to default
	erract_c("SET", 0, "DEFAULT");
	errprt_c("SET", 0, "DEFAULT");
}

SpiceManager& SpiceManager::ref() {
	assert(_manager != nullptr);
	return *_manager;
}

SpiceManager::KernelIdentifier SpiceManager::loadKernel(std::string filePath) {
	if (filePath.empty()) {
		LERROR("No filename provided");
		return KernelFailed;
	}

	KernelIdentifier kernelId = ++_lastAssignedKernel;

	// We need to set the current directory as meta-kernels are usually defined relative
	// to the directory they reside in. The directory change is not necessary for regular
	// kernels
	std::string&& path = absPath(std::move(filePath));
	ghoul::filesystem::Directory currentDirectory = FileSys.currentDirectory();
	std::string&& fileDirectory = ghoul::filesystem::File(path).directoryName();
	FileSys.setCurrentDirectory(fileDirectory);

	// Load the kernel
	furnsh_c(path.c_str());
	
	// Reset the current directory to the previous one
	FileSys.setCurrentDirectory(currentDirectory);

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
		return (success == SPICETRUE);
	}
}

template <typename T, size_t S>
bool getValueInternal(const std::string& body, const std::string& value, T& v) {
	if (body.empty()) {
		LERROR("No body was provided");
		return false;
	}
	if (value.empty()) {
		LERROR("No value was provided");
		return false;
	}

	int n;
	bodvrd_c(body.c_str(), value.c_str(), S, &n, &v);

	bool hasError = checkForError("Error getting value '" + value + "' for body '" + 
		body + "'");
	return !hasError;
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	double& v) const
{
	return getValueInternal<double, 1>(body, value, v);
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	glm::dvec3& v) const
{
	return getValueInternal<glm::dvec3, 3>(body, value, v);
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	glm::dvec4& v) const
{
	return getValueInternal<glm::dvec4, 4>(body, value, v);
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
	if (v.size() > 0) {
		LERROR("Array for values has to be preallocaed");
		return false;
	}

	int n;
	bodvrd_c(body.c_str(), value.c_str(), static_cast<SpiceInt>(v.size()), &n, &v[0]);

	bool hasError = checkForError("Error getting value '" + value + "' for body '" + 
		body + "'");
	return !hasError;
}

bool SpiceManager::getETfromDate(const std::string& epochString,
	double& ephemerisTime) const
{
	str2et_c(epochString.c_str(), &ephemerisTime);
	int failed = failed_c();
	if (failed) {
		char msg[1024];
		getmsg_c("LONG", 1024, msg);
		LERROR("Error converting date '" + epochString+ "'");
		LERROR("Spice reported: " + std::string(msg));
		reset_c();
		return false;
	}
	return true;
}

bool SpiceManager::getDateFromET(double ephemerisTime, std::string& date,
	const std::string& format)
{
	static const int BufferSize = 256;
	SpiceChar buffer[BufferSize];

	timout_c(ephemerisTime, format.c_str(), BufferSize - 1, buffer);
	int failed = failed_c();
	if (failed) {
		char msg[1024];
		getmsg_c("LONG", 1024, msg);
		LERROR("Error converting ephemeris time to date with format '" + format + "'");
		LERROR("Spice reported: " + std::string(msg));
		reset_c();
		return false;
	}

	date = std::string(buffer);
	return true;
}

bool SpiceManager::getTargetPosition(const std::string& target,
	                                 const std::string& observer,
	                                 const std::string& referenceFrame,
	                                 const std::string& aberrationCorrection,
	                                 double ephemerisTime,
	                                 glm::dvec3& targetPosition,
	                                 double& lightTime) const{
	double pos[3] = { 0.0, 0.0, 0.0 };
	
	spkpos_c(target.c_str(), ephemerisTime, referenceFrame.c_str(), 
		     aberrationCorrection.c_str(), observer.c_str(), pos, &lightTime);
    
    int failed = failed_c();
    if(failed) {
        char msg[1024];
        getmsg_c ( "LONG", 1024, msg );
        LERROR("Error retrieving position of target '" + target + "'");
        LERROR("Spice reported: " + std::string(msg));
        reset_c();
        return false;
    }

	
	memcpy(&targetPosition, pos, sizeof(double)* 3);

	return true;
}
bool SpiceManager::getTargetState(const std::string& target,
	                              const std::string& observer,
	                              const std::string& referenceFrame,
	                              const std::string& aberrationCorrection,
	                              double ephemerisTime,
	                              glm::dvec3& targetPosition,
	                              glm::dvec3& targetVelocity,
	                              double& lightTime) const{
	double state[6];
	std::fill_n(state, 6, NULL);

	spkezr_c(target.c_str(), ephemerisTime, referenceFrame.c_str(),
	    	aberrationCorrection.c_str(), observer.c_str(), state, &lightTime);
    
    int failed = failed_c();
    if(failed) {
        char msg[1024];
        getmsg_c ( "LONG", 1024, msg );
        LERROR("Error retrieving state of target '" + target + "'");
        LERROR("Spice reported: " + std::string(msg));
        reset_c();
        return false;
    }

	for (int i = 0; i < 3; i++){
		memcpy(&targetPosition, state   , sizeof(double)* 3);
		memcpy(&targetVelocity, state +3, sizeof(double)* 3);
	}
	return true;
}

bool SpiceManager::getStateTransformMatrix(const std::string& fromFrame,
							const std::string& toFrame,
							double ephemerisTime,
							TransformMatrix& stateMatrix) const{
	sxform_c(fromFrame.c_str(), toFrame.c_str(), 
		     ephemerisTime, (double(*)[6])stateMatrix.data());
	return true; 
}

bool SpiceManager::getPositionTransformMatrix(const std::string& fromFrame,
												 const std::string& toFrame,
												 double ephemerisTime,
												 glm::dmat3& positionMatrix) const{
	pxform_c(fromFrame.c_str(), toFrame.c_str(),
		ephemerisTime, (double(*)[3])glm::value_ptr(positionMatrix));
	positionMatrix = glm::transpose(positionMatrix);
	return true;
}

bool SpiceManager::getFieldOfView(const std::string& instrument,
	                              std::string& fovShape,
	                              std::string& frameName,
	                              glm::dvec3& boresightVector,
	                              std::vector<glm::dvec3>& bounds) const{
	int found;
	int naifId;
	int maxVectors = 12;
	int nrReturned;
	double *boundsArr = new double[maxVectors * 3];

	for (int i = 0; i < maxVectors; i++){
		for (int j = 0; j < 3; j++){
			boundsArr[j + i*3] = 0.0;
		}
	}

	bodn2c_c(instrument.c_str(), &naifId, &found);
	if (!found) return false;

	if (fovShape.size() != 0 && frameName.size() != 0){
		getfov_c(naifId, 
			     maxVectors,
				 fovShape.size(), 
				 frameName.size(), 
				 const_cast<char*>(fovShape.c_str()),
			     const_cast<char*>(frameName.c_str()), 
				 glm::value_ptr(boresightVector), 
				 &nrReturned,
				 (double(*)[3])boundsArr);
	}else{
		std::cout << "Frame name and FOV shape \
			          need to be preallocated" << std::endl;
		return false;
 	}

	for (int i = 0; i < nrReturned; i++){
		glm::dvec3 tmp;
		for (int j = 0; j < 3; j++){
			tmp[j] = boundsArr[j + i*3];
		}
		bounds.push_back(tmp);
	}
	return true;
}

bool SpiceManager::planetocentricToRectangular(const  std::string& body,
								               double longitude,
								               double latitude,
								               glm::dvec3& coordinates) const{
	int naifId;
	int found;
	double rectangular[3];

	bodn2c_c(body.c_str(), &naifId, &found);
	if (!found) return false;
	srfrec_c(naifId, longitude*rpd_c(), latitude*rpd_c(), rectangular);

	memcpy(&coordinates, rectangular, sizeof(double) * 3);

	return true;
}

bool SpiceManager::getSubObserverPoint(std::string target,
	                                   std::string observer,
									   std::string computationMethod,
	                                   std::string bodyFixedFrame,
	                                   std::string aberrationCorrection,
	                                   double      ephemerisTime,
	                                   glm::dvec3& subObserverPoint,
	                                   double&     targetEphemerisTime,
	                                   glm::dvec3& vectorToSurfacePoint) const{
	subpnt_c(computationMethod.c_str(), 
		     target.c_str(), 
			 ephemerisTime,
			 bodyFixedFrame.c_str(), 
			 aberrationCorrection.c_str(), 
			 observer.c_str(), glm::value_ptr(subObserverPoint), &targetEphemerisTime,
			 glm::value_ptr(vectorToSurfacePoint));

	return true;
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

bool SpiceManager::checkForError(std::string errorMessage) const {
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
