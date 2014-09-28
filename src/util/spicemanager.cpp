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

#include <algorithm>
#include <stdio.h>
#include <iostream>
#include <cassert>
#include <cstring>

#include <glm/gtc/type_ptr.hpp>

//#ifdef WIN32
//#include <Windows.h>
//#endif

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
	const std::string _loggerCat = "SpiceManager";
}

namespace openspace {

SpiceManager* SpiceManager::_manager = nullptr;
unsigned int SpiceManager::_lastAssignedKernel = 0;

void SpiceManager::initialize() {
	assert(_manager == nullptr);
	_manager = new SpiceManager;

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
	_lastAssignedKernel = 0;

	// Set values back to default
	erract_c("SET", 0, "DEFAULT");
	errprt_c("SET", 0, "DEFAULT");
}

SpiceManager& SpiceManager::ref() {
	assert(_manager != nullptr);
	return *_manager;
}

int SpiceManager::loadKernel(std::string filePath) {
	unsigned int kernelId = ++_lastAssignedKernel;
	assert(kernelId > 0);

	filePath = std::move(absPath(filePath));
	ghoul::filesystem::Directory currentDirectory = FileSys.currentDirectory();
	std::string&& fileDirectory = ghoul::filesystem::File(filePath).directoryName();
	FileSys.setCurrentDirectory(fileDirectory);

	furnsh_c(filePath.c_str());
	
	FileSys.setCurrentDirectory(currentDirectory);

	int failed = failed_c();
    if (failed) {
        char msg[1024];
        getmsg_c ( "LONG", 1024, msg );
        LERROR("Error loading kernel '" + filePath + "'");
        LERROR("Spice reported: " + std::string(msg));
        reset_c();
        return false;
    }


	KernelInformation&& info = { std::move(filePath), std::move(kernelId) };
	_loadedKernels.push_back(info);
	return kernelId;
}

void SpiceManager::unloadKernel(int kernelId) {
	auto it = std::find_if(_loadedKernels.begin(), _loadedKernels.end(),
		[&kernelId](const KernelInformation& info) { return info.id == kernelId ; });

	if (it != _loadedKernels.end()) {
		unload_c(it->path.c_str());
		_loadedKernels.erase(it);
	}
}

void SpiceManager::unloadKernel(std::string filePath) {
	filePath = absPath(filePath);
	unload_c(filePath.c_str());

	auto it = std::find_if(_loadedKernels.begin(), _loadedKernels.end(),
		[&filePath](const KernelInformation& info) { return info.path == filePath; });

	if (it != _loadedKernels.end())
		_loadedKernels.erase(it);
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
	SpiceBoolean success;
	bods2c_c(body.c_str(), &id, &success);
	return (success == SPICETRUE);
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	double& v) const
{
	int n;
	bodvrd_c(body.c_str(), value.c_str(), 1, &n, &v);

	int failed = failed_c();
	if (failed) {
		char msg[1024];
		getmsg_c("LONG", 1024, msg);
		LERROR("Error getting value '" << value << "' for body '" << body << "'");
		LERROR("Spice reported: " + std::string(msg));
		reset_c();
		return false;
	}

	return true;
}

bool SpiceManager::getValue(int id, const std::string& value, double& v) const {
	return getValue(std::to_string(id), value, v);
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	glm::dvec3& v) const
{
	int n;
	bodvrd_c(body.c_str(), value.c_str(), 3, &n, glm::value_ptr(v));

	int failed = failed_c();
	if (failed) {
		char msg[1024];
		getmsg_c("LONG", 1024, msg);
		LERROR("Error getting value '" << value << "' for body '" << body << "'");
		LERROR("Spice reported: " + std::string(msg));
		reset_c();
		return false;
	}

	return true;
}

bool SpiceManager::getValue(int id, const std::string& value, glm::dvec3& v) const
{
	return getValue(std::to_string(id), value, v);
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	glm::dvec4& v) const
{
	int n;
	bodvrd_c(body.c_str(), value.c_str(), 4, &n, glm::value_ptr(v));

	int failed = failed_c();
	if (failed) {
		char msg[1024];
		getmsg_c("LONG", 1024, msg);
		LERROR("Error getting value '" << value << "' for body '" << body << "'");
		LERROR("Spice reported: " + std::string(msg));
		reset_c();
		return false;
	}

	return true;
}

bool SpiceManager::getValue(int id, const std::string& value, glm::dvec4& v) const
{
	return getValue(std::to_string(id), value, v);
}

bool SpiceManager::getValue(const std::string& body, const std::string& value,
	std::vector<double>& v) const 
{
	assert(v.size() > 0);
	int n;
	bodvrd_c(body.c_str(), value.c_str(), static_cast<SpiceInt>(v.size()), &n, &v[0]);

	int failed = failed_c();
	if (failed) {
		char msg[1024];
		getmsg_c("LONG", 1024, msg);
		LERROR("Error getting value '" << value << "' for body '" << body << "'");
		LERROR("Spice reported: " + std::string(msg));
		reset_c();
		return false;
	}

	return true;
}

bool SpiceManager::getValue(int id, const std::string& value, 
	std::vector<double>& v) const
{
	return getValue(std::to_string(id), value, v);
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
	                                 double ephemerisTime,
	                                 const std::string& referenceFrame,
	                                 const std::string& aberrationCorrection,
	                                 const std::string& observer,
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
	                              double ephemerisTime,
	                              const std::string& referenceFrame,
	                              const std::string& aberrationCorrection,
	                              const std::string& observer,
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
							transformMatrix& stateMatrix) const{
	sxform_c(fromFrame.c_str(), toFrame.c_str(), 
		     ephemerisTime, (double(*)[6])stateMatrix.ptr());
	return true; 
}

bool SpiceManager::getPositionTransformMatrix(const std::string& fromFrame,
	                                          const std::string& toFrame,
	                                          double ephemerisTime,
											  transformMatrix& positionMatrix) const{

	pxform_c(fromFrame.c_str(), toFrame.c_str(), 
		     ephemerisTime, (double(*)[3])positionMatrix.ptr());

	return true;
}

void SpiceManager::getPositionTransformMatrixGLM(const std::string& fromFrame,
												 const std::string& toFrame,
												 double ephemerisTime,
												 glm::dmat3& positionMatrix) const{
	pxform_c(fromFrame.c_str(), toFrame.c_str(),
		ephemerisTime, (double(*)[3])glm::value_ptr(positionMatrix));
}

bool SpiceManager::getFieldOfView(const std::string& naifInstrumentId,
	                              std::string& fovShape,
	                              std::string& frameName,
	                              double boresightVector[],
	                              std::vector<glm::dvec3>& bounds,
								  int& nrReturned) const{
	int found;
	int naifId;
	int maxVectors = 12;
	double *boundsArr = new double[maxVectors * 3];

	for (int i = 0; i < maxVectors; i++){
		for (int j = 0; j < 3; j++){
			boundsArr[j + i*3] = 0.0;
		}
	}

	bodn2c_c(naifInstrumentId.c_str(), &naifId, &found);
	if (!found) return false;

	if (fovShape.size() != 0 && frameName.size() != 0){
		getfov_c(naifId, 
			     maxVectors,
				 fovShape.size(), 
				 frameName.size(), 
				 const_cast<char*>(fovShape.c_str()),
			     const_cast<char*>(frameName.c_str()), 
				 boresightVector, 
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

bool SpiceManager::rectangularToLatitudal(const glm::dvec3 coordinates,
	                                      double& radius,
	                                      double& longitude,
	                                      double& latitude) const{
	double point[3] = { coordinates.x, coordinates.y, coordinates.z };
	reclat_c(point, &radius, &longitude, &latitude);
	//check if returns values
	return (radius && longitude && latitude);
}

bool SpiceManager::latidudinalToRectangular(double  radius,
	                                        double& longitude,
	                                        double& latitude,
	                                        glm::dvec3& coordinates) const{
	double point[3] = { coordinates.x, coordinates.y, coordinates.z };
	latrec_c(radius, longitude, latitude, point);
	//check if returns values
	return (radius && longitude && latitude);
}

bool SpiceManager::planetocentricToRectangular(const   std::string& naifName,
								               double& longitude,
								               double& latitude,
								               glm::dvec3& coordinates) const{
	int naifId;
	int found;
	double rectangular[3];

	bodn2c_c(naifName.c_str(), &naifId, &found);
	if (!found) return false;
	srfrec_c(naifId, longitude*rpd_c(), latitude*rpd_c(), rectangular);

	memcpy(&coordinates, rectangular, sizeof(double) * 3);

	return true;
}

bool SpiceManager::getSubObserverPoint(std::string computationMethod,
	                                   std::string target,
	                                   double      ephemeris,
	                                   std::string bodyFixedFrame,
	                                   std::string aberrationCorrection,
	                                   std::string observer,
	                                   glm::dvec3& subObserverPoint,
	                                   double&     targetEpoch,
	                                   glm::dvec3& vectorToSurfacePoint) const{
	double subPoint[3], vecToSurf[3];
	
	subpnt_c(computationMethod.c_str(), 
		     target.c_str(), 
			 ephemeris,
			 bodyFixedFrame.c_str(), 
			 aberrationCorrection.c_str(), 
			 observer.c_str(), subPoint, &targetEpoch, vecToSurf);

	memcpy(&subObserverPoint    , subPoint , sizeof(double) * 3);
	memcpy(&vectorToSurfacePoint, vecToSurf, sizeof(double) * 3);

	return true;
}
bool SpiceManager::getSubSolarPoint(std::string computationMethod,
	                                std::string target,
	                                double      ephemeris,
	                                std::string bodyFixedFrame,
	                                std::string aberrationCorrection,
	                                std::string observer,
									glm::dvec3& subSolarPoint,
	                                double&     targetEpoch,
	                                glm::dvec3& vectorToSurfacePoint) const{
	double subPoint[3], vecToSurf[3];

	subslr_c(computationMethod.c_str(),
		     target.c_str(),
		     ephemeris,
		     bodyFixedFrame.c_str(),
		     aberrationCorrection.c_str(),
		     observer.c_str(), subPoint, &targetEpoch, vecToSurf);

	memcpy(&subSolarPoint, subPoint, sizeof(double)* 3);
	memcpy(&vectorToSurfacePoint, vecToSurf, sizeof(double)* 3);

	return true;
}

}
