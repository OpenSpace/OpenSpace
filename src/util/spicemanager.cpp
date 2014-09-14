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

#include <stdio.h>
#include <iostream>
#include <cassert>
#include <cstring>

#ifdef WIN32
#include <Windows.h>
#endif

#include "openspace/util/spicemanager.h"
#include "ghoul/filesystem/filesystem.h"
#include "ghoul/logging/logmanager.h"

namespace {
	const std::string _loggerCat = "SpiceManager";
}

namespace openspace{
SpiceManager* SpiceManager::_manager = nullptr;

SpiceManager::~SpiceManager(){
	// cleanup...
}

void SpiceManager::initialize(){
	assert(_manager == nullptr);
	if (_manager == nullptr) _manager = new SpiceManager;
	assert(_manager != nullptr);
}

void SpiceManager::deinitialize(){
	assert(_manager != nullptr);
	delete _manager;
	_manager = nullptr;
}

SpiceManager& SpiceManager::ref() {
	assert(_manager != nullptr);
	return *_manager;
}

int SpiceManager::loadKernel(const std::string& fullPath, const std::string& shorthand){
	unsigned int kernelId = ++_kernelCount;
	assert(kernelId > 0);

	std::string currentDirectory = FileSys.currentDirectory();
	std::string::size_type last = fullPath.find_last_of(ghoul::filesystem::FileSystem::PathSeparator);
	if (last == std::string::npos)
		return false;
	std::string kernelDir = fullPath.substr(0, last);
	FileSys.setCurrentDirectory(kernelDir);
	furnsh_c(fullPath.c_str());
	FileSys.setCurrentDirectory(currentDirectory);

	spiceKernel current = { fullPath, 
		                    shorthand, 
						    kernelId  };

	_loadedKernels.push_back(current);

	return kernelId;
}

bool SpiceManager::unloadKernel(const std::string& shorthand){
	assert(shorthand != "");

	std::vector<spiceKernel>::iterator it;
	for (it = _loadedKernels.begin(); it != _loadedKernels.end(); it++){
		if (it->name == shorthand){
			unload_c((it->path).c_str());
			return true;
		}
	}
	return false;
}

bool SpiceManager::unloadKernel(int kernelId){
	std::vector<spiceKernel>::iterator it;
	for (it = _loadedKernels.begin(); it != _loadedKernels.end(); it++){
		if (it->id == kernelId){
			unload_c((it->path).c_str());
			return true;
		}
	}
	return false;
}

bool SpiceManager::hasValue(int naifId, const std::string& kernelPoolValue) const{
	return bodfnd_c(naifId, kernelPoolValue.c_str());
}

// 1D
bool SpiceManager::getValueFromID(const std::string& bodyname,
	                              const std::string& kernelPoolValue,
	                              double& value) const{
	int n;
	int code;
	int found;

	bodn2c_c(bodyname.c_str(), &code, &found);
	if (!found) return false;
	bodvrd_c(bodyname.c_str(), kernelPoolValue.c_str(), 1, &n, &value);

	return true;
}
// 2D
/*
bool SpiceManager::getValueFromID(const std::string& bodyname,
		                          const std::string& kernelPoolValueName,
	                              glm::dvec2& value) const{
	int n;
	double val[2];
	int code;
	int found;

	bodn2c_c(bodyname.c_str(), &code, &found);
	if (!found) return false;
	bodvrd_c(bodyname.c_str(), kernelPoolValueName.c_str(), 2, &n, val);

	value[0] = val[0];
	value[1] = val[1];

	return true;

}
*/
// 3D
bool SpiceManager::getValueFromID(const std::string& bodyname,
	                              const std::string& kernelPoolValueName,
	                              glm::dvec3& value) const{
	int n;
	double val[3];
	int code;
	int found;

	bodn2c_c(bodyname.c_str(), &code, &found);
	if (!found) return false;
	bodvrd_c(bodyname.c_str(), kernelPoolValueName.c_str(), 3, &n, val);

	memcpy(&value, val, sizeof(double)* 3);

	return true;
}
// 4D
/*
bool SpiceManager::getValueFromID(const std::string& bodyname,
	                              const std::string& kernelPoolValueName,
	                              glm::dvec4& value) const{
	int n;
	double val[4];
	int code;
	int found;

	bodn2c_c(bodyname.c_str(), &code, &found);
	if (!found) return false;
	bodvrd_c(bodyname.c_str(), kernelPoolValueName.c_str(), 4, &n, val);

	value[0] = val[0];
	value[1] = val[1];
	value[2] = val[2];
	value[3] = val[3];

	return true;
}
*/
// ND
bool SpiceManager::getValueFromID(const std::string& bodyname,
	                              const std::string& kernelPoolValueName,
							      std::vector<double>& values, 
								  unsigned int num) const{
	int n;
	double *val;
	val = (double*)malloc(num*sizeof(double));
	int code;
	int found;

	bodn2c_c(bodyname.c_str(), &code, &found);
	if (!found) return false;
	bodvrd_c(bodyname.c_str(), kernelPoolValueName.c_str(), num, &n, val);

	for (int i = 0; i < num; i++){
		values.push_back(val[i]);
	}

	return true;
}

double SpiceManager::stringToEphemerisTime(const std::string& epochString) const{
	double et;
	str2et_c(epochString.c_str(), &et);
	return et;
}

bool SpiceManager::getTargetPosition(const std::string& target,
	                                 double ephemerisTime,
	                                 const std::string& referenceFrame,
	                                 const std::string& aberrationCorrection,
	                                 const std::string& observer,
	                                 glm::dvec3& targetPosition,
	                                 double lightTime) const{
	double pos[3] = { NULL, NULL, NULL };
	//method to put error out...
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
	                              double lightTime) const{
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
			boundsArr[j + i*3] = NULL;
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
