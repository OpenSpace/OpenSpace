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

#ifdef WIN32
#include <Windows.h>
#endif

#include "openspace/util/spicemanager.h"
#include "ghoul/filesystem/filesystem.h"

#define BUFSIZE 64

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

	unsigned int kernelId = ++kernelCount;
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
						   kernelId };

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
	SpiceInt code;
	SpiceBoolean found;

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
	SpiceDouble val[2];
	SpiceInt code;
	SpiceBoolean found;

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
	SpiceDouble val[3];
	SpiceInt code;
	SpiceBoolean found;

	bodn2c_c(bodyname.c_str(), &code, &found);
	if (!found) return false;
	bodvrd_c(bodyname.c_str(), kernelPoolValueName.c_str(), 3, &n, val);

	value[0] = val[0];
	value[1] = val[1];
	value[2] = val[2];

	return true;
}
// 4D
/*
bool SpiceManager::getValueFromID(const std::string& bodyname,
	                              const std::string& kernelPoolValueName,
	                              glm::dvec4& value) const{
	int n;
	SpiceDouble val[4];
	SpiceInt code;
	SpiceBoolean found;

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
							      std::vector<double>& values, unsigned int num) const{
	SpiceInt n;
	SpiceDouble *val;
	val = (SpiceDouble*)malloc(num*sizeof(SpiceDouble));
	SpiceInt code;
	SpiceBoolean found;

	bodn2c_c(bodyname.c_str(), &code, &found);
	if (!found) return false;
	bodvrd_c(bodyname.c_str(), kernelPoolValueName.c_str(), num, &n, val);

	for (int i = 0; i < num; i++){
		values.push_back(val[i]);
	}

	return true;
}

double SpiceManager::stringToEphemerisTime(const std::string& epochString) const{
	SpiceDouble et;
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
	spkpos_c(target.c_str(), ephemerisTime, referenceFrame.c_str(), 
		     aberrationCorrection.c_str(), observer.c_str(), pos, &lightTime);
	
	if (pos[0] == NULL || 
		pos[1] == NULL || 
		pos[2] == NULL) 
		return false;

	targetPosition[0] = pos[0];
	targetPosition[1] = pos[1];
	targetPosition[2] = pos[2];

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

	for (int i = 0; i < 3; i++){
		if (state[i] == NULL || state[i + 3] == NULL){
			return false;
		}
		targetPosition[i] = state[i];
		targetVelocity[i] = state[i+3];
	}
	return true;
}

bool SpiceManager::getStateTransformMatrix(const std::string& fromFrame,
							const std::string& toFrame,
							double ephemerisTime,
							mat6x6& stateMatrix) const{

	sxform_c(fromFrame.c_str(), toFrame.c_str(), ephemerisTime, stateMatrix);
	//error handling?
	return true;
}


}
