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

// open space includes
#include <openspace/util/ImageSequencer2.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/cachemanager.h>

#include <openspace/util/spicemanager.h>
#include <fstream>
#include <iterator>
#include <iomanip>
#include <limits>

namespace {
const std::string _loggerCat = "ImageSequencer2";
}

namespace openspace {

ImageSequencer2* ImageSequencer2::_instance = nullptr;

ImageSequencer2::ImageSequencer2() :
_hasData(false),
_defaultCaptureImage(absPath("${OPENSPACE_DATA}/scene/common/textures/placeholder_blank.png"))
{}


ImageSequencer2& ImageSequencer2::ref() {
	assert(_instance != nullptr);
	return *_instance;
}
void ImageSequencer2::initialize() {
	assert(_instance == nullptr);
	_instance = new ImageSequencer2;
}

void ImageSequencer2::deinitialize() {
	delete _instance;
	_instance = nullptr;
}

bool ImageSequencer2::isReady(){
	return _hasData;
}

bool ImageSequencer2::imageComparer(const Image &a, const Image &b){
	return a.startTime < b.startTime;
};

std::vector<Image>::iterator ImageSequencer2::binary_find(std::vector<Image>::iterator begin,
	std::vector<Image>::iterator end,
	const Image &val,
	bool(*compareFunc)(const Image &a, const Image &b)){
	// Finds the lower bound in at most log(last - first) + 1 comparisons
	std::vector<Image>::iterator it = std::lower_bound(begin, end, val, compareFunc);
	if (it != begin && it != end){
		return it;
	}
	return end;
}

void ImageSequencer2::updateSequencer(double time){
	if (_currentTime != time){
		_previousTime = _currentTime;
		_currentTime = time;
	}
}

std::pair<double, std::string> ImageSequencer2::getNextTarget(){
	// make into template func
	auto compareTime = [](const std::pair<double, std::string> &a,
	                 	  const std::pair<double, std::string> &b)->bool{
		return a.first < b.first;
	};
	std::pair<double, std::string> findEqualToThis;
	findEqualToThis.first = _currentTime;
	auto it = std::lower_bound(_targetTimes.begin(), _targetTimes.end(), findEqualToThis, compareTime);

	if (it != _targetTimes.end()){
		return (*it);
	}
}

std::pair<double, std::string> ImageSequencer2::getCurrentTarget(){
	// make into template func
	auto compareTime = [](const std::pair<double, std::string> &a,
		const std::pair<double, std::string> &b)->bool{
		return a.first < b.first;
	};
	std::pair<double, std::string> findEqualToThis;
	findEqualToThis.first = _currentTime;
	auto it = std::lower_bound(_targetTimes.begin(), _targetTimes.end(), findEqualToThis, compareTime);

	if (it != _targetTimes.end() && it != _targetTimes.begin()){
		return *std::prev(it);
	}
    else
        return std::make_pair(0.0, "No Target");
}

std::pair<double, std::vector<std::string>> ImageSequencer2::getIncidentTargetList(int range){
	std::pair<double, std::vector<std::string>> incidentTargets;

	auto compareTime = [](const std::pair<double, std::string> &a,
		const std::pair<double, std::string> &b)->bool{
		return a.first < b.first;
	};
	std::pair<double, std::string> findEqualToThis;
	findEqualToThis.first = _currentTime;
	auto it = std::lower_bound(_targetTimes.begin(), _targetTimes.end(), findEqualToThis, compareTime);
	
	std::advance(it, -(range+1));

	for (int i = 0; i < 2*range+1; i++){
		incidentTargets.first = it->first;
		incidentTargets.second.push_back(it->second);
		it++;
		if (it == _targetTimes.end()) 
			break;
	}

	return incidentTargets;
}

double ImageSequencer2::getIntervalLength(){
	double upcoming = getNextCaptureTime();
	if (_nextCapture != upcoming){
		_nextCapture = upcoming;
		_intervalLength = upcoming - _currentTime;
	}
	return _intervalLength;
}

double ImageSequencer2::getNextCaptureTime(){
	auto compareTime = [](const double &a, const double &b)->bool{
		return a < b;
	};
	double nextCaptureTime = 0;
	auto it = std::lower_bound(_captureProgression.begin(), _captureProgression.end(), _currentTime, compareTime);
	if (it != _captureProgression.end())
		nextCaptureTime = *it;

	return nextCaptureTime;
}

std::vector<std::pair<std::string, bool>> ImageSequencer2::getActiveInstruments(){
	for (int i = 0; i < _instrumentOnOff.size(); i++){
		_instrumentOnOff[i].second = false;
	}
	for (auto key : _fileTranslation){
		for (auto instrumentID : key.second->getTranslation()){
				if (instumentActive(instrumentID)){
					for (int i = 0; i < _instrumentOnOff.size(); i++){
						if (instrumentID == _instrumentOnOff[i].first){
							_instrumentOnOff[i].second = true;
						}
					}
				}
			}
		}
	return _instrumentOnOff;
}
bool ImageSequencer2::instumentActive(std::string instrumentID){
	for (auto i : _instrumentTimes){
		//check if this instrument is in range
		if (i.second.inRange(_currentTime)){
			//if so, then get the corresponding spiceIDs
			std::vector < std::string> spiceIDs = _fileTranslation[i.first]->getTranslation();
			//check which specific subinstrument is firing
			for (auto s : spiceIDs){
				if (s == instrumentID){
					return true;
				}
			}
		}
	}
	return false;
}
bool ImageSequencer2::getImagePaths(std::vector<std::pair<double, std::string>>& captures,  std::string projectee, std::string instrumentID){
	if (!instumentActive(instrumentID) && !Time::ref().timeJumped()) return false;
	return (instrumentID == "NH_LORRI") ? getImagePaths(captures, projectee) : false;
}

bool ImageSequencer2::getImagePaths(std::vector<std::pair<double, std::string>>& captures, 
	                                std::string projectee){
	if (_subsetMap[projectee]._range.inRange(_currentTime) ||
		_subsetMap[projectee]._range.inRange(_previousTime)){
		auto compareTime = [](const Image &a,
			const Image &b)->bool{
			return a.startTime < b.startTime;
		};

		auto begin = _subsetMap[projectee]._subset.begin();
		auto end = _subsetMap[projectee]._subset.end();

		std::vector<std::pair<double, std::string>> captureTimes;
		Image findPrevious;
		findPrevious.startTime = _previousTime;
		Image findCurrent;
		findCurrent.startTime = _currentTime;

		auto curr = std::lower_bound(begin, end, findCurrent, compareTime);
		auto prev = std::lower_bound(begin, end, findPrevious, compareTime);

		if (curr != begin && curr != end  && prev != begin && prev != end && prev < curr){
			std::transform(prev, curr, std::back_inserter(captureTimes),
				[](const Image& i) {
				return std::make_pair(i.startTime, i.path);
			});
			std::reverse(captureTimes.begin(), captureTimes.end());
			captures = captureTimes;
			return true;
		}
	}
	return false;
}


void ImageSequencer2::runSequenceParser(SequenceParser* parser){
	parser->create();
	_fileTranslation    = parser->getTranslation(); // should perhaps be named 'instrumentTranslation'
	_subsetMap          = parser->getSubsetMap();
	_instrumentTimes    = parser->getIstrumentTimes();
	_targetTimes        = parser->getTargetTimes();
	_captureProgression = parser->getCaptureProgression();

	//copy payload from _fileTranslation 
	for (auto t : _fileTranslation){
		std::vector<std::string> spiceIDs = t.second->getTranslation();
		for (auto id : spiceIDs){
			_instrumentOnOff.push_back(std::make_pair(id, false));
		}
	}
	_instrumentOnOff.erase(std::unique(_instrumentOnOff.begin(),
									   _instrumentOnOff.end()),
									   _instrumentOnOff.end());
	_hasData = true;

}
}  // namespace openspace
