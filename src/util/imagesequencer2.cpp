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
_defaultCaptureImage(absPath("C:/Users/michal/openspace/openspace-data/scene/common/textures/placeholder_blank.png"))
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
/*
auto cmp = [](const Image &a, const Image &b)->bool{
	return a.startTime < b.startTime;
};*/

bool ImageSequencer2::imageComparer(const Image &a, const Image &b){
	return a.startTime < b.startTime;
};

std::vector<Image>::iterator ImageSequencer2::binary_find(std::vector<Image>::iterator begin,
	std::vector<Image>::iterator end,
	const Image &val,
	bool(*compareFunc)(const Image &a, const Image &b)){
	// Finds the lower bound in at most log(last - first) + 1 comparisons
	std::vector<Image>::iterator it = std::lower_bound(begin, end, val, compareFunc);
	if (it != begin){
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

	if (it != _targetTimes.end()){
		return *std::prev(it);
	}
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

std::vector<std::string> ImageSequencer2::getActiveInstruments(){
	return _currentlyActiveInstruments;
}
bool ImageSequencer2::instumentActive(std::string instrumentID){
	// make into template func
	auto compareTime  = [](const std::pair<double, std::string> &a, 
		                   const std::pair<double, std::string> &b)->bool{
		return a.first < b.first;
	};

	std::pair<double, std::string> findEqualToThis;
	findEqualToThis.first = _currentTime;
	auto it = std::lower_bound(_instrumentTimes.begin(), _instrumentTimes.end(), findEqualToThis, compareTime);
	
	it = std::prev(it); 
	if (it != _instrumentTimes.end()){
		std::string key = it->second;
		std::vector<std::string> instruments = _acronymDictionary[key];
		for (auto i : instruments){
			if (i == instrumentID){
				_currentlyActiveInstruments = instruments;
				return true;
			}
		}
	}
	return false;
}

double ImageSequencer2::getNextCaptureTime(){
	// to do this we need getCurrentTarget to ALWAYS work!
	return 0.0;
}

void ImageSequencer2::runSequenceParser(SequenceParser* parser){
	parser->create();

	_subsetMap       = parser->getSubsetMap();
	_instrumentTimes = parser->getIstrumentTimes();
	//_targetTimes     = parser->getTargetTimes();
	_acronymDictionary = parser->getAcronymDictionary();
}

/*
bool ImageSequencer2::parsePlaybookFile(const std::string& fileName, 
	                                          std::string spacecraft, 
											  std::map<std::string, std::vector<std::string>> acronymDictionary,
											  std::vector<std::string> potentialTargets) {
	_acronymDictionary = acronymDictionary;
	if (size_t position = fileName.find_last_of(".") + 1){
		if (position != std::string::npos){
			std::string extension = ghoul::filesystem::File(fileName).fileExtension();

			if (extension == "txt"){// Hong Kang. pre-parsed playbook
				LINFO("Using Preparsed Playbook V9H");
				std::ifstream file(fileName);
				if (!file.good()) LERROR("Failed to open txt file '" << fileName << "'");

				std::string timestr = "";
				double shutter = 0.01;
				double startTime, stopTime;

				// for augmentation we only need the keys in acronymDictionary as a vector
				std::vector<std::string> payload;
				for (auto p : acronymDictionary)
					for (auto t : p.second)
						payload.push_back(t);
				payload.erase(std::unique(payload.begin(), payload.end()), payload.end());
				
				std::string previousInstrument;
				std::string previousTarget;

				double longExposureStart = 0;
				double longExposureStop = 0;
				std::string longExposureKeyword;

				do{
					std::getline(file, timestr);
					//For each instrument in acronym dictionary 
					for (auto it : _acronymDictionary){
						// check if first col in line has keyword of interest
						std::string keyword = timestr.substr(0, timestr.find_first_of(" "));
						auto pos = keyword.find(it.first);
						if (pos != std::string::npos){ 
							// grab the time
							std::string met = timestr.substr(24, 9);
							//convert to ephemeris time
							startTime = getETfromMet(met);
							
							//std::string checkIfMVIC = "MVIC";
							//int count = 0;
							//bool foundStopTime = false;
							//if (findExactMatch("MVIC", keyword)){
							//	std::string abortLine;
							//	int len = file.tellg();
							//	while (!file.eof() && !findExactMatch("ABORT", abortLine)){
							//		
							//		std::getline(file, abortLine);
							//		std::string abortCommand = abortLine.substr(0, abortLine.find_first_of(" "));
							//		count++;
							//		if (findExactMatch("ABORT", abortCommand)){
							//			met = abortLine.substr(24, 9);
							//			stopTime = getETfromMet(met);
							//			foundStopTime = true;
							//		}
							//	}
							//	file.seekg(len, std::ios_base::beg);
							//}else{
							//	//assume regular shutter speed;
							//	stopTime = startTime + shutter;
							//}
							//
							//if (foundStopTime){
							//	longExposureStart = startTime;
							//	longExposureStop = stopTime;
							//	longExposureKeyword = keyword;
							//}

							// retrieve corresponding SPICE call
							std::vector<std::string> instrument = it.second;

							// create image
							Image image;
							//stoptime- quick setting for now
							stopTime = startTime + 0.01;
							createImage(image, startTime, stopTime, instrument, "", _defaultCaptureImage);
							// add targets 
							augmentWithSpice(image, spacecraft, payload, potentialTargets);

							//SKA ERSATTAS MED NAGOT ANNAT - "instrument times"
							if (previousInstrument != it.first){
								previousInstrument = it.first;
								std::pair<double, std::string> v_time = std::make_pair(image.startTime, keyword);
								_instrumentTimes.push_back(v_time);
							}

							if (previousTarget != image.target ){
								previousTarget = image.target;
								std::pair<double, std::string> v_target = std::make_pair(image.startTime, image.target);
								_targetTimes.push_back(v_target);
							}
							
							//_subsetMap[image.target]._subset[keyword].push_back(image);
							_subsetMap[image.target]._subset.push_back(image);
							_subsetMap[image.target]._range.setRange(image.startTime);
						}
					}
				} while (!file.eof());
			}
		}
	}

	//PRINT FUNCTION
	std::string st1 = "2015-07-14T10:00:00.00";
	std::string st2 = "2015-07-14T12:00:00.00";
	double start, stop;

	SpiceManager::ref().getETfromDate(st1, start);
	SpiceManager::ref().getETfromDate(st2, stop);

	std::ofstream myfile;
	myfile.open("augmentedPef.txt");

	//print all
	for (auto target : _subsetMap){
		std::string min, max;
		SpiceManager::ref().getDateFromET(target.second._range._min, min);
		SpiceManager::ref().getDateFromET(target.second._range._max, max);
		
		myfile << std::endl;
		for (auto image : target.second._subset){
			std::string time;
			SpiceManager::ref().getDateFromET(image.startTime, time);
			myfile << std::fixed 
				   << std::setw(10) << time
				   << std::setw(10) << (int)getMetFromET(image.startTime) 
				   << std::setw(10) << image.target << std::setw(10);
			for (auto instrument : image.activeInstruments){
				myfile << " " << instrument;
			}
			myfile << std::endl;
		}
	}

	myfile.close();
	
	return true;
}
*/
}  // namespace openspace
