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

#include <openspace/util/ImageSequencer2.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <fstream>
#include <iterator>
#include <iomanip>
#include <limits>

#include <openspace/util/hongkangparser.h>

namespace {
	const std::string _loggerCat = "HongKangParser";
}

namespace openspace {
HongKangParser::HongKangParser(const std::string& fileName,
							   std::string spacecraft,
							   std::map<std::string, Payload*> fileTranslation,
							   std::vector<std::string> potentialTargets) :
							   _defaultCaptureImage(absPath("C:/Users/michal/openspace/openspace-data/scene/common/textures/placeholder_blank.png"))
{
	_fileName          = fileName;
	_spacecraft        = spacecraft;
	_fileTranslation   = fileTranslation;
	_potentialTargets  = potentialTargets;
}

void HongKangParser::create(){
	if (size_t position = _fileName.find_last_of(".") + 1){
		if (position != std::string::npos){
			std::string extension = ghoul::filesystem::File(_fileName).fileExtension();

			if (extension == "txt"){// Hong Kang. pre-parsed playbook
				LINFO("Using Preparsed Playbook V9H");
				std::ifstream file(_fileName);
				if (!file.good()) LERROR("Failed to open txt file '" << _fileName << "'");

				std::string timestr = "";
				double shutter = 0.01;
				double startTime, stopTime;

				// for augmentation we only need the keys in acronymDictionary as a vector
				std::vector<std::string> payload;
				for (auto p : _acronymDictionary)
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
							/*
							std::string checkIfMVIC = "MVIC";
							int count = 0;
							bool foundStopTime = false;
							if (findExactMatch("MVIC", keyword)){
							std::string abortLine;
							int len = file.tellg();
							while (!file.eof() && !findExactMatch("ABORT", abortLine)){

							std::getline(file, abortLine);
							std::string abortCommand = abortLine.substr(0, abortLine.find_first_of(" "));
							count++;
							if (findExactMatch("ABORT", abortCommand)){
							met = abortLine.substr(24, 9);
							stopTime = getETfromMet(met);
							foundStopTime = true;
							}
							}
							file.seekg(len, std::ios_base::beg);
							}else{
							//assume regular shutter speed;
							stopTime = startTime + shutter;
							}

							if (foundStopTime){
							longExposureStart = startTime;
							longExposureStop = stopTime;
							longExposureKeyword = keyword;
							}*/

							// retrieve corresponding SPICE call
							std::vector<std::string> instrument = it.second;

							// create image
							Image image;
							//stoptime- quick setting for now
							stopTime = startTime + 0.01;
							createImage(image, startTime, stopTime, instrument, "", _defaultCaptureImage);
							// add targets 
							augmentWithSpice(image, _spacecraft, payload, _potentialTargets);

							//SKA ERSATTAS MED NAGOT ANNAT - "instrument times"
							if (previousInstrument != it.first){
								previousInstrument = it.first;
								std::pair<double, std::string> v_time = std::make_pair(image.startTime, keyword);
								_instrumentTimes.push_back(v_time);
							}

							if (previousTarget != image.target){
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
	//PRINT TO FILE FUNCTION
	std::ofstream myfile;
	myfile.open("HongKangOutput.txt");

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
}

bool HongKangParser::augmentWithSpice(Image& image,
	                                  std::string spacecraft, 
									  std::vector<std::string> payload, 
									  std::vector<std::string> potentialTargets){
	image.target = "VOID";
	// we have (?) to cast to int, unfortunately
	int exposureTime = image.stopTime - image.startTime;
	if (exposureTime == 0) exposureTime = 1;
	double et;
	for (int i = 0; i < potentialTargets.size(); i++){
		bool success = false;
		bool _withinFOV = false;
		for (int j = 0; j < image.activeInstruments.size(); j++){
			double time = image.startTime;
			for (int k = 0; k < exposureTime; k++){
				time += k;
				success = openspace::SpiceManager::ref().targetWithinFieldOfView(
					image.activeInstruments[j],
					potentialTargets[i],
					spacecraft,
					"ELLIPSOID",
					"NONE",
					time,
					_withinFOV);
				if (_withinFOV){
					image.target = potentialTargets[i];
					_withinFOV = false;
				}
			}
		}
	}
	return false;
}

void HongKangParser::createImage(Image& image, double startTime, double stopTime, std::vector<std::string> instr, std::string targ, std::string pot) {
	image.startTime = startTime;
	image.stopTime = stopTime;
	image.path = pot;
	for (int i = 0; i < instr.size(); i++){
		image.activeInstruments.push_back(instr[i]);
	}
	image.target = targ;
	image.projected = false;
}

double HongKangParser::getETfromMet(std::string timestr){
	std::string::size_type sz;
	return getETfromMet(std::stod(timestr, &sz));
}

double HongKangParser::getETfromMet(double met){
	double diff;
	double referenceET;
	double et;
	openspace::SpiceManager::ref().getETfromDate("2015-07-14T11:50:00.00", referenceET);
	double missionLaunch = referenceET - _metRef;

	diff = abs(met - _metRef);
	if (met > _metRef){
		et = referenceET + diff;
	}else if (met < _metRef){
		et = referenceET - diff;
	}
	return et;
}


double HongKangParser::getMetFromET(double et){
	double met;
	double referenceET;
	openspace::SpiceManager::ref().getETfromDate("2015-07-14T11:50:00.00", referenceET);

	if (et >= referenceET){
		met = _metRef + (et - referenceET);
	}
	else{
		met = _metRef - (referenceET - et);
	}

	return met;
}

std::map<std::string, ImageSubset> HongKangParser::getSubsetMap(){
	return _subsetMap;
}
std::vector<std::pair<double, std::string>> HongKangParser::getIstrumentTimes(){
	return _instrumentTimes;
}
std::vector<std::pair<double, std::string>> HongKangParser::getTargetTimes(){
	return _targetTimes;
}
}