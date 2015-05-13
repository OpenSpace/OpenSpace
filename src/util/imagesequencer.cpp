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
#include <openspace/util/imagesequencer.h>
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
const std::string _loggerCat = "ImageSequencer";
}

namespace openspace {

ImageSequencer* ImageSequencer::_sequencer = nullptr;

struct ImageParams{
	double startTime;
	std::string path;
	std::string activeInstrument;
	std::string target;
	bool projected;
};



std::vector<std::vector<ImageParams>> _timeStamps;
void createImage(std::vector<ImageParams>& vec, double t1, std::string instrument, std::string target, std::string path = "dummypath");

auto imageComparer = [](const ImageParams &a, const ImageParams &b)->bool{
	return a.startTime < b.startTime;
};


std::vector<ImageParams>::iterator binary_find(std::vector<ImageParams>::iterator begin,
	std::vector<ImageParams>::iterator end,
	const ImageParams &val,
	bool(*imageComparer)(const ImageParams &a, const ImageParams &b)){
	// Finds the lower bound in at most log(last - first) + 1 comparisons
	std::vector<ImageParams>::iterator it = std::lower_bound(begin, end, val, imageComparer);
	if (it != begin){
		return it;
	}
	return end;
}

ImageSequencer::ImageSequencer() 
    : _nextCapture(-1.0)
	, _currentTime(-1.0)
	, _sequenceIDs(0)
    , _defaultCaptureImage(absPath("${OPENSPACE_DATA}/scene/common/textures/placeholder_blank.png"))
    , _targetsAdded(false)
{}


ImageSequencer& ImageSequencer::ref() {
	assert(_sequencer != nullptr);
	return *_sequencer;
}
void ImageSequencer::initialize() {
	assert(_sequencer == nullptr);
	_sequencer = new ImageSequencer;
}

void ImageSequencer::deinitialize() {
	delete _sequencer;
	_sequencer = nullptr;
}

void ImageSequencer::setSequenceId(int& id){
	id = _sequenceIDs;
	_sequenceIDs++;
}

void ImageSequencer::addSequenceObserver(int sequenceID, std::string name, std::vector<std::string> payload){
	if (sequenceID >= 0){
		_observers.insert(std::make_pair(sequenceID, name));
		_instruments.insert(std::make_pair(name, payload));
	}
}

void ImageSequencer::registerTargets(std::vector<std::string>& potential){
	for (auto p : potential){
		if (_projectableTargets.find(p) == _projectableTargets.end()){
			_projectableTargets[p] = _currentTime;
		}
	}
}

void ImageSequencer::update(double time){
	_currentTime = time;
	static bool time_initialized;

	if (!time_initialized){
		for (auto &it : _projectableTargets) {
			it.second = _currentTime;
			assert(it.second > 0.0);
		}
		time_initialized = true;
	}
}

void createImage(std::vector<ImageParams>& vec, double t1, std::string instrument, std::string target, std::string path) {
	// insert
	ImageParams image;
	image.startTime = t1;
	image.path      = path;
	image.activeInstrument = instrument;
	image.target = target;
	image.projected = false;
	
	vec.push_back(image);
}

double ImageSequencer::getNextCaptureTime(){
	return _nextCapture;
}
double ImageSequencer::nextCaptureTime(double time, int sequenceID){
	if (time < _nextCapture) return _nextCapture;
	auto it = binary_find(_timeStamps[sequenceID].begin(), _timeStamps[sequenceID].end(), { time, "", "", "", false }, imageComparer);
	if (it == _timeStamps[sequenceID].end()) return _nextCapture;

	return it->startTime;
}

std::string ImageSequencer::findActiveInstrument(double time, int sequenceID){
	auto it = binary_find(_timeStamps[sequenceID].begin(), _timeStamps[sequenceID].end(), { time, "", "", "",false }, imageComparer);
	if ((it == _timeStamps[sequenceID].end())){
		_activeInstrument = "Not found, incufficient playbook-data";
	}else{
		_activeInstrument = std::prev(it)->activeInstrument;
	}
	
	return _activeInstrument;
}

void ImageSequencer::augumentSequenceWithTargets(int sequenceID){
	if (!_targetsAdded){
		// if there is an registered observer for this sequence 
		if (_observers.count(sequenceID) > 0) {
			// find observer
			std::string observer = _observers.at(sequenceID);
			// find its instruments
			std::map <std::string, std::vector<std::string>>::iterator it2 = _instruments.find(observer);
			if (it2 != _instruments.end()){
				std::string _targetFOV;
				bool _withinFOV;
				// for each image taken
				for (std::vector<ImageParams>::iterator image = _timeStamps[sequenceID].begin(); image != _timeStamps[sequenceID].end(); ++image) {
					// traverse potential targets...
					for (auto t : _projectableTargets){
						// ... and potential instruments
						for (auto i : it2->second){
							//register precisely which target is being projected to upon image-capture
							bool success = openspace::SpiceManager::ref().targetWithinFieldOfView(
								i, // Instrumnet 
								t.first, // projectables
								observer, // new horizons
								"ELLIPSOID",
								"NONE",
								image->startTime,
								_withinFOV);
							//if (!_withinFOV) image->target = "VOID";
							if (success && _withinFOV){
								image->target = t.first;
								//once we find it abort search, break the loop.
								break;
							}
						}
					}
				}
			}else{
				LERROR("Spacecraft payload not provided, cannot write playbook");
			}
		}
		else{
			LERROR("Did not find observing spacecraft for sequence, cannot write playbook");
		}
		_targetsAdded = true;
	}
}

bool ImageSequencer::getImagePath(std::vector<std::pair<double, std::string>>& _imageTimes, int sequenceID, std::string projectee, bool withinFOV){
		/*if (withinFOV && !Time::ref().timeJumped()){
			getSingleImage(_imageTimes, sequenceID, projectee);
		}else{*/
			getMultipleImages(_imageTimes, sequenceID, projectee);
		//}
	return true;
}

bool ImageSequencer::getMultipleImages(std::vector<std::pair<double, std::string>>& _imageTimes, int sequenceID, std::string projectee){
	double previousTime;
	std::map<std::string, double>::iterator it = _projectableTargets.find(projectee); 
	if (it != _projectableTargets.end()){
		previousTime = it->second;
		it->second = _currentTime;
	}
	auto it1 = binary_find(_timeStamps[sequenceID].begin(), _timeStamps[sequenceID].end(), { previousTime, "", "", "", false }, imageComparer);
	auto it2 = binary_find(_timeStamps[sequenceID].begin(), _timeStamps[sequenceID].end(), { _currentTime, "", "", "", false }, imageComparer);

	if (it1 != _timeStamps[sequenceID].end() && it2 != _timeStamps[sequenceID].end() && it1 != it2){
		std::transform(it1, it2, std::back_inserter(_imageTimes),
			[](const ImageParams& i) { 
				return std::make_pair(i.startTime, i.path);
		});
	}
	std::reverse(_imageTimes.begin(), _imageTimes.end());

	double upcoming = nextCaptureTime(_currentTime, sequenceID);
	if (_nextCapture != upcoming){
		_nextCapture = upcoming;
		_intervalLength = upcoming - _currentTime;
	}

	return true;
}

bool ImageSequencer::getSingleImage(std::vector<std::pair<double, std::string>>& _imageTimes, int sequenceID, std::string projectee){

	auto bfind = [](std::vector<ImageParams>::iterator begin,
		std::vector<ImageParams>::iterator end,
		const ImageParams &val,
		bool(*imageComparer)(const ImageParams &a, const ImageParams &b))->std::vector<ImageParams>::iterator{
		// Finds the lower bound in at most log(last - first) + 1 comparisons
		std::vector<ImageParams>::iterator it = std::lower_bound(begin, end, val, imageComparer);
		if (it != begin){
			return std::prev(it);
		}
		return end;
	};

	auto it = bfind(_timeStamps[sequenceID].begin(), _timeStamps[sequenceID].end(), { _currentTime, "", "", "", false }, imageComparer);

	if (it != _timeStamps[sequenceID].end() && !it->projected){
		it->projected = true;
		_imageTimes.push_back(std::make_pair(it->startTime, it->path));
	}

	double upcoming = nextCaptureTime(_currentTime, sequenceID);
	if (_nextCapture != upcoming){
		_nextCapture = upcoming;
		_intervalLength = upcoming - _currentTime;
	}

	return true;
}
/*
bool ImageSequencer::getImagePath(std::vector<std::pair<double, std::string>>& _imageTimes, int sequenceID, std::string projectee, bool closedInterval){
	double t = _currentTime;

	double ptime;
	std::map<std::string, double>::iterator it = _previous.find(projectee);
	if (it != _previous.end()){
		ptime = it->second;
		it->second = _currentTime;
	}

	while (t > ptime){
		auto binary_find = [](std::vector<ImageParams>::iterator begin,
			std::vector<ImageParams>::iterator end,
			const ImageParams &val,
			bool(*imageComparer)(const ImageParams &a, const ImageParams &b))->std::vector<ImageParams>::iterator{
			// Finds the lower bound in at most log(last - first) + 1 comparisons
			std::vector<ImageParams>::iterator it = std::lower_bound(begin, end, val, imageComparer);
			if (it != begin){
				return std::prev(it);
			}
			return end;
		};

		auto it = binary_find(_timeStamps[sequenceID].begin(), _timeStamps[sequenceID].end(), { t, 0, "", "", false }, imageComparer);

		if (it == _timeStamps[sequenceID].end() || it->startTime < ptime) break;

		if (!it->projected || it != _timeStamps[sequenceID].end()){
			_imageTimes.push_back(std::make_pair(it->startTime, it->path));
		}
		t = it->startTime - 1;
		//it->projected = true;
	}
	std::reverse(_imageTimes.begin(), _imageTimes.end());

	double upcoming = nextCaptureTime(_currentTime, sequenceID);
	if (_nextCapture != upcoming){
		_nextCapture = upcoming;
		_intervalLength = upcoming - _currentTime;
	}

	return true;
}*/
/*
bool ImageSequencer::getImagePath(double& currentTime, std::string& path,  bool closedInterval){
	auto binary_find = [](std::vector<ImageParams>::iterator begin,
						std::vector<ImageParams>::iterator end,
						const ImageParams &val,
						bool(*imageComparer)(const ImageParams &a, const ImageParams &b))->std::vector<ImageParams>::iterator{
		// Finds the lower bound in at most log(last - first) + 1 comparisons
	std::vector<ImageParams>::iterator it = std::lower_bound(begin, end, val, imageComparer);
		if (it != begin){
			return std::prev(it);
		}
		return end;
	};

	auto it = binary_find(_timeStamps.begin(), _timeStamps.end(), { currentTime, 0, "", "", false }, imageComparer);
	//check [start, stop] 
	if (closedInterval && (it == _timeStamps.end() || it->stopTime < currentTime || it->projected)){
		return false;
	}else if (!closedInterval && (it == _timeStamps.end() || it->projected)){
		return false;
	}
	
	double upcoming = nextCaptureTime(currentTime);
	if (_nextCapture != upcoming){
		_nextCapture = upcoming;
		_intervalLength = upcoming - currentTime;
	}
	
	it->projected = true;
	path = it->path;
	currentTime = it->startTime;
	
	return true;
}

bool ImageSequencer::sequenceReset(){
	for (auto image : _timeStamps){
		image.projected = false;
	}
	return true;
}
*/

// ----------------- LOAD-DATA RELATED STUFF --------------------------------------

bool replace(std::string& str, const std::string& from, const std::string& to) {
	size_t start_pos = str.find(from);
	if (start_pos == std::string::npos)
		return false;
	str.replace(start_pos, from.length(), to);
	return true;
}

double ImageSequencer::getMissionElapsedTime(std::string timestr){
	std::string::size_type sz;     // alias of size_t
	double met = std::stod(timestr, &sz);
	double diff;
	double et;
	//met ref time.
	openspace::SpiceManager::ref().getETfromDate("2015-07-14T11:50:00.00", et);

    diff = std::abs(met - _metRef);
	if (met > _metRef){
		et += diff;
	}
	else if (met < _metRef){
		et -= diff;
	}
	return et;
}

bool ImageSequencer::parsePlaybookFile(const std::string& fileName, int& sequenceID, std::string year) {
	setSequenceId(sequenceID);
	std::vector<ImageParams> tmp;

	if (size_t position = fileName.find_last_of(".") + 1){
		if (position != std::string::npos){
			std::string extension = ghoul::filesystem::File(fileName).fileExtension();
#ifdef BACKUP_PLAYBOOK // Comma separated playbook in case we dont recieve updates from kang.
			if (extension == "csv"){ // comma separated playbook 
				std::cout << "USING COMMA SEPARATED TIMELINE V9F" << std::endl;

                std::string cachedFile = "";
                FileSys.cacheManager()->getCachedFile(fileName, cachedFile, true);

                bool hasCachedFile = FileSys.fileExists(cachedFile);
                if (hasCachedFile) {
                    std::ifstream file(cachedFile);
                    if (!file.good())
                        LERROR("Error loading cached playbook '" << cachedFile << "'");
                    else {
                        do {
                            std::string line;
                            std::getline(file, line);

                            std::stringstream s(line);

                            double start, end;
                            std::string path;

                            s >> start;
                            s >> end;

                            std::getline(s, path);
                            createImage(start, end, _defaultCaptureImage);
                        } while (!file.eof());
                    }                
                } else {
                    std::ifstream file(fileName);
                    if (!file.good()) LERROR("Failed to open csv file '" << fileName << "'");

                    std::string timestr = "";
                    double shutter = 0.01;
                    double et;
                    do{
                        std::getline(file, timestr);
                        auto pos = timestr.find("LORRI image started");
                        if (pos != std::string::npos){
                            timestr = timestr.substr(timestr.find_first_of(",") + 1);
                            timestr = timestr.substr(0, timestr.find_first_of(","));

                            replace(timestr, " ", "::");
                            timestr = year + " " + timestr;

                            openspace::SpiceManager::ref().getETfromDate(timestr, et);
                            createImage(et, et + shutter, _defaultCaptureImage);
                        }
                    } while (!file.eof());

                    std::sort(_timeStamps.begin(), _timeStamps.end(), imageComparer);

                    std::ofstream cachedFileStream(cachedFile);
                    cachedFileStream << std::setprecision(64);
                    if (cachedFileStream.good()) {
                        for (const ImageParams& i : _timeStamps)
                            cachedFileStream << i.startTime << "\t" << i.stopTime << "\t" << i.path << std::endl;
                    }

                }
			} 
#endif
			if (extension == "txt"){// Hong Kang. pre-parsed playbook
                LINFO("Using Preparsed Playbook V9H");
				std::ifstream file(fileName);
				if (!file.good()) LERROR("Failed to open txt file '" << fileName << "'");

				std::string timestr = "";
				double et;
				
				//@TODO: change similar to renderableFOV
				std::string instruments[3] = { "LORRI", "RALPH_LEISA", "MVIC" };
				std::string id[3] = { "NH_LORRI", "NH_RALPH_LEISA", "MVIC" };
				int isize = sizeof(instruments) / sizeof(instruments[0]);
				
				do{
					std::getline(file, timestr);
					for (int i = 0; i < isize; i++){
						auto pos = timestr.find(instruments[i]);
						if (pos != std::string::npos){
							timestr = timestr.substr(24, 9);
							et = getMissionElapsedTime(timestr);
							createImage(tmp, et, id[i], "", _defaultCaptureImage);
							std::sort(tmp.begin(), tmp.end(), imageComparer);
						}
					}
				} while (!file.eof());
			}
				
		}
	}
	_timeStamps.push_back(tmp);
    return true;
}

bool ImageSequencer::loadSequence(const std::string& dir, int& sequenceID) {
	setSequenceId(sequenceID);
	std::vector<ImageParams> tmp;

	ghoul::filesystem::Directory sequenceDir(dir, true);
	std::vector<std::string> sequencePaths = sequenceDir.read(true, false); // check inputs 
	for (auto path : sequencePaths){
		if (size_t position = path.find_last_of(".") + 1){
			if (position != std::string::npos){
				ghoul::filesystem::File currentFile(path);
				std::string extension = currentFile.fileExtension();

				if (extension == "lbl"){ // discovered header file 		
					std::ifstream file(currentFile.path());

					if (!file.good()) LERROR("Failed to open label file '" << currentFile.path() << "'");
				
					// open up label files
					std::string line = "";
					std::string specsOfInterest = "START_TIME"; // can be extended 
					double timestamp= 0.0;
					bool found = false;
					do {
						std::getline(file, line);
							auto pos = line.find(specsOfInterest);
							if (pos != std::string::npos){
								std::string time = line.substr(line.find("=") + 2);
								time.erase(std::remove(time.begin(), time.end(), ' '), time.end());
								openspace::SpiceManager::ref().getETfromDate(time, timestamp);
							}
						if (timestamp != 0.0){
							found = true;
							std::string ext = "jpg";
							path.replace(path.begin() + position, path.end(), ext);
                            bool fileExists = FileSys.fileExists(path);
							if (fileExists) {
								createImage(tmp, timestamp, "NH_LORRI", "", path); /// fix active instrument!
								std::sort(tmp.begin(), tmp.end(), imageComparer);
							}
						}
					} while (!file.eof() && found == false);
				}
			}
		}
	}
	_timeStamps.push_back(tmp);
	return !_timeStamps.empty();
}

}  // namespace openspace
