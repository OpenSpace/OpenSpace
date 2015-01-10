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

#include <openspace/util/spicemanager.h>
#include <fstream>
#include <iterator>

namespace {
const std::string _loggerCat = "ImageSequencer";
}

namespace openspace {

struct ImageParams{
	double startTime;
	double stopTime;
	std::string path;
};
std::vector<ImageParams> _timeStamps;


ImageSequencer::ImageSequencer(const std::string root){
   
}

ImageSequencer::~ImageSequencer(){

}

bool ImageSequencer::initialize(){
    return true;
}

void ImageSequencer::createImage(double t1, double t2, std::string path){
	// insert
	ImageParams image;
	image.startTime = t1;
	image.stopTime  = t2;
	image.path      = path;
	
	_timeStamps.push_back(image);
	// sort
	auto cmp = [](const ImageParams &a, const ImageParams &b)->bool{
		return a.startTime < b.startTime;
	};
	
	std::sort(_timeStamps.begin(), _timeStamps.end(), cmp);
}

bool ImageSequencer::getImagePath(std::string _currentTime, std::string& path){
	double currentEt = 0;
	openspace::SpiceManager::ref().getETfromDate(_currentTime, currentEt);
	bool success = getImagePath(currentEt, path);
	return success;
}

bool ImageSequencer::getImagePath(double _currentTime, std::string& path){
	auto cmp = [](const ImageParams &a, const ImageParams &b)->bool{
		return a.startTime < b.startTime;
	};

	auto binary_find = [](std::vector<ImageParams>::iterator begin,
						std::vector<ImageParams>::iterator end,
						const ImageParams &val,
						bool(*cmp)(const ImageParams &a, const ImageParams &b))->std::vector<ImageParams>::iterator{
		// Finds the lower bound in at most log(last - first) + 1 comparisons
		std::vector<ImageParams>::iterator it = std::lower_bound(begin, end, val, cmp);
		if (it != begin){
			return std::prev(it);
		}
		return end;
	};

	auto it = binary_find(_timeStamps.begin(), _timeStamps.end(), { _currentTime, 0, "" }, cmp);
	//do this if check against [start, stop] intervals
	/*
	if (it == _timeStamps.end() || it->stopTime < _currentTime){
		return false;
	}
	*/
	//do this if check against [start,) intervals
	if (it == _timeStamps.end()){
		return false;
	}

	path = it->path;
	return true;
}

bool ImageSequencer::loadSequence(const std::string dir){	
	ghoul::filesystem::Directory sequenceDir(dir, true);
	std::vector<std::string> sequencePaths = sequenceDir.read(true, false); // check inputs 
	int count = 0;
	std::cout.precision(15);

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
					
					std::string specsOfInterest[2] = { "START_TIME", "STOP_TIME" };
					double timestamps[2] = { 0.0, 0.0 };
					bool found = false;
					do {
						std::getline(file, line);
						for (int i = 0; i < 2; i++){
							auto pos = line.find(specsOfInterest[i]);
							if (pos != std::string::npos){
								std::string time = line.substr(line.find("=") + 2);
								time.erase(std::remove(time.begin(), time.end(), ' '), time.end());
								if (i == 0) std::cout << "Creating image with startTime: " << time;
								if (i == 1) std::cout << " ---> " << time << std::endl;
								openspace::SpiceManager::ref().getETfromDate(time, timestamps[i]);
							}
						}
						if (timestamps[0] != 0.0 && timestamps[1] != 0.0){
							found = true;
							std::string ext = "jpg";
							path.replace(path.begin() + position, path.end(), ext);
							std::vector<std::string>::const_iterator it = std::find(sequencePaths.begin(), sequencePaths.end(), path);
							if ( it != sequencePaths.end()){
							//	std::cout << "Creating image with time: " << timestamps[0] << "\ne_t: " << timestamps[1] << "\npath: " << path << std::endl;
								createImage(timestamps[0], timestamps[1], path);
							}
						}
					} while (!file.eof() && found == false);
				}
			}
		}
	}
	// testing _timeStamps
	/*double currentEt = 0;
	std::string currentTime = "2007-02-26T17:43:26.362";

	openspace::SpiceManager::ref().getETfromDate(currentTime, currentEt);

	std::cout << "\n Searching for  s_t: " << currentEt << std::endl;

	std::string storedpath = "";
	currentEt -= 1;

	for (int i = 0; i < 10000; i++){
		bool success = getImagePath(currentEt, storedpath);
		currentEt += ((float)i)/10000.f;

		if (success) std::cout << "FOUND AT : "<< currentEt << " PATH "<<  storedpath << std::endl;
	}*/
	return true;
}


}  // namespace openspace
