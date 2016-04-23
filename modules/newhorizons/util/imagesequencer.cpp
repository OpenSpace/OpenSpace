/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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
#include <modules/newhorizons/util/imagesequencer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/cachemanager.h>
#include <modules/newhorizons/util/decoder.h>

#include <openspace/util/spicemanager.h>
#include <fstream>
#include <iterator>
#include <iomanip>
#include <limits>

namespace {
const std::string _loggerCat = "ImageSequencer";
}

namespace openspace {

ImageSequencer* ImageSequencer::_instance = nullptr;

ImageSequencer::ImageSequencer()
    : _hasData(false)
{}

ImageSequencer& ImageSequencer::ref() {
    assert(_instance != nullptr);
    return *_instance;
}
void ImageSequencer::initialize() {
    assert(_instance == nullptr);
    _instance = new ImageSequencer;
    _instance->_defaultCaptureImage = absPath("${OPENSPACE_DATA}/scene/common/textures/placeholder_blank.png");
}

void ImageSequencer::deinitialize() {
    delete _instance;
    _instance = nullptr;
}

bool ImageSequencer::isReady(){
    return _hasData;
}

void ImageSequencer::updateSequencer(double time){
    if (Time::ref().timeJumped() && Time::ref().deltaTime() == 0){
        Time::ref().setDeltaTime(0.1);
    } // Time is not properly updated when time jump with dt = 0 

    if (_currentTime != time){
        _previousTime = _currentTime;
        _currentTime = time;
    }
}

std::pair<double, std::string> ImageSequencer::getNextTarget(){
    auto compareTime = [](const std::pair<double, std::string> &a,
                          const std::pair<double, std::string> &b)->bool{
        return a.first < b.first;
    };
    std::pair<double, std::string> findEqualToThis;
    findEqualToThis.first = _currentTime;
    auto it = std::lower_bound(_targetTimes.begin(), _targetTimes.end(), findEqualToThis, compareTime);

    if (it != _targetTimes.end() && it != _targetTimes.begin())
        return (*it);
    else
        return std::make_pair(0.0, "");
}

std::pair<double, std::string> ImageSequencer::getCurrentTarget(){
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

std::pair<double, std::vector<std::string>> ImageSequencer::getIncidentTargetList(int range){
    std::pair<double, std::vector<std::string>> incidentTargets;

    auto compareTime = [](const std::pair<double, std::string> &a,
                         const std::pair<double, std::string> &b)->bool{
         return a.first < b.first;
    };
    // what to look for
    std::pair<double, std::string> findEqualToThis;
    findEqualToThis.first = _currentTime;
    auto it = std::lower_bound(_targetTimes.begin(), _targetTimes.end(), findEqualToThis, compareTime);
    
    if (it != _targetTimes.end() && it != _targetTimes.begin()){
        // move the iterator to the first element of the range
        std::advance(it, -(range + 1));

        // now extract incident range 
        for (int i = 0; i < 2 * range + 1; i++){
            incidentTargets.first = it->first;
            incidentTargets.second.push_back(it->second);
            it++;
            if (it == _targetTimes.end())
                break;
        }
    }

    return incidentTargets;
}

double ImageSequencer::getIntervalLength(){
    double upcoming = getNextCaptureTime();
    if (_nextCapture != upcoming){
        _nextCapture = upcoming;
        _intervalLength = upcoming - _currentTime;
    }
    return _intervalLength;
}

double ImageSequencer::getNextCaptureTime(){
    auto compareTime = [](const double &a, const double &b)->bool{
        return a < b;
    };
    double nextCaptureTime = 0;
    auto it = std::lower_bound(_captureProgression.begin(), _captureProgression.end(), _currentTime, compareTime);
    if (it != _captureProgression.end())
        nextCaptureTime = *it;

    return nextCaptureTime;
}
const Image ImageSequencer::getLatestImageForInstrument(const std::string _instrumentID){
    auto it = _latestImages.find(_instrumentID);
    if (it != _latestImages.end())
        return _latestImages[_instrumentID];
    else {
        Image dummyImage = { 0, 0, "", std::vector<std::string>(), "", false };
        return dummyImage;
    }
}

std::map<std::string, bool> ImageSequencer::getActiveInstruments(){
    // first set all instruments to off
    for (auto i : _switchingMap)
        _switchingMap[i.first] = false;
    // go over the filetranslation map
    for (const auto &key : _fileTranslation){
        // for each spice-instrument
        for (const auto &instrumentID : key.second->getTranslation()){
            // check if the spice-instrument is active 
                if (instrumentActive(instrumentID)){
                    // go over switching map
                    for (const auto &instrument : _switchingMap){
                        // if instrument is present in switching map
                        if (instrumentID == instrument.first){
                            // set as active
                            _switchingMap[instrumentID] = true;
                        }
                    }
                }
            }
        }
    // return entire map, seen in GUI.
    return _switchingMap;
}
bool ImageSequencer::instrumentActive(std::string instrumentID){ 
    for (auto i : _instrumentTimes){
        //check if this instrument is in range
        if (i.second.inRange(_currentTime)){ 
            //if so, then get the corresponding spiceID
            std::vector<std::string> spiceIDs = _fileTranslation[i.first]->getTranslation(); 
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

float ImageSequencer::instrumentActiveTime(const std::string& instrumentID) const {
    for (auto i : _instrumentTimes){
        //check if this instrument is in range
        if (i.second.inRange(_currentTime)){
            //if so, then get the corresponding spiceID
            std::vector<std::string> spiceIDs = _fileTranslation.at(i.first)->getTranslation();
            //check which specific subinstrument is firing
            for (auto s : spiceIDs){
                if (s == instrumentID) {
                    return static_cast<float>((_currentTime - i.second._min) / (i.second._max - i.second._min));
                }
            }
        }
    }
    return -1.f;
}

bool ImageSequencer::getImagePaths(std::vector<Image>& captures, 
                                    std::string projectee,
                                    std::string instrumentRequest){

    // check if this instance is either in range or 
    // a valid candidate to recieve data 
    if (!instrumentActive(instrumentRequest) && !Time::ref().timeJumped()) return false;


    //if (!Time::ref().timeJumped() && projectee == getCurrentTarget().second)
    if (_subsetMap[projectee]._range.inRange(_currentTime) ||
        _subsetMap[projectee]._range.inRange(_previousTime)){
        auto compareTime = [](const Image &a,
                              const Image &b)->bool{
            return a.startTime < b.startTime;
        };        
        // for readability we store the iterators
        auto begin = _subsetMap[projectee]._subset.begin(); 
        auto end   = _subsetMap[projectee]._subset.end();
        
        // create temporary storage
        std::vector<Image> captureTimes;
        // what to look for 
        Image findPrevious, findCurrent;
        findPrevious.startTime = _previousTime;
        findCurrent.startTime  = _currentTime;

        // find the two iterators that correspond to the latest time jump
        auto curr = std::lower_bound(begin, end, findCurrent , compareTime);
        auto prev = std::lower_bound(begin, end, findPrevious, compareTime);
    
        if (curr != begin && curr != end  && prev != begin && prev != end && prev < curr){
            if (curr->startTime >= prev->startTime){
                std::copy_if(prev, curr, back_inserter(captureTimes), 
                    [instrumentRequest](const Image& i) {
                    return i.activeInstruments[0] == instrumentRequest;
                });

                //std::reverse(captureTimes.begin(), captureTimes.end());
                captures = captureTimes;
                if (!captures.empty())
                    _latestImages[captures.back().activeInstruments.front()] = captures.back();

                return true;
            }
        }
    }
    return false;
}
void ImageSequencer::sortData(){
    auto targetComparer = [](const std::pair<double, std::string> &a,
                             const std::pair<double, std::string> &b)->bool{
        return a.first < b.first;
    };
    auto imageComparer = [](const Image &a, const Image &b)->bool{
        return a.startTime < b.startTime;
    };

    std::sort(_targetTimes.begin(), _targetTimes.end(), targetComparer);
    std::stable_sort(_captureProgression.begin(), _captureProgression.end());

    for (auto sub : _subsetMap){
        std::sort(_subsetMap[sub.first]._subset.begin(),
                  _subsetMap[sub.first]._subset.end(), imageComparer);
    }
}

void ImageSequencer::runSequenceParser(SequenceParser* parser){
    bool parserComplete = parser->create();
    if (parserComplete){
        // get new data 
        std::map<std::string, Decoder*> translations = parser->getTranslation(); // in1
        std::map<std::string, ImageSubset> imageData = parser->getSubsetMap();   // in2
        std::vector<std::pair<std::string, TimeRange>> instrumentTimes = parser->getIstrumentTimes(); //in3
        std::vector<std::pair<double, std::string>> targetTimes = parser->getTargetTimes();  //in4
        std::vector<double> captureProgression = parser->getCaptureProgression();  //in5

        // check for sanity
        if (translations.empty() || imageData.empty() || instrumentTimes.empty() || targetTimes.empty() || captureProgression.empty())
            return;

        // append data
        _fileTranslation.insert(translations.begin(), translations.end());
        for (auto it : imageData){
            if (_subsetMap.find(it.first) == _subsetMap.end()) {
                // if key not exist yet - add sequence data for key (target)
                _subsetMap.insert(it);
            } else {
                std::string key = it.first;
                std::vector<Image> &source      = it.second._subset; // prediction 
                std::vector<Image> &destination = _subsetMap[key]._subset; // imagery

                // simple search function
                double min = 10;                
                auto findMin = [&](std::vector<Image> &vector)->double{
                    for (int i = 1; i < vector.size(); i++){
                        double e = abs(vector[i].startTime - vector[i - 1].startTime);
                        if (e < min){
                            min = e;
                        }
                    }    
                    return min;
                };
                
                // find the smallest separation of images in time
                double epsilon;
                epsilon = findMin(source);
                epsilon = findMin(destination);
                // set epsilon as 1% smaller than min
                epsilon -= min*0.01;
                
                // IFF images have same time as mission planned capture, erase that event from 
                // 'predicted event file' (mission-playbook)
                std::vector<Image> tmp;
                for (int i = 0; i < source.size(); i++){
                    for (int j = 0; j < destination.size(); j++){
                        double diff = abs(source[i].startTime - destination[j].startTime);
                        if (diff < epsilon){
                            source.erase(source.begin() + i);
                        }
                    }
                }
                // pad image data with predictions (ie - where no actual images, add placeholder) 
                _subsetMap[key]._subset.insert(_subsetMap[key]._subset.end(), source.begin(), source.end());
            }
        }

        _instrumentTimes.insert(_instrumentTimes.end(), instrumentTimes.begin(), instrumentTimes.end());
        _targetTimes.insert(_targetTimes.end(), targetTimes.begin(), targetTimes.end());
        _captureProgression.insert(_captureProgression.end(), captureProgression.begin(), captureProgression.end());

        // sorting of data _not_ optional
        sortData();

        // extract payload from _fileTranslation 
        for (auto t : _fileTranslation){
            if (t.second->getDecoderType() == "CAMERA" ||
                t.second->getDecoderType() == "SCANNER"){
                std::vector<std::string> spiceIDs = t.second->getTranslation();
                for (auto id : spiceIDs){
                    _switchingMap[id] = false;
                }
            }
        }
        _hasData = true;
    }
    else
        LERROR("One or more sequence loads failed; please check mod files");
}

}  // namespace openspace
