/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/spacecraftinstruments/util/imagesequencer.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "ImageSequencer";
} // namespace

namespace openspace {

ImageSequencer* ImageSequencer::_instance = nullptr;

ImageSequencer& ImageSequencer::ref() {
    ghoul_assert(_instance != nullptr, "Instance has not been initialized");
    return *_instance;
}
void ImageSequencer::initialize() {
    ghoul_assert(_instance == nullptr, "Instance already has been initialized");
    _instance = new ImageSequencer;
    _instance->_defaultCaptureImage = absPath("${DATA}/placeholder.png");
}

void ImageSequencer::deinitialize() {
    delete _instance;
    _instance = nullptr;
}

bool ImageSequencer::isReady() {
    return _hasData;
}

std::pair<double, std::string> ImageSequencer::nextTarget(double time) {
    const auto it = std::lower_bound(
        _targetTimes.begin(),
        _targetTimes.end(),
        time,
        [](const std::pair<double, std::string>& a, double b) { return a.first < b; }
    );

    if (it != _targetTimes.end() && it != _targetTimes.begin()) {
        return *it;
    }
    else {
        return { 0.0, "No Target" };
    }
}

std::pair<double, std::string> ImageSequencer::currentTarget(double time) {
    const auto it = std::lower_bound(
        _targetTimes.begin(),
        _targetTimes.end(),
        time,
        [](const std::pair<double, std::string>& a, double b) { return a.first < b; }
    );

    if (it != _targetTimes.end() && it != _targetTimes.begin()){
        return *std::prev(it);
    }
    else {
        return { 0.0, "No Target" };
    }
}

std::pair<double, std::vector<std::string>> ImageSequencer::incidentTargetList(
                                                                              double time,
                                                                              int range)
{
    std::pair<double, std::vector<std::string>> incidentTargets;

    auto it = std::lower_bound(
        _targetTimes.begin(),
        _targetTimes.end(),
        time,
        [](const std::pair<double, std::string>& a, double b) { return a.first < b; }
    );

    if (it != _targetTimes.end() && it != _targetTimes.begin()){
        // move the iterator to the first element of the range
        std::advance(it, -(range + 1));

        // now extract incident range
        for (int i = 0; i < 2 * range + 1; i++){
            incidentTargets.first = it->first;
            incidentTargets.second.push_back(it->second);
            it++;
            if (it == _targetTimes.end()) {
                break;
            }
        }
    }

    return incidentTargets;
}

double ImageSequencer::intervalLength(double time) {
    const double upcoming = nextCaptureTime(time);
    if (_nextCapture != upcoming) {
        _nextCapture = upcoming;
        _intervalLength = upcoming - time;
    }
    return _intervalLength;
}

double ImageSequencer::nextCaptureTime(double currentTime) {
    const auto it = std::lower_bound(
        _captureProgression.begin(),
        _captureProgression.end(),
        currentTime
    );
    if (it != _captureProgression.end()) {
        return *it;
    }
    else {
        return 0.0;
    }
}

Image ImageSequencer::latestImageForInstrument(const std::string& instrumentID) {
    const auto it = _latestImages.find(instrumentID);
    if (it != _latestImages.end()) {
        return _latestImages[instrumentID];
    }
    else {
        return Image();
    }
}

std::vector<std::pair<std::string, bool>> ImageSequencer::activeInstruments(double time) {
    // first set all instruments to off
    for (std::pair<std::string, bool>& i : _switchingMap) {
        i.second = false;
    }

    // go over the filetranslaton map
    using K = std::string;
    using V = std::unique_ptr<Decoder>;
    for (const std::pair<const K, V>& key : _fileTranslation) {
        // for each spice-instrument
        for (const std::string& instrumentID : key.second->translations()) {
            // check if the spice-instrument is active
            if (isInstrumentActive(time, instrumentID)) {
                // go over switching map
                for (std::pair<std::string, bool>& instrument : _switchingMap) {
                    // if instrument is present in switching map
                    if (instrumentID == instrument.first) {
                        // set as active
                        instrument.second = true;
                    }
                }
            }
        }
    }
    // return entire map, seen in GUI.
    return _switchingMap;
}

bool ImageSequencer::isInstrumentActive(double time, const std::string& instrumentID) {
    for (const std::pair<std::string, TimeRange>& i : _instrumentTimes) {
        //check if this instrument is in range
        if (i.second.includes(time)) {
            //if so, then get the corresponding spiceID
            const std::vector<std::string>& ids =
                _fileTranslation[i.first]->translations();
            //check which specific subinstrument is firing
            for (const std::string& s : ids) {
                if (s == instrumentID) {
                    return true;
                }
            }
        }
    }
    return false;
}

float ImageSequencer::instrumentActiveTime(double time, const std::string& instrumentID) const {
    for (const std::pair<std::string, TimeRange>& i : _instrumentTimes) {
        //check if this instrument is in range
        if (i.second.includes(time)){
            //if so, then get the corresponding spiceID
            const std::vector<std::string>& ids =
                _fileTranslation.at(i.first)->translations();

            // check which specific subinstrument is firing
            for (const std::string& s : ids) {
                if (s == instrumentID) {
                    return static_cast<float>(
                        (time - i.second.start) / (i.second.end - i.second.start)
                    );
                }
            }
        }
    }
    return -1.f;
}

bool ImageSequencer::imagePaths(std::vector<Image>& captures,
                                const std::string& projectee,
                                const std::string& instrumentRequest,
                                double time, double sinceTime)
{
    // TODO: Check how this works with time jumps

    // check if this instance is either in range or
    // a valid candidate to recieve data

    if (!isInstrumentActive(time, instrumentRequest)) {
        return false;
    }

    if (!_subsetMap[projectee]._range.includes(time) &&
        !_subsetMap[projectee]._range.includes(sinceTime))
    {
        return false;
    }


    // for readability we store the iterators
    auto begin = _subsetMap[projectee]._subset.begin();
    auto end   = _subsetMap[projectee]._subset.end();

    // create temporary storage
    std::vector<Image> captureTimes;
    // what to look for
    Image findPrevious, findCurrent;
    findPrevious.timeRange.start = sinceTime;
    findCurrent.timeRange.start = time;

    // find the two iterators that correspond to the latest time jump
    auto compareTime = [](const Image& a, const Image& b) -> bool {
        return a.timeRange.start < b.timeRange.start;
    };
    auto curr = std::lower_bound(begin, end, findCurrent , compareTime);
    auto prev = std::lower_bound(begin, end, findPrevious, compareTime);

    if (curr == begin || curr == end || prev == begin || prev == end || prev >= curr) {
        return false;
    }

    if (curr->timeRange.start < prev->timeRange.start) {
        return false;
    }

    std::copy_if(
        prev,
        curr,
        back_inserter(captureTimes),
        [instrumentRequest](const Image& i) {
            return i.activeInstruments[0] == instrumentRequest;
        }
    );

    captures = captureTimes;
    if (!captures.empty()) {
        _latestImages[captures.back().activeInstruments.front()] = captures.back();
    }

    std::vector<int> toDelete;
    for (auto it = captures.begin(); it != captures.end(); ++it) {
        if (it->isPlaceholder) {
            double beforeDist = std::numeric_limits<double>::max();
            if (it != captures.begin()) {
                beforeDist = std::abs(
                    std::prev(it)->timeRange.start - it->timeRange.start
                );
            }

            double nextDist = std::numeric_limits<double>::max();
            if (it != captures.end() - 1) {
                nextDist = std::abs(
                    std::next(it)->timeRange.start - it->timeRange.start
                );
            }

            if (beforeDist < 1.0 || nextDist < 1.0) {
                toDelete.push_back(static_cast<int>(std::distance(captures.begin(), it)));
            }
        }
    }

    for (size_t i = 0; i < toDelete.size(); ++i) {
        // We have to subtract i here as we already have deleted i value
        // before this and we need to adjust the location
        int v = toDelete[i] - static_cast<int>(i);
        captures.erase(captures.begin() + v);
    }

    return true;
}

void ImageSequencer::sortData() {
    std::sort(
        _targetTimes.begin(),
        _targetTimes.end(),
        [](const std::pair<double, std::string>& a,
           const std::pair<double, std::string>& b) -> bool
        {
            return a.first < b.first;
        }
    );
    std::stable_sort(_captureProgression.begin(), _captureProgression.end());

    for (const std::pair<const std::string, ImageSubset>& sub : _subsetMap) {
        std::sort(
            _subsetMap[sub.first]._subset.begin(),
            _subsetMap[sub.first]._subset.end(),
            [](const Image& a, const Image& b) -> bool {
                return a.timeRange.start < b.timeRange.start;
            }
        );
    }

    std::sort(
        _instrumentTimes.begin(),
        _instrumentTimes.end(),
        [](const std::pair<std::string, TimeRange>& a,
           const std::pair<std::string, TimeRange>& b)
        {
            return a.second.start < b.second.start;
        }
    );
}

void ImageSequencer::runSequenceParser(SequenceParser& parser) {
    // get new data
    std::map<std::string, std::unique_ptr<Decoder>>& translations =
        parser.translations();
    std::map<std::string, ImageSubset>& imageData = parser.getSubsetMap();
    const std::vector<std::pair<std::string, TimeRange>>& instrumentTimes =
        parser.getInstrumentTimes();
    const std::vector<std::pair<double, std::string>>& targetTimes =
        parser.getTargetTimes();
    const std::vector<double>& captureProgression = parser.getCaptureProgression();

    // check for sanity
    if (imageData.empty() || instrumentTimes.empty() || captureProgression.empty()) {
        LINFO("Parser did not contain images, instrument times or capture progression");
        return;
    }

    // append data
    for (std::pair<const std::string, std::unique_ptr<Decoder>>& it : translations) {
        _fileTranslation[it.first] = std::move(it.second);
    }

    for (std::pair<const std::string, ImageSubset>& it : imageData) {
        if (_subsetMap.find(it.first) == _subsetMap.end()) {
            // if key not exist yet - add sequence data for key (target)
            _subsetMap.insert(it);
            continue;
        }

        const std::string& key = it.first;
        std::vector<Image>& source = it.second._subset; // prediction
        const std::vector<Image>& destination = _subsetMap[key]._subset; // imagery

        // simple search function
        double min = 10;
        auto findMin = [&](const std::vector<Image>& vector) -> double {
            for (size_t i = 1; i < vector.size(); ++i) {
                double e = std::abs(
                    vector[i].timeRange.start - vector[i - 1].timeRange.start
                );
                min = std::min(e, min);
            }
            return min;
        };

        // find the smallest separation of images in time
        double epsilon;
        //epsilon = findMin(source);
        epsilon = findMin(destination);
        // set epsilon as 1% smaller than min
        epsilon -= min * 0.01;

        // IFF images have same time as mission planned capture, erase that event
        // from 'predicted event file' (mission-playbook)
        for (Image& i : source) {
            for (const Image& j : destination) {
                const double diff = std::abs(i.timeRange.start - j.timeRange.start);
                if (diff < epsilon) {
                    source.erase(source.begin() + 1);
                }
            }
        }
        // pad image data with predictions (ie - where no actual images,
        // add placeholder)
        _subsetMap[key]._subset.insert(
            _subsetMap[key]._subset.end(),
            source.begin(),
            source.end()
        );
    }

    _instrumentTimes.insert(
        _instrumentTimes.end(),
        instrumentTimes.begin(),
        instrumentTimes.end()
    );
    _targetTimes.insert(
        _targetTimes.end(),
        targetTimes.begin(),
        targetTimes.end()
    );
    _captureProgression.insert(
        _captureProgression.end(),
        captureProgression.begin(),
        captureProgression.end()
    );

    // sorting of data _not_ optional
    sortData();

    // extract payload from _fileTranslation
    for (std::pair<const std::string, std::unique_ptr<Decoder>>& t : _fileTranslation) {
        if (t.second->decoderType() == "CAMERA" || t.second->decoderType() == "SCANNER") {
            const std::vector<std::string>& spiceIDs = t.second->translations();
            for (const std::string& id : spiceIDs) {
                for (std::pair<std::string, bool>& switching : _switchingMap) {
                    if (switching.first == id) {
                        switching.second = false;
                    }
                }
            }
        }
    }
    _hasData = true;
}

}  // namespace openspace
