/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "ImageSequencer";
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

bool ImageSequencer::isReady() const {
    return _hasData;
}

std::pair<double, std::string> ImageSequencer::nextTarget(double time) const {
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

std::pair<double, std::string> ImageSequencer::currentTarget(double time) const {
    const auto it = std::lower_bound(
        _targetTimes.begin(),
        _targetTimes.end(),
        time,
        [](const std::pair<double, std::string>& a, double b) { return a.first < b; }
    );

    if (it != _targetTimes.end() && it != _targetTimes.begin()) {
        return *std::prev(it);
    }
    else {
        return { 0.0, "No Target" };
    }
}

double ImageSequencer::prevCaptureTime(double time) const {
    const auto it = std::lower_bound(
        _captureProgression.begin(),
        _captureProgression.end(),
        time
    );
    if (it != _captureProgression.end() && it != _captureProgression.begin()) {
        return *std::prev(it);
    }
    else {
        return 0.0;
    }
}

double ImageSequencer::nextCaptureTime(double time) const {
    const auto it = std::lower_bound(
        _captureProgression.begin(),
        _captureProgression.end(),
        time
    );
    if (it != _captureProgression.end()) {
        return *it;
    }
    else {
        return 0.0;
    }
}

Image ImageSequencer::latestImageForInstrument(const std::string& instrumentID) const {
    const auto it = _latestImages.find(instrumentID);
    if (it != _latestImages.end()) {
        return _latestImages.at(instrumentID);
    }
    else {
        return Image();
    }
}

const std::vector<double>& ImageSequencer::captureProgression() const {
    return _captureProgression;
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
    // return entire map, seen in GUI
    return _switchingMap;
}

bool ImageSequencer::isInstrumentActive(double time, const std::string& instrument) const
{
    for (const std::pair<std::string, TimeRange>& i : _instrumentTimes) {
        // check if this instrument is in range
        if (!i.second.includes(time)) {
            continue;
        }

        // if so, then get the corresponding spiceID
        const std::vector<std::string>& is = _fileTranslation.at(i.first)->translations();
        // check which specific subinstrument is firing
        const auto it = std::find(is.begin(), is.end(), instrument);
        if (it != is.end()) {
            return true;
        }
    }
    return false;
}

float ImageSequencer::instrumentActiveTime(double time,
                                           const std::string& instrumentID) const
{
    for (const std::pair<std::string, TimeRange>& i : _instrumentTimes) {
        // check if this instrument is in range
        if (!i.second.includes(time)) {
            continue;
        }
        // if so, then get the corresponding spiceID
        const std::vector<std::string>& is = _fileTranslation.at(i.first)->translations();

        // check which specific subinstrument is firing
        const auto it = std::find(is.begin(), is.end(), instrumentID);
        if (it != is.end()) {
            return static_cast<float>(
                (time - i.second.start) / (i.second.end - i.second.start)
            );
        }
    }
    return -1.f;
}

std::vector<Image> ImageSequencer::imagePaths(const std::string& projectee,
                                              const std::string& instrument, double time,
                                              double sinceTime)
{
    // TODO: Check how this works with time jumps

    // check if this instance is either in range or
    // a valid candidate to recieve data

    const bool instrumentActive = isInstrumentActive(time, instrument);
    const bool hasCurrentTime = _subsetMap[projectee]._range.includes(time);
    const bool hasSinceTime = _subsetMap[projectee]._range.includes(sinceTime);

    if (!instrumentActive || (!hasCurrentTime && !hasSinceTime)) {
        return std::vector<Image>();
    }

    // for readability we store the iterators
    auto begin = _subsetMap[projectee]._subset.begin();
    auto end = _subsetMap[projectee]._subset.end();

    // create temporary storage
    std::vector<Image> captures;
    // what to look for
    Image findPrevious;
    Image findCurrent;
    findPrevious.timeRange.start = sinceTime;
    findCurrent.timeRange.start = time;

    // find the two iterators that correspond to the latest time jump
    auto compareTime = [](const Image& a, const Image& b) -> bool {
        return a.timeRange.start < b.timeRange.start;
    };
    auto curr = std::lower_bound(begin, end, findCurrent , compareTime);
    auto prev = std::lower_bound(begin, end, findPrevious, compareTime);

    if (curr == begin || curr == end || prev == begin || prev == end || prev >= curr ||
        curr->timeRange.start < prev->timeRange.start)
    {
        return std::vector<Image>();
    }

    std::copy_if(
        prev,
        curr,
        back_inserter(captures),
        [instrument](const Image& i) { return i.activeInstruments[0] == instrument; }
    );

    if (!captures.empty()) {
        _latestImages[captures.back().activeInstruments.front()] = captures.back();
    }

    std::vector<int> toDelete;
    for (auto it = captures.begin(); it != captures.end(); it++) {
        if (!it->isPlaceholder) {
            continue;
        }

        double beforeDist = std::numeric_limits<double>::max();
        if (it != captures.begin()) {
            beforeDist = std::abs(std::prev(it)->timeRange.start - it->timeRange.start);
        }

        double nextDist = std::numeric_limits<double>::max();
        if (it != captures.end() - 1) {
            nextDist = std::abs(std::next(it)->timeRange.start - it->timeRange.start);
        }

        if (beforeDist < 1.0 || nextDist < 1.0) {
            toDelete.push_back(static_cast<int>(std::distance(captures.begin(), it)));
        }
    }

    for (size_t i = 0; i < toDelete.size(); i++) {
        // We have to subtract i here as we already have deleted i value before this and
        // we need to adjust the location
        const int v = toDelete[i] - static_cast<int>(i);
        captures.erase(captures.begin() + v);
    }

    return captures;
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
    std::map<std::string, std::unique_ptr<Decoder>>& translations = parser.translations();
    std::map<std::string, ImageSubset>& imageData = parser.subsetMap();
    const std::vector<std::pair<std::string, TimeRange>>& instrumentTimes =
        parser.instrumentTimes();
    const std::vector<std::pair<double, std::string>>& targetTimes = parser.targetTimes();
    const std::vector<double>& captureProgression = parser.captureProgression();

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
        auto findMin = [&min](const std::vector<Image>& vec) -> double {
            for (size_t i = 1; i < vec.size(); i++) {
                const double e = std::abs(
                    vec[i].timeRange.start - vec[i - 1].timeRange.start
                );
                min = std::min(e, min);
            }
            return min;
        };

        // find the smallest separation of images in time
        double epsilon = findMin(destination);
        // set epsilon as 1% smaller than min
        epsilon -= min * 0.01;

        // IFF images have same time as mission planned capture, erase that event
        // from 'predicted event file' (mission-playbook)
        for (const Image& i : source) {
            for (const Image& j : destination) {
                const double diff = std::abs(i.timeRange.start - j.timeRange.start);
                if (diff < epsilon) {
                    source.erase(source.begin() + 1);
                }
            }
        }
        // pad image data with predictions (ie - where no actual images, add placeholder)
        _subsetMap[key]._subset.insert(
            _subsetMap[key]._subset.end(),
            source.begin(),
            source.end()
        );

        _subsetMap[key]._range.include(it.second._range);
    }

    _instrumentTimes.insert(
        _instrumentTimes.end(),
        instrumentTimes.begin(),
        instrumentTimes.end()
    );
    _targetTimes.insert(_targetTimes.end(), targetTimes.begin(), targetTimes.end());
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
