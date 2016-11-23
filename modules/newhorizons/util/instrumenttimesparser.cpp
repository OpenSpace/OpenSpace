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

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <openspace/util/time.h>
#include <openspace/util/spicemanager.h>
#include <modules/newhorizons/util/decoder.h>
#include <fstream>
#include <string>
#include <iterator>
#include <iomanip>
#include <limits>
#include <sstream>
#include <modules/newhorizons/util/instrumenttimesparser.h>

namespace {
    const std::string _loggerCat = "InstrumentTimesParser";

    const char* PlaybookIdentifierName = "InstrumentTimesParser";
    const char* KeyTargetBody = "Target.Body";
    const char* KeyInstruments = "Instruments";
    const char* KeyInstrument = "Instrument";
    const char* KeyInstrumentFiles = "Files";
}

namespace openspace {

InstrumentTimesParser::InstrumentTimesParser(
    const std::string& name,
    const std::string& sequenceSource,
    ghoul::Dictionary& inputDict)
    : _name(name)
    , _fileName(sequenceSource) 
    , _pattern("\"(.{23})\" \"(.{23})\"")
    , _target("")
    , _detectorType("CAMERA")
{
    
    _target = inputDict.value<std::string>(KeyTargetBody);
    ghoul::Dictionary instruments = inputDict.value<ghoul::Dictionary>(KeyInstruments);
    
    for (const auto& instrumentKey : instruments.keys()) {
        ghoul::Dictionary instrument = instruments.value<ghoul::Dictionary>(instrumentKey);
        ghoul::Dictionary files = instrument.value<ghoul::Dictionary>(KeyInstrumentFiles);
        _fileTranslation[instrumentKey] = std::move(Decoder::createFromDictionary(instrument, KeyInstrument));
        for (int i = 0; i < files.size(); i++) {
            std::string filename = files.value<std::string>(std::to_string(i + 1));
            _instrumentFiles[instrumentKey].push_back(filename);
        }
    }
}



bool InstrumentTimesParser::create() {
    auto imageComparer = [](const Image &a, const Image &b)->bool{
        return a.timeRange.start < b.timeRange.start;
    };
    auto targetComparer = [](const std::pair<double, std::string> &a,
        const std::pair<double, std::string> &b)->bool{
        return a.first < b.first;
    };

    
    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(_fileName, RawPath::Yes);
    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load Label Directory '" << sequenceDir.path() << "'");
        return false;
    }

    for (auto it = _instrumentFiles.begin(); it != _instrumentFiles.end(); it++) {
        std::string instrumentID = it->first;
        for (std::string filename: it->second) {
            std::string filepath = FileSys.pathByAppendingComponent(sequenceDir.path(), filename);
            
            if (!FileSys.fileExists(filepath)) {
                LERROR("Unable to read file " << filepath << ". Skipping file.");
                continue;
            }

            // Read file into string 
            std::ifstream inFile(filepath);
            std::string line;
            std::smatch matches;
            TimeRange instrumentActiveTimeRange;
            bool successfulRead = true;
            while (std::getline(inFile, line)) {
                if (std::regex_match(line, matches, _pattern)) {
                    if (matches.size() != 3) {
                        LERROR("Bad event data formatting. Must \
                                have regex 3 matches (source string, start time, stop time).");
                        successfulRead = false;
                        break;
                    }
                    
                    TimeRange captureTimeRange;
                    try { // parse date strings
                        std::string start = matches[1].str();
                        std::string stop = matches[2].str();
                        captureTimeRange.start = SpiceManager::ref().ephemerisTimeFromDate(start);
                        captureTimeRange.end = SpiceManager::ref().ephemerisTimeFromDate(stop);
                    }
                    catch (const SpiceManager::SpiceException& e){
                        LERROR(e.what());
                        successfulRead = false;
                        break;
                    }

                    instrumentActiveTimeRange.include(captureTimeRange);

                    //_instrumentTimes.push_back({ instrumentID, timeRange });
                    _targetTimes.push_back({ captureTimeRange.start, _target });
                    _captureProgression.push_back(captureTimeRange.start);

                    Image image;
                    image.timeRange = captureTimeRange;
                    image.path = "";
                    image.isPlaceholder = true;
                    image.activeInstruments.push_back(instrumentID);
                    image.target = _target;
                    image.projected = false;

                    _subsetMap[_target]._subset.push_back(image);
                }
            }
            if (successfulRead){
                _subsetMap[_target]._range.include(instrumentActiveTimeRange);
                _instrumentTimes.push_back({ instrumentID, instrumentActiveTimeRange });
            }
        }
    }
    
    std::stable_sort(_captureProgression.begin(), _captureProgression.end());
    //std::stable_sort(_instrumentTimes.begin(), _instrumentTimes.end());
    std::stable_sort(_targetTimes.begin(), _targetTimes.end(), targetComparer);

    sendPlaybookInformation(PlaybookIdentifierName);
    return true;
}


}