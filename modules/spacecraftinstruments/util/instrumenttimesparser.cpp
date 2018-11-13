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

#include <modules/spacecraftinstruments/util/instrumenttimesparser.h>

#include <openspace/util/spicemanager.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/directory.h>
#include <ghoul/misc/dictionary.h>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "InstrumentTimesParser";

    constexpr const char* PlaybookIdentifierName = "InstrumentTimesParser";
    constexpr const char* KeyTargetBody = "Target.Body";
    constexpr const char* KeyInstruments = "Instruments";
    constexpr const char* KeyInstrument = "Instrument";
    constexpr const char* KeyInstrumentFiles = "Files";
} // namespace

namespace openspace {

InstrumentTimesParser::InstrumentTimesParser(std::string name, std::string sequenceSource,
                                             ghoul::Dictionary& inputDict)
    : _pattern("\"(.{23})\" \"(.{23})\"")
    , _name(std::move(name))
    , _fileName(std::move(sequenceSource))
{

    _target = inputDict.value<std::string>(KeyTargetBody);
    ghoul::Dictionary instruments = inputDict.value<ghoul::Dictionary>(KeyInstruments);

    for (const std::string& key : instruments.keys()) {
        ghoul::Dictionary instrument = instruments.value<ghoul::Dictionary>(key);
        ghoul::Dictionary files = instrument.value<ghoul::Dictionary>(KeyInstrumentFiles);
        _fileTranslation[key] = Decoder::createFromDictionary(instrument, KeyInstrument);
        for (size_t i = 0; i < files.size(); i++) {
            std::string filename = files.value<std::string>(std::to_string(i + 1));
            _instrumentFiles[key].push_back(std::move(filename));
        }
    }
}

bool InstrumentTimesParser::create() {
    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(_fileName, RawPath::Yes);
    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR(fmt::format("Could not load Label Directory '{}'", sequenceDir.path()));
        return false;
    }

    using K = std::string;
    using V = std::vector<std::string>;
    for (const std::pair<const K, V>& p : _instrumentFiles) {
        const std::string& instrumentID = p.first;
        for (std::string filename : p.second) {
            std::string filepath = FileSys.pathByAppendingComponent(
                sequenceDir.path(),
                std::move(filename)
            );

            if (!FileSys.fileExists(filepath)) {
                LERROR(fmt::format("Unable to read file '{}'. Skipping file", filepath));
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
                        LERROR(
                            "Bad event data formatting. Must \
                            have regex 3 matches (source string, start time, stop time)."
                        );
                        successfulRead = false;
                        break;
                    }

                    TimeRange captureTimeRange;
                    try { // parse date strings
                        std::string start = matches[1].str();
                        std::string stop = matches[2].str();
                        captureTimeRange.start =
                            SpiceManager::ref().ephemerisTimeFromDate(start);
                        captureTimeRange.end =
                            SpiceManager::ref().ephemerisTimeFromDate(stop);
                    }
                    catch (const SpiceManager::SpiceException& e) {
                        LERROR(e.what());
                        successfulRead = false;
                        break;
                    }

                    instrumentActiveTimeRange.include(captureTimeRange);

                    //_instrumentTimes.push_back({ instrumentID, timeRange });
                    _targetTimes.emplace_back(captureTimeRange.start, _target);
                    _captureProgression.push_back(captureTimeRange.start);

                    Image image = {
                        captureTimeRange,
                        std::string(),
                        { instrumentID },
                        _target,
                        true,
                        false
                    };
                    _subsetMap[_target]._subset.push_back(std::move(image));
                }
            }
            if (successfulRead){
                _subsetMap[_target]._range.include(instrumentActiveTimeRange);
                _instrumentTimes.emplace_back(instrumentID, instrumentActiveTimeRange);
            }
        }
    }

    std::stable_sort(_captureProgression.begin(), _captureProgression.end());
    std::stable_sort(
        _targetTimes.begin(),
        _targetTimes.end(),
        [](const std::pair<double, std::string>& a,
           const std::pair<double, std::string>& b) -> bool
        {
            return a.first < b.first;
        }
    );

    sendPlaybookInformation(PlaybookIdentifierName);
    return true;
}

} // namespace openspace
