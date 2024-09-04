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

#include <modules/spacecraftinstruments/util/instrumenttimesparser.h>

#include <openspace/documentation/documentation.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/stringhelper.h>
#include <filesystem>
#include <fstream>

namespace {
    constexpr std::string_view _loggerCat = "InstrumentTimesParser";

    constexpr std::string_view KeyInstrument = "Instrument";
    constexpr std::string_view KeyInstrumentFiles = "Files";

    struct [[codegen::Dictionary(InstrumentTimesParser)]] Parameters {
        std::string target;
        std::map<std::string, ghoul::Dictionary> instruments;
    };
#include "instrumenttimesparser_codegen.cpp"
} // namespace

namespace openspace {

InstrumentTimesParser::InstrumentTimesParser(std::string name,
                                             std::filesystem::path sequenceSource,
                                             ghoul::Dictionary& inputDict)
    : _pattern("\"(.{23})\" \"(.{23})\"")
    , _name(std::move(name))
    , _fileName(std::move(sequenceSource))
{
    const Parameters p = codegen::bake<Parameters>(inputDict);

    _target = p.target;
    for (const std::pair<const std::string, ghoul::Dictionary>& ps : p.instruments) {
        const ghoul::Dictionary files = ps.second.value<ghoul::Dictionary>(
            KeyInstrumentFiles
        );
        _fileTranslation[ps.first] = Decoder::createFromDictionary(
            ps.second,
            KeyInstrument
        );
        for (size_t i = 0; i < files.size(); i++) {
            std::string filename = files.value<std::string>(std::to_string(i + 1));
            _instrumentFiles[ps.first].push_back(std::move(filename));
        }
    }
}

bool InstrumentTimesParser::create() {
    std::filesystem::path sequenceDir = absPath(_fileName);
    if (!std::filesystem::is_directory(sequenceDir)) {
        LERROR(std::format("Could not load label directory '{}'", sequenceDir));
        return false;
    }

    using K = std::string;
    using V = std::vector<std::string>;
    for (const std::pair<const K, V>& p : _instrumentFiles) {
        const std::string& instrumentID = p.first;
        for (const std::string& filename : p.second) {
            std::filesystem::path filepath = sequenceDir / filename;

            if (!std::filesystem::is_regular_file(filepath)) {
                LERROR(std::format("Unable to read file '{}'. Skipping file", filepath));
                continue;
            }

            // Read file into string
            std::ifstream inFile(filepath);
            std::string line;
            std::smatch matches;
            TimeRange instrumentActiveTimeRange;
            bool successfulRead = true;
            while (ghoul::getline(inFile, line)) {
                if (!std::regex_match(line, matches, _pattern)) {
                    continue;
                }

                if (matches.size() != 3) {
                    LERROR(
                        "Bad event data formatting. Must have regex 3 matches "
                        "(source string, start time, stop time)"
                    );
                    successfulRead = false;
                    break;
                }

                TimeRange tr;
                try { // parse date strings
                    const std::string start = matches[1].str();
                    const std::string stop = matches[2].str();
                    tr.start = SpiceManager::ref().ephemerisTimeFromDate(start);
                    tr.end = SpiceManager::ref().ephemerisTimeFromDate(stop);
                }
                catch (const SpiceManager::SpiceException& e) {
                    LERROR(e.what());
                    successfulRead = false;
                    break;
                }

                instrumentActiveTimeRange.include(tr);

                _targetTimes.emplace_back(tr.start, _target);
                _captureProgression.push_back(tr.start);

                Image image = {
                    .timeRange = tr,
                    .path = std::string(),
                    .activeInstruments = { instrumentID },
                    .target = _target,
                    .isPlaceholder = true,
                    .projected = false
                };
                _subsetMap[_target]._subset.push_back(std::move(image));
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
           const std::pair<double, std::string>& b)
        {
            return a.first < b.first;
        }
    );

    return true;
}

} // namespace openspace
