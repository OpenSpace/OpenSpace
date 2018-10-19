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

#include <modules/spacecraftinstruments/util/labelparser.h>

#include <openspace/util/spicemanager.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/directory.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "LabelParser";
    constexpr const char* keySpecs   = "Read";
    constexpr const char* keyConvert = "Convert";

    constexpr const char* PlaybookIdentifierName = "LabelParser";
} // namespace

namespace openspace {

LabelParser::LabelParser(std::string name, std::string fileName,
                         const ghoul::Dictionary& translationDictionary)
    : _name(std::move(name))
    , _fileName(std::move(fileName))
{
    // get the different instrument types
    const std::vector<std::string>& decoders = translationDictionary.keys();
    // for each decoder (assuming might have more if hong makes changes)
    for (const std::string& decoderStr : decoders) {
        ghoul::Dictionary typeDictionary;
        translationDictionary.getValue(decoderStr, typeDictionary);

        //create dictionary containing all {playbookKeys , spice IDs}
        if (decoderStr == "Instrument") {
            // for each playbook call -> create a Decoder object
            const std::vector<std::string>& keys = typeDictionary.keys();
            for (const std::string& key : keys) {
                std::string currentKey = decoderStr + "." + key;

                ghoul::Dictionary decoderDictionary =
                    translationDictionary.value<ghoul::Dictionary>(currentKey);

                std::unique_ptr<Decoder> decoder = Decoder::createFromDictionary(
                    decoderDictionary,
                    decoderStr
                );
                // insert decoder to map - this will be used in the parser to determine
                // behavioral characteristics of each instrument
                _fileTranslation[key] = std::move(decoder);
            }
        }
        if (decoderStr == "Target") {
            ghoul::Dictionary specsOfInterestDictionary;
            typeDictionary.getValue(keySpecs, specsOfInterestDictionary);

            _specsOfInterest.resize(specsOfInterestDictionary.size());
            for (size_t n = 0; n < _specsOfInterest.size(); ++n) {
                std::string readMe;
                specsOfInterestDictionary.getValue(std::to_string(n + 1), readMe);
                _specsOfInterest[n] = readMe;
            }
            ghoul::Dictionary convertDictionary;
            typeDictionary.getValue(keyConvert, convertDictionary);

            const std::vector<std::string>& keys = convertDictionary.keys();
            for (const std::string& key : keys) {
                ghoul::Dictionary itemDictionary;
                convertDictionary.getValue(key, itemDictionary);
                std::unique_ptr<Decoder> decoder = Decoder::createFromDictionary(
                    itemDictionary,
                    decoderStr
                );
                // insert decoder to map - this will be used in the parser to determine
                // behavioral characteristics of each instrument
                _fileTranslation[key] = std::move(decoder);
            };
        }
    }
}

std::string LabelParser::decode(const std::string& line) {
    for (std::pair<const std::string, std::unique_ptr<Decoder>>& key : _fileTranslation) {
        std::size_t value = line.find(key.first);
        if (value != std::string::npos) {
            std::string toTranslate = line.substr(value);
            return _fileTranslation[toTranslate]->translations()[0];

        }
    }
    return "";
}

std::string LabelParser::encode(const std::string& line) const {
    using K = std::string;
    using V = std::unique_ptr<Decoder>;
    for (const std::pair<const K, V>& key : _fileTranslation) {
        std::size_t value = line.find(key.first);
        if (value != std::string::npos) {
            return line.substr(value);
        }
    }
    return "";
}

bool LabelParser::create() {
    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(_fileName, RawPath::Yes);
    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR(fmt::format("Could not load Label Directory '{}'", sequenceDir.path()));
        return false;
    }

    std::string previousTarget;
    std::string lblName;


    using Recursive = ghoul::filesystem::Directory::Recursive;
    using Sort = ghoul::filesystem::Directory::Sort;
    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::No);
    for (const std::string& path : sequencePaths) {
        size_t position = path.find_last_of('.') + 1;
        if (position == 0 || position == std::string::npos) {
            continue;
        }

        ghoul::filesystem::File currentFile(path);
        const std::string& extension = currentFile.fileExtension();

        if (extension != "lbl" && extension != "LBL") {
            continue;
        }

        std::ifstream file(currentFile.path());

        if (!file.good()) {
            LERROR(fmt::format("Failed to open label file '{}'", currentFile.path()));
            return false;
        }

        int count = 0;

        // open up label files
        //TimeRange instrumentRange;

        double startTime = 0.0;
        double stopTime = 0.0;
        std::string line;
        do {
            std::getline(file, line);

            line.erase(std::remove(line.begin(), line.end(), '"'), line.end());
            line.erase(std::remove(line.begin(), line.end(), ' '), line.end());
            line.erase(std::remove(line.begin(), line.end(), '\r'), line.end() );

            std::string read = line.substr(0, line.find_first_of('='));

            _detectorType = "CAMERA"; //default value

            constexpr const char* ErrorMsg =
                "Unrecognized '{}' in line {} in file {}. The 'Convert' table must "
                "contain the identity tranformation for all values encountered in the "
                "label files, for example: ROSETTA = {{ \"ROSETTA\" }}";

            /* Add more  */
            if (read == "TARGET_NAME") {
                _target = decode(line);
                if (_target.empty()) {
                    LWARNING(fmt::format(ErrorMsg, "TARGET_NAME", line, path));
                }
                count++;
            }
            if (read == "INSTRUMENT_HOST_NAME") {
                _instrumentHostID = decode(line);
                if (_instrumentHostID.empty()) {
                    LWARNING(fmt::format(ErrorMsg, "INSTRUMENT_HOST_NAME", line, path));
                }
                count++;
            }
            if (read == "INSTRUMENT_ID") {
                _instrumentID = decode(line);
                if (_instrumentID.empty()) {
                    LWARNING(fmt::format(ErrorMsg, "INSTRUMENT_ID", line, path));
                }
                lblName = encode(line);
                count++;
            }
            if (read == "DETECTOR_TYPE") {
                _detectorType = decode(line);
                if (_detectorType.empty()) {
                    LWARNING(fmt::format(ErrorMsg, "DETECTOR_TYPE", line, path));
                }
                count++;
            }

            if (read == "START_TIME") {
                std::string start = line.substr(line.find('=') + 1);
                start.erase(std::remove(start.begin(), start.end(), ' '), start.end());
                startTime = SpiceManager::ref().ephemerisTimeFromDate(start);
                count++;

                getline(file, line);
                line.erase(std::remove(line.begin(), line.end(), '"'), line.end());
                line.erase(std::remove(line.begin(), line.end(), ' '), line.end());
                line.erase(std::remove(line.begin(), line.end(), '\r'), line.end());

                read = line.substr(0, line.find_first_of('='));
                if (read == "STOP_TIME") {
                    std::string stop = line.substr(line.find('=') + 1);
                    stop.erase(
                        std::remove_if(
                            stop.begin(),
                            stop.end(),
                            [](char c) { return c == ' ' || c == '\r'; }
                        ),
                        stop.end()
                    );
                    stopTime = SpiceManager::ref().ephemerisTimeFromDate(stop);
                    count++;
                }
                else{
                    LERROR(fmt::format(
                        "Label file {} deviates from generic standard", currentFile.path()
                    ));
                    LINFO(
                        "Please make sure input data adheres to format from \
                        https://pds.jpl.nasa.gov/documents/qs/labels.html"
                    );
                }
            }
            if (count == static_cast<int>(_specsOfInterest.size())) {
                const std::vector<std::string> extensions =
                    ghoul::io::TextureReader::ref().supportedExtensions();

                count = 0;

                using namespace std::literals;
                std::string p = path.substr(0, path.size() - ("lbl"s).size());
                for (const std::string& ext : extensions) {
                    std::string imagePath = p + ext;
                    if (FileSys.fileExists(imagePath)) {
                        std::vector<std::string> spiceInstrument;
                        spiceInstrument.push_back(_instrumentID);

                        Image image = {
                            TimeRange(startTime, stopTime),
                            imagePath,
                            spiceInstrument,
                            _target,
                            false,
                            false
                        };

                        _subsetMap[image.target]._subset.push_back(image);
                        _subsetMap[image.target]._range.include(startTime);

                        _captureProgression.push_back(startTime);
                        std::stable_sort(
                            _captureProgression.begin(),
                            _captureProgression.end()
                        );

                        break;
                    }
                }
            }
        } while (!file.eof());
    }

    std::vector<Image> tmp;
    for (const std::pair<const std::string, ImageSubset>& key : _subsetMap) {
        for (const Image& image : key.second._subset) {
            tmp.push_back(image);
        }
    }
    std::sort(
        tmp.begin(),
        tmp.end(),
        [](const Image& a, const Image& b) -> bool {
            return a.timeRange.start < b.timeRange.start;
        }
    );

    for (const Image& image : tmp) {
        if (previousTarget != image.target) {
            previousTarget = image.target;
            _targetTimes.emplace_back(image.timeRange.start , image.target);
            std::sort(
                _targetTimes.begin(),
                _targetTimes.end(),
                [](const std::pair<double, std::string> &a,
                   const std::pair<double, std::string> &b) -> bool
                {
                    return a.first < b.first;
                }
            );
        }
    }

    for (const std::pair<const std::string, ImageSubset>& target : _subsetMap) {
        _instrumentTimes.emplace_back(lblName, _subsetMap[target.first]._range);
    }
    sendPlaybookInformation(PlaybookIdentifierName);
    return true;
}

} // namespace openspace
