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

#include <modules/spacecraftinstruments/util/hongkangparser.h>

#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <modules/spacecraftinstruments/util/instrumentdecoder.h>
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
    double ephemerisTimeFromMissionElapsedTime(double met, double metReference) {
        const double referenceET = openspace::SpiceManager::ref().ephemerisTimeFromDate(
            "2015-07-14T11:50:00.00"
        );

        const double diff = std::abs(met - metReference);
        if (met > metReference) {
            return referenceET + diff;
        }
        else if (met < metReference) {
            return referenceET - diff;
        }
        return 0.0;
    }

    double ephemerisTimeFromMissionElapsedTime(const std::string& line, double met) {
        return ephemerisTimeFromMissionElapsedTime(std::stod(line), met);
    }
} // namespace

namespace openspace {

HongKangParser::HongKangParser(std::string name, std::string fileName,
                               std::string spacecraft,
                               const ghoul::Dictionary& translationDictionary,
                               std::vector<std::string> potentialTargets)
    : _defaultCaptureImage(absPath("${DATA}/placeholder.png"))
    , _name(std::move(name))
    , _fileName(std::move(fileName))
    , _spacecraft(std::move(spacecraft))
    , _potentialTargets(std::move(potentialTargets))
{
    // get the different instrument types
    // for each decoder (assuming might have more if hong makes changes)
    for (std::string_view decoderType : translationDictionary.keys()) {
        // create dictionary containing all {playbookKeys , spice IDs}
        if (decoderType == "Instrument") {
            if (!translationDictionary.hasKey(decoderType) ||
                !translationDictionary.hasValue<ghoul::Dictionary>(decoderType))
            {
                continue;
            }

            const ghoul::Dictionary typeDictionary =
                translationDictionary.value<ghoul::Dictionary>(decoderType);
            // for each playbook call -> create a Decoder object
            for (std::string_view key : typeDictionary.keys()) {
                const std::string& currentKey = std::format("{}.{}", decoderType, key);

                ghoul::Dictionary decoderDictionary;
                if (translationDictionary.hasValue<ghoul::Dictionary>(currentKey)) {
                    decoderDictionary = translationDictionary.value<ghoul::Dictionary>(
                        currentKey
                    );
                }

                std::unique_ptr<Decoder> decoder = Decoder::createFromDictionary(
                    decoderDictionary,
                    std::string(decoderType)
                );
                // insert decoder to map - this will be used in the parser to determine
                // behavioral characteristics of each instrument
                _fileTranslation[std::string(key)] = std::move(decoder);
            }
        }
        //Hong's playbook needs _only_ instrument translation though.
    }
}

std::string HongKangParser::findPlaybookSpecifiedTarget(std::string line) {
    //remembto add this lua later...
    line = ghoul::toUpperCase(line);
    const std::vector<std::string>& ptarg = _potentialTargets;
    std::string target;
    for (const std::string& p : ptarg) {
        // loop over all targets and determine from 4th col which target this instrument
        // points to
        if (line.find(p) != std::string::npos) {
            return p;
        }
        else {
            // not found - we set void until we have more info.
            target = "VOID";
        }
    }
    return target;
}

bool HongKangParser::create() {
    // check input for errors.
    const bool hasObserver = SpiceManager::ref().hasNaifId(_spacecraft);
    if (!hasObserver) {
        throw ghoul::RuntimeError(
            std::format("SPICE has no observer '{}' in kernel pool", _spacecraft),
            "HongKangParser"
        );
    }
    if (_potentialTargets.empty()) {
        throw ghoul::RuntimeError(
            "List of potential target is missing in order to parse the event file",
            "HongKangParser"
        );
    }
    const size_t position = _fileName.find_last_of('.') + 1;
    if (position == 0 || position == std::string::npos) {
        return true;
    }

    if (std::filesystem::path(_fileName).extension() != ".txt") {
        return true;
    }

    std::ifstream file;
    file.exceptions(std::ofstream::failbit | std::ofstream::badbit);
    file.open(absPath(_fileName));

    constexpr double Exposure = 0.01;

    std::string previousTarget;
    std::string previousCamera;

    double captureStart = -1.0;

    std::string cameraTarget = "VOID";

    std::string line;
    while (!file.eof()) {
        std::getline(file, line);

        const std::string event = line.substr(0, line.find_first_of(' '));

        const auto it = _fileTranslation.find(event);
        const bool foundEvent = (it != _fileTranslation.end());

        std::string met = line.substr(25, 9);
        const double time = ephemerisTimeFromMissionElapsedTime(met, _metRef);

        if (foundEvent) {
            // store the time, this is used for nextCaptureTime()
            _captureProgression.push_back(time);

            if (it->second->decoderType() == "CAMERA") {
                if (captureStart == -1.0) {
                    //encountered new camera sequence- store start time
                    captureStart = time;
                    previousCamera = it->first;
                }
                // always store individual image for camera
                std::vector<std::string> cameraSpiceID = it->second->translations();
                // rely on playboook mdl column to determine target
                cameraTarget = findPlaybookSpecifiedTarget(line);

                // fill image
                Image image = {
                    .timeRange = TimeRange(time, time + Exposure),
                    .path = _defaultCaptureImage.string(),
                    .activeInstruments = std::move(cameraSpiceID),
                    .target = cameraTarget,
                    .isPlaceholder = true,
                    .projected = false
                };

                // IFF spacecraft has decided to switch target, store in target
                // map (used for: 'next observation focus')
                if (previousTarget != image.target) {
                    previousTarget = image.target;
                    _targetTimes.emplace_back(time, image.target);
                }

                // compute and store the range for each subset
                _subsetMap[image.target]._range.include(time);
                // store actual image in map. All targets get _only_ their corresp. subset
                _subsetMap[image.target]._subset.push_back(std::move(image));
            }
            if (it->second->decoderType() == "SCANNER") { // SCANNER START
                const double scanStart = time;

                InstrumentDecoder* scanner = static_cast<InstrumentDecoder*>(
                    it->second.get()
                );
                const std::string& endNominal = scanner->stopCommand();

                // store current position in file
                const std::streampos len = file.tellg();
                std::string linePeek;
                while (!file.eof()) {
                    // continue grabbing next line until we find what we need
                    getline(file, linePeek);
                    if (linePeek.find(endNominal) != std::string::npos) {
                        met = linePeek.substr(25, 9);
                        const double scanStop = ephemerisTimeFromMissionElapsedTime(
                            met,
                            _metRef
                        );
                        const std::string scannerTarget = findPlaybookSpecifiedTarget(
                            line
                        );

                        TimeRange scanRange = TimeRange(scanStart, scanStop);
                        ghoul_assert(scanRange.isDefined(), "Invalid time range");
                        _instrumentTimes.emplace_back(it->first, scanRange);

                        // store individual image
                        Image image = {
                            .timeRange = std::move(scanRange),
                            .path = _defaultCaptureImage.string(),
                            .activeInstruments = it->second->translations(),
                            .target = cameraTarget,
                            .isPlaceholder = true,
                            .projected = false
                        };
                        _subsetMap[scannerTarget]._subset.push_back(std::move(image));
                        _subsetMap[scannerTarget]._range.include(scanStart);
                        break;
                    }
                }
                // go back to stored position in file
                file.seekg(len, std::ios_base::beg);
            }
        }
        else {
            // we have reached the end of a scan or consecutive capture sequence
            if (captureStart != -1) {
                // end of capture sequence for camera, store end time of this sequence
                TimeRange cameraRange = TimeRange(captureStart, time);
                ghoul_assert(cameraRange.isDefined(), "Invalid time range");
                _instrumentTimes.emplace_back(previousCamera, std::move(cameraRange));
                captureStart = -1;
            }
        }
    }

    return true;
}

} // namespace openspace
