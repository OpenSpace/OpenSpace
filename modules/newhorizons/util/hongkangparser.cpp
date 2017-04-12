/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/newhorizons/util/hongkangparser.h>

#include <modules/newhorizons/util/imagesequencer.h>
#include <modules/newhorizons/util/instrumentdecoder.h>

#include <openspace/util/spicemanager.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/filesystem/filesystem.h>

#include <fstream>

namespace {
    const char* keyTranslation = "DataInputTranslation";

    const char* PlaybookIdentifierName = "HongKang";
}

namespace openspace {

HongKangParser::HongKangParser(std::string name, std::string fileName, 
                               std::string spacecraft,
                               ghoul::Dictionary translationDictionary,
                               std::vector<std::string> potentialTargets)
    : _name(std::move(name))
    , _defaultCaptureImage(absPath("${OPENSPACE_DATA}/scene/common/textures/placeholder.png"))
    , _fileName(std::move(fileName))
    , _spacecraft(std::move(spacecraft))
    , _potentialTargets(std::move(potentialTargets))
{
    //get the different instrument types
    const std::vector<std::string>& decoders = translationDictionary.keys();
    //for each decoder (assuming might have more if hong makes changes)
    for (int i = 0; i < decoders.size(); ++i) {
        //create dictionary containing all {playbookKeys , spice IDs}
        if (decoders[i] == "Instrument") {
            ghoul::Dictionary typeDictionary;
            translationDictionary.getValue(decoders[i], typeDictionary);
            //for each playbook call -> create a Decoder object
            const std::vector<std::string>& keys = typeDictionary.keys();
            //std::string abort = decoders[i] + "." + keyStopCommand;
            for (int j = 0; j < keys.size(); ++j) {
                std::string currentKey = decoders[i] + "." + keys[j];
                
                ghoul::Dictionary decoderDictionary;
                translationDictionary.getValue(currentKey, decoderDictionary);

                auto decoder = Decoder::createFromDictionary(decoderDictionary, decoders[i]);
                //insert decoder to map - this will be used in the parser to determine
                //behavioral characteristics of each instrument
                _fileTranslation[keys[j]] = std::move(decoder);
            }
        }
        //Hong's playbook needs _only_ instrument translation though.
    }
}

void HongKangParser::findPlaybookSpecifiedTarget(std::string line, std::string& target) {
    //remembto add this lua later... 
    std::transform(
        line.begin(),
        line.end(),
        line.begin(),
        [](char v) { return static_cast<char>(toupper(v)); }
    );
    std::vector<std::string> ptarg = _potentialTargets;
    for (const auto& p : ptarg) {
        // loop over all targets and determine from 4th col which target this instrument points to
        if (line.find(p) != std::string::npos) {
            target = p;
            break;
        }
        else {
            // not found - we set void until we have more info. 
            target = "VOID";
        }
    }
}

bool HongKangParser::create() {
    //check input for errors. 
    bool hasObserver = SpiceManager::ref().hasNaifId(_spacecraft);
    if (!hasObserver) {
        throw ghoul::RuntimeError(
            "SPICE has no observer: '" + _spacecraft + "' in kernel pool",
            "HongKangParser"
        );
    }
    if (_potentialTargets.size() == 0) {
        throw ghoul::RuntimeError(
            "List of potential target is missing in order to parse the event file",
            "HongKangParser"
        );
    }

    if (size_t position = _fileName.find_last_of(".") + 1){
        if (position != std::string::npos){
            std::string extension = ghoul::filesystem::File(_fileName).fileExtension();

            if (extension == "txt") { // Hong Kang. pre-parsed playbook
                std::ifstream file;
                file.exceptions(std::ofstream::failbit | std::ofstream::badbit);
                file.open(absPath(_fileName));
                //std::ifstream file(_fileName , std::ios::binary);
                //if (!file.good()){
                //    LERROR("Failed to open event file '" << _fileName << "'");
                //    return false;
                //}

                std::string line = "";
                double shutter = 0.01;

                std::string previousTarget;

                std::string previousCamera;

                TimeRange cameraRange;
                TimeRange scanRange;

                std::vector<std::string> scannerSpiceID;
                std::vector<std::string> cameraSpiceID;

                double capture_start = -1;
                double capture_stop = -1;
                double scan_start = -1;
                double scan_stop = -1;

                std::string cameraTarget  = "VOID";
                std::string scannerTarget = "VOID";

                while (!file.eof()) {
                    std::getline(file, line);
                    
                    std::string event = line.substr(0, line.find_first_of(" ")); 

                    auto it = _fileTranslation.find(event);
                    bool foundEvent = (it != _fileTranslation.end());

                    std::string met = line.substr(25, 9);
                    double time = getETfromMet(met);

                    if (foundEvent){
                        //store the time, this is used for getNextCaptureTime() 
                        _captureProgression.push_back(time);
                        
                        if (it->second->getDecoderType() == "CAMERA") {
                            if (capture_start == -1) {
                                //encountered new camera sequence- store start time
                                capture_start = time;
                                previousCamera = it->first;
                            }
                            //always store individual image for camera
                            cameraSpiceID = it->second->getTranslation();
                            //rely on playboook mdl column to determine target
                            findPlaybookSpecifiedTarget(line, cameraTarget);

                            //fill image

                            Image image;
                            image.timeRange = TimeRange(time, time + shutter);
                            image.path = _defaultCaptureImage;
                            image.activeInstruments = cameraSpiceID;
                            image.target = cameraTarget;
                            image.isPlaceholder = true;
                            image.projected = false;
                            //createImage(image, time, time + shutter, cameraSpiceID, cameraTarget, _defaultCaptureImage);

                            //IFF spaccraft has decided to switch target, store in target map (used for: 'next observation focus')
                            if (previousTarget != image.target) {
                                previousTarget = image.target;
                                std::pair<double, std::string> v_target = std::make_pair(time, image.target);
                                _targetTimes.push_back(v_target);
                            }

                            //store actual image in map. All targets get _only_ their corresp. subset.
                            _subsetMap[image.target]._subset.push_back(image);
                            //compute and store the range for each subset 
                            _subsetMap[image.target]._range.include(time);
                        }
                        if (it->second->getDecoderType() == "SCANNER") { // SCANNER START 
                            scan_start = time;

                            InstrumentDecoder* scanner = static_cast<InstrumentDecoder*>(it->second.get());
                            std::string endNominal = scanner->getStopCommand();

                            // store current position in file
                            std::streampos len = file.tellg();
                            std::string linePeek;
                            bool foundstop = false;
                            while (!file.eof() && !foundstop) {
                                //continue grabbing next line until we find what we need
                                getline(file, linePeek);
                                if (linePeek.find(endNominal) != std::string::npos) {
                                    foundstop = true;

                                    met = linePeek.substr(25, 9);
                                    scan_stop = getETfromMet(met);
                                    findPlaybookSpecifiedTarget(line, scannerTarget);
                                    scannerSpiceID = it->second->getTranslation();

                                    scanRange = { scan_start, scan_stop };
                                    ghoul_assert(scanRange.isDefined(), "Invalid time range!");
                                    _instrumentTimes.push_back(std::make_pair(it->first, scanRange));

                                    //store individual image
                                    Image image;
                                    image.timeRange = scanRange;
                                    image.path = _defaultCaptureImage;
                                    image.activeInstruments = scannerSpiceID;
                                    image.target = cameraTarget;
                                    image.isPlaceholder = true;
                                    image.projected = false;

                                    _subsetMap[scannerTarget]._subset.push_back(image);
                                    _subsetMap[scannerTarget]._range.include(scan_start);
                                }
                            }
                            //go back to stored position in file
                            file.seekg(len, std::ios_base::beg);
                        }
                    }
                    else { // we have reached the end of a scan or consecutive capture sequence!
                        if (capture_start != -1) {
                            //end of capture sequence for camera, store end time of this sequence
                            capture_stop = time;
                            cameraRange = { capture_start, capture_stop };
                            ghoul_assert(cameraRange.isDefined(), "Invalid time range!");
                            _instrumentTimes.push_back(std::make_pair(previousCamera, cameraRange));

                            capture_start = -1;
                        }
                    }
                }
            }
        }
    }

    sendPlaybookInformation(PlaybookIdentifierName);
    return true;
}

bool HongKangParser::augmentWithSpice(Image& image, std::string spacecraft, 
                                      std::vector<std::string> payload, 
                                      std::vector<std::string> potentialTargets)
{
    image.target = "VOID";
    // we have (?) to cast to int, unfortunately
    // Why? --abock
    // because: old comment --m
    
    int exposureTime = image.timeRange.duration();
    if (exposureTime == 0) {
        exposureTime = 1;
    }

    for (int i = 0; i < potentialTargets.size(); ++i) {
        for (int j = 0; j < image.activeInstruments.size(); ++j) {
            double time = image.timeRange.start;
            for (int k = 0; k < exposureTime; k++) {
                time += k;
                bool withinFOV = SpiceManager::ref().isTargetInFieldOfView(
                    potentialTargets[i],
                    spacecraft,
                    image.activeInstruments[j],
                    SpiceManager::FieldOfViewMethod::Ellipsoid,
                    {},
                    time
                );

                if (withinFOV) {
                    image.target = potentialTargets[i];
                }
            }
        }
    }
    return false;
}

double HongKangParser::getETfromMet(std::string line) {
    std::string::size_type sz;
    return getETfromMet(std::stod(line, &sz));
}

double HongKangParser::getETfromMet(double met) {
    const double referenceET =
        SpiceManager::ref().ephemerisTimeFromDate("2015-07-14T11:50:00.00");

    //_metRef += 3; // MET reference time is off by 3 sec? 

    const double diff = std::abs(met - _metRef);
    if (met > _metRef) {
        return referenceET + diff;
    } else if (met < _metRef) {
        return referenceET - diff;
    }
    return 0.0;
}

double HongKangParser::getMetFromET(double et) {
    const double referenceET =
        SpiceManager::ref().ephemerisTimeFromDate("2015-07-14T11:50:00.00");

    if (et >= referenceET) {
        return _metRef + (et - referenceET);
    } else {
        return _metRef - (referenceET - et);
    }
}

} // namespace openspace
