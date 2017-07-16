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

#include <modules/newhorizons/util/labelparser.h>

#include <modules/newhorizons/util/decoder.h>

#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>

#include <ghoul/filesystem/directory.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>

#include <fstream>
#include <iterator>
#include <iomanip>
#include <limits>

#include <modules/newhorizons/util/imagesequencer.h>

#include <map>
#include <string>
#include <vector>

namespace {
    const char* _loggerCat = "LabelParser";
    const char* keySpecs   = "Read";
    const char* keyConvert = "Convert";

    const char* PlaybookIdentifierName = "LabelParser";
} // namespace

namespace openspace {

LabelParser::LabelParser(std::string name, std::string fileName,
                         const ghoul::Dictionary& translationDictionary)
    : _name(std::move(name))
    , _fileName(std::move(fileName))
    , _badDecoding(false)
{
    //get the different instrument types
    const std::vector<std::string>& decoders = translationDictionary.keys();
    //for each decoder (assuming might have more if hong makes changes)
    for (size_t i = 0; i < decoders.size(); ++i) {
        ghoul::Dictionary typeDictionary;
        translationDictionary.getValue(decoders[i], typeDictionary);

        //create dictionary containing all {playbookKeys , spice IDs}
        if (decoders[i] == "Instrument") {
            //for each playbook call -> create a Decoder object
            std::vector<std::string> keys = typeDictionary.keys();
            for (size_t j = 0; j < keys.size(); ++j) {
                std::string currentKey = decoders[i] + "." + keys[j];

                ghoul::Dictionary decoderDictionary =
                    translationDictionary.value<ghoul::Dictionary>(currentKey);

                auto decoder = Decoder::createFromDictionary(decoderDictionary, decoders[i]);
                //insert decoder to map - this will be used in the parser to determine
                //behavioral characteristics of each instrument
                _fileTranslation[keys[j]] = std::move(decoder);
            }
        }
        if (decoders[i] == "Target"){
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
            for (size_t j = 0; j < keys.size(); j++){
                ghoul::Dictionary itemDictionary;
                convertDictionary.getValue(keys[j], itemDictionary);
                auto decoder = Decoder::createFromDictionary(itemDictionary, decoders[i]);
                //insert decoder to map - this will be used in the parser to determine
                //behavioral characteristics of each instrument
                _fileTranslation[keys[j]] = std::move(decoder);
            };
        }
    }
}

std::string LabelParser::decode(std::string line){
    for (auto& key : _fileTranslation){
        std::size_t value = line.find(key.first);
        if (value != std::string::npos){
            std::string toTranslate = line.substr(value);
            
            //if (_fileTranslation.find(toTranslate) == _fileTranslation.end()) {
            //    // not found
            //    _badDecoding = true;
            //    LERROR("Could not fins '" << toTranslate << "' in translation map." <<
            //           "\nPlease check label files");
            //    return "";
            //}
            return _fileTranslation[toTranslate]->getTranslation()[0]; //lbls always 1:1 -> single value return

        }
    }
    return "";
}

std::string LabelParser::encode(std::string line) {
    for (auto& key : _fileTranslation) {
        std::size_t value = line.find(key.first);
        if (value != std::string::npos) {
            return line.substr(value);
        }
    }
    return "";
}

bool LabelParser::create() {
    auto imageComparer = [](const Image &a, const Image &b)->bool{
        return a.timeRange.start < b.timeRange.start;
    };
    auto targetComparer = [](const std::pair<double, std::string> &a,
        const std::pair<double, std::string> &b)->bool{
        return a.first < b.first;
    };
    std::string previousTarget;
    std::string lblName = "";

    using RawPath = ghoul::filesystem::Directory::RawPath;
    ghoul::filesystem::Directory sequenceDir(_fileName, RawPath::Yes);
    if (!FileSys.directoryExists(sequenceDir)) {
        LERROR("Could not load Label Directory '" << sequenceDir.path() << "'");
        return false;
    }
    using Recursive = ghoul::filesystem::Directory::Recursive;
    using Sort = ghoul::filesystem::Directory::Sort;
    std::vector<std::string> sequencePaths = sequenceDir.read(Recursive::Yes, Sort::No);
    for (auto path : sequencePaths){
        if (size_t position = path.find_last_of(".") + 1){
            if (position != std::string::npos){
                ghoul::filesystem::File currentFile(path);
                std::string extension = currentFile.fileExtension();
                if (extension == "lbl" || extension == "LBL"){ // discovered header file         
                    std::ifstream file(currentFile.path());

                    if (!file.good()){
                        LERROR("Failed to open label file '" << currentFile.path() << "'");
                        return false;
                    }
                    
                    int count = 0;

                    // open up label files
                    std::string line = "";
                    TimeRange instrumentRange;

                    double startTime = 0.0;
                    double stopTime = 0.0;
                    do {
                        std::getline(file, line);


                        line.erase(std::remove(line.begin(), line.end(), '"'), line.end());
                        line.erase(std::remove(line.begin(), line.end(), ' '), line.end());
                        line.erase(std::remove(line.begin(), line.end(), '\r'), line.end());

                        std::string read = line.substr(0, line.find_first_of("="));

                        _detectorType = "CAMERA"; //default value

                        /* Add more  */
                        if (read == "TARGET_NAME"){
                            _target = decode(line);
                            count++;
                        }
                        if (read == "INSTRUMENT_HOST_NAME"){
                            _instrumentHostID = decode(line);
                            count++;
                        }
                        if (read == "INSTRUMENT_ID"){
                            _instrumentID = decode(line);
                            lblName = encode(line);
                            count++;
                        }
                        if (read == "DETECTOR_TYPE"){
                            _detectorType = decode(line);
                            count++; 
                        }
                    //    if (_badDecoding){
                    //        LERROR("Please examine file: '" << currentFile.path() << "'");
                    //        return false;
                    //    }

                        
                        if (read == "START_TIME"){
                            std::string start = line.substr(line.find("=") + 1);
                            start.erase(std::remove(start.begin(), start.end(), ' '), start.end());
                            startTime = SpiceManager::ref().ephemerisTimeFromDate(start);
                            count++;

                            getline(file, line);
                            line.erase(std::remove(line.begin(), line.end(), '"'), line.end());
                            line.erase(std::remove(line.begin(), line.end(), ' '), line.end());
                            line.erase(std::remove(line.begin(), line.end(), '\r'), line.end());

                            read = line.substr(0, line.find_first_of("="));
                            if (read == "STOP_TIME"){
                                std::string stop = line.substr(line.find("=") + 1);
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
                                LERROR("Label file " + currentFile.path() + " deviates from generic standard!");
                                LINFO("Please make sure input data adheres to format https://pds.jpl.nasa.gov/documents/qs/labels.html");
                            }
                        }
                        if (count == static_cast<int>(_specsOfInterest.size())) {
                            using ghoul::io::TextureReader;
                            auto extensions = TextureReader::ref().supportedExtensions();

                            count = 0;

                            using namespace std::literals;
                            std::string p = path.substr(0, path.size() - ("lbl"s).size());
                            for (const std::string& ext : extensions) {
                                path = p + ext;
                                if (FileSys.fileExists(path)) {
                                    Image image;
                                    std::vector<std::string> spiceInstrument;
                                    spiceInstrument.push_back(_instrumentID);
                                    createImage(image, startTime, stopTime, spiceInstrument, _target, path);

                                    _subsetMap[image.target]._subset.push_back(image);
                                    _subsetMap[image.target]._range.include(startTime);

                                    _captureProgression.push_back(startTime);
                                    std::stable_sort(_captureProgression.begin(), _captureProgression.end());

                                    break;
                                }
                            }
                        }
                    } while (!file.eof());
                }
            }
        }
    }
    
    std::vector<Image> tmp;
    for (auto key : _subsetMap){
        for (auto image : key.second._subset){
            tmp.push_back(image);
        }
    }
    std::sort(tmp.begin(), tmp.end(), imageComparer);

    for (auto image : tmp){
        if (previousTarget != image.target){
            previousTarget = image.target;
            std::pair<double, std::string> v_target = std::make_pair(image.timeRange.start, image.target);
            _targetTimes.push_back(v_target);
            std::sort(_targetTimes.begin(), _targetTimes.end(), targetComparer);
        }
    }

    for (auto target : _subsetMap){
        _instrumentTimes.push_back(std::make_pair(lblName, _subsetMap[target.first]._range));

    //    std::string min, max;
    //    SpiceManager::ref().getDateFromET(target.second._range._min, min);
    //    SpiceManager::ref().getDateFromET(target.second._range._max, max);

    //    myfile << std::endl;
    //    for (auto image : target.second._subset){
    //        std::string time_beg;
    //        std::string time_end;
    //        SpiceManager::ref().getDateFromET(image.startTime, time_beg);
    //        SpiceManager::ref().getDateFromET(image.stopTime, time_end);

    //        myfile << std::fixed
    //            << " "   << time_beg
    //            << "-->" << time_end
    //            << " [ " << image.startTime
    //            << " ] "   << image.target << std::setw(10);
    //        for (auto instrument : image.activeInstruments){
    //            myfile << " " << instrument;
    //        }
    //        myfile << std::endl;
    //    }
    }
    sendPlaybookInformation(PlaybookIdentifierName);
    return true;
}

void LabelParser::createImage(Image& image, double startTime, double stopTime, std::vector<std::string> instr, std::string targ, std::string pot) {
    image.timeRange = { startTime , stopTime };
    ghoul_assert(image.timeRange.isDefined(), "Invalid time range!");
    image.path = pot;
    for (int i = 0; i < instr.size(); i++){
        image.activeInstruments.push_back(instr[i]);
    }
    image.target = targ;
    image.projected = false;
}

}
