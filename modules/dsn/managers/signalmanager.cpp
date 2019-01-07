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

#include <modules/dsn/managers/signalmanager.h>

#include <openspace/util/spicemanager.h>
namespace openspace {
    constexpr const char* _loggerCat = "SignalManager";
    constexpr const char* KeySpacecraftIdMap = "SpacecraftIdMap";
    //The abbreviation in the data(NAIF ID) to the SceneGraphNode identifier of the spacecrafts
    std::map<std::string, std::string> spacecraftDataToId;

    struct SignalManager::SignalData SignalManager::signalData;
    std::vector<double> SignalManager::fileStartTimes;
    std::vector<std::string> SignalManager::_dataFiles;

    bool SignalManager::extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary)
    {
        bool dataFilesSuccess = DataFileHelper::checkFileNames(identifier, dictionary, _dataFiles);
        fileStartTimes = DataFileHelper::getDaysFromFileNames(_dataFiles);

        ghoul::Dictionary spacecraftDictionary;

        if (dictionary->getValue(KeySpacecraftIdMap, spacecraftDictionary)) {
            std::vector<std::string> keys = spacecraftDictionary.keys();
            for (int i = 0; i < keys.size(); i++)
            {
                std::string dataAbbr = keys.at(i);
                std::string nodeId = spacecraftDictionary.value<std::string>(keys.at(i));
                spacecraftDataToId[dataAbbr] = nodeId;
            }
        }
        else {
            LERROR("No {} set for {}!", KeySpacecraftIdMap, _loggerCat);
        }


        return dataFilesSuccess;
    }

    bool SignalManager::signalParser(int index) {
    
        std::string filename = _dataFiles[index];
        std::ifstream ifs(filename);
        nlohmann::json j = nlohmann::json::parse(ifs);

        SignalManager::Signal structSignal;  
        int objectCounter = 0;

        for (const auto& signalsInJson : j["Signals"]) {
            objectCounter++;
            try {
                structSignal.dishName = signalsInJson["facility"].get<std::string>();
                structSignal.spacecraft = spacecraftDataToId.at(signalsInJson["projuser"].get<std::string>());
                structSignal.endTime = signalsInJson["eot"].get<std::string>();
                structSignal.startTime = signalsInJson["bot"].get<std::string>();
                structSignal.direction = signalsInJson["direction"].get<std::string>();
                structSignal.lightTravelTime = signalsInJson["DLT"].get<double>();

                if (structSignal.direction == "uplink") {
                    structSignal.startTransmission = SpiceManager::ref().ephemerisTimeFromDate(structSignal.startTime);
                    structSignal.endTransmission = SpiceManager::ref().ephemerisTimeFromDate(structSignal.endTime);
                }
                else if (structSignal.direction == "downlink") {
                    structSignal.startTransmission = SpiceManager::ref().ephemerisTimeFromDate(structSignal.startTime) - 
                                                        structSignal.lightTravelTime;
                    structSignal.endTransmission = SpiceManager::ref().ephemerisTimeFromDate(structSignal.endTime) -
                                                        structSignal.lightTravelTime;;
                }// if we have both an uplink and a downlink, handle these like two different signals
                else if (structSignal.direction == "both") {
                   // handle ordinary signal like uplink
                   // structSignal.endTimeExtension = structSignal.lightTravelTime;
                    structSignal.startTransmission = SpiceManager::ref().ephemerisTimeFromDate(structSignal.startTime);
                    structSignal.endTransmission = SpiceManager::ref().ephemerisTimeFromDate(structSignal.endTime);
                    structSignal.direction = "uplink";
                    // Make an extra downlink
                    SignalManager::Signal structSignal2;
                    structSignal2.dishName = structSignal.dishName;
                    structSignal2.spacecraft = structSignal.spacecraft;
                    structSignal2.endTime = structSignal.endTime;
                    structSignal2.startTime = structSignal.startTime;
                    structSignal2.direction = "downlink";
                    structSignal2.lightTravelTime = structSignal.lightTravelTime;
                    structSignal2.startTransmission = SpiceManager::ref().ephemerisTimeFromDate(structSignal.startTime) -
                        structSignal2.lightTravelTime;
                    structSignal2.endTransmission = SpiceManager::ref().ephemerisTimeFromDate(structSignal.endTime) -
                        structSignal2.lightTravelTime;;

                    //Add extra signal to vector of signals
                    signalData.signals.push_back(structSignal2);
                }
                //Add signal to vector of signals
                signalData.signals.push_back(structSignal);
            }
            catch (const std::exception& e) {
                LERROR(fmt::format("Error in json object number {} while reading signal data file '{}'", objectCounter, filename));
            }
        }

      return true;
    }

    /* We load the signals for the current day, as well as a 
    * buffer for the previous day and the next day. This allows 
    * us to keep signal data in memory that transmit over midnight,
    * as well as signals that have a long light travel time. */
    void SignalManager::updateSignalData(int index, int sizeBuffer) {
        
        // Currently have to be 1, could be optimized in future
        // according to the longest light travel time
        int lightTimeTravelBuffer = 1;

        if (index <= -1 || index >= _dataFiles.size())
        {
            LERROR(fmt::format("{}: File index {} is out of bounds.", _loggerCat, index));
            return;
        }

        signalData.signals.clear();
        signalData.signals.reserve(sizeBuffer);

        std::string activeTimeFilename = _dataFiles[index];
        std::string startTimeString = DataFileHelper::getDayFromFileName(activeTimeFilename);
        const double triggerTime = Time::convertTime(startTimeString);

        signalData.sequenceStartTime = triggerTime;
        //86400 equals 24hrs in seconds
        signalData.sequenceEndTime = triggerTime + 86400; 

        if (index < lightTimeTravelBuffer)
        {
            signalParser(index);
            signalParser(index + lightTimeTravelBuffer);
            signalData.needsUpdate = false;
            signalData.signals.shrink_to_fit();
            return;
        }
        else if (index == (_dataFiles.size()-1)) {

            signalParser(index- lightTimeTravelBuffer);
            signalParser(index);
            signalData.needsUpdate = false;
            signalData.signals.shrink_to_fit();
            return;  
        }
        else {
            signalParser(index - lightTimeTravelBuffer);
            signalParser(index);
            signalParser(index + lightTimeTravelBuffer);
            signalData.needsUpdate = false;
            signalData.signals.shrink_to_fit();
        }
    }

}


