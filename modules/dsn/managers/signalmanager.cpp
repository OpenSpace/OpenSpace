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


namespace openspace {
    constexpr const char* _loggerCat = "SignalManager";

    struct SignalManager::SignalData SignalManager::signalData;
    std::vector<double> SignalManager::fileStartTimes;
    std::vector<std::string> SignalManager::_dataFiles;

    bool SignalManager::extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary)
    {
        bool dataFilesSuccess = DataFileHelper::checkFileNames(identifier, dictionary, _dataFiles);
        fileStartTimes = DataFileHelper::getDaysFromFileNames(_dataFiles);
        //SignalManager::updateSignalData(0, 0);

        return dataFilesSuccess;
    }

    bool SignalManager::signalParser(int index) {
    
        std::string filename = _dataFiles[index];
        std::ifstream ifs(filename);
        nlohmann::json j = nlohmann::json::parse(ifs);

        SignalManager::Signal structSignal;  

        for (const auto& signalsInJson : j["Signals"]) {
             structSignal.dishName = signalsInJson["facility"].get<std::string>();
             structSignal.spacecraft = signalsInJson["projuser"].get<std::string>();
             structSignal.endTime = signalsInJson["eot"].get<std::string>(); 
             structSignal.startTime = signalsInJson["bot"].get<std::string>();
             structSignal.direction = signalsInJson["direction"].get<std::string>();
             structSignal.lightTravelTime = 71397.6659308273;

            if (structSignal.direction == "uplink") {
                structSignal.endTimeExtension = structSignal.lightTravelTime;
            }
            else if (structSignal.direction == "downlink") {
                structSignal.startTimeExtension = structSignal.lightTravelTime;
            }
            else if (structSignal.direction == "both") {
                structSignal.endTimeExtension = structSignal.lightTravelTime;
                structSignal.startTimeExtension = structSignal.lightTravelTime;
            }

             //Add signal to vector of signals
             signalData.signals.push_back(structSignal);
        }

      return true;
    }

    /* We load the signals for the current day, as well as a 
    * buffer for the previous day and the next day. This allows 
    * us to keep signal data in memory that transmit over midnight,
    * as well as signals that have a long light travel time. */
    void SignalManager::updateSignalData(int index, int sizeBuffer) {
        
        // This will all
        int lightTimeTravelBuffer = 1;

        if (index == -1 || index > _dataFiles.size())
            return;

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
            signalData.isLoaded = true;
            signalData.signals.shrink_to_fit();
            return;
        }
        else if (index == _dataFiles.size()) {

            signalParser(index- lightTimeTravelBuffer);
            signalParser(index);
            signalData.isLoaded = true;
            signalData.signals.shrink_to_fit();
            return;  
        }
        else {
            signalParser(index - lightTimeTravelBuffer);
            signalParser(index);
            signalParser(index + lightTimeTravelBuffer);
            signalData.isLoaded = true;
            signalData.signals.shrink_to_fit();
        }
    }

}


