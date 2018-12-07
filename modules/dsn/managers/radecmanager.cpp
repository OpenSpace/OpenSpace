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

#include <modules/dsn/managers/radecmanager.h>

namespace openspace {
    constexpr const char* _loggerCat = "RadecManager";
    constexpr const char* KeyIdentifier = "ObjectIdentifier";

    RadecManager::RadecManager() = default;
    bool RadecManager::extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary){
        if (dictionary->hasKeyAndValue<std::string>(KeyIdentifier)) {
            objectIdentifier = dictionary->value<std::string>(KeyIdentifier);
        }
        bool dataFilesSuccess = DataFileHelper::checkFileNames(identifier, dictionary, _dataFiles);
        return dataFilesSuccess;
    }

   bool RadecManager::correctHour(double time) const{
       const bool isTimeInFileInterval = (time  >= _checkFileTime) &&
           (time < _checkFileEndTime);

       return isTimeInFileInterval;
   }

   bool RadecManager::correctMinute(double time) const {
       const bool isTimeInActiveMinute = (time >= activeMinute && time < activeMinute + 60);
       return isTimeInActiveMinute;
   }

   glm::vec3 RadecManager::getPosForTime(double time) const {
       if (!correctHour(time)) {
           std::vector<double>timeDoubles = DataFileHelper::getHoursFromFileNames(_dataFiles);
           int idx = DataFileHelper::findFileIndexForCurrentTime(time, timeDoubles);
           updateRadecData(idx);
       }
       if(!correctMinute(time)) {
          getPositionInVector(time);
       }
       return glm::vec3(position.ra, position.dec, position.range);
   }

   bool RadecManager::radecParser(int index) const{
       std::string filename;
       filename = _dataFiles[index];
       std::ifstream ifs(filename);
       nlohmann::json j = nlohmann::json::parse(ifs);

       int objectCounter = 0;
       for (const auto& pos : j["Positions"]) {
           objectCounter++;
           try {
               position.timeStamp = pos["TimeStamp"].get<std::string>();
               position.ra = pos["RADn"].get<double>();
               position.dec = pos["DecDn"].get<double>();
               position.range = pos["GeoRngDn"].get<double>();
               position.lightTravelTime = pos["DLT"].get<double>();

           }
           catch (const std::exception& e) {
               LERROR(fmt::format("{}: Error in json object number {} while reading file '{}'", objectIdentifier, objectCounter, filename));
           }
           RadecManager::positions.push_back(position);
       }
       return true;
   }

  RadecManager::Position RadecManager::getPositionInVector(double time) const{
       minuteTimes.clear();
       minuteTimes.reserve(0);
      
       for (int i = 0; i < RadecManager::positions.size(); i++) {
           minuteTimes.push_back(Time::convertTime(positions[i].timeStamp));
       }

       int idx = DataFileHelper::findFileIndexForCurrentTime(time + position.lightTravelTime, minuteTimes);//Compensate for light travel time to the spacecraft
       activeMinute = minuteTimes[idx];
       position.timeStamp = positions[idx].timeStamp;
       position.ra = positions[idx].ra;
       position.dec = positions[idx].dec;
       position.range = positions[idx].range;

       return position;
   }

  void  RadecManager::updateRadecData(int index) const {
        std::string filename;

        if (index == -1 || index > _dataFiles.size()) return;

        positions.clear();
        positions.reserve(10);

        filename = _dataFiles[index];
        std::string startTimeString = DataFileHelper::getHourFromFileName(filename);
        const double triggerTime = Time::convertTime(startTimeString);

        _checkFileTime = triggerTime;
        _checkFileEndTime = triggerTime + 3600;

        //Light travel time in hours determines where to search for the correct position
        int lightTravelHours = ceil(position.lightTravelTime / 3600);

        if (lightTravelHours > 1)
          index = index + lightTravelHours;

        if (index < lightTravelHours + 1) {
            radecParser(index);
            radecParser(index + 1);
            return;
        }
        else if (index == _dataFiles.size() - lightTravelHours) {
            radecParser(index);
            radecParser(index -1);
            return;
        }
        else {
            radecParser(index - 1);
            radecParser(index);
            radecParser(index + 1);
        }
  }
}


