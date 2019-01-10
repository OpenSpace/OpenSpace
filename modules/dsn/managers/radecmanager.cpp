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
        timeDoubles = DataFileHelper::getHoursFromFileNames(_dataFiles);

        return dataFilesSuccess;
    }
    
   bool RadecManager::correctFileInterval(double time) const{
       const bool isTimeInFileInterval = (time  > _checkFileTime) &&
           (time < _checkFileEndTime);

       return isTimeInFileInterval;
   }

   bool RadecManager::correctUpdateInterval(double time) const {
       const bool isTimeInActiveMinute = (time > activeMinute - updateFrequency *60 && time < activeMinute + updateFrequency *60);
           return isTimeInActiveMinute;
   }

   void RadecManager::setUpdateFrequency(double updatedFreq) {
       //Determines how many minutes between updates
       updateFrequency = updatedFreq;
   }

   glm::vec3 RadecManager::getPosForTime(double time) const {

       if (!correctFileInterval(time)) {
           int idx = DataFileHelper::findFileIndexForCurrentTime(time, timeDoubles);

           //If index is same as previous, don't parse the data again
           if(idx != prevFileIndex){
               prevFileIndex = idx;
               updateRadecData(idx);
               if (positions.size()) {
                   int index = DataFileHelper::findFileIndexForCurrentTime(time, minuteTimes);
                   updateActiveMinute(index);
               }
           }
       }

       if (positions.size() && !correctUpdateInterval(time)) {
           //Compensate for light travel time to the spacecraft
           int idx = DataFileHelper::findFileIndexForCurrentTime(time, minuteTimes);
           if (idx != prevMinIndex) {
               prevMinIndex = idx;
               updateActiveMinute(idx);

               double lighttimeCompensation = positions[idx].lightTravelTime;
               int compensatedIdx = DataFileHelper::findFileIndexForCurrentTime(time + lighttimeCompensation, minuteTimes);
               getPositionInVector(compensatedIdx);
           }

       }  

       return glm::vec3(position.ra, position.dec, position.range);
   }

   bool RadecManager::radecParser(int index) const{
       std::string filename;
       if (index > -1 && index < _dataFiles.size()) {
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
                       position.lightTravelHours = ceil(position.lightTravelTime / 3600);
                   }
                   catch (const std::exception& e) {
                       LERROR(fmt::format("{}: Error in json object number {} while reading file '{}'", objectIdentifier, objectCounter, filename));
                   }
                   RadecManager::positions.push_back(position); 
               }
               isReady = true;
               return true;
       }
       else return false;
      
   }

   void RadecManager::updateActiveMinute(int idx) const{
       minuteTimes.clear();
       minuteTimes.reserve(0);

       for (int i = 0; i < RadecManager::positions.size(); i++) {
           minuteTimes.push_back(Time::convertTime(positions[i].timeStamp));
       }
       activeMinute = minuteTimes[idx];
  }

  RadecManager::Position RadecManager::getPositionInVector(int compensatedIndex) const{
       try{

           position.timeStamp = positions[compensatedIndex].timeStamp;
           position.ra = positions[compensatedIndex].ra;
           position.dec = positions[compensatedIndex].dec;
           position.range = positions[compensatedIndex].range;

       }
       catch (const std::exception& e) {
           LERROR(fmt::format("{}: Error when reading data from active minute, index {}",objectIdentifier, compensatedIndex));
       }

       return position;
   }

  void  RadecManager::updateRadecData(int index) const {
      std::string filename;

      if (index <= -1 || index > _dataFiles.size())
          return;

        positions.clear();
        positions.reserve(10);

        filename = _dataFiles[index];
        
        std::string startTimeString = DataFileHelper::getHourFromFileName(filename);
        const double triggerTime = Time::convertTime(startTimeString);

        _checkFileTime = triggerTime;
        _checkFileEndTime = triggerTime + 3600;

        // if our light travel time is longer than an hour,
        // compensate the position parsing index
        if (position.lightTravelHours > 1) {
            index = index + position.lightTravelHours;
        }

        if (index >= _dataFiles.size()) {
            return;
        }else if (index < 1) {
            radecParser(index);
            radecParser(index + 1);
            return;
        }
        else if (index == _dataFiles.size() -1) {
            radecParser(index);
            radecParser(index -1);
            return;
        }
        else{
            radecParser(index - 1);
            radecParser(index);
            radecParser(index + 1);
        }
  }
}


