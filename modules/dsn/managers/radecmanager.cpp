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

    RadecManager::RadecManager() = default;
   bool RadecManager::extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary){
     bool dataFilesSuccess = DataFileHelper::checkFileNames(identifier, dictionary, _dataFiles);
     radecParser(0);
     return dataFilesSuccess;
    }

   glm::vec3 RadecManager::getPosForTime(double time) const {
       std::vector<double> timeDoubles = DataFileHelper::getHoursFromFileNames(_dataFiles); //save as member 

       int idx = DataFileHelper::findFileIndexForCurrentTime(time, timeDoubles);
       if (radecParser(idx)) {
           //If we have the correct file, check for the correct position in that file.
           glm::vec3 pos = findPositionInVector(time);
           return glm::vec3(pos.x,pos.y,pos.z);
       }
       return glm::vec3(-1,-1,-1);
   }

   bool RadecManager::radecParser(int index) const{
       std::string filename;

       if (index == -1 || index > _dataFiles.size())
           return false;

       filename = _dataFiles[index];

       std::string startTimeString = DataFileHelper::getHourFromFileName(filename);
       const double triggerTime = Time::convertTime(startTimeString);

       _checkFileTime = triggerTime;

       std::ifstream ifs(filename);
       nlohmann::json j = nlohmann::json::parse(ifs);

       RadecManager::Position position;
       positions.clear();
       positions.reserve(0);

       for (const auto& pos : j["Positions"]) {
           position.timeStamp = pos["TimeStamp"].get<std::string>();
           position.ra = pos["RAUp"].get<double>();
           position.dec = pos["DecUp"].get<double>();
           position.range = pos["GeoRngUp"].get<double>();
          
           RadecManager::positions.push_back(position);
       }
       return true;
   }

   glm::vec3 RadecManager::findPositionInVector(double time) const{
       minuteTimes.clear();
       minuteTimes.reserve(0);

       for (int i = 0; i < RadecManager::positions.size(); i++) {
           //Convert each timestamp in vector of positions to j2000
           minuteTimes.push_back(Time::convertTime(positions[i].timeStamp));
       }
       int idx = DataFileHelper::findFileIndexForCurrentTime(time, minuteTimes);
       glm::vec3 pos = { positions[idx].ra, positions[idx].dec, positions[idx].range };
       return pos;
   }

}


