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
    std::vector<std::string> RadecManager::_dataFiles;
    double RadecManager::_ra;
    double RadecManager::_dec;
    double RadecManager::_range;

   bool RadecManager::extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary){
     bool dataFilesSuccess = DataFileHelper::checkFileNames(identifier, dictionary, RadecManager::_dataFiles);
     radecParser(0);
     return dataFilesSuccess;
    }

   glm::vec3 RadecManager::GetPosForTime(double time) {
       std::vector<double> timeDoubles = DataFileHelper::getHoursFromFileNames(_dataFiles);
       int idx = RenderableSignals::findFileIndexForCurrentTime(time, timeDoubles);
       
       if (radecParser(idx)) {
           return glm::vec3(_ra,_dec,_range);
       }
       return glm::vec3(-1,-1,-1);
   }

   bool RadecManager::radecParser(int index) {
       std::string filename;

       if (index == -1 || index > _dataFiles.size())
           return false;
       filename = _dataFiles[index];

       std::ifstream ifs(filename);
       nlohmann::json j = nlohmann::json::parse(ifs);
        _ra = j["RADn"].get<double>();
        _dec = j["DecDn"].get<double>();
        _range = j["GeoRngDn"].get<double>();

       return true;
   }

}


