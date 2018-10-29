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

#include <modules/dsn/dsnmanager/dsnmanager.h>


namespace openspace {
    constexpr const char* _loggerCat = "DsnManager";
    
    // Keys to get values from dictionary
    constexpr const char* KeyDataFolder = "DataFolder";
    constexpr const char* KeyDataFileType = "DataFileType";

    struct DsnManager::DsnData DsnManager::_dsnData;
    std::vector<double> DsnManager::_startTimes;
    std::vector<std::string> DsnManager::_dataFiles;

    //Filetypes
    const std::string dataFileTypeStringJson = "json";

    enum class DataFileType : int {
        Json = 0,
        Invalid
    };
 
    /**
    * Extracts the general information (from the lua modfile) that is mandatory for the class
    * to function; such as the file type and the location of the data files.
    * Returns false if it fails to extract mandatory information!
    */
    bool DsnManager::extractMandatoryInfoFromDictionary(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary)
    {
        DataFileType sourceFileType = DataFileType::Invalid;
        
        // ------------------- EXTRACT MANDATORY VALUES FROM DICTIONARY ------------------- //
        std::string dataFileTypeString;
       if (!dictionary->getValue(KeyDataFileType, dataFileTypeString)) {
          LERROR(fmt::format("{}: The field {} is missing", identifier, KeyDataFileType));
        }
        std::transform(
            dataFileTypeString.begin(),
            dataFileTypeString.end(),
            dataFileTypeString.begin(),
            [](char c) { return static_cast<char>(tolower(c)); }
        );
        // Verify that the input type is correct
        if (dataFileTypeString == dataFileTypeStringJson) {
            sourceFileType = DataFileType::Json;
        }
        else {
            LERROR(fmt::format(
                "{}: {} is not a recognized {}",
                identifier, dataFileTypeString, KeyDataFileType
            ));
            return false;
        }
        
        std::string dataFolderPath;
  if (!dictionary->getValue(KeyDataFolder, dataFolderPath)) {
       LERROR(fmt::format("{}: The field {} is missing", identifier, KeyDataFolder));
        return false;
      }

        // Ensure that the source folder exists and then extract
        // the files with the same extension as <inputFileTypeString>
        ghoul::filesystem::Directory dataFolder(dataFolderPath);
        if (FileSys.directoryExists(dataFolder)) {
            // Extract all file paths from the provided folder
            _dataFiles = dataFolder.readFiles(
                ghoul::filesystem::Directory::Recursive::No,
                ghoul::filesystem::Directory::Sort::Yes
            );

            // Remove all files that don't have <dataFileTypeString> as extension
            _dataFiles.erase(
                std::remove_if(
                    _dataFiles.begin(),
                    _dataFiles.end(),
                    [dataFileTypeString](const std::string& str) {
                const size_t extLength = dataFileTypeString.length();
                std::string sub = str.substr(str.length() - extLength, extLength);
                std::transform(
                    sub.begin(),
                    sub.end(),
                    sub.begin(),
                    [](char c) { return static_cast<char>(::tolower(c)); }
                );
                return sub != dataFileTypeString;
            }),
                _dataFiles.end()
                );
            // Ensure that there are available and valid source files left
            if (_dataFiles.empty()) {
                LERROR(fmt::format(
                    "{}: {} contains no {} files",
                    identifier, dataFolderPath, dataFileTypeString
                ));
                return false;
            }
        }
        else {
            LERROR(fmt::format(
                "{}: {} is not a valid directory",
                identifier,
                dataFolderPath
            ));
            return false;
        }
        extractTriggerTimesFromFileNames(_dataFiles);

        return DsnManager::jsonParser(0);
    }

    // Extract J2000 time from file names
    // Requires files to be named as such: 'YYYY-DDDT.json'
    void DsnManager::extractTriggerTimesFromFileNames(std::vector<std::string> _dataFiles) {
        // number of  characters in filename (excluding '.json')
        constexpr const int FilenameSize = 9;
        // size(".json")
        constexpr const int ExtSize = 5;

        for (const std::string& filePath : _dataFiles) {
            const size_t strLength = filePath.size();
            // Extract the filename from the path (without extension)
            std::string timeString = filePath.substr(
                strLength - FilenameSize - ExtSize,
                FilenameSize
            );
            // Ensure the separators are correct
            timeString.replace(4, 1, "-");
            timeString.replace(FilenameSize-1, 1, "T");

            const double triggerTime = Time::convertTime(timeString);
            _startTimes.push_back(triggerTime);
        }
    }

    bool DsnManager::jsonParser(int index) {

        std::string filename;
        if (index == -1 || index > _dataFiles.size())
            return false;

        filename = _dataFiles[index];
        std::ifstream ifs(filename);
        nlohmann::json j = nlohmann::json::parse(ifs);

       DsnManager::Signal structSignal;

       // number of  characters in filename (excluding '.json')
       constexpr const int FilenameSize = 9;
       // size(".json")
       constexpr const int ExtSize = 5;

        const size_t strLength = filename.size();
        // Extract the filename from the path (without extension)
        std::string startTimeString = filename.substr(
            strLength - FilenameSize - ExtSize,
            FilenameSize
        );
        // Ensure the separators are correct
        startTimeString.replace(4, 1, "-");
        startTimeString.replace(FilenameSize - 1, 1, "T");

        const double triggerTime = Time::convertTime(startTimeString);
      
       _dsnData.sequenceStartTime = triggerTime;
       _dsnData.signals.clear();
       _dsnData.signals.reserve(0);

       //loop through all signals in the data 
      for (const auto& signalsInJson : j["Signals"]) {

          structSignal.signalId = signalsInJson["activityid"].get<int>();
          structSignal.dishName = signalsInJson["facility"].get<std::string>();
          structSignal.year = signalsInJson["year"].get<int>();
          structSignal.spacecraft = signalsInJson["projuser"].get<std::string>();
          structSignal.endTime = signalsInJson["eot"].get<std::string>(); 
          structSignal.startTime = signalsInJson["bot"].get<std::string>();
          structSignal.direction = signalsInJson["direction"].get<std::string>();

          //Add signals to vector of signals
          _dsnData.signals.push_back(structSignal);
        }

      _dsnData.isLoaded = true;
      
      return _dsnData.isLoaded;
    }

    glm::vec3 DsnManager::approximateSpacecraftPosition(const char* dishId, glm::vec3 dishPos) {

        double dishPosXYZ[] = { dishPos.x, dishPos.y, dishPos.z };
        double dishPosLLA[] = { -35.383, 148.966, 692 };
        
        double azimuthAngle = 205.87; // angle from true north 
        double elevationAngle = 13.40; // angle from horizontal plane towards zenith
        double range = 1.7762947155343E10 * 1000; //DSS35 to VGR2

        // spacecraft coordinates in RAE
        double spacecraftPosRAE[3] = { range, azimuthAngle, elevationAngle};
        double spacecraftPosXYZ[3] = {};

        // fill up spacecraftPosXYZ
        convertRaeToEcef(dishPosLLA, dishPosXYZ, spacecraftPosRAE, spacecraftPosXYZ);
        glm::vec3 position = { spacecraftPosXYZ[0],spacecraftPosXYZ[1],spacecraftPosXYZ[2] };

        //return position
        return position;
    }

    void DsnManager::convertRaeToEcef(double observerLla[], double observerXyz[], 
        double objectRae[], double objectEcef[]) {
        
        double tempSez[] = { 0.0, 0.0, 0.0 };

        convertRaeToSez(observerLla, objectRae, tempSez);
        convertSezToEcef(observerLla, observerXyz, tempSez, objectEcef);
    }

    void DsnManager::convertRaeToSez(double observerLla[], double objectRae[], double objectSez[]) {
        double range, azimuth, elevation;
        range = objectRae[0];
        azimuth = objectRae[1];
        elevation = objectRae[2];

        // Compute needed math
        double slat = sin(deg2rad(observerLla[0]));
        double slon = sin(deg2rad(observerLla[1]));
        double clat = cos(deg2rad(observerLla[0]));
        double clon = cos(deg2rad(observerLla[1]));

        // Convert to radians
        azimuth = deg2rad(azimuth);
        elevation = deg2rad(elevation);

        // Convert
        objectSez[0] = -range * cos(elevation) * cos(azimuth);
        objectSez[1] = range * cos(elevation) * sin(azimuth);
        objectSez[2] = range * sin(elevation);
    }

    void DsnManager::convertSezToEcef(double observerLla[], double observerXyz[], 
        double objectSez[], double objectEcef[]) {

        double south, east, zenith;
        south = objectSez[0];
        east = objectSez[1];
        zenith = objectSez[2];

        // Compute needed math
        double slat = sin(deg2rad(observerLla[0]));
        double slon = sin(deg2rad(observerLla[1]));
        double clat = cos(deg2rad(observerLla[0]));
        double clon = cos(deg2rad(observerLla[1]));

        // Convert
        objectEcef[0] = (slat * clon * south) + (-slon * east) + (clat * clon * zenith) + observerXyz[0];
        objectEcef[1] = (slat * slon * south) + (clon * east) + (clat * slon * zenith) + observerXyz[1];
        objectEcef[2] = (-clat * south) + (slat * zenith) + observerXyz[2];
    }


    double DsnManager::deg2rad(double degrees)
    {
        const double factor = glm::pi<double>() / 180;
        return degrees * factor;
    }
}


