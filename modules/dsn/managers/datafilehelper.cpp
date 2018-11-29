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

#include <modules/dsn/managers/datafilehelper.h>

namespace openspace {
    constexpr const char* _loggerCat = "DataFileHelper";

    // Keys to get values from dictionary
    constexpr const char* KeyDataFolder = "DataFolder";
    constexpr const char* KeyDataFileType = "DataFileType";

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
    bool DataFileHelper::checkFileNames(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary, std::vector<std::string> &dataFiles)
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
            dataFiles = dataFolder.readFiles(
                ghoul::filesystem::Directory::Recursive::No,
                ghoul::filesystem::Directory::Sort::Yes
            );

            // Remove all files that don't have <dataFileTypeString> as extension
            dataFiles.erase(
                std::remove_if(
                    dataFiles.begin(),
                    dataFiles.end(),
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
                dataFiles.end()
                );
            // Ensure that there are available and valid source files left
            if (dataFiles.empty()) {
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
        return true;
    }

    /*Get one day, must have format YYYY-DDDT*/
    std::string DataFileHelper::getDayFromFileName(std::string filename) {
        // number of  characters in filename (excluding '.json')
        constexpr const int FilenameSize = 9;
        return DataFileHelper::getFileNameTime(filename, FilenameSize);
    }
    /*get a vector of all filenames, must have format YYY-DDDT*/
    std::vector<double> DataFileHelper::getDaysFromFileNames(std::vector<std::string> dataFiles) {
        // number of  characters in filename (excluding '.json')
        constexpr const int FilenameSize = 9;
        return DataFileHelper::extractTriggerTimesFromFileNames(dataFiles, FilenameSize);
    }
    /*Get one hour, must have format YYYY-DDDTHH*/
    std::string DataFileHelper::getHourFromFileName(std::string filename) {
        // number of  characters in filename (excluding '.json')
        constexpr const int FilenameSize = 11;
        return DataFileHelper::getFileNameTime(filename, FilenameSize);
    }
    /*Get a vector of all hour, must have format YYYY-DDDTHH*/
    std::vector<double> DataFileHelper::getHoursFromFileNames(std::vector<std::string> dataFiles) {
        // number of  characters in filename (excluding '.json')
        constexpr const int FilenameSize = 11;
        return DataFileHelper::extractTriggerTimesFromFileNames(dataFiles, FilenameSize);
    }
    /*Get a vector of all hour, must have format YYYY-DDDTHHMM*/

    std::string DataFileHelper::getMinuteFromFileName(std::string filename) {
        // number of  characters in filename (excluding '.json')
        constexpr const int FilenameSize = 14;
        std::string newName = filename.replace(70, 1, ":");
        return DataFileHelper::getFileNameTime(newName, FilenameSize);
    }
    std::vector<double> DataFileHelper::geMinutesFromFileNames(std::vector<std::string> dataFiles) {
        // number of  characters in filename (excluding '.json')
        constexpr const int FilenameSize = 14;
        return DataFileHelper::extractTriggerTimesFromFileNames(dataFiles, FilenameSize);
    }

    std::string DataFileHelper::getFileNameTime(std::string filename, const int FilenameSize) {
        // size(".json")
        constexpr const int ExtSize = 5;
        const size_t strLength = filename.size();
        // Extract the filename from the path (without extension)
        std::string startTimeString = filename.substr(
            strLength - FilenameSize - ExtSize,
            FilenameSize
        );
        return startTimeString;
    }
    // Extract J2000 time from file names
    std::vector<double> DataFileHelper::extractTriggerTimesFromFileNames(std::vector<std::string> dataFiles, const int FilenameSize) {
        std::vector<double> fileStartTimes;
        for (const std::string& filePath : dataFiles) {

            std::string timeString = getFileNameTime(filePath, FilenameSize);
            if(FilenameSize == 14)
                timeString.replace(11, 1, ":");

            const double triggerTime = Time::convertTime(timeString);
            fileStartTimes.push_back(triggerTime);
        }
        return fileStartTimes;
    }

    int DataFileHelper::findFileIndexForCurrentTime(double time, std::vector<double> vec) {
        // upper_bound has O(log n) for sorted vectors, more efficient than for loop
        auto iter = std::upper_bound(vec.begin(), vec.end(), time);

        int fileIndex = -1;
        //check what index we got 
        if (iter != vec.end()) {
            if (iter != vec.begin()) {
                fileIndex = static_cast<int>(
                    std::distance(vec.begin(), iter)
                    ) - 1;
            }
            else {
                fileIndex = 0;
            }
        }
        else {
            fileIndex = static_cast<int>(vec.size()) - 1;
        }

        return fileIndex;
    }

  
}


