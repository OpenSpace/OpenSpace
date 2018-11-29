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
#ifndef __OPENSPACE_MODULE_DSN___DATAFILEHELPER___H__
#define __OPENSPACE_MODULE_DSN___DATAFILEHELPER___H__

#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/time.h>


namespace openspace {

    class DataFileHelper {

    public:
        /* Extracts all the mandatory information we need from our asset files */
        static bool checkFileNames(const char* identifier, std::unique_ptr<ghoul::Dictionary> &dictionary, std::vector<std::string> &dataFiles);
        static std::string getDayFromFileName(std::string filename);
        static std::string getHourFromFileName(std::string filename);
        static std::string getMinuteFromFileName(std::string filename);

        static std::vector<double> getDaysFromFileNames(std::vector<std::string> dataFiles);
        static std::vector<double> getHoursFromFileNames(std::vector<std::string> dataFiles);
        static std::vector <double> geMinutesFromFileNames(std::vector<std::string> dataFiles);
        /* Extracts the timestamp from the filename */
        static std::string getFileNameTime(std::string filename, const int FilenameSize);
        /* Extracts the timestamp from a vector of filenames */
        static std::vector<double> extractTriggerTimesFromFileNames(std::vector<std::string> _dataFiles, const int FilenameSize);
        /* Returns an index for our filenames */
        static int findFileIndexForCurrentTime(double time, std::vector<double> vec);
    };
}


#endif 
