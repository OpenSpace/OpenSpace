/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/fieldlinessequence/util/dynamicdownloadermanager.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/timemanager.h>
#include <openspace/json.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr const char* _loggerCat = "DynamicDownloaderManager";
} // namepace

namespace openspace {

// returns seconds between two datasets. If 0 or 1 file in list 0.0 is returned.
double calculateCadence(std::vector<std::pair<std::string, std::string>>& list) {
    // alternative 1: assums cadence between data files are the same, 
    // or that its a sample and doesn't matter to much
        //if (list.size() < 2) {
        //    //if 0 or 1 files there is no cadence
        //    return 0.0;
        //}
        //else {
        //    std::string firstTime = list[0].first;
        //    std::string secondTime = list[1].first;

        //    double time1 = Time::convertTime(firstTime);
        //    double time2 = Time::convertTime(secondTime);
        //    return time2 - time1;
        //}
    
    // alternative 2, avarage
    double averageTime = 0.0;
    if (list.size() < 2) {
        //if 0 or 1 files there is no cadence
        return averageTime;
    }

    double time1 = Time::convertTime(list.begin()->first);
    double timeN = Time::convertTime(list.rbegin()->first);
    averageTime = (timeN - time1) / list.size();
    return averageTime;

    // alternative 3, dynamic
    // TODO: would be alternative 3 which if nessesary dynamic cadence, 
    // to keep track exactely the right time between every two data files
}
std::string formulateHttpRequest(const double time, std::pair<int, std::string> dataID, 
                                                                      std::string baseURL)
{
    // formulate a min and a max time from time
    // The thing is time might be "now" and no items 
    // ISO8601 format: yyyy-mm-ddThh:mm:ssZ
    
    // hour in seconds      : 3600
    // days in seconds      : 86400
    // 30 days in seconds   : 2592000
    // 1 year in seconds    : 31556926
    // TODO: adapt min and max time dependent on the cadence 
    //std::string min = std::string(Time(time - 2592000.0).ISO8601());
    //std::string max = std::string(Time(time + 2592000.0).ISO8601());

    std::string mintemp = "2017-08-02T11:58:54.81";
    std::string maxtemp = "2017-08-31T11:58:54.815";

    std::string minTime = "&time.min=" + mintemp; //min
    std::string maxTime = "&time.max=" + maxtemp; //max
    return baseURL + std::to_string(dataID.first) + minTime + maxTime;
}

DynamicDownloaderManager::DynamicDownloaderManager(int dataID, std::string baseURL){
    FieldlineOption fieldlineOption;

    std::string name = fieldlineOption.optionName(dataID);
    _syncDir = absPath("${CACHE}/web_fieldlines/" + name + "/" + std::to_string(dataID));
    _dataID = { dataID, name };
    _baseURL = baseURL;
}

BigWindow DynamicDownloaderManager::requestBigWindow(std::pair<int, std::string> dataID) {
    // Declaring big window. pair.first is timestep, pair.second is url to be downloaded
    BigWindow bigWindow;

    // Get big window/ list of available files for the specified data set.
    // If it expands to more of a API call rather than a http-request, that code goes here
    // TODO: make_unique!
    HttpMemoryDownload respons(_httpRequest);
    //respons.onProgress([&c = _shouldCancel](int64_t, std::optional<int64_t>) {
    //    return !c;
    //});
    respons.start();
    respons.wait();

    nlohmann::json jsonResult = nlohmann::json::parse(respons.downloadedData());

    /********************
    *   Example respons
    *********************
    * {
    *  "dataID": 1234,
    *  "files": [
    *   {
    *    "timestamp": "2017-07-01 00:42:02.0",
    *    "url": "https://iswa...fieldlines/trace_scs_outtoin/timestamp.osfls"
    *   },
    *   {
    *    "timestamp": "2017-07-01 00:51:36.0",
    *    "url": "https://iswa...fieldlines/trace_scs_outtoin/timestamp.osfls"
    *   }
    *  ],
    *  "time.max": "2017-10-01 00:00:00.0",
    *  "time.min": "2017-06-01 00:00:00.0"
    * }
    ********************
    * note that requested time can be month 10 but last entry in list is month 07,
    * meaning there are no more available files between month 7-10.
    * *****************/

    // make respons into bigWindow._listOfFiles
    //std::filesystem::path httpResponsFile = respons.destination();
    //std::ifstream fileStream(httpResponsFile);

    // iterate through respons list
    // save each element in list from "listOfFiles" as std::pair<std::string, std::string>
    //std::string line;
    //while (std::getline(fileStream, line)) {
    //    if (line == "[" || line == "]" || line == "{" || line == "}") continue;
    //    // first line, dataID
    //    else if (line.substr(0, 9) == "\"dataID\":") {
    //        if (line.substr(11, 4) != std::to_string(_dataID.first)) {
    //            LERROR(fmt::format(
    //                "http request for data id: '{}' resultet in different id: '{}'",
    //                dataID, line.substr(11, 4)
    //            ));
    //        }
    //    }
    //    // second line, declaring the list:
    //    else if (line == "\"files\": [") {
    //        continue;   //actually same as 
    //    }
    //}

    //if (!fileStream.good()) {
    //    //Thing more about what to do if respons is not good
    //    LERROR(fmt::format("Could not open http respons file '{}'", httpResponsFile));
    //}
    // 
    
    //nlohmann::json jsonResult = nlohmann::json::parse(stringResult);
    
    // push_back into listOfFiles
    std::vector<std::pair<std::string, std::string>> listOfFiles;
    for (auto& element : jsonResult["files"]) {
        std::pair<std::string, std::string> data(element["timestamp"], element["url"]);
        listOfFiles.push_back(data);
    }

    // TODO: do something with time.max and time.min if they don't match the request.

    bigWindow._listOfFiles = listOfFiles;
    bigWindow._cadence = calculateCadence(listOfFiles);

    return bigWindow;
}

void DynamicDownloaderManager::update(const double time, const double deltaTime) {
    // TODO figure out how to adapt the speedThreshhold
    // More than 2hrs a second would generally be unfeasable
    // for a regular internet connection to operate at
    int speedThreshhold = 7200;
    if (abs(deltaTime) > speedThreshhold) {
        // to fast, do nothing
        return;
    }

    // TODO: add in if statement: if we are at edge of big window
    // if dataset is changed or has not yet been initialized, update big window
    _httpRequest = formulateHttpRequest(time, _dataID, _baseURL);
    if (_bigWindow._listOfFiles.empty()) {
        _bigWindow = requestBigWindow(_dataID);
    }

    // should create a sliding window which size depends on the current time
    // and cadence (in a perfect world: also on the size of the files, users
    // internet speed etc.)
    // The downloaderWindow should contain the window but also deal with the 
    // downloading of files.
    // _syncDir should maybe get past in because of that
    // 
    // So far we have the big window list, cadence and syncDir
    //


    // here we will make the decisions depending on if sliding window is on edge 
    // of big window, or current time is past big window
    // or more things?


    //if in big window
    // download
    // if finished downloading sliding window, expand it
    //
    //if downloaded file is at, or almost at, edge of sliding window:
    // move sliding window
    // 
    //if sliding window is at edge of big window
    // request new big window
    // 
    //if at now && the whole big window is downloaded
    // do nothing? Maybe every now and then request new big window to see if a new dataset
    // is available
    //

}

} // openspace namespace
