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

std::string formulateDataHttpRequest(double minTime, double maxTime,
                                                       std::pair<int, std::string> dataID,
                                                                const std::string baseURL)
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

    std::string min = std::string(Time(minTime).ISO8601());
    std::string max = std::string(Time(maxTime).ISO8601());

    std::string minText = "&time.min=" + min; //min
    std::string maxText = "&time.max=" + max; //max


    return baseURL + std::to_string(dataID.first) + minText + maxText;
}

//// Returns first trigger of window
//double DynamicDownloaderManager::windowStart() {
//    return a windows .front().first;
//}

//// Returns last trigger of window
//double DynamicDownloaderManager::windowEnd() {
//    return a windows .back().first;
//}

DynamicDownloaderManager::DynamicDownloaderManager(int dataID, const std::string baseURL)
    : _baseURL(baseURL)
{
    FieldlineOption fieldlineOption;

    std::string name = fieldlineOption.optionName(dataID);
    _syncDir = absPath(
        "${CACHE}/dynamic_downloaded_fieldlines/" + name + "/" + std::to_string(dataID)
    );
    _dataID = { dataID, name };
    std::string httpInfoRequest = _baseURL + std::to_string(_dataID.first);
    requestDataInfo(httpInfoRequest);
    std::string httpDataRequest = formulateDataHttpRequest(
        _dataMinTime, _dataMaxTime, _dataID, _baseURL
    );
    requestAvailableFiles(httpDataRequest);

}

void DynamicDownloaderManager::requestDataInfo(std::string httpInforRequest) {
    HttpMemoryDownload respons(httpInforRequest);
    respons.start();
    respons.wait();

    nlohmann::json jsonResult = nlohmann::json::parse(respons.downloadedData());

    /********************
    *   Example respons
    *********************
    *{
    *    "availability": {
    *        "startDate": "2017-07-01T00:42:02.0Z",
    *        "stopDate" : "2017-09-30T22:43:18.0Z"
    *    },
    *        "description" : "WSA 4.4 field line trace from the SCS outer boundary to the source surface",
    *        "id" : 1177
    *}
    ********************/

    _dataMinTime = jsonResult["availability"]["startDate"];
    _dataMaxTime = jsonResult["availability"]["stopDate"];
    _dataIdDescription = jsonResult["description"];

}

void DynamicDownloaderManager::requestAvailableFiles(std::string httpDataRequest) {
    // Declaring big window. pair.first is timestep, pair.second is url to be downloaded

    // Get big window/ list of available files for the specified data set.
    // If it expands to more of a API call rather than a http-request, that code goes here
    // TODO: make_unique!
    HttpMemoryDownload respons(httpDataRequest);

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

    // push_back into listOfFiles
    std::vector<std::pair<std::string, std::string>> listOfFiles;
    int index = 0;
    for (auto& element : jsonResult["files"]) {
        std::pair<std::string, std::string> data(element["timestamp"], element["url"]);
        listOfFiles.push_back(data);

        std::unique_ptr<HttpFileDownload> downloader =
            std::make_unique<HttpFileDownload>(data.second, _syncDir);
        _availableFiles.push_back(std::move(downloader));

        File fileElement;
        fileElement.download = downloader.get();
        fileElement.state = File::State::Available;
        fileElement.timestep = data.first;
        fileElement.URL = data.second;
        fileElement.cadence = 0;
        fileElement.availableIndex = index;
        _availableData.push_back(fileElement);
        ++index;
    }
    //maybe sort vector if some data set is order in a different parameter.

    // TODO: do something with time.max and time.min if they don't match the request.
    _dataMaxTime = jsonResult["time.max"];
    _dataMinTime = jsonResult["time.min"];

    //bigWindow.listOfFiles = listOfFiles;
    //bigWindow.cadence = calculateCadence(listOfFiles);
    // TODO Shyam: add accurate cadence to each file here
    _tempCadence = calculateCadence();
    for (auto& element : _availableData) {
        element.cadence = _tempCadence;
    }

}

double DynamicDownloaderManager::calculateCadence() {
    double averageTime = 0.0;
    if (_availableData.size() < 2) {
        //if 0 or 1 files there is no cadence
        return averageTime;
    }

    double time1 = Time::convertTime(_availableData.begin()->timestep);
    double timeN = Time::convertTime(_availableData.rbegin()->timestep);
    averageTime = (timeN - time1) / _availableData.size();

    return averageTime;
}

void DynamicDownloaderManager::putOnQueue(std::unique_ptr<HttpFileDownload>& file,
                                                                     const int prioNumber)
{
    _queuedFilesToDownload.push(std::move(file));
}

void DynamicDownloaderManager::putOnQueue(std::string urlToDownload) {
    //// Alternative 1 dont save file
    //HttpMemoryDownload downloading = HttpMemoryDownload(fileInfo.second);
    //downloading.start();

    // Alternative 2 save file
    std::unique_ptr<HttpFileDownload> downloader =
        std::make_unique<HttpFileDownload>(urlToDownload, _syncDir);
    _queuedFilesToDownload.push(std::move(downloader));
}

void
DynamicDownloaderManager::prioritizeQueue(const double& time, const double& deltaTime) {
    using Unq = std::unique_ptr<HttpFileDownload>;
    using Q = std::priority_queue<Unq>;
    using Vec = std::vector<Unq>;
    using VecIt = Vec::iterator;
    // Delta time changed direction and priory of queue is not correct anymore
    // Emptying queue
    _queuedFilesToDownload = std::priority_queue<std::unique_ptr<HttpFileDownload>>();

    File closestFileToNow = DynamicDownloaderManager::closestFileToNow(time);
    // priority 0 is highest. Higher number is lower priority.
    int prioNumber = 0;
    if (_forward) {
        for (std::vector<File>::iterator it =
                _availableData.begin() + closestFileToNow.availableIndex;
            it == _availableData.end() ;
            ++it)
        {
            //file in availableFiles at index i
            // moved to q with prio prioNumber
            putOnQueue(*it.download, prionumber);
            // increase prioNumber
            //
        }
        for (std::vector<File>::iterator it =
                _availableData.begin() + closestFileToNow.availableIndex - 1;
            it == _availableData.begin() ;
            --it)
        {
            putOnQueue(*it.download, prionumber);
        }

    }
    else {
        for (std::vector<File>::iterator it =
            _availableData.begin() + closestFileToNow.availableIndex;
            it == _availableData.begin();
            --it)
        {
            //file in availableFiles at index i
            // moved to q with prio prioNumber
            putOnQueue(*it.download, prionumber);
            // increase prioNumber
            //
        }
        for (std::vector<File>::iterator it =
            _availableData.begin() + closestFileToNow.availableIndex - 1;
            it == _availableData.end();
            ++it)
        {
            putOnQueue(*it.download, prionumber);
        }
    }



}

void DynamicDownloaderManager::downloadFile(){
    if (_filesCurrentlyDownloading.size() < 4) {
        // there is no way to traverse
        // priority_queue's content without erasing the queue.
        HttpFileDownload* dl = _queuedFilesToDownload.top().get();
        dl->start();
        _filesCurrentlyDownloading.push_back(std::move(_queuedFilesToDownload.top()));
        // after doing the push_back(move), do I still need to pop to remove from queue?
        //_queuedFilesToDownload.pop();
    }
}

void DynamicDownloaderManager::checkForFinishedDownloads() {
    std::vector<std::unique_ptr<HttpFileDownload>>::iterator currentIt =
        _filesCurrentlyDownloading.begin();
    while (currentIt != _filesCurrentlyDownloading.end()) {
        HttpFileDownload* dl = currentIt->get();

        if (dl->hasSucceeded()) {
            //_downloadedFiles.push_back(std::move(*currentIt));
            //std::vector<std::filesystem::path> downloadedFiles;
            _downloadedFiles.push_back(dl->destination());

            currentIt->reset();
            //currentIt = _filesCurrentlyDownloading.erase(currentIt);

        }
        else if (dl->hasFailed()) {
            ghoul_assert(!dl->hasFailed(), "downloading of file failed");
            LERROR("TODO, make better handling if file download fails");
        }
        else {
            ++currentIt;
        }
    }
}

// negative part of this is it has to go through the whole list.
// Maybe a std::map is better to fetch to most relavent file to download.
File& DynamicDownloaderManager::closestFileToNow(const double time) {
    File* closest;
    double smallest = DBL_MAX;
    for (File& file : _availableData) {
        const double fileTime = Time::convertTime(file.timestep);
        // smallest differens determains closest file
        const double differens = abs(time - fileTime);
        if (differens < smallest) {
            smallest = differens;
            closest = &file;
        }
    }
    return *closest;
}

void DynamicDownloaderManager::update(const double time, const double deltaTime) {
    // First frame cannot guarantee time and deltatime has been set yet.
    if (_firstFrame) {
        _firstFrame = false;
        return;
    }
    // TODO figure out how to adapt the speedThreshhold
    // More than 2hrs a second would generally be unfeasable
    // for a regular internet connection to operate at
    int speedThreshhold = 7200;
    if (abs(deltaTime) > speedThreshhold) {
        // to fast, do nothing
        return;
    }

    if (_availableFiles.empty()) {
        // TODO: add in if statement: if we are at edge of big window
        // if dataset is changed or has not yet been initialized, update big window
        // Also TODO: request a bigwindow in constructur to begin with.
        //_httpInfoRequest = _baseURL + std::to_string(_dataID.first);
        std::string httpInfoRequest = _baseURL + std::to_string(_dataID.first);
        requestDataInfo(httpInfoRequest);
        std::string httpDataRequest = formulateDataHttpRequest(
            _dataMinTime, _dataMaxTime, _dataID, _baseURL
        );
        requestAvailableFiles(httpDataRequest);
    }

    if (_downloadedFiles.size() > 20) {
        // cache files.
        // But because we now already save the files (not using HttpMemoryDownload),
        // this might not be needed
        // Maybe instead we should check if we have to many cached files and remove them
    }

    checkForFinishedDownloads();

    // We should still check for finishedDownloads even if direction of time changed
    // therefore, this is done after checkForFinishedDownloads()
    if (_forward && deltaTime < 0 || !_forward && deltaTime > 0) {
        _forward == true ? _forward = false : _forward = true;
        // if std::queue :clear(_queuedFilesToDownload);
        // if priority q:
        _queueIsPrioritized = false;
    }

    // here we will make the decisions depending on if sliding window is on edge
    // of big window, or current time is past big window
    // or more things?


     //std::pair<std::string, std::string> mostRelaventFileToDownload =
     //    DynamicDownloaderManager::findMostRelevantFileToDownload(time);
     //putOnQueue(mostRelaventFileToDownload.second);

    if (!_queueIsPrioritized) {

        prioritizeQueue(time, deltaTime);
        _queueIsPrioritized = true;
    }

    downloadFile();


    // if in big window
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
void DynamicDownloaderManager::clearDownloaded() {
    _downloadedFiles.clear();
}

//reference or no reference? return path to where they are instead?
const std::vector<std::filesystem::path>&
DynamicDownloaderManager::downloadedFiles()
{
    return _downloadedFiles;
    // Maybe do...
    // temp = _downloadedFiles
    // _downloadedFiles.clear()
    // return temp;
    // ... to make sure not the same file gets loaded as state in renderable?
}

} // openspace namespace
