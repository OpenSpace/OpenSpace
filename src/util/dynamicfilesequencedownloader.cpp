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

#include <openspace/util/dynamicfilesequencedownloader.h>
#include <openspace/util/httprequest.h>
#include <openspace/util/timemanager.h>
#include <openspace/json.h>
#include <ghoul/filesystem/filesystem.h>

namespace {
    constexpr const char* _loggerCat = "DynamicFileSequenceDownloader";
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

DynamicFileSequenceDownloader::DynamicFileSequenceDownloader(int dataID,
    const std::string infoURL,
    const std::string dataURL,
    int nOfFilesToQ)
    : _infoURL(infoURL)
    , _dataURL(dataURL)
    , _nOfFilesToQueue(nOfFilesToQ)
{
    FieldlineOption fieldlineOption;

    std::string name = fieldlineOption.optionName(dataID);
    _syncDir = absPath(
        "${SYNC}/dynamically_downloaded/" + std::to_string(dataID)
    );

    _dataID = { dataID, name };
    std::string httpInfoRequest = _infoURL + std::to_string(_dataID.first);
    requestDataInfo(httpInfoRequest);
    std::string httpDataRequest = formulateDataHttpRequest(
        _dataMinTime, _dataMaxTime, _dataID, _dataURL
    );
    requestAvailableFiles(httpDataRequest, _syncDir);
}

void DynamicFileSequenceDownloader::requestDataInfo(std::string httpInfoRequest) {
    HttpMemoryDownload respons(httpInfoRequest);
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
    *        "description" : "WSA 4.4 field line trace from the SCS outer boundary to the
    *                         source surface",
    *        "id" : 1177
    *}
    ********************/

    _dataMinTime = Time::convertTime(jsonResult["availability"]["startDate"]);
    _dataMaxTime = Time::convertTime(jsonResult["availability"]["stopDate"]);
    //_dataIdDescription = jsonResult["description"];
}

void DynamicFileSequenceDownloader::requestAvailableFiles(std::string httpDataRequest,
                                                            std::filesystem::path syncDir)
{
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

    int index = 0;
    for (auto& element : jsonResult["files"]) {
        std::string timestamp = element["timestamp"];
        std::string url = element["url"];

        //timestamp = "2022-11-13T16:14:00.000";
        //url =
        //https://iswaA-webservice1.ccmc.gsfc.nasa.gov/
        //...iswa_data_tree/model/solar/WSA5.X/fieldlines/
        //...GONG_Z/trace_pfss_intoout/2022/11/2022-11-13T16-14-00.000.osfls";

        std::string fileName = url.substr(url.find_last_of("//"));
        std::filesystem::path destination = _syncDir;
        destination += fileName;

        double time = Time::convertTime(timestamp);

        File fileElement;
        fileElement.timestep = timestamp;
        fileElement.time = time;
        fileElement.URL = url;
        fileElement.cadence = 0;
        fileElement.availableIndex = index;
        if (std::filesystem::exists(destination)) {
            if (std::filesystem::file_size(destination) == 0) {
                //TODO flag as failed, maybe do twice, then warn or something, skipping
            }
            fileElement.download = nullptr;
            fileElement.state = File::State::Downloaded;
            _downloadedFiles.push_back(destination);
        }
        else {
            fileElement.download = std::make_unique<HttpFileDownload>(url, destination);
            fileElement.state = File::State::Available;
        }

        _availableData.push_back(std::move(fileElement));
        ++index;
    }
    //maybe sort vector if some data set is order in a different parameter.

    // TODO: do something with time.max and time.min if they don't match the request.
    //_dataMaxTime = jsonResult["time.max"];
    //_dataMinTime = jsonResult["time.min"];

    // TODO Shyam: add accurate cadence to each file here
    _tempCadence = calculateCadence();
    for (File& element : _availableData) {
        element.cadence = _tempCadence;
    }
}

double DynamicFileSequenceDownloader::calculateCadence() {
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

void DynamicFileSequenceDownloader::downloadFile() {
    ZoneScoped;

    if (_filesCurrentlyDownloading.size() < 4 && _queuedFilesToDownload.size() > 0) {
        File* dl = _queuedFilesToDownload.front();
        if (dl->state != File::State::OnQueue) {
            throw ghoul::RuntimeError(
                "Trying to download file from list of queued files,"
                "but its status is not OnQueue"
            );
        }
        _queuedFilesToDownload.erase(_queuedFilesToDownload.begin());
        dl->download->start();
        _filesCurrentlyDownloading.push_back(dl);
        dl->state = File::State::Downloading;
    }
}

void DynamicFileSequenceDownloader::checkForFinishedDownloads() {
    ZoneScoped;

    std::vector<File*>::iterator currentIt = _filesCurrentlyDownloading.begin();

    // since size of filesCurrentlyDownloading can change per iteration, keep size-call
    for (size_t i = 0; i != _filesCurrentlyDownloading.size(); ++i) {

        File* file = *currentIt;
        HttpFileDownload* dl = file->download.get();

        if (dl->hasSucceeded()) {
            //_downloadedFiles.push_back(std::move(*currentIt));
            //std::vector<std::filesystem::path> downloadedFiles;
            _downloadedFiles.push_back(dl->destination());
            file->state = File::State::Downloaded;
            currentIt = _filesCurrentlyDownloading.erase(currentIt);
            // if one is removed, i is reduced, else we'd skip one in the list
            --i;
        }
        else if (dl->hasFailed()) {
            ghoul_assert(!dl->hasFailed(), "downloading of file failed");
            LERROR("TODO, make better handling if file download fails");
        }
        // The file is not finnished downloading, move on to next
        else {
            ++currentIt;
        }

        // Since in the if statement one is removed and else statement it got incremented,
        // check if at end
        if (currentIt == _filesCurrentlyDownloading.end()) {
            return;
        }
    }
}

// negative part of this is it has to go through the whole list.
// Maybe a std::map is better to fetch to most relavent file to download.
std::vector<File>::iterator DynamicFileSequenceDownloader::closestFileToNow(
                                                                        const double time)
{
    ZoneScoped;

    File* closest;
    std::vector<File>::iterator it = _availableData.begin();
    double smallest = DBL_MAX;
    for (File& file : _availableData) {
        // smallest differens determains closest file
        const double differens = abs(time - file.time);
        if (differens < smallest) {
            smallest = differens;
            closest = &file;
        }
    }
    it += closest->availableIndex;
    return it;
    //return *closest;
}

void DynamicFileSequenceDownloader::putOnQueue() {
    ZoneScoped;

    std::vector<File>::iterator end;
    if (_forward) {
        end = _availableData.end();
    }
    else {
        end = _availableData.begin();
        // to catch first file (since begin points to a file but end() does not
        if (_thisFile == end && _thisFile->state == File::State::Available) {
            _queuedFilesToDownload.push_back(&*_thisFile);
            _thisFile->state = File::State::OnQueue;
            return;
        }
    }

    // if forward iterate from now to end. else reverse from now to begin
    int notToMany = 0;
    for (std::vector<File>::iterator it = _thisFile; it != end; _forward ? ++it : --it) {
        if (it->state == File::State::Available) {
            _queuedFilesToDownload.push_back(&*it);
            it->state = File::State::OnQueue;
        }
        ++notToMany;
        // exit out early if enough files are queued / already downloaded
        if (notToMany == _nOfFilesToQueue) break;
    }
}

void DynamicFileSequenceDownloader::update(const double time, const double deltaTime) {
    ZoneScoped;
    // First frame cannot guarantee time and deltatime has been set yet.
    if (_firstFrame) {
        _firstFrame = false;
        return;
    }
    if (_secondFrame) {
        _secondFrame = false;
        _thisFile = closestFileToNow(time);
        return;
    }
    // TODO figure out how to adapt the speedThreshhold
    // More than 2hrs a second would generally be unfeasable
    // for a regular internet connection to operate at
    int speedThreshhold = 7200;
    if (abs(deltaTime) > speedThreshhold) {
        // to fast, do nothing
        LWARNING("Dynamic file sequence downloader: Paused. Time moves to fast.");
        return;
    }

    // if delta time direction got changed
    if (_forward && deltaTime < 0 || !_forward && deltaTime > 0) {
        _forward = !_forward;
        _queuedFilesToDownload.clear();
    }

    if (_forward && _thisFile != _availableData.end()) {
        // if files are there and time is between next file (+1) and +2 file
        // (meaning the this file is active from now till next file)
        // change this to be next
        if (_thisFile + 1 != _availableData.end() &&
            _thisFile + 2 != _availableData.end() &&
            //_thisFile->time + _thisFile->cadence > time &&
            (_thisFile + 1)->time < time &&
            (_thisFile + 2)->time > time)
        {
            ++_thisFile;
        }
        // if its beyond the +2 file, arguably that can mean delta time is to fast
        // and files might be missed. But we also know we went past beyond the next so
        // we no longer know where we are so we reinitialize
        else if (_thisFile + 1 != _availableData.end() &&
                 _thisFile + 2 != _availableData.end() &&
                (_thisFile + 2)->time < time)
        {
            _thisFile = closestFileToNow(time);
        }
    }
    else if (!_forward && _thisFile != _availableData.begin()) {
        // if file is there and time is between prev and this file
        // then change this to be prev. Same goes here as if time is moving forward
        // we will use forward 'usage', meaning file is active from now till next
        if (_thisFile - 1 != _availableData.begin() &&
            (_thisFile)->time < time &&
            (_thisFile - 1)->time > time)
        //_thisFile->time + _thisFile->cadence < time &&
        {
            --_thisFile;
        }
        // if we are beyond the prev file, again delta time might be to fast, but we
        // no longer know where we are so we reinitialize
        else if (_thisFile - 1 != _availableData.begin() &&
                (_thisFile - 1)->time > time)
        {
            _thisFile = closestFileToNow(time);
        }
    }

    if (_downloadedFiles.size() > 20) {
        // cache files.
        // But because we now already save the files (not using HttpMemoryDownload),
        // this might not be needed
        // Maybe instead we should check if we have to many cached files and remove them
    }

    if (_filesCurrentlyDownloading.size() > 0) {
        checkForFinishedDownloads();
    }

    putOnQueue();

    downloadFile();

// The todo list: //
//
//     TBD if we should implement this or not.
// 2. OnChange functions for if a different dataID is selected - reinitialize things
//     note: can there be the same id for for different models other than wsa.
//     If that is the case, it wouldnt make sense to change id, and therefor dataset,
//     for the same renderable. If they are all the same model, it could work.
//
// 3. Rename folder for where files are being downloaded to
//     note: same as above:
//     if same id for diffrent models: then include modelname in path
//     else: Only rename after what new name for dynamic downloader is
//
// 4. Move class into a different module + rename class
//
// 5. recall data info every now and then to get new files
//
// 6. ultimet test: test with different data
//
// 9. maybe make a copy of the once that gets returned with downloadedFiles() so that
//     the originals can be removed with clearDownloaded() before returning out of
//     downloadedFiles()
// 10. deal with jsonResult["description"]
//
// Done items:
//
// 7. optamize the closestFileToNow function
//
// 8. tracy
//
// 1. When initializing add the already cached files to the _availableData
// list as downloaded
//
//


}
void DynamicFileSequenceDownloader::clearDownloaded() {
    _downloadedFiles.clear();
}

//reference or no reference? return path to where they are instead?
const std::vector<std::filesystem::path>&
DynamicFileSequenceDownloader::downloadedFiles()
{
    return _downloadedFiles;
    // Maybe do...
    // temp = _downloadedFiles
    // _downloadedFiles.clear()
    // return temp;
    // ... to make sure not the same file gets loaded as state in renderable?
}

} // openspace namespace
