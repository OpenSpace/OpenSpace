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

std::string formulateDataHttpRequest(double minTime, double maxTime, int dataID,
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

    std::string minText = "&time.min=" + min;
    std::string maxText = "&time.max=" + max;

    return baseURL + std::to_string(dataID) + minText + maxText;
}

DynamicFileSequenceDownloader::DynamicFileSequenceDownloader(int dataID,
    const std::string infoURL,
    const std::string dataURL,
    size_t nOfFilesToQ)
    : _infoURL(infoURL)
    , _dataURL(dataURL)
    , _nOfFilesToQueue(nOfFilesToQ)
{
    _syncDir = absPath(
        "${SYNC}/dynamically_downloaded/" + std::to_string(dataID)
    );

    _dataID = dataID;
    std::string httpInfoRequest = _infoURL + std::to_string(_dataID);
    requestDataInfo(httpInfoRequest);
    std::string httpDataRequest = formulateDataHttpRequest(
        _dataMinTime, _dataMaxTime, _dataID, _dataURL
    );
    requestAvailableFiles(httpDataRequest, _syncDir);
}

void DynamicFileSequenceDownloader::deinitialize(bool cacheFiles) {
    std::vector<openspace::File*>& currentlyDownloadingFiles =
        filesCurrentlyDownloading();
    for (auto file : currentlyDownloadingFiles) {
        file->download->cancel();

        std::error_code ec;
        std::filesystem::remove(file->path, ec);
        if (ec) {
            LERROR(std::format("Failed to delete unfinished file: {}", file->path));
        }
        else {
            LINFO(std::format("Removing unfinished download:: {}", file->path));
        }
    }
    if (!cacheFiles) {
        if (!std::filesystem::exists(_syncDir)) {
            return;
        }
        for (auto& file : std::filesystem::directory_iterator(_syncDir)) {
            std::error_code ec;
            std::filesystem::remove(file.path(), ec);
            if (ec) {
                LERROR(std::format("Failed to delete file: {}", file.path()));
            }
        }
    }
}

void DynamicFileSequenceDownloader::requestDataInfo(std::string httpInfoRequest) {
    HttpMemoryDownload response(httpInfoRequest);
    response.start();
    response.wait();

    bool success = false;
    int attempt = 0;
    const int maxRetries = 1;
    nlohmann::json jsonResult;

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
    while (attempt <= maxRetries && !success) {
        try {
            auto responseText = response.downloadedData();
            if (responseText.empty()) {
                throw std::runtime_error("Empty HTTP response");
            }

            jsonResult = nlohmann::json::parse(responseText);
            success = true;
            _dataMinTime = Time::convertTime(
                jsonResult["availability"]["startDate"].get<std::string>()
            );
            _dataMaxTime = Time::convertTime(
                jsonResult["availability"]["stopDate"].get<std::string>()
            );
            //_dataIdDescription = jsonResult["description"];
        }
        catch (nlohmann::json::parse_error& e) {
            LWARNING(std::format("JSON parse error: {}", e.what()));
        }
        catch (std::exception& e) {
            LWARNING(std::format("HTTP or other error: {}", e.what()));
        }

        if (!success) {
            if (attempt < maxRetries) {
                LINFO(std::format("Retry nr {}.", attempt+1));
                std::this_thread::sleep_for(std::chrono::seconds(2));

                response.start();
                response.wait();
            }
            else {
                LERROR(std::format(
                    "Failed according to warning above with http request of url: {}",
                    httpInfoRequest
                ));
            }
        }
        attempt++;
    }
}

void DynamicFileSequenceDownloader::requestAvailableFiles(std::string httpDataRequest,
                                                            std::filesystem::path syncDir)
{
    // Declaring big window. pair.first is timestep, pair.second is url to be downloaded

    // Get big window/ list of available files for the specified data set.
    // If it expands to more of a API call rather than a http-request, that code goes here
    // TODO: make_unique?
    HttpMemoryDownload response(httpDataRequest);
    response.start();
    response.wait();

    bool success = false;
    int attempt = 0;
    const int maxRetries = 1;
    nlohmann::json jsonResult;

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
    while (attempt <= maxRetries && !success) {
        try {
            const std::vector<char>& data = response.downloadedData();
            if (data.empty()) {
                throw std::runtime_error("Empty HTTP response");
            }
            //TODO what value is actually to large to handle?
            if (data.size() > std::numeric_limits<std::size_t>::max()) {
                throw std::runtime_error(
                    "Http responds with list of available files to large"
                );
            }

            jsonResult = nlohmann::json::parse(data);
            success = true;
        }
        catch (const nlohmann::json::parse_error& ex) {
            LERROR(std::format("JSON parsing error: '{}'", ex.what()));
        }
        catch (const std::bad_alloc& ex) {
            LERROR(std::format(
                "Memory allocation error while parsing JSON: '{}'",
                ex.what()
            ));
        }
        catch (const std::exception& ex) {
            LERROR(std::format("An error occurred: '{}'", ex.what()));
        }

        if (!success) {
            if (attempt < maxRetries) {
                LINFO(std::format("Retry nr {}.", attempt + 1));
                std::this_thread::sleep_for(std::chrono::seconds(1));

                response.start();
                response.wait();
            }
            else {
                LERROR(std::format(
                    "Failed according to warning above with http request of url: {}",
                    httpDataRequest
                ));
            }
        }
        attempt++;
    }

    if (!success) {
        return;
    }

    int index = 0;
    for (auto& element : jsonResult["files"]) {
        std::string timestamp = element["timestamp"].get<std::string>();
        std::string url = element["url"].get<std::string>();

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
        fileElement.path = destination;
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

    _tempCadence = calculateCadence();
    for (File& element : _availableData) {
        element.cadence = _tempCadence;
    }
}

double DynamicFileSequenceDownloader::calculateCadence() {
    double averageTime = 0.0;
    if (_availableData.size() < 2) {
        // If 0 or 1 files there is no cadence
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
        if (dl->download != nullptr) {
            dl->download->start();
            _filesCurrentlyDownloading.push_back(dl);
            dl->state = File::State::Downloading;
            _queuedFilesToDownload.erase(_queuedFilesToDownload.begin());
        }
    }
}

void DynamicFileSequenceDownloader::checkForFinishedDownloads() {
    ZoneScoped;

    std::vector<File*>::iterator currentIt = _filesCurrentlyDownloading.begin();

    // Since size of filesCurrentlyDownloading can change per iteration, keep size-call
    for (size_t i = 0; i != _filesCurrentlyDownloading.size(); ++i) {

        File* file = *currentIt;
        HttpFileDownload* dl = file->download.get();

        if (dl->hasSucceeded()) {
            std::ifstream tempFile(file->URL);
            std::streampos size = tempFile.tellg();
            if (size == 0){
                LERROR(std::format("File '{}' was 0kb, removing", dl->destination()));
                currentIt = _filesCurrentlyDownloading.erase(currentIt);
                --i;
            }
            else {
                _downloadedFiles.push_back(dl->destination());
                file->state = File::State::Downloaded;
                currentIt = _filesCurrentlyDownloading.erase(currentIt);
                // if one is removed, i is reduced, else we'd skip one in the list
                --i;
            }
        }
        else if (dl->hasFailed()) {
            LERROR(std::format("File '{}' failed to download. Removing file", file->URL));
            std::string filename;
            size_t lastSlash = file->URL.find_last_of('/');
            if (lastSlash == std::string::npos || lastSlash + 1 >= file->URL.size()) {
                filename = "";
            }
            else {
                filename = file->URL.substr(lastSlash + 1);
            }

            std::filesystem::path filepath = std::filesystem::path(_syncDir) / filename;
            if (std::filesystem::exists(filepath)) {
                std::error_code ec;
                std::filesystem::remove(filepath, ec);
                if (ec) {
                    LERROR(std::format("Failed to delete file: {}", filepath));
                }
                else {
                    LINFO(std::format("Deleted file after failed download: {}", filepath));
                }
            }

            currentIt = _filesCurrentlyDownloading.erase(currentIt);
            // If one is removed, i is reduced, else we'd skip one in the list
            --i;
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

std::vector<File>::iterator DynamicFileSequenceDownloader::closestFileToNow(
                                                                        const double time)
{
    ZoneScoped;
    std::vector<File>::iterator closestIt = _availableData.begin();
    double smallest = DBL_MAX;

    std::vector<File>::iterator it = std::lower_bound(
        _availableData.begin(),
        _availableData.end(),
        time,
        [](const File& file, double t) {
            return file.time < t;
        }
    );

    if (it == _availableData.end()) {
        return std::prev(_availableData.end());
    }
    if (it == _availableData.begin()) {
        return it;
    }
    std::vector<File>::iterator prev = std::prev(it);
    if (std::abs(prev->time - time) <= std::abs(it->time - time)) {
        return prev;
    }
    else {
        return it;
    }
}

void DynamicFileSequenceDownloader::putOnQueue() {
    ZoneScoped;

    std::vector<File>::iterator end;
    if (_forward) {
        end = _availableData.end();
    }
    else {
        end = _availableData.begin();
        // To catch first file (since begin points to a file but end() does not
        if (_thisFile == end && _thisFile->state == File::State::Available) {
            _queuedFilesToDownload.push_back(&*_thisFile);
            _thisFile->state = File::State::OnQueue;
            return;
        }
    }

    // If forward iterate from now to end. else reverse from now to begin
    size_t notToMany = 0;
    for (std::vector<File>::iterator it = _thisFile; it != end; _forward ? ++it : --it) {
        if (it->state == File::State::Available) {
            _queuedFilesToDownload.push_back(&*it);
            it->state = File::State::OnQueue;
        }
        ++notToMany;
        // exit out early if enough files are queued / already downloaded
        if (notToMany == _nOfFilesToQueue) {
            break;
        }
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
        if (_availableData.size() != 0) {
            _secondFrame = false;
            _thisFile = closestFileToNow(time);
        }
        return;
    }
    // More than 2hrs a second would generally be unfeasable
    // for a regular internet connection to operate at
    int speedThreshhold = 7200;
    if (abs(deltaTime) > speedThreshhold) {
        // to fast, do nothing. This is not optimal since it prints a lot
        LWARNING("Dynamic file sequence downloader: Paused. Time moves to fast.");
        return;
    }

    // if delta time direction got changed
    if (_forward && deltaTime < 0 || !_forward && deltaTime > 0) {
        _forward = !_forward;
        // Remove from queue when time changed, to start downloading most relevant files
        for (auto file : _queuedFilesToDownload) {
            file->state = File::State::Available;
        }
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
        // We've jumped back without interpolating to a previous time step,
        // past circa 2 files worth of time and without changing delta time
        // >>>>>>>we jumped to here>>>>>>>>>now>>>>>>>
        else if (_thisFile->time - 2 * _thisFile->cadence > time) {
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
        // We've jumped forward without interpolating to a future time step,
        // past circa 2 files worth of time and without changing delta time
        // <<<<<<now<<<<<<<<<we jumped to here<<<<<<<
        else if (_thisFile->time - 2 * _thisFile->cadence < time) {
            _thisFile = closestFileToNow(time);
        }
    }
    else if (_thisFile->download == nullptr && !_availableData.empty()) {
        _thisFile = closestFileToNow(time);
    }

    if (_filesCurrentlyDownloading.size() > 0) {
        checkForFinishedDownloads();
    }

    putOnQueue();
    downloadFile();
}

std::filesystem::path DynamicFileSequenceDownloader::destinationDirectory() {
    return _syncDir;
}

void DynamicFileSequenceDownloader::clearDownloaded() {
    _downloadedFiles.clear();
}
bool DynamicFileSequenceDownloader::areFilesCurrentlyDownloading() {
    return _filesCurrentlyDownloading.size() != 0;
}

std::vector<File*>& DynamicFileSequenceDownloader::filesCurrentlyDownloading() {
    return _filesCurrentlyDownloading;
}

const std::vector<std::filesystem::path>&
DynamicFileSequenceDownloader::downloadedFiles()
{
    return _downloadedFiles;
}

} // openspace namespace
