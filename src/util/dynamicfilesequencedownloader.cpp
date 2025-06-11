/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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
#include <openspace/json.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/stringhelper.h>
#include <unordered_set>

namespace {
    constexpr const std::string_view _loggerCat = "DynamicFileSequenceDownloader";
} // namepace

namespace openspace {
void trackFinishedDownloads(const std::filesystem::path& syncFilePath,
                            const std::filesystem::path& newFilePath)
{
    std::unordered_set<std::string> existingEntries;
    std::ifstream inFile = std::ifstream(syncFilePath);
    std::string line;

    // load existing entries
    while (ghoul::getline(inFile, line)) {
        if (!line.empty()) {
            existingEntries.insert(std::filesystem::path(line).filename().string());
        }
    }
    inFile.close();

    const std::string fileName = newFilePath.filename().string();
    const std::string filePath = newFilePath.string();

    if (!existingEntries.contains(fileName)) {
        std::ofstream outFile = std::ofstream(syncFilePath, std::ios::app);
        if (outFile.is_open()) {
            outFile << filePath << std::endl;
        }
    }
}

std::string buildDataHttpRequest(double minTime, double maxTime, int dataID,
                                 const std::string& baseUrl)
{
    // formulate a min and a max time from time
    // The thing is time might be "now" and no items
    // ISO8601 format: yyyy-mm-ddThh:mm:ssZ

    // hour in seconds      : 3600
    // days in seconds      : 86400
    // 30 days in seconds   : 2592000
    // 1 year in seconds    : 31556926
    std::string_view min = Time(minTime).ISO8601();
    std::string_view max = Time(maxTime).ISO8601();

    return std::format(
        "{}{}&time.min={}&time.max={}", baseUrl, dataID, min, max
    );
}

DynamicFileSequenceDownloader::DynamicFileSequenceDownloader(int dataID,
                                                            const std::string& identifier,
                                                            std::string infoUrl,
                                                            std::string dataUrl,
                                                            size_t nOfFilesToQueue)
    : _dataID(dataID)
    , _infoUrl(std::move(infoUrl))
    , _dataUrl(std::move(dataUrl))
    , _nFilesToQueue(nOfFilesToQueue)
{
    _syncDir = absPath(
        std::format("${{SYNC}}/dynamically_downloaded/{}/{}", dataID, identifier)
    );

    std::filesystem::path syncFile = _syncDir / std::format("{}.synced", dataID);
    _trackSynced = syncFile;
    // Just to create the folder
    std::filesystem::path folder = _trackSynced.parent_path();
    std::filesystem::create_directories(folder);
    // Just to create the file
    {
        std::ofstream file(_trackSynced, std::ios::app);
    }
    // Delete the files in the folder whos file name is not in the _trackSynced file
    std::unordered_set<std::string> keepFiles;
    std::ifstream listFile = std::ifstream(_trackSynced);
    std::string filename;
    while (ghoul::getline(listFile, filename)) {
        if (!filename.empty()) {
            keepFiles.insert(std::filesystem::path(filename).filename().string());
        }
    }
    for (const auto& entry : std::filesystem::directory_iterator(folder)) {
        if (!entry.is_regular_file()) {
            continue;
        }
        std::string name = entry.path().filename().string();
        if (name != _trackSynced.filename().string() &&
            !keepFiles.contains(name))
        {
            std::filesystem::remove(entry.path());
        }
    }

    std::string httpInfoRequest = _infoUrl + std::to_string(_dataID);
    requestDataInfo(httpInfoRequest);
    std::string httpDataRequest = buildDataHttpRequest(
        _dataMinTime,
        _dataMaxTime,
        _dataID,
        _dataUrl
    );
    requestAvailableFiles(httpDataRequest, _syncDir);
}

void DynamicFileSequenceDownloader::deinitialize(bool cacheFiles) {
    const std::vector<File*>& currentlyDownloadingFiles = filesCurrentlyDownloading();
    for (File* file : currentlyDownloadingFiles) {
        file->download->cancel();
        file->download->wait();
        std::error_code ec;
        std::filesystem::remove(file->path, ec);
        if (ec) {
            LERROR(std::format(
                "Failed to delete unfinished file '{}'. {}", file->path, ec.message()
            ));
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
                LERROR(std::format(
                    "Failed to delete file '{}'. {}", file.path(), ec.message()
                ));
            }
        }
    }
    if (std::filesystem::is_empty(_syncDir)) {
        std::filesystem::remove(_syncDir);
    }
}

void DynamicFileSequenceDownloader::requestDataInfo(std::string httpInfoRequest) {
    HttpMemoryDownload response = HttpMemoryDownload(httpInfoRequest);
    response.start();
    response.wait();

    bool success = false;
    int attempt = 0;
    constexpr int MaxRetries = 1;

    /********************
    * Example response
    *********************
    * {
    *    "availability": {
    *        "startDate": "2017-07-01T00:42:02.0Z",
    *        "stopDate" : "2017-09-30T22:43:18.0Z"
    *    },
    *        "description" : "WSA 4.4 field line trace from the SCS outer boundary to the
    *                         source surface",
    *        "id" : 1177
    * }
    */
    while (attempt <= MaxRetries && !success) {
        try {
            std::vector<char> responseText = response.downloadedData();
            if (responseText.empty()) {
                throw ghoul::RuntimeError("Empty HTTP response");
            }
            nlohmann::json jsonResult = nlohmann::json::parse(responseText);
            success = true;
            _dataMinTime = Time::convertTime(
                jsonResult["availability"]["startDate"].get<std::string>()
            );
            _dataMaxTime = Time::convertTime(
                jsonResult["availability"]["stopDate"].get<std::string>()
            );
        }
        catch (const nlohmann::json::parse_error& e) {
            LWARNING(std::format("JSON parse error: {}", e.what()));
        }

        if (!success) {
            if (attempt < MaxRetries) {
                LINFO(std::format("Retry number {}.", attempt + 1));
                std::this_thread::sleep_for(std::chrono::seconds(2));
                response.start();
                response.wait();
            }
            else {
                LERROR(std::format(
                    "Failed according to warning above with HTTP request of URL: {}",
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
    // If it expands to more of a API call rather than a http-request, that code goes here
    HttpMemoryDownload response = HttpMemoryDownload(httpDataRequest);
    response.start();
    response.wait();

    bool success = false;
    int attempt = 0;
    constexpr int MaxRetries = 1;
    nlohmann::json jsonResult;

    /********************
    * Example response
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
    *
    * Note that requested time can be month 10 but last entry in list is month 07,
    * meaning there are no more available files between month 7-10.
    * *****************/
    while (attempt <= MaxRetries && !success) {
        try {
            std::vector<char> data = response.downloadedData();
            if (data.empty()) {
                throw ghoul::RuntimeError("Empty HTTP response");
            }
            // @TODO (2025-06-10, Elon) What value is actually too large to handle?
            if (data.size() > std::numeric_limits<std::size_t>::max()) {
                throw ghoul::RuntimeError(
                    "Http response with list of available files is too large, i.e. too "
                    "many files"
                );
            }

            jsonResult = nlohmann::json::parse(data);
            success = true;
        }
        catch (const nlohmann::json::parse_error& ex) {
            LERROR(std::format("JSON parsing error: '{}'", ex.what()));
        }

        if (!success) {
            if (attempt < MaxRetries) {
                LINFO(std::format("Retry nr {}.", attempt + 1));
                std::this_thread::sleep_for(std::chrono::seconds(1));

                response.start();
                response.wait();
            }
            else {
                LERROR(std::format(
                    "Failed according to warning above with HTTP request of URL: {}",
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
    for (const nlohmann::json& element : jsonResult["files"]) {
        std::string timestamp = element["timestamp"].get<std::string>();
        std::string url = element["url"].get<std::string>();

        // An example of how one element in the list from the JSON-result look like:
        // timestamp = "2022-11-13T16:14:00.000";
        // url =
        // https://iswaA-webservice1.ccmc.gsfc.nasa.gov/
        // ...iswa_data_tree/model/solar/WSA5.X/fieldlines/
        // ...GONG_Z/trace_pfss_intoout/2022/11/2022-11-13T16-14-00.000.osfls";

        std::string fileName = url.substr(url.find_last_of("//"));
        std::filesystem::path destination = _syncDir;
        destination += fileName;

        double time = Time::convertTime(timestamp);
        File fileElement = {
            .timestep = timestamp,
            .time = time,
            .URL = url,
            .path = destination,
            .cadence = 0,
            .availableIndex = index
        };
        if (std::filesystem::exists(destination)) {
            fileElement.download = nullptr;
            fileElement.state = File::State::Downloaded;
            _downloadedFiles.push_back(destination);
            trackFinishedDownloads(_trackSynced, destination);
        }
        else {
            fileElement.download = std::make_unique<HttpFileDownload>(url, destination);
            fileElement.state = File::State::Available;
        }
        _availableData.push_back(std::move(fileElement));
        ++index;
    }

    const double cadence = calculateCadence();
    for (File& element : _availableData) {
        element.cadence = cadence;
    }
}

double DynamicFileSequenceDownloader::calculateCadence() const {
    double averageTime = 0.0;

    if (_availableData.size() < 2) {
        // If 0 or 1 files there is no cadence
        return averageTime;
    }
    const double time1 = Time::convertTime(_availableData.begin()->timestep);
    const double timeN = Time::convertTime(_availableData.rbegin()->timestep);
    averageTime = (timeN - time1) / _availableData.size();

    return averageTime;
}

void DynamicFileSequenceDownloader::downloadFile() {
    ZoneScoped;

    // The MaxDownloads number is a educated guess. Ideally this would vary depending on
    // each users computer and internet specifications. It allows at least a few files to
    // download in parallel, but not too many to take up more bandwidth than it would
    // allow the current files to download as fast as possible
    constexpr int MaxDownloads = 4;

    if (_filesCurrentlyDownloading.size() < MaxDownloads &&
        !_queuedFilesToDownload.empty())
    {
        File* dl = _queuedFilesToDownload.front();
        if (dl->state != File::State::OnQueue) {
            throw ghoul::RuntimeError(
                "Trying to download file from list of queued files, but its status is "
                "not OnQueue"
            );
        }
        if (dl->download) {
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
            std::ifstream tempFile = std::ifstream(file->URL);
            std::streampos size = tempFile.tellg();
            if (size == 0) {
                LERROR(std::format("File '{}' is empty, removing", dl->destination()));
                currentIt = _filesCurrentlyDownloading.erase(currentIt);
                --i;
            }
            else {
                _downloadedFiles.push_back(dl->destination());
                file->state = File::State::Downloaded;
                trackFinishedDownloads(_trackSynced, file->path);
                currentIt = _filesCurrentlyDownloading.erase(currentIt);
                // if one is removed, i is reduced, else we'd skip one in the list
                --i;
            }
        }
        else if (dl->hasFailed()) {
            LERROR(std::format("File '{}' failed to download. Removing file", file->URL));
            std::string filename;
            size_t lastSlash = file->URL.find_last_of('/');
            if (lastSlash != std::string::npos && lastSlash + 1 < file->URL.size()) {
                filename = file->URL.substr(lastSlash + 1);
            }

            std::filesystem::path filepath = std::filesystem::path(_syncDir) / filename;
            if (std::filesystem::exists(filepath)) {
                std::error_code ec;
                std::filesystem::remove(filepath, ec);
                if (ec) {
                    LERROR(std::format(
                        "Failed to delete file '{}'. {}", filepath, ec.message()
                    ));
                }
                else {
                    LINFO(std::format(
                        "Deleted file after failed download: {}", filepath
                    ));
                }
            }

            currentIt = _filesCurrentlyDownloading.erase(currentIt);
            // If one is removed, i is reduced, else we'd skip one in the list
            --i;
        }
        // The file is not finished downloading, move on to next
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

std::vector<File>::iterator DynamicFileSequenceDownloader::closestFileToNow(double time) {
    ZoneScoped;
    std::vector<File>::iterator closestIt = _availableData.begin();

    std::vector<File>::iterator it = std::lower_bound(
        _availableData.begin(),
        _availableData.end(),
        time,
        [](const File& file, double t) { return file.time < t; }
    );

    if (it == _availableData.end()) {
        return std::prev(_availableData.end());
    }
    if (it == _availableData.begin()) {
        return it;
    }

    std::vector<File>::iterator prev = std::prev(it);
    return _isForwardDirection ? prev : it;
}

void DynamicFileSequenceDownloader::putInQueue() {
    ZoneScoped;

    std::vector<File>::iterator end;
    if (_isForwardDirection) {
        end = _availableData.end();
    }
    else {
        end = _availableData.begin();
        // To catch first file (since begin points to a file but end() does not)
        if (_currentFile == end && _currentFile->state == File::State::Available) {
            _queuedFilesToDownload.push_back(&*_currentFile);
            _currentFile->state = File::State::OnQueue;
            return;
        }
    }

    // If forward iterate from now to end. else reverse from now to begin
    size_t nFileLimit = 0;
    for (std::vector<File>::iterator it = _currentFile;
        it != end;
        _isForwardDirection ? ++it : --it)
    {
        if (it->state == File::State::Available) {
            _queuedFilesToDownload.push_back(&*it);
            it->state = File::State::OnQueue;
        }
        ++nFileLimit;
        // Exit out early if enough files are queued / already downloaded
        if (nFileLimit == _nFilesToQueue) {
            break;
        }
    }
}

void DynamicFileSequenceDownloader::update(double time, double deltaTime) {
    ZoneScoped;

    // First frame cannot guarantee time and deltatime has been set yet
    if (_isFirstFrame) {
        _isFirstFrame = false;
        return;
    }
    if (_isSecondFrame) {
        if (!_availableData.empty()) {
            _isSecondFrame = false;
            _currentFile = closestFileToNow(time);
        }
        return;
    }
    // More than 2hrs a second would generally be unfeasable
    // for a regular internet connection to operate at
    constexpr int SpeedThreshold = 7200; // 2 hours in seconds
    if (std::abs(deltaTime) > SpeedThreshold) {
        // Too fast, do nothing
        if (!_hasNotifiedTooFast) {
            LWARNING(
                "Setting time speed faster than 2h per second will pause the downloader."
            );
            _hasNotifiedTooFast = true;
        }
        return;
    }

    // if delta time direction got changed
    if (_isForwardDirection && deltaTime < 0 || !_isForwardDirection && deltaTime > 0) {
        _isForwardDirection = !_isForwardDirection;
        // Remove from queue when time changed, to start downloading most relevant files
        for (File* file : _queuedFilesToDownload) {
            file->state = File::State::Available;
        }
        _queuedFilesToDownload.clear();
    }

    if (_isForwardDirection && _currentFile != _availableData.end()) {
        // if files are there and time is between next file (+1) and +2 file
        // (meaning the this file is active from now till next file)
        // change this to be next
        if (_currentFile + 1 != _availableData.end() &&
            _currentFile + 2 != _availableData.end() &&
            (_currentFile + 1)->time < time &&
            (_currentFile + 2)->time > time)
        {
            _currentFile++;
        }
        // if its beyond the +2 file, arguably that can mean delta time is to fast
        // and files might be missed. But we also know we went past beyond the next so
        // we no longer know where we are so we reinitialize
        else if (_currentFile + 1 != _availableData.end() &&
                 _currentFile + 2 != _availableData.end() &&
                 (_currentFile + 2)->time < time)
        {
            _currentFile = closestFileToNow(time);
        }
        // We've jumped back without interpolating to a previous time step,
        // past circa 2 files worth of time and without changing delta time
        // >>>>>>>we jumped to here>>>>>>>>>now>>>>>>>
        else if (_currentFile->time - 2 * _currentFile->cadence > time) {
            _currentFile = closestFileToNow(time);
        }
    }
    else if (!_isForwardDirection && _currentFile != _availableData.begin()) {
        // If file is there and time is between prev and this file
        // then change this to be prev. Same goes here as if time is moving forward
        // we will use forward 'usage', meaning file is active from now till next
        if (_currentFile - 1 != _availableData.begin() &&
            _currentFile->time < time &&
            (_currentFile - 1)->time > time)
        {
            _currentFile--;
        }
        // If we are beyond the prev file, again delta time might be to fast, but we
        // no longer know where we are so we reinitialize
        else if (_currentFile - 1 != _availableData.begin() &&
                 (_currentFile - 1)->time > time)
        {
            _currentFile = closestFileToNow(time);
        }
        // We've jumped forward without interpolating to a future time step,
        // past circa 2 files worth of time and without changing delta time
        // <<<<<<now<<<<<<<<<we jumped to here<<<<<<<
        else if (_currentFile->time - 2 * _currentFile->cadence < time) {
            _currentFile = closestFileToNow(time);
        }
    }
    else if (_currentFile->download == nullptr && !_availableData.empty()) {
        _currentFile = closestFileToNow(time);
    }

    if (!_filesCurrentlyDownloading.empty()) {
        checkForFinishedDownloads();
    }

    putInQueue();
    downloadFile();
}

const std::filesystem::path& DynamicFileSequenceDownloader::destinationDirectory() const {
    return _syncDir;
}

void DynamicFileSequenceDownloader::clearDownloaded() {
    _downloadedFiles.clear();
}

bool DynamicFileSequenceDownloader::areFilesCurrentlyDownloading() const {
    return !_filesCurrentlyDownloading.empty();
}

const std::vector<File*>& DynamicFileSequenceDownloader::filesCurrentlyDownloading() const
{
    return _filesCurrentlyDownloading;
}

const std::vector<std::filesystem::path>&
DynamicFileSequenceDownloader::downloadedFiles() const
{
    return _downloadedFiles;
}

} // openspace namespace
