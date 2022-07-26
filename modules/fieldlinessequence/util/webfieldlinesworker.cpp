/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/fieldlinessequence/util/webfieldlinesworker.h>

#include <ghoul/logging/logmanager.h>
#include <modules/sync/syncs/httpsynchronization.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <openspace/util/timemanager.h>
#include <openspace/engine/globals.h>
#include <openspace/json.h>


namespace {
    constexpr const char* _loggerCat = "FieldlinesSequence[ Web FLs Worker ]";
    using json = nlohmann::json;
} // namespace

namespace openspace{

WebFieldlinesWorker::WebFieldlinesWorker(std::string syncDir, std::string serverUrl)
: _syncDir(syncDir), _serverUrl(serverUrl) {
    // Maybe to be used
    _endpointSingleDownload = _serverUrl;
    // should be set by argument to be more general
    // [DEPRICATED FOR NOW, DO WE NEED THIS?]
}

WebFieldlinesWorker::~WebFieldlinesWorker() {
    // Deleting all files that were downloaded during the run.
    // Cancel any potential download
    if (_downloading && _downloading->hasStarted())
        _downloading->wait();
    // Remove all files
    std::vector<std::string> temp =
        ghoul::filesystem::Directory(_syncDir).readFiles();
    // Sneaky check, just want to make sure it is only deleting fieldlines for now
    if (temp.back().substr(temp.back().size() - 5) == "osfls" &&
        temp.front().substr(temp.front().size() - 5) == "osfls")
    {
        std::for_each(temp.begin(), temp.end(), [&](auto it) {
            FileSys.deleteFile(it);
        });
    }
}
    
// PUBLIC FUNCTIONS
void WebFieldlinesWorker::getRangeOfAvailableTriggerTimes(double startTime,
                                                            double endTime,
                                                            std::vector<std::pair<double,
                                                    std::string>> &_triggerTimesWeb)
{
    // We don't want to keep sending request, if we just get empty responses.
    if (!_noMoreRequests) {
        auto time = global::timeManager.time().ISO8601();
        Time maxTime;
        Time minTime;

        // The timespan we would like to request (in seconds)
        // [ 1 day = 86400, 1 week = 604800 ]
        const int timeSpan = 2*86400;

        const std::string dataID = "dataID";
        const std::string files = "files";
        std::string stringResult;
        std::vector<std::string> urlList;
        std::vector<std::string> timeList;

        maxTime.setTime(time);
        minTime.setTime(time);
        maxTime.advanceTime(timeSpan);
        minTime.advanceTime(-timeSpan);

        std::string url = _serverUrl +
            "&time.min=" + minTime.ISO8601() +
            "&time.max=" + maxTime.ISO8601();

        SyncHttpMemoryDownload mmryDld = SyncHttpMemoryDownload(url);
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        mmryDld.download(opt);

        // TODO emiax: std::copy or similar should be possible here
        std::transform(
            mmryDld.downloadedData().begin(),
            mmryDld.downloadedData().end(),
            std::back_inserter(stringResult),
            [](char c) {
                return c;
            }
        );

        auto res = json::parse(stringResult);
        auto temp = std::move(_triggerTimesWeb);
        _triggerTimesWeb.clear(); // Clear old big window

        for (auto& elem : res[files]) {
            timeList.push_back(elem["timestamp"]);
            urlList.push_back(elem["url"]);
        }

        // Just want to make sure there is no error in the parsing,
        // so taking the smallest dimension, but should be the same
        for (int i = 0; i < std::min(timeList.size(), urlList.size()); i++) {
            _triggerTimesWeb.push_back(
                std::make_pair(triggerTimeString2Double(timeList[i]), urlList[i])
            );
        }

        std::sort(_triggerTimesWeb.begin(), _triggerTimesWeb.end());
        // If by any chance it would not sort in properly

        if (_triggerTimesWeb.size() == 0 ||
            std::equal(temp.begin(),
                temp.end(),
                _triggerTimesWeb.begin(),
                _triggerTimesWeb.end()
            )
        ) { // We got an empty response or the same response twice, stahp it
            _strikes++;
        }

        // We have got 2 strikes, no more requests for you, Mr.Sir.
        if (_strikes % 2 == 0) { 
            _bigWindowHasData = (_triggerTimesWeb.size() > 0);
            _noMoreRequests = true;
            acceptableToStartRequestingAgain =
                std::make_pair(minTime.j2000Seconds(), maxTime.j2000Seconds());
        }     
    }
}

// Download all files in the current window
// This function starts usually in the middle of the window and proceeds to download all
// future timesteps, then steps backwards from the startingpoint 
// TODO(Axel): Different behaviour depending on direction the user is moving in,
// might be wanted?
void WebFieldlinesWorker::downloadWindow(
                                 std::vector<std::pair<double, std::string>> triggerTimes)
{
    // Helper variables
    int startingPoint = triggerTimes.size() / 2;
    bool downloaded = false;
    bool oneUpdate = false;
    bool fastDownload = global::timeManager.deltaTime() > 1800.0;

    if (fastDownload) {
        startingPoint = triggerTimes.size() - 1;
    }

    // Is there a download thread to be joined and added to the list?
    if (_downloading && _downloading->hasSucceeded() && _newWindow) {
        _downloading->wait();
        addToDownloadedList(_latestDownload);
        _readyToDownload = true;
        // This is to trigger one update of the fieldline timestamp that the user is
        // currently on, while the rest of them will be downloaded in the background,
        // and updated once ready
        if (_latestDownload.second == triggerTimes[startingPoint].second)
            oneUpdate = true;
    }

    if (_readyToDownload) {
        // Forwards
        std::vector<std::pair<double, std::string>>::iterator forwardIt =
            triggerTimes.begin();

        std::advance(forwardIt, startingPoint);
        std::for_each(forwardIt, triggerTimes.end(), [this, &downloaded](auto it) {
            if (!downloaded && !fileIsOnDisk(it.first)) {
                downloadOsfls(it);
                downloaded = true;
                _doneUpdating = false;
            }
        });

        // Backwards
        if (!downloaded) {
            std::for_each(
                triggerTimes.rbegin(),
                triggerTimes.rend(),
                [this, &downloaded](auto it) {
                    if (!downloaded && !fileIsOnDisk(it.first)) {
                        downloadOsfls(it);
                        downloaded = true;
                        _doneUpdating = false;
                    }
                }
            );
        }
    }

    if ((!downloaded &&
         !_doneUpdating &&
          _newWindow &&
          _readyToDownload &&
          _downloadedSomething) ||
        oneUpdate && _downloadedSomething)
    {
        // If reach this point, we now know that we have downloaded all the sets
        _readyToUpdateSourceFiles = true;
        if (!oneUpdate) {
            _newWindow = false;
        }
        oneUpdate = false;
    }
}

// Updates the list of available sourcefiles, owned by renderablefieldlinessequence.
void WebFieldlinesWorker::updateRFSSourceFiles(std::vector<std::string>& _sourceFiles) {
    if (_readyToUpdateSourceFiles) {
        std::vector<std::string> toInsert;
        std::transform(
            _downloadedTriggerTimes.begin(),
            _downloadedTriggerTimes.end(),
            std::back_inserter(toInsert),
            [this] (auto const& pair) {
                return _syncDir + FileSys.PathSeparator + pair.second;
            }
        );

        auto sourcePtr = _sourceFiles.begin();
        std::for_each(
            toInsert.begin(),
            toInsert.end(),
            [&sourcePtr, &_sourceFiles] (auto insertableElement) {
                for (sourcePtr; sourcePtr != _sourceFiles.end(); sourcePtr++) {
                    if (sourcePtr != (--_sourceFiles.end())) {
                        if (insertableElement > *sourcePtr &&
                            insertableElement < *sourcePtr++)
                        {
                            _sourceFiles.insert(sourcePtr++, insertableElement);
                            break;
                        }
                    }
                    if (insertableElement < *sourcePtr) {
                        sourcePtr = _sourceFiles.insert(sourcePtr, insertableElement);
                        break;
                    }
                    if (insertableElement == *sourcePtr) {
                        break;
                    }
                }
                if (sourcePtr == _sourceFiles.end()) {
                    sourcePtr = _sourceFiles.insert(sourcePtr, insertableElement);
                }
            }
        );

        // Set flags to let anyone interested know that this has been done.
        _readyToUpdateSourceFiles = false;
        _doneUpdating = true;
    }
 
}

bool WebFieldlinesWorker::windowIsComplete() {
    return _doneUpdating;
}

void WebFieldlinesWorker::flagUpdated() {
    _doneUpdating = false;
}

void WebFieldlinesWorker::newWindowToDownload() {
    _newWindow = true;
    _downloadedSomething = false;
}

bool WebFieldlinesWorker::edgeMode() {
    const double currentTime = global::timeManager.time().j2000Seconds();

    if (currentTime < acceptableToStartRequestingAgain.first ||
        currentTime > acceptableToStartRequestingAgain.second)
    {
        _noMoreRequests = false;
        _bigWindowHasData = false;
    }
    return _noMoreRequests && _bigWindowHasData;
}
    
std::string WebFieldlinesWorker::downloadOsfls(std::pair<double,std::string> downloadKey)
{
    _downloadedSomething = true;
    _latestDownload = downloadKey;

    // YYYY-MM-DDTHH-MM-SS.sss.osfls - Might change
    const int fileNameLength = 29;
    const std::string fileName =
        downloadKey.second.substr(downloadKey.second.size() - fileNameLength);

    std::string url = downloadKey.second;
    std::string destinationPath =
        absPath(_syncDir + ghoul::filesystem::FileSystem::PathSeparator + fileName);
    // what the downloaded filename is to be

    _downloading = std::make_unique<AsyncHttpFileDownload>(
        url, destinationPath, HttpFileDownload::Overwrite::Yes
    );

    HttpRequest::RequestOptions opt = {};
    opt.requestTimeoutSeconds = 0;

    _downloading->start(opt);
    _readyToDownload = false;

    return destinationPath;
}

// This function searches for triggerTime in _downloadedTriggerTimes, 
// to see weather a file has already been downloaded or not
bool WebFieldlinesWorker::fileIsOnDisk(double triggerTime) {
    return std::find_if(
        _downloadedTriggerTimes.begin(),
        _downloadedTriggerTimes.end(),
        [&triggerTime] (std::pair<double, std::string> const &element) {
            return element.first == triggerTime;
        }) != _downloadedTriggerTimes.end();
}
    
void WebFieldlinesWorker::checkForExistingData(
    std::vector<std::tuple<double, std::string, int>>& _triggerTimesWeb,
    std::vector<std::pair<double, std::string>>& _triggerTimesOnDisk)
{
    int indexWeb = 0;
    int indexDisk = 0;

    // TODO emiax: This looks like an infinite loop.
    // Need a way to break out from this when destroying the worker..?
    while (true) {
        if (compareTimetriggersEqual(
                std::get<0>(_triggerTimesWeb[indexWeb]),
                _triggerTimesOnDisk[indexDisk].first
        )) {
            std::get<1>(_triggerTimesWeb[indexWeb]) =
                _triggerTimesOnDisk[indexDisk].second;
        }
    }
}
    
void WebFieldlinesWorker::parseTriggerTimesList(std::string s,
                      std::vector<std::tuple<double, std::string, int>> &_triggerTimesWeb)
{
    // Turn into stringstream to parse the comma-delimited string into vector
    std::stringstream ss(s);
    char c;
    std::string sub;
    while(ss >> c) {
        if (c == '[' || c == ']' || c == '"') {
            continue;
        }

        if (c == ',') {
            _triggerTimesWeb.push_back(
                std::make_tuple(triggerTimeString2Double(sub), sub, -1)
            );
            sub.clear();
        }
        else {
            sub += c;
        }
    }

    _triggerTimesWeb.push_back(std::make_tuple(triggerTimeString2Double(sub), sub, -1));
}
    
double WebFieldlinesWorker::triggerTimeString2Double(std::string s){
    s.replace(13, 1, ":");
    s.replace(16, 1, ":");
    Time time = Time();
    time.setTime(s);
    return time.j2000Seconds() /*- 69.185013294*/;
    // openspace timeconverter gives an error.
    // but we're gonna be consistent with the error
}
    
void WebFieldlinesWorker::triggerTimeDouble2String(double i, std::string& s) {
    double temp = i /*+ 69.185013294*/;
    Time time = Time();
    time.setTime(temp);
    s = time.ISO8601();
    s.replace(13, 1, "-");
    s.replace(16, 1, "-");
}
    
// Inserts the pair in sorted order
void WebFieldlinesWorker::addToDownloadedList(std::pair<double, std::string> pair) {
    const std::string fileName = pair.second.substr(pair.second.size() - 29);
    _downloadedTriggerTimes.insert(
        std::upper_bound(
            _downloadedTriggerTimes.begin(), _downloadedTriggerTimes.end(), pair
        ), std::make_pair(pair.first, fileName)
    );
}

bool WebFieldlinesWorker::compareTimetriggersEqual(double first, double second) {
    const double eps = 100.0;
    return first > second - eps && first < second + eps;
}
    
} // namespace openspace