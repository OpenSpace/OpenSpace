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
#include <openspace/engine/globals.h>
#include <openspace/json.h>

// Tracy
//#include <Tracy.hpp>

#include <ghoul/misc/assert.h>


namespace {
    constexpr const char* _loggerCat = "FieldlinesSequence[ Web FLs Worker ]";
    using json = nlohmann::json;
} // namespace

namespace openspace{

    WebFieldlinesWorker::WebFieldlinesWorker(std::string syncDir, std::string serverUrl)
: _syncDir(syncDir), _serverUrl(serverUrl) {
    // Maybe to be used
    _endpointSingleDownload = _serverUrl;
    // TODO should be set by argument to be more general
    // [DEPRICATED FOR NOW, DO WE NEED THIS?]
}

WebFieldlinesWorker::~WebFieldlinesWorker() {
    // Deleting all files that were downloaded during the run.
    // Cancel any potential download
    if (_downloading && _downloading->hasStarted()) {
        _downloading->wait();
    }

    if (_downloading && _availableTimesDownloader
                     && _availableTimesDownloader->hasStarted()) {
        _downloading->wait();
    }

    // Remove all files
    std::vector<std::string> temp =
        ghoul::filesystem::Directory(_syncDir).readFiles();
    // Sneaky check, just want to make sure it is only deleting fieldlines for now
    if (!_fileExtension.empty()) {
        if (temp.back().substr(temp.back().size() - _fileExtension.size()) == 
            _fileExtension &&
            temp.front().substr(temp.front().size() - _fileExtension.size()) == 
            _fileExtension)
        {
            std::for_each(temp.begin(), temp.end(), [&](auto it) {
                FileSys.deleteFile(it);
            });
        }
    }
}
    
// PUBLIC FUNCTIONS
void WebFieldlinesWorker::getRangeOfAvailableTriggerTimes(double time, 
                                                        std::vector<std::pair<double,
                                                        std::string>> &_triggerTimesWeb)
{
    
    // We don't want to keep sending request, if we just get empty responses.
    if (!_noMoreRequests && !_requestSent) {
        _requestSent = true;
        
        // The timespan we would like to request (in seconds)
        // [ 1 day = 86400, 1 week = 604800 ]
        const int timeSpan = 2 * 86400;

        _maxTime.setTime(time);
        _minTime.setTime(time);
        _maxTime.advanceTime(timeSpan);
        _minTime.advanceTime(-timeSpan);

        std::string url = _serverUrl +
            "&time.min=" + _minTime.ISO8601() +
            "&time.max=" + _maxTime.ISO8601();

        AsyncHttpMemoryDownload mmryDld = AsyncHttpMemoryDownload(url);
        _availableTimesDownloader = std::make_unique<AsyncHttpMemoryDownload>(url);
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        _availableTimesDownloader->start(opt);
        _availableTimesDownloader->wait();
    }

    if (!_noMoreRequests && _availableTimesDownloader && 
                _availableTimesDownloader->hasStarted() && 
                _availableTimesDownloader->hasSucceeded()) {
        std::string stringResult;
        std::vector<std::string> urlList;
        std::vector<std::string> timeList;
        const std::string dataID = "dataID";
        const std::string files = "files";

        // TODO emiax: std::copy or similar should be possible here
        std::transform(
            _availableTimesDownloader->downloadedData().begin(),
            _availableTimesDownloader->downloadedData().end(),
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

        // If by any chance it would not sort in properly
        std::sort(_triggerTimesWeb.begin(), _triggerTimesWeb.end());

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
                std::make_pair(_minTime.j2000Seconds(), _maxTime.j2000Seconds());
        }
        _requestSent = false;
    }
    
}

// Download all files in the current window
// This function starts usually in the middle of the window and proceeds to download all
// future timesteps, then steps backwards from the startingpoint 
// TODO(Axel): Different behaviour depending on direction the user is moving in,
// might be wanted?
//#pragma optimize( "", off ) // unoptimized code section 
void WebFieldlinesWorker::downloadWindow(
                                 std::vector<TriggerTime> triggerTimes)
{
    // Helper variables
    size_t startingPoint = triggerTimes.size() / 2;
    bool downloaded = false;
    bool oneUpdate = false;
    bool fastDownload = global::timeManager.deltaTime() > 1800.0;

    if (fastDownload) {
        startingPoint = triggerTimes.size() - 1;
    }

    // Is there a download thread to be joined and added to the list?
    if (_downloading && _downloading->hasSucceeded() && _newWindow) {
        //ZoneScopedN("wait(), add to downloaded list")

        if (true) {
            //ZoneScopedN("Actually still waited for something to download")
            _downloading->wait();
        }
        addToDownloadedList(_latestDownload.triggertime, _latestDownload.url);
        _latestDownload.downloaded = true;
        _readyToDownload = true;
        // This is to trigger one update of the fieldline timestamp that the user is
        // currently on, while the rest of them will be downloaded in the background,
        // and updated once ready
        if (_latestDownload.url == triggerTimes[startingPoint].url) {
            oneUpdate = true;
        }
    }

    if (_readyToDownload) {
        //ZoneScopedN("is ready to download")

        // Forwards
        std::vector<TriggerTime>::iterator forwardIt =
            triggerTimes.begin();
        std::advance(forwardIt, startingPoint);

        //TODO it.downloaded is redundant as it is now. Ment to replace "DownloadedList"
        std::for_each(forwardIt, triggerTimes.end(), [this, &downloaded](TriggerTime& it){
            if (!downloaded && !it.downloaded && 
                            !isFileInDownloadedList(it.triggertime)) { 
                //ZoneScopedN("Forward")
              
                downloadOsfls(it); 
                downloaded = true;
                it.downloaded = true;
                _doneUpdating = false;
            }
        });
               
        // Backwards
        if (!downloaded) {
            std::for_each(
                triggerTimes.rbegin(),
                triggerTimes.rend(),
                [this, &downloaded](TriggerTime it) {
                    if (!downloaded && !it.downloaded && 
                                !isFileInDownloadedList(it.triggertime)) {
                        //ZoneScopedN("Backwards")
                        downloadOsfls(it);
                        downloaded = true;
                        it.downloaded = true;
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
//#pragma optimize( "", on )    // end of unoptimized code session

// Updates the list of available sourcefiles, owned by renderablefieldlinessequence.
// TODO RFLS
void WebFieldlinesWorker::updateRFSSourceFiles(std::vector<std::string>& sourceFiles) {
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

        auto sourcePtr = sourceFiles.begin();
        std::for_each(
            toInsert.begin(),
            toInsert.end(),
            [&sourcePtr, &sourceFiles] (auto insertableElement) {
                for (sourcePtr; sourcePtr != sourceFiles.end(); sourcePtr++) {
                    if (sourcePtr != (--sourceFiles.end())) {
                        if (insertableElement > *sourcePtr &&
                            insertableElement < *sourcePtr++)
                        {
                            sourceFiles.insert(sourcePtr++, insertableElement);
                            break;
                        }
                    }
                    if (insertableElement < *sourcePtr) {
                        sourcePtr = sourceFiles.insert(sourcePtr, insertableElement);
                        break;
                    }
                    if (insertableElement == *sourcePtr) {
                        break;
                    }
                }
                if (sourcePtr == sourceFiles.end()) {
                    sourcePtr = sourceFiles.insert(sourcePtr, insertableElement);
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
    
void WebFieldlinesWorker::downloadOsfls(TriggerTime downloadKey){
    _downloadedSomething = true;
    _latestDownload = downloadKey;

    const std::string fileName =
                        downloadKey.url.substr(downloadKey.url.find_last_of("\\/", 
                                                  downloadKey.url.size() - 1));
    std::string url = downloadKey.url;
    std::string destinationPath =
        absPath(_syncDir + ghoul::filesystem::FileSystem::PathSeparator + fileName);
    // what the downloaded filename is to be

    // Extracting the file type
    if (_fileExtension.empty())
        _fileExtension = fileName.substr(fileName.find_last_of('.', fileName.size() - 1), 
                                                                        fileName.size());
    
    _downloading = std::make_unique<AsyncHttpFileDownload>(
        url, destinationPath, HttpFileDownload::Overwrite::Yes
    );

    HttpRequest::RequestOptions opt = {};
    opt.requestTimeoutSeconds = 0;
    if (true) { //only to scope the ZoneScopedN
        ZoneScopedN("before Start")
        _downloading->start(opt);
        _downloading->wait();
        _readyToDownload = false;
    }

    //return destinationPath;
}


// This function searches for triggerTime in _downloadedTriggerTimes, 
// to see weather a file has already been downloaded or not
bool WebFieldlinesWorker::isFileInDownloadedList(const double time) {
    //ZoneScopedN("file is on disk")

    //bool exists = FileSys.fileExists(path);
    
    std::vector<std::pair<double, std::string>>::const_iterator inList = std::find_if(
        _downloadedTriggerTimes.cbegin(),
        _downloadedTriggerTimes.cend(),
        [&time](const std::pair<double, std::string>& element) {
            return element.first == time;
        }
    );
        
    return inList != _downloadedTriggerTimes.cend();
    //return  exists; 
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
void WebFieldlinesWorker::addToDownloadedList(const double time, const std::string path){     
    const size_t begin = path.find_last_of("\\/");
    const std::string fileName = path.substr(begin + 1);

    ghoul_assert(std::is_sorted(_downloadedTriggerTimes.begin(),
        _downloadedTriggerTimes.end()));

    _downloadedTriggerTimes.insert(
        std::find_if(
            _downloadedTriggerTimes.cbegin(),
            _downloadedTriggerTimes.cend(),
            [&time](const std::pair<double, std::string>& p ) {
                return p.first > time;
            }
        ),
        // this pair is inserted when finding the first bigger time stamp
        std::make_pair(time, fileName)  
    );

    //_downloadedTriggerTimes.insert(
    //    std::upper_bound(
    //        _downloadedTriggerTimes.begin(), _downloadedTriggerTimes.end(), pair
    //    ), std::make_pair(pair.first, fileName)
    //);
}

bool WebFieldlinesWorker::compareTimetriggersEqual(double first, double second) {
    const double eps = 100.0;
    return first > (second - eps) && first < (second + eps);
}
    
} // namespace openspace
