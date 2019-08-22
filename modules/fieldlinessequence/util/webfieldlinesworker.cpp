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
#include <openspace/util/httprequest.h>
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
    // CONSTRUCTOR
    WebFieldlinesWorker::WebFieldlinesWorker(std::string syncDir, std::string serverUrl,std::shared_ptr<std::vector<std::pair<double, std::string>>> triggerTimesOnDisk)
    : _serverUrl(serverUrl)
    , _syncDir(syncDir)
    , _triggerTimesOnDisk(triggerTimesOnDisk)
    , _fileEnding(".osfls")
    , _FLType("WSA_Fieldlines_PFSS_IO"){
        _endpointSingleDownload = _serverUrl + "WSA/fieldline/" + _FLType + "/"; // should be set by argument to be more general
    }

    // Destructor, deleting all files that were downloaded during the run.
    WebFieldlinesWorker::~WebFieldlinesWorker() {
        std::for_each(_downloadedTriggerTimes.begin(), _downloadedTriggerTimes.end(), [&](auto it) {
            FileSys.deleteFile(_syncDir + FileSys.PathSeparator + it.second + ".osfls");
        });
    }
    
    // PUBLIC FUNCTIONS
    void WebFieldlinesWorker::getRangeOfAvailableTriggerTimes(double start, double end, std::vector<std::tuple<double, std::string, int>> &_triggerTimesWeb, size_t id){
        const std::string oSpaceStartTime = "2000-01-01";
        // temporary, should use parameters for real endpoint later on
        auto time = global::timeManager.time().ISO8601();
        Time maxTime;
        Time minTime;

        maxTime.setTime(time);
        maxTime.advanceTime(48000);
        
        std::string url = "https://iswa.gsfc.nasa.gov/IswaSystemWebApp/FilesInRangeServlet?dataID=" + std::to_string(id) + "&time.min=" + time + "&time.max=" + maxTime.ISO8601();
        // This is done quite early in the startup, requesting based on Open Space start time, will result
        // In fetching filed lines that doesnt exist, and is too far away back in time.
        //if (time.substr(0, 10) != oSpaceStartTime)
        //    url = "http://localhost:3000/WSA/times/" + time + "50";
        SyncHttpMemoryDownload mmryDld = SyncHttpMemoryDownload(url);
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        mmryDld.download(opt);
        
        // Put the results in a string
        std::string s;
        
        std::transform(mmryDld.downloadedData().begin(), mmryDld.downloadedData().end(), std::back_inserter(s),
                       [](char c) {
                           return c;
                       });


        auto res = json::parse(s);
        
        const std::string dataID = "dataID";
        const std::string files = "files";
        const std::string timeMax = "time.max";
        const std::string timeMin = "time.min";
        std::vector<std::string> urlList;
        std::vector<std::string> timeList;
        

        for (auto& elem : res[files]) {
            timeList.push_back(elem["timestamp"]);
            urlList.push_back(elem["url"]);
        }
  
        // NEXT: Combine lists

        _triggerTimesWeb.clear(); // we don't want to save too much in memory at the same time

        std::sort(_triggerTimesWeb.begin(), _triggerTimesWeb.end());

    }

    // Download all files in the current window
    // This function starts in the middle of the window and proceeds to download all future timesteps, 
    // then steps backwards from the middle
    //TODO(Axel): Different behaviour depending on direction the user is moving in.
    void WebFieldlinesWorker::downloadWindow(std::vector<std::pair<double, std::string>> triggerTimes) {

        // Want to use std::find_if to break the for_each
        int middle = triggerTimes.size() / 2;
        bool downloaded = false;

        // Forwards
        std::vector<std::pair<double, std::string>>::iterator forwardIt = triggerTimes.begin();
        std::advance(forwardIt, middle);
        std::for_each(forwardIt, triggerTimes.end(), [this, &downloaded](auto it) {
            if (!downloaded && !fileIsOnDisk(it.first)) {
                downloadOsfls(it.second);
                addToDownloadedList(it);
                downloaded = true;
                _doneUpdating = false;
            }
        });

        // Backwards
        if (!downloaded) {
            std::for_each(triggerTimes.rbegin(), triggerTimes.rend(), [this, &downloaded](auto it) {
                if (!downloaded && !fileIsOnDisk(it.first)) {
                    downloadOsfls(it.second);
                    addToDownloadedList(it);
                    downloaded = true;
                    _doneUpdating = false;  
                }
            });
        }

        if (!downloaded && !_doneUpdating && _newWindow) {
            // If reach this point, we now know that we have downloaded all the sets
            _readyToUpdateSourceFiles = true;
            _newWindow = false;
        }

            
    }

    // Updates the list of available sourcefiles, owned by renderablefieldlinessequence.
    void WebFieldlinesWorker::updateRFSSourceFiles(std::vector<std::string>& _sourceFiles) {
        if (_readyToUpdateSourceFiles) {
            std::vector<std::string> toInsert;
            std::transform(_downloadedTriggerTimes.begin(), _downloadedTriggerTimes.end(), std::back_inserter(toInsert), [this](auto const& pair) { return _syncDir + FileSys.PathSeparator + pair.second + ".osfls"; });
            auto sourcePtr = _sourceFiles.begin();
            std::for_each(toInsert.begin(), toInsert.end(), [&sourcePtr, &_sourceFiles](auto insertableElement) {

                for (sourcePtr; sourcePtr != _sourceFiles.end(); sourcePtr++) {

                    if (sourcePtr != (--_sourceFiles.end())) {
                        if (insertableElement > *sourcePtr && insertableElement < *sourcePtr++) {
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
            });

            // Set flags to let anyone interested know that this has been done.
            _readyToUpdateSourceFiles = false;
            _doneUpdating = true;
        }
 
    }

    bool WebFieldlinesWorker::windowIsComplete()
    {
        return _doneUpdating;
    }

    void WebFieldlinesWorker::flagUpdated() {
        _doneUpdating = false;
    }

    void WebFieldlinesWorker::newWindowToDownload()
    {
        _newWindow = true;
    }
    
    std::string WebFieldlinesWorker::downloadOsfls(std::string downloadkey){
        //std::string url = _endpointSingleDownload + downloadkey + _fileEnding; // adjust this to match final endpoint
        std::string url = "http://localhost:3000/WSA/PfssIo2019-09-16T13-21-20.000.osfls/" + downloadkey; // temp thing, should be the latest one
        std::string destinationpath = absPath(_syncDir + ghoul::filesystem::FileSystem::PathSeparator + downloadkey + _fileEnding); // what the downloaded filename is to be
        AsyncHttpFileDownload ashd = AsyncHttpFileDownload(url, destinationpath, HttpFileDownload::Overwrite::Yes);
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        ashd.start(opt);
        ashd.wait();
        if(ashd.hasSucceeded() == true ){
            //LERROR("succeeeded: " + destinationpath);
        }
        if(ashd.hasFailed() == true ){
            //LERROR("failed: " + destinationpath);
        }
        return destinationpath;
    }

    // This function searches for triggerTime in _downloadedTriggerTimes, 
    // to see weather a file has already been downloaded or not
    bool WebFieldlinesWorker::fileIsOnDisk(double triggerTime){
        if(std::find_if(_downloadedTriggerTimes.begin(), _downloadedTriggerTimes.end(),
            [&triggerTime](std::pair<double, std::string> const &element){
            return element.first == triggerTime;
        }) != _downloadedTriggerTimes.end())
            return true;
        else
            return false;
    }
    
    void WebFieldlinesWorker::checkForExistingData(std::vector<std::tuple<double, std::string, int>>& _triggerTimesWeb,
                                                              std::vector<std::pair<double, std::string>>& _triggerTimesOnDisk){
        int indexWeb = 0;
        int indexDisk = 0;
        
        while(true){
            if(compareTimetriggersEqual(std::get<0>(_triggerTimesWeb[indexWeb]), _triggerTimesOnDisk[indexDisk].first)){
                std::get<1>(_triggerTimesWeb[indexWeb]) = _triggerTimesOnDisk[indexDisk].second;
            }
        }
    }
    
    // PRIVATE FUNCTIONS
    void WebFieldlinesWorker::parseTriggerTimesList(std::string s, std::vector<std::tuple<double, std::string, int>> &_triggerTimesWeb){
        // Turn into stringstream to parse the comma-delimited string into vector
        std::stringstream ss(s);
        char c;
        std::string sub;
        while(ss >> c)
        {
            if (c == '[' || c == ']' || c == '"' ) continue;
            else if (c == ','){
                _triggerTimesWeb.push_back(std::make_tuple(triggerTimeString2Double(sub), sub, -1));
                sub.clear();
            }
            else sub += c;
        }

        _triggerTimesWeb.push_back(std::make_tuple(triggerTimeString2Double(sub), sub, -1));
    }
    
    double WebFieldlinesWorker::triggerTimeString2Double(std::string s){
        s.replace(13, 1, ":");
        s.replace(16, 1, ":");
        Time time = Time();
        time.setTime(s);
        return (time.j2000Seconds() /*- 69.185013294*/); // openspace timeconverter gives an error. but we're gonna be consistent with the error
    }
    
    void WebFieldlinesWorker::triggerTimeDouble2String(double i, std::string& s){
        double temp = i /*+ 69.185013294*/;
        Time time = Time();
        time.setTime(temp);
        s = time.ISO8601();
        s.replace(13, 1, "-");
        s.replace(16, 1, "-");
    }
    
    // Inserts the pair in sorted order
    void WebFieldlinesWorker::addToDownloadedList(std::pair<double, std::string> pair){
        _downloadedTriggerTimes.insert(std::upper_bound(_downloadedTriggerTimes.begin(),_downloadedTriggerTimes.end(),pair),pair);
    }

    bool WebFieldlinesWorker::compareTimetriggersEqual(double first, double second){
        double eps = 100.0;
        if(first > second - eps && first < second + eps) return true;
        return false;
    }
    
} // namespace openspace
