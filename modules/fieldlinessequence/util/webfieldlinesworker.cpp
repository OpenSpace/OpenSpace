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


namespace {
    constexpr const char* _loggerCat = "FieldlinesSequence[ Web FLs Worker ]";
    
} // namespace

namespace openspace{
    // CONSTRUCTOR
    WebFieldlinesWorker::WebFieldlinesWorker(std::string syncDir, std::string serverUrl,std::shared_ptr<std::vector<std::pair<double, std::string>>> triggerTimesOnDisk)
    : _serverUrl(serverUrl)
    , _syncDir(syncDir)
    , _triggerTimesOnDisk(triggerTimesOnDisk)
    , _fileEnding(".osfls")
    , _FLType("PfssIo"){
        _endpointSingleDownload = _serverUrl + "/WSA/" + _FLType; // should be set by argument to be more general
    }
    
    void WebFieldlinesWorker::downloadWindow(std::vector<std::pair<double, std::string>> triggerTimes){
        // Check which files are already on disk
    }
    
    bool fileIsOnDisk(double triggerTime){
        //
    }
    
    
    // PUBLIC FUNCTIONS
    void WebFieldlinesWorker::getRangeOfAvailableTriggerTimes(double start, double end, std::vector<std::tuple<double, std::string, int>>& _triggerTimesWeb){
        
        // temporary, should use parameters for real endpoint later on
        std::string url = "http://localhost:3000/WSA/times/2019-05-02T12:00:00:00/100";
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
        
        _triggerTimesWeb.clear(); // we don't want to save too much in memory at the same time
        parseTriggerTimesList(s, _triggerTimesWeb);
        
        // sort ascending by trigger time
        std::sort(_triggerTimesWeb.begin(), _triggerTimesWeb.end());
        
    }
    
    std::string WebFieldlinesWorker::downloadOsfls(std::string downloadkey){
        std::string url = _endpointSingleDownload + downloadkey + _fileEnding; // adjust this to match final endpoint
        std::string destinationpath = absPath(_syncDir + ghoul::filesystem::FileSystem::PathSeparator + downloadkey + _fileEnding); // what the downloaded filename is to be
        AsyncHttpFileDownload ashd = AsyncHttpFileDownload(url, destinationpath, HttpFileDownload::Overwrite::Yes);
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        ashd.start(opt);
        ashd.wait();
        if(ashd.hasSucceeded() == true ){
            LERROR("succeeeded: " + destinationpath);
        }
        if(ashd.hasFailed() == true ){
            LERROR("failed: " + destinationpath);
        }
        return destinationpath;
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
    void WebFieldlinesWorker::parseTriggerTimesList(std::string s, std::vector<std::tuple<double, std::string, int>>& _triggerTimesWeb){
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
    
    bool WebFieldlinesWorker::compareTimetriggersEqual(double first, double second){
        double eps = 100.0;
        if(first > second - eps && first < second + eps) return true;
        return false;
    }
    
} // namespace openspace
