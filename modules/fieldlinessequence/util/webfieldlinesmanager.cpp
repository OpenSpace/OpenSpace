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

#include <modules/fieldlinessequence/util/webfieldlinesmanager.h>

#include <ghoul/logging/logmanager.h>
#include <openspace/util/httprequest.h>
#include <modules/sync/syncs/httpsynchronization.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/file.h>
#include <openspace/util/timemanager.h>
#include <openspace/engine/globals.h>


namespace {
    constexpr const char* _loggerCat = "FieldlinesSequence[ Web FLs Manager ]";
    
} // namespace

namespace openspace{
    
    // dowload files specified in _filestodownload
    void WebFieldlinesManager::downloadFieldlines(){
        LERROR("starting download");
        for (int index : _filesToDownload){
            

            std::string downloadkey = _availableTriggertimes[index].second;
            double timetrigger = _availableTriggertimes[index].first;
            
            // download fieldlines file
            std::string destPath = downloadOsfls(downloadkey);
            
            //add the timetrigger at the right place in the list
            int i = 0;
            if(*rfs_nStates > 0){
                while(timetrigger > (*rfs_startTimes)[i]){
                    i++;
                    if( i == static_cast<int>(*rfs_nStates)) break;
                    
                }
            }
            rfs_sourceFiles->insert(rfs_sourceFiles->begin() + i, destPath);
            rfs_startTimes->insert(rfs_startTimes->begin() + i, timetrigger);
            (*rfs_nStates) += 1;
        }
    }


    void WebFieldlinesManager::initializeWebFieldlinesManager(std::string identifier, std::string fieldLineModelType, int& _activeTriggerTimeIndex, size_t& _nStates, std::vector<std::string>& _sourceFiles, std::vector<double>& _startTimes)
    {
        _flsType = fieldLineModelType;
        _syncDir = initializeSyncDirectory(identifier);

        _downloadMargin = 2;
        _timeTriggerDelta = 7200;
        
        rfs_activeTriggerTimeIndex = &_activeTriggerTimeIndex;
        rfs_nStates = &_nStates;
        rfs_sourceFiles = &_sourceFiles;
        rfs_startTimes = &_startTimes;
        
        getAvailableTriggertimes();
        
        setInitialSet(global::timeManager.time().j2000Seconds());
        downloadInitialSequence();
        
        LERROR("WebFieldlinesManager initialized");

    }

    std::string WebFieldlinesManager::getDirectory(){
        return _syncDir;
    }

    // --------------------------- PRIVATE FUNCTIONS --------------------------- //
    
    // this function aint done
    void WebFieldlinesManager::update(){
        
        updateTTIndexWeb();
        
        // check how many are left until fieldlinessequence runs out - add direction information later
        double nextTimeTrigger;
        double eps = 10.0;
        if(*rfs_activeTriggerTimeIndex ==  static_cast<int>(*rfs_nStates)-1){
            // if it's at the last index, definetily start some downloading
            LERROR("gonna need some more files");
            return;
        }
        
        int setsLeftUntilGapOrEnd = 0;
        for (int i = *rfs_activeTriggerTimeIndex; i < static_cast<int>(*rfs_nStates)-1; i++){
            setsLeftUntilGapOrEnd++;
            nextTimeTrigger = _availableTriggertimes[_activeTTIndexWeb + setsLeftUntilGapOrEnd].first;
            // if the next file in renderablefieldsequence is further away than the actual next timetrigger, download it
            if((*rfs_startTimes)[i + 1] > (nextTimeTrigger + eps)){
                break;
            }
            if(setsLeftUntilGapOrEnd > 2){
                //we're all gucci
                break;
            }
        }
        if(setsLeftUntilGapOrEnd < 2){
            LERROR("gonna need some more files");
            setFilesToDownload(setsLeftUntilGapOrEnd);
            downloadFieldlines(); // this should be thread with lock
        }
    }
    void WebFieldlinesManager::setFilesToDownload(int setsLeftUntilGapOrEnd){
        int startInd = _activeTTIndexWeb + setsLeftUntilGapOrEnd + 1;
        int endInd = startInd + _downloadMargin;
        
        if(endInd >= static_cast<int>(_availableTriggertimes.size())){
            endInd = static_cast<int>(_availableTriggertimes.size()) - 1;
        }
        
        for( int i = startInd; i <= endInd; i++){
            _filesToDownload.push_back(i);
        }
        
    }
    
    std::string WebFieldlinesManager::downloadOsfls(std::string downloadkey){
        std::string url = "http://localhost:3000/WSA/" + downloadkey;
        std::string destinationpath = absPath(_syncDir + '/' + downloadkey.substr(6));
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

    // Make sure that the sync directory exists
    // Also creates a new directory in the web_fieldlines directory corresponding to the field line identifier
    std::string WebFieldlinesManager::initializeSyncDirectory(std::string identifier) {
        std::string path = absPath("${BASE}/sync/http/web_fieldlines") + FileSys.PathSeparator;
        
        if (!FileSys.directoryExists(path)) {
            FileSys.createDirectory(path);
        }
        path = absPath(path + identifier);
        if(!FileSys.directoryExists(path)) {
            FileSys.createDirectory(path);
        }
        return path;
    }
    
    
    void WebFieldlinesManager::getAvailableTriggertimes(){
        std::string url = "http://localhost:3000/WSA/available/" + _flsType;
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
        parseTriggerTimesList(s);
        
        // sort ascending by trigger time
        std::sort(_availableTriggertimes.begin(), _availableTriggertimes.end());
    }
    
    void WebFieldlinesManager::setInitialSet(double openSpaceTime){
        
        _activeTTIndexWeb = -1;
        do _activeTTIndexWeb++;
        while (openSpaceTime > _availableTriggertimes[_activeTTIndexWeb].first);
        
        int startInd = _activeTTIndexWeb - _downloadMargin;
        int endInd = _activeTTIndexWeb + _downloadMargin;
        
        if(startInd < 0) startInd = 0;
        if(endInd >= static_cast<int>(_availableTriggertimes.size()))
            endInd = static_cast<int>(_availableTriggertimes.size())-1;
        
        for(int i = startInd; i <= endInd; i++)
            _filesToDownload.push_back(i);
    }
    
    // Download the initial set of fieldlines
    void WebFieldlinesManager::downloadInitialSequence(){
        for (int index : _filesToDownload){
            // download fieldlines file
            std::string downloadkey = _availableTriggertimes[index].second;
            std::string destPath = downloadOsfls(downloadkey);
        }
        _filesToDownload.clear();
    }
    

    void WebFieldlinesManager::updateTTIndexWeb(){
        double rfs_time = (*rfs_startTimes)[*rfs_activeTriggerTimeIndex];
        _activeTTIndexWeb = 0;
        double eps = 10.0;
        
        while(true){
            if((rfs_time > _availableTriggertimes[_activeTTIndexWeb].first - eps)
               && (rfs_time < _availableTriggertimes[_activeTTIndexWeb].first + eps))
                break;
            _activeTTIndexWeb++;
        }
    }
    
    void WebFieldlinesManager::parseTriggerTimesList(std::string s){
        // Turn into stringstream to parse the comma-delimited string into vector
        std::stringstream ss(s);
        char c;
        std::string sub;
        while(ss >> c)
        {
            if (c == '[' || c == ']' || c == '"' ) continue;
            else if (c == ','){
                double tt = triggerTimeString2Double(sub.substr(6, 23));
                _availableTriggertimes.push_back(std::make_pair(tt, sub));
                sub.clear();
            }
            else sub += c;
        }
        double tt = triggerTimeString2Double(sub.substr(6, 23));
        _availableTriggertimes.push_back(std::make_pair(tt, sub));
    }
    
    double WebFieldlinesManager::triggerTimeString2Double(std::string s){
        s.replace(13, 1, ":");
        s.replace(16, 1, ":");
        Time time = Time();
        time.setTime(s);
        return (time.j2000Seconds() /*- 69.185013294*/); // openspace timeconverter gives an error. but we're gonna be consistent with the error
    }
    
    void WebFieldlinesManager::triggerTimeDouble2String(double i, std::string& s){
        double temp = i /*+ 69.185013294*/;
        Time time = Time();
        time.setTime(temp);
        s = time.ISO8601();
        s.replace(13, 1, "-");
        s.replace(16, 1, "-");
    }
    
} // namespace openspace
