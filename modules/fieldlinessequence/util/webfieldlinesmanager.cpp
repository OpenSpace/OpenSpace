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

    WebFieldlinesManager::WebFieldlinesManager(std::string syncDir, int& _activeTriggerTimeIndex, size_t& _nStates, std::vector<std::string>& _sourceFiles, std::vector<double>& _startTimes){
        
        // change to parameter
        _syncDir = syncDir;
        _flsType = "PfssIo";
        _downloadMargin = 3;
        _timeTriggerDelta = 7200;
        
        rfs_activeTriggerTimeIndex = &_activeTriggerTimeIndex;
        rfs_nStates = &_nStates;
        rfs_sourceFiles = &_sourceFiles;
        rfs_startTimes = &_startTimes;
   
        getAvailableTriggertimes();
        
        setInitialSet(global::timeManager.time().j2000Seconds());
        
        LERROR("WebFieldlinesManager initialized");
    
    }
    
    // dowload files specified in _filestodownload
    // I'm thinking we can replace the parameters with pointers to the lists that will be
    // initialized in the constuctor instead
    void WebFieldlinesManager::downloadFieldlines(){
        LERROR("starting download");
        for (int index : _filesToDownload){
            

            std::string filename = _availableTriggertimes[index].second;
            double timetrigger = _availableTriggertimes[index].first;
            
            // download fieldlines file
            std::string destPath = downloadOsfls(filename);
            
            //add the timetrigger at the right place in the list
            int i = 0;
            while(timetrigger > (*rfs_startTimes)[i]){
                if( i == static_cast<int>(*rfs_nStates)-1) break;
                else i++;
            }
            rfs_sourceFiles->insert(rfs_sourceFiles->begin() + i, destPath);
            rfs_startTimes->insert(rfs_startTimes->begin() + i, timetrigger);
            (*rfs_nStates) += 1;
        }
    }

    // ------------------------------- OPERATORS ------------------------------- //

    // Operator ()
    void WebFieldlinesManager::operator()(std::string identifier, std::string fieldLineModelType)
    {
        _flsType = fieldLineModelType;
        _syncDir = initializeSyncDirectory(identifier);

        std::string testTime;
        triggerTimeInt2String(610056000, testTime);

        int testInt;
        triggerTimeString2Int(testTime, testInt);

        getAvailableTriggertimes();

        LERROR("WebFieldlinesManager initialized");

    }

    std::string WebFieldlinesManager::getDirectory(){
        return _syncDir;
    }

    // --------------------------- PRIVATE FUNCTIONS --------------------------- //
    
    // this function aint done
    void WebFieldlinesManager::update(){
        // check how many are left until fieldlinessequence runs out - add direction information later
        double nextTheroticalTimeTrigger;
        double eps = 100;
        if(*rfs_activeTriggerTimeIndex ==  *rfs_nStates - 1){
            // if it's at the last index, definetily start some downloading
            return;
        }
        for (int i = *rfs_activeTriggerTimeIndex; i < *rfs_nStates; i++){
            nextTheroticalTimeTrigger = (*rfs_startTimes)[i] +_timeTriggerDelta;
            if((*rfs_startTimes)[i + 1] > (nextTheroticalTimeTrigger + eps)){
                // do some downloading
            }
            
        }
    }
    
    std::string WebFieldlinesManager::downloadOsfls(std::string triggertime){
        std::string url = "http://localhost:3000/WSA/" + triggertime;
        std::string destinationpath = absPath(_syncDir + '/' + triggertime.substr(6));
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
        
        int openspaceindex = -1;
        do openspaceindex++;
        while (openSpaceTime > _availableTriggertimes[openspaceindex].first);
        
        int startInd = openspaceindex - _downloadMargin;
        int endInd = openspaceindex + _downloadMargin;
        
        if(startInd < 0) startInd = 0;
        if(endInd >= _availableTriggertimes.size())
            endInd = _availableTriggertimes.size()-1;
        
        for(int i = startInd; i <= endInd; i++)
            _filesToDownload.push_back(i);
    }
    
    // TODO
    void WebFieldlinesManager::downloadInitialSequence(std::vector<double> triggertimes){
        
    }
    
    // TODO
    void WebFieldlinesManager::updateStartTimes(){
        
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
                double tt = triggerTimeString2Int(sub.substr(6, 23));
                _availableTriggertimes.push_back(std::make_pair(tt, sub));
                sub.clear();
            }
            else sub += c;
        }
        double tt = triggerTimeString2Int(sub.substr(6, 23));
        _availableTriggertimes.push_back(std::make_pair(tt, sub));
    }
    
    int WebFieldlinesManager::triggerTimeString2Int(std::string s){
        s.replace(13, 1, ":");
        s.replace(16, 1, ":");
        Time time = Time();
        time.setTime(s);
        return static_cast<int> (time.j2000Seconds() - 69.185013294);
    }
    
    void WebFieldlinesManager::triggerTimeInt2String(int i, std::string& s){
        double temp = i + 69.185013294;
        Time time = Time();
        time.setTime(temp);
        s = time.ISO8601();
        s.replace(13, 1, "-");
        s.replace(16, 1, "-");
    }
    
} // namespace openspace
