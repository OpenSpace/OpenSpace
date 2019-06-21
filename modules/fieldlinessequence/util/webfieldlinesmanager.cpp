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
    
    WebFieldlinesManager::WebFieldlinesManager(std::string syncDir){
        
        // Using constructor for some testing
        ghoul::filesystem::File tempFile = ghoul::filesystem::File(syncDir);
        _syncDir = tempFile.directoryName();
        _flsType = PFSSIO;
        _flsTypeString = "PFSSIO";
        
        std::string testTime;
        triggerTimeInt2String(610056000, testTime);
        

        int testInt;
        triggerTimeString2Int(testTime, testInt);
        
        getAvailableTriggertimes();
        
        
        for (auto& tt : _availableTriggertimes){
            downloadOsfls(_flsType, tt);
        }
    }
    
    void WebFieldlinesManager::update(){
        
    }
    
    void WebFieldlinesManager::downloadOsfls(FlsType type, std::string triggertime){
            std::string url = "http://localhost:3000/WSA/" + triggertime;
            std::string destinationpath = absPath(_syncDir + '/' + triggertime);
            AsyncHttpFileDownload ashd = AsyncHttpFileDownload(url, destinationpath, HttpFileDownload::Overwrite::Yes);
            HttpRequest::RequestOptions opt = {};
            opt.requestTimeoutSeconds = 0;
            ashd.start(opt);
            ashd.wait();
    }
    
    
    void WebFieldlinesManager::getAvailableTriggertimes(){
        std::string url = "http://localhost:3000/WSA/available/" + _flsTypeString;
        SyncHttpMemoryDownload mmryDld = SyncHttpMemoryDownload(url);
        HttpRequest::RequestOptions opt = {};
        opt.requestTimeoutSeconds = 0;
        mmryDld.download(opt);
        
        // Put the results in a string and remove [ ]
        std::string s;
        std::transform(mmryDld.downloadedData().begin(), mmryDld.downloadedData().end(), std::back_inserter(s),
                       [](char c) {
                           return c;
                       });
        parseTriggerTimesList(s);
    }
    
    void WebFieldlinesManager::setInitialSet(double openSpaceTime){
        
    }
    
    void WebFieldlinesManager::downloadInitialSequence(std::vector<double> triggertimes){
        
    }
    
    void WebFieldlinesManager::updateStartTimes(){
        
    }
    
    void WebFieldlinesManager::parseTriggerTimesList(std::string s){
        // Turn into stringstream to parse the comma-delimited string into vector
        std::stringstream ss(s);
        char c;
        std::string substr;
        while(ss >> c)
        {
            if (c == '[' || c == ']' || c == '"' ) continue;
            else if (c == ','){
                _availableTriggertimes.push_back(substr);
                substr.clear();
            }
            else substr += c;
        }
        _availableTriggertimes.push_back(substr);
    }
    
    void WebFieldlinesManager::triggerTimeString2Int(std::string s, int& i){
        s.replace(13, 1, ":");
        s.replace(16, 1, ":");
        Time time = Time();
        time.setTime(s);
        i =  static_cast<int> (time.j2000Seconds() - 69.185013294);
        
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
