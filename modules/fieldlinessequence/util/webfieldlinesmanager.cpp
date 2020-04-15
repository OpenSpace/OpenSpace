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

#include <ghoul/filesystem/cachemanager.h>

 // Tracy
//#include <Tracy.hpp>

namespace {
    constexpr const char* _loggerCat = "WebFieldlinesManager";
    
} // namepace


namespace openspace{

// --------------------------- PUBLIC FUNCTIONS --------------------------- //

// IMPORTANT: This initializing function must be called AFTER initializeSyncDirectory
// TODO Might be included in this function in the future.
void WebFieldlinesManager::initializeWebFieldlinesManager(std::string identifier,
                                                          std::string url,
                                            std::vector<std::string>& _sourceFiles,
                                                 std::vector<double>& _startTimes)
{
    // Initialize the sliding window
    _webFieldlinesWindow =
        WebFieldlinesWindow(_syncDir, url, _sourceFiles, _startTimes);

    LTRACE("WebFieldlinesManager initialized " + identifier);
}
    
// Make sure that the sync directory exists
// Also creates a new directory in the web_fieldlines directory corresponding to the
// field line identifier. The separation of this sync-directory initialization and
// the initialization function of the class is because of RFLS, that can't
// handle empty directories to begin with.

std::string WebFieldlinesManager::initializeSyncDirectory(std::string identifier, 
                                 std::unique_ptr<ghoul::Dictionary> & dictionary) {

    // this does not work since cacheManager assumes that the given
    // file is the one being cached
        // std::string path = FileSys.cacheManager()->cachedFilename(
        //     identifier,
        //     "WebFieldlinesManager",
        //     ghoul::filesystem::CacheManager::Persistent::No
        // );
        // //remove file name
        // ghoul::filesystem::File f(path);
        // path = f.directoryName();

    std::string path = absPath("${TEMPORARY}") + FileSys.PathSeparator + identifier;
        
    if (!FileSys.directoryExists(path)) {
        FileSys.createDirectory(path);
    }
    
    _syncDir = path;    
    return _syncDir;
}
    
// Temporary function - this should be moved to the worker. It's to download
// the start latest line
// this could be used to test the connection too
void WebFieldlinesManager::preDownload(std::string dUrl){

    int ID = std::stoi(dUrl.substr(dUrl.size() - 4));
    std::string type = "";
    switch (ID)
    {
    case 1176:
        type = "trace_sun_earth";
        break;
    case 1177:
        type = "trace_scs_outtoin";
        break;
    case 1178:
        type = "trace_pfss_intoout";
        break;
    case 1179:
        type = "trace_pfss_outtoin";
        break;
    case 1180:
        type = "WSA_OUT";
        break;

    // Parker solar probe endpoints
    case 1192:
        type = "trace_sub_psp";
        break;
    case 1193:
        type = "trace_scs_outtoin";
        break;
    case 1194:
        type = "trace_pfss_intoout";
        break;
    case 1195:
        type = "trace_pfss_outtoin";
        break;
    case 1196:
        type = "ADAPT_WSA_OUT";
        break;
    }

    // TODO(Axel): Change this endpoint to be dynamic for each dataset 
    std::string url = "https://iswa.ccmc.gsfc.nasa.gov/iswa_data_tree/model/solar/";
    url += "WSA4.5/WSA4.5_fieldlines/" + type + "/2017/09/2017-09-28T00-23-22.000.osfls";
    std::string destinationpath = absPath(_syncDir + 
        ghoul::filesystem::FileSystem::PathSeparator + 
        "2017-09-28T00-23-22.000.osfls"); // what the downloaded filename is to be
       

    /* For experimentation with suntexturemanager & parker solor probe, 
       hopefully this hardcoding can go away */
    if (ID == 1180) {
        url = "https://iswa.ccmc.gsfc.nasa.gov/iswa_data_tree/model/solar/WSA4.5/" 
            + type + "/2017/09/wsa_201709280023R000_gong.fits";
        destinationpath = absPath(_syncDir + 
            ghoul::filesystem::FileSystem::PathSeparator + 
            "wsa_201709280023R000_gong.fits"); // what the downloaded filename is to be
    }
    // For parker solar probe
    if (ID > 1190) {
        url = "https://iswa.ccmc.gsfc.nasa.gov/iswa_data_tree/model/solar/ADAPT_WSA4.5/";
        url += "ADAPT_WSA4.5_fieldlines/" + type + 
            "/2018/11/2018-11-01T00-00-00.000.osfls";
        destinationpath = absPath(_syncDir + 
            ghoul::filesystem::FileSystem::PathSeparator + 
            "2018-11-01T00-00-00.000.osfls"); // what the downloaded filename is to be
    }

    if (ID == 1196) {
        url = "https://iswa.ccmc.gsfc.nasa.gov/iswa_data_tree/model/solar/ADAPT_WSA4.5/" 
            + type + "/2018/11/wsa_201811010000R007_agong.fits";
        destinationpath = absPath(_syncDir + 
            ghoul::filesystem::FileSystem::PathSeparator + 
            "wsa_201811010000R007_agong.fits"); // what the downloaded filename is to be
    }
    /* End of experiment */

    AsyncHttpFileDownload ashd = AsyncHttpFileDownload(url, destinationpath, 
                                          HttpFileDownload::Overwrite::Yes);
    HttpRequest::RequestOptions opt = {};
    opt.requestTimeoutSeconds = 0;
    ashd.start(opt);
    ashd.wait();
}
    
    
void WebFieldlinesManager::update(){
    const double openspaceTime = global::timeManager.time().j2000Seconds();
    const auto deltaTime = global::timeManager.deltaTime();

    // More than 2hrs a second would generally be unfeasable
    // for a regular internet connection to operate at
    const int speedThreshhold = 4000;

        
    // Hold your horses, we don't want to do anything while deltatime is too high
    if (abs(deltaTime) < speedThreshhold) {
        // First it checks the time against the "bigger window" aka the long list of
        // timesteps we know are available online.
        // If it's outside that we're gonna need a new one
        if (_webFieldlinesWindow.timeIsInTriggerTimesWebList(openspaceTime) &&
            !_webFieldlinesWindow.expectedWindowIsOutOfBounds(openspaceTime) ||
            _webFieldlinesWindow.checkWorkerEdgeMode())
        {
            //ZoneScopedN("time is in, not out of bounds")

            // Check if in window
            if (_webFieldlinesWindow.edgeWindowReady() ||
                _webFieldlinesWindow.timeIsInWindow(openspaceTime))
            {
                //ZoneScopedN("time is in window")

                // Check if in the edge of the window,
                // so we can start downloading a new one
                if (!_webFieldlinesWindow.edgeWindowReady() &&
                    _webFieldlinesWindow.timeIsInWindowMargin(openspaceTime, deltaTime))
                {
                    //ZoneScopedN("If time is in window margin. newWindow()")
                    // get new window
                    _webFieldlinesWindow.newWindow(openspaceTime);
                    hasUpdated = false;
                }
                else {
                    //ZoneScopedN("download worker")

                    // If it's in the middle of the window,
                    // we can just sit back and relax and let the worker work
                    _webFieldlinesWindow.executeDownloadWorker();
                    if (_webFieldlinesWindow.workerWindowIsReady()) {
                        hasUpdated = false;
                    }
                }
            }
            else {
                //ZoneScopedN("Big Else! New window")

                // get new window

                _webFieldlinesWindow.newWindow(openspaceTime);
                hasUpdated = false;
            }
        }
        else {
            //ZoneScopedN("even bigger else. Get new trigger times web list")

            _webFieldlinesWindow.getNewTriggerTimesWebList(openspaceTime);
        }
    }
}

bool WebFieldlinesManager::isWindowReadyToLoad()
{
    return _webFieldlinesWindow.workerWindowIsReady();
}

void WebFieldlinesManager::resetWorker() {
    _webFieldlinesWindow.rfsHasUpdated();
}
    
// --------------------------- PRIVATE FUNCTIONS --------------------------- //

std::string WebFieldlinesManager::getDirectory(){
    return _syncDir;
}

} // namespace openspace
