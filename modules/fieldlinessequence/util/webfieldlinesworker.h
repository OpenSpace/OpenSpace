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

#ifndef __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESWORKER___H__
#define __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESWORKER___H__

#include <string>
#include <vector>
#include <openspace/util/timemanager.h>
#include <openspace/util/httprequest.h>

namespace openspace {
    
class WebFieldlinesWorker{
public:
    // Constructor
    WebFieldlinesWorker() = default;

    WebFieldlinesWorker& operator=(WebFieldlinesWorker&&) = default;

    ~WebFieldlinesWorker();
    
    WebFieldlinesWorker(std::string syncDir, std::string serverUrl);
    
    void getRangeOfAvailableTriggerTimes(double startTime, double endTime, std::vector<std::pair<double, std::string>>& _triggerTimesWeb);
    
    // Download all the steps within one window.
    // Spawn one thread per file to download?
    void downloadWindow(std::vector<std::pair<double, std::string>> triggerTimes);

    void updateRFSSourceFiles(std::vector<std::string>& _sourceFiles);

    // Returns true when the worker has downloaded a window and saved the path to the files in _sourceFiles
    bool windowIsComplete();

    // This is to set the updating flag false, use it once rednerablefieldlinessequence is notified of the ready update
    void flagUpdated();

    // Notifies the worker that a new window is ready
    void newWindowToDownload();

    // If the current window is on the edge of a datasets, but if there are some file in that window, it is still desired to download it.
    bool edgeMode();

private:

    // This list is the keep all the started downloads alive between frames, the second argument is a pair, used for identifying which download it is
    std::vector<AsyncHttpFileDownload> _downloadList;
    std::vector<std::pair<double,std::string>> _downloadListIdentifier;

    // Asynchttpdownloader worker.
    std::unique_ptr<AsyncHttpFileDownload> _downloading;
    std::unique_ptr<AsyncHttpMemoryDownload> _availableTimesDownloader;
    Time _maxTime;
    Time _minTime;

    // Might need this l8r
    std::pair<double, std::string> _latestDownload;

    // Contains a list of all the trigger times that has been downloaded already,
    std::vector<std::pair<double, std::string>> _downloadedTriggerTimes;
    
    // The base url for the server
    std::string _serverUrl;
    
    // The url of the endpoint to fetch one file based on downloadkey
    std::string _endpointSingleDownload;
    
    // The directory for downloaded files. Passed down from WebFieldlinesManager.
    std::string _syncDir;

    // Fileextension is used for the destructor
    std::string _fileExtension = "";
    
    // TODO(Axel): Hmm, can we get around using these bools somehow? 
    bool _readyToUpdateSourceFiles = false;
    bool _doneUpdating = false;
    bool _newWindow = false;
    bool _readyToDownload = true;
    bool _noMoreRequests = false;
    bool _bigWindowHasData = false;
    bool _downloadedSomething = false;
    bool _requestSent = false;
    unsigned int _strikes = 0;
    std::pair<double, double> acceptableToStartRequestingAgain = std::make_pair(0.0, 0.0);
    
    // Download one file to sync directory
    std::string downloadOsfls(std::pair<double, std::string> downloadKey);
    
    bool fileIsOnDisk(double triggerTime);
    
    /********************************************
    |               Helper Functions            |
    |                                           |
    ********************************************/

    // Parse the data list from http request
    void parseTriggerTimesList(std::string s, std::vector<std::tuple<double, std::string, int>>& _triggerTimesWeb);
    
    // functions to translate the filenames to doubles
    double triggerTimeString2Double(std::string s);
    void triggerTimeDouble2String(double d, std::string& s);

    // Add a downloaded file to the list of downloaded files
    void addToDownloadedList(std::pair<double, std::string> pair);
    
    // Compares two trigger times, since they are doubles, that may be weird otherwise.
    static bool compareTimetriggersEqual(double first, double second);

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESWORKER___H__
