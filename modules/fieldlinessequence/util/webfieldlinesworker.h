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

namespace openspace {
    
class WebFieldlinesWorker{
public:
    // Constructor
    WebFieldlinesWorker() = default;

    ~WebFieldlinesWorker();
    
    WebFieldlinesWorker(std::string syncDir, std::string serverUrl, std::shared_ptr<std::vector<std::pair<double, std::string>>> _triggerTimesOnDisk);
    
    void getRangeOfAvailableTriggerTimes(double start, double end, std::vector<std::tuple<double, std::string, int>>& _triggerTimesWeb);
    
    // Sets the third index in _triggerTimesWeb to whether it is on disk already or not
    // If it's on disk, get the index of the corresponding triggertime in _triggerTimesOnDisk
    // This means the _triggerTimesOnDisk can't be sorted or shuffled. We could also use pointers instead?
    void checkForExistingData(std::vector<std::tuple<double, std::string, int>>& _triggerTimesWeb,
                                                   std::vector<std::pair<double, std::string>>& _triggerTimesOnDisk);
    // Download all the steps within one window.
    // Spawn one thread per file to download?
    void downloadWindow(std::vector<std::pair<double, std::string>> triggerTimes);

    void updateRFSSourceFiles(std::vector<std::string>& _sourceFiles);

    // Returns true when the worker has downloaded a window and saved the path to the files in _sourceFiles
    bool windowIsComplete();

private:
    // POINTERS
    std::shared_ptr<std::vector<std::pair<double, std::string>>> _triggerTimesOnDisk;
    // Pointers end

    // Contains a list of all the trigger times that has been downloaded already,
    std::vector<std::pair<double, std::string>> _downloadedTriggerTimes;
    
    // The base url for the server
    std::string _serverUrl;
    
    // The url of the endpoint to fetch one file based on downloadkey
    std::string _endpointSingleDownload;
    
    // The directory for downloaded files. Passed down from WebFieldlinesManager.
    std::string _syncDir;
    
    // File format, only .oslfs for now
    std::string _fileEnding;
    
    std::string _FLType;

    /********************************************
    |               USED FOR THREADS            |
    |                                           |
    ********************************************/
    //std::condition_variable _workerConditional;
    //std::mutex _workerMutex;

    // TODO(Axel): Hmm, can we get around using these bools somehow? 
    bool _readyToUpdateSourceFiles = false;
    bool _doneUpdating = false;
    
    // Download one file to sync directory
    std::string downloadOsfls(std::string downloadkey);
    
    bool fileIsOnDisk(double triggerTime);
    
    // HELPER FUNCTIONS
    // Parse the data list from http request
    void parseTriggerTimesList(std::string s, std::vector<std::tuple<double, std::string, int>>& _triggerTimesWeb);
    
    // functions to translate the filenames to doubles
    double triggerTimeString2Double(std::string s);
    void triggerTimeDouble2String(double d, std::string& s);

    // Add a downloaded file to the list of downloaded files
    void addToDownloadedList(std::pair<double, std::string> pair);
    
    static bool compareTimetriggersEqual(double first, double second);

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_FIELDLINESSEQUENCE___WEBFIELDLINESWORKER___H__
