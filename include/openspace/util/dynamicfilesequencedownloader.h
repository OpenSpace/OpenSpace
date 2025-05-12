/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#ifndef __OPENSPACE_MODULE_SYNC___DYNAMICFILESEQUENCEDOWNLOADER___H__
#define __OPENSPACE_MODULE_SYNC___DYNAMICFILESEQUENCEDOWNLOADER___H__

#include <openspace/util/httprequest.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>
#include <queue>

namespace openspace {

struct File {
    std::unique_ptr<HttpFileDownload> download;
    std::string timestep;
    double time;
    std::string URL;
    std::filesystem::path path;
    //TODO: need Cadence?
    double cadence;
    int availableIndex;
    enum class State {
        Available,
        OnQueue,
        Downloading,
        Downloaded
    };
    State state;
};

class DynamicFileSequenceDownloader {
public:
    //Constructor of dynamic downloader. Add one last parameter to change number of files
    //that it will keep around during run time instead of the default of 100 files.
    DynamicFileSequenceDownloader(
        int dataID, const std::string infoURL, const std::string dataURL, size_t nOfFilesToQ
    );
    void deinitialize(bool cacheFiles);
    void requestDataInfo(std::string httpInfoRequest);
    void requestAvailableFiles(std::string httpDataRequest, std::filesystem::path syncdir);
    std::vector<File>::iterator closestFileToNow(const double time); //const File&
    void update(const double time, const double deltaTime);
    const std::vector<std::filesystem::path>& downloadedFiles();
    void checkForFinishedDownloads();
    void clearDownloaded();
    std::filesystem::path destinationDirectory();
    bool areFilesCurrentlyDownloading();
    std::vector<File*>& filesCurrentlyDownloading();

private:

    void downloadFile();
    double calculateCadence();
    void putOnQueue();
    bool _forward = true;
    bool _firstFrame = true;
    bool _secondFrame = true;

    std::filesystem::path _syncDir;
    std::filesystem::path _trackSynced;
    int _dataID;
    const std::string _infoURL;
    const std::string _dataURL;

    double _dataMinTime;
    double _dataMaxTime;
    //std::string _dataIdDescription;
    //Currently have both a global cadence and it's assigned to every element too.
    double _tempCadence = 0;
    const size_t _nOfFilesToQueue = 0;

    //to iterate window
    std::vector<File>::iterator _thisFile;

    std::vector<File> _availableData;
    std::vector<File*> _queuedFilesToDownload;
    std::vector<File*> _filesCurrentlyDownloading;
    std::vector<std::filesystem::path> _downloadedFiles;

};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SYNC___DYNAMICFILESEQUENCEDOWNLOADER___H__
