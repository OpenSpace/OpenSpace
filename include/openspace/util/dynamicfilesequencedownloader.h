/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___DYNAMICFILESEQUENCEDOWNLOADER___H__
#define __OPENSPACE_CORE___DYNAMICFILESEQUENCEDOWNLOADER___H__

#include <openspace/util/httprequest.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>
#include <map>
#include <memory>
#include <queue>
#include <string>
#include <unordered_map>
#include <vector>

namespace openspace {

struct File {
    std::unique_ptr<HttpFileDownload> download;
    std::string timestep;
    double time = 0.0;
    std::string URL;
    std::filesystem::path path;
    double cadence = 0.0;
    int availableIndex = -1;
    enum class State {
        Available,
        OnQueue,
        Downloading,
        Downloaded,
        Unknown
    };
    State state = State::Unknown;
};

class DynamicFileSequenceDownloader {
public:
    DynamicFileSequenceDownloader(int dataID, std::string identifier, std::string infoUrl,
        std::string dataUrl, size_t nFilesToQueue);

    void deinitialize(bool cacheFiles);
    void requestDataInfo(std::string httpInfoRequest);
    void requestAvailableFiles(std::string httpDataRequest,
        std::filesystem::path syncDir);
    std::vector<File>::iterator closestFileToNow(double time);
    void update(double time, double deltaTime);
    const std::vector<std::filesystem::path>& downloadedFiles() const;
    void checkForFinishedDownloads();
    void clearDownloaded();
    const std::filesystem::path& destinationDirectory() const;
    bool areFilesCurrentlyDownloading() const;
    std::vector<File*>& filesCurrentlyDownloading();

private:
    void downloadFile();
    double calculateCadence() const;
    void putInQueue();

    bool _isForwardDirection = true;
    bool _isFirstFrame = true;
    bool _isSecondFrame = true;
    bool _hasNotifiedTooFast = false;

    std::filesystem::path _syncDir;
    std::filesystem::path _trackSynced;
    const int _dataID;
    const std::string _infoUrl;
    const std::string _dataUrl;

    double _dataMinTime;
    double _dataMaxTime;

    const size_t _nFilesToQueue = 0;

    std::vector<File>::iterator _currentFile;

    std::vector<File> _availableData;
    std::vector<File*> _queuedFilesToDownload;
    std::vector<File*> _filesCurrentlyDownloading;
    std::vector<std::filesystem::path> _downloadedFiles;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___DYNAMICFILESEQUENCEDOWNLOADER___H__
