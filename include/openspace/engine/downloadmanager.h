/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __DOWNLOADMANAGER_H__
#define __DOWNLOADMANAGER_H__

#include <ghoul/designpattern/singleton.h>

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/directory.h>

#include <functional>
#include <memory>
#include <string>
#include <vector>
#include <future>

namespace openspace {

// Multithreaded
class DownloadManager : public ghoul::Singleton<DownloadManager> {
public:
    struct FileFuture {
        // Since the FileFuture object will be used from multiple threads, we have to be
        // careful about the access pattern, that is, no values should be read and written
        // by both the DownloadManager and the outside threads.
        FileFuture(std::string file);

        // Values that are written by the DownloadManager to be consumed by others
        long long currentSize;
        long long totalSize;
        float progress; // [0,1]
        float secondsRemaining;
        bool isFinished;
        bool isAborted;
        std::string filePath;
        std::string errorMessage;
        std::string format;
        // Values set by others to be consumed by the DownloadManager
        bool abortDownload;
    };

    struct MemoryFile {
        std::string buffer;
        std::string format;
        bool corrupted;
    };

    using DownloadProgressCallback = std::function<void(const FileFuture&)>;
    using DownloadFinishedCallback = std::function<void(const FileFuture&)>;

    using SuccessCallback = std::function<void(const MemoryFile&)>;
    using ErrorCallback = std::function<void(const std::string&)>;

    using RequestFinishedCallback = std::function<void(std::string)>;
    using AsyncDownloadFinishedCallback =
        std::function<void(const std::vector<std::shared_ptr<FileFuture>>&)>;

    //Just a helper function to check if a future is ready to ".get()". Not specific
    // to DownloadManager but is useful for anyone using the DownloadManager
    template<typename R>
    static bool futureReady(std::future<R> const& f)
    { return f.wait_for(std::chrono::seconds(0)) == std::future_status::ready; }

    DownloadManager(std::string requestURL, int applicationVersion,
        bool useMultithreadedDownload = true);

    // callbacks happen on a different thread
    std::shared_ptr<FileFuture> downloadFile(const std::string& url, const ghoul::filesystem::File& file,
        bool overrideFile = true,
        DownloadFinishedCallback finishedCallback = DownloadFinishedCallback(),
        DownloadProgressCallback progressCallback = DownloadProgressCallback()
    );

    std::shared_ptr<FileFuture> downloadToMemory(
        const std::string& url, std::string& memoryBuffer,
        DownloadFinishedCallback finishedCallback = DownloadFinishedCallback()
    );

    std::future<MemoryFile> fetchFile(
    const std::string& url,
    SuccessCallback successCallback = SuccessCallback(), ErrorCallback errorCallback = ErrorCallback());

    std::vector<std::shared_ptr<FileFuture>> downloadRequestFiles(const std::string& identifier,
        const ghoul::filesystem::Directory& destination, int version,
        bool overrideFiles = true,
        DownloadFinishedCallback finishedCallback = DownloadFinishedCallback(),
        DownloadProgressCallback progressCallback = DownloadProgressCallback()
    );

    void downloadRequestFilesAsync(const std::string& identifier,
        const ghoul::filesystem::Directory& destination, int version,
        bool overrideFiles, AsyncDownloadFinishedCallback callback
    );

    void getFileExtension(const std::string& url,
        RequestFinishedCallback finishedCallback = RequestFinishedCallback());

private:
    std::vector<std::string> _requestURL;
    int _applicationVersion;
    bool _useMultithreadedDownload;
};

#define DlManager (openspace::DownloadManager::ref())

} // namespace openspace

#endif // __DOWNLOADMANAGER_H__
