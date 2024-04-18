/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#ifndef __OPENSPACE_CORE___DOWNLOADMANAGER___H__
#define __OPENSPACE_CORE___DOWNLOADMANAGER___H__

#include <ghoul/misc/boolean.h>
#include <filesystem>
#include <functional>
#include <future>
#include <memory>
#include <string>
#include <vector>

namespace ghoul::filesystem { class File; }

namespace openspace {

// Multithreaded
class DownloadManager {
public:
    struct FileFuture {
        // Since the FileFuture object will be used from multiple threads, we have to be
        // careful about the access pattern, that is, no values should be read and written
        // by both the DownloadManager and the outside threads.
        FileFuture(std::filesystem::path file);

        // Values that are written by the DownloadManager to be consumed by others
        long long currentSize = -1;
        long long totalSize = -1;
        float progress = 0.f; // [0,1]
        float secondsRemaining = -1.f;
        bool isFinished = false;
        bool isAborted = false;
        std::filesystem::path filePath;
        std::string errorMessage;
        std::string format;
        // Values set by others to be consumed by the DownloadManager
        bool abortDownload = false;
    };

    struct MemoryFile {
        char* buffer = nullptr;
        size_t size = 0;
        std::string format;
        bool corrupted = false;
    };

    BooleanType(UseMultipleThreads);
    BooleanType(OverrideFile);
    BooleanType(FailOnError);


    using DownloadProgressCallback = std::function<void(const FileFuture&)>;
    using DownloadFinishedCallback = std::function<void(const FileFuture&)>;

    using SuccessCallback = std::function<void(const MemoryFile&)>;
    using ErrorCallback = std::function<void(const std::string&)>;

    using RequestFinishedCallback = std::function<void(std::string)>;
    using AsyncDownloadFinishedCallback =
        std::function<void(const std::vector<std::shared_ptr<FileFuture>>&)>;

    // Just a helper function to check if a future is ready to ".get()". Not specific
    // to DownloadManager but is useful for anyone using the DownloadManager
    template<typename R>
    static bool futureReady(std::future<R> const& f) {
        return f.wait_for(std::chrono::seconds(0)) == std::future_status::ready;
    }

    DownloadManager(UseMultipleThreads useMultipleThreads = UseMultipleThreads::Yes);

    //downloadFile
    // url - specifies the target of the download
    // file - specifies path to local saved file
    // overrideFile - if true, overrides existing file of same name
    // failOnError - if true, http codes >= 400 (client/server errors) result in fail
    // timeout_secs - timeout in seconds before giving up on download (0 = no timeout)
    // finishedCallback - callback when download finished (happens on different thread)
    // progressCallback - callback for status during (happens on different thread)
    std::shared_ptr<FileFuture> downloadFile(const std::string& url,
        const std::filesystem::path& file,
        OverrideFile overrideFile = OverrideFile::Yes,
        FailOnError failOnError = FailOnError::No, unsigned int timeout_secs = 0,
        DownloadFinishedCallback finishedCallback = DownloadFinishedCallback(),
        DownloadProgressCallback progressCallback = DownloadProgressCallback()) const;

    std::future<MemoryFile> fetchFile(const std::string& url,
        SuccessCallback successCallback = SuccessCallback(),
        ErrorCallback errorCallback = ErrorCallback());

    void fileExtension(const std::string& url,
        RequestFinishedCallback finishedCallback = RequestFinishedCallback()) const;

private:
    bool _useMultithreadedDownload;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___DOWNLOADMANAGER___H__
