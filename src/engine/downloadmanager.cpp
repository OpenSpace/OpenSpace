/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/engine/downloadmanager.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>

#include <chrono>
#include <fstream>
#include <thread>

#ifdef OPENSPACE_CURL_ENABLED
#include <curl/curl.h>
#endif

#ifdef WIN32
#include <Windows.h>
#endif

#define USE_MULTITHREADED_DOWNLOAD

namespace {
    const std::string _loggerCat = "DownloadManager";
    
    const std::string RequestIdentifier = "identifier";
    const std::string RequestFileVersion = "file_version";
    const std::string RequestApplicationVersion = "application_version";
    
    struct ProgressInformation {
        openspace::DownloadManager::FileFuture* future;
        std::chrono::system_clock::time_point startTime;
        const openspace::DownloadManager::DownloadProgressCallback* callback;
    };

    size_t writeData(void* ptr, size_t size, size_t nmemb, FILE* stream) {
        size_t written;
        written = fwrite(ptr, size, nmemb, stream);
        return written;
    }


    int xferinfo(void* p, curl_off_t dltotal, curl_off_t dlnow, curl_off_t ultotal,
                 curl_off_t ulnow)
    {
        if (dltotal == 0)
            return 0;

        ghoul_assert(p, "Passed progress information is nullptr");
        ProgressInformation* i = static_cast<ProgressInformation*>(p);
        ghoul_assert(i, "Passed pointer is not a ProgressInformation");
        ghoul_assert(i->future, "FileFuture is not initialized");
        ghoul_assert(i->callback, "Callback pointer is nullptr");
        
        if (i->future->abortDownload) {
            i->future->isAborted = true;
            return 1;
        }

        i->future->currentSize = dlnow;
        i->future->totalSize = dltotal;
        i->future->progress = static_cast<float>(dlnow) / static_cast<float>(dltotal);

        auto now = std::chrono::system_clock::now();

        // Compute time spent transferring.
        auto transferTime = now - i->startTime;
        // Compute estimated transfer time.
        auto estimatedTime = transferTime / i->future->progress;
        // Compute estimated time remaining.
        auto timeRemaining = estimatedTime - transferTime;

        float s = std::chrono::duration_cast<std::chrono::seconds>(timeRemaining).count();

        i->future->secondsRemaining = s;

        if (*(i->callback)) {
            // The callback function is a pointer to an std::function; that is the reason
            // for the excessive referencing
            (*(i->callback))(*(i->future));
        }
 
        return 0;
    }
}

namespace openspace {

DownloadManager::FileFuture::FileFuture(std::string file)
    : currentSize(-1)
    , totalSize(-1)
    , progress(0.f)
    , secondsRemaining(-1.f)
    , isFinished(false)
    , isAborted(false)
    , filePath(std::move(file))
    , errorMessage("")
    , abortDownload(false)
{}

DownloadManager::DownloadManager(std::string requestURL, int applicationVersion,
                                 bool useMultithreadedDownload)
    : _applicationVersion(std::move(applicationVersion))
    , _useMultithreadedDownload(useMultithreadedDownload)
{
    curl_global_init(CURL_GLOBAL_ALL);
    
    _requestURL.push_back(std::move(requestURL));
    
    // TODO: Check if URL is accessible ---abock
    // TODO: Allow for multiple requestURLs
}

DownloadManager::FileFuture* DownloadManager::downloadFile(
    const std::string& url, const ghoul::filesystem::File& file, bool overrideFile,
    DownloadFinishedCallback finishedCallback, DownloadProgressCallback progressCallback)
{
    if (!overrideFile && FileSys.fileExists(file))
        return nullptr;

    FileFuture* future = new FileFuture(file.filename());
    FILE* fp = fopen(file.path().c_str(), "wb");

    LDEBUG("Start downloading file: '" << url << "' into file '" << file.path() << "'");
    
    auto downloadFunction = [url, finishedCallback, progressCallback, future, fp]() {
        CURL* curl = curl_easy_init();
        if (curl) {
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeData);
            
            ProgressInformation p = {
                future,
                std::chrono::system_clock::now(),
                &progressCallback
            };
            curl_easy_setopt(curl, CURLOPT_XFERINFOFUNCTION, xferinfo);
            curl_easy_setopt(curl, CURLOPT_XFERINFODATA, &p);
            curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0L);
            
            CURLcode res = curl_easy_perform(curl);
            curl_easy_cleanup(curl);
            fclose(fp);
            
            if (res == CURLE_OK)
                future->isFinished = true;
            else
                future->errorMessage = curl_easy_strerror(res);
            
            if (finishedCallback)
                finishedCallback(*future);
        }
    };
    
    if (_useMultithreadedDownload) {
        std::thread t = std::thread(downloadFunction);
     
#ifdef WIN32
        std::thread::native_handle_type h = t.native_handle();
        SetPriorityClass(h, IDLE_PRIORITY_CLASS);
        SetThreadPriority(h, THREAD_PRIORITY_LOWEST);
#else
        // TODO: Implement thread priority ---abock
#endif
        
        t.detach();
    }
    else {
        downloadFunction();
    }
    
    return future;
}

std::vector<DownloadManager::FileFuture*> DownloadManager::downloadRequestFiles(
    const std::string& identifier, const ghoul::filesystem::Directory& destination,
    int version, bool overrideFiles, DownloadFinishedCallback finishedCallback,
    DownloadProgressCallback progressCallback)
{
    std::vector<FileFuture*> futures;
    FileSys.createDirectory(destination, true);
    // TODO: Check s ---abock
    // TODO: Escaping is necessary ---abock
    const std::string fullRequest =_requestURL.back() + "?" +
        RequestIdentifier + "=" + identifier + "&" +
        RequestFileVersion + "=" + std::to_string(version) + "&" +
        RequestApplicationVersion + "=" + std::to_string(_applicationVersion);
    LDEBUG("Request: " << fullRequest);

    std::string requestFile = absPath("${TEMPORARY}/" + identifier);
    LDEBUG("Request File: " << requestFile);

    bool isFinished = false;
    auto callback = [&futures, destination, &progressCallback, &isFinished, requestFile, overrideFiles](const FileFuture& f) {
        LDEBUG("Finished: " << requestFile);
        std::ifstream temporary(requestFile);
        std::string line;
        int nFiles = 0;
        int nFinished = 0;
        while (std::getline(temporary, line)) {
            ++nFiles;
#ifdef __APPLE__
            // @TODO: Fix this so that the ifdef is not necessary anymore ---abock
            std::string file = ghoul::filesystem::File(line, true).filename();
#else
            std::string file = ghoul::filesystem::File(line).filename();
#endif

            LDEBUG("\tLine: " << line << " ; Dest: " << destination.path() + "/" + file);

            FileFuture* future = DlManager.downloadFile(
                line,
                destination.path() + "/" + file,
                overrideFiles,
                [](const FileFuture& f) { LDEBUG("Finished: " << f.filePath); }
            );
            if (future)
                futures.push_back(future);
        }
        isFinished = true;
    };
    
    FileFuture* f = downloadFile(
        fullRequest,
        requestFile,
        true,
        callback
    );

    while (!isFinished) {}

    return futures;
}

void DownloadManager::downloadRequestFilesAsync(const std::string& identifier,
    const ghoul::filesystem::Directory& destination, int version, bool overrideFiles,
    AsyncDownloadFinishedCallback callback)
{
    auto downloadFunction = [this, identifier, destination, version, overrideFiles, callback](){
        std::vector<FileFuture*> f = downloadRequestFiles(
            identifier,
            destination,
            version,
            overrideFiles
        );
        
        callback(f);
    };
    
    if (_useMultithreadedDownload) {
        std::thread t = std::thread(downloadFunction);
        
#ifdef WIN32
        std::thread::native_handle_type h = t.native_handle();
        SetPriorityClass(h, IDLE_PRIORITY_CLASS);
        SetThreadPriority(h, THREAD_PRIORITY_LOWEST);
#else
        // TODO: Implement thread priority ---abock
#endif
        
        t.detach();
    }
    else
        downloadFunction();
}

} // namespace openspace
