/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <ghoul/fmt.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/thread.h>

#include <chrono>
#include <cstring>
#include <fstream>
#include <stdio.h>
#include <thread>

#ifdef OPENSPACE_CURL_ENABLED
#ifdef WIN32
#pragma warning (push)
#pragma warning (disable: 4574) // 'INCL_WINSOCK_API_TYPEDEFS' is defined to be '0'
#endif // WIN32

#include <curl/curl.h>

#ifdef WIN32
#pragma warning (pop)
#endif // WIN32
#endif

namespace {
    constexpr const char* _loggerCat = "DownloadManager";

    struct ProgressInformation {
        std::shared_ptr<openspace::DownloadManager::FileFuture> future;
        std::chrono::system_clock::time_point startTime;
        const openspace::DownloadManager::DownloadProgressCallback* callback;
    };

    size_t writeData(void* ptr, size_t size, size_t nmemb, FILE* stream) {
        size_t written = fwrite(ptr, size, nmemb, stream);
        return written;
    }

    size_t writeMemoryCallback(void *contents, size_t size, size_t nmemb, void* userp) {
        size_t realsize = size * nmemb;
        auto mem = static_cast<openspace::DownloadManager::MemoryFile *>(userp);

        mem->buffer = reinterpret_cast<char*>(
            realloc(mem->buffer, mem->size + realsize + 1)
        );
        if (!mem->buffer) {
            // out of memory
            printf("not enough memory (realloc returned NULL)\n");
            return 0;
        }

        std::memcpy(&(mem->buffer[mem->size]), contents, realsize);
        mem->size += realsize;
        mem->buffer[mem->size] = 0;

        return realsize;
    }

    int xferinfo(void* p, curl_off_t dltotal, curl_off_t dlnow, curl_off_t, curl_off_t) {
        if (dltotal == 0) {
            return 0;
        }

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

        float s = static_cast<float>(
            std::chrono::duration_cast<std::chrono::seconds>(timeRemaining).count()
        );

        i->future->secondsRemaining = s;

        if (*(i->callback)) {
            // The callback function is a pointer to an std::function; that is the reason
            // for the excessive referencing
            (*(i->callback))(*(i->future));
        }

        return 0;
    }
} // namespace

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

DownloadManager::DownloadManager(bool useMultithreadedDownload)
    : _useMultithreadedDownload(useMultithreadedDownload)
{
    curl_global_init(CURL_GLOBAL_ALL);
}

std::shared_ptr<DownloadManager::FileFuture> DownloadManager::downloadFile(
    const std::string& url, const ghoul::filesystem::File& file, bool overrideFile,
    bool failOnError, unsigned int timeout_secs,
    DownloadFinishedCallback finishedCallback, DownloadProgressCallback progressCallback)
{
    if (!overrideFile && FileSys.fileExists(file)) {
        return nullptr;
    }

    std::shared_ptr<FileFuture> future = std::make_shared<FileFuture>(file.filename());
    errno = 0;
#ifdef WIN32
    FILE* fp;
    errno_t error = fopen_s(&fp, file.path().c_str(), "wb");
    if (error != 0) {
        LERROR(
            "Could not open/create file:" + file.path() +
            ". Errno: " + std::to_string(errno)
        );
    }
#else
    FILE* fp = fopen(file.path().c_str(), "wb"); // write binary
#endif // WIN32
    if (!fp) {
        LERROR(
            "Could not open/create file:" + file.path() +
            ". Errno: " + std::to_string(errno)
        );
    }

    auto downloadFunction = [url, failOnError, timeout_secs, finishedCallback,
                             progressCallback, future, fp]() {
        CURL* curl = curl_easy_init();
        if (curl) {
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeData);
            if (timeout_secs)
                curl_easy_setopt(curl, CURLOPT_TIMEOUT, timeout_secs);
            if (failOnError)
                curl_easy_setopt(curl, CURLOPT_FAILONERROR, 1L);

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

            if (res == CURLE_OK) {
                future->isFinished = true;
            }
            else {
                future->errorMessage = curl_easy_strerror(res);
            }

            if (finishedCallback) {
                finishedCallback(*future);
            }
        }
    };

    if (_useMultithreadedDownload) {
        std::thread t = std::thread(downloadFunction);

        using namespace ghoul::thread;
        ghoul::thread::setPriority(
            t,
            ghoul::thread::ThreadPriorityClass::Idle,
            ghoul::thread::ThreadPriorityLevel::Lowest
        );

        t.detach();
    }
    else {
        downloadFunction();
    }

    return future;
}

std::future<DownloadManager::MemoryFile> DownloadManager::fetchFile(
    const std::string& url,
    SuccessCallback successCallback, ErrorCallback errorCallback)
{
    LDEBUG(fmt::format("Start downloading file: '{}' into memory", url));

    auto downloadFunction = [url, successCallback, errorCallback]() {
        DownloadManager::MemoryFile file;
        file.buffer = reinterpret_cast<char*>(malloc(1));
        file.size = 0;
        file.corrupted = false;

        CURL* curl = curl_easy_init();
        if (!curl) {
            throw ghoul::RuntimeError("Error initializing cURL");
        }

        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, reinterpret_cast<void*>(&file));
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeMemoryCallback);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, 5L);
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, false);

        // Will fail when response status is 400 or above
        curl_easy_setopt(curl, CURLOPT_FAILONERROR, 1L);

        CURLcode res = curl_easy_perform(curl);
        if (res == CURLE_OK){
            // ask for the content-type
            char *ct;
            res = curl_easy_getinfo(curl, CURLINFO_CONTENT_TYPE, &ct);
            if (res == CURLE_OK){
                std::string extension = std::string(ct);
                std::stringstream ss(extension);
                getline(ss, extension ,'/');
                getline(ss, extension);
                file.format = extension;
            } else {
                LWARNING("Could not get extension from file downloaded from: " + url);
            }
            successCallback(file);
            curl_easy_cleanup(curl);
            return file;
        } else {
            std::string err = curl_easy_strerror(res);
            if (errorCallback) {
                errorCallback(err);
            } else {
                LWARNING(fmt::format("Error downloading '{}': {}", url, err));
            }
            curl_easy_cleanup(curl);
            // Throw an error and use try-catch around call to future.get()
            //throw std::runtime_error( err );

            // or set a boolean variable in MemoryFile to determine if it is
            // valid/corrupted or not.
            // Return MemoryFile even if it is not valid, and check if it is after
            // future.get() call.
            file.corrupted = true;
            return file;
        }
    };

    return std::async(std::launch::async, downloadFunction);
}

void DownloadManager::getFileExtension(const std::string& url,
    RequestFinishedCallback finishedCallback){

    auto requestFunction = [url, finishedCallback]() {
        CURL* curl = curl_easy_init();
        if (curl) {
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            //USING CURLOPT NOBODY
            curl_easy_setopt(curl, CURLOPT_NOBODY,1);
            CURLcode res = curl_easy_perform(curl);
            if (CURLE_OK == res) {
                char* ct;
                // ask for the content-type
                res = curl_easy_getinfo(curl, CURLINFO_CONTENT_TYPE, &ct);
                if ((res == CURLE_OK) && ct && finishedCallback) {
                    finishedCallback(std::string(ct));
                }
            }

            curl_easy_cleanup(curl);
        }
    };
    if (_useMultithreadedDownload) {
        using namespace ghoul::thread;
        std::thread t = std::thread(requestFunction);
        ghoul::thread::setPriority(
            t, ThreadPriorityClass::Idle, ThreadPriorityLevel::Lowest
        );
        t.detach();
    }
    else {
        requestFunction();
    }
}

} // namespace openspace
