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

#include <openspace/engine/downloadmanager.h>

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/misc/thread.h>
#include <curl/curl.h>
#include <chrono>
#include <filesystem>
#include <sstream>
#include <thread>

namespace {
    constexpr std::string_view _loggerCat = "DownloadManager";

    struct ProgressInformation {
        std::shared_ptr<openspace::DownloadManager::FileFuture> future;
        std::chrono::system_clock::time_point startTime;
        const openspace::DownloadManager::DownloadProgressCallback* callback;
    };

    size_t writeData(void* ptr, size_t size, size_t nmemb, FILE* stream) {
        const size_t written = fwrite(ptr, size, nmemb, stream);
        return written;
    }

    size_t writeMemoryCallback(void* contents, size_t size, size_t nmemb, void* userp) {
        const size_t realsize = size * nmemb;
        auto* mem = static_cast<openspace::DownloadManager::MemoryFile*>(userp);

        // @TODO(abock): Remove this and replace mem->buffer with std::vector<char>
        mem->buffer = reinterpret_cast<char*>(
            realloc(mem->buffer, mem->size + realsize + 1)
        );

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
        ghoul_assert(i && i->future, "FileFuture is not initialized");
        ghoul_assert(i && i->callback, "Callback pointer is nullptr");

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

        i->future->secondsRemaining = static_cast<float>(
            std::chrono::duration_cast<std::chrono::seconds>(timeRemaining).count()
        );

        if (*(i->callback)) {
            // The callback function is a pointer to an std::function; that is the reason
            // for the excessive referencing
            (*(i->callback))(*(i->future));
        }

        return 0;
    }
} // namespace

namespace openspace {

DownloadManager::FileFuture::FileFuture(std::filesystem::path file)
    : filePath(std::move(file))
{}

DownloadManager::DownloadManager(UseMultipleThreads useMultipleThreads)
    : _useMultithreadedDownload(useMultipleThreads)
{
    curl_global_init(CURL_GLOBAL_ALL);
}

std::shared_ptr<DownloadManager::FileFuture> DownloadManager::downloadFile(
                                                                   const std::string& url,
                                                        const std::filesystem::path& file,
                                                                OverrideFile overrideFile,
                                                                  FailOnError failOnError,
                                                                unsigned int timeout_secs,
                                                DownloadFinishedCallback finishedCallback,
                                          DownloadProgressCallback progressCallback) const
{
    if (!overrideFile && std::filesystem::is_regular_file(file)) {
        return nullptr;
    }

    auto future = std::make_shared<FileFuture>(file.filename());
    errno = 0;
#ifdef WIN32
    FILE* fp;
    const std::string f = file.string();
    errno_t error = fopen_s(&fp, f.c_str(), "wb");
    if (error != 0) {
        LERROR(std::format(
            "Could not open/create file: {}. Errno: {}", file, errno
        ));
    }
#else
    const std::string f = file.string();
    FILE* fp = fopen(f.c_str(), "wb"); // write binary
#endif // WIN32
    if (!fp) {
        LERROR(std::format(
            "Could not open/create file: {}. Errno: {}", file, errno
        ));
    }

    auto downloadFunction = [url, failOnError, timeout_secs,
                             finishedCb = std::move(finishedCallback),
                             progressCb = std::move(progressCallback), future, fp]()
    {
        CURL* curl = curl_easy_init();
        if (curl) {
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            curl_easy_setopt(curl, CURLOPT_USERAGENT, "OpenSpace");
            curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);
            curl_easy_setopt(curl, CURLOPT_WRITEDATA, fp);
            curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, &writeData);
            if (timeout_secs) {
                curl_easy_setopt(curl, CURLOPT_TIMEOUT, timeout_secs);
            }
            if (failOnError) {
                curl_easy_setopt(curl, CURLOPT_FAILONERROR, 1L);
            }

            ProgressInformation p = {
                .future = future,
                .startTime = std::chrono::system_clock::now(),
                .callback = &progressCb
            };
            #if LIBCURL_VERSION_NUM >= 0x072000
            // xferinfo was introduced in 7.32.0, if a lower curl version is used the
            // progress will not be shown for downloads on the splash screen
            curl_easy_setopt(curl, CURLOPT_XFERINFOFUNCTION, xferinfo);
            curl_easy_setopt(curl, CURLOPT_XFERINFODATA, &p);
            #endif
            curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0L);

            const CURLcode res = curl_easy_perform(curl);
            curl_easy_cleanup(curl);
            fclose(fp);

            if (res == CURLE_OK) {
                future->isFinished = true;
            }
            else {
                long rescode = 0;
                curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &rescode);
                future->errorMessage = std::format(
                    "{}. HTTP code: {}", curl_easy_strerror(res), rescode
                );
            }

            if (finishedCb) {
                finishedCb(*future);
            }
        }
    };

    if (_useMultithreadedDownload) {
        std::thread t = std::thread(downloadFunction);
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
                                                          SuccessCallback successCallback,
                                                              ErrorCallback errorCallback)
{
    LDEBUG(std::format("Start downloading file '{}' into memory", url));

    auto downloadFunction = [url, successCb = std::move(successCallback),
                             errorCb = std::move(errorCallback)]()
    {
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
        curl_easy_setopt(curl, CURLOPT_USERAGENT, "OpenSpace");
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, reinterpret_cast<void*>(&file));
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, writeMemoryCallback);
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, 5L);
        curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, false);

        // Will fail when response status is 400 or above
        curl_easy_setopt(curl, CURLOPT_FAILONERROR, 1L);

        CURLcode res = curl_easy_perform(curl);
        if (res == CURLE_OK) {
            // ask for the content-type
            char* ct = nullptr;
            res = curl_easy_getinfo(curl, CURLINFO_CONTENT_TYPE, &ct);
            if (res == CURLE_OK) {
                std::string extension = std::string(ct);
                std::stringstream ss(extension);
                ghoul::getline(ss, extension ,'/');
                ghoul::getline(ss, extension);
                file.format = extension;
            }
            else {
                LWARNING("Could not get extension from file downloaded from: " + url);
            }
            successCb(file);
            curl_easy_cleanup(curl);
            return file;
        }
        else {
            std::string err = curl_easy_strerror(res);
            if (errorCb) {
                errorCb(err);
            }
            else {
                LWARNING(std::format("Error downloading '{}': {}", url, err));
            }
            curl_easy_cleanup(curl);
            // Set a boolean variable in MemoryFile to determine if it is
            // valid/corrupted or not.
            // Return MemoryFile even if it is not valid, and check if it is after
            // future.get() call.
            file.corrupted = true;
            return file;
        }
    };

    return std::async(std::launch::async, downloadFunction);
}

void DownloadManager::fileExtension(const std::string& url,
                                    RequestFinishedCallback finishedCallback) const
{
    auto requestFunction = [url, finishedCb = std::move(finishedCallback)]() {
        CURL* curl = curl_easy_init();
        if (curl) {
            curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
            //USING CURLOPT NOBODY
            curl_easy_setopt(curl, CURLOPT_NOBODY, 1);
            CURLcode res = curl_easy_perform(curl);
            if (CURLE_OK == res) {
                char* ct = nullptr;
                // ask for the content-type
                res = curl_easy_getinfo(curl, CURLINFO_CONTENT_TYPE, &ct);
                if ((res == CURLE_OK) && ct && finishedCb) {
                    finishedCb(std::string(ct));
                }
            }

            curl_easy_cleanup(curl);
        }
    };
    if (_useMultithreadedDownload) {
        std::thread t = std::thread(requestFunction);
        ghoul::thread::setPriority(
            t,
            ghoul::thread::ThreadPriorityClass::Idle,
            ghoul::thread::ThreadPriorityLevel::Lowest
        );
        t.detach();
    }
    else {
        requestFunction();
    }
}

} // namespace openspace
