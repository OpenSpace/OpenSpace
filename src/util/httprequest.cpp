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

#include <openspace/util/httprequest.h>

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/logging/logmanager.h>
#include <curl/curl.h>
#include <filesystem>

namespace openspace {

HttpRequest::HttpRequest(std::string url)
    : _url(std::move(url))
{
    ghoul_assert(!_url.empty(), "url must not be empty");
}

void HttpRequest::onProgress(ProgressCallback cb) {
    _onProgress = std::move(cb);
}

void HttpRequest::onData(DataCallback cb) {
    _onData = std::move(cb);
}

void HttpRequest::onHeader(HeaderCallback cb) {
    _onHeader = std::move(cb);
}

bool HttpRequest::perform(std::chrono::milliseconds timeout) {
    CURL* curl = curl_easy_init();
    if (!curl) {
        return false;
    }

    curl_easy_setopt(curl, CURLOPT_URL, _url.data());
    curl_easy_setopt(curl, CURLOPT_USERAGENT, "OpenSpace");
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

    // The leading + in all of the lambda expressions are to cause an implicit conversion
    // to a standard C function pointer. Since the `curl_easy_setopt` function just takes
    // anything as an argument, passing the standard lambda-created anonoymous struct
    // causes crashes while using it if the + is not there

    curl_easy_setopt(curl, CURLOPT_HEADERDATA, this);
    curl_easy_setopt(
        curl,
        CURLOPT_HEADERFUNCTION,
        +[](char* ptr, size_t size, size_t nmemb, void* userData) {
            HttpRequest* r = reinterpret_cast<HttpRequest*>(userData);
            const bool shouldContinue =
                r->_onHeader ? r->_onHeader(ptr, size * nmemb) : true;
            return shouldContinue ? size * nmemb : 0;
        }
    );

    curl_easy_setopt(curl, CURLOPT_WRITEDATA, this);
    curl_easy_setopt(
        curl,
        CURLOPT_WRITEFUNCTION,
        +[](char* ptr, size_t size, size_t nmemb, void* userData) {
            HttpRequest* r = reinterpret_cast<HttpRequest*>(userData);
            const bool shouldContinue = r->_onData ? r->_onData(ptr, size * nmemb) : true;
            return shouldContinue ? size * nmemb : 0;
        }
    );

    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0L);
    curl_easy_setopt(curl, CURLOPT_XFERINFODATA, this);
    curl_easy_setopt(
        curl,
        CURLOPT_XFERINFOFUNCTION,
        +[](void* userData, int64_t nTotalDownloadBytes, int64_t nDownloadedBytes,
           int64_t, int64_t)
        {
            HttpRequest* r = reinterpret_cast<HttpRequest*>(userData);

            std::optional<int64_t> totalBytes;
            if (nTotalDownloadBytes > 0) {
                totalBytes = nTotalDownloadBytes;
            }

            const bool shouldContinue =
                r->_onProgress ?
                r->_onProgress(nDownloadedBytes, totalBytes) :
                true;
            return shouldContinue ? 0 : 1;
        }
    );

    curl_easy_setopt(curl, CURLOPT_TIMEOUT_MS, static_cast<long>(timeout.count()));

    const CURLcode res = curl_easy_perform(curl);
    bool success = false;
    if (res == CURLE_OK) {
        long responseCode = 0;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &responseCode);

        if (responseCode >= 400) {
            LERRORC(
                "HttpRequest",
                std::format("Failed download '{}' with code {}", _url, responseCode)
            );
            success = false;
        }
        else {
            success = true;
        }
    }
    else {
        LERRORC(
            "HttpRequest",
            std::format(
                "Failed download '{}' with error {}", _url, curl_easy_strerror(res)
            )
        );
    }
    curl_easy_cleanup(curl);
    return success;
}

const std::string& HttpRequest::url() const {
    return _url;
}




HttpDownload::HttpDownload(std::string url)
    : _httpRequest(std::move(url))
{
    _httpRequest.onData([this](char* buffer, size_t size) {
        return handleData(buffer, size) && !_shouldCancel;
    });

    _httpRequest.onProgress(
        [this](int64_t nDownloaded, std::optional<int64_t> nTotal) {
            const bool cont =_onProgress ? _onProgress(nDownloaded, nTotal) : true;
            return cont && !_shouldCancel;
        }
    );
}

HttpDownload::~HttpDownload() {
    cancel();
    wait();
}

void HttpDownload::onProgress(HttpRequest::ProgressCallback progressCallback) {
    _onProgress = std::move(progressCallback);
}

bool HttpDownload::hasFailed() const {
    return _isFinished && !_isSuccessful;
}

bool HttpDownload::hasSucceeded() const {
    return _isFinished && _isSuccessful;
}

void HttpDownload::start(std::chrono::milliseconds timeout) {
    if (_isDownloading) {
        return;
    }
    _isDownloading = true;
    _downloadThread = std::thread([this, timeout]() {
        _isFinished = false;
        LTRACEC("HttpDownload", std::format("Start download '{}'", _httpRequest.url()));

        const bool setupSuccess = setup();
        if (setupSuccess) {
            _isSuccessful = _httpRequest.perform(timeout);

            const bool teardownSuccess = teardown();
            _isSuccessful = _isSuccessful && teardownSuccess;
        }
        else {
            _isSuccessful = false;
        }

        _isFinished = true;
        if (_isSuccessful) {
            LTRACEC(
                "HttpDownload",
                std::format("Finished async download '{}'", _httpRequest.url())
            );
        }
        else {
            LTRACEC(
                "HttpDownload",
                std::format("Failed async download '{}'", _httpRequest.url())
            );
        }

        _downloadFinishCondition.notify_all();
        _isDownloading = false;
    });
}

void HttpDownload::cancel() {
    _shouldCancel = true;
}

bool HttpDownload::wait() {
    std::mutex conditionMutex;
    std::unique_lock lock(conditionMutex);
    _downloadFinishCondition.wait(lock, [this]() { return _isFinished; });
    if (_downloadThread.joinable()) {
        _downloadThread.join();
    }
    return _isSuccessful;
}

const std::string& HttpDownload::url() const {
    return _httpRequest.url();
}

bool HttpDownload::setup() {
    return true;
}

bool HttpDownload::teardown() {
    return true;
}



std::atomic_int HttpFileDownload::nCurrentFileHandles = 0;
std::mutex HttpFileDownload::_directoryCreationMutex;

HttpFileDownload::HttpFileDownload(std::string url, std::filesystem::path destination,
                                   Overwrite overwrite)
    : HttpDownload(std::move(url))
    , _destination(std::move(destination))
{
    if (!overwrite && std::filesystem::is_regular_file(_destination)) {
        throw ghoul::RuntimeError(std::format("File '{}' already exists", _destination));
    }
}

bool HttpFileDownload::setup() {
    {
        const std::lock_guard g(_directoryCreationMutex);
        const std::filesystem::path d = _destination.parent_path();
        if (!std::filesystem::is_directory(d)) {
            std::filesystem::create_directories(d);
        }
    }

    while (nCurrentFileHandles >= MaxFileHandles) {
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
    }

    nCurrentFileHandles++;
    _hasHandle = true;
    _file = std::ofstream(_destination, std::ofstream::binary);

    if (_file.good()) {
        return true;
    }


#ifdef WIN32
    // GetLastError() gives more details than errno.
    DWORD errorId = GetLastError();
    if (errorId == 0) {
        LERRORC("HttpFileDownload", std::format("Cannot open file '{}'", _destination));
        return false;
    }
    std::array<char, 256> Buffer;
    size_t size = FormatMessageA(
        FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
            FORMAT_MESSAGE_IGNORE_INSERTS,
        nullptr,
        errorId,
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
        Buffer.data(),
        static_cast<DWORD>(Buffer.size()),
        nullptr
    );

    std::string message(Buffer.data(), size);
    LERRORC(
        "HttpFileDownload",
        std::format("Cannot open file '{}': {}", _destination, message)
    );
    return false;
#else // ^^^ WIN32 / !WIN32 vvv
    if (errno) {
#ifdef __unix__
        std::array<char, 256> buffer;
        LERRORC(
            "HttpFileDownload",
            std::format(
                "Cannot open file '{}': {}",
                _destination,
                std::string(strerror_r(errno, buffer.data(), sizeof(buffer)))
            )
        );
        return false;
#else // ^^^ __unix__ / !__unix__ vvv
        LERRORC(
            "HttpFileDownload",
            std::format(
                "Cannot open file '{}': {}", _destination, std::string(strerror(errno))
            )
        );
        return false;
#endif // __unix__
    }

    LERRORC("HttpFileDownload", std::format("Cannot open file '{}'", _destination));
    return false;
#endif // WIN32
}

std::filesystem::path HttpFileDownload::destination() const {
    return _destination;
}

bool HttpFileDownload::teardown() {
    if (_hasHandle) {
        _hasHandle = false;
        _file.close();
        nCurrentFileHandles--;
        ghoul_assert(nCurrentFileHandles >= 0, "More handles returned than taken out");
        return _file.good();
    }
    else {
        return true;
    }
}

bool HttpFileDownload::handleData(char* buffer, size_t size) {
    _file.write(buffer, size);
    return _file.good();
}



HttpMemoryDownload::HttpMemoryDownload(std::string url)
    : HttpDownload(std::move(url))
{}

const std::vector<char>& HttpMemoryDownload::downloadedData() const {
    return _buffer;
}

bool HttpMemoryDownload::handleData(char* buffer, size_t size) {
    _buffer.insert(_buffer.end(), buffer, buffer + size);
    return true;
}

} // namespace openspace
