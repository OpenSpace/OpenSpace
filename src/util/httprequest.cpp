/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <ghoul/fmt.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <filesystem>

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
    constexpr const long StatusCodeOk = 200;
    constexpr const char* _loggerCat = "HttpRequest";
} // namespace

namespace openspace {

HttpRequest::HttpRequest(std::string url)
    : _url(std::move(url))
{}

void HttpRequest::onProgress(ProgressCallback cb) {
    _onProgress = std::move(cb);
}

void HttpRequest::onData(DataCallback cb) {
    _onData = std::move(cb);
}

void HttpRequest::onHeader(HeaderCallback cb) {
    _onHeader = std::move(cb);
}

bool HttpRequest::perform(int requestTimeoutSeconds) {
    CURL* curl = curl_easy_init();
    if (!curl) {
        return false;
    }

    curl_easy_setopt(curl, CURLOPT_URL, _url.c_str());
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
            size_t nBytes = r->_onHeader ?
                r->_onHeader(ptr, size * nmemb) :
                size * nmemb;
            return nBytes;
        }
    );

    curl_easy_setopt(curl, CURLOPT_WRITEDATA, this);
    curl_easy_setopt(
        curl,
        CURLOPT_WRITEFUNCTION,
        +[](char* ptr, size_t size, size_t nmemb, void* userData) {
            HttpRequest* r = reinterpret_cast<HttpRequest*>(userData);
            
            if (r->_onData) {
                return r->_onData(ptr, size * nmemb);
            }
            else {
                return size * nmemb;
            }
        }
    );

    curl_easy_setopt(curl, CURLOPT_NOPROGRESS, 0L);
    #if LIBCURL_VERSION_NUM >= 0x072000
    // xferinfo was introduced in 7.32.0, if a lower curl version is used the progress
    // will not be shown for downloads on the splash screen
    curl_easy_setopt(curl, CURLOPT_XFERINFODATA, this);
    curl_easy_setopt(
        curl,
        CURLOPT_XFERINFOFUNCTION,
        +[](void* userData, int64_t nTotalDownloadBytes, int64_t nDownloadedBytes,
           int64_t, int64_t)
        {
            HttpRequest* r = reinterpret_cast<HttpRequest*>(userData);
            
            bool totalBytesKnown = nTotalDownloadBytes > 0;
            size_t totalBytes = static_cast<size_t>(nTotalDownloadBytes);
            size_t downloadedBytes = static_cast<size_t>(nDownloadedBytes);

            if (r->_onProgress) {
                return r->_onProgress(totalBytesKnown, totalBytes, downloadedBytes);
            }
            else {
                return 0;
            }
        }
    );
    #endif

    if (requestTimeoutSeconds > 0) {
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, requestTimeoutSeconds);
    }

    CURLcode res = curl_easy_perform(curl);
    bool success = false;
    if (res == CURLE_OK) {
        long responseCode;
        curl_easy_getinfo(curl, CURLINFO_RESPONSE_CODE, &responseCode);
        success = (responseCode == StatusCodeOk);
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
        return handleData(buffer, size);
    });

    _httpRequest.onProgress(
        [this](bool totalBytesKnown, size_t totalBytes, size_t downloadedBytes) {
            // Return a non-zero value to cancel download
            bool shouldContinue = _onProgress ?
                _onProgress(totalBytesKnown, totalBytes, downloadedBytes) :
                true;

            if (!shouldContinue || _shouldCancel) {
                return 1;
            }
            else {
                return 0;
            }
        }
    );
}

HttpDownload::~HttpDownload() {
    cancel();
    wait();
}

void HttpDownload::onProgress(ProgressCallback progressCallback) {
    _onProgress = std::move(progressCallback);
}

bool HttpDownload::hasFailed() const {
    return _finished && !_successful;
}

bool HttpDownload::hasSucceeded() const {
    return _finished && _successful;
}

void HttpDownload::start(int requestTimeoutSeconds) {
    if (_started) {
        return;
    }
    _started = true;
    _downloadThread = std::thread([this, requestTimeoutSeconds] {
        try {
            LTRACE(fmt::format("Start async download '{}'", _httpRequest.url()));

            setup();
            _successful = _httpRequest.perform(requestTimeoutSeconds);
            teardown();

            _finished = true;
            if (_successful) {
                LTRACE(fmt::format("Finished async download '{}'", _httpRequest.url()));
            }
            else {
                LTRACE(fmt::format("Failed async download '{}'", _httpRequest.url()));
            }
            
            _downloadFinishCondition.notify_all();
        }
        catch (const ghoul::RuntimeError& e) {
            LERRORC(e.component, e.message);
        }
    });
}

void HttpDownload::cancel() {
    _shouldCancel = true;
}

void HttpDownload::wait() {
    std::mutex conditionMutex;
    std::unique_lock lock(conditionMutex);
    _downloadFinishCondition.wait(lock, [this]() { return _finished; });
    if (_downloadThread.joinable()) {
        _downloadThread.join();
    }
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

HttpMemoryDownload::HttpMemoryDownload(std::string url)
    : HttpDownload(url)
{}

const std::vector<char>& HttpMemoryDownload::downloadedData() const {
    return _downloadedData;
}

size_t HttpMemoryDownload::handleData(char* buffer, size_t size) {
    _downloadedData.insert(_downloadedData.end(), buffer, buffer + size);
    return size;
}

std::atomic_int HttpFileDownload::nCurrentFilehandles = 0;
std::mutex HttpFileDownload::_directoryCreationMutex;

HttpFileDownload::HttpFileDownload(std::string url, std::filesystem::path destination,
                                   Overwrite overwrite)
    : HttpDownload(std::move(url))
    , _destination(std::move(destination))
{
    if (!overwrite && std::filesystem::is_regular_file(_destination)) {
        throw ghoul::RuntimeError(fmt::format("File {} already exists", _destination));
    }
}

bool HttpFileDownload::setup() {
    {
        std::lock_guard g(_directoryCreationMutex);
        std::filesystem::path d = _destination.parent_path();
        if (!std::filesystem::is_directory(d)) {
            std::filesystem::create_directories(d);
        }
    }

    while (nCurrentFilehandles >= MaxFilehandles) {
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }

    ++nCurrentFilehandles;
    _hasHandle = true;
    _file = std::ofstream(_destination, std::ofstream::binary);

    if (_file.fail()) {
#ifdef WIN32
        // GetLastError() gives more details than errno.
        DWORD errorId = GetLastError();
        if (errorId != 0) {
            char Buffer[256];
            size_t size = FormatMessageA(
                FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM |
                    FORMAT_MESSAGE_IGNORE_INSERTS,
                nullptr,
                errorId,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                Buffer,
                256,
                nullptr
            );

            std::string message(Buffer, size);

            LERROR(fmt::format(
                "Cannot open file {}: {}", _destination, message)
            );

            return false;
        }
        else {
            LERROR(fmt::format("Cannot open file {}", _destination));
            return false;
        }
#else
        if (errno) {
#if defined(__unix__)
            char buffer[255];
            LERROR(fmt::format(
                "Cannot open file '{}': {}",
                _destination,
                std::string(strerror_r(errno, buffer, sizeof(buffer)))
            ));
            return false;
#else
            LERROR(fmt::format(
                "Cannot open file '{}': {}", _destination, std::string(strerror(errno))
            ));
            return false;
#endif
        }

        LERROR(fmt::format("Cannot open file {}", _destination));
        return false;
#endif
    }
    return true;
}

std::filesystem::path HttpFileDownload::destination() const {
    return _destination;
}

bool HttpFileDownload::teardown() {
    if (_hasHandle) {
        _hasHandle = false;
        _file.close();
        --nCurrentFilehandles;
    }
    return _file.good();
}

size_t HttpFileDownload::handleData(char* buffer, size_t size) {
    _file.write(buffer, size);
    return size;
}

} // namespace openspace
