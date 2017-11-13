/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
    const char* _loggerCat = "HttpRequest";
}

namespace openspace {

namespace curlfunctions {
int progressCallback(
    void* userData,
    int64_t nTotalBytes,
    int64_t nDownloadedBytes,
    int64_t,
    int64_t)
{
    HttpRequest* r = reinterpret_cast<HttpRequest*>(userData);
    return r->_onProgress(
        HttpRequest::Progress{
            true,
            static_cast<size_t>(nTotalBytes),
            static_cast<size_t>(nDownloadedBytes)
        }
    );

    LINFO("Transfer info from curl: " << nDownloadedBytes << " out of " <<  nTotalBytes);
    return 0;
}

size_t writeCallback(char* ptr, size_t size, size_t nmemb, void* userData) {
    HttpRequest* r = reinterpret_cast<HttpRequest*>(userData);
    return r->_onData(HttpRequest::Data{ptr, size * nmemb});
}
}
    
HttpRequest::HttpRequest(std::string url) 
    : _url(std::move(url))
    , _onReadyStateChange([](ReadyState) {})
    , _onProgress([](Progress) { return 0; })
    , _onData([](Data d) { return d.size; })
{}

void HttpRequest::onReadyStateChange(ReadyStateChangeCallback cb) {
    _onReadyStateChange = std::move(cb);
}

void HttpRequest::onProgress(ProgressCallback cb) {
    _onProgress = std::move(cb);
}

void HttpRequest::onData(DataCallback cb) {
    _onData = std::move(cb);
}

void HttpRequest::perform(RequestOptions opt) {
    setReadyState(ReadyState::Loading);

    CURL* curl = curl_easy_init();
    if (!curl) {
        setReadyState(ReadyState::Fail);
        return;
    }

    curl_easy_setopt(curl, CURLOPT_URL, _url.c_str());
    curl_easy_setopt(curl, CURLOPT_FOLLOWLOCATION, 1L);

    curl_easy_setopt(curl, CURLOPT_WRITEDATA, this);
    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, curlfunctions::writeCallback);
    
    curl_easy_setopt(curl, CURLOPT_XFERINFODATA, this);
    curl_easy_setopt(curl, CURLOPT_XFERINFOFUNCTION, curlfunctions::progressCallback);
    
    if (opt.requestTimeoutSeconds > 0) {
        curl_easy_setopt(curl, CURLOPT_TIMEOUT, opt.requestTimeoutSeconds);
    }

    CURLcode res = curl_easy_perform(curl);
    setReadyState(res == CURLE_OK ? ReadyState::Success : ReadyState::Fail);
    curl_easy_cleanup(curl);
}
    
void HttpRequest::setReadyState(openspace::HttpRequest::ReadyState state) {
    _readyState = state;
}

SyncHttpDownload::SyncHttpDownload(std::string url)
    : _httpRequest(std::move(url))
{}

void SyncHttpDownload::download(HttpRequest::RequestOptions opt) {
    initDownload();
    _httpRequest.onData([this] (HttpRequest::Data d) {
        return handleData(d);
    });
    _httpRequest.perform(opt);
    deinitDownload();
}

AsyncHttpDownload::AsyncHttpDownload(std::string url)
    : _httpRequest(std::move(url))
{}

void AsyncHttpDownload::start(HttpRequest::RequestOptions opt) {
    std::lock_guard<std::mutex> guard(_mutex);
    if (_started) {
        return;
    }
    _downloadThread = std::thread([this, opt] {
        download(opt);
    });
    _started = true;
}

void AsyncHttpDownload::cancel() {
    std::lock_guard<std::mutex> guard(_mutex);
    _shouldCancel = true;
}

void AsyncHttpDownload::wait() {
    std::unique_lock<std::mutex> lock(_mutex);
    _downloadFinishCondition.wait(lock, [this] {
        return _finished;
    });
}

void AsyncHttpDownload::download(HttpRequest::RequestOptions opt) {
    std::unique_lock<std::mutex> lock(_mutex);

    initDownload();

    _httpRequest.onData([this](HttpRequest::Data d) {
        std::lock_guard<std::mutex> guard(_mutex);
        return handleData(d);
    });

    _httpRequest.onProgress([this](HttpRequest::Progress p) {
        std::lock_guard<std::mutex> guard(_mutex);
        if (_shouldCancel) {
            return 1;
        }
        if (p.totalBytesKnown && p.downloadedBytes == p.totalBytes) {
            _successful = true;
        }
        return 0;
    });

    lock.release();
    _httpRequest.perform(opt);
    _finished = true;
    _downloadFinishCondition.notify_all();

    lock.lock();
    deinitDownload();
}

const std::vector<char>& HttpMemoryDownload::downloadedData() {
    return _downloadedData;
}

bool HttpMemoryDownload::initDownload() {
    return true;
}
bool HttpMemoryDownload::deinitDownload() {
    return true;
}
size_t HttpMemoryDownload::handleData(HttpRequest::Data d) {
    _downloadedData.insert(_downloadedData.end(), d.buffer, d.buffer + d.size);
    return d.size;
}

HttpFileDownload::HttpFileDownload(std::string destination) {
    _destination = std::move(destination);
}

bool HttpFileDownload::initDownload() {
    if (FileSys.fileExists(_destination)) {
        LERROR("File " << _destination << " already exists!");
        return false;
    }

    ghoul::filesystem::File destinationFile = _destination;
    ghoul::filesystem::Directory d = destinationFile.directoryName();
    if (!FileSys.directoryExists(d)) {
        FileSys.createDirectory(d, ghoul::filesystem::Directory::Recursive::Yes);
    }

    _file = std::ofstream(_destination, std::ofstream::binary);

    if (_file.fail()) {
        LERROR("Cannot open file " << destinationFile);
        return false;
    }
}

bool HttpFileDownload::deinitDownload() {
    _file.close();
    return _file.good();
}

size_t HttpFileDownload::handleData(HttpRequest::Data d) {
    _file.write(d.buffer, d.size);
    return d.size;
}

SyncHttpMemoryDownload::SyncHttpMemoryDownload(std::string url)
    : SyncHttpDownload(std::move(url))
{}

SyncHttpFileDownload::SyncHttpFileDownload(std::string url, std::string destinationPath)
    : SyncHttpDownload(std::move(url))
    , HttpFileDownload(std::move(destinationPath))
{}

AsyncHttpMemoryDownload::AsyncHttpMemoryDownload(std::string url)
    : AsyncHttpDownload(std::move(url))
{}

AsyncHttpFileDownload::AsyncHttpFileDownload(std::string url, std::string destinationPath)
    : AsyncHttpDownload(std::move(url))
    , HttpFileDownload(std::move(destinationPath))
{}

} // namespace openspace
