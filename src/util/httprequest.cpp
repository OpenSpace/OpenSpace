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
    if (res == CURLE_OK) {
        LINFO("CURL is ok");
    } else {
        LINFO("CURL failed");
    }

    curl_easy_cleanup(curl);
}
    
void HttpRequest::setReadyState(openspace::HttpRequest::ReadyState state) {
    _readyState = state;
}
    
    
namespace curlfunctions {
int progressCallback(void* userData,
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


using namespace openspace;

HttpMemoryDownload::HttpMemoryDownload(std::string url)
    : _httpRequest(url)
{}

void HttpMemoryDownload::download(HttpRequest::RequestOptions opt) {
    _httpRequest.onData([this](HttpRequest::Data d) {
        _downloadedData.insert(_downloadedData.end(), d.buffer, d.buffer + d.size);
        return d.size;
    });
    _httpRequest.perform(opt);
}
    
const std::vector<char>& HttpMemoryDownload::downloadedData() {
    return _downloadedData;
}

HttpFileDownload::HttpFileDownload(std::string url, std::string destinationPath)
    : _httpRequest(url)
    , _destination(destinationPath)
{}
    
void HttpFileDownload::download(HttpRequest::RequestOptions opt) {
    LINFO(_destination);
    
    if (FileSys.fileExists(_destination)) {
        LERROR("File already exists!");
        return;
    }
    
    {
        ghoul::filesystem::File f = _destination;
        ghoul::filesystem::Directory d = f.directoryName();
        if (!FileSys.directoryExists(d)) {
            FileSys.createDirectory(d, ghoul::filesystem::Directory::Recursive::Yes);
        }
    }
    
    std::ofstream f(_destination, std::ofstream::binary);
    
    if (f.fail()) {
        LERROR("Cannot open file!");
        return;
    }
    _httpRequest.onData([this, &f](HttpRequest::Data d) {
        f.write(d.buffer, d.size);
        return d.size;
    });
    _httpRequest.perform(opt);
    f.close();
}

} // namespace openspace
