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

#ifndef __OPENSPACE_CORE___HTTPREQUEST___H__
#define __OPENSPACE_CORE___HTTPREQUEST___H__

#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/directory.h>

#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace openspace {
    
namespace curlfunctions {
    size_t writeCallback(char *ptr,
                         size_t size,
                         size_t nmemb,
                         void *userdata);

    int progressCallback(void *clientp,
                         int64_t dltotal,
                         int64_t dlnow,
                         int64_t ultotal,
                         int64_t ulnow);
}

// Synchronous http request
class HttpRequest {
public:
    enum class ReadyState : unsigned int {
        Unsent,
        Loading,
        ReceivedHeaders,
        Fail,
        Success
    };

    struct Progress {
        bool totalBytesKnown = false;
        size_t totalBytes = 0;
        size_t downloadedBytes = 0;
    };

    struct Data {
        char* buffer;
        size_t size;
    };

    using ReadyStateChangeCallback = std::function<void(ReadyState)>;
    using ProgressCallback = std::function<int(Progress)>;
    using DataCallback = std::function<size_t(Data)>;

    struct RequestOptions {
        int requestTimeoutSeconds; // 0 for no timeout
    };

    HttpRequest(std::string url);

    void onReadyStateChange(ReadyStateChangeCallback cb);
    void onProgress(ProgressCallback cb);
    void onData(DataCallback cb);

    void perform(RequestOptions opt);

private:
    void setReadyState(ReadyState state);

    std::string _url;

    ReadyStateChangeCallback _onReadyStateChange;
    ProgressCallback _onProgress;
    DataCallback _onData;

    ReadyState _readyState;
    Progress _progress;
    RequestOptions _options;
    
    friend size_t curlfunctions::writeCallback(
                                char *ptr,
                                size_t size,
                                size_t nmemb,
                                void *userdata);

    friend int curlfunctions::progressCallback(void *clientp,
                                               int64_t dltotal,
                                               int64_t dlnow,
                                               int64_t ultotal,
                                               int64_t ulnow);
};
    
class HttpMemoryDownload {
public:
    HttpMemoryDownload(std::string url);
    
    void onReadyStateChange(HttpRequest::ReadyStateChangeCallback cb);
    void onProgress(HttpRequest::ProgressCallback cb);
    
    void download(HttpRequest::RequestOptions opt);
    
    const std::vector<char>& downloadedData();

private:
    HttpRequest _httpRequest;
    std::vector<char> _downloadedData;
};

class HttpFileDownload {
public:
    HttpFileDownload(std::string url, std::string destinationPath);
    
    void onReadyStateChange(HttpRequest::ReadyStateChangeCallback cb);
    void onProgress(HttpRequest::ProgressCallback cb);
    
    void download(HttpRequest::RequestOptions opt);

private:
    HttpRequest _httpRequest;
    std::string _destination;
};
    

} // namespace openspace

#endif // __OPENSPACE_CORE___HTTPREQUEST___H__
