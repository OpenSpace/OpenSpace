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

#ifndef __OPENSPACE_CORE___HTTPREQUEST___H__
#define __OPENSPACE_CORE___HTTPREQUEST___H__

#include <ghoul/misc/boolean.h>
#include <atomic>
#include <condition_variable>
#include <filesystem>
#include <fstream>
#include <functional>
#include <string>
#include <thread>
#include <vector>

namespace openspace {

// Synchronous http request
class HttpRequest {
public:
    // ProgressCallback: Return non-zero value to cancel download
    using ProgressCallback = std::function<
        int(bool totalBytesKnown, size_t totalBytes, size_t downloadedBytes)
    >;

    // DataCallback: Return number of bytes successfully stored. If this does not match
    // the data buffer sice reported in Data, the request will fail
    using DataCallback = std::function<size_t(char* buffer, size_t size)>;

    // HeaderCallback: Return number of bytes successfully stored. If this does not match
    // the data buffer sice reported in Header, the request will fail
    using HeaderCallback = std::function<size_t(char* buffer, size_t size)>;

    explicit HttpRequest(std::string url);

    void onHeader(HeaderCallback cb);
    void onProgress(ProgressCallback cb);
    void onData(DataCallback cb);

    // 0 for no timeout
    // return success
    bool perform(int requestTimeoutSeconds = 0);

    const std::string& url() const;

private:
    ProgressCallback _onProgress;
    DataCallback _onData;
    HeaderCallback _onHeader;

    std::string _url;
};

class HttpDownload {
public:
    using ProgressCallback = std::function<
        bool(bool totalBytesKnown, size_t totalBytes, size_t downloadedBytes)
    >;

    HttpDownload(std::string url);
    virtual ~HttpDownload();

    void onProgress(ProgressCallback progressCallback);

    void start(int requestTimeoutSeconds = 0);
    void cancel();
    void wait();

    bool hasFailed() const;
    bool hasSucceeded() const;

    const std::string& url() const;

protected:
    virtual size_t handleData(char* buffer, size_t size) = 0;
    virtual bool setup();
    virtual bool teardown();

private:
    ProgressCallback _onProgress;

    bool _started = false;
    bool _finished = false;
    bool _successful = false;
    bool _shouldCancel = false;

    HttpRequest _httpRequest;
    std::thread _downloadThread;
    std::condition_variable _downloadFinishCondition;
};

class HttpFileDownload : public HttpDownload {
public:
    BooleanType(Overwrite);

    HttpFileDownload(std::string url, std::filesystem::path destinationPath,
        HttpFileDownload::Overwrite = Overwrite::No);
    virtual ~HttpFileDownload() = default;

    std::filesystem::path destination() const;

private:
    bool setup() override;
    bool teardown() override;
    size_t handleData(char* buffer, size_t size) override;

    static std::mutex _directoryCreationMutex;
    std::atomic_bool _hasHandle = false;

    std::filesystem::path _destination;
    std::ofstream _file;

    static const int MaxFilehandles = 32;
    static std::atomic_int nCurrentFilehandles;
};

class HttpMemoryDownload : public HttpDownload {
public:
    HttpMemoryDownload(std::string url);

    virtual ~HttpMemoryDownload() = default;
    const std::vector<char>& downloadedData() const;

private:
    size_t handleData(char* buffer, size_t size) override;

    std::vector<char> _downloadedData;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___HTTPREQUEST___H__
