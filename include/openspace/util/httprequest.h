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

#ifndef __OPENSPACE_CORE___HTTPREQUEST___H__
#define __OPENSPACE_CORE___HTTPREQUEST___H__

#include <ghoul/misc/boolean.h>
#include <atomic>
#include <condition_variable>
#include <fstream>
#include <functional>
#include <string>
#include <thread>
#include <vector>

// @TODO:  This class is a diamond-of-death;  maybe some redesign to make the things into
//         components, rather than inheritance?

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4250)
#endif

namespace openspace {

namespace curlfunctions {
    size_t writeCallback(char* ptr, size_t size, size_t nmemb, void* userData);

    int progressCallback(void* userData, int64_t nTotalDownloadBytes,
        int64_t nDownloadedBytes, int64_t nTotalUploadBytes, int64_t nUploadBytes);

    size_t headerCallback(char* ptr, size_t size, size_t nmemb, void* userData);
}

// Synchronous http request
class HttpRequest {
public:
    enum class ReadyState : unsigned int {
        Unsent,
        Loading,
        ReceivedHeader,
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

    struct Header {
        char* buffer;
        size_t size;
    };

    using ReadyStateChangeCallback = std::function<void(ReadyState)>;

    // ProgressCallback: Return non-zero value to cancel download.
    using ProgressCallback = std::function<int(Progress)>;

    // DataCallback: Return number of bytes successfully stored.
    // If this does not match the data buffer sice reported in Data,
    // the request will fail.
    using DataCallback = std::function<size_t(Data)>;

    // HeaderCallback: Return number of bytes successfully stored.
    // If this does not match the data buffer sice reported in Header,
    // the request will fail.
    using HeaderCallback = std::function<size_t(Header)>;

    struct RequestOptions {
        int requestTimeoutSeconds; // 0 for no timeout
    };

    HttpRequest(std::string url);

    void onReadyStateChange(ReadyStateChangeCallback cb);
    void onHeader(HeaderCallback cb);
    void onProgress(ProgressCallback cb);
    void onData(DataCallback cb);

    void perform(RequestOptions opt);

    const std::string& url() const;

private:
    void setReadyState(ReadyState state);

    std::string _url;

    ReadyStateChangeCallback _onReadyStateChange;
    ProgressCallback _onProgress;
    DataCallback _onData;
    HeaderCallback _onHeader;

    ReadyState _readyState;
    Progress _progress;
    RequestOptions _options;

    friend size_t curlfunctions::writeCallback(char* ptr, size_t size, size_t nmemb,
        void* userData);

    friend int curlfunctions::progressCallback(void* userData,
        int64_t nTotalDownloadBytes,
        int64_t nDownloadedBytes, int64_t nTotalUploadBytes, int64_t nUploadBytes);

    friend size_t curlfunctions::headerCallback(char* ptr, size_t size, size_t nmemb,
       void* userData);
};

class HttpDownload {
public:
    using ProgressCallback = std::function<bool(HttpRequest::Progress)>;
    using HeaderCallback = std::function<bool(HttpRequest::Header)>;

    HttpDownload();
    HttpDownload(HttpDownload&& d) = default;
    virtual ~HttpDownload() = default;
    void onProgress(ProgressCallback progressCallback);
    void onHeader(HeaderCallback headerCallback);
    bool hasStarted() const;
    bool hasFailed() const;
    bool hasSucceeded() const;

protected:
    virtual size_t handleData(HttpRequest::Data d) = 0;
    virtual bool initDownload() = 0;
    virtual bool deinitDownload() = 0;

    bool callOnProgress(HttpRequest::Progress p);
    void markAsStarted();
    void markAsSuccessful();
    void markAsFailed();

private:
    ProgressCallback _onProgress;
    bool _started = false;
    bool _failed = false;
    bool _successful = false;
};

class SyncHttpDownload : public virtual HttpDownload {
public:
    SyncHttpDownload(std::string url);
    SyncHttpDownload(SyncHttpDownload&& d) = default;
    virtual ~SyncHttpDownload() = default;
    void download(HttpRequest::RequestOptions opt);

    const std::string& url() const;

protected:
    HttpRequest _httpRequest;
};

class AsyncHttpDownload : public virtual HttpDownload {
public:
    AsyncHttpDownload(std::string url);
    AsyncHttpDownload(AsyncHttpDownload&& d);
    virtual ~AsyncHttpDownload() = default;
    void start(HttpRequest::RequestOptions opt);
    void cancel();
    void wait();

    const std::string& url() const;

protected:
    void download(HttpRequest::RequestOptions opt);

private:
    HttpRequest _httpRequest;
    std::thread _downloadThread;
    std::mutex _conditionMutex;
    std::condition_variable _downloadFinishCondition;
    bool _shouldCancel = false;
    std::mutex _stateChangeMutex;
};

class HttpFileDownload : public virtual HttpDownload {
public:
    BooleanType(Overwrite);

    HttpFileDownload() = default;
    HttpFileDownload(std::string destination, Overwrite = Overwrite::No);
    HttpFileDownload(HttpFileDownload&& d) = default;
    virtual ~HttpFileDownload() = default;

    const std::string& destination() const;

protected:
    bool initDownload() override;
    bool deinitDownload() override;
    size_t handleData(HttpRequest::Data d) override;

    static std::mutex _directoryCreationMutex;

private:
    std::string _destination;
    bool _overwrite;
    std::ofstream _file;

    static const int MaxFilehandles = 35;
    static std::atomic_int nCurrentFilehandles;
};

class HttpMemoryDownload : public virtual HttpDownload {
public:
    HttpMemoryDownload() = default;
    HttpMemoryDownload(HttpMemoryDownload&& d) = default;
    virtual ~HttpMemoryDownload() = default;
    const std::vector<char>& downloadedData() const;

protected:
    bool initDownload() override;
    bool deinitDownload() override;
    size_t handleData(HttpRequest::Data d) override;

private:
    std::vector<char> _downloadedData;
};

// Synchronous download to memory
class SyncHttpMemoryDownload : public SyncHttpDownload, public HttpMemoryDownload {
public:
    SyncHttpMemoryDownload(std::string url);
    virtual ~SyncHttpMemoryDownload() = default;
};

// Synchronous download to file
class SyncHttpFileDownload : public SyncHttpDownload, public HttpFileDownload {
public:
    SyncHttpFileDownload(
        std::string url,
        std::string destinationPath,
        HttpFileDownload::Overwrite = Overwrite::No
    );
    virtual ~SyncHttpFileDownload() = default;
};

// Asynchronous download to memory
class AsyncHttpMemoryDownload : public AsyncHttpDownload, public HttpMemoryDownload {
public:
    AsyncHttpMemoryDownload(std::string url);
    virtual ~AsyncHttpMemoryDownload() = default;
};

// Asynchronous download to file
class AsyncHttpFileDownload : public AsyncHttpDownload, public HttpFileDownload {
public:
    AsyncHttpFileDownload(
        std::string url,
        std::string destinationPath,
        HttpFileDownload::Overwrite = Overwrite::No
    );
    virtual ~AsyncHttpFileDownload() = default;
};

} // namespace openspace

#ifdef _MSC_VER
#pragma warning(pop)
#endif


#endif // __OPENSPACE_CORE___HTTPREQUEST___H__
