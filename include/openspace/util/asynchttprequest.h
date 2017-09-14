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

#ifndef __OPENSPACE_CORE___ASYNCHTTPREQUEST___H__
#define __OPENSPACE_CORE___ASYNCHTTPREQUEST___H__

#include <future>
#include <string>

namespace openspace {

class AsncHttpRequestData {
public:
    std::chrono::system_clock::time_point startTime();
    std::chrono::system_clock::time_point finishTime();
private:
    std::chrono::system_clock::time_point _startTime;
    std::chrono::system_clock::time_point _finishTime;
};

class AsyncHttpRequest {
public:
    enum class ReadyState : unsigned int {
        Unsent,
        Loading,
        Done
    };

    using StateChangeCallback = std::function<void(ReadyState)>;
    using ProgressCallback = std::function<void(float)>;
    using DataCallback = std::function<void(char*, size_t)>;

    AsyncHttpRequest(
        std::string url,
        DataCallback dataCallback);
    ~AsyncHttpRequest();

    virtual std::future<AsncHttpRequestData> start(
        StateChangeCallback stateChangeCallback,
        ProgressCallback progressCallback);
    void wait();

    std::chrono::system_clock::duration timeElapsed();
    std::chrono::system_clock::duration estimatedTimeRemaining();
    float progress();
    ReadyState readyState();
private:
    std::chrono::system_clock::time_point startTime;
    float _progress;
};

//////

class AsncHttpDownloadData : public AsncHttpRequestData {
public:
    std::string url();
private:
    std::string _url; 
};

class AsyncHttpDownload : public AsyncHttpRequest {
public:
    AsyncHttpDownload(
        std::string url, 
        std::string path);
    virtual ~AsyncHttpDownload();

    /*std::future<AsncHttpDownloadData> start(
        StateChangeCallback stateChangeCallback,
        ProgressCallback progressCallback) override;*/
};

////

class AsyncHttpReadData : public AsncHttpRequestData {
public:
    std::vector<char>& buffer();
private:
    std::vector<char> _buffer;
};

class AsyncHttpRead : public AsyncHttpRequest {
public:
    AsyncHttpRead(std::string url);
    ~AsyncHttpRead();
    /*std::future<AsncHttpReadData> start(
        StateChangeCallback stateChangeCallback,
        ProgressCallback progressCallback) override;*/
};

} // namespace openspace

#endif // __OPENSPACE_CORE___ASYNCHTTPREQUEST___H__

/*
// start
std::ostream file();
AsyncHttpDownload d(url, file);
std::future<AsyncHttpDownloadData> data = d->start(progressCallback, progressCallback);

// in event loop
AsyncHttpRequest::ReadyState status = d.readyState();
if (status == AsyncHttpRequest::ReadyState::Loading) {
    float progress = d.progress();
    // update some progress bar..
}




// usage:

HTTPDownload ();

*/
