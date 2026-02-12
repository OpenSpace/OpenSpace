/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_SOLARBROWSING___ASYNCIMAGEDECODER___H__
#define __OPENSPACE_MODULE_SOLARBROWSING___ASYNCIMAGEDECODER___H__

#include <modules/solarbrowsing/util/structs.h>

#include <atomic>
#include <condition_variable>
#include <filesystem>
#include <functional>
#include <mutex>
#include <unordered_map>
#include <thread>
#include <vector>
#include <queue>

// Implementation based on http://progsch.net/wordpress/?p=81

namespace openspace::solarbrowsing {


using DecodeCompleteCallback = std::function<void(DecodedImageData&&)>;

struct DecodeRequest {
    const ImageMetadata* metadata; // non-owning
    int downsamplingLevel;
    DecodeCompleteCallback callback; // Synchronous callback assumed, can lead to race conditions if async
};

class AsyncImageDecoder {
public:
    explicit AsyncImageDecoder(size_t numThreads);
    ~AsyncImageDecoder();

    void requestDecode(const DecodeRequest& request);

private:
    void workerThread();
    void decodeRequest(const DecodeRequest& request);

    // Thread management
    std::vector<std::thread> _workers;
    std::atomic<bool> _stopRequest = false;

    // Request queue
    std::mutex _queueMutex;
    std::condition_variable _queueCV;
    std::queue<DecodeRequest> _requestQueue;
    std::unordered_map<std::filesystem::path, bool> _activeRequests;
};

} //namespace openspace::solarbrowsing

#endif // !__OPENSPACE_MODULE_SOLARBROWSING___ASYNCIMAGEDECODER___H__
