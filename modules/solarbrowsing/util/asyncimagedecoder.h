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

/**
 * An asynchronous image decoding utility that processes image decode requests
 * on a dedicated pool of worker threads.
 *
 * The AsyncImageDecoder manages a set of background threads that process
 * DecodeRequest objects submitted through requestDecode(). Each request
 * decodes a specific image at a given downsampling level and delivers the
 * resulting data through a callback function.
 *
 * Each decode operation is uniquely identified by the combination of image file path and
 * downsampling level. If multiple identical requests are issued while one is already
 * being processed, only a single decode operation will be performed.
 *
 * The decoder owns its worker threads for its entire lifetime and joins them during
 * destruction.
 */
class AsyncImageDecoder {
public:
    /**
     * Creates an asynchronous image decoder with \p numThreads worker threads.
     *
     * \param numThreads The number of background threads used for decoding
     */
    explicit AsyncImageDecoder(size_t numThreads, bool verbose = false);

    /**
     * Stops all worker threads and waits for them to finish.
     *
     * Pending requests in the queue may not be processed after destruction
     * begins.
     */
    ~AsyncImageDecoder();

    /**
     * Submits a decode request to be processed asynchronously. If an equivalent request
     * (same image file and downsampling level) is already being processed, the request
     * is ignored.
     *
     * The provided callback will be invoked once decoding has completed.
     *
     * \param request The decode request to enqueue
     */
    void requestDecode(DecodeRequest request);
    void setVerboseFlag(bool verbose);

private:
    /**
     * Entry point for each worker thread.
     *
     * Continuously waits for queued decode requests and processes them until
     * the decoder is stopped.
     */
    void workerThread();

    /**
     * Performs the actual decoding for a single request.
     *
     * Decodes the requested image at the specified downsampling level and invokes the
     * request callback with the resulting image data.
     *
     * \param request The decode request to process
     */
    void decodeRequest(const DecodeRequest& request);

    bool _verbose;
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
