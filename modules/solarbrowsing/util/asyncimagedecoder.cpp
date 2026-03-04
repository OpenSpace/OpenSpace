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

#include <modules/solarbrowsing/util/asyncimagedecoder.h>

#include <modules/solarbrowsing/util/j2kcodec.h>

namespace openspace {

AsyncImageDecoder::AsyncImageDecoder(size_t numThreads, bool verbose)
    : _verbose(verbose)
{
    _workers.reserve(numThreads);
    for (size_t i = 0; i < numThreads; i++) {
        _workers.emplace_back(&AsyncImageDecoder::workerThread, this);
    }
}

AsyncImageDecoder::~AsyncImageDecoder() {
    _stopRequest = true;
    _queueCV.notify_all();

    for (std::thread& worker : _workers) {
        if (worker.joinable()) {
            worker.join();
        }
    }
}

void AsyncImageDecoder::requestDecode(DecodeRequest request) {
    {
        std::lock_guard<std::mutex> lock(_queueMutex);
        const std::string key = std::format("{}_ds_{}",
            request.metadata.filePath, request.downsamplingLevel
        );
        if (_activeRequests.contains(key)) {
            // Request is already being processed
            return;
        }

        _activeRequests[key] = true;
        _requestQueue.push(std::move(request));
    }

    _queueCV.notify_one();
}

void AsyncImageDecoder::workerThread() {
    while (!_stopRequest) {
        DecodeRequest request;
        {
            // Aquire lock
            std::unique_lock<std::mutex> lock(_queueMutex);

            // Wait for work or stop signal
            _queueCV.wait(lock, [this]() {
                return _stopRequest || !_requestQueue.empty();
            });

            // Exit working if the async decoder is stopped
            if (_stopRequest) {
                break;
            }

            if (_requestQueue.empty()) {
                continue;
            }

            request = _requestQueue.front();
            _requestQueue.pop();
        }

        // Decode request
        decodeRequest(request);
    }
}

void AsyncImageDecoder::decodeRequest(const DecodeRequest& request) {
    DecodedImageData decodedData;

    const unsigned int imageSize = static_cast<unsigned int>(
        request.metadata.fullResolution /
        std::pow(2, request.downsamplingLevel)
    );
    decodedData.imageSize = imageSize;

    decodedData.buffer.resize(imageSize * imageSize * sizeof(ImagePrecision));
    decodedData.metadata = request.metadata;

    J2kCodec j2c(_verbose);
    j2c.decodeIntoBuffer(
        request.metadata.filePath,
        decodedData.buffer.data(),
        request.downsamplingLevel
    );

    // Invoke callback and pass the image data back to caller thread
    request.callback(std::move(decodedData));

    // Once the callback is finished we're done with the request and can remove it
    {
        std::lock_guard lock(_queueMutex);
        const std::string key = std::format("{}_ds_{}",
            request.metadata.filePath, request.downsamplingLevel
        );
        _activeRequests.erase(key);
    }
}

void AsyncImageDecoder::setVerboseFlag(bool verbose) {
    _verbose = verbose;
}

} // namespace openspace
