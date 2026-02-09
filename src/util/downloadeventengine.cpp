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

#include <openspace/util/downloadeventengine.h>

#include <utility>

namespace openspace {

int DownloadEventEngine::subscribe(Callback cb) {
    std::lock_guard lock(_mutex);
    int id = _id++;
    _subscribers[id] = std::move(cb);
    return id;
}

void DownloadEventEngine::unsubscribe(int id) {
    std::lock_guard lock(_mutex);
    _subscribers.erase(id);
}

void DownloadEventEngine::publish(const DownloadEvent& event) {
    std::lock_guard lock(_mutex);
    for (auto& [_, callback] : _subscribers) {
        callback(event);
    }
}

void DownloadEventEngine::publish(const std::string& id, DownloadEvent::Type type,
                                  int64_t downloadedBytes,
                                  std::optional<int64_t> totalBytes)
{
    const DownloadEvent event = {
        .type = type,
        .id = id,
        .downloadedBytes = downloadedBytes,
        .totalBytes = totalBytes
    };

    publish(event);
}

} // namespace openspace
