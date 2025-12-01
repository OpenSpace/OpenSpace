/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_SYNC___DOWNLOAD_EVENT_ENGINE___H__
#define __OPENSPACE_MODULE_SYNC___DOWNLOAD_EVENT_ENGINE___H__

#include <functional>
#include <map>
#include <mutex>
#include <optional>
#include <string>

namespace openspace {

// @TODO (anden88 2025-10-10): This class was specifically written for the multi-threaded
// url- and httpSynchronization events. In the future we should make this more general
// purposed.
class DownloadEventEngine {
public:
    struct DownloadEvent {
        enum class Type {
            Started,
            Progress,
            Finished,
            Failed
        };

        Type type;
        std::string id;
        int64_t downloadedBytes;
        std::optional<int64_t> totalBytes;
    };

    using Callback = std::function<void(const DownloadEvent&)>;

    int subscribe(Callback cb);
    void unsubscribe(int id);

    void publish(const DownloadEvent& event);
    void publish(const std::string& id, DownloadEvent::Type type,
        int64_t downloadedBytes = 0, std::optional<int64_t> totalBytes = std::nullopt);

private:
    std::mutex _mutex;
    int _id = 0;
    std::unordered_map<int, Callback> _subscribers;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SYNC___DOWNLOAD_EVENT_ENGINE___H__
