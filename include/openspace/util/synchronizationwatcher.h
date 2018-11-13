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

#ifndef __OPENSPACE_CORE___SYNCHRONIZATIONWATCHER___H__
#define __OPENSPACE_CORE___SYNCHRONIZATIONWATCHER___H__

#include <openspace/util/resourcesynchronization.h>
#include <memory>
#include <mutex>
#include <vector>
#include <unordered_map>

namespace openspace {

/**
 * Delays callbacks of synchronization state changes to
 * when notify is called.
 */
class SynchronizationWatcher {
public:
    using WatchHandle = size_t;

    struct WatchData {
        std::weak_ptr<ResourceSynchronization> synchronization;
        ResourceSynchronization::CallbackHandle callbackHandle;
    };

    struct NotificationData {
        std::weak_ptr<ResourceSynchronization> synchronization;
        ResourceSynchronization::State state;
        WatchHandle handle;
        ResourceSynchronization::StateChangeCallback callback;
    };

    /*using SyncStateChangeCallback =
        std::function<void(
            std::shared_ptr<ResourceSynchronization>,
            ResourceSynchronization::State
        )>;*/

    WatchHandle watchSynchronization(
        std::shared_ptr<ResourceSynchronization> synchronization,
        ResourceSynchronization::StateChangeCallback callback
    );

    void unwatchSynchronization(WatchHandle watchHandle);

    void notify();

private:
    WatchHandle generateWatchHandle();
    std::mutex _mutex;
    std::unordered_map<WatchHandle, WatchData> _watchedSyncs;
    std::vector<NotificationData> _pendingNotifications;

    WatchHandle nextWatchHandle = 0;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___SYNCHRONIZATIONWATCHER___H__
