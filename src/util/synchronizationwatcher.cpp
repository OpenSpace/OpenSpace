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

#include <openspace/util/synchronizationwatcher.h>

#include <algorithm>

namespace openspace {

SynchronizationWatcher::WatchHandle SynchronizationWatcher::watchSynchronization(
                                 std::shared_ptr<ResourceSynchronization> synchronization,
                                    ResourceSynchronization::StateChangeCallback callback)
{
    std::lock_guard<std::mutex> guard(_mutex);

    WatchHandle watchHandle = generateWatchHandle();

    ResourceSynchronization::CallbackHandle cbh = synchronization->addStateChangeCallback(
        [this, synchronization, watchHandle, cb = std::move(callback)]
        (ResourceSynchronization::State state)
        {
            std::lock_guard<std::mutex> g(_mutex);
            _pendingNotifications.push_back({
                synchronization,
                state,
                watchHandle,
                cb
            });
        }
    );

    _watchedSyncs.insert({ watchHandle, { std::move(synchronization), cbh } });

    return watchHandle;
}

void SynchronizationWatcher::unwatchSynchronization(WatchHandle watchHandle) {
    std::lock_guard<std::mutex> guard(_mutex);

    const auto it = _watchedSyncs.find(watchHandle);
    if (it == _watchedSyncs.end()) {
        return;
    }

    // Remove callback from syncrhonization
    std::shared_ptr<ResourceSynchronization> sync = it->second.synchronization.lock();
    if (sync) {
        sync->removeStateChangeCallback(it->second.callbackHandle);
    }

    // Remove from the list of watches
    _watchedSyncs.erase(it);

    // Remove notifications that are pending
    _pendingNotifications.erase(std::remove_if(
        _pendingNotifications.begin(),
        _pendingNotifications.end(),
        [watchHandle](const NotificationData& data) { return data.handle == watchHandle; }
    ), _pendingNotifications.end());
}


void SynchronizationWatcher::notify() {
    std::vector<NotificationData> notifications;
    {
        std::lock_guard<std::mutex> guard(_mutex);
        notifications = _pendingNotifications;
        _pendingNotifications.clear();
    }

    for (const NotificationData& n : notifications) {
        std::shared_ptr<ResourceSynchronization> sync = n.synchronization.lock();
        if (!sync) {
            continue;
        }
        n.callback(n.state);
    }
}

SynchronizationWatcher::WatchHandle SynchronizationWatcher::generateWatchHandle() {
    return nextWatchHandle++;
}

} // namespace openspace
