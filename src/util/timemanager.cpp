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

#include <openspace/util/timemanager.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/util/timeline.h>

namespace openspace {

void TimeManager::preSynchronization(double dt) {
    removeKeyframesBefore(_latestConsumedTimestamp);
    if (_shouldSetTime) {
        time().setTime(_timeNextFrame.j2000Seconds());
        _shouldSetTime = false;
    } else if (_timeline.nKeyframes() == 0) {
        time().advanceTime(dt);
    } else {
        consumeKeyframes(dt);
    }

    // Notify observers about time changes if any.
    const double newTime = time().j2000Seconds();
    const double newDeltaTime = time().deltaTime();
    if (newTime != _lastTime) {
        using K = const CallbackHandle;
        using V = TimeChangeCallback;
        for (const std::pair<K, V>& it : _timeChangeCallbacks) {
            it.second();
        }
    }
    if (newDeltaTime != _lastDeltaTime) {
        using K = const CallbackHandle;
        using V = TimeChangeCallback;
        for (const std::pair<K, V>& it : _deltaTimeChangeCallbacks) {
            it.second();
        }
    }
    _lastTime = newTime;
    _lastDeltaTime = newDeltaTime;
}

void TimeManager::consumeKeyframes(double dt) {
    const double now = OsEng.windowWrapper().applicationTime();

    const std::deque<Keyframe<Time>>& keyframes = _timeline.keyframes();
    const auto firstFutureKeyframe = std::lower_bound(
        keyframes.begin(),
        keyframes.end(),
        now,
        &compareKeyframeTimeWithTime
    );

    const bool consumingTimeJump = std::find_if(
        keyframes.begin(),
        firstFutureKeyframe,
        [](const Keyframe<Time>& f) { return f.data.timeJumped(); }
    ) != firstFutureKeyframe;

    if (firstFutureKeyframe == keyframes.end()) {
        // All keyframes are in the past.
        // Consume the latest one.
        const Keyframe<Time>& current = keyframes.back();
        const Time& currentTime = current.data;
        time().setTime(currentTime.j2000Seconds(), consumingTimeJump);
        time().setDeltaTime(currentTime.deltaTime());
        time().setPause(currentTime.paused());
        _latestConsumedTimestamp = current.timestamp;
    }
    else {
        const Keyframe<Time>& next = *firstFutureKeyframe;
        const Time& nextTime = next.data;

        if (firstFutureKeyframe != keyframes.begin()) {
            const Keyframe<Time>& latest = *(firstFutureKeyframe - 1);
            const Time& latestTime = latest.data;
            // In case of unconsumed passed keyframes, let the last one
            // determine whether the time should be paused or not.
            // If there was a time jump or time is paused, apply it directly.
            // Then consume the last keyframe.

            time().setPause(latestTime.paused());
            time().setTimeJumped(consumingTimeJump);
            time().setDeltaTime(latestTime.deltaTime());

            if (consumingTimeJump || latestTime.paused()) {
                time().setTime(latestTime.j2000Seconds(), consumingTimeJump);
            }
            _latestConsumedTimestamp = latest.timestamp;
        }

        // Do not interpolate with time jumping keyframes.
        // Instead, wait until their timestamp and apply them directly.
        if (nextTime.timeJumped()) {
            time().advanceTime(dt);
            return;
        }

        if (time().paused()) {
            return;
        }

        const double secondsOffTolerance = OsEng.parallelPeer().timeTolerance();

        const double predictedTime = time().j2000Seconds() + time().deltaTime() *
                                     (next.timestamp - now);
        const bool withinTolerance = std::abs(predictedTime - nextTime.j2000Seconds()) <
                                     std::abs(nextTime.deltaTime() * secondsOffTolerance);

        if (nextTime.deltaTime() == time().deltaTime() && withinTolerance) {
            time().advanceTime(dt);
            return;
        }

        const double t0 = now - dt;
        const double t1 = now;
        const double t2 = next.timestamp;

        const double parameter = (t1 - t0) / (t2 - t0);

        const double y0 = time().j2000Seconds();
        // double yPrime0 = time().deltaTime();

        const double y2 = nextTime.j2000Seconds();
        // double yPrime2 = nextTime.deltaTime();

        const double y1 = (1 - parameter) * y0 + parameter * y2;
        const double y1Prime = (y1 - y0) / dt;

        time().setDeltaTime(y1Prime);
        time().setTime(y1, false);
    }
}

void TimeManager::addKeyframe(double timestamp, Time keyframeTime) {
    _timeline.addKeyframe(timestamp, std::move(keyframeTime));
}

void TimeManager::removeKeyframesAfter(double timestamp) {
    _timeline.removeKeyframesAfter(timestamp);
}

void TimeManager::removeKeyframesBefore(double timestamp) {
    _timeline.removeKeyframesBefore(timestamp);
}

void TimeManager::clearKeyframes() {
    _timeline.clearKeyframes();
}

void TimeManager::setTimeNextFrame(Time t) {
    _shouldSetTime = true;
    _timeNextFrame = std::move(t);
}

size_t TimeManager::nKeyframes() const {
    return _timeline.nKeyframes();
}

Time& TimeManager::time() {
    return _currentTime;
}

std::vector<Syncable*> TimeManager::getSyncables() {
    return { &_currentTime };
}

TimeManager::CallbackHandle TimeManager::addTimeChangeCallback(TimeChangeCallback cb) {
    CallbackHandle handle = _nextCallbackHandle++;
    _timeChangeCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

TimeManager::CallbackHandle TimeManager::addDeltaTimeChangeCallback(TimeChangeCallback cb)
{
    CallbackHandle handle = _nextCallbackHandle++;
    _deltaTimeChangeCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

void TimeManager::removeTimeChangeCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _timeChangeCallbacks.begin(),
        _timeChangeCallbacks.end(),
        [handle](const std::pair<CallbackHandle, TimeChangeCallback>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _timeChangeCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _timeChangeCallbacks.erase(it);
}

void TimeManager::removeDeltaTimeChangeCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _deltaTimeChangeCallbacks.begin(),
        _deltaTimeChangeCallbacks.end(),
        [handle](const std::pair<CallbackHandle, std::function<void()>>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _deltaTimeChangeCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _deltaTimeChangeCallbacks.erase(it);
}

} // namespace openspace
