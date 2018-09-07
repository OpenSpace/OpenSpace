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

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/util/timeline.h>
#include <ghoul/logging/logmanager.h>

namespace {
    // Properties for time interpolation
    // These are used when setting the time from lua time interpolation functions,
    // when called without arguments.

    constexpr openspace::properties::Property::PropertyInfo
    DefaultTimeInterpolationDurationInfo = {
        "DefaultTimeInterpolationDuration",
        "Default Time Interpolation Duration",
        "The default duration taken to interpolate between times"
    };

    constexpr openspace::properties::Property::PropertyInfo
    DefaultDeltaTimeInterpolationDurationInfo = {
        "DefaultDeltaTimeInterpolationDuration",
        "Default Delta Time Interpolation Duration",
        "The default duration taken to interpolate between delta times"
    };

    constexpr openspace::properties::Property::PropertyInfo
        DefaultPauseInterpolationDurationInfo = {
        "DefaultPauseInterpolationDuration",
        "Default Pause Interpolation Duration",
        "The default duration taken to transition to the paused state, "
        "when interpolating"
    };

    constexpr openspace::properties::Property::PropertyInfo
        DefaultUnpauseInterpolationDurationInfo = {
        "DefaultUnpauseInterpolationDuration",
        "Default Unpause Interpolation Duration",
        "The default duration taken to transition to the unpaused state, "
        "when interpolating"
    };

    constexpr const char* _loggerCat = "TimeManager";
}

namespace openspace {

using datamessagestructures::TimeKeyframe;

TimeManager::TimeManager()
    : properties::PropertyOwner({ "TimeManager" })
    , _defaultTimeInterpolationDuration(DefaultTimeInterpolationDurationInfo,
        2.f,
        0.f,
        5.f
    )
    , _defaultDeltaTimeInterpolationDuration(
        DefaultDeltaTimeInterpolationDurationInfo,
        1.f,
        0.f,
        5.f
    )
    , _defaultPauseInterpolationDuration(
        DefaultPauseInterpolationDurationInfo,
        0.5f,
        0.f,
        5.f
    )
    , _defaultUnpauseInterpolationDuration(
        DefaultUnpauseInterpolationDurationInfo,
        1.f,
        0.f,
        5.f
    )
{
    addProperty(_defaultTimeInterpolationDuration);
    addProperty(_defaultDeltaTimeInterpolationDuration);
    addProperty(_defaultPauseInterpolationDuration);
    addProperty(_defaultUnpauseInterpolationDuration);
}

void TimeManager::interpolateTime(double targetTime, double durationSeconds) {
    ghoul_precondition(durationSeconds > 0.f, "durationSeconds must be positive");

    const double now = global::windowDelegate.applicationTime();
    const bool pause = isPaused();

    const TimeKeyframeData current = { time(), deltaTime(), false, false };
    const TimeKeyframeData next = { targetTime, targetDeltaTime(), pause, false };

    clearKeyframes();
    addKeyframe(now, current);
    addKeyframe(now + durationSeconds, next);
}

void TimeManager::preSynchronization(double dt) {
    removeKeyframesBefore(_latestConsumedTimestamp);
    progressTime(dt);

    // Notify observers about time changes if any.
    const double newTime = time().j2000Seconds();
    const double newDeltaTime = _deltaTime;
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
    if (_timelineChanged) {
        using K = const CallbackHandle;
        using V = TimeChangeCallback;
        for (const std::pair<K, V>& it : _timelineChangeCallbacks) {
            it.second();
        }
    }

    _lastTime = newTime;
    _lastDeltaTime = newDeltaTime;
    _timelineChanged = false;
}

TimeKeyframeData TimeManager::interpolate(double applicationTime) {
    const std::deque<Keyframe<TimeKeyframeData>>& keyframes = _timeline.keyframes();

    auto firstFutureKeyframe = std::lower_bound(
        keyframes.begin(),
        keyframes.end(),
        applicationTime,
        &compareKeyframeTimeWithTime
    );

    const bool hasFutureKeyframes = firstFutureKeyframe != keyframes.end();
    const bool hasPastKeyframes = firstFutureKeyframe != keyframes.begin();

    const auto lastPastKeyframe = hasPastKeyframes ?
        (firstFutureKeyframe - 1) :
        keyframes.end();

    if (hasPastKeyframes && hasFutureKeyframes && !firstFutureKeyframe->data.jump) {
        return interpolate(
            *lastPastKeyframe,
            *firstFutureKeyframe,
            applicationTime
        );
    } else if (hasPastKeyframes) {
        // Extrapolate based on last past keyframe
        const double deltaApplicationTime = applicationTime - lastPastKeyframe->timestamp;
        Time predictedTime = {
            lastPastKeyframe->data.time.j2000Seconds() +
            deltaApplicationTime *
                (lastPastKeyframe->data.pause ? 0.0 : lastPastKeyframe->data.delta)
        };
        return TimeKeyframeData{
            predictedTime,
            lastPastKeyframe->data.delta,
            false,
            false
        };
    }
    // As the last option, fall back on the current time.
    return TimeKeyframeData{ _currentTime, _targetDeltaTime, _timePaused, false };
}

void TimeManager::progressTime(double dt) {
    _integrateFromTime = static_cast<Time&>(_currentTime);
    // Frames     |    1                    2          |
    //            |------------------------------------|
    // Keyframes  | a     b             c       d   e  |

    // 1: Previous frame
    // 2: Current frame (now)
    // lastPastKeyframe: c
    // firstFutureKeyframe: d
    // _latestConsumedTimestamp: a.timestamp

    if (_shouldSetTime) {
        // Setting the time using `setTimeNextFrame`
        // will override any timeline operations.
        _currentTime.data().setTime(_timeNextFrame.j2000Seconds());
        _integrateFromTime.data().setTime(_timeNextFrame.j2000Seconds());
        _shouldSetTime = false;

        using K = const CallbackHandle;
        using V = TimeChangeCallback;
        for (const std::pair<K, V>& it : _timeJumpCallbacks) {
            it.second();
        }
        return;
    }

    const double now = global::windowDelegate.applicationTime();
    const std::deque<Keyframe<TimeKeyframeData>>& keyframes = _timeline.keyframes();

    auto firstFutureKeyframe = std::lower_bound(
        keyframes.begin(),
        keyframes.end(),
        now,
        &compareKeyframeTimeWithTime
    );

    const bool hasFutureKeyframes = firstFutureKeyframe != keyframes.end();
    const bool hasPastKeyframes = firstFutureKeyframe != keyframes.begin();

    const auto lastPastKeyframe = hasPastKeyframes ?
        (firstFutureKeyframe - 1) :
        keyframes.end();

    const bool hasConsumedLastPastKeyframe = hasPastKeyframes ?
        (lastPastKeyframe->timestamp <= _latestConsumedTimestamp) :
        true;

    if (hasFutureKeyframes && hasPastKeyframes && !firstFutureKeyframe->data.jump) {
        // If keyframes exist before and after this frame,
        // interpolate between those.
        TimeKeyframeData interpolated = interpolate(
            *lastPastKeyframe,
            *firstFutureKeyframe,
            now
        );

        _currentTime.data().setTime(interpolated.time.j2000Seconds());
        _deltaTime = interpolated.delta;
    } else if (!hasConsumedLastPastKeyframe) {
        applyKeyframeData(lastPastKeyframe->data);
    } else if (!isPaused()) {
        // If there are no keyframes to consider
        // and time is not paused, just advance time.
        _deltaTime = _targetDeltaTime;
        _currentTime.data().advanceTime(dt * _deltaTime);
    }

    if (hasPastKeyframes) {
        _latestConsumedTimestamp = lastPastKeyframe->timestamp;
    }
}

TimeKeyframeData TimeManager::interpolate(const Keyframe<TimeKeyframeData>& past,
                                          const Keyframe<TimeKeyframeData>& future,
                                          double appTime)
{
    // https://en.wikipedia.org/wiki/Spline_interpolation
    // interpolatedTime = (1 - t)y1 + t*y2 + t(1 - t)(a(1 - t) + bt), where
    // a = k1 * deltaAppTime - deltaSimTime,
    // b = -k2 * deltaAppTime + deltaSimTime,
    // with y1 = pastTime, y2 = futureTime, k1 = past dt, k2 = future dt.

    const double pastSimTime = past.data.time.j2000Seconds();
    const double futureSimTime = future.data.time.j2000Seconds();
    const double pastDerivative = past.data.pause ? 0.0 : past.data.delta;
    const double futureDerivative = future.data.pause ? 0.0 : future.data.delta;

    const double deltaAppTime = future.timestamp - past.timestamp;
    const double deltaSimTime = futureSimTime - pastSimTime;

    const double t = (appTime - past.timestamp) / deltaAppTime;
    const double a = pastDerivative * deltaAppTime - deltaSimTime;
    const double b = -futureDerivative * deltaAppTime + deltaSimTime;

    const double interpolatedTime =
        (1 - t)*pastSimTime + t * futureSimTime + t * (1 - t)*(a*(1 - t) + b * t);

    // Derivative of interpolated time.
    // Division by deltaAppTime to get appTime derivative
    // as opposed to t (in [0, 1]) derivative.
    const double interpolatedDeltaTime =
        (3 * a*t*t - 4 * a*t + a - 3 * b*t*t + 2 * b*t - pastSimTime + futureSimTime) /
        deltaAppTime;

    TimeKeyframeData data {
        interpolatedTime,
        interpolatedDeltaTime,
        past.data.pause,
        past.data.jump
    };

    return data;
}

void TimeManager::applyKeyframeData(const TimeKeyframeData& keyframeData) {
    const Time& currentTime = keyframeData.time;
    _currentTime.data().setTime(currentTime.j2000Seconds());
    _timePaused = keyframeData.pause;
    _targetDeltaTime = keyframeData.delta;
    _deltaTime = _timePaused ? 0.0 : _targetDeltaTime;
}

void TimeManager::addKeyframe(double timestamp, TimeKeyframeData time) {
    _timeline.addKeyframe(timestamp, std::move(time));
    _timelineChanged = true;
}

void TimeManager::removeKeyframesAfter(double timestamp, bool inclusive) {
    size_t nKeyframes = _timeline.nKeyframes();
    _timeline.removeKeyframesAfter(timestamp, inclusive);
    if (nKeyframes != _timeline.nKeyframes()) {
        _timelineChanged = true;
    }
}

void TimeManager::removeKeyframesBefore(double timestamp, bool inclusive) {
    size_t nKeyframes = _timeline.nKeyframes();
    _timeline.removeKeyframesBefore(timestamp, inclusive);
    if (nKeyframes != _timeline.nKeyframes()) {
        _timelineChanged = true;
    }
}

void TimeManager::clearKeyframes() {
    size_t nKeyframes = _timeline.nKeyframes();
    _timeline.clearKeyframes();
    if (nKeyframes != _timeline.nKeyframes()) {
        _timelineChanged = true;
    }
}

void TimeManager::setTimeNextFrame(Time t) {
    _shouldSetTime = true;
    _timeNextFrame = std::move(t);
    clearKeyframes();
}

void TimeManager::setDeltaTime(double deltaTime) {
    interpolateDeltaTime(deltaTime, 0.0);
}

size_t TimeManager::nKeyframes() const {
    return _timeline.nKeyframes();
}

const Time& TimeManager::time() const {
    return _currentTime;
}

const Time& TimeManager::integrateFromTime() const {
    return _integrateFromTime;
}

const Timeline<TimeKeyframeData>& TimeManager::timeline() const {
    return _timeline;
}

std::vector<Syncable*> TimeManager::getSyncables() {
    return { &_currentTime, &_integrateFromTime };
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

TimeManager::CallbackHandle TimeManager::addTimeJumpCallback(TimeChangeCallback cb) {
    CallbackHandle handle = _nextCallbackHandle++;
    _timeJumpCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

TimeManager::CallbackHandle TimeManager::addTimelineChangeCallback(TimeChangeCallback cb)
{
    CallbackHandle handle = _nextCallbackHandle++;
    _timelineChangeCallbacks.push_back({ handle, std::move(cb) });
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

void TimeManager::removeTimeJumpCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _timeJumpCallbacks.begin(),
        _timeJumpCallbacks.end(),
        [handle](const std::pair<CallbackHandle, std::function<void()>>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _timeJumpCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _timeJumpCallbacks.erase(it);
}

void TimeManager::removeTimelineChangeCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _timelineChangeCallbacks.begin(),
        _timelineChangeCallbacks.end(),
        [handle](const std::pair<CallbackHandle, std::function<void()>>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _timelineChangeCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _timelineChangeCallbacks.erase(it);
}

bool TimeManager::isPaused() const {
    return _timePaused;
}

float TimeManager::defaultTimeInterpolationDuration() const {
    return _defaultTimeInterpolationDuration;
}

float TimeManager::defaultDeltaTimeInterpolationDuration() const {
    return _defaultDeltaTimeInterpolationDuration;
}

float TimeManager::defaultPauseInterpolationDuration() const {
    return _defaultPauseInterpolationDuration;
}

float TimeManager::defaultUnpauseInterpolationDuration() const {
    return _defaultUnpauseInterpolationDuration;
}

double TimeManager::deltaTime() const {
    return _deltaTime;
}

double TimeManager::targetDeltaTime() const {
    return _targetDeltaTime;
}

void TimeManager::interpolateDeltaTime(double newDeltaTime, double interpolationDuration)
{
    if (newDeltaTime == _targetDeltaTime) {
        return;
    }

    clearKeyframes();
    if (_timePaused || interpolationDuration <= 0.0) {
        _targetDeltaTime = newDeltaTime;
        _deltaTime = _timePaused ? 0.0 : _targetDeltaTime;
        return;
    }

    const double now = global::windowDelegate.applicationTime();

    Time newTime = time().j2000Seconds() +
        (_deltaTime + newDeltaTime) * 0.5 * interpolationDuration;

    TimeKeyframeData currentKeyframe = { time(), _deltaTime, false, false };
    TimeKeyframeData futureKeyframe = { newTime, newDeltaTime, false, false };

    _targetDeltaTime = newDeltaTime;

    addKeyframe(now, currentKeyframe);
    addKeyframe(now + interpolationDuration, futureKeyframe);
}

void TimeManager::setPause(bool pause) {
    interpolatePause(pause, 0);
}

void TimeManager::interpolatePause(bool pause, double interpolationDuration) {
    if (pause == _timePaused) {
        return;
    }

    const double now = global::windowDelegate.applicationTime();
    double targetDelta = pause ? 0.0 : _targetDeltaTime;
    Time newTime = time().j2000Seconds() +
        (_deltaTime + targetDelta) * 0.5 * interpolationDuration;

    TimeKeyframeData currentKeyframe = { time(), _deltaTime, false, false };
    TimeKeyframeData futureKeyframe = { newTime, _targetDeltaTime, pause, false };
    _timePaused = pause;

    clearKeyframes();
    if (interpolationDuration > 0) {
        addKeyframe(now, currentKeyframe);
    }
    addKeyframe(now + interpolationDuration, futureKeyframe);
}

} // namespace openspace
