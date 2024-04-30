/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/scripting/scriptscheduler.h>
#include <openspace/util/keys.h>
#include <openspace/util/timeline.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>

namespace {
    constexpr std::string_view _loggerCat = "TimeManager";

    // Properties for time interpolation
    // These are used when setting the time from lua time interpolation functions,
    // when called without arguments.

    constexpr openspace::properties::Property::PropertyInfo
    DefaultTimeInterpolationDurationInfo = {
        "DefaultTimeInterpolationDuration",
        "Default Time Interpolation Duration",
        "The default duration taken to interpolate between times."
    };

    constexpr openspace::properties::Property::PropertyInfo
    DefaultDeltaTimeInterpolationDurationInfo = {
        "DefaultDeltaTimeInterpolationDuration",
        "Default Delta Time Interpolation Duration",
        "The default duration taken to interpolate between delta times."
    };

    constexpr openspace::properties::Property::PropertyInfo
        DefaultPauseInterpolationDurationInfo = {
        "DefaultPauseInterpolationDuration",
        "Default Pause Interpolation Duration",
        "The default duration taken to transition to the paused state, when "
        "interpolating."
    };

    constexpr openspace::properties::Property::PropertyInfo
        DefaultUnpauseInterpolationDurationInfo = {
        "DefaultUnpauseInterpolationDuration",
        "Default Unpause Interpolation Duration",
        "The default duration taken to transition to the unpaused state, when "
        "interpolating."
    };

    constexpr std::string_view DeltaTimeStepsGuiPath = "/Time/Simulation Speed/Steps";

    constexpr std::string_view DeltaTimeActionPrefix = "core.time.delta_time";

    bool isPlayingBackSessionRecording() {
        using namespace openspace;

        return (global::openSpaceEngine->currentMode() ==
                OpenSpaceEngine::Mode::SessionRecordingPlayback);
    }

    double currentApplicationTimeForInterpolation() {
        using namespace openspace;

        if (global::sessionRecording->isSavingFramesDuringPlayback()) {
            return global::sessionRecording->currentApplicationInterpolationTime();
        }
        else {
            return global::windowDelegate->applicationTime();
    }
}

} // namespace

namespace openspace {

TimeManager::TimeManager()
    : properties::PropertyOwner({ "TimeManager", "Time Manager" })
    , _defaultTimeInterpolationDuration(
        DefaultTimeInterpolationDurationInfo,
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

    const OpenSpaceEngine::Mode m = global::openSpaceEngine->currentMode();
    if (m == OpenSpaceEngine::Mode::CameraPath) {
        LERROR("Cannot change simulation time during camera path");
        return;
    }

    const double now = currentApplicationTimeForInterpolation();
    const bool pause = isPaused();

    TimeKeyframeData current = {
        .time = time(),
        .delta = deltaTime(),
        .pause = false,
        .jump = false
    };
    TimeKeyframeData next = {
        .time = Time(targetTime),
        .delta = targetDeltaTime(),
        .pause = pause,
        .jump = false
    };

    clearKeyframes();
    addKeyframe(now, std::move(current));
    addKeyframe(now + durationSeconds, std::move(next));
}

void TimeManager::interpolateTimeRelative(double delta, double durationSeconds) {
    ghoul_precondition(durationSeconds > 0.f, "durationSeconds must be positive");

    const float duration = global::timeManager->defaultTimeInterpolationDuration();

    const TimeKeyframeData predictedTime = interpolate(
        currentApplicationTimeForInterpolation() + duration
    );
    const double targetTime = predictedTime.time.j2000Seconds() + delta;
    interpolateTime(targetTime, durationSeconds);
}

void TimeManager::preSynchronization(double dt) {
    ZoneScoped;

    removeKeyframesBefore(_latestConsumedTimestamp);
    progressTime(dt);

    // Notify observers about time changes if any.
    const double newTime = time().j2000Seconds();

    if (newTime != _lastTime) {
        ZoneScopedN("newTime != _lastTime");
        using K = CallbackHandle;
        using V = TimeChangeCallback;
        for (const std::pair<K, V>& it : _timeChangeCallbacks) {
            ZoneScopedN("tcc");
            it.second();
        }
    }
    if (_deltaTime != _lastDeltaTime ||
        _timePaused != _lastTimePaused ||
        _targetDeltaTime != _lastTargetDeltaTime)
    {
        ZoneScopedN("delta time changed");
        using K = CallbackHandle;
        using V = TimeChangeCallback;
        for (const std::pair<K, V>& it : _deltaTimeChangeCallbacks) {
            ZoneScopedN("dtcc");
            it.second();
        }
    }
    if (_deltaTimeStepsChanged) {
        using K = CallbackHandle;
        using V = TimeChangeCallback;
        for (const std::pair<K, V>& it : _deltaTimeStepsChangeCallbacks) {
            it.second();
        }
    }
    if (_timelineChanged) {
        ZoneScopedN("timeline changed");
        using K = CallbackHandle;
        using V = TimeChangeCallback;
        for (const std::pair<K, V>& it : _timelineChangeCallbacks) {
            ZoneScopedN("tlcc");
            it.second();
        }
    }

    _lastTime = newTime;
    _lastDeltaTime = _deltaTime;
    _lastTargetDeltaTime = _targetDeltaTime;
    _lastTimePaused = _timePaused;
    _deltaTimeStepsChanged = false;
    _timelineChanged = false;
    _previousApplicationTime = currentApplicationTimeForInterpolation();
}

TimeManager::TimeKeyframeData TimeManager::interpolate(double applicationTime) {
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
    }
    else if (hasPastKeyframes) {
        // Extrapolate based on last past keyframe
        const double deltaApplicationTime = applicationTime - lastPastKeyframe->timestamp;
        const Time predictedTime = Time(
            lastPastKeyframe->data.time.j2000Seconds() +
            deltaApplicationTime *
                (lastPastKeyframe->data.pause ? 0.0 : lastPastKeyframe->data.delta)
        );
        return TimeKeyframeData {
            predictedTime,
            lastPastKeyframe->data.delta,
            false,
            false
        };
    }
    // As the last option, fall back on the current time.
    return TimeKeyframeData { _currentTime, _targetDeltaTime, _timePaused, false };
}

void TimeManager::progressTime(double dt) {
    ZoneScoped;

    const OpenSpaceEngine::Mode m = global::openSpaceEngine->currentMode();
    if (m == OpenSpaceEngine::Mode::CameraPath) {
        // We don't want to progress time when a camera path is playing, so return
        return;
    }

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

        // Also make sure the script scheduler is up to date
        global::scriptScheduler->setCurrentTime(_timeNextFrame.j2000Seconds());

        _shouldSetTime = false;

        using K = CallbackHandle;
        using V = TimeChangeCallback;
        for (const std::pair<K, V>& it : _timeJumpCallbacks) {
            it.second();
        }
        return;
    }

    const double now = currentApplicationTimeForInterpolation();
    const std::deque<Keyframe<TimeKeyframeData>>& keyframes = _timeline.keyframes();

    const std::function<bool(const KeyframeBase&, double)> comparisonFunc =
        (isPlayingBackSessionRecording()) ?
        &compareKeyframeTimeWithTime_playbackWithFrames : &compareKeyframeTimeWithTime;

    auto firstFutureKeyframe = std::lower_bound(
        keyframes.begin(),
        keyframes.end(),
        now,
        comparisonFunc
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
        const TimeKeyframeData interpolated = interpolate(
            *lastPastKeyframe,
            *firstFutureKeyframe,
            now
        );

        _currentTime.data().setTime(interpolated.time.j2000Seconds());
        _deltaTime = interpolated.delta;
    }
    else if (!hasConsumedLastPastKeyframe) {
        applyKeyframeData(lastPastKeyframe->data, dt);
    }
    else if (!isPaused()) {
        // If there are no keyframes to consider
        // and time is not paused, just advance time.
        _deltaTime = _targetDeltaTime;
        _currentTime.data().advanceTime(dt * _deltaTime);
    }

    if (hasPastKeyframes) {
        _latestConsumedTimestamp = lastPastKeyframe->timestamp;
    }
}

TimeManager::TimeKeyframeData TimeManager::interpolate(
                                                   const Keyframe<TimeKeyframeData>& past,
                                                 const Keyframe<TimeKeyframeData>& future,
                                                                              double time)
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

    const double t = (time - past.timestamp) / deltaAppTime;
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
        Time(interpolatedTime),
        interpolatedDeltaTime,
        past.data.pause,
        past.data.jump
    };

    return data;
}

void TimeManager::applyKeyframeData(const TimeKeyframeData& keyframe, double dt) {
    const Time& currentTime = keyframe.time;
    _deltaTime = _timePaused ? 0.0 : _targetDeltaTime;
    if (isPlayingBackSessionRecording()) {
        _currentTime.data().advanceTime(dt * _deltaTime);
    }
    else {
        _currentTime.data().setTime(currentTime.j2000Seconds());
    }
    _timePaused = keyframe.pause;
    _targetDeltaTime = keyframe.delta;
    _deltaTime = _timePaused ? 0.0 : _targetDeltaTime;
}

void TimeManager::addKeyframe(double timestamp, TimeKeyframeData time) {
    _timeline.addKeyframe(timestamp, std::move(time));
    _timelineChanged = true;
}

void TimeManager::removeKeyframesAfter(double timestamp, bool inclusive) {
    const size_t nKeyframes = _timeline.nKeyframes();
    _timeline.removeKeyframesAfter(timestamp, inclusive);
    if (nKeyframes != _timeline.nKeyframes()) {
        _timelineChanged = true;
    }
}

void TimeManager::removeKeyframesBefore(double timestamp, bool inclusive) {
    const size_t nKeyframes = _timeline.nKeyframes();
    _timeline.removeKeyframesBefore(timestamp, inclusive);
    if (nKeyframes != _timeline.nKeyframes()) {
        _timelineChanged = true;
    }
}

void TimeManager::clearKeyframes() {
    const size_t nKeyframes = _timeline.nKeyframes();
    _timeline.clearKeyframes();
    if (nKeyframes != _timeline.nKeyframes()) {
        _timelineChanged = true;
    }
}

void TimeManager::setTimeNextFrame(Time t) {
    const OpenSpaceEngine::Mode m = global::openSpaceEngine->currentMode();
    if (m == OpenSpaceEngine::Mode::CameraPath) {
        LERROR("Cannot change simulation time during camera path");
        return;
    }
    _shouldSetTime = true;
    _timeNextFrame = std::move(t);
    clearKeyframes();
}

void TimeManager::setDeltaTime(double deltaTime) {
    interpolateDeltaTime(deltaTime, 0.0);
}

void TimeManager::setDeltaTimeSteps(std::vector<double> deltaTimes) {
    // Add negative versions
    std::vector<double> negatives;
    negatives.reserve(deltaTimes.size());
    std::transform(
        deltaTimes.begin(),
        deltaTimes.end(),
        std::back_inserter(negatives),
        std::negate<>()
    );

    deltaTimes.reserve(2 * deltaTimes.size());
    deltaTimes.insert(deltaTimes.end(), negatives.begin(), negatives.end());

    // Sort and remove duplicates
    std::sort(deltaTimes.begin(), deltaTimes.end());
    deltaTimes.erase(std::unique(deltaTimes.begin(), deltaTimes.end()), deltaTimes.end());

    _deltaTimeSteps = std::move(deltaTimes);
    _deltaTimeStepsChanged = true;

    clearDeltaTimesKeybindings();
    addDeltaTimesKeybindings();
}

void TimeManager::addDeltaTimesKeybindings() {
    constexpr std::array<Key, 10> Keys = {
        Key::Num1,
        Key::Num2,
        Key::Num3,
        Key::Num4,
        Key::Num5,
        Key::Num6,
        Key::Num7,
        Key::Num8,
        Key::Num9,
        Key::Num0
    };

    clearDeltaTimesKeybindings();
    _deltaTimeStepKeybindings.reserve(_deltaTimeSteps.size());

    // Find positive delta time steps
    std::vector<double> steps;
    const int nStepsGuess = static_cast<int>(std::floor(_deltaTimeSteps.size() * 0.5f));
    steps.reserve(nStepsGuess);
    std::copy_if(
        _deltaTimeSteps.begin(),
        _deltaTimeSteps.end(),
        std::back_inserter(steps),
        [](double value) { return value >= 0.0; }
    );

    auto addDeltaTimeKeybind = [this](Key key, KeyModifier mod, double step) {
        const std::string s = std::format("{:.0f}", step);

        std::string identifier = std::format("{}.{}", DeltaTimeActionPrefix, s);
        interaction::Action action;
        action.identifier = identifier;
        action.command = std::format("openspace.time.interpolateDeltaTime({})", s);
        action.documentation = std::format(
            "Setting the simulation speed to {} seconds per realtime second", s
        );
        action.name = std::format("Set: {}", s);
        action.guiPath = DeltaTimeStepsGuiPath;
        action.isLocal = interaction::Action::IsLocal::Yes;
        global::actionManager->registerAction(std::move(action));
        global::keybindingManager->bindKey(key, mod, std::move(identifier));
        _deltaTimeStepKeybindings.push_back(KeyWithModifier{ key, mod });
    };

    const int nKeys = static_cast<int>(Keys.size());
    const int nSteps = static_cast<int>(steps.size());

    // For each key, add upp to three keybinds (no modifier, then SHIFT and then CTRL),
    // plus inverted version of each time step one using the ALT modifier
    for (int i = 0; i < nSteps; i++) {
        const Key key = Keys[i % nKeys];
        const double deltaTimeStep = steps[i];

        KeyModifier modifier = KeyModifier::None;
        if (i > nKeys - 1) {
            modifier = KeyModifier::Shift;
        }
        else if (i > 2 * nKeys - 1) {
            modifier = KeyModifier::Control;
        }

        const KeyModifier negativeModifier = modifier | KeyModifier::Alt;

        addDeltaTimeKeybind(key, modifier, deltaTimeStep);
        addDeltaTimeKeybind(key, negativeModifier, -deltaTimeStep);
    }

    LINFO("Added keybindings for specified delta time steps");
    const int maxKeyBinds = 3 * nKeys;
    if (nSteps > maxKeyBinds) {
        LWARNING(std::format(
            "Error settings delta time keys: Too many delta times, so not all could be "
            "mapped to a key. Total: {} steps, which is {} more than the number of "
            "available keybindings",
            nSteps, nSteps - maxKeyBinds
        ));
    }
}

void TimeManager::clearDeltaTimesKeybindings() {
    // Iterate over all of the registered actions with the common prefix that we created
    // in the addDeltaTimesKeybindings function
    const std::vector<interaction::Action> actions = global::actionManager->actions();
    for (const interaction::Action& action : actions) {
        if (action.identifier.starts_with(DeltaTimeActionPrefix)) {
            global::actionManager->removeAction(action.identifier);
        }
    }

    for (const KeyWithModifier& kb : _deltaTimeStepKeybindings) {
        // Check if there are multiple keys bound to the same key
        auto bindings = global::keybindingManager->keyBinding(kb);
        if (bindings.size() > 1) {
            std::string names;
            for (auto& b : bindings) {
                names += std::format("'{}' ", b.second);
            }
            LWARNING(std::format(
                "Updating keybindings for new delta time steps: More than one action "
                "was bound to key '{}'. The following actions are removed: {}",
                ghoul::to_string(kb), names
            ));
        }
        global::keybindingManager->removeKeyBinding(kb);
    }
    _deltaTimeStepKeybindings.clear();
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

const Timeline<TimeManager::TimeKeyframeData>& TimeManager::timeline() const {
    return _timeline;
}

std::vector<Syncable*> TimeManager::syncables() {
    return { &_currentTime, &_integrateFromTime };
}

TimeManager::CallbackHandle TimeManager::addTimeChangeCallback(TimeChangeCallback cb) {
    const CallbackHandle handle = _nextCallbackHandle++;
    _timeChangeCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

TimeManager::CallbackHandle TimeManager::addDeltaTimeChangeCallback(TimeChangeCallback cb)
{
    const CallbackHandle handle = _nextCallbackHandle++;
    _deltaTimeChangeCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

TimeManager::CallbackHandle TimeManager::addDeltaTimeStepsChangeCallback(
                                                                    TimeChangeCallback cb)
{
    const CallbackHandle handle = _nextCallbackHandle++;
    _deltaTimeStepsChangeCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

TimeManager::CallbackHandle TimeManager::addTimeJumpCallback(TimeChangeCallback cb) {
    const CallbackHandle handle = _nextCallbackHandle++;
    _timeJumpCallbacks.emplace_back(handle, std::move(cb));
    return handle;
}

TimeManager::CallbackHandle TimeManager::addTimelineChangeCallback(TimeChangeCallback cb)
{
    const  CallbackHandle handle = _nextCallbackHandle++;
    _timelineChangeCallbacks.emplace_back(handle, std::move(cb));
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

void TimeManager::removeDeltaTimeStepsChangeCallback(CallbackHandle handle) {
    const auto it = std::find_if(
        _deltaTimeStepsChangeCallbacks.begin(),
        _deltaTimeStepsChangeCallbacks.end(),
        [handle](const std::pair<CallbackHandle, std::function<void()>>& cb) {
            return cb.first == handle;
        }
    );

    ghoul_assert(
        it != _deltaTimeStepsChangeCallbacks.end(),
        "handle must be a valid callback handle"
    );

    _deltaTimeStepsChangeCallbacks.erase(it);
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

std::vector<double> TimeManager::deltaTimeSteps() const {
    return _deltaTimeSteps;
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

    const Time newTime = Time(
        time().j2000Seconds() + (_deltaTime + newDeltaTime) * 0.5 * interpolationDuration
    );

    TimeKeyframeData currentKeyframe = {
        .time = time(),
        .delta = _deltaTime,
        .pause = false,
        .jump = false
    };
    TimeKeyframeData futureKeyframe = {
        .time = newTime,
        .delta = newDeltaTime,
        .pause = false,
        .jump = false
    };

    _targetDeltaTime = newDeltaTime;

    const double now = isPlayingBackSessionRecording() ?
        previousApplicationTimeForInterpolation() :
        currentApplicationTimeForInterpolation();

    addKeyframe(now, std::move(currentKeyframe));
    addKeyframe(now + interpolationDuration, std::move(futureKeyframe));
}

std::optional<double> TimeManager::nextDeltaTimeStep() {
    if (!hasNextDeltaTimeStep()) {
        return std::nullopt;
    }

    const std::vector<double>::iterator nextStepIterator = std::upper_bound(
        _deltaTimeSteps.begin(),
        _deltaTimeSteps.end(),
        _targetDeltaTime
    );

    if (nextStepIterator == _deltaTimeSteps.end()) {
        return std::nullopt; // should not get here
    }

    return *nextStepIterator;
}

std::optional<double> TimeManager::previousDeltaTimeStep() {
    if (!hasPreviousDeltaTimeStep()) {
        return std::nullopt;
    }
    const std::vector<double>::iterator lowerBoundIterator = std::lower_bound(
        _deltaTimeSteps.begin(),
        _deltaTimeSteps.end(),
        _targetDeltaTime
    );

    if (lowerBoundIterator == _deltaTimeSteps.begin()) {
        return std::nullopt; // should not get here
    }

    const std::vector<double>::iterator prevStepIterator = lowerBoundIterator - 1;
    return *prevStepIterator;
}

bool TimeManager::hasNextDeltaTimeStep() const {
    if (_deltaTimeSteps.empty()) {
        return false;
    }

    return _targetDeltaTime < _deltaTimeSteps.back();
}

bool TimeManager::hasPreviousDeltaTimeStep() const {
    if (_deltaTimeSteps.empty()) {
        return false;
    }

    return _targetDeltaTime > _deltaTimeSteps.front();
}

void TimeManager::setNextDeltaTimeStep() {
    interpolateNextDeltaTimeStep(0);
}

void TimeManager::setPreviousDeltaTimeStep() {
    interpolatePreviousDeltaTimeStep(0);
}

void TimeManager::interpolateNextDeltaTimeStep(double durationSeconds) {
    if (!hasNextDeltaTimeStep()) {
        return;
    }

    const double nextDeltaTime = nextDeltaTimeStep().value();
    interpolateDeltaTime(nextDeltaTime, durationSeconds);
}

void TimeManager::interpolatePreviousDeltaTimeStep(double durationSeconds) {
    if (!hasPreviousDeltaTimeStep()) {
        return;
    }

    const double previousDeltaTime = previousDeltaTimeStep().value();
    interpolateDeltaTime(previousDeltaTime, durationSeconds);
}

void TimeManager::setPause(bool pause) {
    interpolatePause(pause, 0);
}

void TimeManager::interpolatePause(bool pause, double interpolationDuration) {
    if (pause == _timePaused) {
        return;
    }

    const OpenSpaceEngine::Mode engineMode = global::openSpaceEngine->currentMode();
    if (!pause && engineMode == OpenSpaceEngine::Mode::CameraPath) {
        LERROR("Cannot unpause simulation time during camera path");
        return;
    }

    const double targetDelta = pause ? 0.0 : _targetDeltaTime;
    const Time newTime = Time(
        time().j2000Seconds() + (_deltaTime + targetDelta) * 0.5 * interpolationDuration
    );

    const TimeKeyframeData currentKeyframe = {
        .time = time(),
        .delta = _deltaTime,
        .pause = false,
        .jump = false
    };
    const TimeKeyframeData futureKeyframe = {
        .time = newTime,
        .delta = _targetDeltaTime,
        .pause = pause,
        .jump = false
    };
    _timePaused = pause;

    const double now = isPlayingBackSessionRecording() ?
        previousApplicationTimeForInterpolation() :
        currentApplicationTimeForInterpolation();

    clearKeyframes();
    if (interpolationDuration > 0) {
        addKeyframe(now, currentKeyframe);
    }
    addKeyframe(now + interpolationDuration, futureKeyframe);
}

double TimeManager::previousApplicationTimeForInterpolation() const {
    // If playing back with frames, this function needs to be called when a time rate
    // interpolation (either speed change or pause) begins and ends. If the application
    // time of the interpolation keyframe timestamp (when it was added to timeline) is
    // exactly the same as when it is evaluated, then the interpolation math fails and
    // two identical frames are generated at the begin & end. This only happens when the
    // application time is forced to discrete intervals for a fixed rendering framerate.
    // Using the previous frame render time fixes this problem. This doesn't adversely
    // affect playback without frames.
    return _previousApplicationTime;
}

void TimeManager::setTimeFromProfile(const Profile& p) {
    if (p.time.has_value()) {
        switch (p.time->type) {
            case Profile::Time::Type::Relative:
            {
                const std::string t = Time::currentWallTime();
                std::variant<std::string, double> t2 =
                    Time::advancedTime(t, p.time->value);
                ghoul_assert(std::holds_alternative<std::string>(t2), "Wrong type");
                _currentTime.data() = Time(std::get<std::string>(t2));
                break;
            }
            case Profile::Time::Type::Absolute:
                _currentTime.data() = Time(p.time->value);
                break;
        }

        setPause(p.time->startPaused);
    }
    else {
        // No value was specified so we are using 'now' instead
        _currentTime.data() = Time::now();
    }
}

} // namespace openspace
