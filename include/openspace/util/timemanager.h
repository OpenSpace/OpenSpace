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

#ifndef __OPENSPACE_CORE___TIMEMANAGER___H__
#define __OPENSPACE_CORE___TIMEMANAGER___H__

#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/floatproperty.h>
#include "openspace/scene/profile.h"
#include <openspace/util/keys.h>
#include <openspace/util/syncdata.h>
#include <openspace/util/time.h>
#include <openspace/util/timeline.h>
#include <functional>
#include <optional>
#include <utility>
#include <vector>

namespace openspace {

class TimeManager : public properties::PropertyOwner {
public:
    using CallbackHandle = int;
    using TimeChangeCallback = std::function<void()>;

    struct TimeKeyframeData {
        Time time;
        double delta = 0.0;
        bool pause = false;
        bool jump = false;
    };

    TimeManager();

    const Time& time() const;
    const Time& integrateFromTime() const;
    const Timeline<TimeKeyframeData>& timeline() const;

    std::vector<Syncable*> syncables();
    void preSynchronization(double dt);

    TimeKeyframeData interpolate(double applicationTime);

    void setTimeNextFrame(Time t);
    void setDeltaTime(double deltaTime);
    void setPause(bool pause);
    void setDeltaTimeSteps(const std::vector<double> deltaTimes);

    /**
     * Returns the delta time, unaffected by pause.
     */
    double targetDeltaTime() const;

    /*
     * Returns the current delta time, as affected by pause.
     */
    double deltaTime() const;

    /**
     * Sets the simulation time using the time contents of a profile. The function will
     * set either a relative or absolute time.
     *
     * \param p The Profile to be read
     */
    void setTimeFromProfile(const Profile& p);

    bool isPaused() const;

    std::vector<double> deltaTimeSteps() const;

    float defaultTimeInterpolationDuration() const;
    float defaultDeltaTimeInterpolationDuration() const;
    float defaultPauseInterpolationDuration() const;
    float defaultUnpauseInterpolationDuration() const;

    void interpolateTime(double targetTime, double durationSeconds);
    void interpolateTimeRelative(double delta, double durationSeconds);
    void interpolateDeltaTime(double newDeltaTime, double interpolationDuration);
    void interpolatePause(bool pause, double interpolationDuration);

    std::optional<double> nextDeltaTimeStep();
    std::optional<double> previousDeltaTimeStep();
    bool hasNextDeltaTimeStep() const;
    bool hasPreviousDeltaTimeStep() const;
    void setNextDeltaTimeStep();
    void setPreviousDeltaTimeStep();
    void interpolateNextDeltaTimeStep(double durationSeconds);
    void interpolatePreviousDeltaTimeStep(double durationSeconds);

    void addKeyframe(double timestamp, TimeKeyframeData time);
    void removeKeyframesBefore(double timestamp, bool inclusive = false);
    void removeKeyframesAfter(double timestamp, bool inclusive = false);

    size_t nKeyframes() const;
    void clearKeyframes();

    CallbackHandle addTimeChangeCallback(TimeChangeCallback cb);
    CallbackHandle addDeltaTimeChangeCallback(TimeChangeCallback cb);
    CallbackHandle addDeltaTimeStepsChangeCallback(TimeChangeCallback cb);
    CallbackHandle addTimeJumpCallback(TimeChangeCallback cb);
    CallbackHandle addTimelineChangeCallback(TimeChangeCallback cb);

    void removeTimeChangeCallback(CallbackHandle handle);
    void removeDeltaTimeChangeCallback(CallbackHandle handle);
    void removeDeltaTimeStepsChangeCallback(CallbackHandle handle);
    void removeTimeJumpCallback(CallbackHandle handle);
    void removeTimelineChangeCallback(CallbackHandle handle);

private:
    void progressTime(double dt);
    void applyKeyframeData(const TimeKeyframeData& keyframe, double dt);
    TimeKeyframeData interpolate(const Keyframe<TimeKeyframeData>& past,
        const Keyframe<TimeKeyframeData>& future, double time);

    void addDeltaTimesKeybindings();
    void clearDeltaTimesKeybindings();
    double previousApplicationTimeForInterpolation() const;

    Timeline<TimeKeyframeData> _timeline;
    SyncData<Time> _currentTime;
    SyncData<Time> _integrateFromTime;

    bool _timePaused = false;
    double _targetDeltaTime = 1.0;
    double _deltaTime = 0.0;
    double _lastTime = 0.0;
    bool _lastTimePaused = false;
    double _lastDeltaTime = 0.0;
    double _lastTargetDeltaTime = 0.0;
    double _previousApplicationTime = 0.0;

    bool _deltaTimeStepsChanged = false;
    std::vector<double> _deltaTimeSteps;
    std::vector<KeyWithModifier> _deltaTimeStepKeybindings;

    properties::FloatProperty _defaultTimeInterpolationDuration;
    properties::FloatProperty _defaultDeltaTimeInterpolationDuration;
    properties::FloatProperty _defaultPauseInterpolationDuration;
    properties::FloatProperty _defaultUnpauseInterpolationDuration;

    bool _shouldSetTime = false;
    Time _timeNextFrame;

    bool _timelineChanged = false;

    double _latestConsumedTimestamp = -std::numeric_limits<double>::max();
    int _nextCallbackHandle = 0;

    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _timeChangeCallbacks;
    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _deltaTimeChangeCallbacks;
    std::vector<std::pair<CallbackHandle, TimeChangeCallback>>
        _deltaTimeStepsChangeCallbacks;

    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _timeJumpCallbacks;
    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _timelineChangeCallbacks;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TIMEMANAGER___H__
