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

#ifndef __OPENSPACE_CORE___TIMEMANAGER___H__
#define __OPENSPACE_CORE___TIMEMANAGER___H__

#include <openspace/util/syncdata.h>
#include <openspace/util/time.h>
#include <openspace/util/timeline.h>
#include <functional>
#include <utility>
#include <vector>

#include <deque>
#include <functional>
#include <openspace/util/timeline.h>
#include <openspace/util/time.h>
#include <openspace/util/syncdata.h>
#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalar/floatproperty.h>

namespace openspace {

struct TimeKeyframeData {
    Time time;
    double delta;
    bool pause = false;
    bool jump = false;
};

class TimeManager : public properties::PropertyOwner {
public:
    using CallbackHandle = int;
    using TimeChangeCallback = std::function<void()>;

    TimeManager();

    const Time& time() const;
    const Time& integrateFromTime() const;
    const Timeline<TimeKeyframeData>& timeline() const;

    std::vector<Syncable*> getSyncables();
    void preSynchronization(double dt);

    TimeKeyframeData interpolate(double applicationTime);

    void setTimeNextFrame(Time t);
    void setDeltaTime(double deltaTime);
    void setPause(bool pause);

    /**
     * Returns the delta time, unaffected by pause
     */
    double targetDeltaTime() const;

    /*
     * Returns the current delta time, as affected by pause
     */
    double deltaTime() const;
    bool isPaused() const;

    float defaultTimeInterpolationDuration() const;
    float defaultDeltaTimeInterpolationDuration() const;
    float defaultPauseInterpolationDuration() const;
    float defaultUnpauseInterpolationDuration() const;

    void interpolateTime(double targetTime, double durationSeconds);
    void interpolateDeltaTime(double targetDeltaTime, double durationSeconds);
    void interpolatePause(bool pause, double durationSeconds);

    void addKeyframe(double timestamp, TimeKeyframeData kf);
    void removeKeyframesBefore(double timestamp, bool inclusive = false);
    void removeKeyframesAfter(double timestamp, bool inclusive = false);

    size_t nKeyframes() const;
    void clearKeyframes();

    CallbackHandle addTimeChangeCallback(TimeChangeCallback cb);
    CallbackHandle addDeltaTimeChangeCallback(TimeChangeCallback cb);
    CallbackHandle addTimeJumpCallback(TimeChangeCallback cb);
    CallbackHandle addTimelineChangeCallback(TimeChangeCallback cb);

    void removeTimeChangeCallback(CallbackHandle handle);
    void removeDeltaTimeChangeCallback(CallbackHandle handle);
    void removeTimeJumpCallback(CallbackHandle handle);
    void removeTimelineChangeCallback(CallbackHandle handle);

private:
    void progressTime(double dt);
    void applyKeyframeData(const TimeKeyframeData& keyframe);
    TimeKeyframeData interpolate(const Keyframe<TimeKeyframeData>& past,
        const Keyframe<TimeKeyframeData>& future, double time);

    Timeline<TimeKeyframeData> _timeline;
    SyncData<Time> _currentTime;
    SyncData<Time> _integrateFromTime;

    bool _timePaused = false;
    double _targetDeltaTime = 1.0;
    double _deltaTime = 0.0;
    double _lastTime = 0;
    double _lastDeltaTime = 0;

    properties::FloatProperty _defaultTimeInterpolationDuration;
    properties::FloatProperty _defaultDeltaTimeInterpolationDuration;
    properties::FloatProperty _defaultPauseInterpolationDuration;
    properties::FloatProperty _defaultUnpauseInterpolationDuration;

    bool _shouldSetTime = false;
    Time _timeNextFrame;

    bool _timelineChanged;

    double _latestConsumedTimestamp = -std::numeric_limits<double>::max();
    int _nextCallbackHandle = 0;

    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _timeChangeCallbacks;
    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _deltaTimeChangeCallbacks;
    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _timeJumpCallbacks;
    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _timelineChangeCallbacks;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TIMEMANAGER___H__
