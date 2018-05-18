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

#include <utility>
#include <vector>
#include <deque>
#include <functional>
#include <openspace/util/timeline.h>
#include <openspace/util/time.h>
#include <openspace/util/syncdata.h>

namespace openspace {

struct TimeKeyframeData {
    Time time;
    double delta;
    bool pause = false;
    bool jump = false;
};

class TimeManager {
public:
    using CallbackHandle = int;
    using TimeChangeCallback = std::function<void()>;

    Time& time();
    std::vector<Syncable*> getSyncables();
    void preSynchronization(double dt);
    void addKeyframe(double timestamp, TimeKeyframeData kf);
    void removeKeyframesBefore(double timestamp, bool inclusive = false);
    void removeKeyframesAfter(double timestamp, bool inclusive = false);
    void clearKeyframes();
    void setTimeNextFrame(Time t);
    size_t nKeyframes() const;

    double deltaTime() const;
    void setDeltaTime(double deltaTime, double interpolationDuration = 0.0);

    void setPause(bool pause, double interpolationDuration = 0.0);
    bool togglePause(double interpolationDuration = 0.0);
    bool isPaused() const;

    CallbackHandle addTimeChangeCallback(TimeChangeCallback cb);
    CallbackHandle addDeltaTimeChangeCallback(TimeChangeCallback cb);
    CallbackHandle addTimeJumpCallback(TimeChangeCallback cb);

    void removeTimeChangeCallback(CallbackHandle handle);
    void removeDeltaTimeChangeCallback(CallbackHandle handle);
    void removeTimeJumpCallback(CallbackHandle handle);
private:
    void progressTime(double dt);
    void applyKeyframe(const Keyframe<TimeKeyframeData>& keyframe);
    void interpolate(
        const Keyframe<TimeKeyframeData>& past,
        const Keyframe<TimeKeyframeData>& future,
        double time);

    bool _timePaused = false;
    double _deltaTime = 1.0;

    bool _shouldSetTime = false;
    Time _timeNextFrame;

    double _lastTime = 0;
    double _lastDeltaTime = 0;

    Timeline<TimeKeyframeData> _timeline;
    SyncData<Time> _currentTime;

    double _latestConsumedTimestamp;
    int _nextCallbackHandle = 0;

    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _timeChangeCallbacks;
    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _deltaTimeChangeCallbacks;
    std::vector<std::pair<CallbackHandle, TimeChangeCallback>> _timeJumpCallbacks;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TIMEMANAGER___H__
