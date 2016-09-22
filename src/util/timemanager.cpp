/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2016                                                               *
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
#include <openspace/util/time.h>

namespace {
    double SecondsOffTolerance = 0.1;
}

namespace openspace {

using network::datamessagestructures::TimeKeyframe;

void TimeManager::preSynchronization(double dt) {
    double now = OsEng.runTime();
    removeKeyframesBefore(_latestConsumedTimestamp);
    if (_keyframes.size() == 0) {
        Time::ref().advanceTime(dt);
    } else {
        consumeKeyframes(dt);
    }
}

void TimeManager::consumeKeyframes(double dt) {
    double now = OsEng.runTime();
    TimeKeyframe kf;
    kf._timestamp = now;
    auto firstFutureKeyframe = std::lower_bound(_keyframes.begin(), _keyframes.end(), kf, &TimeManager::compareKeyframeTimes);

    bool consumingTimeJump = std::find_if(_keyframes.begin(), firstFutureKeyframe, [] (const TimeKeyframe& f) {
        return f._requiresTimeJump;
    }) != firstFutureKeyframe;

    Time& time = Time::ref();

    if (firstFutureKeyframe == _keyframes.end()) {
        // All keyframes are in the past.
        // Consume the latest one.
        TimeKeyframe& current = _keyframes.back();
        time.setTime(current._time, consumingTimeJump);
        time.setDeltaTime(current._dt);
        time.setPause(current._paused);
        _latestConsumedTimestamp = current._timestamp;
    }
    else {
        TimeKeyframe& next = *firstFutureKeyframe;

        if (firstFutureKeyframe != _keyframes.begin()) {
            TimeKeyframe& latest = *(firstFutureKeyframe - 1);
            // In case of unconsumed passed keyframes, let the last one
            // determine whether the time should be paused or not.
            // If there was a time jump or time is paused, apply it directly.
            // Then consume the last keyframe.

            time.setPause(latest._paused);
            time.setTimeJumped(consumingTimeJump);
            time.setDeltaTime(latest._dt);

            if (consumingTimeJump || latest._paused) {
                time.setTime(latest._time, consumingTimeJump);
            }
            _latestConsumedTimestamp = latest._timestamp;
        }

        // Do not interpolate with time jumping keyframes.
        // Instead, wait until their timestamp and apply them directly.
        if (next._requiresTimeJump) {
            time.setTime(time.j2000Seconds() + dt * time.deltaTime(), false);
            return;
        }

        if (time.paused()) {
            return;
        }

        double predictedTime = time.j2000Seconds() + time.deltaTime() * (next._timestamp - now);
        bool withinTolerance = std::abs(predictedTime - next._time) < std::abs(next._dt * SecondsOffTolerance);
        
        if (next._dt == time.deltaTime() && withinTolerance) {
            Time::ref().advanceTime(dt);
            return;
        }

        double t0 = now - dt;
        double t1 = now;
        double t2 = next._timestamp;

        double parameter = (t1 - t0) / (t2 - t0);
        
        double y0 = time.j2000Seconds();
        double yPrime0 = time.deltaTime();

        double y2 = next._time;
        double yPrime2 = next._dt;

        double y1 = (1 - parameter) * y0 + parameter * y2;
        // For continuous derivative, use this instead:
        // double y1 = (1 - parameter) * (y0 + (t1 - t0) * yPrime0) + parameter * (y2 + (t1 - t2) * yPrime2);
        double y1Prime = (y1 - y0) / dt;

        time.setDeltaTime(y1Prime);
        time.setTime(y1, false);
    }
}


void TimeManager::addKeyframe(const TimeKeyframe& kf) {
    if (kf._timestamp < OsEng.runTime()) {
        return;
    }
    auto iter = std::upper_bound(_keyframes.begin(), _keyframes.end(), kf, &TimeManager::compareKeyframeTimes);
    _keyframes.insert(iter, kf);
}


void TimeManager::removeKeyframesBefore(double timestamp) {
    network::datamessagestructures::TimeKeyframe kf;
    kf._timestamp = timestamp;
    auto iter = std::upper_bound(_keyframes.begin(), _keyframes.end(), kf, &TimeManager::compareKeyframeTimes);
    _keyframes.erase(_keyframes.begin(), iter);
}

void TimeManager::clearKeyframes() {
    _keyframes.clear();
}

bool TimeManager::compareKeyframeTimes(const TimeKeyframe& a, const TimeKeyframe& b)
{
    return a._timestamp < b._timestamp;
}

}