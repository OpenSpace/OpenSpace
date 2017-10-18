/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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
#include <openspace/network/parallelconnection.h>
#include <openspace/util/timeline.h>

namespace openspace {

using datamessagestructures::TimeKeyframe;

void TimeManager::preSynchronization(double dt) {
//    double now = OsEng.runTime();
    removeKeyframesBefore(_latestConsumedTimestamp);
    if (_shouldSetTime) {
        time().setTime(_timeNextFrame.j2000Seconds());
        _shouldSetTime = false;
    } else if (_timeline.nKeyframes() == 0) {
        time().advanceTime(dt);
    } else {
        consumeKeyframes(dt);
    }
}

void TimeManager::consumeKeyframes(double dt) {
    double now = OsEng.runTime();
    
    const std::deque<Keyframe<Time>>& keyframes = _timeline.keyframes();
    auto firstFutureKeyframe = std::lower_bound(keyframes.begin(), keyframes.end(), now, &compareKeyframeTimeWithTime);

    bool consumingTimeJump = std::find_if(keyframes.begin(), firstFutureKeyframe, [] (const Keyframe<Time>& f) {
        return f.data.timeJumped();
    }) != firstFutureKeyframe;

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

        const double secondsOffTolerance = OsEng.parallelConnection().timeTolerance();

        double predictedTime = time().j2000Seconds() + time().deltaTime() * (next.timestamp - now);
        bool withinTolerance = std::abs(predictedTime - nextTime.j2000Seconds()) < std::abs(nextTime.deltaTime() * secondsOffTolerance);
        
        if (nextTime.deltaTime() == time().deltaTime() && withinTolerance) {
            time().advanceTime(dt);
            return;
        }

        double t0 = now - dt;
        double t1 = now;
        double t2 = next.timestamp;

        double parameter = (t1 - t0) / (t2 - t0);
        
        double y0 = time().j2000Seconds();
        double yPrime0 = time().deltaTime();

        double y2 = nextTime.j2000Seconds();
        double yPrime2 = nextTime.deltaTime();

        double y1 = (1 - parameter) * y0 + parameter * y2;
        double y1Prime = (y1 - y0) / dt;

        time().setDeltaTime(y1Prime);
        time().setTime(y1, false);
    }
}

void TimeManager::addKeyframe(double timestamp, Time time) {
    _timeline.addKeyframe(timestamp, time);
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
    _timeNextFrame = t;
}

size_t TimeManager::nKeyframes() const {
    return _timeline.nKeyframes();
}

Time& TimeManager::time() {
    return _currentTime;
}

std::vector<Syncable*> TimeManager::getSyncables() {
    return{ &_currentTime };
}

}
