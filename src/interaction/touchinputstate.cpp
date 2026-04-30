/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/interaction/touchinputstate.h>

namespace {
    std::chrono::milliseconds now() {
        return std::chrono::duration_cast<std::chrono::milliseconds>(
            std::chrono::high_resolution_clock::now().time_since_epoch()
        );
    }
} // namespace

namespace openspace {

void TouchInputState::initialize() {
    _lastTapTime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::high_resolution_clock::now().time_since_epoch()
    );
}

void TouchInputState::touchDetectedCallback(TouchInput i) {
    // We know this is a completely new input, so just add it immediately
    _touchPoints.emplace_back(i);
}

void TouchInputState::touchUpdatedCallback(TouchInput i) {
    updateOrAddTouchInput(std::move(i));
}

void TouchInputState::touchExitCallback(TouchInput i) {
    removeTouchInput(std::move(i));
}

bool TouchInputState::touchHappened() const {
    return !_touchPoints.empty();
}

bool TouchInputState::isTap() const {
    return _isTap;
}

bool TouchInputState::isDoubleTap() const {
    return _isDoubleTap;
}

bool TouchInputState::isTripleTap() const {
    return _isTripleTap;
}

void TouchInputState::setMaxDoubleTapTime(unsigned int milliseconds)  {
    _maxDoubleTapTimeInterval = milliseconds;
}

const std::vector<TouchInputHolder>& TouchInputState::touchPoints() const {
    return _touchPoints;
}

const std::vector<TouchInput>& TouchInputState::lastProcessedInputs() const {
    return _lastTouchInputs;
}

void TouchInputState::processTouchInput(const std::vector<TouchInput>& inputs,
                                        const std::vector<TouchInput>& removals)
{
    for (const TouchInput& input : inputs) {
        updateOrAddTouchInput(input);
    }
    for (const TouchInput& removal : removals) {
        removeTouchInput(removal);
    }

    // Erase old input ids that no longer exist
    _lastTouchInputs.erase(
        std::remove_if(
            _lastTouchInputs.begin(),
            _lastTouchInputs.end(),
            [this](const TouchInput& input) {
                return !std::any_of(
                    _touchPoints.cbegin(),
                    _touchPoints.cend(),
                    [&input](const TouchInputHolder& holder) {
                        return holder.holdsInput(input);
                    }
                );
            }
        ),
        _lastTouchInputs.end()
    );
}

void TouchInputState::clearInputs() {
    // Clear old tap data and evaluate new tap gesture so we have it for next frame
    clearTapData();
    if (_touchPoints.size() == 1 && _deferredRemovals.size() == 1) {
        // All fingers lifted => evaluate tap
        evaluateTap(_deferredRemovals.back());
    }

    for (const TouchInput& input : _deferredRemovals) {
        for (TouchInputHolder& inputHolder : _touchPoints) {
            if (inputHolder.holdsInput(input)) {
                inputHolder = std::move(_touchPoints.back());
                _touchPoints.pop_back();
                break;
            }
        }
    }
    _deferredRemovals.clear();
}

void TouchInputState::updateLastTouchPoints() {
    _lastTouchInputs.clear();
    for (const TouchInputHolder& points : _touchPoints) {
        _lastTouchInputs.emplace_back(points.latestInput());
    }
}

void TouchInputState::updateOrAddTouchInput(TouchInput input) {
    for (TouchInputHolder& inputHolder : _touchPoints) {
        if (inputHolder.holdsInput(input)) {
            inputHolder.tryAddInput(input);
            return;
        }
    }
    _touchPoints.emplace_back(input);
}

void TouchInputState::removeTouchInput(TouchInput input) {
    _deferredRemovals.emplace_back(input);

    // Update so the holder has the final gesture data
    TouchInputHolder& inputHolder = _touchPoints.front();
    if (inputHolder.holdsInput(input)) {
        inputHolder.tryAddInput(input);
    }
}

void TouchInputState::clearTapData() {
    // Reset the timers so we don't keep detecting the combo taps if tapping continuously.
    // After a triple, we should start on single tap again
    if (_isDoubleTap || _isTripleTap) {
        _lastTapTime = std::chrono::milliseconds::zero();
    }
    if (_isTripleTap) {
        _lastDoubleTapTime = std::chrono::milliseconds::zero();
    }

    _isTap = false;
    _isDoubleTap = false;
    _isTripleTap = false;
}

void TouchInputState::evaluateTap(const TouchInput& lastRemovedInput) {
    TouchInputHolder& inputHolder = _touchPoints.front();

    if (!inputHolder.holdsInput(lastRemovedInput)) {
        return;
    }

    const bool isWithinTapTime = inputHolder.gestureTime() < 0.18;

    if (isWithinTapTime) {
        std::chrono::milliseconds time = now();

        // @TODO (emmbr26, 2026-04-16) We should also check that the positions of the taps
        // match. Now you can quickly tap with both hand on multiple sides of the screen
        // and it will register as a multi-tap. But I don't consider this too much of an
        // issue right now.

        if ((time - _lastDoubleTapTime).count() < _maxDoubleTapTimeInterval) {
            _isTripleTap = true;
        }
        else if ((time - _lastTapTime).count() < _maxDoubleTapTimeInterval) {
            _isDoubleTap = true;
            _lastDoubleTapTime = time;
        }
        else {
            _isTap = true;
        }

        _lastTapTime = time;
    }
}

} // namespace openspace
