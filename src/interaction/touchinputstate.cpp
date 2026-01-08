/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

namespace openspace::interaction {

void TouchInputState::initialize() {
    _time = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::high_resolution_clock::now().time_since_epoch()
    );
}

bool TouchInputState::touchDetectedCallback(TouchInput i) {
    addTouchInput(std::move(i));
    return true;
}

bool TouchInputState::touchUpdatedCallback(TouchInput i) {
    updateOrAddTouchInput(std::move(i));
    return true;
}

void TouchInputState::touchExitCallback(TouchInput i) {
    removeTouchInput(std::move(i));
}

bool TouchInputState::touchHappened() const {
    return !_touchPoints.empty();
}

bool TouchInputState::isTap() const {
    return _tap;
}

bool TouchInputState::isDoubleTap() const {
    return _doubleTap;
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

    // Reset tap detected state (TODO Can this relly be done here? every frame..?)
    _doubleTap = false;
    _tap = false;
}

void TouchInputState::updateLastTouchPoints() {
    _lastTouchInputs.clear();
    for (const TouchInputHolder& points : _touchPoints) {
        _lastTouchInputs.emplace_back(points.latestInput());
    }
}

void TouchInputState::addTouchInput(TouchInput input) {
    _touchPoints.emplace_back(input);
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

    // Check for "tap" gesture
    if (_touchPoints.size() == 1 && _deferredRemovals.size() == 1) {
        TouchInputHolder& inputHolder = _touchPoints.front();

        if (inputHolder.holdsInput(input)) {
            inputHolder.tryAddInput(input);
            // @TODO: Thís is copied from tuioear.cpp, should be configurable? Or
            // moved to the module that takes tuio input?
            // Magic values taken from tuioear.cpp:
            const bool isWithinTapTime = inputHolder.gestureTime() < 0.18;
            // This makes the tap detection much less responsive,
            // so disable it for now (emmbr, 2025-01-07)
            const bool wasStationary = true; //inputHolder.gestureDistance() < 0.0004f;

            if (isWithinTapTime && wasStationary && _touchPoints.size() == 1 &&
                _deferredRemovals.size() == 1)
            {
                // Check for double tap
                std::chrono::milliseconds timestamp =
                    duration_cast<std::chrono::milliseconds>(
                        std::chrono::high_resolution_clock::now().time_since_epoch()
                    );

                if ((timestamp - _time).count() < _maxDoubleTapTimeInterval) {
                    _doubleTap = true;
                }
                else {
                    _tap = true;
                }
                _time = timestamp;
            }
        }
    }
}

} // namespace openspace::interaction
