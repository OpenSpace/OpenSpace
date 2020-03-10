/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/util/touch.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <cmath>

namespace openspace {

TouchInput::TouchInput(size_t touchDeviceId, size_t fingerId, float x, float y,
                       double timestamp)
    : touchDeviceId(touchDeviceId)
    , fingerId(fingerId)
    , x(x)
    , y(y)
    , timestamp(timestamp)
{}

glm::vec2 TouchInput::screenCoordinates(glm::vec2 resolution) const {
    return { std::floor(x * resolution.x + 0.5f), std::floor(y * resolution.y + 0.5f) };
}

glm::vec2 TouchInput::currentWindowCoordinates() const {
    glm::vec2 res = global::windowDelegate.currentSubwindowSize();
    return { std::floor(x * res.x + 0.5f), std::floor(y * res.y + 0.5f) };
}

bool TouchInput::isMoving() const {
    return dx != 0.f || dy != 0.f;
}

float TouchInput::distanceToPos(float otherX, float otherY) const {
    const float distX = x - otherX;
    const float distY = y - otherY;
    return std::sqrt(distX*distX + distY*distY);
}

float TouchInput::angleToPos(float otherX, float otherY) const {
    const float side = x - otherX;
    const float height = y - otherY;
    const float distance = distanceToPos(otherX, otherY);

    float angle = glm::half_pi<float>() + std::asin(side / distance);
    if (height < 0.f) {
        angle = 2.f * glm::pi<float>() - angle;
    }

    return angle;
}

TouchInputHolder::TouchInputHolder(TouchInput input)
    : _inputs{ input }
    , _firstInput(input)
    , _touchDeviceId(input.touchDeviceId)
    , _fingerId(input.fingerId)
{}

bool TouchInputHolder::tryAddInput(TouchInput input) {
    if(_inputs.empty()) {
        _inputs.emplace_front(input);
        return true;
    }
    constexpr const double ONE_MS = 0.001;
    const TouchInput& lastInput = latestInput();
    input.dx = input.x - lastInput.x;
    input.dy = input.y - lastInput.y;

    const bool sameTimeAsLastInput = (input.timestamp - lastInput.timestamp) < ONE_MS;
    bool successful = false;
    if (!sameTimeAsLastInput && isMoving()) {
        _inputs.emplace_front(input);
        successful = true;
    }
    else if (!sameTimeAsLastInput && input.isMoving()) {
        _inputs.emplace_front(input);
        successful = true;
    }
    else if (!sameTimeAsLastInput){
        _inputs.front().timestamp = input.timestamp;
        successful = true;
    }

    constexpr const int MaxInputs = 128;
    if (_inputs.size() > MaxInputs) {
        _inputs.pop_back();
    }
    return successful;
}

void TouchInputHolder::clearInputs() {
    _inputs.clear();
}

bool TouchInputHolder::holdsInput(const TouchInput &input) const {
    return input.fingerId == _fingerId && input.touchDeviceId == _touchDeviceId;
}

size_t TouchInputHolder::touchDeviceId() const {
    return _touchDeviceId;
}

size_t TouchInputHolder::fingerId() const {
    return _fingerId;
}

float TouchInputHolder::speedX() const {
    if (_inputs.size() <= 1) {
        return 0.f;
    }
    const TouchInput& currentInput = _inputs[0];
    const TouchInput& previousInput = _inputs[1];
    const float dt = static_cast<float>(currentInput.timestamp - previousInput.timestamp);
    return currentInput.dx / dt;
}

float TouchInputHolder::speedY() const {
    if (_inputs.size() <= 1) {
        return 0.f;
    }
    const TouchInput& currentInput = _inputs[0];
    const TouchInput& previousInput = _inputs[1];
    const float dt = static_cast<float>(currentInput.timestamp - previousInput.timestamp);

    return currentInput.dy / dt;
}

bool TouchInputHolder::isMoving() const {
    if (_inputs.size() <= 1) {
        return false;
    }
    return latestInput().isMoving();
}

float TouchInputHolder::gestureDistance() const {
    if (_inputs.size() <= 1) {
        return 0.f;
    }
    float distX = 0.f;
    float distY = 0.f;
    const float startX = _inputs.front().x;
    const float startY = _inputs.front().y;
    for (const TouchInput& input : _inputs) {
        distX += std::abs(input.x - startX);
        distY += std::abs(input.y - startY);
    }
    return std::sqrt(distX*distX + distY*distY);
}

double TouchInputHolder::gestureTime() const {
    if (_inputs.size() <= 1) {
        return 0.0;
    }
    const double before = _inputs.back().timestamp;
    const double after = _inputs.front().timestamp;
    return after - before;
}

size_t TouchInputHolder::numInputs() const {
    return _inputs.size();
}

const TouchInput& TouchInputHolder::firstInput() const {
    return _firstInput;
}

const TouchInput& TouchInputHolder::latestInput() const {
    return _inputs.front();
}

const std::deque<TouchInput>& TouchInputHolder::peekInputs() const {
    return _inputs;
}

} // namespace openspace
