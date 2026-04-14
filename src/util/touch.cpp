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

#include <openspace/util/touch.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <cmath>
#include <cstdlib>

namespace openspace {

TouchInput::TouchInput(size_t touchDeviceId_, size_t fingerId_, glm::vec2 pos_,
                       double timestamp_)
    : touchDeviceId(touchDeviceId_)
    , fingerId(fingerId_)
    , pos(std::move(pos_))
    , timestamp(timestamp_)
{}

glm::vec2 TouchInput::screenCoordinates(const glm::vec2& resolution) const {
    return glm::vec2(
        std::floor(pos.x * resolution.x + 0.5f),
        std::floor(pos.y * resolution.y + 0.5f)
    );
}

glm::vec2 TouchInput::currentWindowCoordinates() const {
    const glm::vec2 res = global::windowDelegate->currentSubwindowSize();
    return glm::vec2(std::floor(pos.x * res.x + 0.5f), std::floor(pos.y * res.y + 0.5f));
}

bool TouchInput::isMoving() const {
    return dPos.x != 0.f || dPos.y != 0.f;
}

float TouchInput::distanceToPos(const glm::vec2& other) const {
    const glm::vec2 dist = pos - other;
    return glm::length(dist);
}

float TouchInput::angleToPos(const glm::vec2& other) const {
    const float side = pos.x - other.x;
    const float height = pos.y - other.y;
    const float distance = distanceToPos(other);

    float angle = glm::half_pi<float>() + std::asin(side / distance);
    if (height < 0.f) {
        angle = 2.f * glm::pi<float>() - angle;
    }

    return angle;
}

TouchInputHolder::TouchInputHolder(TouchInput input)
    : _inputs({ input })
    , _firstInput(input)
    , _touchDeviceId(input.touchDeviceId)
    , _fingerId(input.fingerId)
{}

bool TouchInputHolder::tryAddInput(TouchInput input) {
    if (_inputs.empty()) {
        _inputs.emplace_front(input);
        return true;
    }
    const TouchInput& lastInput = latestInput();
    input.dPos = input.pos - lastInput.pos;

    constexpr double OneMs = 0.001;
    const bool sameTimeAsLastInput = (input.timestamp - lastInput.timestamp) < OneMs;
    bool successful = false;
    if (!sameTimeAsLastInput && isMoving()) {
        _inputs.emplace_front(input);
        successful = true;
    }
    else if (!sameTimeAsLastInput && input.isMoving()) {
        _inputs.emplace_front(input);
        successful = true;
    }
    else if (!sameTimeAsLastInput) {
        _inputs.front().timestamp = input.timestamp;
        successful = true;
    }

    constexpr int MaxInputs = 128;
    if (_inputs.size() > MaxInputs) {
        _inputs.pop_back();
    }
    return successful;
}

void TouchInputHolder::clearInputs() {
    _inputs.clear();
}

bool TouchInputHolder::holdsInput(const TouchInput& input) const {
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
    return currentInput.dPos.x / dt;
}

float TouchInputHolder::speedY() const {
    if (_inputs.size() <= 1) {
        return 0.f;
    }
    const TouchInput& currentInput = _inputs[0];
    const TouchInput& previousInput = _inputs[1];
    const float dt = static_cast<float>(currentInput.timestamp - previousInput.timestamp);
    return currentInput.dPos.y / dt;
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
    glm::vec2 dist = glm::vec2(0.f);
    const glm::vec2 start = _inputs.front().pos;
    for (const TouchInput& input : _inputs) {
        dist += glm::abs(input.pos - start);
    }
    return glm::length(dist);
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
