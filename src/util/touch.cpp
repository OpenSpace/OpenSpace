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

#define _USE_MATH_DEFINES
#include <openspace/util/touch.h>
#include <cmath>

namespace openspace {

TouchInput::TouchInput(size_t touchDeviceId, size_t fingerId, float x, float y)
: touchDeviceId(touchDeviceId), fingerId(fingerId), x(x), y(y), dx(0.f), dy(0.f)
, timestamp(0.0)
{}

glm::vec2 TouchInput::getScreenCoordinates(glm::vec2 resolution) const {
    return {std::floor(x * resolution.x + 0.5f), std::floor(y * resolution.y + 0.5f)};
}

float TouchInput::getDistanceToPos(float otherX, float otherY) const {
    float distX = x - otherX;
    float distY = y - otherY;
    return std::sqrt(distX*distX + distY*distY);
}

float TouchInput::getAngleToPos(float otherX, float otherY) const {
    float side = x - otherX;
    float height = y - otherY;
    float distance = getDistanceToPos(otherX, otherY);

    float angle = float(M_PI_2) + std::asin(side / distance);
    if (height < 0.f) {
        angle = 2.0f * float(M_PI) - angle;
    }

    return angle;
}

TouchInputs::TouchInputs(TouchInput input)
    : _inputs{input}
    , _touchDeviceId(input.touchDeviceId)
    , _fingerId(input.fingerId)
{}

void TouchInputs::addInput(const TouchInput &input) {
    _inputs.emplace_front(input);
    if(_inputs.size() > MAX_INPUTS){
        _inputs.pop_back();
    }
}

void TouchInputs::clearInputs() {
    _inputs.clear();
}

const size_t TouchInputs::getTouchDeviceId() const {
    return _touchDeviceId;
}

const size_t TouchInputs::getFingerId() const {
    return _fingerId;
}

float TouchInputs::getCurrentSpeed() const {
    if(_inputs.size() <= 1){ return 0.0; }
    TouchInput currentInput = _inputs[0];
    TouchInput previousInput = _inputs[1];
    //dt in seconds:
    float dt = static_cast<float>(currentInput.timestamp - previousInput.timestamp);
    float dist = sqrt(currentInput.dx*currentInput.dx + currentInput.dy*currentInput.dy);
    return dist / dt;
}

float TouchInputs::getSpeedX() const {
    if(_inputs.size() <= 1) { return 0.0; }
    const TouchInput &currentInput = _inputs[0];
    const TouchInput &previousInput = _inputs[1];
    float dt = static_cast<float>(currentInput.timestamp - previousInput.timestamp);

    return currentInput.dx / dt;
}

float TouchInputs::getSpeedY() const {
    if(_inputs.size() <= 1) { return 0.0; }
    const TouchInput &currentInput = _inputs[0];
    const TouchInput &previousInput = _inputs[1];
    float dt = static_cast<float>(currentInput.timestamp - previousInput.timestamp);

    return currentInput.dy / dt;
}

bool TouchInputs::isMoving() const {
    if(_inputs.size() <= 1) { return false; }
    const TouchInput &currentInput = _inputs[0];
    return currentInput.dx != 0.f || currentInput.dy != 0.f;
}

float TouchInputs::getGestureDistance() const {
    if(_inputs.size() <= 1) { return 0.f; }
    float distX = 0.f;
    float distY = 0.f;
    float startX = _inputs.front().x;
    float startY = _inputs.front().y;
    for(const auto& input : _inputs){
        distX += std::abs(input.x - startX);
        distY += std::abs(input.y - startY);
    }
    return std::sqrt(distX*distX + distY*distY);
}

double TouchInputs::getGestureTime() const {
    if(_inputs.size() <= 1) { return 0.0; }
    double before = _inputs.back().timestamp;
    double after = _inputs.front().timestamp;
    return after - before;
}

size_t TouchInputs::getNumInputs() const {
    return _inputs.size();
}

const TouchInput& TouchInputs::getLatestInput() const {
    return _inputs.front();
}

const std::deque<TouchInput>& TouchInputs::peekInputs() const { return _inputs; }

} // namespace openspace
