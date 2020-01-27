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

#ifndef __OPENSPACE_CORE___TOUCH___H__
#define __OPENSPACE_CORE___TOUCH___H__

#include <glm/detail/type_vec2.hpp>

#include <cstdint>
#include <deque>

namespace openspace {

struct TouchInput {
    TouchInput(size_t touchDeviceId, size_t fingerId, float x, float y, double timestamp);
    glm::vec2 screenCoordinates(glm::vec2 resolution) const;
    glm::vec2 currentWindowCoordinates() const;
    bool isMoving() const;
    float distanceToPos(float otherX, float otherY) const;
    float angleToPos(float otherX, float otherY) const;

    size_t touchDeviceId;
    size_t fingerId;
    float x;
    float y;
    float dx = 0.f;         // movement in x direction since last touch input
    float dy = 0.f;         // movement in y direction since last touch input
    double timestamp; // timestamp in seconds from global touch initialization
};

class TouchInputHolder {
public:
    TouchInputHolder(TouchInput input);

    // tryAddInput:
    // Succeeds upon a different input than last.
    // Fails upon a too similar input as last.
    bool tryAddInput(TouchInput input);
    void clearInputs();

    bool holdsInput(const TouchInput &input) const;

    size_t touchDeviceId() const;
    size_t fingerId() const;

    float speedX() const;
    float speedY() const;

    bool isMoving() const;
    float gestureDistance() const;
    double gestureTime() const;

    size_t numInputs() const;
    const TouchInput& latestInput() const;
    const std::deque<TouchInput>& peekInputs() const;

private:
    //A deque of recorded inputs. Adding newer points to the front of the queue
    std::deque<TouchInput> _inputs;

    size_t _touchDeviceId;
    size_t _fingerId;
};

} // namespace openspace

#endif // __OPENSPACE_CORE___TOUCH___H__
