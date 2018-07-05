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

#ifndef __OPENSPACE_CORE___TIMELINE___H__
#define __OPENSPACE_CORE___TIMELINE___H__

#include <algorithm>
#include <deque>
#include <cstddef>

namespace openspace {

/**
* Base class for keyframes
*/
struct KeyframeBase {
    size_t id;
    double timestamp;
};

/**
* Templated class for keyframes containing data
*/
template <typename T>
struct Keyframe : public KeyframeBase {
    Keyframe(size_t i, double t, T p);
    T data;
};

/**
* Templated class for timelines
*/
template <typename T>
class Timeline {
public:
    virtual ~Timeline() = default;

    void addKeyframe(double time, T data);
    void clearKeyframes();
    void removeKeyframe(size_t id);
    void removeKeyframesBefore(double timestamp, bool inclusive = false);
    void removeKeyframesAfter(double timestamp, bool inclusive = false);
    void removeKeyframesBetween(double begin, double end, bool inclusiveBegin = false,
        bool inclusiveEnd = false);
    size_t nKeyframes() const;
    const Keyframe<T>* firstKeyframeAfter(double timestamp, bool inclusive = false) const;
    const Keyframe<T>* lastKeyframeBefore(double timestamp, bool inclusive = false) const;
    const std::deque<Keyframe<T>>& keyframes() const;

private:
    size_t _nextKeyframeId = 1;
    std::deque<Keyframe<T>> _keyframes;
};

/**
* Return true if the timestamp of a is smaller the timestamp of b.
*/
bool compareKeyframeTimes(const KeyframeBase& a, const KeyframeBase& b);

/**
* Return true if a is smaller than the timestamp of b.
*/
bool compareTimeWithKeyframeTime(double a, const KeyframeBase& b);

/**
* Return true if the timestamp of a is smaller than b.
*/
bool compareKeyframeTimeWithTime(const KeyframeBase& a, double b);

} // namespace openspace

#include "timeline.inl"

#endif // __OPENSPACE_CORE___TIMELINE___H__
