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

#ifndef __OPENSPACE_CORE___TIMELINE___H__
#define __OPENSPACE_CORE___TIMELINE___H__

#include <deque>

namespace openspace {

struct KeyframeBase {
    KeyframeBase(size_t i, double t)
        : id(i)
        , timestamp(t)
    {}
    size_t id;
    double timestamp;
};

template <typename T>
struct Keyframe : public KeyframeBase {
    Keyframe(size_t i, double t, T p)
        : KeyframeBase(i, t)
        , payload(p)
    {}
    T payload;
};

template <typename T>
class Timeline {
public:
    Timeline();
    void addKeyframe(double time, T payload);
    void clearKeyframes();
    void removeKeyframe(size_t id);
    void removeKeyframesBefore(double timestamp, bool inclusive = false);
    void removeKeyframesAfter(double timestamp, bool inclusive = false);
    void removeKeyframesBetween(double begin, double end, bool inclusiveBegin = false, bool inclusiveEnd = false);
    size_t nKeyframes() const;
    const Keyframe<T>* firstKeyframeAfter(double timestamp, bool inclusive = false) const;
    const Keyframe<T>* lastKeyframeBefore(double timestamp, bool inclusive = false) const;
    const std::deque<Keyframe<T>>& keyframes() const;
private:
    size_t _nextKeyframeId;
    std::deque<Keyframe<T>> _keyframes;
};

bool compareKeyframeTimes(const KeyframeBase& a, const KeyframeBase& b);

bool compareTimeWithKeyframeTime(double a, const KeyframeBase& b);

bool compareKeyframeTimeWithTime(const KeyframeBase& a, double b);

} // namespace openspace

#include <openspace/util/timeline.inl>;

#endif // __OPENSPACE_CORE___TIMELINE___H__
