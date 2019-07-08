/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

namespace openspace {

template <typename T, bool C>
void Timeline<T, C>::addKeyframe(double timestamp, T&& data) {
    Keyframe<T, C> keyframe(++_nextKeyframeId, timestamp, std::move(data));
    const auto iter = std::upper_bound(
        _keyframes.cbegin(),
        _keyframes.cend(),
        keyframe,
        &compareKeyframeTimes
    );
    _keyframes.insert(iter, keyframe);
}


template <typename T, bool C>
void Timeline<T, C>::removeKeyframesAfter(double timestamp, bool inclusive) {
    typename std::deque<Keyframe<T>>::const_iterator iter;
    if (inclusive) {
        iter = std::lower_bound(
            _keyframes.cbegin(),
            _keyframes.cend(),
            timestamp,
            &compareKeyframeTimeWithTime
        );
    }
    else {
        iter = std::upper_bound(
            _keyframes.cbegin(),
            _keyframes.cend(),
            timestamp,
            &compareTimeWithKeyframeTime
        );
    }

    _keyframes.erase(iter, _keyframes.end());
}

template <typename T, bool C>
void Timeline<T, C>::removeKeyframesBefore(double timestamp, bool inclusive) {
    typename std::deque<Keyframe<T>>::const_iterator iter;
    if (inclusive) {
        iter = std::upper_bound(
            _keyframes.cbegin(),
            _keyframes.cend(),
            timestamp,
            &compareTimeWithKeyframeTime
        );
    }
    else {
        iter = std::lower_bound(
            _keyframes.cbegin(),
            _keyframes.cend(),
            timestamp,
            &compareKeyframeTimeWithTime
        );
    }

    _keyframes.erase(_keyframes.begin(), iter);
}

template <typename T, bool C>
void Timeline<T, C>::removeKeyframesBetween(double begin, double end, bool inclusiveBegin,
                                         bool inclusiveEnd)
{
    typename std::deque<Keyframe<T>>::const_iterator beginIter;
    if (inclusiveBegin) {
        beginIter = std::lower_bound(
            _keyframes.cbegin(),
            _keyframes.cend(),
            begin,
            &compareKeyframeTimeWithTime
        );
    }
    else {
        beginIter = std::upper_bound(
            _keyframes.cbegin(),
            _keyframes.cend(),
            begin,
            &compareTimeWithKeyframeTime
        );
    }


    typename std::deque<Keyframe<T>>::const_iterator endIter;
    if (inclusiveEnd) {
        endIter = std::upper_bound(
            beginIter,
            _keyframes.cend(),
            end,
            &compareTimeWithKeyframeTime
        );
    }
    else {
        endIter = std::lower_bound(
            beginIter,
            _keyframes.cend(),
            end,
            &compareKeyframeTimeWithTime
        );
    }

    _keyframes.erase(beginIter, endIter);
}

template <typename T, bool C>
void Timeline<T, C>::clearKeyframes() {
    _keyframes.clear();
}

template <typename T, bool C>
void Timeline<T, C>::removeKeyframe(size_t id) {
    _keyframes.erase(
        std::remove_if(
            _keyframes.begin(),
            _keyframes.end(),
            [id] (Keyframe<T> keyframe) { return keyframe.id == id; }
        ),
        _keyframes.end()
    );
}

template <typename T, bool C>
size_t Timeline<T, C>::nKeyframes() const {
    return _keyframes.size();
}

template <typename T, bool C>
const Keyframe<T, C>* Timeline<T, C>::firstKeyframeAfter(double timestamp,
                                                         bool inclusive) const
{
    typename std::deque<Keyframe<T, C>>::const_iterator it;
    if (inclusive) {
        it = std::lower_bound(
            _keyframes.begin(),
            _keyframes.end(),
            timestamp,
            &compareKeyframeTimeWithTime
        );
    }
    else {
        it = std::upper_bound(
            _keyframes.begin(),
            _keyframes.end(),
            timestamp,
            &compareTimeWithKeyframeTime
        );
    }

    if (it == _keyframes.end()) {
        return nullptr;
    }
    return &(*it);
}

template <typename T, bool C>
const Keyframe<T, C>* Timeline<T, C>::lastKeyframeBefore(double timestamp,
                                                         bool inclusive) const
{
    typename std::deque<Keyframe<T, C>>::const_iterator it;
    if (inclusive) {
        it = std::upper_bound(
            _keyframes.begin(),
            _keyframes.end(),
            timestamp,
            &compareTimeWithKeyframeTime
        );
    }
    else {
        it = std::lower_bound(
            _keyframes.begin(),
            _keyframes.end(),
            timestamp,
            &compareKeyframeTimeWithTime
        );
    }

    if (it == _keyframes.begin()) {
        return nullptr;
    }
    it--;
    return &(*it);
}

template <typename T, bool C>
const std::deque<Keyframe<T, C>>& Timeline<T, C>::keyframes() const {
    return _keyframes;
}

}  // namespace openspace
