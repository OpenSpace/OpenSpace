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

#include <ghoul/misc/assert.h>
#include <ghoul/glm.h>

namespace openspace::interaction {

template <typename T, typename ScaleType>
DelayedVariable<T, ScaleType>::DelayedVariable(ScaleType scaleFactor, ScaleType friction)
    : _scaleFactor(std::move(scaleFactor))
    , _friction(friction)
{
    ghoul_assert(_friction >= ScaleType(0.0), "Friction must be positive");
}

template <typename T, typename ScaleType>
void DelayedVariable<T, ScaleType>::set(T value, double dt) {
    _currentValue = _currentValue + (value - _currentValue) *
        glm::min(_scaleFactor * dt, 1.0); // less or equal to 1.0 keeps it stable
}

template <typename T, typename ScaleType>
void DelayedVariable<T, ScaleType>::decelerate(double dt) {
    set(T(0), _friction * dt);
}

template <typename T, typename ScaleType>
void DelayedVariable<T, ScaleType>::setHard(T value) {
    _currentValue = value;
}

template <typename T, typename ScaleType>
void DelayedVariable<T, ScaleType>::setFriction(ScaleType friction) {
    _friction = friction;
    ghoul_assert(_friction >= ScaleType(0.0), "Friction must be positive");
}

template <typename T, typename ScaleType>
void DelayedVariable<T, ScaleType>::setScaleFactor(ScaleType scaleFactor) {
    _scaleFactor = scaleFactor;
}

template <typename T, typename ScaleType>
T DelayedVariable<T, ScaleType>::get() const {
    return _currentValue;
}

} // namespace openspace::interaction
