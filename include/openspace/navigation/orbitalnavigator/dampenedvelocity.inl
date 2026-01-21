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

#include <ghoul/misc/assert.h>
#include <ghoul/glm.h>

namespace openspace::interaction {

template <typename T>
DampenedVelocity<T>::DampenedVelocity(double scaleFactor, bool useFriction)
    : _scaleFactor(scaleFactor)
    , _frictionEnabled(useFriction)
{}

template <typename T>
void DampenedVelocity<T>::set(T value, double dt) {
    _targetValue = value;
    _currentValue += (_targetValue - _currentValue) * glm::min(_scaleFactor * dt, 1.0);
    // less or equal to 1.0 keeps it stable
}

template <typename T>
void DampenedVelocity<T>::decelerate(double dt) {
    if (!_frictionEnabled) {
        return;
    }
    _currentValue *= (1.0 - glm::min(_scaleFactor * dt, 1.0));
    // less or equal to 1.0 keeps it stable
}

template <typename T>
void DampenedVelocity<T>::setImmediate(T value) {
    _targetValue = value;
    _currentValue = value;
}

template <typename T>
void DampenedVelocity<T>::setFriction(bool enabled) {
    _frictionEnabled = enabled;
}

template <typename T>
void DampenedVelocity<T>::setScaleFactor(double scaleFactor) {
    _scaleFactor = scaleFactor;
}

template <typename T>
T DampenedVelocity<T>::get() const {
    return _currentValue;
}

template <typename T>
void DampenedVelocity<T>::update(std::optional<T> value, double dt) {
    if (value.has_value()) {
        set(*value, dt);
    }
    else {
        decelerate(dt);
    }
}

} // namespace openspace::interaction
