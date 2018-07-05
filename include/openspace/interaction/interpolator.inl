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

#include <ghoul/misc/assert.h>

namespace openspace::interaction {

template <typename T>
Interpolator<T>::Interpolator()
    : _transferFunction([](float t){ return t; })
{}

template <typename T>
void Interpolator<T>::start() {
    _t = 0.f;
}

template <typename T>
void Interpolator<T>::end() {
    _t = 1.f;
}

template <typename T>
void Interpolator<T>::setDeltaTime(float deltaTime) {
    _scaledDeltaTime = deltaTime / _interpolationTime;
}

template <typename T>
void Interpolator<T>::setTransferFunction(std::function<T(float)> transferFunction) {
    _transferFunction = std::move(transferFunction);
}

template <typename T>
void Interpolator<T>::setInterpolationTime(float interpolationTime) {
    _interpolationTime = interpolationTime;
}

template <typename T>
void Interpolator<T>::step() {
    _t += _scaledDeltaTime;
    _t = glm::clamp(_t, 0.0f, 1.0f);
}

template <typename T>
float Interpolator<T>::deltaTimeScaled() const {
    return _scaledDeltaTime;
}

template <typename T>
T Interpolator<T>::value() const {
    return _transferFunction(_t);
}

template <typename T>
bool Interpolator<T>::isInterpolating() const {
    return (_t < 1.f) && (_t >= 0.f);
}

} // namespace openspace::interaction
