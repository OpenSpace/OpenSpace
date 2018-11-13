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

#ifndef __OPENSPACE_CORE___DELAYEDVARIABLE___H__
#define __OPENSPACE_CORE___DELAYEDVARIABLE___H__

namespace openspace::interaction {

/**
 * Class that acts as a smoothing filter to a variable. The filter has a step
 * response on a form that resembles the function y = 1-e^(-t/scale). The variable
 * will be updated as soon as it is set to a value (calling the set() function).
 */
template <typename T, typename ScaleType>
class DelayedVariable {
public:
    DelayedVariable(ScaleType scaleFactor, ScaleType friction);
    void set(T value, double dt);
    void decelerate(double dt);
    void setHard(T value);
    void setFriction(ScaleType friction);
    void setScaleFactor(ScaleType scaleFactor);
    T get() const;

private:
    ScaleType _scaleFactor;
    ScaleType _friction;
    T _targetValue;
    T _currentValue;
};

} // namespace openspace::interaction

#include "delayedvariable.inl"

#endif // __OPENSPACE_CORE___DELAYEDVARIABLE___H__
