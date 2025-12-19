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

#ifndef __OPENSPACE_CORE___DAMPENEDVELOCITY___H__
#define __OPENSPACE_CORE___DAMPENEDVELOCITY___H__

namespace openspace::interaction {

/**
 * This class acts as a smoothing filter to a variable representing a velocity.
 * If friction is enabled, a decelleration can also be applied over time.
 *
 * The filter has a step response on a form that resembles the function
 * y = 1-e^(-t/scale). The variable will be updated as soon as it is set to a
 * value (calling the set() function).
 */
template <typename T>
class DampenedVelocity {
public:
    DampenedVelocity(double scaleFactor, bool useFriction = true);

    void set(T value, double dt);
    void decelerate(double dt);
    void setImmediate(T value);
    void setFriction(bool enabled);
    void setScaleFactor(double scaleFactor);
    T get() const;

    /**
     * Update the velocity based on a new input value. If a value was provided, set it.
     * Otherwise, decellerate based on the given timestep.
     *
     * \param value The new value
     * \param dt The timestep
     */
    void update(std::optional<T> value, double dt);

private:
    double _scaleFactor;
    bool _frictionEnabled;
    T _targetValue = T(0);
    T _currentValue = T(0);
};

} // namespace openspace::interaction

#include "dampenedvelocity.inl"

#endif // __OPENSPACE_CORE___DAMPENEDVELOCITY___H__
