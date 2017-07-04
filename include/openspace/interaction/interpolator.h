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

#ifndef __OPENSPACE_CORE___INTERPOLATOR___H__
#define __OPENSPACE_CORE___INTERPOLATOR___H__

#include <ghoul/misc/assert.h>

#include <functional>

namespace openspace {
namespace interaction {

/*
 * Interpolates a typename T using a transfer function.
 */
template <typename T>
class Interpolator
{
public:
    Interpolator()
    : _transferFunction([](double t){ return t; })
    , _t(0.0)
    , _interpolationTime(1.0) {};
    ~Interpolator() {};

    void start(double interpolationTime = 0.0) {
        ghoul_assert(interpolationTime >= 0.0, "interpolationTime should not be negative!");
        if (interpolationTime) {
            _interpolationTime = interpolationTime;
        }
        _t = 0.0;
    };

    void end() { _t = 1.0; };
    
    void step(double deltaTime) { _t += deltaTime / _interpolationTime; };

    T value() { return _transferFunction(_t); };
    bool isInterpolating() { return _t < 1.0; };

    void setTransferFunction(std::function<T(double)> transferFunction) {
        _transferFunction = transferFunction;
    }
private:
    std::function<T(double)> _transferFunction;
    double _t;
    double _interpolationTime;
};

} // namespace interaction
} // namespace openspace

#endif // __OPENSPACE_CORE___INTERPOLATOR___H__
