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

#ifndef __OPENSPACE_MODULE_AUTONAVIGATION___SPEEDFUNCTION___H__
#define __OPENSPACE_MODULE_AUTONAVIGATION___SPEEDFUNCTION___H__

namespace openspace::autonavigation {

// The speed function describing the shape of the speed curve. Values in [0,1].
class SpeedFunction {
public:
    SpeedFunction() = default;
    virtual ~SpeedFunction();

    double scaledValue(double time, double duration, double pathLength) const;

    virtual double value(double t) const = 0;

protected:
    // must be called by each subclass after initialization
    void initIntegratedSum();

    // store the sum of the function over the duration of the segment, 
    // so we don't need to recompue it every time we access the speed 
    double _integratedSum = 0.0;
};

class CubicDampenedSpeed : public SpeedFunction {
public:
    CubicDampenedSpeed();
    double value(double t) const override;
}; 

} // namespace openspace::autonavigation

#endif // __OPENSPACE_MODULE_AUTONAVIGATION___SPEEDFUNCTION___H__
