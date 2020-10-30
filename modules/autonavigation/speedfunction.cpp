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

#include <modules/autonavigation/speedfunction.h>

#include <modules/autonavigation/helperfunctions.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/easing.h>
#include <algorithm>

namespace {
    constexpr const char* _loggerCat = "SpeedFunction";
} // namespace

namespace openspace::autonavigation {

SpeedFunction::~SpeedFunction() {}

/*
* Get speed at time value in the range [0, duration], scaled according to the constraint
* in eq. 14 in Eberly 2007
* (https://www.geometrictools.com/Documentation/MovingAlongCurveSpecifiedSpeed.pdf)
* OBS! If integrated over the duration for the path it shall match the total length.
*/
double SpeedFunction::scaledValue(double time, double duration, double pathLength) const {
    ghoul_assert(time >= 0 && time <= duration, "Time out of range [0, duration]");
    double t = std::clamp(time / duration, 0.0, 1.0);
    return (pathLength * this->value(t)) / (duration * _integratedSum);
}

void SpeedFunction::initIntegratedSum() {
    const int steps = 100;
    _integratedSum = helpers::simpsonsRule(
        0.0,
        1.0,
        steps,
        [this](double t) { return value(t); }
    );
}

SexticDampenedSpeed::SexticDampenedSpeed() {
    initIntegratedSum();
}

double SexticDampenedSpeed::value(double t) const {
    ghoul_assert(t >= 0.0 && t <= 1.0, "Variable t out of range [0,1]");

    const double tPeak = 0.5;
    double speed = 0.0;

    // accelerate
    if (t <= tPeak) {
        double tScaled = t / tPeak;
        speed = ghoul::cubicEaseInOut(ghoul::cubicEaseInOut(tScaled));
    }
    // deaccelerate
    else if (t <= 1.0) {
        double tScaled = (t - tPeak) / (1.0 - tPeak);
        speed = 1.0 - ghoul::cubicEaseInOut(ghoul::cubicEaseInOut(tScaled));
    }

    // avoid zero speed
    speed += 0.00001; // TODO: Minimal speed should depend on size of visible object/node
    return speed;
}

QuinticDampenedSpeed::QuinticDampenedSpeed() {
    initIntegratedSum();
}

double QuinticDampenedSpeed::value(double t) const {
    ghoul_assert(t >= 0.0 && t <= 1.0, "Variable t out of range [0,1]");

    const double tPeak = 0.5;
    double speed = 0.0;

    // accelerate
    if (t <= tPeak) {
        double tScaled = t / tPeak;
        speed = ghoul::cubicEaseInOut(ghoul::quadraticEaseInOut(tScaled));
    }
    // deaccelerate
    else if (t <= 1.0) {
        double tScaled = (t - tPeak) / (1.0 - tPeak);
        speed = 1.0 - ghoul::cubicEaseInOut(ghoul::quadraticEaseInOut(tScaled));
    }

    // avoid zero speed
    speed += 0.00001; // TODO: Minimal speed should depend on size of visible object/node
    return speed;
}

} // namespace openspace::autonavigation
