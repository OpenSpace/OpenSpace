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

#include <modules/autonavigation/transferfunctions.h>

#include <ghoul/glm.h>

namespace openspace::autonavigation::transferfunctions {

double linear(double t) { return t; };

double step(double t) { return (t > 0.5); };

double circularEaseOut(double p){
    return sqrt((2 - p) * p);
}

double cubicEaseIn(double t) { return (t*t*t); };

double cubicEaseOut(double t) {
    double p = 1 - t;
    return (p*p*p);
};

double cubicEaseInOut(double t) {
    if (t < 0.5) {
        return 4 * t * t * t;
    }
    else {
        double f = ((2 * t) - 2);
        return 0.5 * f * f * f + 1;
    }
};

double quadraticEaseInOut(double t) {
    if (t < 0.5) {
        return 2 * t * t;
    }
    else {
        return (-2 * t * t) + (4 * t) - 1;
    }
}

double exponentialEaseInOut(double t) {
    if (t == 0.0 || t == 1.0) return t;

    if (t < 0.5) {
        return 0.5 * glm::pow(2, (20 * t) - 10);
    }
    else {
        return -0.5 * glm::pow(2, (-20 * t) + 10) + 1;
    }
};

} // namespace
