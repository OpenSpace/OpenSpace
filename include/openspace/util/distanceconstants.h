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

#ifndef __OPENSPACE_CORE___DISTANCECONSTANTS___H__
#define __OPENSPACE_CORE___DISTANCECONSTANTS___H__

namespace openspace::distanceconstants {
    constexpr double EarthRadius = 6371E3;
    constexpr double JupiterRadius = 7.1492E7;
    constexpr double SolarRadius = 6.95700E8;
    constexpr double LightYear = 9.4607304725808E15;
    constexpr double LightMonth = LightYear / 12;
    constexpr double LightDay = LightYear / 365;
    constexpr double LightHour = LightDay / 24;
    constexpr double LightSecond = 299792458.0;
    constexpr double AstronomicalUnit = 1.495978707E11;
    constexpr double Parsec = 3.0856776E16;

    constexpr double Inch = 0.0254;
    constexpr double Foot = 0.3048;
    constexpr double Yard = 0.9144;
    constexpr double Chain = 20.1168;
    constexpr double Mile = 1609.344;
    constexpr double NauticalMile = 1852.0;
} // openspace::distanceconstants

#endif // __OPENSPACE_CORE___DISTANCECONSTANTS___H__
