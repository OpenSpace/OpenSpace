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

#ifndef __OPENSPACE_CORE___UNIVERSALHELPERS___H__
#define __OPENSPACE_CORE___UNIVERSALHELPERS___H__

//
// This file is meant to contain generally useful functions that is used a bit all over
// the place but that do not belong in any other, more specific, file.
//
// If you implement a function that can be useful in several other situations, feel free
// to document it and put it this file
//

namespace openspace::helpers {

/**
 * Remap a parameter t in [0,1] to a subinterval [start, end], by shifting and scaling
 * the value to match the interval. If \p t is smaller than \p start, the return value
 * will be 0 and if bigger than \p end it will be 1. Other values will be linearly
 * interpolated within the range [start, end].
 */
double shiftAndScale(double t, double start, double end);

} // namespace openspace::helpers

#endif // __OPENSPACE_CORE___UNIVERSALHELPERS___H__
