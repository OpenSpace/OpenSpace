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

#include "windowcolors.h"

#include <ghoul/misc/assert.h>

QColor colorForWindow(int idx) {
    constexpr std::array<QColor, 4> Hardcoded = {
        QColor(43, 158, 195),
        QColor(252, 171, 16),
        QColor(68, 175, 105),
        QColor(248, 51, 60)
    };

    ghoul_assert(idx >= 0, "idx must be non-negative");

    if (idx < 4) {
        return Hardcoded[idx];
    }
    else {
        // If someone asks for a color of an index greater than what we have, we generate
        // them by multiplying some prime numbers together
        const int r = (idx * 31 * 3083) % 256;
        const int g = (idx * 43 * 4273) % 256;
        const int b = (idx * 59 * 5857) % 256;
        return QColor(r, g, b);
    }
}
