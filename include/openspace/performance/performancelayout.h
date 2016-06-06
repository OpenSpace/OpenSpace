/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#ifndef __PERFORMANCELAYOUT_H__
#define __PERFORMANCELAYOUT_H__

#include <cstdint>

namespace openspace {
namespace performance {

struct PerformanceLayout {
    static const int8_t Version = 0;
    static const int LengthName = 256;
    static const int NumberValues = 256;
    static const int MaxValues = 256;

    PerformanceLayout();

    struct SceneGraphPerformanceLayout {
        char name[LengthName];
        float renderTime[NumberValues];
        float updateRenderable[NumberValues];
        float updateEphemeris[NumberValues];

        int32_t currentRenderTime;
        int32_t currentUpdateRenderable;
        int32_t currentUpdateEphemeris;
    };
    SceneGraphPerformanceLayout sceneGraphEntries[MaxValues];
    int16_t nScaleGraphEntries;

    struct FunctionPerformanceLayout {
        char name[LengthName];
        float time[NumberValues];
        int32_t currentTime;
    };
    FunctionPerformanceLayout functionEntries[MaxValues];
    int16_t nFunctionEntries;
};

} // namespace performance
} // namespace openspace

#endif // __PERFORMANCELAYOUT_H__
