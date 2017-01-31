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

#ifndef __OPENSPACE_CORE___PERFORMANCEMEASUREMENT___H__
#define __OPENSPACE_CORE___PERFORMANCEMEASUREMENT___H__

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

#include <chrono>
#include <string>

namespace openspace {
namespace performance {

class PerformanceManager;

class PerformanceMeasurement {
public:
    PerformanceMeasurement(std::string identifier, performance::PerformanceManager* manager);
    ~PerformanceMeasurement();

private:
    std::string _identifier;
    performance::PerformanceManager* _manager;

    std::chrono::high_resolution_clock::time_point _startTime;
};
    
#define __MERGE(a,b)  a##b
#define __LABEL(a) __MERGE(unique_name_, a)

/// Declare a new variable for measuring the performance of the current block
#define PerfMeasure(name) \
    auto __LABEL(__LINE__) = \
        openspace::performance::PerformanceMeasurement(\
            (name), \
            OsEng.renderEngine().performanceManager() \
        )
    
} // namespace performance
} // namespace openspace

#endif // __OPENSPACE_CORE___PERFORMANCEMEASUREMENT___H__
