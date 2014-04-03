/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2014                                                               *
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

#ifdef GHL_TIMING_TESTS

#ifdef WIN32
#define START_TIMER(__name__, __stream__, __num__) \
    __stream__ << #__name__; \
    double __name__##Total = 0.0; \
    unsigned int __name__##Num = 0; \
    for (__name__##Num = 0; __name__##Num < __num__; ++__name__##Num) { \
        reset(); \
        LARGE_INTEGER __name__##Start; \
        LARGE_INTEGER __name__##End; \
        QueryPerformanceCounter(&__name__##Start)

#define START_TIMER_NO_RESET(__name__, __stream__, __num__) \
    __stream__ << #__name__; \
    double __name__##Total = 0.0; \
    unsigned int __name__##Num = 0; \
    for (__name__##Num = 0; __name__##Num < __num__; ++__name__##Num) { \
        LARGE_INTEGER __name__##Start; \
        LARGE_INTEGER __name__##End; \
        QueryPerformanceCounter(&__name__##Start)

#define START_TIMER_PREPARE(__name__, __stream__, __num__, __prepare__) \
    __stream__ << #__name__; \
    double __name__##Total = 0.0; \
    unsigned int __name__##Num = 0; \
    for (__name__##Num = 0; __name__##Num < __num__; ++__name__##Num) { \
        reset(); \
        __prepare__; \
        LARGE_INTEGER __name__##Start; \
        LARGE_INTEGER __name__##End; \
        QueryPerformanceCounter(&__name__##Start)

#define FINISH_TIMER(__name__, __stream__) \
        QueryPerformanceCounter(&__name__##End); \
        LARGE_INTEGER freq; \
        QueryPerformanceFrequency(&freq); \
        const double freqD = double(freq.QuadPart) / 1000000.0; \
        const double res = ((__name__##End.QuadPart - __name__##Start.QuadPart) / freqD);\
        __name__##Total += res; \
    } \
    __stream__ << '\t' << __name__##Total / __name__##Num << "us\n";

#else

#include <chrono>

#define START_TIMER(__name__, __stream__, __num__) \
    __stream__ << #__name__; \
    std::chrono::nanoseconds __name__##Total = std::chrono::nanoseconds(0); \
    unsigned int __name__##Num = 0; \
    for (__name__##Num = 0; __name__##Num < __num__; ++__name__##Num) { \
        reset(); \
        std::chrono::high_resolution_clock::time_point __name__##End; \
        std::chrono::high_resolution_clock::time_point __name__##Start = \
        std::chrono::high_resolution_clock::now()

#define START_TIMER_NO_RESET(__name__, __stream__, __num__) \
    __stream__ << #__name__; \
    std::chrono::nanoseconds __name__##Total = std::chrono::nanoseconds(0); \
    unsigned int __name__##Num = 0; \
    for (__name__##Num = 0; __name__##Num < __num__; ++__name__##Num) { \
        std::chrono::high_resolution_clock::time_point __name__##End; \
        std::chrono::high_resolution_clock::time_point __name__##Start = \
        std::chrono::high_resolution_clock::now()

#define START_TIMER_PREPARE(__name__, __stream__, __num__, __prepare__) \
    __stream__ << #__name__; \
    std::chrono::nanoseconds __name__##Total = std::chrono::nanoseconds(0); \
    unsigned int __name__##Num = 0; \
    for (__name__##Num = 0; __name__##Num < __num__; ++__name__##Num) { \
        reset(); \
        __prepare__; \
        std::chrono::high_resolution_clock::time_point __name__##End; \
        std::chrono::high_resolution_clock::time_point __name__##Start = \
        std::chrono::high_resolution_clock::now()

#define FINISH_TIMER(__name__, __stream__) \
        __name__##End = std::chrono::high_resolution_clock::now(); \
        const std::chrono::nanoseconds d = __name__##End - __name__##Start; \
        __name__##Total += d; \
    } \
    __stream__ << #__name__ << '\t' << __name__##Total.count() / 1000.0 <<  "us" << '\n';
#endif

#endif // GHL_TIMING_TESTS
