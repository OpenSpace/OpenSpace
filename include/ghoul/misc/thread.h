/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___THREAD___H__
#define __GHOUL___THREAD___H__

#include <ghoul/misc/boolean.h>
#include <thread>

namespace ghoul::thread {

/**
 * Determines the priority class for a specific threads. The available classes described
 * in this enum are most likely to be a subset of the supported classes for a specific
 * operating system. However, they represent a greatest common factor for Windows and
 * POSIX threads.
 */
enum class ThreadPriorityClass {
    Idle = 0,
    Normal,
    High
};

/**
 * Determines the priority level of a thread within a ThreadPriorityClass. The available
 * levels in this enum are most likely to be a subset of the supported classes for a
 * specific operating system. However, they represent a reasonable subset for Windows and
 * POSIX threads.
 */
enum class ThreadPriorityLevel {
    Lowest = 0,
    BelowNormal,
    Normal,
    AboveNormal,
    Highest
};

BooleanType(Background);

/**
 * This method sets the priorty of the thread \p t to the ThreadPriorityClass
 * \p priorityClass and the ThreadPriorityLevel to \p priorityLevel.
 *
 * \param t The thread for which to set the priority class and level
 * \param priorityClass The ThreadPriorityClass that is to be set for \p t
 * \param priorityLevel The ThreadPriorityLevel that is to be set for \p t
 *
 * \throw ghoul::RuntimeError If a non-recoverable error occurs while setting the thread
 *        class or level
 */
void setPriority(std::thread& t, ThreadPriorityClass priorityClass,
    ThreadPriorityLevel priorityLevel
);

/**
 * This method enables or disables the background threading for a specific thread \p t.
 * This function might not be supported on all platforms and reverts to a no-op on
 * platforms that are not supported. On platforms that are supported, a background state
 * will cause the schedule to reduce the resource allocation for the specific thread.
 *
 * \param t The thread for which to enable the background state
 * \param background BackgroundThread::Yes if the background state should be enabled or
 *        BackgroundThread::No if the state should be disabled
 */
void setThreadBackground(std::thread& t, Background background);

} // namespace ghoul::thread

#endif // __GHOUL___THREAD___H__
