/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_CORE___STACKTRACE___H__
#define __OPENSPACE_CORE___STACKTRACE___H__

#include <string>
#ifdef WIN32
#include <stacktrace>
#endif // WIN32
#include <vector>

namespace openspace {

/**
 * Returns the stack trace at the calling site of the function. The vector that is
 * returned contains one line for each level of the stack trace. On Windows, the stack
 * trace is retrieved via the stacktrace standard library functions, whereas Unix and Mac
 * uses the `backtrace_symbols` function.
 *
 * \return A list of the full stack trace at the calling site
 */
#ifdef WIN32
std::vector<std::string> stackTrace(std::stacktrace trace = std::stacktrace::current());

/**
 * Returns the stack trace that is described by the passed \p context. The regular
 * #stackTrace function cannot be used for this as it can only capture the stack of its
 * own calling site. Inside an unhandled exception filter that is the filter itself
 * rather than the code that raised the exception, which is why the crash handler has to
 * unwind the context it is handed instead.
 *
 * \param context The thread context from which to start unwinding, which is usually the
 *        `ContextRecord` of the `EXCEPTION_POINTERS` that an unhandled exception filter
 *        receives. It is a `const CONTEXT*` that is passed as a `const void*` so that
 *        this header does not have to include `Windows.h`. If it is `nullptr`, an empty
 *        list is returned
 * 
 * \return A list of the full stack trace described by \p context
 */
std::vector<std::string> stackTraceFromContext(const void* context);
#else // ^^^^ WIN32 // !WIN32 vvvv
std::vector<std::string> stackTrace();
#endif // WIN32

} // namespace openspace

#endif // __OPENSPACE_CORE___STACKTRACE___H__
