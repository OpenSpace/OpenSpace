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

#ifndef __GHOUL___SUPPORTMACROS___H__
#define __GHOUL___SUPPORTMACROS___H__

// The DISABLE_OPTIMIZATION macro can be used to locally disable any optimization that
// might occur. This is potentially useful when working in Visual Studio in RelWithDebInfo
// configuration, but optimizations make debugging a problem difficult.
// OBS: Each DISABLE_OPTIMIZATION must have a RESTORE_OPTIMIZATION or the results are
// undefined

#if defined(_MSC_VER) && !defined(__INTEL_COMPILER)

// We are running Microsoft Visual Studio
#define DISABLE_OPTIMIZATION #pragma optimize("", off)
#define RESTORE_OPTIMIZATION #pragma optimize("", on)
#elif defined __INTEL_COMPILER
// We are running the Intel compiler (which also defines _MSC_VER)
#define DISABLE_OPTIMIZATION #warning("OPTIMIZATION macros not implemented")
#define RESTORE_OPTIMIZATION #warning("OPTIMIZATION macros not implemented")

#elif defined __clang__
// We are running Clang
#define DISABLE_OPTIMIZATION #pragma optimize off
#define RESTORE_OPTIMIZATION #pragma optimize on

#elif defined __GNUC__
// We are running GCC.  Requires GCC support 4.4 or higher
#define DISABLE_OPTIMIZATION                                                             \
    #pragma GCC push_options                                                             \
    #pragma GCC optimize("O0")
#define RESTORE_OPTIMIZATION #pragma GCC pop_options
#endif // DISABLE_OPTIMIZATION

#endif // __GHOUL___SUPPORTMACROS___H__
