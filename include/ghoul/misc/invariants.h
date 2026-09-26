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

#ifndef __GHOUL___INVARIANTS___H__
#define __GHOUL___INVARIANTS___H__

#if !(defined(NDEBUG) || defined(DEBUG))

#include <ghoul/misc/assert.h>
#include <ghoul/misc/defer.h>

#define ghoul_precondition(__condition__, __message__) \
    ghoul_assert(__condition__, "Precondition failed: " __message__)

#define ghoul_postcondition(__condition__, __message__) \
    defer { ghoul_assert(__condition__, "Postcondition failed: " __message__); };

#else // ^^^^ DEBUG // !DEBUG vvvv

#define ghoul_precondition(__condition__, __message__)
#define ghoul_postcondition(__condition__, __message__)

#endif // DEBUG

#endif // __GHOUL___INVARIANTS___H__
