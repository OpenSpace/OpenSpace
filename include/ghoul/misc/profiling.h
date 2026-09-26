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

#ifndef __GHOUL___PROFILING___H__
#define __GHOUL___PROFILING___H__

#ifdef TRACY_ENABLE

#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/opengl/ghoul_gl.h>

#include <tracy/Tracy.hpp>
#include <tracy/TracyOpenGL.hpp>

#ifdef WIN32
#pragma warning(push)
#pragma warning(disable: 4100)
#endif // WIN32

#include <tracy/TracyLua.hpp>

#ifdef WIN32
#pragma warning(pop)
#endif // WIN32

#else // ^^^^ TRACY_ENABLE // !TRACY_ENABLE vvvv

// Just a dump of defines to prevent needing to include the header files even if we don't
// use Tracy
#define FrameMarkStart(dummy)
#define FrameMarkEnd(dummy)
#define TracyGpuZone(dummy)
#define ZoneName(dummy, dummy2)
#define ZoneScoped
#define ZoneScopedN(dummy)
#define ZoneText(text, length)
#define TracyLockable(type, var) type var
#define TracyAlloc(ptr, bytes)
#define TracyAllocN(ptr, bytes, name)

#endif // TRACY_ENABLE

#endif // __GHOUL___PROFILING___H__
