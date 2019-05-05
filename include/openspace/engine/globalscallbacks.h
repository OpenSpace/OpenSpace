/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_CORE___GLOBALSCALLBACKS___H__
#define __OPENSPACE_CORE___GLOBALSCALLBACKS___H__

#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <functional>
#include <vector>

namespace openspace::global {

namespace detail {

std::vector<std::function<void()>>& gInitialize();
std::vector<std::function<void()>>& gDeinitialize();

std::vector<std::function<void()>>& gInitializeGL();
std::vector<std::function<void()>>& gDeinitializeGL();

std::vector<std::function<void()>>& gPreSync();
std::vector<std::function<void()>>& gPostSyncPreDraw();
std::vector<std::function<void()>>& gRender();
std::vector<std::function<void()>>& gDraw2D();
std::vector<std::function<void()>>& gPostDraw();

std::vector<std::function<bool(Key, KeyModifier, KeyAction)>>& gKeyboard();
std::vector<std::function<bool(unsigned int, KeyModifier)>>& gCharacter();

std::vector<std::function<bool(MouseButton, MouseAction)>>& gMouseButton();
std::vector<std::function<void(double, double)>>& gMousePosition();
std::vector<std::function<bool(double, double)>>& gMouseScrollWheel();

} // namespace detail

namespace callback {

static std::vector<std::function<void()>>& initialize = detail::gInitialize();
static std::vector<std::function<void()>>& deinitialize = detail::gDeinitialize();
static std::vector<std::function<void()>>& initializeGL = detail::gInitializeGL();
static std::vector<std::function<void()>>& deinitializeGL = detail::gDeinitializeGL();
static std::vector<std::function<void()>>& preSync = detail::gPreSync();
static std::vector<std::function<void()>>& postSyncPreDraw = detail::gPostSyncPreDraw();
static std::vector<std::function<void()>>& render = detail::gRender();
static std::vector<std::function<void()>>& draw2D = detail::gDraw2D();
static std::vector<std::function<void()>>& postDraw = detail::gPostDraw();
static std::vector<std::function<bool(Key, KeyModifier, KeyAction)>>& keyboard =
    detail::gKeyboard();
static std::vector<std::function<bool(unsigned int, KeyModifier)>>& character =
    detail::gCharacter();
static std::vector<std::function<bool(MouseButton, MouseAction)>>& mouseButton =
    detail::gMouseButton();
static std::vector<std::function<void(double, double)>>& mousePosition =
    detail::gMousePosition();
static std::vector<std::function<bool(double, double)>>& mouseScrollWheel =
    detail::gMouseScrollWheel();

} // namespace callback

} // namespace openspace::global

#endif // __OPENSPACE_CORE___GLOBALSCALLBACKS___H__
