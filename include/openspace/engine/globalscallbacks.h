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

#ifndef __OPENSPACE_CORE___GLOBALSCALLBACKS___H__
#define __OPENSPACE_CORE___GLOBALSCALLBACKS___H__

#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <openspace/util/touch.h>
#include <ghoul/misc/boolean.h>
#include <functional>
#include <vector>

namespace openspace {
    BooleanType(IsGuiWindow);
} // namespace openspace

namespace openspace::global::callback {

using KeyboardCallback = std::function<bool(Key, KeyModifier, KeyAction, IsGuiWindow)>;
using CharacterCallback = std::function<bool(unsigned int, KeyModifier, IsGuiWindow)>;
using MouseButtonCallback =
    std::function<bool(MouseButton, MouseAction, KeyModifier, IsGuiWindow)>;
using MousePositionCallback = std::function<void(double, double, IsGuiWindow)>;
using MouseScrollWheelCallback = std::function<bool(double, double, IsGuiWindow)>;

inline std::vector<std::function<void()>>* initialize;
inline std::vector<std::function<void()>>* deinitialize;
inline std::vector<std::function<void()>>* initializeGL;
inline std::vector<std::function<void()>>* deinitializeGL;
inline std::vector<std::function<void()>>* preSync;
inline std::vector<std::function<void()>>* postSyncPreDraw;
inline std::vector<std::function<void()>>* render;
inline std::vector<std::function<void()>>* draw2D;
inline std::vector<std::function<void()>>* postDraw;
inline std::vector<KeyboardCallback>* keyboard;
inline std::vector<CharacterCallback>* character;
inline std::vector<MouseButtonCallback>* mouseButton;
inline std::vector<MousePositionCallback>* mousePosition;
inline std::vector<MouseScrollWheelCallback>* mouseScrollWheel;
inline std::vector<std::function<bool(TouchInput)>>* touchDetected;
inline std::vector<std::function<bool(TouchInput)>>* touchUpdated;
inline std::vector<std::function<void(TouchInput)>>* touchExit;

/**
 * If the framerate becomes slow, Chromium Embedded Framework (used in Web Browser Module)
 * needs to perform its message loop work more frequently than once per frame. If this
 * method is not called frequently enough, the GUI will become much less responsive. A
 * future more long-term may decouple the browser's message work loop from the main render
 * loop altogehter using a separate thread. Currently, this method is called from within
 * the RenderEngine, between calls to individual renderables.
 */
extern void (*webBrowserPerformanceHotfix)();

void create();
void destroy();

} // namespace openspace::global::callback

#endif // __OPENSPACE_CORE___GLOBALSCALLBACKS___H__
