/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/engine/globalscallbacks.h>

namespace openspace::global::callback {

void create() {
    initialize = new std::vector<std::function<void()>>;
    deinitialize = new std::vector<std::function<void()>>;
    initializeGL = new std::vector<std::function<void()>>;
    deinitializeGL = new std::vector<std::function<void()>>;
    preSync = new std::vector<std::function<void()>> ;
    postSyncPreDraw = new std::vector<std::function<void()>> ;
    render = new std::vector<std::function<void()>>;
    draw2D = new std::vector<std::function<void()>>;
    postDraw = new std::vector<std::function<void()>>; 
    keyboard = new std::vector<std::function<bool(Key, KeyModifier, KeyAction)>>;
    character = new std::vector<std::function<bool(unsigned int, KeyModifier)>>;
    mouseButton =
        new std::vector<std::function<bool(MouseButton, MouseAction, KeyModifier)>>;
    mousePosition = new std::vector<std::function<void(double, double)>>;
    mouseScrollWheel = new std::vector<std::function<bool(double, double)>>;
    touchDetected = new std::vector<std::function<bool(TouchInput)>>;
    touchUpdated = new std::vector<std::function<bool(TouchInput)>>;
    touchExit = new std::vector<std::function<void(TouchInput)>>;
}

void destroy() {
    delete touchExit;
    delete touchUpdated;
    delete touchDetected;
    delete mouseScrollWheel;
    delete mousePosition;
    delete mouseButton;
    delete character;
    delete keyboard;
    delete postDraw;
    delete draw2D;
    delete render;
    delete postSyncPreDraw;
    delete preSync;
    delete deinitializeGL;
    delete initializeGL;
    delete deinitialize;
    delete initialize;
}

void(*webBrowserPerformanceHotfix)() = nullptr;

} // namespace openspace::global::callback
