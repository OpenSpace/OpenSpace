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

#include <openspace/engine/globalscallbacks.h>

#include <ghoul/misc/assert.h>
#include <ghoul/misc/profiling.h>
#include <array>

namespace openspace::global::callback {

namespace {
    // Using the same mechanism as in the globals file
#ifdef WIN32
    constexpr int TotalSize =
        sizeof(std::vector<std::function<void()>>) +
        sizeof(std::vector<std::function<void()>>) +
        sizeof(std::vector<std::function<void()>>) +
        sizeof(std::vector<std::function<void()>>) +
        sizeof(std::vector<std::function<void()>>) +
        sizeof(std::vector<std::function<void()>>) +
        sizeof(std::vector<std::function<void()>>) +
        sizeof(std::vector<std::function<void()>>) +
        sizeof(std::vector<std::function<void()>>) +
        sizeof(std::vector<KeyboardCallback>) +
        sizeof(std::vector<CharacterCallback>) +
        sizeof(std::vector<MouseButtonCallback>) +
        sizeof(std::vector<MousePositionCallback>) +
        sizeof(std::vector<MouseScrollWheelCallback>) +
        sizeof(std::vector<std::function<bool(TouchInput)>>) +
        sizeof(std::vector<std::function<bool(TouchInput)>>) +
        sizeof(std::vector<std::function<void(TouchInput)>>);

    std::array<std::byte, TotalSize> DataStorage;
#endif // WIN32

} // namespace

void create() {
    ZoneScoped;

#ifdef WIN32
    std::fill(DataStorage.begin(), DataStorage.end(), std::byte(0));
    std::byte* currentPos = DataStorage.data();
#endif // WIN32

#ifdef WIN32
    initialize = new (currentPos) std::vector<std::function<void()>>();
    ghoul_assert(initialize, "No initialize");
    currentPos += sizeof(std::vector<std::function<void()>>);
#else // ^^^ WIN32 / !WIN32 vvv
    initialize = new std::vector<std::function<void()>>();
#endif // WIN32

#ifdef WIN32
    deinitialize = new (currentPos) std::vector<std::function<void()>>();
    ghoul_assert(deinitialize, "No deinitialize");
    currentPos += sizeof(std::vector<std::function<void()>>);
#else // ^^^ WIN32 / !WIN32 vvv
    deinitialize = new std::vector<std::function<void()>>();
#endif // WIN32

#ifdef WIN32
    initializeGL = new (currentPos) std::vector<std::function<void()>>();
    ghoul_assert(initializeGL, "No initializeGL");
    currentPos += sizeof(std::vector<std::function<void()>>);
#else // ^^^ WIN32 / !WIN32 vvv
    initializeGL = new std::vector<std::function<void()>>();
#endif // WIN32

#ifdef WIN32
    deinitializeGL = new (currentPos) std::vector<std::function<void()>>();
    ghoul_assert(deinitializeGL, "No deinitializeGL");
    currentPos += sizeof(std::vector<std::function<void()>>);
#else // ^^^ WIN32 / !WIN32 vvv
    deinitializeGL = new std::vector<std::function<void()>>();
#endif // WIN32

#ifdef WIN32
    preSync = new (currentPos) std::vector<std::function<void()>>();
    ghoul_assert(preSync, "No preSync");
    currentPos += sizeof(std::vector<std::function<void()>>);
#else // ^^^ WIN32 / !WIN32 vvv
    preSync = new std::vector<std::function<void()>>();
#endif // WIN32

#ifdef WIN32
    postSyncPreDraw = new (currentPos) std::vector<std::function<void()>>();
    ghoul_assert(postSyncPreDraw, "No postSyncPreDraw");
    currentPos += sizeof(std::vector<std::function<void()>>);
#else // ^^^ WIN32 / !WIN32 vvv
    postSyncPreDraw = new std::vector<std::function<void()>>();
#endif // WIN32

#ifdef WIN32
    render = new (currentPos) std::vector<std::function<void()>>();
    ghoul_assert(render, "No render");
    currentPos += sizeof(std::vector<std::function<void()>>);
#else // ^^^ WIN32 / !WIN32 vvv
    render = new std::vector<std::function<void()>>();
#endif // WIN32

#ifdef WIN32
    draw2D = new (currentPos) std::vector<std::function<void()>>();
    ghoul_assert(draw2D, "No draw2D");
    currentPos += sizeof(std::vector<std::function<void()>>);
#else // ^^^ WIN32 / !WIN32 vvv
    draw2D = new std::vector<std::function<void()>>();
#endif // WIN32

#ifdef WIN32
    postDraw = new (currentPos) std::vector<std::function<void()>>();
    ghoul_assert(postDraw, "No postDraw");
    currentPos += sizeof(std::vector<std::function<void()>>);
#else // ^^^ WIN32 / !WIN32 vvv
    postDraw = new std::vector<std::function<void()>>();
#endif // WIN32

#ifdef WIN32
    keyboard = new (currentPos) std::vector<KeyboardCallback>();
    ghoul_assert(keyboard, "No keyboard");
    currentPos += sizeof(std::vector<KeyboardCallback>);
#else // ^^^ WIN32 / !WIN32 vvv
    keyboard = new std::vector<KeyboardCallback>();
#endif // WIN32

#ifdef WIN32
    character = new (currentPos) std::vector<CharacterCallback>();
    ghoul_assert(character, "No character");
    currentPos += sizeof(std::vector<CharacterCallback>);
#else // ^^^ WIN32 / !WIN32 vvv
    character = new std::vector<CharacterCallback>();
#endif // WIN32

#ifdef WIN32
    mouseButton = new (currentPos) std::vector<MouseButtonCallback>();
    ghoul_assert(mouseButton, "No mouseButton");
    currentPos += sizeof(std::vector<MouseButtonCallback>);
#else // ^^^ WIN32 / !WIN32 vvv
    mouseButton = new std::vector<MouseButtonCallback>();
#endif // WIN32

#ifdef WIN32
    mousePosition =
        new (currentPos) std::vector<MousePositionCallback>();
    ghoul_assert(mousePosition, "No mousePosition");
    currentPos += sizeof(std::vector<MousePositionCallback>);
#else // ^^^ WIN32 / !WIN32 vvv
    mousePosition = new std::vector<MousePositionCallback>();
#endif // WIN32

#ifdef WIN32
    mouseScrollWheel = new (currentPos) std::vector<MouseScrollWheelCallback>();
    ghoul_assert(mouseScrollWheel, "No mouseScrollWheel");
    currentPos += sizeof(std::vector<MouseScrollWheelCallback>);
#else // ^^^ WIN32 / !WIN32 vvv
    mouseScrollWheel = new std::vector<MouseScrollWheelCallback>();
#endif // WIN32

#ifdef WIN32
    touchDetected = new (currentPos) std::vector<std::function<bool(TouchInput)>>();
    ghoul_assert(touchDetected, "No touchDetected");
    currentPos += sizeof(std::vector<std::function<bool(TouchInput)>>);
#else // ^^^ WIN32 / !WIN32 vvv
    touchDetected = new std::vector<std::function<bool(TouchInput)>>();
#endif // WIN32

#ifdef WIN32
    touchUpdated = new (currentPos) std::vector<std::function<bool(TouchInput)>>();
    ghoul_assert(touchUpdated, "No touchUpdated");
    currentPos += sizeof(std::vector<std::function<bool(TouchInput)>>);
#else // ^^^ WIN32 / !WIN32 vvv
    touchUpdated = new std::vector<std::function<bool(TouchInput)>>();
#endif // WIN32

#ifdef WIN32
    touchExit = new (currentPos) std::vector<std::function<void(TouchInput)>>();
    ghoul_assert(touchExit, "No touchExit");
    //currentPos += sizeof(std::vector<std::function<void(TouchInput)>>);
#else // ^^^ WIN32 / !WIN32 vvv
    touchExit = new std::vector<std::function<void(TouchInput)>>();
#endif // WIN32
}

void destroy() {
#ifdef WIN32
    touchExit->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete touchExit;
#endif // WIN32

#ifdef WIN32
    touchUpdated->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete touchUpdated;
#endif // WIN32

#ifdef WIN32
    touchDetected->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete touchDetected;
#endif // WIN32

#ifdef WIN32
    mouseScrollWheel->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete mouseScrollWheel;
#endif // WIN32

#ifdef WIN32
    mousePosition->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete mousePosition;
#endif // WIN32

#ifdef WIN32
    mouseButton->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete mouseButton;
#endif // WIN32

#ifdef WIN32
    character->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete character;
#endif // WIN32

#ifdef WIN32
    keyboard->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete keyboard;
#endif // WIN32

#ifdef WIN32
    postDraw->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete postDraw;
#endif // WIN32

#ifdef WIN32
    draw2D->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete draw2D;
#endif // WIN32

#ifdef WIN32
    render->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete render;
#endif // WIN32

#ifdef WIN32
    postSyncPreDraw->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete postSyncPreDraw;
#endif // WIN32

#ifdef WIN32
    preSync->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete preSync;
#endif // WIN32

#ifdef WIN32
    deinitializeGL->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete deinitializeGL;
#endif // WIN32

#ifdef WIN32
    initializeGL->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete initializeGL;
#endif // WIN32

#ifdef WIN32
    deinitialize->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete deinitialize;
#endif // WIN32

#ifdef WIN32
    initialize->~vector();
#else // ^^^ WIN32 / !WIN32 vvv
    delete initialize;
#endif // WIN32
}

void(*webBrowserPerformanceHotfix)() = nullptr;

} // namespace openspace::global::callback
