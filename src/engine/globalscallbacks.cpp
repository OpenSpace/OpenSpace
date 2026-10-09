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

#include <openspace/engine/globalscallbacks.h>

#include <openspace/misc/assert.h>
#include <openspace/misc/profiling.h>
#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <new>
#include <utility>

namespace openspace::global::callback {

namespace {
    // Using the same mechanism as in the globals file
#ifdef WIN32
    // The number of objects that are placed into the DataStorage below
    constexpr int NumberOfCallbacks = 17;

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
        sizeof(std::vector<std::function<void(TouchInput)>>) +
        // Every object has to start at an address that satisfies its own alignment
        // requirement, so there might be some padding in front of each of them. Reserve
        // enough slack for a maximally sized padding per object
        NumberOfCallbacks * alignof(std::max_align_t);

    alignas(std::max_align_t) std::array<std::byte, TotalSize> DataStorage;
#endif // WIN32

/**
 * Creates the callback list of type \p T. Works the same way as the `createGlobal`
 * function in the globals file, see the documentation there for why the alignment
 * rounding is necessary.
 */
template <typename T, typename... Args>
T* createCallback([[maybe_unused]] std::byte*& pos, Args&&... args) {
#ifdef WIN32
    constexpr std::uintptr_t Alignment = alignof(T);
    const std::uintptr_t p = reinterpret_cast<std::uintptr_t>(pos);
    pos = reinterpret_cast<std::byte*>((p + Alignment - 1) & ~(Alignment - 1));

    assert_msg(
        pos + sizeof(T) <= DataStorage.data() + TotalSize,
        "Ran out of space in the callback DataStorage"
    );

    T* obj = new (pos) T(std::forward<Args>(args)...);
    pos += sizeof(T);
    return obj;
#else // ^^^^ WIN32 / !WIN32 vvvv
    return new T(std::forward<Args>(args)...);
#endif // WIN32
}

} // namespace

void create() {
    ZoneScoped;

#ifdef WIN32
    std::fill(DataStorage.begin(), DataStorage.end(), std::byte(0));
    std::byte* currentPos = DataStorage.data();
#else // ^^^^ WIN32 / !WIN32 vvvv
    std::byte* currentPos = nullptr;
#endif // WIN32

    using VoidCallbacks = std::vector<std::function<void()>>;

    initialize = createCallback<VoidCallbacks>(currentPos);
    deinitialize = createCallback<VoidCallbacks>(currentPos);
    initializeGL = createCallback<VoidCallbacks>(currentPos);
    deinitializeGL = createCallback<VoidCallbacks>(currentPos);
    preSync = createCallback<VoidCallbacks>(currentPos);
    postSyncPreDraw = createCallback<VoidCallbacks>(currentPos);
    render = createCallback<std::vector<
        std::function<void(const glm::mat4&, const glm::mat4&, const glm::mat4&)>
    >>(currentPos);
    draw2D = createCallback<VoidCallbacks>(currentPos);
    postDraw = createCallback<VoidCallbacks>(currentPos);
    keyboard = createCallback<std::vector<KeyboardCallback>>(currentPos);
    character = createCallback<std::vector<CharacterCallback>>(currentPos);
    mouseButton = createCallback<std::vector<MouseButtonCallback>>(currentPos);
    mousePosition = createCallback<std::vector<MousePositionCallback>>(currentPos);
    mouseScrollWheel = createCallback<std::vector<MouseScrollWheelCallback>>(currentPos);
    touchDetected =
        createCallback<std::vector<std::function<bool(TouchInput)>>>(currentPos);
    touchUpdated =
        createCallback<std::vector<std::function<bool(TouchInput)>>>(currentPos);
    touchExit = createCallback<std::vector<std::function<void(TouchInput)>>>(currentPos);
}

void destroy() {
#ifdef WIN32
    touchExit->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete touchExit;
#endif // WIN32

#ifdef WIN32
    touchUpdated->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete touchUpdated;
#endif // WIN32

#ifdef WIN32
    touchDetected->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete touchDetected;
#endif // WIN32

#ifdef WIN32
    mouseScrollWheel->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete mouseScrollWheel;
#endif // WIN32

#ifdef WIN32
    mousePosition->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete mousePosition;
#endif // WIN32

#ifdef WIN32
    mouseButton->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete mouseButton;
#endif // WIN32

#ifdef WIN32
    character->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete character;
#endif // WIN32

#ifdef WIN32
    keyboard->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete keyboard;
#endif // WIN32

#ifdef WIN32
    postDraw->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete postDraw;
#endif // WIN32

#ifdef WIN32
    draw2D->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete draw2D;
#endif // WIN32

#ifdef WIN32
    render->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete render;
#endif // WIN32

#ifdef WIN32
    postSyncPreDraw->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete postSyncPreDraw;
#endif // WIN32

#ifdef WIN32
    preSync->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete preSync;
#endif // WIN32

#ifdef WIN32
    deinitializeGL->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete deinitializeGL;
#endif // WIN32

#ifdef WIN32
    initializeGL->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete initializeGL;
#endif // WIN32

#ifdef WIN32
    deinitialize->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete deinitialize;
#endif // WIN32

#ifdef WIN32
    initialize->~vector();
#else // ^^^^ WIN32 / !WIN32 vvvv
    delete initialize;
#endif // WIN32
}

void(*webBrowserPerformanceHotfix)() = nullptr;

} // namespace openspace::global::callback
