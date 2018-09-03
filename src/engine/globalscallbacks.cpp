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

#include <openspace/engine/globalscallbacks.h>

namespace openspace::global::detail {

std::vector<std::function<void()>>& gInitialize() {
    static std::vector<std::function<void()>> g;
    return g;
}

std::vector<std::function<void()>>& gDeinitialize() {
    static std::vector<std::function<void()>> g;
    return g;
}

std::vector<std::function<void()>>& gInitializeGL() {
    static std::vector<std::function<void()>> g;
    return g;
}

std::vector<std::function<void()>>& gDeinitializeGL() {
    static std::vector<std::function<void()>> g;
    return g;
}

std::vector<std::function<void()>>& gPreSync() {
    static std::vector<std::function<void()>> g;
    return g;
}

std::vector<std::function<void()>>& gPostSyncPreDraw() {
    static std::vector<std::function<void()>> g;
    return g;
}

std::vector<std::function<void()>>& gRender() {
    static std::vector<std::function<void()>> g;
    return g;
}

std::vector<std::function<void()>>& gDraw2D() {
    static std::vector<std::function<void()>> g;
    return g;
}

std::vector<std::function<void()>>& gPostDraw() {
    static std::vector<std::function<void()>> g;
    return g;
}

std::vector<std::function<bool(Key, KeyModifier, KeyAction)>>& gKeyboard() {
    static std::vector<std::function<bool(Key, KeyModifier, KeyAction)>> g;
    return g;
}

std::vector<std::function<bool(unsigned int, KeyModifier)>>& gCharacter() {
    static std::vector<std::function<bool(unsigned int, KeyModifier)>> g;
    return g;
}

std::vector<std::function<bool(MouseButton, MouseAction)>>& gMouseButton() {
    static std::vector<std::function<bool(MouseButton, MouseAction)>> g;
    return g;
}

std::vector<std::function<void(double, double)>>& gMousePosition() {
    static std::vector<std::function<void(double, double)>> g;
    return g;
}

std::vector<std::function<bool(double, double)>>& gMouseScrollWheel() {
    static std::vector<std::function<bool(double, double)>> g;
    return g;
}

} // namespace openspace::global::callback
