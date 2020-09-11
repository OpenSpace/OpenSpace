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

#ifndef __OPENSPACE_CORE__WINDOWDELEGATE___H__
#define __OPENSPACE_CORE__WINDOWDELEGATE___H__

#include <ghoul/glm.h>
#include <glbinding/glbinding.h>
#include <vector>

namespace openspace {

struct WindowDelegate {
    enum class Frustum { Mono, LeftEye, RightEye };

    void (*terminate)() = [](){};

    void (*setBarrier)(bool enabled) = [](bool) {};

    void (*setSynchronization)(bool enabled) = [](bool) {};

    bool (*windowHasResized)() = []() { return false; };

    double (*averageDeltaTime)() = []() { return 0.0; };

    double (*minDeltaTime)() = []() { return 0.0; };

    double (*maxDeltaTime)() = []() { return 0.0; };

    double (*deltaTimeStandardDeviation)() = []() { return 0.0; };

    double (*deltaTime)() = []() { return 0.0; };

    double (*applicationTime)() = []() { return 0.0; };

    glm::ivec2 (*currentWindowSize)() = []() { return glm::ivec2(0); };

    glm::ivec2 (*currentSubwindowSize)() = []() { return glm::ivec2(0); };

    glm::ivec2 (*currentDrawBufferResolution)() = []() { return glm::ivec2(0); };

    glm::ivec2 (*currentViewportSize)() = []() { return glm::ivec2(0); };

    glm::vec2 (*dpiScaling)() = []() { return glm::vec2(1.f); };

    bool (*hasGuiWindow)() = []() { return false; };

    bool (*isGuiWindow)() = []() { return false; };

    bool (*isMaster)() = []() { return true; };

    glm::mat4 (*modelMatrix)() = []() { return glm::mat4(1.f); };

    void (*setNearFarClippingPlane)(float near, float far) = [](float, float) {};

    bool (*isFisheyeRendering)() = []() { return false; };

    unsigned int (*takeScreenshot)(bool applyWarping) = [](bool) { return 0u; };

    void (*swapBuffer)() = []() {};

    int (*nWindows)() = []() { return 0; };

    int (*currentWindowId)() = []() { return 0; };

    double (*getHorizFieldOfView)() = []() { return 0.0; };

    void (*setHorizFieldOfView)(float hFovDeg) = [](float) { };

    void* (*getNativeWindowHandle)(size_t windowIndex) = [](size_t) -> void* {
        return nullptr;
    };

    using GLProcAddress = void(*)(void);

    GLProcAddress (*openGLProcedureAddress)(const char*) =
        [](const char*) -> GLProcAddress { return []() {}; };

    Frustum (*frustumMode)() = []() { return Frustum::Mono; };

    uint64_t (*swapGroupFrameNumber)() = []() { return uint64_t(0); };
};

} // namespace openspace

#endif // __OPENSPACE_CORE__WINDOWDELEGATE___H__
