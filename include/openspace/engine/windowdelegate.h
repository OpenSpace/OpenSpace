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

#ifndef __OPENSPACE_CORE__WINDOWDELEGATE___H__
#define __OPENSPACE_CORE__WINDOWDELEGATE___H__

#include <ghoul/glm.h>
#include <vector>

namespace openspace {

struct WindowDelegate {
    void (*terminate)() = [](){};

    void (*setBarrier)(bool enabled) = [](bool) {};

    void (*setSynchronization)(bool enabled) = [](bool) {};

    void (*clearAllWindows)(const glm::vec4& clearColor) = [](const glm::vec4&) {};

    bool (*windowHasResized)() = []() { return false; };

    double (*averageDeltaTime)() = []() { return 0.0; };

    double (*deltaTime)() = []() { return 0.0; };

    double (*applicationTime)() = []() { return 0.0; };

    glm::vec2 (*mousePosition)() = []() { return glm::vec2(0.f); };

    uint32_t (*mouseButtons)(int maxNumber) = [](int) { return uint32_t(0); };

    glm::ivec2 (*currentWindowSize)() = []() { return glm::ivec2(0); };

    glm::ivec2 (*currentSubwindowSize)() = []() { return glm::ivec2(0); };

    glm::ivec2 (*currentWindowResolution)() = []() { return glm::ivec2(0); };

    glm::ivec2 (*currentDrawBufferResolution)() = []() { return glm::ivec2(0); };

    glm::ivec2 (*currentViewportSize)() = []() { return glm::ivec2(0); };

    glm::vec2 (*dpiScaling)() = []() { return glm::vec2(1.f); };

    int (*currentNumberOfAaSamples)() = []() { return 1; };

    bool (*isRegularRendering)() = []() { return true; };

    bool (*hasGuiWindow)() = []() { return false; };

    bool (*isGuiWindow)() = []() { return false; };

    bool (*isMaster)() = []() { return false; };

    int (*clusterId)() = []() { return 0; };

    bool (*isUsingSwapGroups)() = []() { return false; };

    bool (*isSwapGroupMaster)() = []() { return false; };

    glm::mat4 (*viewProjectionMatrix)() = []() { return glm::mat4(1.f); };

    glm::mat4 (*modelMatrix)() = []() { return glm::mat4(1.f); };

    void (*setNearFarClippingPlane)(float near, float far) = [](float, float) {};

    void (*setEyeSeparationDistance)(float distance) = [](float) {};

    glm::ivec4 (*viewportPixelCoordinates)() = []() { return glm::ivec4(0, 0, 0, 0); };

    bool (*isExternalControlConnected)() = []() { return false; };

    void (*sendMessageToExternalControl)(const std::vector<char>& message) =
        [](const std::vector<char>&) {};

    bool (*isSimpleRendering)() = []() { return true; };

    bool (*isFisheyeRendering)() = []() { return false; };

    void (*takeScreenshot)(bool applyWarping) = [](bool) { };

    void (*swapBuffer)() = []() {};

    int (*nWindows)() = []() { return 0; };

    int (*currentWindowId)() = []() { return 0; };
};

} // namespace openspace

#endif // __OPENSPACE_CORE__WINDOWDELEGATE___H__
