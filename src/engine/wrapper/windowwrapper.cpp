/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/engine/wrapper/windowwrapper.h>
#include <ghoul/misc/exception.h>
#include <string>

namespace openspace {

WindowWrapper::WindowWrapperException::WindowWrapperException(const std::string& msg)
    : ghoul::RuntimeError(msg, "WindowWrapper")
{}
    
void WindowWrapper::setBarrier(bool) {}
    
void WindowWrapper::clearAllWindows(const glm::vec4& clearColor) {}

bool WindowWrapper::windowHasResized() const {
    return false;
}
    
double WindowWrapper::averageDeltaTime() const {
    return 0.0;
}
    
glm::vec2 WindowWrapper::mousePosition() const {
    return glm::vec2(0.f);
}
    
uint32_t WindowWrapper::mouseButtons(int maxNumber) const {
    return uint32_t(0);
}
    
glm::ivec2 WindowWrapper::currentWindowSize() const {
    return glm::ivec2(0);
}
    
glm::ivec2 WindowWrapper::currentWindowResolution() const {
    return currentWindowSize();
}

glm::ivec2 WindowWrapper::currentDrawBufferResolution() const {
    return currentWindowSize();
}

int WindowWrapper::currentNumberOfAaSamples() const {
    return 1;
}

bool WindowWrapper::isRegularRendering() const {
    return true;
}

glm::mat4 WindowWrapper::viewProjectionMatrix() const {
    return glm::mat4(1.f);
}
    
void WindowWrapper::setNearFarClippingPlane(float near, float far) {}
    
glm::ivec4 WindowWrapper::viewportPixelCoordinates() const {
    return glm::ivec4(
        0,
        currentWindowResolution().x,
        0,
        currentWindowResolution().y
    );
}
    
    
bool WindowWrapper::isExternalControlConnected() const {
    return false;
}
    
void WindowWrapper::sendMessageToExternalControl(const std::vector<char>& message) const {
}
    
bool WindowWrapper::isSimpleRendering() const {
    return true;
}
    
void WindowWrapper::takeScreenshot() const {}
    
} // namespace openspace
