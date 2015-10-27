/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2015                                                               *
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

#include <openspace/engine/wrapper/dummywindowwrapper.h>

namespace openspace {
    
void DummyWindowWrapper::setBarrier(bool enabled) {}
    
void DummyWindowWrapper::clearAllWindows() {}

bool DummyWindowWrapper::windowHasResized() const {
    return false;
}
    
double DummyWindowWrapper::time() const {
    return 0.0;
}
    
double DummyWindowWrapper::averageDeltaTime() const {
    return 0.0;
}
    
glm::vec2 DummyWindowWrapper::mousePosition() const {
    return glm::vec2(0.f);
}
    
uint32_t DummyWindowWrapper::mouseButtons(int maxNumber) const {
    return 0;
}
    
glm::ivec2 DummyWindowWrapper::currentWindowSize() const {
    return glm::ivec2(0);
}
    
glm::ivec2 DummyWindowWrapper::currentWindowResolution() const {
    return glm::ivec2(0);
}
    
bool DummyWindowWrapper::isRegularRendering() const {
    return true;
}
    
glm::mat4 DummyWindowWrapper::viewProjectionMatrix() const {
    return glm::mat4(1.f);
}
    
void DummyWindowWrapper::setNearFarClippingPlane(float near, float far) {}
    
glm::ivec4 DummyWindowWrapper::viewportPixelCoordinates() const {
    return glm::ivec4(0);
}
    
bool DummyWindowWrapper::isExternalControlConnected() const {
    return false;
}
    
void DummyWindowWrapper::sendMessageToExternalControl(const std::vector<char>& message) const {
}
    
bool DummyWindowWrapper::isSimpleRendering() const {
    return true;
}
    
void DummyWindowWrapper::takeScreenshot() const {}
    
} // namespace openspace

