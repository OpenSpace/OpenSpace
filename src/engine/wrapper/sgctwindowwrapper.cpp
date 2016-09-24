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

#include <ghoul/opengl/ghoul_gl.h>
#include "sgct.h"

#include <openspace/engine/wrapper/sgctwindowwrapper.h>

#undef near
#undef far

namespace {
    const std::string GuiWindowName = "GUI";
}

namespace openspace {
    
void SGCTWindowWrapper::terminate() {
    sgct::Engine::instance()->terminate();
}

void SGCTWindowWrapper::setBarrier(bool enabled) {
    sgct::SGCTWindow::setBarrier(enabled);
}
    
void SGCTWindowWrapper::setSynchronization(bool enabled) {
    sgct_core::ClusterManager::instance()->setUseIgnoreSync(enabled);
}

void SGCTWindowWrapper::clearAllWindows(const glm::vec4& clearColor) {
    size_t n = sgct::Engine::instance()->getNumberOfWindows();
    for (size_t i = 0; i < n; ++i) {
        glClearColor(clearColor.r, clearColor.g, clearColor.b, clearColor.a);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        GLFWwindow* win = sgct::Engine::instance()->getWindowPtr(i)->getWindowHandle();
        glfwSwapBuffers(win);
    }
}

bool SGCTWindowWrapper::windowHasResized() const {
    return sgct::Engine::instance()->getCurrentWindowPtr()->isWindowResized();
}
    
double SGCTWindowWrapper::averageDeltaTime() const {
    return sgct::Engine::instance()->getAvgDt();
}

double SGCTWindowWrapper::deltaTime() const {
    return sgct::Engine::instance()->getDt();
}
    
glm::vec2 SGCTWindowWrapper::mousePosition() const {
    int id = sgct::Engine::instance()->getCurrentWindowPtr()->getId();
    double posX, posY;
    sgct::Engine::instance()->getMousePos(id, &posX, &posY);
    return glm::vec2(posX, posY);
}
    
uint32_t SGCTWindowWrapper::mouseButtons(int maxNumber) const {
    int id = sgct::Engine::instance()->getCurrentWindowPtr()->getId();
    uint32_t result = 0;
    for (int i = 0; i < maxNumber; ++i) {
        bool button = (sgct::Engine::instance()->getMouseButton(id, i) != 0);
        if (button)
            result |= (1 << i);
        
    }
    return result;
}
    
glm::ivec2 SGCTWindowWrapper::currentWindowSize() const {
    return glm::ivec2(sgct::Engine::instance()->getCurrentWindowPtr()->getXResolution(),
                      sgct::Engine::instance()->getCurrentWindowPtr()->getYResolution());
}
    
glm::ivec2 SGCTWindowWrapper::currentWindowResolution() const {
    int x,y;
    sgct::Engine::instance()->getCurrentWindowPtr()->getFinalFBODimensions(x, y);
    return glm::ivec2(x, y);
}

glm::ivec2 SGCTWindowWrapper::currentDrawBufferResolution() const {
    sgct_core::Viewport* viewport = sgct::Engine::instance()->getCurrentWindowPtr()->getViewport(0);
    if (viewport != nullptr){
        if (viewport->hasSubViewports() && viewport->getNonLinearProjectionPtr()) {
            int res = viewport->getNonLinearProjectionPtr()->getCubemapResolution();
            return glm::ivec2(res, res);
        } else {
            return currentWindowResolution();
        }
    }
    throw WindowWrapperException("No viewport available");
}

int SGCTWindowWrapper::currentNumberOfAaSamples() const {
    return sgct::Engine::instance()->getCurrentWindowPtr()->getNumberOfAASamples();
} 
    
bool SGCTWindowWrapper::isRegularRendering() const {
    // TODO: Needs to implement the nonlinear rendering check ---abock
    
    // sgct::SGCTWindow* w = sgct::Engine::instance()->getCurrentWindowPtr();
    // !w->isUsingFisheyeRendering() does not exist anymore ---abock
    //        if (_isMaster && !w->isUsingFisheyeRendering() && _console->isVisible()) {

    return true;
}

bool SGCTWindowWrapper::hasGuiWindow() const {
    auto engine = sgct::Engine::instance();
    for (size_t i = 0; i < engine->getNumberOfWindows(); ++i) {
        if (engine->getWindowPtr(i)->getName() == GuiWindowName)
            return true;
    }
    return false;
}

bool SGCTWindowWrapper::isGuiWindow() const {
    return sgct::Engine::instance()->getCurrentWindowPtr()->getName() == GuiWindowName;
}

bool SGCTWindowWrapper::isSwapGroupMaster() const {
    return sgct::Engine::instance()->getCurrentWindowPtr()->isSwapGroupMaster();
}

bool SGCTWindowWrapper::isUsingSwapGroups() const {
    return sgct::Engine::instance()->getCurrentWindowPtr()->isUsingSwapGroups();
}

    
glm::mat4 SGCTWindowWrapper::viewProjectionMatrix() const {
    return sgct::Engine::instance()->getCurrentModelViewProjectionMatrix();
}

glm::mat4 SGCTWindowWrapper::modelMatrix() const {
    return sgct::Engine::instance()->getModelMatrix();
}
    
void SGCTWindowWrapper::setNearFarClippingPlane(float nearPlane, float farPlane) {
    sgct::Engine::instance()->setNearAndFarClippingPlanes(nearPlane, farPlane);
}

void SGCTWindowWrapper::setEyeSeparationDistance(float distance) {
    sgct::Engine::instance()->setEyeSeparation(distance);
}

glm::ivec4 SGCTWindowWrapper::viewportPixelCoordinates() const {
    int x1, xSize, y1, ySize;
    sgct::Engine::instance()->getCurrentWindowPtr()->getCurrentViewportPixelCoords(x1,
                                                                                   y1,
                                                                                   xSize,
                                                                                   ySize);
    return glm::ivec4(x1, xSize, y1, ySize);
}
    
bool SGCTWindowWrapper::isExternalControlConnected() const {
    return sgct::Engine::instance()->isExternalControlConnected();
}
    
void SGCTWindowWrapper::sendMessageToExternalControl(const std::vector<char>& message) const {
    sgct::Engine::instance()->sendMessageToExternalControl(
                                                           message.data(),
                                                           message.size());
}
    
bool SGCTWindowWrapper::isSimpleRendering() const {
    return (sgct::Engine::instance()->getCurrentRenderTarget() != sgct::Engine::NonLinearBuffer);

}
    
void SGCTWindowWrapper::takeScreenshot(bool applyWarping) const {
    sgct::SGCTSettings::instance()->setCaptureFromBackBuffer(applyWarping);
    sgct::Engine::instance()->takeScreenshot();
}
    
//void forEachWindow(std::function<void (void)> function) {
//    size_t n = sgct::Engine::instance()->getNumberOfWindows();
//    for (size_t i = 0; i < n; ++i)
//        function();
//}
    
} // namespace openspace

