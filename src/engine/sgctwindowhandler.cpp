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

#include <ghoul/opengl/ghoul_gl.h>
#include "sgct.h"

#include <openspace/engine/sgctwindowhandler.h>

namespace openspace {
    
void SGCTWindowHandler::setBarrier(bool enabled) {
    sgct::SGCTWindow::setBarrier(enabled);
}
    
void SGCTWindowHandler::clearAllWindows() {
    size_t n = sgct::Engine::instance()->getNumberOfWindows();
    for (size_t i = 0; i < n; ++i) {
        glClearColor(0, 0, 0, 0);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        GLFWwindow* win = sgct::Engine::instance()->getWindowPtr(i)->getWindowHandle();
        glfwSwapBuffers(win);
    }
}
    
double SGCTWindowHandler::averageDeltaTime() const {
    return sgct::Engine::instance()->getAvgDt();
}
    
glm::vec2 SGCTWindowHandler::mousePosition() const {
    int id = sgct::Engine::instance()->getCurrentWindowPtr()->getId();
    double posX, posY;
    sgct::Engine::instance()->getMousePos(id, &posX, &posY);
    return glm::vec2(posX, posY);
}
    
uint32_t SGCTWindowHandler::mouseButtons(int maxNumber) const {
    int id = sgct::Engine::instance()->getCurrentWindowPtr()->getId();
    uint32_t result = 0;
    for (int i = 0; i < maxNumber; ++i) {
        bool button = (sgct::Engine::instance()->getMouseButton(id, i) != 0);
        if (button)
            result |= (1 << i);
        
    }
    return result;
}
    
glm::ivec2 SGCTWindowHandler::currentWindowSize() const {
    return glm::ivec2(sgct::Engine::instance()->getCurrentWindowPtr()->getXResolution(),
                      sgct::Engine::instance()->getCurrentWindowPtr()->getYResolution());
}
    
glm::ivec2 SGCTWindowHandler::currentWindowResolution() const {
    int x,y;
    sgct::Engine::instance()->getCurrentWindowPtr()->getFinalFBODimensions(x, y);
    return glm::ivec2(x, y);
}
    
bool SGCTWindowHandler::isRegularRendering() const {
    // TODO: Needs to implement the nonlinear rendering check ---abock
    
    // sgct::SGCTWindow* w = sgct::Engine::instance()->getCurrentWindowPtr();
    // !w->isUsingFisheyeRendering() does not exist anymore ---abock
    //        if (_isMaster && !w->isUsingFisheyeRendering() && _console->isVisible()) {

    return true;
}
    
glm::mat4 SGCTWindowHandler::viewProjectionMatrix() const {
    return sgct::Engine::instance()->getCurrentModelViewProjectionMatrix();
}

//void forEachWindow(std::function<void (void)> function) {
//    size_t n = sgct::Engine::instance()->getNumberOfWindows();
//    for (size_t i = 0; i < n; ++i)
//        function();
//}
    
} // namespace openspace

