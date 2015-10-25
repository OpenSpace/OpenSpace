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
    
double SGCTWindowHandler::averageDeltaTime() {
    return sgct::Engine::instance()->getAvgDt();
}
    
glm::vec2 SGCTWindowHandler::mousePosition() {
    double posX, posY;
    sgct::Engine::instance()->getMousePos(0, &posX, &posY);
    return glm::vec2(posX, posY);
}
    
glm::ivec2 SGCTWindowHandler::currentWindowSize() {
    return glm::ivec2(0);
}
    
glm::ivec2 SGCTWindowHandler::currentWindowResolution() {
    int x,y;
    sgct::Engine::instance()->getWindowPtr(0)->getFinalFBODimensions(x, y);
    return glm::ivec2(x, y);
}

//void forEachWindow(std::function<void (void)> function) {
//    size_t n = sgct::Engine::instance()->getNumberOfWindows();
//    for (size_t i = 0; i < n; ++i)
//        function();
//}
    
} // namespace openspace

