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

#include <openspace/engine/wrapper/sgctwindowwrapper.h>

#include "sgct.h"

#undef near
#undef far

namespace {
    const char* GuiWindowTag = "GUI";

    static const openspace::properties::Property::PropertyInfo EyeSeparationInfo = {
        "EyeSeparation",
        "Eye Separation",
        "Sets a static eye separation for use in stereoscopic rendering. If no "
        "stereoscopic rendering is performed, this value is unused."
    };

    static const openspace::properties::Property::PropertyInfo ShowStatsGraphInfo = {
        "ShowStatsGraph",
        "Show Statistics Graph",
        "Toggles the rendering of the SGCT statistics graph that is rendered on top of "
        "every image. The statistics show the frame time, synchronization time, and many "
        "other timings as reported by SGCT."
    };
} // namespace

namespace openspace {

SGCTWindowWrapper::SGCTWindowWrapper()
    : _eyeSeparation(EyeSeparationInfo, 0.f, 0.f, 0.2f)
    , _showStatsGraph(ShowStatsGraphInfo, false)
{
    _showStatsGraph.onChange([this](){
        sgct::Engine::instance()->setStatsGraphVisibility(_showStatsGraph);
    });
    addProperty(_showStatsGraph);

    addProperty(_eyeSeparation);
    _eyeSeparation.onChange([this](){
        setEyeSeparationDistance(_eyeSeparation);
    });
}

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

double SGCTWindowWrapper::applicationTime() const {
    return sgct::Engine::getTime();
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
        if (button) {
            result |= (1 << i);
        }
    }
    return result;
}

glm::ivec2 SGCTWindowWrapper::currentWindowSize() const {
    auto window = sgct::Engine::instance()->getCurrentWindowPtr();
    return glm::ivec2(
        window->getXResolution(),
        window->getYResolution());
}

glm::ivec2 SGCTWindowWrapper::currentSubwindowSize() const {
    auto window = sgct::Engine::instance()->getCurrentWindowPtr();
    switch (window->getStereoMode()) {
    case sgct::SGCTWindow::Side_By_Side_Stereo:
    case sgct::SGCTWindow::Side_By_Side_Inverted_Stereo:
        return glm::ivec2(
            window->getXResolution() / 2,
            window->getYResolution());
    case sgct::SGCTWindow::Top_Bottom_Stereo:
    case sgct::SGCTWindow::Top_Bottom_Inverted_Stereo:
        return glm::ivec2(
            window->getXResolution(),
            window->getYResolution() / 2);
    default:
        return glm::ivec2(
            window->getXResolution(),
            window->getYResolution());
    }
}

glm::ivec2 SGCTWindowWrapper::currentWindowResolution() const {
    int x, y;
    auto window = sgct::Engine::instance()->getCurrentWindowPtr();
    window->getFinalFBODimensions(x, y);
    return glm::ivec2(x, y);
}

glm::ivec2 SGCTWindowWrapper::currentDrawBufferResolution() const {
    sgct_core::Viewport* viewport =
                          sgct::Engine::instance()->getCurrentWindowPtr()->getViewport(0);
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

glm::ivec2 SGCTWindowWrapper::getCurrentViewportSize() const {
    sgct_core::Viewport* viewport =
        sgct::Engine::instance()->getCurrentWindowPtr()->getViewport(0);
    if (viewport != nullptr) {
        int xx = 0, yy = 0;
        sgct::Engine::instance()->getCurrentViewportSize(xx, yy);
        return glm::ivec2(xx, yy);
    }
    throw WindowWrapperException("No viewport available");
}

glm::vec2 SGCTWindowWrapper::dpiScaling() const {
    return glm::vec2(
        sgct::Engine::instance()->getCurrentWindowPtr()->getXScale(),
        sgct::Engine::instance()->getCurrentWindowPtr()->getYScale()
    );
}

int SGCTWindowWrapper::currentNumberOfAaSamples() const {
    return sgct::Engine::instance()->getCurrentWindowPtr()->getNumberOfAASamples();
}

bool SGCTWindowWrapper::isRegularRendering() const {
    sgct::SGCTWindow* w = sgct::Engine::instance()->getCurrentWindowPtr();
    ghoul_assert(
        w->getNumberOfViewports() > 0,
        "At least one viewport must exist at this time"
    );
    sgct_core::Viewport* vp = w->getViewport(0);
    sgct_core::NonLinearProjection* nlp = vp->getNonLinearProjectionPtr();
    return nlp == nullptr;
}

bool SGCTWindowWrapper::hasGuiWindow() const {
    auto engine = sgct::Engine::instance();
    for (size_t i = 0; i < engine->getNumberOfWindows(); ++i) {
        if (engine->getWindowPtr(i)->checkIfTagExists(GuiWindowTag)) {
            return true;
        }
    }
    return false;
}

bool SGCTWindowWrapper::isGuiWindow() const {
    return sgct::Engine::instance()->getCurrentWindowPtr()->checkIfTagExists(
        GuiWindowTag
    );
}

bool SGCTWindowWrapper::isMaster() const {
    return sgct::Engine::instance()->isMaster();
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
    sgct::SGCTWindow* window = sgct::Engine::instance()->getCurrentWindowPtr();   
    if (!window || !window->getCurrentViewport()) {
        return glm::ivec4(0, 0, 0, 0);
    }

    const int* viewportData = sgct::Engine::instance()->getCurrentViewportPixelCoords();
    return glm::ivec4(viewportData[0], viewportData[2], viewportData[1], viewportData[3]);
}

bool SGCTWindowWrapper::isExternalControlConnected() const {
    return sgct::Engine::instance()->isExternalControlConnected();
}

void SGCTWindowWrapper::sendMessageToExternalControl(
                                                   const std::vector<char>& message) const
{
    sgct::Engine::instance()->sendMessageToExternalControl(
        message.data(),
        static_cast<int>(message.size())
    );
}

bool SGCTWindowWrapper::isSimpleRendering() const {
    return (sgct::Engine::instance()->getCurrentRenderTarget() !=
            sgct::Engine::NonLinearBuffer);
}

void SGCTWindowWrapper::takeScreenshot(bool applyWarping) const {
    sgct::SGCTSettings::instance()->setCaptureFromBackBuffer(applyWarping);
    sgct::Engine::instance()->takeScreenshot();
}

void SGCTWindowWrapper::swapBuffer() const {
    GLFWwindow* w = glfwGetCurrentContext();
    glfwSwapBuffers(w);

    glfwPollEvents();
}

int SGCTWindowWrapper::nWindows() const {
    return static_cast<int>(sgct::Engine::instance()->getNumberOfWindows());
}

int SGCTWindowWrapper::currentWindowId() const {
    return sgct::Engine::instance()->getCurrentWindowPtr()->getId();
}

} // namespace openspace
