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

#ifndef __SGCTWINDOWHANDLER_H__
#define __SGCTWINDOWHANDLER_H__

#include <openspace/engine/windowhandler.h>

namespace openspace {

class SGCTWindowHandler : public WindowHandler {
public:
    void setBarrier(bool enabled) override;
    void clearAllWindows() override;
    bool windowHasResized() const override;

    double time() const override;
    double averageDeltaTime() const override;
    glm::vec2 mousePosition() const override;
    uint32_t mouseButtons(int maxNumber) const override;
    glm::ivec2 currentWindowSize() const override;
    glm::ivec2 currentWindowResolution() const override;
    
    bool isRegularRendering() const override;
    
    glm::mat4 viewProjectionMatrix() const override;
    void setNearFarClippingPlane(float near, float far) override;
    
    glm::ivec4 viewportPixelCoordinates() const override;
    
    bool isExternalControlConnected() const override;
    void sendMessageToExternalControl(const std::vector<char>& message) const override;

    bool isSimpleRendering() const override;

    void takeScreenshot() const override;
    
    
    //    void forEachWindow(std::function<void (void)> function) override;
};

} // namespace openspace

#endif // __SGCTWINDOWHANDLER_H__
