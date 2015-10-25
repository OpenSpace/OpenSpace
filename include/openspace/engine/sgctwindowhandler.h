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
    
    double averageDeltaTime() override;
    glm::vec2 mousePosition() override;
    uint32_t mouseButtons(int maxNumber) override;
    glm::ivec2 currentWindowSize() override;
    glm::ivec2 currentWindowResolution() override;
    
    bool isRegularRendering() override;

    
    //    void forEachWindow(std::function<void (void)> function) override;

};

} // namespace openspace

#endif // __SGCTWINDOWHANDLER_H__
