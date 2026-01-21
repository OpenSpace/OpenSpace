/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#ifndef __OPENSPACE_MODULE_TOUCH___TOUCHMODULE___H__
#define __OPENSPACE_MODULE_TOUCH___TOUCHMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <openspace/util/touch.h>
#include <memory>
#include <set>

namespace openspace {

class TuioEar;

#ifdef WIN32
class Win32TouchHook;
#endif //WIN32

class TouchModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "Touch";

    TouchModule();
    ~TouchModule();

protected:
    void internalInitialize(const ghoul::Dictionary& dictionary) override;

private:
    /**
     * Process TUIO touch input that occured since the last frame.
     */
    void processNewInput();

    std::unique_ptr<TuioEar> _ear;

    properties::IntProperty _tuioPort;
    properties::BoolProperty _hasActiveTouchEvent;

    // contains an id and the Point that was processed last frame
    glm::ivec2 _webPositionCallback = glm::ivec2(0);
#ifdef WIN32
    std::unique_ptr<Win32TouchHook> _win32TouchHook;
#endif // WIN32
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_TOUCH___TOUCHMODULE___H__
