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

#ifndef __GUI_H__
#define __GUI_H__

#include <modules/onscreengui/include/guicomponent.h>
#include <modules/onscreengui/include/guihelpcomponent.h>
#include <modules/onscreengui/include/guiperformancecomponent.h>
#include <modules/onscreengui/include/guipropertycomponent.h>
#include <modules/onscreengui/include/guiorigincomponent.h>
#include <modules/onscreengui/include/guitimecomponent.h>
#include <modules/onscreengui/include/guiiswacomponent.h>
#include <openspace/scripting/scriptengine.h>

#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>

namespace openspace {
namespace gui {

class GUI : public GuiComponent {
public:
    GUI();
    ~GUI();

    void initialize();
    void deinitialize();

    void initializeGL();
    void deinitializeGL();

    bool mouseButtonCallback(MouseButton button, MouseAction action);
    bool mouseWheelCallback(double position);
    bool keyCallback(Key key, KeyModifier modifier, KeyAction action);
    bool charCallback(unsigned int character, KeyModifier modifier);

    void startFrame(float deltaTime, const glm::vec2& windowSize, const glm::vec2& mousePosCorrectionFactor, const glm::vec2& mousePos, uint32_t mouseButtons);
    void endFrame();

    void render();

    static openspace::scripting::ScriptEngine::LuaLibrary luaLibrary();

//protected:
    GuiHelpComponent _help;
    GuiOriginComponent _origin;
    GuiPerformanceComponent _performance;
    GuiPropertyComponent _property;
    GuiPropertyComponent _screenSpaceProperty;
    GuiPropertyComponent _globalProperty;
    GuiTimeComponent _time;
    GuiIswaComponent _iswa;
};

} // namespace gui
} // namespace openspace

#endif // __GUI_H__
