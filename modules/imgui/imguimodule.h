/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_MODULE_IMGUI___IMGUIMODULE___H__
#define __OPENSPACE_MODULE_IMGUI___IMGUIMODULE___H__

#include <openspace/util/openspacemodule.h>

#include <modules/imgui/include/guiactioncomponent.h>
#include <modules/imgui/include/guifilepathcomponent.h>
#include <modules/imgui/include/guigibscomponent.h>
#include <modules/imgui/include/guiglobebrowsingcomponent.h>
#include <modules/imgui/include/guihelpcomponent.h>
#include <modules/imgui/include/guijoystickcomponent.h>
#include <modules/imgui/include/guimemorycomponent.h>
#include <modules/imgui/include/guimissioncomponent.h>
#include <modules/imgui/include/guiparallelcomponent.h>
#include <modules/imgui/include/guipropertycomponent.h>
#include <modules/imgui/include/guiscenecomponent.h>
#include <modules/imgui/include/guispacetimecomponent.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <openspace/util/touch.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <ghoul/opengl/texture.h>
#include <array>

struct ImGuiContext;

namespace openspace {

class ImGUIModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "ImGUI";

    ImGUIModule();

    void internalInitialize(const ghoul::Dictionary& configuration) override;
    void internalDeinitialize() override;
    void internalInitializeGL() override;
    void internalDeinitializeGL() override;

private:
    bool mouseButtonCallback(MouseButton button, MouseAction action);
    bool mouseWheelCallback(double position);
    bool keyCallback(Key key, KeyModifier modifier, KeyAction action);
    bool charCallback(unsigned int character, KeyModifier modifier);

    bool touchDetectedCallback(TouchInput input);
    bool touchUpdatedCallback(TouchInput input);
    void touchExitCallback(TouchInput input);

    void renderFrame(float deltaTime, const glm::vec2& windowSize,
        const glm::vec2& dpiScaling, const glm::vec2& mousePos,
        uint32_t mouseButtonsPressed);

    properties::BoolProperty _isEnabled;
    properties::BoolProperty _isCollapsed;

    gui::GuiPropertyComponent _sceneProperty;
    gui::GuiPropertyComponent _property;
    gui::GuiSpaceTimeComponent _spaceTime;
    gui::GuiJoystickComponent _joystick;
    gui::GuiActionComponent _actions;
    gui::GuiParallelComponent _parallel;
    gui::GuiGlobeBrowsingComponent _globeBrowsing;
    gui::GuiGIBSComponent _gibs;
    gui::GuiMissionComponent _mission;
    gui::GuiMemoryComponent _memoryComponent;
    gui::GuiSceneComponent _sceneView;
    gui::GuiFilePathComponent _filePath;
    gui::GuiHelpComponent _help;

    properties::BoolProperty _showHelpText;
    properties::FloatProperty _helpTextDelay;

    // The ordering of this array determines the order of components in the in-game menu
    static constexpr int nComponents = 13;
    std::array<gui::GuiComponent*, nComponents> _components = {
        &_sceneProperty,
        &_property,
        &_spaceTime,
        &_joystick,
        &_actions,
        &_parallel,
        &_globeBrowsing,
        &_gibs,
        &_mission,
        &_memoryComponent,
        &_sceneView,
        &_filePath,
        &_help
    };

    GLuint vao = 0;
    GLuint vbo = 0;
    GLuint vboElements = 0;
    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(tex, ortho) _uniformCache;
    std::unique_ptr<ghoul::opengl::Texture> _fontTexture;

    std::vector<ImGuiContext*> _contexts;

    std::vector<TouchInput> _validTouchStates;

    std::vector<char> _iniFileBuffer;

    glm::vec2 _mousePosition = glm::vec2(0.f);
    uint32_t _mouseButtons = 0;
};

void CaptionText(const char* text);

} // namespace openspace

#endif // __OPENSPACE_MODULE_IMGUI___IMGUIMODULE___H__
