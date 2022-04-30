/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/imgui/include/gui.h>
#include <modules/imgui/include/guiactioncomponent.h>
#include <modules/imgui/include/guifilepathcomponent.h>
#include <modules/imgui/include/guigibscomponent.h>
#include <modules/imgui/include/guiglobebrowsingcomponent.h>
#include <modules/imgui/include/guihelpcomponent.h>
#include <modules/imgui/include/guiiswacomponent.h>
#include <modules/imgui/include/guijoystickcomponent.h>
#include <modules/imgui/include/guimemorycomponent.h>
#include <modules/imgui/include/guimissioncomponent.h>
#include <modules/imgui/include/guiparallelcomponent.h>
#include <modules/imgui/include/guipropertycomponent.h>
#include <modules/imgui/include/guispacetimecomponent.h>
#include <openspace/properties/property.h>
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

namespace detail {
    constexpr int nComponents() {
        const int nRegularComponents = 12;
        int totalComponents = nRegularComponents;

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
        ++totalComponents;
#endif

        return totalComponents;
    }
} // namespace detail

class ImGUIModule : public OpenSpaceModule {
public:
    constexpr static const char* Name = "ImGUI";

    ImGUIModule();

    void internalInitialize(const ghoul::Dictionary& configuration) override;
    void internalDeinitialize() override;
    void internalInitializeGL() override;
    void internalDeinitializeGL() override;

    bool mouseButtonCallback(MouseButton button, MouseAction action);
    bool mouseWheelCallback(double position);
    bool keyCallback(Key key, KeyModifier modifier, KeyAction action);
    bool charCallback(unsigned int character, KeyModifier modifier);

    bool touchDetectedCallback(TouchInput input);
    bool touchUpdatedCallback(TouchInput input);
    void touchExitCallback(TouchInput input);

    void startFrame(float deltaTime, const glm::vec2& windowSize,
        const glm::vec2& dpiScaling, const glm::vec2& mousePos,
        uint32_t mouseButtonsPressed);
    void endFrame();

    void render();

    //gui::GUI gui;

private:
    void renderAndUpdatePropertyVisibility();

    properties::BoolProperty _isEnabled;
    properties::BoolProperty _isCollapsed;

    gui::GuiHelpComponent _help;
    gui::GuiFilePathComponent _filePath;
    gui::GuiGIBSComponent _gibs;
    gui::GuiGlobeBrowsingComponent _globeBrowsing;

    gui::GuiPropertyComponent _sceneProperty;
    gui::GuiPropertyComponent _property;
    gui::GuiMemoryComponent _memoryComponent;

    gui::GuiSpaceTimeComponent _spaceTime;
    gui::GuiMissionComponent _mission;
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    gui::GuiIswaComponent _iswa;
#endif // OPENSPACE_MODULE_ISWA_ENABLED
    gui::GuiActionComponent _actions;
    gui::GuiJoystickComponent _joystick;
    gui::GuiParallelComponent _parallel;


    properties::BoolProperty _showHelpText;
    properties::FloatProperty _helpTextDelay;

    // The ordering of this array determines the order of components in the in-game menu
    std::array<gui::GuiComponent*, detail::nComponents()> _components = {
        &_sceneProperty,
        &_property,
        &_memoryComponent,
        &_spaceTime,
        &_mission,
        &_parallel,
        &_gibs,
        &_globeBrowsing,
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
        & _iswa,
#endif
        & _actions,
        &_joystick,
        &_filePath,
        &_help
    };

    GLuint vao = 0;
    GLuint vbo = 0;
    GLuint vboElements = 0;
    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(tex, ortho) _uniformCache;
    std::unique_ptr<ghoul::opengl::Texture> _fontTexture;

#ifdef SHOW_IMGUI_HELPERS
    bool _showInternals = false;
#endif // SHOW_IMGUI_HELPERS

    properties::Property::Visibility _currentVisibility =
        properties::Property::Visibility::Developer;

    std::vector<ImGuiContext*> _contexts;

    std::vector<TouchInput> _validTouchStates;


    glm::vec2 _mousePosition = glm::vec2(0.f);
    uint32_t _mouseButtons = 0;
};

void CaptionText(const char* text);

} // namespace openspace

#endif // __OPENSPACE_MODULE_IMGUI___IMGUIMODULE___H__
