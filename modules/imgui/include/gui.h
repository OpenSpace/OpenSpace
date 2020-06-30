/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#ifndef __OPENSPACE_MODULE_IMGUI___GUI___H__
#define __OPENSPACE_MODULE_IMGUI___GUI___H__

#include <modules/imgui/include/guicomponent.h>

#include <modules/imgui/include/guiassetcomponent.h>
#include <modules/imgui/include/guifilepathcomponent.h>
#include <modules/imgui/include/guiglobebrowsingcomponent.h>
#include <modules/imgui/include/guihelpcomponent.h>
#include <modules/imgui/include/guiiswacomponent.h>
#include <modules/imgui/include/guijoystickcomponent.h>
#include <modules/imgui/include/guimissioncomponent.h>
#include <modules/imgui/include/guiparallelcomponent.h>
#include <modules/imgui/include/guiperformancecomponent.h>
#include <modules/imgui/include/guipropertycomponent.h>
#include <modules/imgui/include/guishortcutscomponent.h>
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
#include <array>

//#define SHOW_IMGUI_HELPERS

struct ImGuiContext;

namespace ghoul::opengl {
    class ProgramObject;
    class Texture;
} // namespace ghoul::opengl

namespace openspace::gui {

namespace detail {
    constexpr int nComponents() {
        const int nRegularComponents = 16;
        int totalComponents = nRegularComponents;

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
        ++totalComponents;
#endif

        return totalComponents;
    }
} // namespace detail

class GUI : public GuiComponent {
public:
    GUI();
    ~GUI();

    void initialize() override;
    void deinitialize() override;

    void initializeGL() override;
    void deinitializeGL() override;

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

    void render() override;

//protected:
    GuiHelpComponent _help;
    GuiFilePathComponent _filePath;
    GuiAssetComponent _asset;
    GuiGlobeBrowsingComponent _globeBrowsing;
    GuiPerformanceComponent _performance;

    GuiPropertyComponent _globalProperty;
    GuiPropertyComponent _sceneProperty;
    GuiPropertyComponent _screenSpaceProperty;
    GuiPropertyComponent _moduleProperty;

    GuiPropertyComponent _virtualProperty;
    GuiSpaceTimeComponent _spaceTime;
    GuiMissionComponent _mission;
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    GuiIswaComponent _iswa;
#endif // OPENSPACE_MODULE_ISWA_ENABLED
    GuiShortcutsComponent _shortcuts;
    GuiJoystickComponent _joystick;
    GuiParallelComponent _parallel;
    GuiPropertyComponent _featuredProperties;


    properties::BoolProperty _showHelpText;
    properties::FloatProperty _helpTextDelay;

private:
    void renderAndUpdatePropertyVisibility();

    // The ordering of this array determines the order of components in the in-game menu
    std::array<GuiComponent*, detail::nComponents()> _components = {
        &_sceneProperty,
        &_screenSpaceProperty,
        &_featuredProperties,
        &_virtualProperty,
        &_globalProperty,
        &_moduleProperty,

        &_spaceTime,
        &_mission,
        &_parallel,
        &_globeBrowsing,
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
        &_iswa,
#endif

        &_asset,
        &_shortcuts,
        &_joystick,
        &_filePath,

        &_performance,

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
};

void CaptionText(const char* text);

} // namespace openspace::gui

#endif // __OPENSPACE_MODULE_IMGUI___GUI___H__
