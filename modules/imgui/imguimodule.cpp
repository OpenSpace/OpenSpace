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

#include <modules/imgui/imguimodule.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/globalscallbacks.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/interaction/sessionrecording.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/rendering/dashboard.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <modules/imgui/imguimodule.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/fmt.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <filesystem>
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
#include <array>

namespace {
    constexpr const char* _loggerCat = "GUI";
    constexpr const char* configurationFile = "imgui.ini";
    constexpr const char* GuiFont = "${FONTS}/arimo/Arimo-Regular.ttf";
    constexpr const float FontSize = 14.f;

    char* iniFileBuffer = nullptr;

    ImFont* captionFont = nullptr;

    constexpr const std::array<const char*, 2> UniformNames = { "tex", "ortho" };

    void addScreenSpaceRenderableLocal(std::string identifier, std::string texturePath) {
        if (!std::filesystem::is_regular_file(absPath(texturePath))) {
            LWARNING(fmt::format("Could not find image '{}'", texturePath));
            return;
        }

        std::string script;
        if (identifier.empty()) {
            script = fmt::format(
                "openspace.addScreenSpaceRenderable({{\
                    Type = 'ScreenSpaceImageLocal',\
                    TexturePath = openspace.absPath('{}')\
                }});",
                texturePath
            );
        }
        else {
            script = fmt::format(
                "openspace.addScreenSpaceRenderable({{\
                    Type = 'ScreenSpaceImageLocal',\
                    TexturePath = openspace.absPath('{}'),\
                    Identifier = '{}',\
                    Name = '{}'\
                }});",
                texturePath,
                identifier,
                identifier
            );
        }

        openspace::global::scriptEngine->queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    void addScreenSpaceRenderableOnline(std::string identifier, std::string texturePath) {
        std::string script;
        if (identifier.empty()) {
            script = fmt::format(
                "openspace.addScreenSpaceRenderable({{\
                    Type = 'ScreenSpaceImageOnline',\
                    URL = '{}'\
                }});",
                texturePath
            );
        }
        else {
            script = fmt::format(
                "openspace.addScreenSpaceRenderable({{\
                    Type = 'ScreenSpaceImageOnline',\
                    URL = '{}',\
                    Identifier = '{}',\
                    Name = '{}'\
                }});",
                texturePath,
                identifier,
                identifier
            );
        }

        openspace::global::scriptEngine->queueScript(
            script,
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this object will be visible or not."
    };

    constexpr openspace::properties::Property::PropertyInfo CollapsedInfo = {
        "Collapsed",
        "Is Collapsed",
        "This setting determines whether this window is collapsed or not."
    };

    constexpr openspace::properties::Property::PropertyInfo ShowHelpInfo = {
        "ShowHelpText",
        "Show tooltip help",
        "If this value is enabled these kinds of tooltips are shown for most properties "
        "explaining what impact they have on the visuals."
    };

    constexpr openspace::properties::Property::PropertyInfo HelpTextDelayInfo = {
        "HelpTextDelay",
        "Tooltip Delay (in s)",
        "This value determines the delay in seconds after which the tooltip is shown."
    };
} // namespace

namespace openspace {

void CaptionText(const char* text) {
    ImGui::PushFont(captionFont);
    ImGui::Text("%s", text);
    ImGui::PopFont();
}

ImGUIModule::ImGUIModule()
    : OpenSpaceModule(Name)
    , _isEnabled(EnabledInfo, false)
    , _isCollapsed(CollapsedInfo, false)
    , _sceneProperty("SceneProperties", "Scene", gui::GuiPropertyComponent::UseTreeLayout::Yes)
    , _property("Property", "Settings")
    , _showHelpText(ShowHelpInfo, true)
    , _helpTextDelay(HelpTextDelayInfo, 1.0, 0.0, 10.0)
{
    addProperty(_isEnabled);
    addProperty(_isCollapsed);

    for (gui::GuiComponent* comp : _components) {
        addPropertySubOwner(comp);
    }
    _spaceTime.setEnabled(true);

    {
        auto showHelpTextFunc = [this]() {
            for (gui::GuiComponent* comp : _components) {
                comp->setShowHelpTooltip(_showHelpText);
            }
        };
        showHelpTextFunc();
        _showHelpText.onChange(std::move(showHelpTextFunc));
        addProperty(_showHelpText);
    }

    {
        auto helpTextDelayFunc = [this]() {
            for (gui::GuiComponent* comp : _components) {
                comp->setShowHelpTooltipDelay(_helpTextDelay);
            }
        };
        helpTextDelayFunc();
        _helpTextDelay.onChange(std::move(helpTextDelayFunc));
        addProperty(_helpTextDelay);
    }

    global::callback::draw2D->emplace_back([&]() {
        ZoneScopedN("ImGUI")

        WindowDelegate& delegate = *global::windowDelegate;
        const bool showGui = delegate.hasGuiWindow() ? delegate.isGuiWindow() : true;
        if (delegate.isMaster() && showGui) {
            const glm::ivec2 windowSize = delegate.currentSubwindowSize();
            const glm::ivec2 resolution = delegate.currentDrawBufferResolution();

            if (windowSize.x <= 0 || windowSize.y <= 0) {
                return;
            }

            const double dt = std::max(delegate.averageDeltaTime(), 0.0);
            // We don't do any collection of immediate mode user interface, so it
            // is fine to open and close a frame immediately
            startFrame(
                static_cast<float>(dt),
                glm::vec2(windowSize),
                resolution / windowSize,
                _mousePosition,
                _mouseButtons
            );

            endFrame();
        }
    });

    global::callback::keyboard->emplace_back(
        [&](Key key, KeyModifier mod, KeyAction action) -> bool {
            ZoneScopedN("ImGUI")

            // A list of all the windows that can show up by themselves
            if (_isEnabled || _property.isEnabled()) {
                return keyCallback(key, mod, action);
            }
            else {
                return false;
            }
        }
    );

    global::callback::character->emplace_back(
        [&](unsigned int codepoint, KeyModifier modifier) -> bool {
            ZoneScopedN("ImGUI")

            // A list of all the windows that can show up by themselves
            if (_isEnabled || _property.isEnabled()) {
                return charCallback(codepoint, modifier);
            }
            else {
                return false;
            }
        }
    );

    global::callback::mousePosition->emplace_back(
        [&](double x, double y) {
            _mousePosition = glm::vec2(static_cast<float>(x), static_cast<float>(y));
        }
    );

    global::callback::mouseButton->emplace_back(
        [&](MouseButton button, MouseAction action, KeyModifier) -> bool {
            ZoneScopedN("ImGUI")

            if (action == MouseAction::Press) {
                _mouseButtons |= (1 << static_cast<int>(button));
            }
            else if (action == MouseAction::Release) {
                _mouseButtons &= ~(1 << static_cast<int>(button));
            }

            // A list of all the windows that can show up by themselves
            if (_isEnabled || _property.isEnabled()) {
                return mouseButtonCallback(button, action);
            }
            else {
                return false;
            }
        }
    );

    global::callback::mouseScrollWheel->emplace_back(
        [&](double, double posY) -> bool {
            ZoneScopedN("ImGUI")

            // A list of all the windows that can show up by themselves
            if (_isEnabled || _property.isEnabled()) {
                return mouseWheelCallback(posY);
            }
            else {
                return false;
            }
        }
    );

    global::callback::touchDetected->emplace_back(
        [&](TouchInput input) -> bool {
            return touchDetectedCallback(input);
        }
    );

    global::callback::touchUpdated->emplace_back(
        [&](TouchInput input) -> bool {
            return touchUpdatedCallback(input);
        }
    );

    global::callback::touchExit->emplace_back(
        [&](TouchInput input) {
            touchExitCallback(input);
        }
    );
}

void ImGUIModule::internalInitialize(const ghoul::Dictionary&) {
    LDEBUGC("ImGUIModule", "Initializing GUI");

    _property.setSource(
        []() -> std::vector<properties::PropertyOwner*> {
            return {
                global::renderEngine->scene(),
                global::screenSpaceRootPropertyOwner,
                global::moduleEngine,
                global::navigationHandler,
                global::sessionRecording,
                global::timeManager,
                global::renderEngine,
                global::parallelPeer,
                global::luaConsole,
                global::dashboard
            };
        }
    );

    _sceneProperty.setSource(
        []() {
            const Scene* scene = global::renderEngine->scene();
            const std::vector<SceneGraphNode*>& nodes = scene ?
                scene->allSceneGraphNodes() :
                std::vector<SceneGraphNode*>();

            return std::vector<properties::PropertyOwner*>(
                nodes.begin(),
                nodes.end()
                );
        }
    );
}

void ImGUIModule::internalDeinitialize() {
    for (ImGuiContext* ctx : _contexts) {
        ImGui::DestroyContext(ctx);
    }

    for (gui::GuiComponent* comp : _components) {
        comp->deinitialize();
    }

    delete iniFileBuffer;
}


void ImGUIModule::internalInitializeGL() {
    std::filesystem::path cachedFile = FileSys.cacheManager()->cachedFilename(
        configurationFile,
        ""
    );

    LDEBUG(fmt::format("Using {} as ImGUI cache location", cachedFile));

    iniFileBuffer = new char[cachedFile.string().size() + 1];

#ifdef WIN32
    strcpy_s(iniFileBuffer, cachedFile.string().size() + 1, cachedFile.string().c_str());
#else
    strcpy(iniFileBuffer, cachedFile.c_str());
#endif

    int nWindows = global::windowDelegate->nWindows();
    _contexts.resize(nWindows);

    for (int i = 0; i < nWindows; ++i) {
        _contexts[i] = ImGui::CreateContext();
        ImGui::SetCurrentContext(_contexts[i]);

        ImGuiIO& io = ImGui::GetIO();
        io.IniFilename = iniFileBuffer;
        io.DeltaTime = 1.f / 60.f;
        io.KeyMap[ImGuiKey_Tab] = static_cast<int>(Key::Tab);
        io.KeyMap[ImGuiKey_LeftArrow] = static_cast<int>(Key::Left);
        io.KeyMap[ImGuiKey_RightArrow] = static_cast<int>(Key::Right);
        io.KeyMap[ImGuiKey_UpArrow] = static_cast<int>(Key::Up);
        io.KeyMap[ImGuiKey_DownArrow] = static_cast<int>(Key::Down);
        io.KeyMap[ImGuiKey_Home] = static_cast<int>(Key::Home);
        io.KeyMap[ImGuiKey_End] = static_cast<int>(Key::End);
        io.KeyMap[ImGuiKey_Delete] = static_cast<int>(Key::Delete);
        io.KeyMap[ImGuiKey_Backspace] = static_cast<int>(Key::BackSpace);
        io.KeyMap[ImGuiKey_Enter] = static_cast<int>(Key::Enter);
        io.KeyMap[ImGuiKey_Escape] = static_cast<int>(Key::Escape);
        io.KeyMap[ImGuiKey_A] = static_cast<int>(Key::A);
        io.KeyMap[ImGuiKey_C] = static_cast<int>(Key::C);
        io.KeyMap[ImGuiKey_V] = static_cast<int>(Key::V);
        io.KeyMap[ImGuiKey_X] = static_cast<int>(Key::X);
        io.KeyMap[ImGuiKey_Y] = static_cast<int>(Key::Y);
        io.KeyMap[ImGuiKey_Z] = static_cast<int>(Key::Z);

        io.Fonts->AddFontFromFileTTF(absPath(GuiFont).string().c_str(), FontSize);
        captionFont = io.Fonts->AddFontFromFileTTF(
            absPath(GuiFont).string().c_str(),
            FontSize * 1.5f
        );

        ImGuiStyle& style = ImGui::GetStyle();
        style.WindowPadding = { 4.f, 4.f };
        style.WindowRounding = 0.f;
        style.FramePadding = { 3.f, 3.f };
        style.FrameRounding = 0.f;
        style.ItemSpacing = { 3.f, 2.f };
        style.ItemInnerSpacing = { 3.f, 2.f };
        style.TouchExtraPadding = { 1.f, 1.f };
        style.IndentSpacing = 15.f;
        style.ScrollbarSize = 10.f;
        style.ScrollbarRounding = 0.f;
        style.GrabMinSize = 10.f;
        style.GrabRounding = 16.f;

        style.Colors[ImGuiCol_Text] = ImVec4(0.90f, 0.90f, 0.90f, 1.00f);
        style.Colors[ImGuiCol_TextDisabled] = ImVec4(0.60f, 0.60f, 0.60f, 1.00f);
        style.Colors[ImGuiCol_WindowBg] = ImVec4(0.13f, 0.13f, 0.13f, 0.96f);
        style.Colors[ImGuiCol_ChildBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
        style.Colors[ImGuiCol_PopupBg] = ImVec4(0.05f, 0.05f, 0.10f, 0.90f);
        style.Colors[ImGuiCol_Border] = ImVec4(0.65f, 0.65f, 0.65f, 0.59f);
        style.Colors[ImGuiCol_BorderShadow] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
        style.Colors[ImGuiCol_FrameBg] = ImVec4(0.80f, 0.80f, 0.80f, 0.30f);
        style.Colors[ImGuiCol_FrameBgHovered] = ImVec4(0.91f, 0.94f, 0.99f, 0.40f);
        style.Colors[ImGuiCol_FrameBgActive] = ImVec4(0.90f, 0.90f, 0.90f, 0.45f);
        style.Colors[ImGuiCol_TitleBg] = ImVec4(0.71f, 0.81f, 1.00f, 0.45f);
        style.Colors[ImGuiCol_TitleBgCollapsed] = ImVec4(0.71f, 0.81f, 1.00f, 0.45f);
        style.Colors[ImGuiCol_TitleBgActive] = ImVec4(0.51f, 0.69f, 1.00f, 0.63f);
        style.Colors[ImGuiCol_MenuBarBg] = ImVec4(0.26f, 0.27f, 0.33f, 0.80f);
        style.Colors[ImGuiCol_ScrollbarBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
        style.Colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.40f, 0.75f, 0.80f, 0.43f);
        style.Colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.40f, 0.75f, 0.80f, 0.65f);
        style.Colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.40f, 0.75f, 0.80f, 0.65f);
        style.Colors[ImGuiCol_CheckMark] = ImVec4(1.00f, 1.00f, 1.00f, 0.50f);
        style.Colors[ImGuiCol_SliderGrab] = ImVec4(1.00f, 1.00f, 1.00f, 0.30f);
        style.Colors[ImGuiCol_SliderGrabActive] = ImVec4(0.50f, 0.80f, 0.76f, 1.00f);
        style.Colors[ImGuiCol_Button] = ImVec4(0.36f, 0.54f, 0.68f, 0.62f);
        style.Colors[ImGuiCol_ButtonHovered] = ImVec4(0.36f, 0.54f, 0.68f, 1.00f);
        style.Colors[ImGuiCol_ButtonActive] = ImVec4(0.36f, 0.61f, 0.81f, 1.00f);
        style.Colors[ImGuiCol_Header] = ImVec4(0.69f, 0.69f, 0.69f, 0.45f);
        style.Colors[ImGuiCol_HeaderHovered] = ImVec4(0.36f, 0.54f, 0.68f, 0.62f);
        style.Colors[ImGuiCol_HeaderActive] = ImVec4(0.53f, 0.63f, 0.87f, 0.80f);
        style.Colors[ImGuiCol_Separator] = ImVec4(0.50f, 0.50f, 0.50f, 1.00f);
        style.Colors[ImGuiCol_SeparatorHovered] = ImVec4(0.70f, 0.60f, 0.60f, 1.00f);
        style.Colors[ImGuiCol_SeparatorActive] = ImVec4(0.90f, 0.70f, 0.70f, 1.00f);
        style.Colors[ImGuiCol_ResizeGrip] = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
        style.Colors[ImGuiCol_ResizeGripHovered] = ImVec4(1.00f, 1.00f, 1.00f, 0.60f);
        style.Colors[ImGuiCol_ResizeGripActive] = ImVec4(1.00f, 1.00f, 1.00f, 0.90f);
        style.Colors[ImGuiCol_PlotLines] = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
        style.Colors[ImGuiCol_PlotLinesHovered] = ImVec4(0.90f, 0.70f, 0.00f, 1.00f);
        style.Colors[ImGuiCol_PlotHistogram] = ImVec4(0.90f, 0.70f, 0.00f, 1.00f);
        style.Colors[ImGuiCol_PlotHistogramHovered] = ImVec4(1.00f, 0.60f, 0.00f, 1.00f);
        style.Colors[ImGuiCol_TextSelectedBg] = ImVec4(0.44f, 0.63f, 1.00f, 0.35f);
        style.Colors[ImGuiCol_ModalWindowDimBg] = ImVec4(0.20f, 0.20f, 0.20f, 0.35f);
    }

    for (gui::GuiComponent* comp : _components) {
        comp->initialize();
    }

    _program = ghoul::opengl::ProgramObject::Build(
        "GUI",
        absPath("${MODULE_IMGUI}/shaders/gui_vs.glsl"),
        absPath("${MODULE_IMGUI}/shaders/gui_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    {
        unsigned char* texData;
        glm::ivec2 texSize = glm::ivec2(0);
        for (int i = 0; i < nWindows; ++i) {
            //_contexts[i] = ImGui::CreateContext();
            ImGui::SetCurrentContext(_contexts[i]);

            ImGui::GetIO().Fonts->GetTexDataAsRGBA32(&texData, &texSize.x, &texSize.y);
        }
        _fontTexture = std::make_unique<ghoul::opengl::Texture>(
            texData,
            glm::uvec3(texSize.x, texSize.y, 1),
            GL_TEXTURE_2D
            );
        _fontTexture->setName("Gui Text");
        _fontTexture->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
        _fontTexture->uploadTexture();
    }
    for (int i = 0; i < nWindows; ++i) {
        uintptr_t texture = static_cast<GLuint>(*_fontTexture);
        ImGui::SetCurrentContext(_contexts[i]);
        ImGui::GetIO().Fonts->TexID = reinterpret_cast<void*>(texture);
    }

    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, 0, nullptr, GL_DYNAMIC_DRAW);

    glGenBuffers(1, &vboElements);

    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);

    GLuint positionAttrib = _program->attributeLocation("in_position");
    GLuint uvAttrib = _program->attributeLocation("in_uv");
    GLuint colorAttrib = _program->attributeLocation("in_color");

    glEnableVertexAttribArray(positionAttrib);
    glVertexAttribPointer(
        positionAttrib,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(ImDrawVert),
        nullptr
    );
    glEnableVertexAttribArray(uvAttrib);
    glVertexAttribPointer(
        uvAttrib,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(ImDrawVert),
        reinterpret_cast<GLvoid*>(offsetof(ImDrawVert, uv)) // NOLINT
    );
    glEnableVertexAttribArray(colorAttrib);
    glVertexAttribPointer(
        colorAttrib,
        4,
        GL_UNSIGNED_BYTE,
        GL_TRUE,
        sizeof(ImDrawVert),
        reinterpret_cast<GLvoid*>(offsetof(ImDrawVert, col)) // NOLINT
    );
    glBindVertexArray(0);

    for (gui::GuiComponent* comp : _components) {
        comp->initializeGL();
    }
}

void ImGUIModule::internalDeinitializeGL() {
    _program = nullptr;
    _fontTexture = nullptr;

    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vbo);
    glDeleteBuffers(1, &vboElements);

    for (gui::GuiComponent* comp : _components) {
        comp->deinitializeGL();
    }
}

void ImGUIModule::startFrame(float deltaTime, const glm::vec2& windowSize,
    const glm::vec2& dpiScaling, const glm::vec2& mousePos,
    uint32_t mouseButtonsPressed)
{
    const int iWindow = global::windowDelegate->currentWindowId();
    ImGui::SetCurrentContext(_contexts[iWindow]);

    ImGuiIO& io = ImGui::GetIO();
    io.DisplaySize = ImVec2(windowSize.x, windowSize.y);
    io.DisplayFramebufferScale = ImVec2(dpiScaling.x, dpiScaling.y);
    io.DeltaTime = deltaTime;

    io.MousePos = ImVec2(mousePos.x, mousePos.y);

    io.MouseDown[0] = mouseButtonsPressed & (1 << 0);
    io.MouseDown[1] = mouseButtonsPressed & (1 << 1);

    ImGui::NewFrame();
}

void ImGUIModule::endFrame() {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }

    if (_isEnabled) {
        render();

        for (gui::GuiComponent* comp : _components) {
            if (comp->isEnabled()) {
                comp->render();
            }
        }
    }

    ImGui::Render();

    if (!_isEnabled) {
        return;
    }

    // Drawing
    ImDrawData* drawData = ImGui::GetDrawData();

    // Avoid rendering when minimized, scale coordinates for retina displays
    // (screen coordinates != framebuffer coordinates)
    ImGuiIO& io = ImGui::GetIO();
    GLsizei fb_width = static_cast<GLsizei>(
        io.DisplaySize.x * io.DisplayFramebufferScale.x
        );
    GLsizei fb_height = static_cast<GLsizei>(
        io.DisplaySize.y * io.DisplayFramebufferScale.y
        );
    if (fb_width == 0 || fb_height == 0) {
        return;
    }
    drawData->ScaleClipRects(io.DisplayFramebufferScale);

    // Setup render state:
    // alpha-blending enabled, no face culling, no depth testing, scissor enabled
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_SCISSOR_TEST);

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    _fontTexture->bind();

    // Setup orthographic projection matrix
    const float width = ImGui::GetIO().DisplaySize.x;
    const float height = ImGui::GetIO().DisplaySize.y;
    glViewport(0, 0, fb_width, fb_height);
    const glm::mat4 ortho(
        2.f / width, 0.0f, 0.0f, 0.f,
        0.0f, 2.0f / -height, 0.0f, 0.f,
        0.0f, 0.0f, -1.0f, 0.0f,
        -1.0f, 1.0f, 0.0f, 1.0f
    );
    _program->activate();

    _program->setUniform(_uniformCache.tex, unit);
    _program->setUniform(_uniformCache.ortho, ortho);

    glBindVertexArray(vao);

    for (int i = 0; i < drawData->CmdListsCount; ++i) {
        const ImDrawList* cmdList = drawData->CmdLists[i];
        const ImDrawIdx* indexBufferOffset = nullptr;

        glBindBuffer(GL_ARRAY_BUFFER, vbo);
        glBufferData(
            GL_ARRAY_BUFFER,
            cmdList->VtxBuffer.size() * sizeof(ImDrawVert),
            reinterpret_cast<const GLvoid*>(&cmdList->VtxBuffer.front()),
            GL_STREAM_DRAW
        );

        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboElements);
        glBufferData(
            GL_ELEMENT_ARRAY_BUFFER,
            cmdList->IdxBuffer.size() * sizeof(ImDrawIdx),
            reinterpret_cast<const GLvoid*>(&cmdList->IdxBuffer.front()),
            GL_STREAM_DRAW
        );

        for (const ImDrawCmd* pcmd = cmdList->CmdBuffer.begin();
            pcmd != cmdList->CmdBuffer.end();
            pcmd++)
        {
            if (pcmd->UserCallback) {
                pcmd->UserCallback(cmdList, pcmd);
            }
            else {
                glBindTexture(
                    GL_TEXTURE_2D,
                    static_cast<GLuint>(reinterpret_cast<intptr_t>(pcmd->TextureId))
                );
                glScissor(
                    static_cast<int>(pcmd->ClipRect.x),
                    static_cast<int>(fb_height - pcmd->ClipRect.w),
                    static_cast<int>(pcmd->ClipRect.z - pcmd->ClipRect.x),
                    static_cast<int>(pcmd->ClipRect.w - pcmd->ClipRect.y)
                );
                glDrawElements(
                    GL_TRIANGLES,
                    static_cast<GLsizei>(pcmd->ElemCount),
                    sizeof(ImDrawIdx) == 2 ? GL_UNSIGNED_SHORT : GL_UNSIGNED_INT,
                    indexBufferOffset
                );
            }
            indexBufferOffset += pcmd->ElemCount;
        }
    }

    glBindVertexArray(0);
    _program->deactivate();
    glDisable(GL_SCISSOR_TEST);
}

bool ImGUIModule::mouseButtonCallback(MouseButton, MouseAction) {
    ImGuiIO& io = ImGui::GetIO();
    bool consumeEvent = io.WantCaptureMouse;
    return consumeEvent;
}

bool ImGUIModule::mouseWheelCallback(double position) {
    ImGuiIO& io = ImGui::GetIO();
    bool consumeEvent = io.WantCaptureMouse;
    if (consumeEvent) {
        io.MouseWheel = static_cast<float>(position);
    }

    return consumeEvent;
}

bool ImGUIModule::keyCallback(Key key, KeyModifier modifier, KeyAction action) {
    const int keyIndex = static_cast<int>(key);
    if (keyIndex < 0) {
        return false;
    }

    const bool hasShift = hasKeyModifier(modifier, KeyModifier::Shift);
    const bool hasCtrl = hasKeyModifier(modifier, KeyModifier::Control);
    const bool hasAlt = hasKeyModifier(modifier, KeyModifier::Alt);

    ImGuiIO& io = ImGui::GetIO();

    const bool consumeEvent = io.WantCaptureKeyboard;
    if (consumeEvent) {
        if (action == KeyAction::Press) {
            io.KeysDown[keyIndex] = true;
        }
        io.KeyShift = hasShift;
        io.KeyCtrl = hasCtrl;
        io.KeyAlt = hasAlt;
    }

    // Even if the event is not consumed,
    // set keys and modifiers to false when they are released.
    if (action == KeyAction::Release) {
        io.KeysDown[keyIndex] = false;
    }
    if (!hasShift) {
        io.KeyShift = false;
    }
    if (!hasCtrl) {
        io.KeyCtrl = false;
    }
    if (!hasAlt) {
        io.KeyAlt = false;
    }

    return consumeEvent;
}

bool ImGUIModule::charCallback(unsigned int character, KeyModifier) {
    ImGuiIO& io = ImGui::GetIO();
    bool consumeEvent = io.WantCaptureKeyboard;

    if (consumeEvent) {
        io.AddInputCharacter(static_cast<unsigned short>(character));
    }

    return consumeEvent;
}

bool ImGUIModule::touchDetectedCallback(TouchInput input) {
    ImGuiIO& io = ImGui::GetIO();
    const glm::vec2 windowPos = input.currentWindowCoordinates();
    const bool consumeEvent = io.WantCaptureMouse;

    if (!consumeEvent) {
        return false;
    }
    if (_validTouchStates.empty()) {
        io.MousePos = { windowPos.x, windowPos.y };
        io.MouseClicked[0] = true;
    }
    _validTouchStates.push_back(input);
    return true;
}

bool ImGUIModule::touchUpdatedCallback(TouchInput input) {
    if (_validTouchStates.empty()) {
        return false;
    }
    ImGuiIO& io = ImGui::GetIO();

    auto it = std::find_if(
        _validTouchStates.cbegin(),
        _validTouchStates.cend(),
        [&](const TouchInput& state) {
            return state.fingerId == input.fingerId &&
                state.touchDeviceId == input.touchDeviceId;
        }
    );

    if (it == _validTouchStates.cbegin()) {
        glm::vec2 windowPos = input.currentWindowCoordinates();
        io.MousePos = { windowPos.x, windowPos.y };
        io.MouseClicked[0] = true;
        return true;
    }
    else if (it != _validTouchStates.cend()) {
        return true;
    }
    return false;
}

void ImGUIModule::touchExitCallback(TouchInput input) {
    if (_validTouchStates.empty()) {
        return;
    }

    const auto found = std::find_if(
        _validTouchStates.cbegin(),
        _validTouchStates.cend(),
        [&](const TouchInput& state) {
            return state.fingerId == input.fingerId &&
                state.touchDeviceId == input.touchDeviceId;
        }
    );

    if (found == _validTouchStates.cend()) {
        return;
    }

    ImGuiIO& io = ImGui::GetIO();
    _validTouchStates.erase(found);
    if (_validTouchStates.empty()) {
        glm::vec2 windowPos = input.currentWindowCoordinates();
        io.MousePos = { windowPos.x, windowPos.y };
        io.MouseClicked[0] = false;
    }
}


void ImGUIModule::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);

    ImGui::Begin("OpenSpace GUI", nullptr);

    _isCollapsed = ImGui::IsWindowCollapsed();

    for (gui::GuiComponent* comp : _components) {
        bool enabled = comp->isEnabled();
        ImGui::Checkbox(comp->guiName().c_str(), &enabled);
        comp->setEnabled(enabled);
    }

    renderAndUpdatePropertyVisibility();

    static const int addImageBufferSize = 256;
    static char identifierBuffer[addImageBufferSize];
    static char addImageLocalBuffer[addImageBufferSize];
    static char addImageOnlineBuffer[addImageBufferSize];

    ImGui::InputText(
        "Identifier for Local/Online Images",
        identifierBuffer,
        addImageBufferSize
    );

    bool addImageLocal = ImGui::InputText(
        "Add Local Image",
        addImageLocalBuffer,
        addImageBufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );
    if (addImageLocal) {
        addScreenSpaceRenderableLocal(
            std::string(identifierBuffer),
            std::string(addImageLocalBuffer)
        );
    }

    bool addImageOnline = ImGui::InputText(
        "Add Online Image",
        addImageOnlineBuffer,
        addImageBufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );

    if (addImageOnline) {
        addScreenSpaceRenderableOnline(
            std::string(identifierBuffer),
            std::string(addImageOnlineBuffer)
        );
    }

    bool addDashboard = ImGui::Button("Add New Dashboard");
    if (addDashboard) {
        global::scriptEngine->queueScript(
            "openspace.addScreenSpaceRenderable({ Type = 'ScreenSpaceDashboard' });",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    bool addDashboardCopy = ImGui::Button("Add Copy of Main Dashboard");
    if (addDashboardCopy) {
        global::scriptEngine->queueScript(
            "openspace.addScreenSpaceRenderable({ "
            "Type = 'ScreenSpaceDashboard', UseMainDashboard = true "
            "});",
            openspace::scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

#ifdef SHOW_IMGUI_HELPERS
    ImGui::Checkbox("ImGUI Internals", &_showInternals);
    if (_showInternals) {
        ImGui::Begin("Style Editor");
        ImGui::ShowStyleEditor();
        ImGui::End();

        ImGui::Begin("Test Window");
        ImGui::ShowDemoWindow();
        ImGui::End();

        ImGui::Begin("Metrics Window");
        ImGui::ShowMetricsWindow();
        ImGui::End();
    }
#endif

    ImGui::End();
}

void ImGUIModule::renderAndUpdatePropertyVisibility() {
    // Fragile! Keep this in sync with properties::Property::Visibility
    using V = properties::Property::Visibility;
    int t = static_cast<std::underlying_type_t<V>>(_currentVisibility);

    // Array is sorted by importance
    std::array<const char*, 4> items = { "User", "Developer", "Hidden", "All" };
    ImGui::Combo("PropertyVisibility", &t, items.data(), static_cast<int>(items.size()));

    _currentVisibility = static_cast<V>(t);
    _property.setVisibility(_currentVisibility);
}

} // namespace openspace
