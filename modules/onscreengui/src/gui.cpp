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

#include <modules/onscreengui/include/gui.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/properties/property.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/util/keys.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <imgui.h>

#include "gui_lua.inl"

namespace {

const std::string _loggerCat = "GUI";
const std::string configurationFile = "imgui.ini";
const ImVec2 size = ImVec2(350, 500);

//GLuint fontTex = 0;
GLint positionLocation = 0;
GLint uvLocation = 0;
GLint colorLocation = 0;
// A VBO max size of 0 will cause a lazy instantiation of the buffer
size_t vboMaxSize = 0;
GLuint vao = 0;
GLuint vbo = 0;
std::unique_ptr<ghoul::opengl::ProgramObject> _program;
std::unique_ptr<ghoul::opengl::Texture> _fontTexture;
char* iniFileBuffer = nullptr;

static void RenderDrawLists(ImDrawList** const commandLists, int nCommandLists) {
    if (nCommandLists == 0)
        return;

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
    const glm::mat4 ortho(
        2.f / width, 0.0f, 0.0f, 0.f,
        0.0f, 2.0f / -height, 0.0f, 0.f,
        0.0f, 0.0f, -1.0f, 0.0f,
        -1.0f, 1.0f, 0.0f, 1.0f
    );
    _program->activate();

    _program->setUniform("tex", unit);
    _program->setUniform("ortho", ortho);

    // Grow our buffer according to what we need
    size_t totalVertexCount = 0;
    for (int i = 0; i < nCommandLists; ++i)
        totalVertexCount += commandLists[i]->vtx_buffer.size();

    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    size_t neededBufferSize = totalVertexCount * sizeof(ImDrawVert);
    if (neededBufferSize > vboMaxSize) {
        // Grow buffer
        vboMaxSize = neededBufferSize * 1.25f;
        glBufferData(GL_ARRAY_BUFFER, vboMaxSize, NULL, GL_STREAM_DRAW);
    }

    // Copy and convert all vertices into a single contiguous buffer
    unsigned char* bufferData = reinterpret_cast<unsigned char*>(
        glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY)
        );

    if (!bufferData) {
        LFATAL("Error mapping ImGui buffer");
        return;
    }

    for (int i = 0; i < nCommandLists; ++i) {
        const ImDrawList* cmd_list = commandLists[i];
        memcpy(
            bufferData,
            &cmd_list->vtx_buffer[0],
            cmd_list->vtx_buffer.size() * sizeof(ImDrawVert)
        );
        bufferData += (cmd_list->vtx_buffer.size() * sizeof(ImDrawVert));
    }
    glUnmapBuffer(GL_ARRAY_BUFFER);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(vao);

    int cmdOffset = 0;
    for (int i = 0; i < nCommandLists; ++i) {
        const ImDrawList* cmd_list = commandLists[i];
        int vtxOffset = cmdOffset;
        for (const auto& pcmd : cmd_list->commands) {
            glScissor(
                static_cast<int>(pcmd.clip_rect.x),
                static_cast<int>(height - pcmd.clip_rect.w),
                static_cast<int>(pcmd.clip_rect.z - pcmd.clip_rect.x),
                static_cast<int>(pcmd.clip_rect.w - pcmd.clip_rect.y)
            );
            glDrawArrays(GL_TRIANGLES, vtxOffset, pcmd.vtx_count);
            vtxOffset += pcmd.vtx_count;
        }
        cmdOffset = vtxOffset;
    }

    glBindVertexArray(0);
    _program->deactivate();
    glDisable(GL_SCISSOR_TEST);
}


void addScreenSpaceRenderable(std::string texturePath) {
    if (!FileSys.fileExists(texturePath)) {
        LWARNING("Could not find image '" << texturePath << "'");
        return;
    }
    std::string luaTable =
        "{Type = 'ScreenSpaceImage', TexturePath = '+" + absPath(texturePath) + " ' }";
    std::string script = "openspace.registerScreenSpaceRenderable(" + luaTable + ");";
    OsEng.scriptEngine().queueScript(script);
}
} // namespace 

namespace openspace {
namespace gui {

GUI::GUI() 
    : GuiComponent()
    , _showHelp(false)
{}

GUI::~GUI() {
    ImGui::Shutdown();
}

void GUI::initialize() {
    std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        configurationFile, "", ghoul::filesystem::CacheManager::Persistent::Yes
    );

    iniFileBuffer = new char[cachedFile.size() + 1];

#ifdef WIN32
    strcpy_s(iniFileBuffer, cachedFile.size() + 1, cachedFile.c_str());
#else
    strcpy(iniFileBuffer, cachedFile.c_str());
#endif

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

    io.RenderDrawListsFn = RenderDrawLists;
    //io.SetClipboardTextFn = SetClipboardTextFn_DefaultImpl; // @TODO implement? ---abock
    //io.GetClipboardTextFn = GetClipboardTextFn_DefaultImpl; // @TODO implement? ---abock
    io.Fonts->AddFontFromFileTTF(
        absPath("${FONTS}/Roboto/Roboto-Regular.ttf").c_str(),
        16.f
    );

    ImGuiStyle& style = ImGui::GetStyle();
    style.WindowPadding = { 4.f, 4.f };
    style.WindowRounding = 0.f;
    style.FramePadding = { 3.f, 3.f };
    style.FrameRounding = 3.f;
    style.ScrollbarWidth = 15.f;
    style.ScrollbarRounding = 0.f;

    style.Colors[ImGuiCol_WindowBg] = ImVec4(0.0f, 0.21f, 0.24f, 1.0f);
    style.Colors[ImGuiCol_Border] = ImVec4(0.1f, 0.39f, 0.42f, 0.59f);
    style.Colors[ImGuiCol_BorderShadow] = ImVec4(0.0f, 0.0f, 0.0f, 0.0f);
    style.Colors[ImGuiCol_TitleBg] = ImVec4(0.5f, 0.94f, 1.0f, 0.45f);
    style.Colors[ImGuiCol_TitleBgCollapsed] = ImVec4(0.5f, 0.94f, 1.0f, 0.45f);
    style.Colors[ImGuiCol_ScrollbarBg] = ImVec4(0.0f, 0.0f, 0.0f, 0.0f);
    style.Colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.12f, 0.71f, 0.8f, 0.43f);
    style.Colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.4f, 0.75f, 0.8f, 0.65f);
    style.Colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.4f, 0.75f, 0.8f, 0.65f);
    style.Colors[ImGuiCol_SliderGrabActive] = ImVec4(0.5f, 0.8f, 0.76f, 1.0f);
    style.Colors[ImGuiCol_Button] = ImVec4(0.0f, 0.36f, 0.67f, 0.6f);
    style.Colors[ImGuiCol_ButtonHovered] = ImVec4(0.0f, 0.51f, 0.94f, 1.0f);
    style.Colors[ImGuiCol_ButtonActive] = ImVec4(0.0f, 0.43f, 0.8f, 1.0f);
    style.Colors[ImGuiCol_Header] = ImVec4(0.34f, 0.5f, 0.9f, 0.45f);
    style.Colors[ImGuiCol_HeaderHovered] = ImVec4(0.34f, 0.5f, 0.9f, 0.8f);
    style.Colors[ImGuiCol_HeaderActive] = ImVec4(0.53f, 0.63f, 0.87f, 0.8f);
    style.Colors[ImGuiCol_ResizeGrip] = ImVec4(1.0f, 1.0f, 1.0f, 1.0f);
    style.Colors[ImGuiCol_CloseButton] = ImVec4(0.75f, 0.75f, 0.75f, 1.0f);
    style.Colors[ImGuiCol_CloseButtonHovered] = ImVec4(0.52f, 0.52f, 0.52f, 0.6f);
    style.Colors[ImGuiCol_CloseButtonActive] = ImVec4(0.52f, 0.52f, 0.52f, 1.0f);

    _property.initialize();
    _screenSpaceProperty.initialize();
    _globalProperty.initialize();
    _performance.initialize();
    _help.initialize();
    _iswa.initialize();

}

void GUI::deinitialize() {
    delete iniFileBuffer;
}

void GUI::initializeGL() {
    _program = ghoul::opengl::ProgramObject::Build(
        "GUI",
        "${MODULE_ONSCREENGUI}/shaders/gui_vs.glsl",
        "${MODULE_ONSCREENGUI}/shaders/gui_fs.glsl"
    );
    if (!_program)
        return;

    positionLocation = glGetAttribLocation(*_program, "in_position");
    uvLocation = glGetAttribLocation(*_program, "in_uv");
    colorLocation = glGetAttribLocation(*_program, "in_color");

    unsigned char* png_data;
    int tex_x, tex_y;
    ImGui::GetIO().Fonts->GetTexDataAsRGBA32(&png_data, &tex_x, &tex_y);

    _fontTexture = std::make_unique<ghoul::opengl::Texture>(
        png_data,
        glm::uvec3(tex_x, tex_y, 1)
    );
    _fontTexture->setName("Gui Text");
    _fontTexture->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
    _fontTexture->uploadTexture();

    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, vboMaxSize, NULL, GL_DYNAMIC_DRAW);
    

    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glEnableVertexAttribArray(positionLocation);
    glEnableVertexAttribArray(uvLocation);
    glEnableVertexAttribArray(colorLocation);

    glVertexAttribPointer(
        positionLocation,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(ImDrawVert),
        reinterpret_cast<GLvoid*>(offsetof(ImDrawVert, pos))
    );
    glVertexAttribPointer(
        uvLocation,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(ImDrawVert),
        reinterpret_cast<GLvoid*>(offsetof(ImDrawVert, uv))
    );
    glVertexAttribPointer(
        colorLocation,
        4,
        GL_UNSIGNED_BYTE,
        GL_TRUE,
        sizeof(ImDrawVert),
        reinterpret_cast<GLvoid*>(offsetof(ImDrawVert, col))
    );
    glBindVertexArray(0);
    glBindBuffer(GL_ARRAY_BUFFER, 0);

    _property.initializeGL();
    _screenSpaceProperty.initializeGL();
    _globalProperty.initializeGL();
    _performance.initializeGL();
    _help.initializeGL();
    _iswa.initializeGL();
}

void GUI::deinitializeGL() {
    _program = nullptr;

    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vbo);

    _property.deinitializeGL();
    _screenSpaceProperty.deinitializeGL();
    _globalProperty.deinitializeGL();
    _performance.deinitializeGL();
    _help.deinitializeGL();
    _iswa.deinitializeGL();
}

void GUI::startFrame(float deltaTime, const glm::vec2& windowSize,
                     const glm::vec2& mousePosCorrectionFactor,
                     const glm::vec2& mousePos,
                     uint32_t mouseButtonsPressed)
{
    ImGuiIO& io = ImGui::GetIO();
    io.DisplaySize = ImVec2(windowSize.x, windowSize.y);
    io.DeltaTime = deltaTime;

    io.MousePos = ImVec2(
        mousePos.x * mousePosCorrectionFactor.x,
        mousePos.y * mousePosCorrectionFactor.y
    );

    io.MouseDown[0] = mouseButtonsPressed & (1 << 0);
    io.MouseDown[1] = mouseButtonsPressed & (1 << 1);

    ImGui::NewFrame();
}

void GUI::endFrame() {
    render();

    if (_property.isEnabled())
        _property.render();
    if (_screenSpaceProperty.isEnabled())
        _screenSpaceProperty.render();
    if (_globalProperty.isEnabled())
        _globalProperty.render();
    if (_performance.isEnabled())
        _performance.render();
    if (_help.isEnabled())
        _help.render();
    if (_iswa.isEnabled())
        _iswa.render();

    ImGui::Render();
}

bool GUI::mouseButtonCallback(MouseButton button, MouseAction action) {
    ImGuiIO& io = ImGui::GetIO();
    bool consumeEvent = io.WantCaptureMouse;
    return consumeEvent;
}

bool GUI::mouseWheelCallback(double position) {
    ImGuiIO& io = ImGui::GetIO();
    bool consumeEvent = io.WantCaptureMouse;
    if (consumeEvent) {
        io.MouseWheel = static_cast<float>(position);
    }

    return consumeEvent;
}

bool GUI::keyCallback(Key key, KeyModifier modifier, KeyAction action) {
    ImGuiIO& io = ImGui::GetIO();
    bool consumeEvent = io.WantCaptureKeyboard;
    if (consumeEvent) {
        int keyIndex = static_cast<int>(key);
        if (keyIndex < 0)
            LERROR("Pressed key of index '" << keyIndex << "' was negative");
        else {
            if (action == KeyAction::Press)
                io.KeysDown[keyIndex] = true;
            if (action == KeyAction::Release)
                io.KeysDown[keyIndex] = false;
        }

        io.KeyShift = hasKeyModifier(modifier, KeyModifier::Shift);
        io.KeyCtrl = hasKeyModifier(modifier, KeyModifier::Control);
        io.KeyAlt = hasKeyModifier(modifier, KeyModifier::Alt);
    }
    return consumeEvent;
}

bool GUI::charCallback(unsigned int character, KeyModifier modifier) {
    ImGuiIO& io = ImGui::GetIO();
    bool consumeEvent = io.WantCaptureKeyboard;

    if (consumeEvent)
        io.AddInputCharacter((unsigned short)character);

    return consumeEvent;
}

void GUI::render() {
    ImGui::Begin("OpenSpace GUI", nullptr);

    ImGui::Checkbox("Scene Graph Properties", &_property._isEnabled);
    ImGui::Checkbox("ScreenSpace Properties", &_screenSpaceProperty._isEnabled);
    ImGui::Checkbox("Global Properties", &_globalProperty._isEnabled);
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    ImGui::Checkbox("iSWA", &_iswa._isEnabled);
#endif
    ImGui::Checkbox("Performance", &_performance._isEnabled);
    _origin.render();
    _time.render();

    // These are temporary until the scalegraph is in effect ---abock
    bool toSun = ImGui::Button("Coordinate System to Sun");
    bool toPluto = ImGui::Button("Coordinate System to Pluto");
    bool toJupiter = ImGui::Button("Coordinate System to Jupiter");
    bool to67P = ImGui::Button("Coordinate System to 67P");

    if (toSun) {
        OsEng.scriptEngine().queueScript(
            "openspace.setPropertyValue('Interaction.coordinateSystem', 'Sun');"
        );
    }
    if (toPluto) {
        OsEng.scriptEngine().queueScript(
            "openspace.setPropertyValue('Interaction.coordinateSystem', 'Pluto');"
         );
    }
    if (toJupiter) {
        OsEng.scriptEngine().queueScript(
            "openspace.setPropertyValue('Interaction.coordinateSystem', 'Jupiter');"
        );
    }
    if (to67P) {
        OsEng.scriptEngine().queueScript(
            "openspace.setPropertyValue('Interaction.coordinateSystem', '67P');"
        );
    }

    ImGui::Checkbox("Help", &_help._isEnabled);

    static const int addImageBufferSize = 256;
    static char addImageBuffer[addImageBufferSize];
    ImGui::InputText("addImage", addImageBuffer, addImageBufferSize);

    if (ImGui::SmallButton("Add Image")){
        addScreenSpaceRenderable(std::string(addImageBuffer));
    }

#if 0
    ImGui::Begin("Style Editor");
    ImGui::ShowStyleEditor();
    ImGui::End();
#endif

    ImGui::End();
}
    
scripting::ScriptEngine::LuaLibrary GUI::luaLibrary() {
    return {
        "gui",
        {
            {
                "show",
                &luascriptfunctions::gui::show,
                "",
                "Shows the console"
            },
            {
                "hide",
                &luascriptfunctions::gui::hide,
                "",
                "Hides the console"
            },
            {
                "toggle",
                &luascriptfunctions::gui::toggle,
                "",
                "Toggles the console"
            }
        }
    };
}

} // namespace gui
} // namespace openspace
