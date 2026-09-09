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

#include <modules/exoplanetsexperttool/gui.h>

#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <implot.h>
#include <string_view>

namespace {
    constexpr std::string_view GuiFont = "${FONTS}/arimo/Arimo-Regular.ttf";
    constexpr float FontSize = 16.f;

    ImGuiKey toImGuiKey(openspace::Key key) {
        using openspace::Key;
        switch (key) {
            case Key::Tab: return ImGuiKey_Tab;
            case Key::Left: return ImGuiKey_LeftArrow;
            case Key::Right: return ImGuiKey_RightArrow;
            case Key::Up: return ImGuiKey_UpArrow;
            case Key::Down: return ImGuiKey_DownArrow;
            case Key::Home: return ImGuiKey_Home;
            case Key::End: return ImGuiKey_End;
            case Key::Delete: return ImGuiKey_Delete;
            case Key::BackSpace: return ImGuiKey_Backspace;
            case Key::Enter: return ImGuiKey_Enter;
            case Key::Escape: return ImGuiKey_Escape;
            case Key::A: return ImGuiKey_A;
            case Key::C: return ImGuiKey_C;
            case Key::V: return ImGuiKey_V;
            case Key::X: return ImGuiKey_X;
            case Key::Y: return ImGuiKey_Y;
            case Key::Z: return ImGuiKey_Z;
            default: return ImGuiKey_None;
        }
    }
} // namespace

namespace openspace::exoplanets::gui {

Gui::Gui(std::string identifier, std::string guiName)
    : PropertyOwner({ std::move(identifier), std::move(guiName) })
    , _dataViewer("DataViewer")
{
    addPropertySubOwner(_dataViewer);
}

Gui::~Gui() {} // NOLINT

void Gui::initialize() {}

void Gui::deinitialize() {
    for (const Context& ctx : _contexts) {
        destroyContext(ctx);
    }
}

void Gui::initializeGL() {
    size_t nWindows = global::windowDelegate->nWindows();
    _contexts.resize(nWindows);

    // TODO: maybe set up .ini file?

    for (size_t i = 0; i < nWindows; ++i) {
        _contexts[i] = createContext();
        setCurrectContext(_contexts[i]);

        ImGuiIO& io = ImGui::GetIO();
        io.DeltaTime = 1.f / 60.f;

        io.Fonts->AddFontFromFileTTF(absPath(GuiFont).string().c_str(), FontSize);

        //ImGuiStyle& style = ImGui::GetStyle();
        // TODO: can set style by altering this value
    }

    _program = ghoul::opengl::ProgramObject::Build(
        "ExoToolGui",
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/gui_vs.glsl"),
        absPath("${MODULE_EXOPLANETSEXPERTTOOL}/shaders/gui_fs.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache);

    ImGuiIO& io = ImGui::GetIO();

    io.ConfigFlags |= ImGuiConfigFlags_NavEnableKeyboard;

    {
        unsigned char* texData = nullptr;
        glm::ivec2 texSize = glm::ivec2(0);
        for (int i = 0; i < nWindows; ++i) {
            setCurrectContext(_contexts[i]);

            io.Fonts->GetTexDataAsRGBA32(&texData, &texSize.x, &texSize.y);
        }

        _fontTexture = std::make_unique<ghoul::opengl::Texture>(
            ghoul::opengl::Texture::FormatInit{
                .dimensions = glm::uvec3(texSize.x, texSize.y, 1),
                .type = GL_TEXTURE_2D,
                .format = ghoul::opengl::Texture::Format::RGBA,
                .dataType = GL_UNSIGNED_BYTE
            },
            ghoul::opengl::Texture::SamplerInit{},
            reinterpret_cast<std::byte*>(texData)
        );
        _fontTexture->setName("ExoGui Text");
    }
    for (int i = 0; i < nWindows; ++i) {
        uintptr_t texture = static_cast<GLuint>(*_fontTexture);
        setCurrectContext(_contexts[i]);
        io.Fonts->TexID = reinterpret_cast<void*>(texture);
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

    // The dataviewer's initialization requires the imgui/impot contexts,
    // so do it afterc onstexts have been created
    _dataViewer.initializeGL();
}

void Gui::deinitializeGL() {
    _program = nullptr;
    _fontTexture = nullptr;

    glDeleteVertexArrays(1, &vao);
    glDeleteBuffers(1, &vbo);
    glDeleteBuffers(1, &vboElements);
}

void Gui::initializeDataset() {
    _dataViewer.initializeData();
}

void Gui::startFrame(float deltaTime, const glm::vec2& windowSize,
                     const glm::vec2& dpiScaling, const glm::vec2& mousePos,
                     uint32_t mouseButtonsPressed)
{
    const int iWindow = global::windowDelegate->currentWindowId();
    ImGui::SetCurrentContext(_contexts[iWindow].imgui);
    ImPlot::SetCurrentContext(_contexts[iWindow].implot);

    ImGuiIO& io = ImGui::GetIO();
    io.DisplaySize = ImVec2(windowSize.x, windowSize.y);
    io.DisplayFramebufferScale = ImVec2(dpiScaling.x, dpiScaling.y);
    io.DeltaTime = deltaTime;

    io.MousePos = ImVec2(mousePos.x, mousePos.y);

    io.MouseDown[0] = mouseButtonsPressed & (1 << 0);
    io.MouseDown[1] = mouseButtonsPressed & (1 << 1);

    ImGui::NewFrame();
}

void Gui::endFrame() {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
    }

    render();

    ImGui::Render();

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
    unit.bind(*_fontTexture);

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
                    static_cast<GLuint>(pcmd->GetTexID())
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

bool Gui::mouseButtonCallback(MouseButton, MouseAction) {
    ImGuiIO& io = ImGui::GetIO();
    bool consumeEvent = io.WantCaptureMouse;
    return consumeEvent;
}

bool Gui::mouseWheelCallback(double position) {
    ImGuiIO& io = ImGui::GetIO();
    const bool consumeEvent = io.WantCaptureMouse;
    if (consumeEvent) {
        io.AddMouseWheelEvent(0.f, static_cast<float>(position));
    }
    return consumeEvent;
}

bool Gui::keyCallback(Key key, KeyModifier modifier, KeyAction action) {
    const int keyIndex = static_cast<int>(key);
    if (keyIndex < 0) {
        return false;
    }

    ImGuiIO& io = ImGui::GetIO();

    // Always update the modifier and keys, even if event is not consumed, so keys and
    // modifiers are set to false on release

    if (const ImGuiKey k = toImGuiKey(key); k != ImGuiKey_None) {
        const bool isDown = (action != KeyAction::Release);
        io.AddKeyEvent(k, isDown);
    }

    io.AddKeyEvent(ImGuiMod_Shift, hasKeyModifier(modifier, KeyModifier::Shift));
    io.AddKeyEvent(ImGuiMod_Ctrl, hasKeyModifier(modifier, KeyModifier::Control));
    io.AddKeyEvent(ImGuiMod_Alt, hasKeyModifier(modifier, KeyModifier::Alt));
    io.AddKeyEvent(ImGuiMod_Super, hasKeyModifier(modifier, KeyModifier::Super));

    return io.WantCaptureKeyboard;
}

bool Gui::charCallback(unsigned int character, KeyModifier) {
    ImGuiIO& io = ImGui::GetIO();
    const bool consumeEvent = io.WantCaptureKeyboard;
    if (consumeEvent) {
        io.AddInputCharacter(static_cast<unsigned short>(character));
    }
    return consumeEvent;
}

void Gui::render() {
    _dataViewer.render();
}

Gui::Context Gui::createContext() {
    Context c;
    c.imgui = ImGui::CreateContext();
    c.implot = ImPlot::CreateContext();
    return std::move(c);
}

void Gui::destroyContext(const Context& ctx) {
    ImPlot::DestroyContext(ctx.implot);
    ImGui::DestroyContext(ctx.imgui);
}

void Gui::setCurrectContext(const Context& ctx) {
    ImGui::SetCurrentContext(ctx.imgui);
    ImPlot::SetCurrentContext(ctx.implot);
}

} // namespace openspace::gui
