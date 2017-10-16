/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/imgui/include/gui.h>

#include <modules/imgui/imguimodule.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/keys.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>

#include <modules/imgui/include/imgui_include.h>

#include "gui_lua.inl"

//#define SHOW_IMGUI_HELPERS

namespace {

const char* _loggerCat = "GUI";
const char* configurationFile = "imgui.ini";
//const char* GuiFont = "${FONTS}/ubuntu/Ubuntu-Regular.ttf";
const char* GuiFont = "${FONTS}/arimo/Arimo-Regular.ttf";
const float FontSize = 14.f;
const ImVec2 size = ImVec2(350, 500);

//GLuint fontTex = 0;
// A VBO max size of 0 will cause a lazy instantiation of the buffer
size_t vboMaxSize = 0;
GLuint vao = 0;
GLuint vbo = 0;
GLuint vboElements = 0;
std::unique_ptr<ghoul::opengl::ProgramObject> _program;
std::unique_ptr<ghoul::opengl::Texture> _fontTexture;
char* iniFileBuffer = nullptr;

static void RenderDrawLists(ImDrawData* drawData) {
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

    _program->setUniform("tex", unit);
    _program->setUniform("ortho", ortho);

    glBindVertexArray(vao);

    for (int i = 0; i < drawData->CmdListsCount; ++i) {
        const ImDrawList* cmdList = drawData->CmdLists[i];
        const ImDrawIdx* indexBufferOffset = nullptr;

        glBindBuffer(GL_ARRAY_BUFFER, vbo);
        glBufferData(
            GL_ARRAY_BUFFER,
            static_cast<GLsizeiptr>(cmdList->VtxBuffer.size() * sizeof(ImDrawVert)),
            reinterpret_cast<const GLvoid*>(&cmdList->VtxBuffer.front()),
            GL_STREAM_DRAW
        );

        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, vboElements);
        glBufferData(
            GL_ELEMENT_ARRAY_BUFFER,
            static_cast<GLsizeiptr>(cmdList->IdxBuffer.size() * sizeof(ImDrawIdx)),
            reinterpret_cast<const GLvoid*>(&cmdList->IdxBuffer.front()),
            GL_STREAM_DRAW
        );

        for (const ImDrawCmd* pcmd = cmdList->CmdBuffer.begin(); pcmd != cmdList->CmdBuffer.end(); pcmd++) {
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


    //// Grow our buffer according to what we need
    //size_t totalVertexCount = 0;
    //for (int i = 0; i < nCommandLists; ++i)
    //    totalVertexCount += commandLists[i]->vtx_buffer.size();

    //glBindBuffer(GL_ARRAY_BUFFER, vbo);
    //size_t neededBufferSize = totalVertexCount * sizeof(ImDrawVert);
    //if (neededBufferSize > vboMaxSize) {
    //    // Grow buffer
    //    vboMaxSize = neededBufferSize * 1.25f;
    //    glBufferData(GL_ARRAY_BUFFER, vboMaxSize, NULL, GL_STREAM_DRAW);
    //}

    //// Copy and convert all vertices into a single contiguous buffer
    //unsigned char* bufferData = reinterpret_cast<unsigned char*>(
    //    glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY)
    //    );

    //if (!bufferData) {
    //    LFATAL("Error mapping ImGui buffer");
    //    return;
    //}

    //for (int i = 0; i < nCommandLists; ++i) {
    //    const ImDrawList* cmd_list = commandLists[i];
    //    memcpy(
    //        bufferData,
    //        &cmd_list->vtx_buffer[0],
    //        cmd_list->vtx_buffer.size() * sizeof(ImDrawVert)
    //    );
    //    bufferData += (cmd_list->vtx_buffer.size() * sizeof(ImDrawVert));
    //}
    //glUnmapBuffer(GL_ARRAY_BUFFER);
    //glBindBuffer(GL_ARRAY_BUFFER, 0);
    //glBindVertexArray(vao);

    //int cmdOffset = 0;
    //for (int i = 0; i < nCommandLists; ++i) {
    //    const ImDrawList* cmd_list = commandLists[i];
    //    int vtxOffset = cmdOffset;
    //    for (const auto& pcmd : cmd_list->commands) {
    //        glScissor(
    //            static_cast<int>(pcmd.clip_rect.x),
    //            static_cast<int>(height - pcmd.clip_rect.w),
    //            static_cast<int>(pcmd.clip_rect.z - pcmd.clip_rect.x),
    //            static_cast<int>(pcmd.clip_rect.w - pcmd.clip_rect.y)
    //        );
    //        glDrawArrays(GL_TRIANGLES, vtxOffset, pcmd.vtx_count);
    //        vtxOffset += pcmd.vtx_count;
    //    }
    //    cmdOffset = vtxOffset;
    //}

    glBindVertexArray(0);
    _program->deactivate();
    glDisable(GL_SCISSOR_TEST);
}


void addScreenSpaceRenderableLocal(std::string texturePath) {
    if (!FileSys.fileExists(texturePath)) {
        LWARNING("Could not find image '" << texturePath << "'");
        return;
    }

    const std::string luaTable =
        "{Type = 'ScreenSpaceImageLocal', TexturePath = openspace.absPath('" + texturePath + "') }";
    const std::string script = "openspace.registerScreenSpaceRenderable(" + luaTable + ");";
    OsEng.scriptEngine().queueScript(script, openspace::scripting::ScriptEngine::RemoteScripting::Yes);
}

void addScreenSpaceRenderableOnline(std::string texturePath) {
    const std::string luaTable =
        "{Type = 'ScreenSpaceImageOnline', TexturePath = '" + texturePath + "' }";
    const std::string script = "openspace.registerScreenSpaceRenderable(" + luaTable + ");";
    OsEng.scriptEngine().queueScript(script, openspace::scripting::ScriptEngine::RemoteScripting::Yes);
}

} // namespace 

namespace openspace::gui {

GUI::GUI() 
    : GuiComponent("Main")
    , _globalProperty("Global")
    , _property("Properties")
    , _screenSpaceProperty("ScreenSpace Properties")
    , _virtualProperty("Virtual Properties")
    , _showInternals(false)
    , _currentVisibility(properties::Property::Visibility::Developer)
{
    addPropertySubOwner(_help);
    addPropertySubOwner(_origin);
    addPropertySubOwner(_performance);
    addPropertySubOwner(_globalProperty);
    addPropertySubOwner(_property);
    addPropertySubOwner(_screenSpaceProperty);
    addPropertySubOwner(_virtualProperty);
#ifdef GLOBEBROWSING_USE_GDAL
    addPropertySubOwner(_globeBrowsing);
#endif // GLOBEBROWSING_USE_GDAL
    addPropertySubOwner(_filePath);
    addPropertySubOwner(_time);
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    addPropertySubOwner(_iswa);
#endif // OPENSPACE_MODULE_ISWA_ENABLED
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
    io.Fonts->AddFontFromFileTTF(
        absPath(GuiFont).c_str(),
        FontSize
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

    style.Colors[ImGuiCol_WindowBg] = ImVec4(0.13f, 0.13f, 0.13f, 0.96f);
    style.Colors[ImGuiCol_Border] = ImVec4(0.65f, 0.65f, 0.65f, 0.59f);
    style.Colors[ImGuiCol_TitleBg] = ImVec4(0.71f, 0.81f, 1.00f, 0.45f);
    style.Colors[ImGuiCol_TitleBgCollapsed] = ImVec4(0.71f, 0.81f, 1.00f, 0.45f);
    style.Colors[ImGuiCol_TitleBgActive] = ImVec4(0.51f, 0.69f, 1.00f, 0.63f);
    style.Colors[ImGuiCol_ScrollbarBg] = ImVec4(0.00f, 0.00f, 0.00f, 0.00f);
    style.Colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.12f, 0.71f, 0.80f, 0.43f);
    style.Colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.40f, 0.75f, 0.80f, 0.65f);
    style.Colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.40f, 0.75f, 0.80f, 0.65f);
    style.Colors[ImGuiCol_ComboBg] = ImVec4(0.18f, 0.51f, 0.78f, 1.00f);
    style.Colors[ImGuiCol_SliderGrabActive] = ImVec4(0.50f, 0.80f, 0.76f, 1.00f);
    style.Colors[ImGuiCol_Button] = ImVec4(0.36f, 0.54f, 0.68f, 0.62f);
    style.Colors[ImGuiCol_ButtonHovered] = ImVec4(0.00f, 0.51f, 0.94f, 1.00f);
    style.Colors[ImGuiCol_ButtonActive] = ImVec4(0.00f, 0.43f, 0.80f, 1.00f);
    style.Colors[ImGuiCol_Header] = ImVec4(0.69f, 0.69f, 0.69f, 0.45f);
    style.Colors[ImGuiCol_HeaderHovered] = ImVec4(0.36f, 0.54f, 0.68f, 0.62f);
    style.Colors[ImGuiCol_HeaderActive] = ImVec4(0.53f, 0.63f, 0.87f, 0.80f);
    style.Colors[ImGuiCol_ResizeGrip] = ImVec4(1.00f, 1.00f, 1.00f, 1.00f);
    style.Colors[ImGuiCol_CloseButton] = ImVec4(0.75f, 0.75f, 0.75f, 1.00f);
    style.Colors[ImGuiCol_CloseButtonHovered] = ImVec4(0.52f, 0.52f, 0.52f, 0.60f);
    style.Colors[ImGuiCol_CloseButtonActive] = ImVec4(0.52f, 0.52f, 0.52f, 1.00f);

    //style.Colors[ImGuiCol_WindowBg] = ImVec4(0.0f, 0.21f, 0.24f, 1.0f);
    //style.Colors[ImGuiCol_Border] = ImVec4(0.1f, 0.39f, 0.42f, 0.59f);
    //style.Colors[ImGuiCol_BorderShadow] = ImVec4(0.0f, 0.0f, 0.0f, 0.0f);
    //style.Colors[ImGuiCol_TitleBg] = ImVec4(0.5f, 0.94f, 1.0f, 0.45f);
    //style.Colors[ImGuiCol_TitleBgCollapsed] = ImVec4(0.5f, 0.94f, 1.0f, 0.45f);
    //style.Colors[ImGuiCol_ScrollbarBg] = ImVec4(0.0f, 0.0f, 0.0f, 0.0f);
    //style.Colors[ImGuiCol_ScrollbarGrab] = ImVec4(0.12f, 0.71f, 0.8f, 0.43f);
    //style.Colors[ImGuiCol_ScrollbarGrabHovered] = ImVec4(0.4f, 0.75f, 0.8f, 0.65f);
    //style.Colors[ImGuiCol_ScrollbarGrabActive] = ImVec4(0.4f, 0.75f, 0.8f, 0.65f);
    //style.Colors[ImGuiCol_ComboBg] = ImVec4(0.18f, 0.51f, 0.78f, 1.f);
    //style.Colors[ImGuiCol_SliderGrabActive] = ImVec4(0.5f, 0.8f, 0.76f, 1.0f);
    //style.Colors[ImGuiCol_Button] = ImVec4(0.0f, 0.36f, 0.67f, 0.6f);
    //style.Colors[ImGuiCol_ButtonHovered] = ImVec4(0.0f, 0.51f, 0.94f, 1.0f);
    //style.Colors[ImGuiCol_ButtonActive] = ImVec4(0.0f, 0.43f, 0.8f, 1.0f);
    //style.Colors[ImGuiCol_Header] = ImVec4(0.f, 0.36f, 0.67f, 0.45f);
    //style.Colors[ImGuiCol_HeaderHovered] = ImVec4(0.f, 0.54f, 1.0f, 0.8f);
    //style.Colors[ImGuiCol_HeaderActive] = ImVec4(0.53f, 0.63f, 0.87f, 0.8f);
    //style.Colors[ImGuiCol_ResizeGrip] = ImVec4(1.0f, 1.0f, 1.0f, 1.0f);
    //style.Colors[ImGuiCol_CloseButton] = ImVec4(0.75f, 0.75f, 0.75f, 1.0f);
    //style.Colors[ImGuiCol_CloseButtonHovered] = ImVec4(0.52f, 0.52f, 0.52f, 0.6f);
    //style.Colors[ImGuiCol_CloseButtonActive] = ImVec4(0.52f, 0.52f, 0.52f, 1.0f);

    _property.initialize();
    _property.setHasRegularProperties(true);
    _screenSpaceProperty.initialize();
    _screenSpaceProperty.setHasRegularProperties(true);
    _globalProperty.initialize();
    _globalProperty.setHasRegularProperties(true);
    _virtualProperty.initialize();
    _filePath.initialize();
#ifdef GLOBEBROWSING_USE_GDAL    
    _globeBrowsing.initialize();
#endif // GLOBEBROWSING_USE_GDAL
    _performance.initialize();
    _help.initialize();
    _parallel.initialize();
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    _iswa.initialize(); 
#endif // OPENSPACE_MODULE_ISWA_ENABLED
}

void GUI::deinitialize() {
    ImGui::Shutdown();

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    _iswa.deinitialize();
#endif // OPENSPACE_MODULE_ISWA_ENABLED
    _parallel.deinitialize();
    _help.deinitialize();
    _performance.deinitialize();
    _globalProperty.deinitialize();
    _screenSpaceProperty.deinitialize();
    _virtualProperty.deinitialize();
    _filePath.deinitialize();
#ifdef GLOBEBROWSING_USE_GDAL
    _globeBrowsing.deinitialize();
#endif // GLOBEBROWSING_USE_GDAL
    _property.deinitialize();

    delete iniFileBuffer;
}

void GUI::initializeGL() {
    _program = ghoul::opengl::ProgramObject::Build(
        "GUI",
        "${MODULE_IMGUI}/shaders/gui_vs.glsl",
        "${MODULE_IMGUI}/shaders/gui_fs.glsl"
    );
    if (!_program) {
        return;
    }

    unsigned char* pngData;
    glm::ivec2 textureSize;
    ImGui::GetIO().Fonts->GetTexDataAsRGBA32(&pngData, &textureSize.x, &textureSize.y);

    _fontTexture = std::make_unique<ghoul::opengl::Texture>(
        pngData,
        glm::uvec3(textureSize.x, textureSize.y, 1)
    );
    _fontTexture->setName("Gui Text");
    _fontTexture->setDataOwnership(ghoul::opengl::Texture::TakeOwnership::No);
    _fontTexture->uploadTexture();
    GLuint id = *_fontTexture;
    uint64_t tmp = id;
    ImGui::GetIO().Fonts->TexID = reinterpret_cast<void*>(tmp);

    glGenBuffers(1, &vbo);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, vboMaxSize, nullptr, GL_DYNAMIC_DRAW);
    
    glGenBuffers(1, &vboElements);
    
    glGenVertexArrays(1, &vao);
    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glEnableVertexAttribArray(_program->attributeLocation("in_position"));
    glEnableVertexAttribArray(_program->attributeLocation("in_uv"));
    glEnableVertexAttribArray(_program->attributeLocation("in_color"));

    glVertexAttribPointer(
        _program->attributeLocation("in_position"),
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(ImDrawVert),
        nullptr // = reinterpret_cast<GLvoid*>(offsetof(ImDrawVert, pos))
    );
    glVertexAttribPointer(
        _program->attributeLocation("in_uv"),
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(ImDrawVert),
        reinterpret_cast<GLvoid*>(offsetof(ImDrawVert, uv))
    );
    glVertexAttribPointer(
        _program->attributeLocation("in_color"),
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
#ifdef GLOBEBROWSING_USE_GDAL
    _globeBrowsing.initializeGL();
#endif // GLOBEBROWSING_USE_GDAL
    _filePath.initializeGL();
    _parallel.initializeGL();
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    _iswa.initializeGL();
#endif // OPENSPACE_MODULE_ISWA_ENABLED

}

void GUI::deinitializeGL() {
    _program = nullptr;
    _fontTexture = nullptr;

    if (vao) {
        glDeleteVertexArrays(1, &vao);
    }
    if (vbo) {
        glDeleteBuffers(1, &vbo);
    }
    if (vboElements) {
        glDeleteBuffers(1, &vboElements);
    }

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    _iswa.deinitializeGL();
#endif // OPENSPACE_MODULE_ISWA_ENABLED
    _parallel.deinitializeGL();
    _help.deinitializeGL();
    _performance.deinitializeGL();
    _globalProperty.deinitializeGL();
    _screenSpaceProperty.deinitializeGL();
#ifdef GLOBEBROWSING_USE_GDAL
    _globeBrowsing.deinitializeGL();
#endif // GLOBEBROWSING_USE_GDAL
    _filePath.deinitializeGL();
    _property.deinitializeGL();
}

void GUI::startFrame(float deltaTime, const glm::vec2& windowSize,
                     const glm::vec2& dpiScaling, const glm::vec2& mousePos,
                     uint32_t mouseButtonsPressed)
{
    ImGuiIO& io = ImGui::GetIO();
    io.DisplaySize = ImVec2(windowSize.x, windowSize.y);
    io.DisplayFramebufferScale = ImVec2(dpiScaling.x, dpiScaling.y);
    io.DeltaTime = deltaTime;

    io.MousePos = ImVec2(mousePos.x, mousePos.y);

    io.MouseDown[0] = mouseButtonsPressed & (1 << 0);
    io.MouseDown[1] = mouseButtonsPressed & (1 << 1);

    ImGui::NewFrame();
}

void GUI::endFrame() {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
    }

    if (OsEng.renderEngine().doesPerformanceMeasurements()) {
        _performance.render();
    }

    if (_isEnabled) {
        render();

        if (_globalProperty.isEnabled()) {
            _globalProperty.render();
        }
        if (_property.isEnabled()) {
            _property.render();
        }
        if (_screenSpaceProperty.isEnabled()) {
            _screenSpaceProperty.render();
        }
        if (_virtualProperty.isEnabled()) {
            _virtualProperty.render();
        }
        if (_help.isEnabled()) {
            _help.render();
        }
        if (_parallel.isEnabled()) {
            _parallel.render();
        }
#ifdef OPENSPACE_MODULE_ISWA_ENABLED
        if (_iswa.isEnabled()) {
            _iswa.render();
        }
#endif // OPENSPACE_MODULE_ISWA_ENABLED
        if (_filePath.isEnabled()) {
            _filePath.render();
        }

#ifdef GLOBEBROWSING_USE_GDAL
        if (_globeBrowsing.isEnabled()) {
            _globeBrowsing.render();
        }
#endif // GLOBEBROWSING_USE_GDAL
    }

    ImGui::Render();
}

bool GUI::mouseButtonCallback(MouseButton, MouseAction) {
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

bool GUI::charCallback(unsigned int character, KeyModifier) {
    ImGuiIO& io = ImGui::GetIO();
    bool consumeEvent = io.WantCaptureKeyboard;

    if (consumeEvent) {
        io.AddInputCharacter(static_cast<unsigned short>(character));
    }

    return consumeEvent;
}

void GUI::render() {
    ImGui::Begin("OpenSpace GUI", nullptr);

    bool property = _property.isEnabled();
    ImGui::Checkbox("Scene Graph Properties", &property);
    _property.setEnabled(property);

    bool screenSpaceProperty = _screenSpaceProperty.isEnabled();
    ImGui::Checkbox("ScreenSpace Properties", &screenSpaceProperty);
    _screenSpaceProperty.setEnabled(screenSpaceProperty);

    bool globalProperty = _globalProperty.isEnabled();
    ImGui::Checkbox("Global Properties", &globalProperty);
    _globalProperty.setEnabled(globalProperty);

    bool parallel = _parallel.isEnabled();
    ImGui::Checkbox("Parallel Connection", &parallel);
    _parallel.setEnabled(parallel);

    bool virtualProperty = _virtualProperty.isEnabled();
    ImGui::Checkbox("Virtual Properties", &virtualProperty);
    _virtualProperty.setEnabled(virtualProperty);

    bool filePath = _filePath.isEnabled();
    ImGui::Checkbox("File Paths", &filePath);
    _filePath.setEnabled(filePath);

#ifdef GLOBEBROWSING_USE_GDAL
    bool globeBrowsing = _globeBrowsing.isEnabled();
    ImGui::Checkbox("GlobeBrowsing", &globeBrowsing);
    _globeBrowsing.setEnabled(globeBrowsing);
#endif // GLOBEBROWSING_USE_GDAL

#ifdef OPENSPACE_MODULE_ISWA_ENABLED
    bool iswa = _iswa.isEnabled();
    ImGui::Checkbox("iSWA", &iswa);
    _iswa.setEnabled(iswa);
#endif // OPENSPACE_MODULE_ISWA_ENABLED

    _origin.render();
    _time.render();

    bool help = _help.isEnabled();
    ImGui::Checkbox("Help", &help);
    _help.setEnabled(help);

    renderAndUpdatePropertyVisibility();

    static const int addImageBufferSize = 256;
    static char addImageLocalBuffer[addImageBufferSize];
    static char addImageOnlineBuffer[addImageBufferSize];

    bool addImageLocal = ImGui::InputText(
        "Add Local Image",
        addImageLocalBuffer,
        addImageBufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );
    if (addImageLocal) {
        addScreenSpaceRenderableLocal(std::string(addImageLocalBuffer));
    }

    bool addImageOnline = ImGui::InputText(
        "Add Online Image",
        addImageOnlineBuffer,
        addImageBufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );
    if (addImageOnline) {
        addScreenSpaceRenderableOnline(std::string(addImageOnlineBuffer));
    }

    ImGui::Checkbox("ImGUI Internals", &_showInternals);
    if (_showInternals) {
        ImGui::Begin("Style Editor");
        ImGui::ShowStyleEditor();
        ImGui::End();

        ImGui::Begin("Test Window");
        ImGui::ShowTestWindow();
        ImGui::End();

        ImGui::Begin("Metrics Window");
        ImGui::ShowMetricsWindow();
        ImGui::End();
    }

    ImGui::End();
}
    
void GUI::renderAndUpdatePropertyVisibility() {
    // Fragile! Keep this in sync with properties::Property::Visibility
    using V = properties::Property::Visibility;
    int t = static_cast<std::underlying_type_t<V>>(_currentVisibility);

    // Array is sorted by importance
    std::array<const char*, 4> items = { "User", "Developer", "Hidden", "All"};
    ImGui::Combo("PropertyVisibility", &t, items.data(), static_cast<int>(items.size()));

    _currentVisibility = static_cast<V>(t);
    _globalProperty.setVisibility(_currentVisibility);
    _property.setVisibility(_currentVisibility);
    _screenSpaceProperty.setVisibility(_currentVisibility);
    _virtualProperty.setVisibility(_currentVisibility);
}


} // namespace openspace::gui
