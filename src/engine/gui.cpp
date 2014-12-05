/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014                                                                    *
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

#include <openspace/engine/gui.h>

#include <ghoul/opengl/ghoul_gl.h>

#include <imgui.h>
#include <sgct.h>
#define STB_IMAGE_IMPLEMENTATION
#include <stb_image.h>

namespace {
	const std::string _loggerCat = "GUI";
	
	GLuint fontTex = 0; 
	GLint shader_handle = 0;
	GLint texture_location = 0;
	GLint ortho_location = 0;
	GLuint vbo_handle = 0;
	size_t vbo_max_size = 20000;
	GLuint vao_handle;



static void ImImpl_RenderDrawLists(ImDrawList** const cmd_lists, int cmd_lists_count) {
    if (cmd_lists_count == 0)
        return;

    // Setup render state: alpha-blending enabled, no face culling, no depth testing, scissor enabled
    glEnable(GL_BLEND);
    glBlendEquation(GL_FUNC_ADD);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_SCISSOR_TEST);

    // Setup texture
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, fontTex);

    // Setup orthographic projection matrix
    const float width = ImGui::GetIO().DisplaySize.x;
    const float height = ImGui::GetIO().DisplaySize.y;
    const float ortho_projection[4][4] =
    {
        { 2.0f/width,	0.0f,			0.0f,		0.0f },
        { 0.0f,			2.0f/-height,	0.0f,		0.0f },
        { 0.0f,			0.0f,			-1.0f,		0.0f },
        { -1.0f,		1.0f,			0.0f,		1.0f },
    };
    glUseProgram(shader_handle);
    glUniform1i(texture_location, 0);
    glUniformMatrix4fv(ortho_location, 1, GL_FALSE, &ortho_projection[0][0]);

    // Grow our buffer according to what we need
    size_t total_vtx_count = 0;
    for (int n = 0; n < cmd_lists_count; n++)
        total_vtx_count += cmd_lists[n]->vtx_buffer.size();
    glBindBuffer(GL_ARRAY_BUFFER, vbo_handle);
    size_t neededBufferSize = total_vtx_count * sizeof(ImDrawVert);
    if (neededBufferSize > vbo_max_size)
    {
        vbo_max_size = neededBufferSize + 5000;  // Grow buffer
        glBufferData(GL_ARRAY_BUFFER, neededBufferSize, NULL, GL_STREAM_DRAW);
    }

    // Copy and convert all vertices into a single contiguous buffer
    unsigned char* buffer_data = (unsigned char*)glMapBuffer(GL_ARRAY_BUFFER, GL_WRITE_ONLY);
    if (!buffer_data)
        return;
    for (int n = 0; n < cmd_lists_count; n++)
    {
        const ImDrawList* cmd_list = cmd_lists[n];
        memcpy(buffer_data, &cmd_list->vtx_buffer[0], cmd_list->vtx_buffer.size() * sizeof(ImDrawVert));
        buffer_data += cmd_list->vtx_buffer.size() * sizeof(ImDrawVert);
    }
    glUnmapBuffer(GL_ARRAY_BUFFER);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(vao_handle);

    int cmd_offset = 0;
    for (int n = 0; n < cmd_lists_count; n++)
    {
        const ImDrawList* cmd_list = cmd_lists[n];
        int vtx_offset = cmd_offset;
        const ImDrawCmd* pcmd_end = cmd_list->commands.end();
        for (const ImDrawCmd* pcmd = cmd_list->commands.begin(); pcmd != pcmd_end; pcmd++)
        {
            glScissor((int)pcmd->clip_rect.x, (int)(height - pcmd->clip_rect.w), (int)(pcmd->clip_rect.z - pcmd->clip_rect.x), (int)(pcmd->clip_rect.w - pcmd->clip_rect.y));
            glDrawArrays(GL_TRIANGLES, vtx_offset, pcmd->vtx_count);
            vtx_offset += pcmd->vtx_count;
        }
        cmd_offset = vtx_offset;
    }

    // Restore modified state
    glBindVertexArray(0);
    glUseProgram(0);
    glDisable(GL_SCISSOR_TEST);
    glBindTexture(GL_TEXTURE_2D, 0);
}
}

namespace openspace {

GUI::GUI(const glm::vec2& windowSize) {
	ImGuiIO& io = ImGui::GetIO();
	io.DisplaySize = ImVec2(windowSize.x, windowSize.y);
	io.DeltaTime = 1.f / 60.f;
	io.PixelCenterOffset = 0.5f;
    io.KeyMap[ImGuiKey_Tab] = SGCT_KEY_TAB;                       // Keyboard mapping. ImGui will use those indices to peek into the io.KeyDown[] array.
    io.KeyMap[ImGuiKey_LeftArrow] = SGCT_KEY_LEFT;
    io.KeyMap[ImGuiKey_RightArrow] = SGCT_KEY_RIGHT;
    io.KeyMap[ImGuiKey_UpArrow] = SGCT_KEY_UP;
    io.KeyMap[ImGuiKey_DownArrow] = SGCT_KEY_DOWN;
    io.KeyMap[ImGuiKey_Home] = SGCT_KEY_HOME;
    io.KeyMap[ImGuiKey_End] = SGCT_KEY_END;
    io.KeyMap[ImGuiKey_Delete] = SGCT_KEY_DELETE;
    io.KeyMap[ImGuiKey_Backspace] = SGCT_KEY_BACKSPACE;
    io.KeyMap[ImGuiKey_Enter] = SGCT_KEY_ENTER;
    io.KeyMap[ImGuiKey_Escape] = SGCT_KEY_ESCAPE;
    io.KeyMap[ImGuiKey_A] = SGCT_KEY_A;
    io.KeyMap[ImGuiKey_C] = SGCT_KEY_C;
    io.KeyMap[ImGuiKey_V] = SGCT_KEY_V;
    io.KeyMap[ImGuiKey_X] = SGCT_KEY_X;
    io.KeyMap[ImGuiKey_Y] = SGCT_KEY_Y;
    io.KeyMap[ImGuiKey_Z] = SGCT_KEY_Z;

	io.RenderDrawListsFn = ImImpl_RenderDrawLists;
    //io.SetClipboardTextFn = ImImpl_SetClipboardTextFn; // @TODO implement? ---abock
    //io.GetClipboardTextFn = ImImpl_GetClipboardTextFn; // @TODO implement? ---abock
}

GUI::~GUI() {
	ImGui::Shutdown();
}

void GUI::initializeGL() {
    glGenTextures(1, &fontTex);
    glBindTexture(GL_TEXTURE_2D, fontTex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    const void* png_data;
    unsigned int png_size;
    ImGui::GetDefaultFontData(NULL, NULL, &png_data, &png_size);
    int tex_x, tex_y, tex_comp;
    void* tex_data = stbi_load_from_memory((const unsigned char*)png_data, (int)png_size, &tex_x, &tex_y, &tex_comp, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, tex_x, tex_y, 0, GL_RGBA, GL_UNSIGNED_BYTE, tex_data);
    stbi_image_free(tex_data);
}

void GUI::deinitializeGL() {
	if (vao_handle) glDeleteVertexArrays(1, &vao_handle);
    if (vbo_handle) glDeleteBuffers(1, &vbo_handle);
    glDeleteProgram(shader_handle);
}

void GUI::render(float deltaTime,
				 const glm::vec2& mousePos,
				 bool mouseButtonsPressed[2])
{
	ImGuiIO& io = ImGui::GetIO();
	io.DeltaTime = deltaTime;
	io.MousePos = ImVec2(mousePos.x, mousePos.y);
	io.MouseDown[0] = mouseButtonsPressed[0];
	io.MouseDown[1] = mouseButtonsPressed[1];

	ImGui::NewFrame();
	static bool show = true;
	//if (show) {
		ImGui::ShowTestWindow(&show);
	//}

	ImGui::Render();
}

} // namespace openspace
