/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/imgui/include/guifilepathcomponent.h>

#include <modules/imgui/include/imgui_include.h>
#include <ghoul/filesystem/filesystem.h>

namespace openspace::gui {

GuiFilePathComponent::GuiFilePathComponent() : GuiComponent("FilePath", "File Path") {}

void GuiFilePathComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);

    bool v = _isEnabled;
    ImGui::Begin("File Path", &v);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    ImGui::Text(
        "%s",
        "These are file paths registered in the current OpenSpace instance."
    );
    ImGui::Separator();

    ImGui::Columns(2);
    ImGui::Separator();
    const std::vector<std::string>& tokens = FileSys.tokens();
    for (const std::string& t : tokens) {
        ImGui::Text("%s", t.c_str());
        ImGui::NextColumn();
        ImGui::Text("%s", absPath(t).c_str());
        ImGui::NextColumn();
        ImGui::Separator();
    }
    ImGui::End();
}

} // namespace openspace::gui
