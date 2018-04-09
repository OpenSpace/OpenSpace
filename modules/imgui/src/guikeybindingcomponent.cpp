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

#include <modules/imgui/include/guikeybindingcomponent.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/scripting/scriptengine.h>

#include <modules/imgui/include/imgui_include.h>

namespace openspace::gui {

GuiKeybindingComponent::GuiKeybindingComponent()
    : GuiComponent("Keybinding", "Key Bindings")
{}

void GuiKeybindingComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);

    bool v = _isEnabled;
    ImGui::Begin("File Path", &v);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    using K = KeyWithModifier;
    using V = interaction::KeyBindingManager::KeyInformation;
    const std::multimap<K, V>& binds = OsEng.keyBindingManager().keyBindings();

    for (const std::pair<K, V>& p : binds) {
        if (ImGui::Button(std::to_string(p.first).c_str())) {
            OsEng.scriptEngine().queueScript(
                p.second.command,
                scripting::ScriptEngine::RemoteScripting(p.second.synchronization)
            );
        }
        ImGui::SameLine();

        // Poor mans table layout
        ImGui::SetCursorPosX(125.f);

        ImGui::Text("%s", p.second.documentation.c_str());
        if (!p.second.synchronization) {
            ImGui::SameLine();
            ImGui::Text("(%s)", "local");
        }

    }
}


} // namespace openspace::gui
