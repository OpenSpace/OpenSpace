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

#include <modules/imgui/include/guiactioncomponent.h>

#include <modules/imgui/imguimodule.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/util/keys.h>
#include <set>

namespace openspace::gui {

GuiActionComponent::GuiActionComponent()
    : GuiComponent("Actions", "Actions")
{}

void GuiActionComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);

    bool v = _isEnabled;
    ImGui::Begin("Shortcuts", &v);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    using K = KeyWithModifier;
    using V = std::string;
    const std::multimap<K, V>& binds = global::keybindingManager->keyBindings();

    std::set<std::string> boundActions;
    CaptionText("Keybindings");
    for (const std::pair<const K, V>& p : binds) {
        boundActions.insert(p.second);
        if (ImGui::Button(ghoul::to_string(p.first).c_str())) {
            global::actionManager->triggerAction(
                p.second,
                ghoul::Dictionary(),
                interaction::ActionManager::ShouldBeSynchronized::Yes,
                interaction::ActionManager::ShouldBeLogged::Yes
            );
        }
        ImGui::SameLine();

        // Poor mans table layout
        ImGui::SetCursorPosX(125.f);

        const interaction::Action& a = global::actionManager->action(p.second);
        ImGui::Text("%s", a.documentation.c_str());
        if (a.isLocal) {
            ImGui::SameLine();
            ImGui::Text("(%s)", "local");
        }
    }

    CaptionText("Other Actions");
    for (const interaction::Action& action : global::actionManager->actions()) {
        // We only show all of the other actions that are not currently bound to keys here
        if (boundActions.find(action.identifier) != boundActions.end()) {
            continue;
        }

        if (ImGui::Button(action.identifier.c_str())) {
            global::actionManager->triggerAction(
                action.command,
                ghoul::Dictionary(),
                interaction::ActionManager::ShouldBeSynchronized::Yes,
                interaction::ActionManager::ShouldBeLogged::Yes
            );
        }
        ImGui::SameLine();

        // Poor mans table layout
        ImGui::SetCursorPosX(350.f);

        ImGui::Text("%s", action.documentation.c_str());
        if (action.isLocal) {
            ImGui::SameLine();
            ImGui::Text("(%s)", "local");
        }
    }
    ImGui::End();
}

} // namespace openspace::gui
