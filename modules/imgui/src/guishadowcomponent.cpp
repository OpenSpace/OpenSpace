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

#include <modules/imgui/include/guishadowcomponent.h>

#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/shadowmapping.h>
#include <openspace/scene/scenegraphnode.h>

namespace {
    const ImVec2 WindowSize = ImVec2(350, 500);
} // namespace

namespace openspace {

GuiShadowComponent::GuiShadowComponent()
    : GuiComponent("ShadowMaps", "Shadow Maps")
{}

void GuiShadowComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);
    bool v = _isEnabled;
    ImGui::SetNextWindowSize(WindowSize, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowBgAlpha(0.5f);
    ImGui::Begin(guiName().c_str(), &v, ImGuiWindowFlags_AlwaysAutoResize);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    const FramebufferRenderer& renderer = global::renderEngine->renderer();
    const std::vector<std::string> groups = renderer.shadowGroups();

    for (const std::string& group : groups) {
        ShadowInfo sm = renderer.shadowInformation(group);
        std::string lightSource = sm.lightSource->identifier();

        ImGui::BeginGroup();
        ImGui::Text("%s", group.c_str());
        ImGui::Text("Light Source: %s", lightSource.c_str());
        ImGui::Text("Targets");
        for (const SceneGraphNode* node : sm.targets) {
            ghoul_assert(node, "No SceneGraphNode");
            ghoul_assert(node->renderable(), "No Renderable");
            const Shadower* shadower = dynamic_cast<const Shadower*>(node->renderable());
            ghoul_assert(shadower, "No shadower");

            std::string id = node->identifier();
            ImGui::Text("  %s", id.c_str());
            ImGui::Text("    Is Enabled: %d", node->renderable()->isEnabled());
            ImGui::Text("    Is Ready: %d", node->renderable()->isReady());
            ImGui::Text("    Is Casting Shadow: %d", shadower->isCastingShadow());

        }

        ImGui::EndGroup();
    }

    ImGui::End();
}

} // namespace openspace
