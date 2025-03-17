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

#include <modules/imgui/include/guiscenecomponent.h>

#include <modules/imgui/imguimodule.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>

namespace {
    const ImVec2 Size = ImVec2(350, 500);

    void renderSceneGraphNode(const openspace::SceneGraphNode& node,
                              const openspace::Time& time)
    {
        using namespace openspace;

        if (ImGui::TreeNode(node.identifier().c_str())) {
            const std::vector<SceneGraphNode*> children = node.children();
            for (SceneGraphNode* c : children) {
                renderSceneGraphNode(*c, time);
            }

            bool timeRangeActive = node.isTimeFrameActive();
            ImGui::Checkbox("Time Range Active", &timeRangeActive);

            const Renderable* renderable = node.renderable();
            if (renderable) {
                CaptionText("Renderable");
                bool enabled = renderable->isEnabled();
                ImGui::Checkbox("Enabled", &enabled);

                bool isVisible = renderable->isVisible();
                ImGui::Checkbox("Is Visible", &isVisible);

                bool shouldUpdateIfDisabled = renderable->shouldUpdateIfDisabled();
                ImGui::Checkbox("Should update if disabled", &shouldUpdateIfDisabled);

                bool isReady = renderable->isReady();
                ImGui::Checkbox("Is Ready", &isReady);

                const Renderable::RenderBin bin = renderable->renderBin();
                const std::string binStr = [](Renderable::RenderBin b) {
                    switch (b) {
                        case Renderable::RenderBin::Background:
                            return "Background";
                        case Renderable::RenderBin::Opaque:
                            return "Opaque";
                        case Renderable::RenderBin::PreDeferredTransparent:
                            return "PreDeferredTransparent";
                        case Renderable::RenderBin::PostDeferredTransparent:
                            return "PostDeferredTransparent";
                        case Renderable::RenderBin::Overlay:
                            return "Overlay";
                        default:
                            throw ghoul::MissingCaseException();
                    }
                }(bin);
                ImGui::Text("RenderBin: %s", binStr.c_str());
            }
            ImGui::TreePop();
        }
    }
} // namespace

namespace openspace::gui {

GuiSceneComponent::GuiSceneComponent() : GuiComponent("SceneView", "Scene View") {}

void GuiSceneComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);

    bool v = _isEnabled;
    ImGui::Begin("File Path", &v);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    SceneGraphNode* root = global::renderEngine->scene()->root();
    const Time& now = global::timeManager->time();
    renderSceneGraphNode(*root, now);

    ImGui::End();
}

} // namespace openspace::gui
