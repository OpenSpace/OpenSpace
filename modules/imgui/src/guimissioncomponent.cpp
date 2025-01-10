/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/imgui/include/guimissioncomponent.h>

#include <modules/imgui/imguimodule.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/mission/mission.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/util/timemanager.h>

namespace {
    const ImVec2 Size = ImVec2(350, 500);

    void renderMission(const openspace::Mission& mission) {
        // The hashname is necessary since ImGui computes a hash based off the name of the
        // elements.  This does not play well with using %s as the name
        const std::string missionHashname = "##" + mission.name();


        const double currentTime = openspace::global::timeManager->time().j2000Seconds();
        const openspace::MissionPhase::Trace t = mission.phaseTrace(currentTime, 0);

        const int treeOption = t.empty() ? 0 : ImGuiTreeNodeFlags_DefaultOpen;
        if (ImGui::TreeNodeEx(
                ("%s" + missionHashname).c_str(),
                treeOption,
                "%s",
                mission.name().c_str())
            )
        {
            if (!mission.description().empty()) {
                ImGui::Text("%s", mission.description().c_str());
            }

            const openspace::TimeRange range = mission.timeRange();
            const openspace::Time startTime = openspace::Time(range.start);
            const openspace::Time endTime  = openspace::Time(range.end);

            openspace::CaptionText("Mission Progress");

            ImGui::Text("%s", std::string(startTime.UTC()).c_str());
            ImGui::SameLine();
            float v = static_cast<float>(currentTime);
            const float s = static_cast<float>(startTime.j2000Seconds());
            const float e = static_cast<float>(endTime.j2000Seconds());

            ImGui::SliderFloat(
                missionHashname.c_str(),
                &v,
                s,
                e,
                std::string(openspace::global::timeManager->time().UTC()).c_str()
            );
            ImGui::SameLine();
            ImGui::Text("%s", std::string(endTime.UTC()).c_str());

            openspace::CaptionText("Phases");

            for (const openspace::Mission& m : mission.phases()) {
                renderMission(m);
            }

            ImGui::TreePop();
        }
    }

} // namespace

namespace openspace::gui {

GuiMissionComponent::GuiMissionComponent()
    : GuiComponent("Missions", "Mission Information")
{}

void GuiMissionComponent::render() {


    ImGui::SetNextWindowCollapsed(_isCollapsed);
    bool v = _isEnabled;
    ImGui::SetNextWindowSize(Size, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowBgAlpha(0.75f);
    ImGui::Begin(guiName().c_str(), &v);
    _isEnabled = v;

    _isCollapsed = ImGui::IsWindowCollapsed();

    if (global::missionManager->hasCurrentMission()) {
        const Mission& currentMission = global::missionManager->currentMission();
        renderMission(currentMission);
    }
    else {
        ImGui::Text("%s", "No mission loaded");
    }

    ImGui::End();
}

} // namespace openspace::gui
