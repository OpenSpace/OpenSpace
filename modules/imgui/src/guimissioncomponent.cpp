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

#include <modules/imgui/include/guimissioncomponent.h>

#include <modules/imgui/include/gui.h>
#include <modules/imgui/include/imgui_include.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/mission/mission.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/util/timerange.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>

namespace {
    const ImVec2 size = ImVec2(350, 500);

    void renderMission(const openspace::Mission& mission) {
        // The hashname is necessary since ImGui computes a hash based off the name of the
        // elements.  This does not play well with using %s as the name
        std::string missionHashname = "##" + mission.name();


        double currentTime = OsEng.timeManager().time().j2000Seconds();
        openspace::MissionPhase::Trace t = mission.phaseTrace(currentTime, 0);
        
        int treeOption = t.empty() ? 0 : ImGuiTreeNodeFlags_DefaultOpen;
        if (ImGui::TreeNodeEx(("%s" + missionHashname).c_str(), treeOption, mission.name().c_str())) {
            if (!mission.description().empty()) {
                ImGui::Text("%s", mission.description().c_str());
            }

            openspace::TimeRange range = mission.timeRange();
            openspace::Time startTime = openspace::Time(range.start);
            openspace::Time endTime  = openspace::Time(range.end);
            
            openspace::gui::CaptionText("Mission Progress");

            ImGui::Text("%s", startTime.UTC().c_str());
            ImGui::SameLine();
            float v = static_cast<float>(currentTime);
            float s = static_cast<float>(startTime.j2000Seconds());
            float e = static_cast<float>(endTime.j2000Seconds());

            ImGui::SliderFloat(missionHashname.c_str(), &v, s, e, OsEng.timeManager().time().UTC().c_str());
            ImGui::SameLine();
            ImGui::Text("%s", endTime.UTC().c_str());

            openspace::gui::CaptionText("Phases");

            for (const openspace::Mission& m : mission.phases()) {
                renderMission(m);
            }


            ImGui::TreePop();
        }
    }

} // namespace

namespace openspace::gui {

GuiMissionComponent::GuiMissionComponent() : GuiComponent("Mission Information") {}

void GuiMissionComponent::render() {
    bool v = _isEnabled;
    ImGui::Begin(name().c_str(), &v, size, 0.75f);
    _isEnabled = v;

    ghoul_assert(
        MissionManager::ref().hasCurrentMission(),
        "Must have a current mission"
    );

    const Mission& currentMission = MissionManager::ref().currentMission();
    renderMission(currentMission);

    ImGui::End();
}

} // namespace openspace gui
