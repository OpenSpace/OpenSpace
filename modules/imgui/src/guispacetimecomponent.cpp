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

#include <modules/imgui/include/guispacetimecomponent.h>

#include <modules/imgui/include/imgui_include.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/time.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>

namespace {
    const ImVec2 size = ImVec2(350, 500);
} // namespace

namespace openspace::gui {

GuiSpaceTimeComponent::GuiSpaceTimeComponent() : GuiComponent("Space/Time") {}

void GuiSpaceTimeComponent::render() {
    bool v = _isEnabled;
    ImGui::Begin(name().c_str(), &v, size, 0.5f, ImGuiWindowFlags_AlwaysAutoResize);
    _isEnabled = v;



    std::vector<SceneGraphNode*> nodes =
        OsEng.renderEngine().scene()->allSceneGraphNodes();

    std::sort(
        nodes.begin(),
        nodes.end(),
        [](SceneGraphNode* lhs, SceneGraphNode* rhs) {
            return lhs->name() < rhs->name();
        }
    );

    ImGui::BeginGroup();
    if (ImGui::IsItemHovered()) {
        ImGui::SetTooltip(
            "%s",
            "These buttons and the dropdown menu determine the focus object in the scene "
            "that is the center of all camera movement"
        );
    }


    ImGui::Text("%s", "Focus selection");

    // Buttons for important SceneGraphNodes
    for (SceneGraphNode* n : nodes) {
        const std::vector<std::string>& tags = n->tags();
        auto it = std::find(tags.begin(), tags.end(), "GUI.Interesting");
        if (it != tags.end()) {
            bool pressed = ImGui::Button(n->name().c_str());
            ImGui::SameLine();
            if (pressed) {
                OsEng.scriptEngine().queueScript(
                    "openspace.setPropertyValue('NavigationHandler.Origin', '" +
                    n->name() + "');",
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            }
        }
    }

    ImGui::NewLine();

    SceneGraphNode* currentFocus = OsEng.navigationHandler().focusNode();

    std::string nodeNames = "";
    for (SceneGraphNode* n : nodes) {
        nodeNames += n->name() + '\0';
    }

    auto iCurrentFocus = std::find(nodes.begin(), nodes.end(), currentFocus); 
    if (!nodes.empty()) { 
        // Only check if we found the current focus node if we have any nodes at all
        // only then it would be a real error
        ghoul_assert(iCurrentFocus != nodes.end(), "Focus node not found");
    }
    int currentPosition = static_cast<int>(std::distance(nodes.begin(), iCurrentFocus));

    bool hasChanged = ImGui::Combo("Focus Node", &currentPosition, nodeNames.c_str());
    if (hasChanged) {
        OsEng.scriptEngine().queueScript(
            "openspace.setPropertyValue('NavigationHandler.Origin', '" +
            nodes[currentPosition]->name() + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    ImGui::EndGroup();

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);

    ImGui::Separator();
    ImGui::Separator();
    ImGui::Separator();

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);


    ImGui::BeginGroup();
    if (ImGui::IsItemHovered()) {
        ImGui::SetTooltip(
            "%s",
            "These elements determine the simulation time inside OpenSpace."
        );
    }
    ImGui::Text("%s", "Time Controls");

    constexpr int BufferSize = 256;
    static char Buffer[BufferSize];
    bool dateChanged = ImGui::InputText(
        "Date",
        Buffer,
        BufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );
    if (dateChanged) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setTime(\"" + std::string(Buffer) + "\")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    if (ImGui::IsItemHovered()) {
        ImGui::SetTooltip(
            "%s",
            "Entering a date here and confirming with ENTER sets the current simulation "
            "time to the entered date. The format of the date has to be either ISO 8601 "
            "YYYY-MM-DDThh:mm:ss (2017-08-27T04:00:00) or YYYY MMM DD hh:mm:ss "
            "(2017 MAY 01 12:00:00). The hours are in 24h and specified as UTC."
        );
    }

    auto incrementTime = [](int days) {
        using namespace std::chrono;
        double j2000 = OsEng.timeManager().time().j2000Seconds();

        long long seconds = duration_cast<std::chrono::seconds>(
            std::chrono::hours(24) * std::abs(days)
        ).count();

        double newTime = [days, j2000, seconds](){
            if (days < 0) {
                return static_cast<double>(j2000 - seconds);
            }
            else {
                return static_cast<double>(j2000 + seconds);
            }
        }();

        OsEng.scriptEngine().queueScript(
            "openspace.time.setTime(" + std::to_string(newTime) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };

    bool minusMonth = ImGui::Button("-Month");
    if (ImGui::IsItemHovered()) {
        ImGui::SetTooltip(
            "%s",
            "OBS: A month here equals 30 days."
        );
    }
    if (minusMonth) {
        incrementTime(-30);
    }
    ImGui::SameLine();
    bool minusWeek = ImGui::Button("-Week");
    if (minusWeek) {
        incrementTime(-7);
    }
    ImGui::SameLine();
    bool minusDay = ImGui::Button("-Day");
    if (minusDay) {
        incrementTime(-1);
    }
    ImGui::SameLine();
    
    ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 55.f);

    bool plusDay = ImGui::Button("+Day");
    if (plusDay) {
        incrementTime(1);
    }
    ImGui::SameLine();
    bool plusWeek = ImGui::Button("+Week");
    if (plusWeek) {
        incrementTime(7);
    }
    ImGui::SameLine();
    bool plusMonth = ImGui::Button("+Month");
    if (plusMonth) {
        incrementTime(30);
    }
    if (ImGui::IsItemHovered()) {
        ImGui::SetTooltip(
            "%s",
            "OBS: A month here equals 30 days."
        );
    }

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);


    float deltaTime = static_cast<float>(OsEng.timeManager().time().deltaTime());
    bool changed = ImGui::SliderFloat("Delta Time", &deltaTime, -100000.f, 100000.f, "%.3f", 5.f);
    if (changed) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(deltaTime) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    if (ImGui::IsItemHovered()) {
        ImGui::SetTooltip(
            "%s",
            "This determines the simulation time increment, that is the passage "
            "of time in OpenSpace relative to a wall clock. Times are expressed as "
            "simulation time / real world time."
        );
    }

    bool isPaused = OsEng.timeManager().time().paused();

    
    bool pauseChanged = ImGui::Button(isPaused ? "Resume" : "Pause", { ImGui::GetWindowWidth() - 7.5f, 0.f } );
    if (pauseChanged) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.togglePause()",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    auto setDeltaTime = [](std::chrono::seconds dt) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(dt.count()) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    };

    bool minusDs = ImGui::Button("-1d/s");
    if (minusDs) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(-24 * 60 * 60) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    ImGui::SameLine();

    bool minusHs = ImGui::Button("-1h/s");
    if (minusHs) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(-60 * 60) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    ImGui::SameLine();

    bool minusMs = ImGui::Button("-1min/s");
    if (minusMs) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(-60) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    ImGui::SameLine();

    bool minusSs = ImGui::Button("-1s/s");
    if (minusSs) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(-1) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    ImGui::SameLine();

    bool zero = ImGui::Button("0");
    if (zero) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(0) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    ImGui::SameLine();


    bool plusSs = ImGui::Button("+1s/s");
    if (plusSs) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(1) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    ImGui::SameLine();

    bool plusMs = ImGui::Button("1min/s");
    if (plusMs) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(60) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    ImGui::SameLine();

    bool plusHs = ImGui::Button("1h/s");
    if (plusHs) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(60 * 60) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    ImGui::SameLine();

    bool plusDs = ImGui::Button("1d/s");
    if (plusDs) {
        OsEng.scriptEngine().queueScript(
            "openspace.time.setDeltaTime(" + std::to_string(24 * 60 * 60) + ")",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }
    ImGui::SameLine();


    ImGui::EndGroup();

    ImGui::End();

}

} // namespace openspace gui
