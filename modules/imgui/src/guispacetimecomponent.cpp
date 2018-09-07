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

#include <modules/imgui/include/guispacetimecomponent.h>

#include <modules/imgui/include/gui.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/time.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>
#include <openspace/scripting/scriptengine.h>

#include <numeric>

namespace {
    const ImVec2 Size = ImVec2(350, 500);

    void showTooltip(const std::string& message, double delay) {
        // Hackish way to enfore a window size for TextWrapped (SetNextWindowSize did not
        // do the trick)
        constexpr std::string::size_type FirstLineLength = 64;
        if (ImGui::IsItemHovered() && GImGui->HoveredIdTimer > delay) {
            ImGui::BeginTooltip();
            ImGui::Text(
                "%s",
                message.substr(0, std::min(message.size() - 1, FirstLineLength)).c_str()
            );
            if (message.size() > FirstLineLength) {
                ImGui::TextWrapped(
                    "%s",
                    message.substr(std::min(message.size() - 1, FirstLineLength)).c_str()
                );
            }

            ImGui::EndTooltip();
        }
    }

} // namespace

namespace openspace::gui {

GuiSpaceTimeComponent::GuiSpaceTimeComponent()
    : GuiComponent("SpaceTime", "Space/Time")
{}

void GuiSpaceTimeComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);
    bool v = _isEnabled;
    ImGui::Begin(guiName().c_str(), &v, Size, 0.5f, ImGuiWindowFlags_AlwaysAutoResize);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    std::vector<SceneGraphNode*> nodes =
        global::renderEngine.scene()->allSceneGraphNodes();

    std::sort(
        nodes.begin(),
        nodes.end(),
        [](SceneGraphNode* lhs, SceneGraphNode* rhs) {
            return lhs->guiName() < rhs->guiName();
        }
    );

    CaptionText("Focus Selection");

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 10.f);

    // Buttons for important SceneGraphNodes
    for (SceneGraphNode* n : nodes) {
        const std::vector<std::string>& tags = n->tags();
        const auto it = std::find(tags.begin(), tags.end(), "GUI.Interesting");
        if (it != tags.end()) {
            const bool pressed = ImGui::Button(n->guiName().c_str());
            ImGui::SameLine();
            if (pressed) {
                global::scriptEngine.queueScript(
                    "openspace.setPropertyValue('NavigationHandler.Origin', '" +
                    n->identifier() + "');",
                    scripting::ScriptEngine::RemoteScripting::Yes
                );
            }
        }
    }

    ImGui::NewLine();
    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 10.f);

    SceneGraphNode* currentFocus = global::navigationHandler.focusNode();

    std::string nodeNames;
    for (SceneGraphNode* n : nodes) {
        nodeNames += n->identifier() + '\0';
    }

    const auto iCurrentFocus = std::find(nodes.begin(), nodes.end(), currentFocus);
    if (!nodes.empty()) {
        // Only check if we found the current focus node if we have any nodes at all
        // only then it would be a real error
        ghoul_assert(iCurrentFocus != nodes.end(), "Focus node not found");
    }
    int currentPosition = static_cast<int>(std::distance(nodes.begin(), iCurrentFocus));

    const bool hasChanged = ImGui::Combo("", &currentPosition, nodeNames.c_str());
    if (hasChanged) {
        global::scriptEngine.queueScript(
            "openspace.setPropertyValue('NavigationHandler.Origin', '" +
            nodes[currentPosition]->identifier() + "');",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    ImGui::SameLine();
    const bool pressed = ImGui::Button("Refocus");
    if (pressed) {
        // To refocus, we are first clearing the origin property before setting it back
        // to its old value. The property mechanism's onChange does not fire if the same
        // value is set again, hence the need for the clearing
        global::scriptEngine.queueScript(
            R"(
                local o = openspace.getPropertyValue('NavigationHandler.Origin');
                openspace.setPropertyValue('NavigationHandler.Origin', '');
                openspace.setPropertyValue('NavigationHandler.Origin', o);
            )",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    }

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);

    ImGui::Separator();
    ImGui::Separator();
    ImGui::Separator();

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);

    CaptionText("Time Controls");
    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 10.f);

    ImGui::Text("Current Date: %s", global::timeManager.time().UTC().c_str());

    constexpr int BufferSize = 256;
    static char Buffer[BufferSize];
    bool dateChanged = ImGui::InputText(
        "Change Date",
        Buffer,
        BufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );
    if (dateChanged) {
        global::scriptEngine.queueScript(
            "openspace.time.setTime(\"" + std::string(Buffer) + "\")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }

    showTooltip(
        "Entering a date here and confirming with ENTER sets the current simulation time "
        "to the entered date. The format of the date has to be either ISO 8601 "
        "YYYY-MM-DDThh:mm:ss (2017-08-27T04:00:00) or YYYY MMM DD hh:mm:ss "
        "(2017 MAY 01 12:00:00). The hours are in 24h and specified as UTC.",
        _tooltipDelay
    );

    auto incrementTime = [](float days) {
        using namespace std::chrono;

        const float duration = global::timeManager.defaultTimeInterpolationDuration();

        const TimeKeyframeData predictedTime = global::timeManager.interpolate(
            global::windowDelegate.applicationTime() + duration
        );
        const double j2000 = predictedTime.time.j2000Seconds();
        const long long seconds = duration_cast<std::chrono::seconds>(
            std::chrono::hours(24) * std::abs(days)
        ).count();

        const double newTime = days < 0 ?
            j2000 - seconds :
            j2000 + seconds;

        global::scriptEngine.queueScript(
            "openspace.time.interpolateTime(" + std::to_string(newTime) + ", " +
            std::to_string(duration) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    };

    const bool minusMonth = ImGui::Button("-Month");
    showTooltip("OBS: A month here equals 30 days.", _tooltipDelay);
    if (minusMonth) {
        incrementTime(-30);
    }
    ImGui::SameLine();
    const bool minusWeek = ImGui::Button("-Week");
    if (minusWeek) {
        incrementTime(-7);
    }
    ImGui::SameLine();
    const bool minusDay = ImGui::Button("-Day");
    if (minusDay) {
        incrementTime(-1);
    }
    ImGui::SameLine();

    const bool minusHour = ImGui::Button("-Hour");
    if (minusHour) {
        incrementTime(-1/24.f);
    }
    ImGui::SameLine();

    ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15.f);

    const bool nowDay = ImGui::Button("Now");
    if (nowDay) {
        std::string nowTime = Time::now().UTC();
        // UTC returns a string of the type YYYY MMM DDTHH:mm:ss.xxx
        // setTime doesn't like the T in it and wants a space instead
        nowTime[11] = ' ';

        global::scriptEngine.queueScript(
            "openspace.time.setTime(\"" + nowTime + "\")",
            scripting::ScriptEngine::RemoteScripting::No
        );

    }
    ImGui::SameLine();

    ImGui::SetCursorPosX(ImGui::GetCursorPosX() + 15.f);

    const bool plusHour = ImGui::Button("+Hour");
    if (plusHour) {
        incrementTime(1/24.f);
    }
    ImGui::SameLine();

    const bool plusDay = ImGui::Button("+Day");
    if (plusDay) {
        incrementTime(1);
    }
    ImGui::SameLine();
    const bool plusWeek = ImGui::Button("+Week");
    if (plusWeek) {
        incrementTime(7);
    }
    ImGui::SameLine();
    const bool plusMonth = ImGui::Button("+Month");
    if (plusMonth) {
        incrementTime(30);
    }
    showTooltip("OBS: A month here equals 30 days.", _tooltipDelay);

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);
//
    {
        const float dt = static_cast<float>(global::timeManager.targetDeltaTime());
        if (_firstFrame) {
            const std::pair<double, std::string>& dtInfo = simplifyTime(dt);
            _deltaTime = static_cast<float>(dtInfo.first);
            _deltaTimeUnit = timeUnitFromString(dtInfo.second.c_str());

            _timeUnits = std::accumulate(
                openspace::TimeUnits.begin(),
                openspace::TimeUnits.end(),
                std::string(""),
                [](const std::string& a, const openspace::TimeUnit& unit) {
                    return a + nameForTimeUnit(unit, true) + " / second" + '\0';
                }
            );

            _firstFrame = false;
        }

        _deltaTime = static_cast<float>(
            convertTime(dt, TimeUnit::Second, _deltaTimeUnit)
        );

        bool valueChanged = ImGui::InputFloat(
            "##inputValueDeltaTime",
            &_deltaTime,
            1.f,
            100.f,
            -1,
            ImGuiInputTextFlags_EnterReturnsTrue
        );
        ImGui::SameLine();

        int deltaTimeUnit = static_cast<int>(_deltaTimeUnit);
        bool unitChanged = ImGui::Combo(
            "##inputUnit",
            &deltaTimeUnit,
            _timeUnits.c_str()
        );
        _deltaTimeUnit = static_cast<TimeUnit>(deltaTimeUnit);

        if (valueChanged) {
            // If the value changed, we want to change the delta time to the new value

            double newDt = convertTime(_deltaTime, _deltaTimeUnit, TimeUnit::Second);
            global::scriptEngine.queueScript(
                "openspace.time.interpolateDeltaTime(" + std::to_string(newDt) + ")",
                scripting::ScriptEngine::RemoteScripting::No
            );
        }
        if (unitChanged) {
            // If only the unit changes, we keep the delta time, but need to convert the
            // value to the new unit

            _deltaTime = static_cast<float>(
                convertTime(dt, TimeUnit::Second, static_cast<TimeUnit>(_deltaTimeUnit)
                )
            );
        }
    }

    ImGui::Text("%s", "Time Slider");
    ImGui::Checkbox("Reset time after shuttle", &_resetTimeAfterShuttle);
    if (ImGui::IsItemHovered()) {
        ImGui::SetTooltip(
            "%s",
            "If this is enabled, after moving time with the slider below the delta time "
            "is reset to its old value. If this is disabled, the end value of the slider "
            "is retained"
        );
    }

    if (_resetTimeAfterShuttle) {
        bool firstSlidingValue = _slidingDelta == 0.f;
        const bool slidingDeltaChanged = ImGui::SliderFloat(
            "Delta Time Slider",
            &_slidingDelta,
            -100.f,
            100.f,
            "%.6f"
        );
        firstSlidingValue &= _slidingDelta != 0.f;

        if (slidingDeltaChanged) {
            if (firstSlidingValue) {
                _oldDeltaTime = _deltaTime;
            }

            const double newDeltaTime = convertTime(
                _deltaTime + _slidingDelta,
                _deltaTimeUnit,
                TimeUnit::Second
            );

            global::scriptEngine.queueScript(
                "openspace.time.setDeltaTime(" + std::to_string(newDeltaTime) + ")",
                scripting::ScriptEngine::RemoteScripting::No
            );
        }
        if (!ImGui::IsItemActive() && !ImGui::IsItemClicked()) {
            if (_slidingDelta != 0.f) {
                global::scriptEngine.queueScript(
                    "openspace.time.setDeltaTime(" + std::to_string(_oldDeltaTime) + ")",
                    scripting::ScriptEngine::RemoteScripting::No
                );

            }
            _slidingDelta = 0.f;
        }
    }
    else {
        bool accelerationDeltaChanged = ImGui::SliderFloat(
            "Delta Time Slider",
            &_accelerationDelta,
            -100.f,
            100.f,
            "%.6f"
        );

        if (accelerationDeltaChanged || ImGui::IsItemActive() || ImGui::IsItemClicked()) {
            // We want the value to change by _accelerationDelta every 100 real world ms
            const double newDeltaTime = convertTime(
                _deltaTime + _accelerationDelta * global::windowDelegate.deltaTime() * 10,
                static_cast<TimeUnit>(_deltaTimeUnit),
                TimeUnit::Second
            );

            global::scriptEngine.queueScript(
                "openspace.time.setDeltaTime(" + std::to_string(newDeltaTime) + ")",
                scripting::ScriptEngine::RemoteScripting::No
            );
        }
        else {
            _accelerationDelta = 0.f;
        }

        _deltaTime -= _slidingDelta;
    }
    
    const bool isPaused = global::timeManager.isPaused();
    const bool pauseChanged = ImGui::Button(
        isPaused ? "Resume" : "Pause",
        { ImGui::GetWindowWidth() / 2 - 7.5f, 0.f }
    );
    if (pauseChanged) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateTogglePause()",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }
    ImGui::SameLine();
    const bool invert = ImGui::Button(
        "Invert",
        { ImGui::GetWindowWidth() / 2 - 7.5f, 0.f }
    );
    if (invert) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(-1 * openspace.time.deltaTime());",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }

    const bool minusDs = ImGui::Button("-1d/s");
    if (minusDs) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(-24 * 60 * 60) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }
    ImGui::SameLine();

    const bool minusHs = ImGui::Button("-1h/s");
    if (minusHs) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(-60 * 60) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }
    ImGui::SameLine();

    const bool minusMs = ImGui::Button("-1min/s");
    if (minusMs) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(-60) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }
    ImGui::SameLine();

    const bool minusSs = ImGui::Button("-1s/s");
    if (minusSs) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(-1) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }
    ImGui::SameLine();

    const bool zero = ImGui::Button("0");
    if (zero) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(0) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }
    ImGui::SameLine();


    const bool plusSs = ImGui::Button("+1s/s");
    if (plusSs) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(1) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }
    ImGui::SameLine();

    const bool plusMs = ImGui::Button("1min/s");
    if (plusMs) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(60) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }
    ImGui::SameLine();

    const bool plusHs = ImGui::Button("1h/s");
    if (plusHs) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(60 * 60) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }
    ImGui::SameLine();

    const bool plusDs = ImGui::Button("1d/s");
    if (plusDs) {
        global::scriptEngine.queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(24 * 60 * 60) + ")",
            scripting::ScriptEngine::RemoteScripting::No
        );
    }

    ImGui::End();
}

} // namespace openspace gui
