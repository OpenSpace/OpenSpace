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

#include <modules/imgui/include/guispacetimecomponent.h>

#include <modules/imgui/imguimodule.h>
#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scene/scene.h>
#include <openspace/util/timemanager.h>
#include <openspace/scripting/scriptengine.h>

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

    constexpr std::string_view AnchorProperty =
        "NavigationHandler.OrbitalNavigator.Anchor";

    constexpr std::string_view RetargetAnchorProperty =
        "NavigationHandler.OrbitalNavigator.RetargetAnchor";

} // namespace

namespace openspace::gui {

GuiSpaceTimeComponent::GuiSpaceTimeComponent()
    : GuiComponent("SpaceTime", "Space/Time")
{}

void GuiSpaceTimeComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);
    bool v = _isEnabled;
    ImGui::SetNextWindowSize(Size, ImGuiCond_FirstUseEver);
    ImGui::SetNextWindowBgAlpha(0.5f);
    ImGui::Begin(guiName().c_str(), &v, ImGuiWindowFlags_AlwaysAutoResize);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    std::vector<SceneGraphNode*> nodes =
        global::renderEngine->scene()->allSceneGraphNodes();

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
                global::scriptEngine->queueScript(
                    "openspace.setPropertyValue('" +
                    std::string(AnchorProperty) + "', '" +
                    n->identifier() + "');",
                    scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                    scripting::ScriptEngine::ShouldSendToRemote::Yes
                );
                global::scriptEngine->queueScript(
                    "openspace.setPropertyValue('" +
                    std::string(RetargetAnchorProperty) + "', nil);",
                    scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                    scripting::ScriptEngine::ShouldSendToRemote::Yes
                );
            }
        }
    }

    ImGui::NewLine();
    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 10.f);

    const SceneGraphNode* currentFocus =
        global::navigationHandler->orbitalNavigator().anchorNode();

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
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('" + std::string(AnchorProperty) + "', '" +
            nodes[currentPosition]->identifier() + "');",
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('" +
            std::string(RetargetAnchorProperty) + "', nil);",
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }

    ImGui::SameLine();
    const bool pressed = ImGui::Button("Refocus");
    if (pressed) {
        global::scriptEngine->queueScript(
            "openspace.setPropertyValue('" +
            std::string(RetargetAnchorProperty) + "', nil);",
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }

    float interpolationTime = global::navigationHandler->interpolationTime();
    const bool interpolationTimeChanged = ImGui::SliderFloat(
        "Interpolation Time",
        &interpolationTime,
        0.f,
        10.f,
        "%.1f seconds"
    );

    if (interpolationTimeChanged) {
        global::navigationHandler->setInterpolationTime(interpolationTime);
    }

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);

    ImGui::Separator();
    ImGui::Separator();
    ImGui::Separator();

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);

    CaptionText("Time Controls");
    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 10.f);

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 10.f);

    ImGui::Text(
        "Current Date: %s",
        std::string(global::timeManager->time().UTC()).c_str()
    );

    constexpr int BufferSize = 256;
    static char Buffer[BufferSize];
    const bool dateChanged = ImGui::InputText(
        "Change Date",
        Buffer,
        BufferSize,
        ImGuiInputTextFlags_EnterReturnsTrue
    );
    if (dateChanged) {
        // No sync or send because time settings are always synced and sent to the
        // connected nodes and peers
        global::scriptEngine->queueScript(
            std::format("openspace.time.setTime([[{}]])", std::string_view(Buffer)),
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }

    showTooltip(
        "Entering a date here and confirming with ENTER sets the current simulation time "
        "to the entered date. The format of the date has to be either ISO 8601 "
        "YYYY-MM-DDThh:mm:ss (2017-08-27T04:00:00) or YYYY MMM DD hh:mm:ss "
        "(2017 MAY 01 12:00:00). The hours are in 24h and specified as UTC",
        _tooltipDelay
    );

    auto incrementTime = [shift = ImGui::GetIO().KeyShift](float days) {
        using namespace std::chrono;

        const float duration = global::timeManager->defaultTimeInterpolationDuration();

        const TimeKeyframeData predictedTime = global::timeManager->interpolate(
            global::windowDelegate->applicationTime() + duration
        );
        const double j2000 = predictedTime.time.j2000Seconds();
        const long long seconds = duration_cast<std::chrono::seconds>(
            std::chrono::hours(24) * std::abs(days)
        ).count();

        const double newTime = days < 0 ?
            j2000 - seconds :
            j2000 + seconds;

        if (shift) {
            // If any shift key is pressed we want to always jump to the time.
            // No sync or send because time settings are always synced and sent
            // to the connected nodes and peers
            global::scriptEngine->queueScript(
                "openspace.time.setTime(" + std::to_string(newTime) + ")",
                scripting::ScriptEngine::ShouldBeSynchronized::No,
                scripting::ScriptEngine::ShouldSendToRemote::No
            );
        }
        else {
            // No sync or send because time settings are always synced and sent
            // to the connected nodes and peers
            global::scriptEngine->queueScript(
                "openspace.time.interpolateTime(" + std::to_string(newTime) + ", " +
                std::to_string(duration) + ")",
                scripting::ScriptEngine::ShouldBeSynchronized::No,
                scripting::ScriptEngine::ShouldSendToRemote::No
            );
        }
    };

    const bool minusMonth = ImGui::Button("-Month");
    showTooltip("OBS: A month here equals 30 days", _tooltipDelay);
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
        std::string nowTime = std::string(Time::now().UTC());
        // UTC returns a string of the type YYYY MMM DDTHH:mm:ss.xxx
        // setTime doesn't like the T in it and wants a space instead
        nowTime[11] = ' ';

        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.setTime(\"" + nowTime + "\")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
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
    showTooltip("OBS: A month here equals 30 days", _tooltipDelay);

    ImGui::SetCursorPosY(ImGui::GetCursorPosY() + 20.f);
//
    {
        const float dt = static_cast<float>(global::timeManager->targetDeltaTime());
        if (_firstFrame) {
            const std::pair<double, std::string_view>& dtInfo = simplifyTime(dt);
            _deltaTime = static_cast<float>(dtInfo.first);
            _deltaTimeUnit = timeUnitFromString(dtInfo.second);

            _timeUnits = std::accumulate(
                openspace::TimeUnits.begin(),
                openspace::TimeUnits.end(),
                std::string(""),
                [](const std::string& a, const openspace::TimeUnit& unit) {
                    return std::format(
                        "{}{} / second", a, nameForTimeUnit(unit, true)
                    ) + '\0';
                }
            );
            _firstFrame = false;
        }

        _deltaTime = static_cast<float>(
            convertTime(dt, TimeUnit::Second, _deltaTimeUnit)
        );

        const bool valueChanged = ImGui::InputFloat(
            "##inputValueDeltaTime",
            &_deltaTime,
            1.f,
            100.f,
            "%.8f",
            ImGuiInputTextFlags_EnterReturnsTrue
        );
        ImGui::SameLine();

        int deltaTimeUnit = static_cast<int>(_deltaTimeUnit);
        const bool unitChanged = ImGui::Combo(
            "##inputUnit",
            &deltaTimeUnit,
            _timeUnits.c_str()
        );
        _deltaTimeUnit = static_cast<TimeUnit>(deltaTimeUnit);

        if (valueChanged) {
            // If the value changed, we want to change the delta time to the new value
            const double newDt = convertTime(
                _deltaTime,
                _deltaTimeUnit,
                TimeUnit::Second
            );

            // No sync or send because time settings are always synced and sent
            // to the connected nodes and peers
            global::scriptEngine->queueScript(
                "openspace.time.interpolateDeltaTime(" + std::to_string(newDt) + ")",
                scripting::ScriptEngine::ShouldBeSynchronized::No,
                scripting::ScriptEngine::ShouldSendToRemote::No
            );
        }
        if (unitChanged) {
            // If only the unit changes, we keep the delta time, but need to convert the
            // value to the new unit

            _deltaTime = static_cast<float>(
                convertTime(dt, TimeUnit::Second, _deltaTimeUnit)
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

            // No sync or send because time settings are always synced and sent
            // to the connected nodes and peers
            global::scriptEngine->queueScript(
                "openspace.time.setDeltaTime(" + std::to_string(newDeltaTime) + ")",
                scripting::ScriptEngine::ShouldBeSynchronized::No,
                scripting::ScriptEngine::ShouldSendToRemote::No
            );
        }
        if (!ImGui::IsItemActive() && !ImGui::IsItemClicked()) {
            if (_slidingDelta != 0.f) {
                // No sync or send because time settings are always synced and sent
                // to the connected nodes and peers
                global::scriptEngine->queueScript(
                    "openspace.time.setDeltaTime(" + std::to_string(_oldDeltaTime) + ")",
                    scripting::ScriptEngine::ShouldBeSynchronized::No,
                    scripting::ScriptEngine::ShouldSendToRemote::No
                );

            }
            _slidingDelta = 0.f;
        }
    }
    else {
        const bool accelerationDeltaChanged = ImGui::SliderFloat(
            "Delta Time Slider",
            &_accelerationDelta,
            -100.f,
            100.f,
            "%.6f"
        );

        if (accelerationDeltaChanged || ImGui::IsItemActive() || ImGui::IsItemClicked()) {
            // We want the value to change by _accelerationDelta every 100 real world ms
            const double newDeltaTime = convertTime(
                _deltaTime +
                    _accelerationDelta * global::windowDelegate->deltaTime() * 10,
                _deltaTimeUnit,
                TimeUnit::Second
            );

            // No sync or send because time settings are always synced and sent
            // to the connected nodes and peers
            global::scriptEngine->queueScript(
                "openspace.time.setDeltaTime(" + std::to_string(newDeltaTime) + ")",
                scripting::ScriptEngine::ShouldBeSynchronized::No,
                scripting::ScriptEngine::ShouldSendToRemote::No
            );
        }
        else {
            _accelerationDelta = 0.f;
        }

        _deltaTime -= _slidingDelta;
    }

    const bool isPaused = global::timeManager->isPaused();
    const bool pauseChanged = ImGui::Button(
        isPaused ? "Resume" : "Pause",
        { ImGui::GetWindowWidth() / 2 - 7.5f, 0.f }
    );
    if (pauseChanged) {
        global::scriptEngine->queueScript(
            "openspace.time.interpolateTogglePause()",
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }
    ImGui::SameLine();
    const bool invert = ImGui::Button(
        "Invert",
        { ImGui::GetWindowWidth() / 2 - 7.5f, 0.f }
    );
    if (invert) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(-1 * openspace.time.deltaTime());",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }

    const bool minusDs = ImGui::Button("-1d/s");
    if (minusDs) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(-24 * 60 * 60) + ")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
    ImGui::SameLine();

    const bool minusHs = ImGui::Button("-1h/s");
    if (minusHs) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(-60 * 60) + ")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
    ImGui::SameLine();

    const bool minusMs = ImGui::Button("-1min/s");
    if (minusMs) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(-60) + ")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
    ImGui::SameLine();

    const bool minusSs = ImGui::Button("-1s/s");
    if (minusSs) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(-1) + ")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
    ImGui::SameLine();

    const bool zero = ImGui::Button("0");
    if (zero) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(0) + ")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
    ImGui::SameLine();


    const bool plusSs = ImGui::Button("+1s/s");
    if (plusSs) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(1) + ")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
    ImGui::SameLine();

    const bool plusMs = ImGui::Button("1min/s");
    if (plusMs) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(60) + ")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
    ImGui::SameLine();

    const bool plusHs = ImGui::Button("1h/s");
    if (plusHs) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(60 * 60) + ")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
    ImGui::SameLine();

    const bool plusDs = ImGui::Button("1d/s");
    if (plusDs) {
        // No sync or send because time settings are always synced and sent
        // to the connected nodes and peers
        global::scriptEngine->queueScript(
            "openspace.time.interpolateDeltaTime(" + std::to_string(24 * 60 * 60) + ")",
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }

    ImGui::End();
}

} // namespace openspace::gui
