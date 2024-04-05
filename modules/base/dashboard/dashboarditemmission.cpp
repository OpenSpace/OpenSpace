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

#include <modules/base/dashboard/dashboarditemmission.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/mission/mission.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/util/timemanager.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/profiling.h>
#include <stack>

namespace {
    std::string progressToStr(int size, double t) {
        std::string progress = "|";
        int g = static_cast<int>((t * (size - 1)) + 1);
        g = std::max(g, 0);
        for (int i = 0; i < g; i++) {
            progress.append("-");
        }
        progress.append(">");
        for (int i = 0; i < size - g; i++) {
            progress.append(" ");
        }
        progress.append("|");
        return progress;
    }
} // namespace

namespace openspace {

documentation::Documentation DashboardItemMission::Documentation() {
    documentation::Documentation doc = DashboardTextItem::Documentation();
    doc.name = "DashboardItemMission";
    doc.id = "base_dashboarditem_mission";
    return doc;
}

DashboardItemMission::DashboardItemMission(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary, 15.f)
{}

void DashboardItemMission::render(glm::vec2& penPosition) {
    ZoneScoped;

    if (!global::missionManager->hasCurrentMission()) {
        return;
    }
    const double currentTime = global::timeManager->time().j2000Seconds();
    const Mission& mission = global::missionManager->currentMission();

    if (mission.phases().empty()) {
        return;
    }
    static constexpr glm::vec4 nextMissionColor = glm::vec4(0.7f, 0.3f, 0.3f, 1.f);
    static constexpr glm::vec4 currentMissionColor = glm::vec4(0.f, 0.5f, 0.5f, 1.f);
    static constexpr glm::vec4 missionProgressColor = currentMissionColor;
    static constexpr glm::vec4 nonCurrentMissionColor = glm::vec4(0.3f, 0.3f, 0.3f, 1.f);

    // Add spacing
    RenderFont(
        *_font,
        penPosition,
        " ",
        nonCurrentMissionColor,
        ghoul::fontrendering::CrDirection::Down
    );

    MissionPhase::Trace phaseTrace = mission.phaseTrace(currentTime);
    if (!phaseTrace.empty()) {
        const MissionPhase& phase = phaseTrace.back().get();
        const std::string title = "Current Mission Phase: " + phase.name();
        penPosition.y -= _font->height();
        RenderFont(*_font, penPosition, title, missionProgressColor);
        double remaining = phase.timeRange().end - currentTime;
        const float t = static_cast<float>(
            1.0 - remaining / phase.timeRange().duration()
        );
        std::string progress = progressToStr(25, t);
        penPosition.y -= _font->height();
        RenderFont(
            *_font,
            penPosition,
            std::format("{:.0f} s {:s} {:.1f} %", remaining, progress, t * 100),
            missionProgressColor
        );
    }
    else {
        penPosition.y -= _font->height();
        RenderFont(*_font, penPosition, "Next Mission:", nextMissionColor);
        const double remaining = mission.timeRange().start - currentTime;
        penPosition.y -= _font->height();
        RenderFont(
            *_font,
            penPosition,
            std::format("{:.0f} s", remaining),
            nextMissionColor
        );
    }

    constexpr bool ShowAllPhases = false;

    using PhaseWithDepth = std::pair<const MissionPhase*, int>;
    std::stack<PhaseWithDepth> S;

    constexpr int PixelIndentation = 20;
    S.emplace(&mission, 0);
    while (!S.empty()) {
        const MissionPhase* phase = S.top().first;
        const int depth = S.top().second;
        S.pop();

        const bool isCurrentPhase = phase->timeRange().includes(currentTime);

        penPosition.x += depth * PixelIndentation;
        if (isCurrentPhase) {
            const double remaining = phase->timeRange().end - currentTime;
            const float t = static_cast<float>(
                1.0 - remaining / phase->timeRange().duration()
            );
            const std::string progress = progressToStr(25, t);
            RenderFont(
                *_font,
                penPosition,
                std::format(
                    "{:s}  {:s} {:.1f} %",
                    phase->name(),progress,t * 100
                ),
                currentMissionColor
            );
            penPosition.y -= _font->height();
        }
        else {
            if (!phase->name().empty()) {
                RenderFont(
                    *_font,
                    penPosition,
                    phase->name(),
                    nonCurrentMissionColor
                );
                penPosition.y -= _font->height();
            }
        }
        penPosition.x -= depth * PixelIndentation;

        if (isCurrentPhase || ShowAllPhases) {
            // phases are sorted increasingly by start time, and will be
            // popped last-in-first-out from the stack, so add them in
            // reversed order.
            const int indexLastPhase = static_cast<int>(phase->phases().size()) - 1;
            for (int i = indexLastPhase; 0 <= i; --i) {
                S.emplace(&phase->phases()[i], depth + 1);
            }
        }
    }
}

glm::vec2 DashboardItemMission::size() const {
    ZoneScoped;

    // @TODO fix this up ---abock
    return { 0.f, 0.f };
}

} // namespace openspace
