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

#include <modules/base/dashboard/dashboarditemmission.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/mission/mission.h>
#include <openspace/mission/missionmanager.h>
#include <openspace/util/timemanager.h>

#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

#include <stack>

namespace {
    const char* KeyFontMono = "Mono";

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

DashboardItemMission::DashboardItemMission(ghoul::Dictionary dictionary)
    : DashboardItem("Mission")
    , _font(OsEng.fontManager().font(KeyFontMono, 10))
{}

void DashboardItemMission::render(glm::vec2& penPosition) {
    if (MissionManager::ref().hasCurrentMission()) {
        double currentTime = OsEng.timeManager().time().j2000Seconds();
        const Mission& mission = MissionManager::ref().currentMission();

        if (mission.phases().size() > 0) {
            static const glm::vec4 nextMissionColor(0.7, 0.3, 0.3, 1);
            //static const glm::vec4 missionProgressColor(0.4, 1.0, 1.0, 1);
            static const glm::vec4 currentMissionColor(0.0, 0.5, 0.5, 1);
            static const glm::vec4 missionProgressColor = currentMissionColor;
           // static const glm::vec4 currentLeafMissionColor = missionProgressColor;
            static const glm::vec4 nonCurrentMissionColor(0.3, 0.3, 0.3, 1);

            // Add spacing
            RenderFontCr(*_font, penPosition, nonCurrentMissionColor, " ");

            auto phaseTrace = mission.phaseTrace(currentTime);

            if (phaseTrace.size()) {
                const MissionPhase& phase = phaseTrace.back().get();
                std::string title = "Current Mission Phase: " + phase.name();
                RenderFontCr(
                    *_font,
                    penPosition,
                    missionProgressColor,
                    title.c_str()
                );
                double remaining = phase.timeRange().end - currentTime;
                float t = static_cast<float>(
                    1.0 - remaining / phase.timeRange().duration()
                );
                std::string progress = progressToStr(25, t);
                RenderFontCr(*_font, penPosition, missionProgressColor,
                   "%.0f s %s %.1f %%", remaining, progress.c_str(), t * 100);
            }
            else {
                RenderFontCr(
                    *_font,
                    penPosition,
                    nextMissionColor,
                    "Next Mission:"
                );
                double remaining = mission.timeRange().start - currentTime;
                RenderFontCr(*_font, penPosition, nextMissionColor,
                    "%.0f s", remaining);
            }

            bool showAllPhases = false;

            typedef std::pair<const MissionPhase*, int> PhaseWithDepth;
            std::stack<PhaseWithDepth> S;
            int pixelIndentation = 20;
            S.push({ &mission, 0 });
            while (!S.empty()) {
                const MissionPhase* phase = S.top().first;
                int depth = S.top().second;
                S.pop();

                bool isCurrentPhase = phase->timeRange().includes(currentTime);

                penPosition.x += depth * pixelIndentation;
                if (isCurrentPhase) {
                    double remaining = phase->timeRange().end - currentTime;
                    float t = static_cast<float>(
                        1.0 - remaining / phase->timeRange().duration()
                    );
                    std::string progress = progressToStr(25, t);
                    RenderFontCr(*_font, penPosition, currentMissionColor,
                        "%s  %s %.1f %%",
                        phase->name().c_str(),
                        progress.c_str(),
                        t * 100
                        );
                }
                else {
                    if (!phase->name().empty()) {
                        RenderFontCr(
                            *_font,
                            penPosition,
                            nonCurrentMissionColor,
                            phase->name().c_str()
                        );
                    }
                }
                penPosition.x -= depth * pixelIndentation;

                if (isCurrentPhase || showAllPhases) {
                    // phases are sorted increasingly by start time, and will be
                    // popped last-in-first-out from the stack, so add them in
                    // reversed order.
                    int indexLastPhase = static_cast<int>(
                        phase->phases().size()
                    ) - 1;
                    for (int i = indexLastPhase; 0 <= i; --i) {
                        S.push({ &phase->phases()[i], depth + 1 });
                    }
                }
            }
        }
    }
}

glm::vec2 DashboardItemMission::size() const {
    // @TODO fix this up ---abock
    return { 0.f, 0.f };
}

} // namespace openspace
