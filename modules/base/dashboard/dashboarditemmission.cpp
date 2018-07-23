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
#include <stack>

namespace {
    constexpr const char* KeyFontMono = "Mono";
    constexpr const float DefaultFontSize = 15.f;

    constexpr openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used."
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "This value determines the size of the font that is used to render the date."
    };

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
    using namespace documentation;
    return {
        "DashboardItem Mission",
        "base_dashboarditem_mission",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemMission"),
                Optional::No
            },
            {
                FontNameInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                FontNameInfo.description
            },
            {
                FontSizeInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                FontSizeInfo.description
            }
        }
    };
}

DashboardItemMission::DashboardItemMission(const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemMission"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    _fontName.onChange([this](){
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(
            dictionary.value<double>(FontSizeInfo.identifier)
        );
    }
    _fontSize.onChange([this](){
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    _font = global::fontManager.font(_fontName, _fontSize);
}

void DashboardItemMission::render(glm::vec2& penPosition) {
    if (global::missionManager.hasCurrentMission()) {
        double currentTime = global::timeManager.time().j2000Seconds();
        const Mission& mission = global::missionManager.currentMission();

        if (!mission.phases().empty()) {
            static const glm::vec4 nextMissionColor(0.7f, 0.3f, 0.3f, 1.f);
            static const glm::vec4 currentMissionColor(0.f, 0.5f, 0.5f, 1.f);
            static const glm::vec4 missionProgressColor = currentMissionColor;
            static const glm::vec4 nonCurrentMissionColor(0.3f, 0.3f, 0.3f, 1.f);

            // Add spacing
            RenderFont(
                *_font,
                penPosition,
                " ",
                nonCurrentMissionColor,
                ghoul::fontrendering::CrDirection::Down
            );

            auto phaseTrace = mission.phaseTrace(currentTime);

            if (!phaseTrace.empty()) {
                const MissionPhase& phase = phaseTrace.back().get();
                const std::string title = "Current Mission Phase: " + phase.name();
                penPosition.y -= _font->height();
                RenderFont(*_font, penPosition, title, missionProgressColor);
                double remaining = phase.timeRange().end - currentTime;
                float t = static_cast<float>(
                    1.0 - remaining / phase.timeRange().duration()
                );
                std::string progress = progressToStr(25, t);
                penPosition.y -= _font->height();
                RenderFont(
                    *_font,
                    penPosition,
                    fmt::format("{:.0f} s {:s} {:.1f} %", remaining, progress, t * 100),
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
                    fmt::format("{:.0f} s", remaining),
                    nextMissionColor
                );
            }

            bool showAllPhases = false;

            using PhaseWithDepth = std::pair<const MissionPhase*, int>;
            std::stack<PhaseWithDepth> S;

            constexpr const int PixelIndentation = 20;
            S.push({ &mission, 0 });
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
                    penPosition.y -= _font->height();
                    RenderFont(
                        *_font,
                        penPosition,
                        fmt::format(
                            "{:s}  {:s} {:.1f} %",
                            phase->name(),progress,t * 100
                        ),
                        currentMissionColor
                    );
                }
                else {
                    if (!phase->name().empty()) {
                        penPosition.y -= _font->height();
                        RenderFont(
                            *_font,
                            penPosition,
                            phase->name(),
                            nonCurrentMissionColor
                        );
                    }
                }
                penPosition.x -= depth * PixelIndentation;

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
