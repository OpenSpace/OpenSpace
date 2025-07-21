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

#include <modules/spacecraftinstruments/dashboard/dashboarditeminstruments.h>

#include <modules/spacecraftinstruments/util/imagesequencer.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/profiling.h>
#include <chrono>

namespace {
    constexpr openspace::properties::Property::PropertyInfo ActiveColorInfo = {
        "ActiveColor",
        "Active Color",
        "This value determines the color that the active instrument is rendered in. "
        "Shortly after activation, the used color is mixture of this and the flash "
        "color. The default value is (0.6, 1.0, 0.0).",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FlashColorInfo = {
        "FlashColor",
        "Flash Color",
        "This value determines the color that is used shortly after an instrument "
        "activation. The default value is (0.9, 1.0, 0.75).",
        openspace::properties::Property::Visibility::User
    };

    // This dashboard item shows information about the status of individual instruments
    // onboard a spacecraft with regards to upcoming image capture. An image sequence has
    // to be registered in order to be able to show the necessary information. The
    // dashboard item shows a visual representation on how much time has passed since the
    // previous image capture and how much time remains until the instrument captures the
    // next image.
    struct [[codegen::Dictionary(DashboardItemInstruments)]] Parameters {
        // [[codegen::verbatim(ActiveColorInfo.description)]]
        std::optional<glm::vec3> activeColor [[codegen::color()]];

        // [[codegen::verbatim(FlashColorInfo.description)]]
        std::optional<glm::vec3> flashColor [[codegen::color()]];
    };
#include "dashboarditeminstruments_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemInstruments::Documentation() {
    return codegen::doc<Parameters>(
        "spacecraftinstruments_dashboarditem_instuments",
        DashboardTextItem::Documentation()
    );
}

DashboardItemInstruments::DashboardItemInstruments(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _activeColor(
        ActiveColorInfo,
        glm::vec3(0.6f, 1.f, 0.f),
        glm::vec3(0.f),
        glm::vec3(1.f)
    )
    , _activeFlash(FlashColorInfo,
        glm::vec3(0.9f, 1.f, 0.75f),
        glm::vec3(0.f),
        glm::vec3(1.f)
    )
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _activeColor.setViewOption(properties::Property::ViewOptions::Color);
    _activeColor = p.activeColor.value_or(_activeColor);
    addProperty(_activeColor);
    _activeFlash.setViewOption(properties::Property::ViewOptions::Color);
    _activeFlash = p.flashColor.value_or(_activeFlash);
    addProperty(_activeFlash);
}

void DashboardItemInstruments::update() {}

void DashboardItemInstruments::render(glm::vec2& penPosition) {
    ZoneScoped;

    const double currentTime = global::timeManager->time().j2000Seconds();

    if (!ImageSequencer::ref().isReady()) {
        return;
    }
    ImageSequencer& sequencer = ImageSequencer::ref();

    penPosition.y -= _font->height();

    constexpr glm::vec4 targetColor(0.f, 0.75f, 1.f, 1.f);

    const double previous = sequencer.prevCaptureTime(currentTime);
    const double next = sequencer.nextCaptureTime(currentTime);
    const double remaining = next - currentTime;
    float t = static_cast<float>(1.0 - remaining / (next - previous));
    t = std::clamp(t, 0.f, 1.f);

    if (remaining > 0.0) {
        RenderFont(
            *_font,
            penPosition,
            "Next instrument activity:",
            glm::vec4(glm::mix(_activeColor.value(), _activeFlash.value(), t), 1.f),
            ghoul::fontrendering::CrDirection::Down
        );

        std::pair<double, std::string_view> remainingConv = simplifyTime(remaining);

        // If the remaining time is below 5 minutes, we switch over to seconds display
        if (remaining < 5 * 60) {
            remainingConv = std::pair(remaining, "seconds");
        }

        const int Size = 25;
        const int p = std::max(static_cast<int>((t * (Size - 1)) + 1), 0);
        RenderFont(
            *_font,
            penPosition,
            std::format(
                "{:4.0f} {:s} |{:s}>{:s}| {:.1f} %",
                remainingConv.first,
                remainingConv.second,
                std::string(p, '-'),
                std::string(Size - p, ' '),
                t * 100
            ),
            glm::vec4(glm::mix(_activeColor.value(), _activeFlash.value(), t), 1.f),
            ghoul::fontrendering::CrDirection::Down
        );

        std::string str = SpiceManager::ref().dateFromEphemerisTime(
            sequencer.nextCaptureTime(global::timeManager->time().j2000Seconds()),
            "YYYY MON DD HR:MN:SC"
        );

        RenderFont(
            *_font,
            penPosition,
            std::format("Data acquisition time: {}", str),
            glm::vec4(_activeColor.value(), 1.f),
            ghoul::fontrendering::CrDirection::Down
        );
    }
    const std::pair<double, std::string>& nextTarget = sequencer.nextTarget(currentTime);
    const std::pair<double, std::string>& currentTarget =
        sequencer.currentTarget(currentTime);

    if (currentTarget.first <= 0.0) {
        return;
    }
    using namespace std::chrono;
    seconds tls = seconds(static_cast<int>(nextTarget.first - currentTime));

    const hours tlh = duration_cast<hours>(tls);
    tls -= tlh;
    const minutes tlm = duration_cast<minutes>(tls);
    tls -= tlm;

    RenderFont(
        *_font,
        penPosition,
        std::format(
            "Next image: [{:02d}:{:02d}:{:02d}]", tlh.count(), tlm.count(), tls.count()
        ),
        targetColor,
        ghoul::fontrendering::CrDirection::Down
    );

    penPosition.y -= _font->height();

    const std::vector<std::pair<std::string, bool>>& activeMap =
        sequencer.activeInstruments(currentTime);

    const glm::vec4 firing = glm::vec4(0.58f - t, 1.f - t, 1.f - t, 1.f);

    RenderFont(
        *_font,
        penPosition,
        "Active Instruments:",
        glm::vec4(_activeColor.value(), 1.f),
        ghoul::fontrendering::CrDirection::Down
    );

    for (const std::pair<std::string, bool>& m : activeMap) {
        if (m.second) {
            RenderFont(*_font, penPosition, "|", glm::vec4(0.3f, 0.3f, 0.3f, 1.f));
            if (m.first == "NH_LORRI") {
                RenderFont(*_font, penPosition, " + ", firing);
            }
            RenderFont(*_font, penPosition, "  |", glm::vec4(0.3f, 0.3f, 0.3f, 1.f));
            RenderFont(*_font,
                penPosition,
                std::format("    {:5s}", m.first),
                glm::vec4(_activeColor.value(), 1.f),
                ghoul::fontrendering::CrDirection::Down
            );
        }
        else {
            RenderFont(*_font, penPosition, "| |", glm::vec4(0.3f, 0.3f, 0.3f, 1.f));
            RenderFont(
                *_font,
                penPosition,
                std::format("    {:5s}", m.first),
                glm::vec4(0.3f, 0.3f, 0.3f, 1.f),
                ghoul::fontrendering::CrDirection::Down
            );
        }
    }

    // The last item added a CR but we want to undo it since it's the next item's
    // responsibility to move down
    penPosition.y += _font->height();
}

} // namespace openspace
