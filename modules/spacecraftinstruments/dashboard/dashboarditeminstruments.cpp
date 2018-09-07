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
#include <chrono>

namespace {
    constexpr const char* KeyFontMono = "Mono";
    constexpr const float DefaultFontSize = 10.f;

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

    constexpr openspace::properties::Property::PropertyInfo ActiveColorInfo = {
        "ActiveColor",
        "Active Color",
        "This value determines the color that the active instrument is rendered in. "
        "Shortly after activation, the used color is mixture of this and the flash "
        "color. The default value is (0.6, 1.0, 0.0)."
    };

    constexpr openspace::properties::Property::PropertyInfo FlashColorInfo = {
        "FlashColor",
        "Flash Color",
        "This value determines the color that is used shortly after an instrument "
        "activation. The default value is (0.9, 1.0, 0.75)"
    };

    std::string progressToStr(int size, double t) {
        std::string progress = "|";
        const int g = std::max(0, static_cast<int>((t * (size - 1)) + 1));
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

    glm::vec2 addToBoundingbox(glm::vec2 lhs, glm::vec2 rhs) {
        return { std::max(lhs.x, rhs.x), lhs.y + rhs.y };
    }
} // namespace

namespace openspace {

documentation::Documentation DashboardItemInstruments::Documentation() {
    using namespace documentation;
    return {
        "DashboardItem Instruments",
        "spacecraftinstruments_dashboarditem_instuments",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemInstruments"),
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
            },
            {
                ActiveColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                ActiveColorInfo.description
            },
            {
                FlashColorInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                FlashColorInfo.description
            }
        }
    };
}

DashboardItemInstruments::DashboardItemInstruments(const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
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
    , _font(global::fontManager.font(KeyFontMono, 10))
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemInstruments"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = static_cast<float>(dictionary.value<double>(FontSizeInfo.identifier));
    }

    _fontName.onChange([this]() {
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    _fontSize.onChange([this]() {
        _font = global::fontManager.font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    _activeColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_activeColor);
    _activeFlash.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_activeFlash);

    _font = global::fontManager.font(_fontName, _fontSize);
}

void DashboardItemInstruments::render(glm::vec2& penPosition) {
    double currentTime = global::timeManager.time().j2000Seconds();

    if (!ImageSequencer::ref().isReady()) {
        return;
    }
    ImageSequencer& sequencer = ImageSequencer::ref();

    penPosition.y -= 25.f;

    glm::vec4 targetColor(0.f, 0.75f, 1.f, 1.f);

    double remaining = sequencer.nextCaptureTime(currentTime) - currentTime;
    const float t = static_cast<float>(
        1.0 - remaining / sequencer.intervalLength(currentTime)
    );

    if (remaining > 0) {
        RenderFont(
            *_font,
            penPosition,
            "Next instrument activity:",
            glm::vec4(glm::mix(_activeColor.value(), _activeFlash.value(), t), 1.f),
            ghoul::fontrendering::CrDirection::Down
        );

        std::pair<double, std::string> remainingConv = simplifyTime(remaining);

        // If the remaining time is below 5 minutes, we switch over to seconds display
        if (remaining < 5 * 60) {
            remainingConv = { remaining, "seconds" };
        }

        const int Size = 25;
        int p = std::max(static_cast<int>((t * (Size - 1)) + 1), 0);
        RenderFont(
            *_font,
            penPosition,
            fmt::format(
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
            sequencer.nextCaptureTime(global::timeManager.time().j2000Seconds()),
            "YYYY MON DD HR:MN:SC"
        );

        RenderFont(
            *_font,
            penPosition,
            fmt::format("Data acquisition time: {}", str),
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
        fmt::format(
            "Next image: [{:02d}:{:02d}:{:02d}]",
            tlh.count(), tlm.count(), tls.count()
        ),
        targetColor,
        ghoul::fontrendering::CrDirection::Down
    );

    penPosition.y -= _font->height();

    const std::vector<std::pair<std::string, bool>>& activeMap =
        sequencer.activeInstruments(currentTime);

    glm::vec4 firing(0.58 - t, 1 - t, 1 - t, 1);
    glm::vec4 notFiring(0.5, 0.5, 0.5, 1);

    RenderFont(
        *_font,
        penPosition,
        "Active Instruments:",
        glm::vec4(_activeColor.value(), 1.f),
        ghoul::fontrendering::CrDirection::Down
    );

    for (const std::pair<std::string, bool>& m : activeMap) {
        if (m.second) {
            RenderFont(*_font, penPosition, "|", glm::vec4(0.3, 0.3, 0.3, 1));
            if (m.first == "NH_LORRI") {
                RenderFont(*_font, penPosition, " + ", firing);
            }
            RenderFont(*_font, penPosition, "  |", glm::vec4(0.3, 0.3, 0.3, 1));
            RenderFont(*_font,
                penPosition,
                fmt::format("    {:5s}", m.first),
                glm::vec4(_activeColor.value(), 1.f),
                ghoul::fontrendering::CrDirection::Down
            );
        }
        else {
            RenderFont(*_font, penPosition, "| |", glm::vec4(0.3, 0.3, 0.3, 1));
            RenderFont(
                *_font,
                penPosition,
                fmt::format("    {:5s}", m.first),
                glm::vec4(0.3, 0.3, 0.3, 1),
                ghoul::fontrendering::CrDirection::Down
            );
        }
    }
}

glm::vec2 DashboardItemInstruments::size() const {
    glm::vec2 size = { 0.f, 0.f };
    //return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
    double currentTime = global::timeManager.time().j2000Seconds();

    if (!ImageSequencer::ref().isReady()) {
        return { 0.f, 0.f };
    }
    ImageSequencer& sequencer = ImageSequencer::ref();

    const double remaining = sequencer.nextCaptureTime(currentTime) - currentTime;
    const float t = static_cast<float>(
        1.0 - remaining / sequencer.intervalLength(currentTime)
    );

    const std::string& str = SpiceManager::ref().dateFromEphemerisTime(
        sequencer.nextCaptureTime(currentTime),
        "YYYY MON DD HR:MN:SC"
    );


    if (remaining > 0) {
        using FR = ghoul::fontrendering::FontRenderer;
        FR& renderer = FR::defaultRenderer();
        std::string progress = progressToStr(25, t);

        size = addToBoundingbox(
            size,
            renderer.boundingBox(*_font, "Next instrument activity:").boundingBox
        );

        size = addToBoundingbox(
            size,
            renderer.boundingBox(
                *_font,
                fmt::format("{:.0f} s {:s} {:.1f} %", remaining, progress, t * 100)
            ).boundingBox
        );

        size = addToBoundingbox(
            size,
            renderer.boundingBox(
                *_font,
                fmt::format("Data acquisition time: {}", str)
            ).boundingBox
        );
    }
    std::pair<double, std::string> nextTarget = sequencer.nextTarget(currentTime);
    std::pair<double, std::string> currentTarget = sequencer.currentTarget(currentTime);

    if (currentTarget.first <= 0.0) {
        return size;
    }

    using FR = ghoul::fontrendering::FontRenderer;
    FR& renderer = FR::defaultRenderer();

    const int timeleft = static_cast<int>(nextTarget.first - currentTime);

    const int hour = timeleft / 3600;
    int second = timeleft % 3600;
    const int minute = second / 60;
    second = second % 60;


    std::string hh;
    if (hour < 10) {
        hh = "0";
    }
    std::string mm;
    if (minute < 10) {
        mm = "0";
    }
    std::string ss;
    if (second < 10) {
        ss = "0";
    }

    hh.append(std::to_string(hour));
    mm.append(std::to_string(minute));
    ss.append(std::to_string(second));

    size = addToBoundingbox(
        size,
        renderer.boundingBox(
            *_font,
            fmt::format("Data acquisition adjacency: [{}:{}:{}]", hh, mm, ss)
        ).boundingBox
    );

    size.y += _font->height();

    size = addToBoundingbox(
        size,
        ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
            *_font,
            "Active Instruments:"
        ).boundingBox
    );
    return size;
}

} // namespace openspace
