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
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/timemanager.h>

#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

#include <chrono>

namespace {
    const char* KeyFontMono = "Mono";
    const float DefaultFontSize = 10.f;

    static const openspace::properties::Property::PropertyInfo FontNameInfo = {
        "FontName",
        "Font Name",
        "This value is the name of the font that is used. It can either refer to an "
        "internal name registered previously, or it can refer to a path that is used."
    };

    static const openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "This value determines the size of the font that is used to render the date."
    };

    static const openspace::properties::Property::PropertyInfo ActiveColorInfo = {
        "ActiveColor",
        "Active Color",
        "This value determines the color that the active instrument is rendered in. "
        "Shortly after activation, the used color is mixture of this and the flash "
        "color. The default value is (0.6, 1.0, 0.0)."
    };

    static const openspace::properties::Property::PropertyInfo FlashColorInfo = {
        "FlashColor",
        "Flash Color",
        "This value determines the color that is used shortly after an instrument "
        "activation. The default value is (0.9, 1.0, 0.75)"
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

    glm::vec2 addToBoundingbox(glm::vec2 lhs, glm::vec2 rhs) {
        return {
            std::max(lhs.x, rhs.x),
            lhs.y + rhs.y
        };
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

DashboardItemInstruments::DashboardItemInstruments(ghoul::Dictionary dictionary)
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
    , _font(OsEng.fontManager().font(KeyFontMono, 10))
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
        _fontSize = static_cast<float>(
            dictionary.value<double>(FontSizeInfo.identifier)
            );
    }

    _fontName.onChange([this]() {
        _font = OsEng.fontManager().font(_fontName, _fontSize);
    });
    addProperty(_fontName);

    _fontSize.onChange([this]() {
        _font = OsEng.fontManager().font(_fontName, _fontSize);
    });
    addProperty(_fontSize);

    _activeColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_activeColor);
    _activeFlash.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_activeFlash);

    _font = OsEng.fontManager().font(_fontName, _fontSize);
}

void DashboardItemInstruments::render(glm::vec2& penPosition) {
    double currentTime = OsEng.timeManager().time().j2000Seconds();

    if (ImageSequencer::ref().isReady()) {
        penPosition.y -= 25.f;

        glm::vec4 targetColor(0.f, 0.75f, 1.f, 1.f);

        double remaining = ImageSequencer::ref().getNextCaptureTime() - currentTime;
        float t = static_cast<float>(
            1.0 - remaining / ImageSequencer::ref().getIntervalLength()
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
                    remainingConv.second.c_str(),
                    std::string(p, '-').c_str(),
                    std::string(Size - p, ' ').c_str(),
                    t * 100
                ),
                glm::vec4(glm::mix(_activeColor.value(), _activeFlash.value(), t), 1.f),
                ghoul::fontrendering::CrDirection::Down
            );

            std::string str = SpiceManager::ref().dateFromEphemerisTime(
                ImageSequencer::ref().getNextCaptureTime(),
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
        std::pair<double, std::string> nextTarget = ImageSequencer::ref().getNextTarget();
        std::pair<double, std::string> currentTarget =
            ImageSequencer::ref().getCurrentTarget();

        if (currentTarget.first > 0.0) {
            using namespace std::chrono;
            seconds tls = seconds(static_cast<int>(nextTarget.first - currentTime));

            hours tlh = duration_cast<hours>(tls);
            tls -= tlh;
            minutes tlm = duration_cast<minutes>(tls);
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

            std::map<std::string, bool> activeMap =
                ImageSequencer::ref().getActiveInstruments();
            glm::vec4 firing(0.58 - t, 1 - t, 1 - t, 1);
            glm::vec4 notFiring(0.5, 0.5, 0.5, 1);

            RenderFont(
                *_font,
                penPosition,
                "Active Instruments:",
                glm::vec4(_activeColor.value(), 1.f),
                ghoul::fontrendering::CrDirection::Down
            );

            for (auto m : activeMap) {
                if (m.second == false) {
                    RenderFont(
                        *_font,
                        penPosition,
                        "| |",
                        glm::vec4(0.3, 0.3, 0.3, 1)
                    );
                    RenderFont(
                        *_font,
                        penPosition,
                        fmt::format("    {:5s}", m.first),
                        glm::vec4(0.3, 0.3, 0.3, 1),
                        ghoul::fontrendering::CrDirection::Down
                    );

                }
                else {
                    RenderFont(*_font,
                        penPosition,
                        "|",
                        glm::vec4(0.3, 0.3, 0.3, 1)
                    );
                    if (m.first == "NH_LORRI") {
                        RenderFont(
                            *_font,
                            penPosition,
                            " + ",
                            firing
                        );
                    }
                    RenderFont(*_font,
                        penPosition,
                        "  |",
                        glm::vec4(0.3, 0.3, 0.3, 1)
                    );
                    RenderFont(*_font,
                        penPosition,
                        fmt::format("    {:5s}", m.first),
                        glm::vec4(_activeColor.value(), 1.f),
                        ghoul::fontrendering::CrDirection::Down
                    );
                }
            }
        }
    }
}

glm::vec2 DashboardItemInstruments::size() const {
    glm::vec2 size = { 0.f, 0.f };
    //return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
    bool hasNewHorizons = OsEng.renderEngine().scene()->sceneGraphNode("NewHorizons");
    double currentTime = OsEng.timeManager().time().j2000Seconds();

    if (ImageSequencer::ref().isReady()) {
        glm::vec4 targetColor(0.f, 0.75f, 1.f, 1.f);

        if (hasNewHorizons) {
            try {
                double lt;
                glm::dvec3 p = SpiceManager::ref().targetPosition(
                    "PLUTO",
                    "NEW HORIZONS",
                    "GALACTIC",
                    {},
                    currentTime,
                    lt
                );
                float a, b;
                glm::dvec3 radii;
                SpiceManager::ref().getValue("PLUTO", "RADII", radii);
                a = static_cast<float>(radii.x);
                b = static_cast<float>(radii.y);
                float radius = (a + b) / 2.f;
                float distToSurf = glm::length(glm::vec3(p)) - radius;

                size = addToBoundingbox(
                    size,
                    ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                        *_font,
                        fmt::format("Distance to Pluto: {:.1f} (KM)", distToSurf)
                    ).boundingBox
                );
            }
            catch (...) {
                // @CLEANUP:  This is bad as it will discard all exceptions
                // without telling us about it! ---abock
            }
        }

        double remaining = openspace::ImageSequencer::ref().getNextCaptureTime() -
            currentTime;
        float t = static_cast<float>(
            1.0 - remaining / openspace::ImageSequencer::ref().getIntervalLength()
            );

        std::string str = SpiceManager::ref().dateFromEphemerisTime(
            ImageSequencer::ref().getNextCaptureTime(),
            "YYYY MON DD HR:MN:SC"
        );


        if (remaining > 0) {
            std::string progress = progressToStr(25, t);

            size = addToBoundingbox(
                size,
                ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                    *_font,
                    "Next instrument activity:"
                ).boundingBox
            );

            size = addToBoundingbox(
                size,
                ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                    *_font,
                    fmt::format(
                        "{:.0f} s {:s} {:.1f} %",
                        remaining,
                        progress.c_str(), t * 100
                    )
                ).boundingBox
            );

            size = addToBoundingbox(
                size,
                ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                    *_font,
                    fmt::format("Data acquisition time: {}", str)
                ).boundingBox
            );
        }
        std::pair<double, std::string> nextTarget = ImageSequencer::ref().getNextTarget();
        std::pair<double, std::string> currentTarget =
            ImageSequencer::ref().getCurrentTarget();

        if (currentTarget.first > 0.0) {
            int timeleft = static_cast<int>(nextTarget.first - currentTime);

            int hour = timeleft / 3600;
            int second = timeleft % 3600;
            int minute = second / 60;
            second = second % 60;

            std::string hh, mm, ss;

            if (hour   < 10)
                hh.append("0");
            if (minute < 10)
                mm.append("0");
            if (second < 10)
                ss.append("0");

            hh.append(std::to_string(hour));
            mm.append(std::to_string(minute));
            ss.append(std::to_string(second));

            size = addToBoundingbox(
                size,
                ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                    *_font,
                    fmt::format(
                        "Data acquisition adjacency: [{}:{}:{}]",
                        hh.c_str(), mm.c_str(), ss.c_str()
                    )


                ).boundingBox
            );

            size.y += _font->height();

            std::map<std::string, bool> activeMap =
                ImageSequencer::ref().getActiveInstruments();
            glm::vec4 firing(0.58 - t, 1 - t, 1 - t, 1);
            glm::vec4 notFiring(0.5, 0.5, 0.5, 1);

            size = addToBoundingbox(
                size,
                ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                    *_font,
                    "Active Instruments:"
                ).boundingBox
            );

            // @TODO:  Fix this up ---abock

            //for (auto m : activeMap) {
            //    if (m.second == false) {
            //        RenderFont(*_font,
            //            penPosition,
            //            glm::vec4(0.3, 0.3, 0.3, 1),
            //            "| |"
            //        );
            //        RenderFontCr(*_font,
            //            penPosition,
            //            glm::vec4(0.3, 0.3, 0.3, 1),
            //            "    %5s",
            //            m.first.c_str()
            //        );

            //    }
            //    else {
            //        RenderFont(*_font,
            //            penPosition,
            //            glm::vec4(0.3, 0.3, 0.3, 1),
            //            "|"
            //        );
            //        if (m.first == "NH_LORRI") {
            //            RenderFont(*_font,
            //                penPosition,
            //                firing,
            //                " + "
            //            );
            //        }
            //        RenderFont(*_font,
            //            penPosition,
            //            glm::vec4(0.3, 0.3, 0.3, 1),
            //            "  |"
            //        );
            //        RenderFontCr(*_font,
            //            penPosition,
            //            active,
            //            "    %5s",
            //            m.first.c_str()
            //        );
            //    }
            //}
        }
    }

    return size;
}

} // namespace openspace
