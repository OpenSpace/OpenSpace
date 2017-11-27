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

#include <modules/spacecraftinstruments/dashboard/dashboarditeminstruments.h>

#include <modules/spacecraftinstruments/util/imagesequencer.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/timemanager.h>

#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

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

    glm::vec2 addToBoundingbox(glm::vec2 lhs, glm::vec2 rhs) {
        return {
            std::max(lhs.x, rhs.x),
            lhs.y + rhs.y
        };
    }
} // namespace

namespace openspace {

DashboardItemInstruments::DashboardItemInstruments(ghoul::Dictionary dictionary)
    : DashboardItem("Instruments")
    , _font(OsEng.fontManager().font(KeyFontMono, 10))
{}

void DashboardItemInstruments::render(glm::vec2& penPosition) {
    bool hasNewHorizons = OsEng.renderEngine().scene()->sceneGraphNode("NewHorizons");
    double currentTime = OsEng.timeManager().time().j2000Seconds();

    if (ImageSequencer::ref().isReady()) {
        penPosition.y -= 25.f;

        glm::vec4 targetColor(0.00, 0.75, 1.00, 1);

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
                psc nhPos = PowerScaledCoordinate::CreatePowerScaledCoordinate(
                    p.x,
                    p.y,
                    p.z
                );
                float a, b;
                glm::dvec3 radii;
                SpiceManager::ref().getValue("PLUTO", "RADII", radii);
                a = static_cast<float>(radii.x);
                b = static_cast<float>(radii.y);
                float radius = (a + b) / 2.f;
                float distToSurf = glm::length(nhPos.vec3()) - radius;

                RenderFont(*_font,
                    penPosition,
                    "Distance to Pluto: % .1f (KM)",
                    distToSurf
                );
                penPosition.y -= _font->height();
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

        glm::vec4 active(0.6, 1, 0.00, 1);
        glm::vec4 brigther_active(0.9, 1, 0.75, 1);

        if (remaining > 0) {
            std::string progress = progressToStr(25, t);
            brigther_active *= (1 - t);

            RenderFontCr(*_font,
                penPosition,
                active * t + brigther_active,
                "Next instrument activity:"
            );

            RenderFontCr(*_font,
                penPosition,
                active * t + brigther_active,
                "%.0f s %s %.1f %%",
                remaining, progress.c_str(), t * 100
            );

            RenderFontCr(*_font,
                penPosition,
                active,
                "Data acquisition time: %s",
                str.c_str()
            );
        }
        std::pair<double, std::string> nextTarget =
            ImageSequencer::ref().getNextTarget();
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

            RenderFontCr(*_font,
                penPosition,
                targetColor,
                "Data acquisition adjacency: [%s:%s:%s]",
                hh.c_str(), mm.c_str(), ss.c_str()
            );

            penPosition.y -= _font->height();

            std::map<std::string, bool> activeMap =
                ImageSequencer::ref().getActiveInstruments();
            glm::vec4 firing(0.58 - t, 1 - t, 1 - t, 1);
            glm::vec4 notFiring(0.5, 0.5, 0.5, 1);

            RenderFontCr(*_font,
                penPosition,
                active,
                "Active Instruments:"
            );

            for (auto m : activeMap) {
                if (m.second == false) {
                    RenderFont(*_font,
                        penPosition,
                        glm::vec4(0.3, 0.3, 0.3, 1),
                        "| |"
                    );
                    RenderFontCr(*_font,
                        penPosition,
                        glm::vec4(0.3, 0.3, 0.3, 1),
                        "    %5s",
                        m.first.c_str()
                    );

                }
                else {
                    RenderFont(*_font,
                        penPosition,
                        glm::vec4(0.3, 0.3, 0.3, 1),
                        "|"
                    );
                    if (m.first == "NH_LORRI") {
                        RenderFont(*_font,
                            penPosition,
                            firing,
                            " + "
                        );
                    }
                    RenderFont(*_font,
                        penPosition,
                        glm::vec4(0.3, 0.3, 0.3, 1),
                        "  |"
                    );
                    RenderFontCr(*_font,
                        penPosition,
                        active,
                        "    %5s",
                        m.first.c_str()
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
                psc nhPos = PowerScaledCoordinate::CreatePowerScaledCoordinate(
                    p.x,
                    p.y,
                    p.z
                );
                float a, b;
                glm::dvec3 radii;
                SpiceManager::ref().getValue("PLUTO", "RADII", radii);
                a = static_cast<float>(radii.x);
                b = static_cast<float>(radii.y);
                float radius = (a + b) / 2.f;
                float distToSurf = glm::length(nhPos.vec3()) - radius;

                size = addToBoundingbox(
                    size,
                    ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                        *_font,
                        "Distance to Pluto: % .1f (KM)",
                        distToSurf
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
                    "%.0f s %s %.1f %%",
                    remaining, progress.c_str(), t * 100
                ).boundingBox
            );

            size = addToBoundingbox(
                size,
                ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                    *_font,
                    "Data acquisition time: %s",
                    str.c_str()
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
                    "Data acquisition adjacency: [%s:%s:%s]",
                    hh.c_str(), mm.c_str(), ss.c_str()
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
