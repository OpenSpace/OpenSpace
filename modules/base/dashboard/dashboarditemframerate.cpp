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

#include <modules/base/dashboard/dashboarditemframerate.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/wrapper/windowwrapper.h>

#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

namespace {
    const char* KeyFontMono = "Mono";

    static const openspace::properties::Property::PropertyInfo FrametimeInfo = {
        "FrametimeType",
        "Type of the frame time display",
        "This value determines the units in which the frame time is displayed."
    };
} // namespace

namespace openspace {

DashboardItemFramerate::DashboardItemFramerate(ghoul::Dictionary dictionary) 
    : DashboardItem("Framerate")
    , _frametimeType(FrametimeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _font(OsEng.fontManager().font(KeyFontMono, 10))
{
    _frametimeType.addOptions({
        { static_cast<int>(FrametimeType::DtTimeAvg), "Average Deltatime" },
        { static_cast<int>(FrametimeType::FPS), "Frames per second" },
        { static_cast<int>(FrametimeType::FPSAvg), "Average frames per second" },
        { static_cast<int>(FrametimeType::None), "None" }
    });
    _frametimeType = static_cast<int>(FrametimeType::FPS);
    addProperty(_frametimeType);
}

void DashboardItemFramerate::render(glm::vec2& penPosition) {
    FrametimeType frametimeType = FrametimeType(_frametimeType.value());
    switch (frametimeType) {
        case FrametimeType::DtTimeAvg:
            RenderFontCr(
                *_font,
                penPosition,
                "Avg. Frametime: %.5f",
                OsEng.windowWrapper().averageDeltaTime()
            );
            break;
        case FrametimeType::FPS:
            RenderFontCr(
                *_font,
                penPosition,
                "FPS: %3.2f",
                1.0 / OsEng.windowWrapper().deltaTime()
            );
            break;
        case FrametimeType::FPSAvg:
            RenderFontCr(
                *_font,
                penPosition,
                "Avg. FPS: %3.2f",
                1.0 / OsEng.windowWrapper().averageDeltaTime()
            );
            break;
        case FrametimeType::None:
            break;
    }
}

glm::vec2 DashboardItemFramerate::size() const {
    FrametimeType frametimeType = FrametimeType(_frametimeType.value());
    switch (frametimeType) {
        case FrametimeType::DtTimeAvg:
            return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                *_font,
                "Avg. Frametime: %.5f",
                OsEng.windowWrapper().averageDeltaTime()
            ).boundingBox;
        case FrametimeType::FPS:
            return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                *_font,
                "FPS: %3.2f",
                1.0 / OsEng.windowWrapper().deltaTime()
            ).boundingBox;
        case FrametimeType::FPSAvg:
            return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                *_font,
                "Avg. FPS: %3.2f",
                1.0 / OsEng.windowWrapper().averageDeltaTime()
            ).boundingBox;
        case FrametimeType::None:
            return { 0.f, 0.f };
            break;
    }
}

} // namespace openspace
