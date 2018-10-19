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

#include <modules/base/dashboard/dashboarditemframerate.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

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

    constexpr openspace::properties::Property::PropertyInfo FrametimeInfo = {
        "FrametimeType",
        "Type of the frame time display",
        "This value determines the units in which the frame time is displayed."
    };
} // namespace

namespace openspace {

documentation::Documentation DashboardItemFramerate::Documentation() {
    using namespace documentation;

    return {
        "DashboardItem Framerate",
        "base_dashboarditem_framerate",
        {
            {
                "Type",
                new StringEqualVerifier("DashboardItemFramerate"),
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
                FrametimeInfo.identifier,
                new StringInListVerifier({
                    "Average Deltatime", "Frames per second", "Average frames per second",
                    "None"
                }),
                Optional::Yes,
                FrametimeInfo.description
            }
        }
    };
}

DashboardItemFramerate::DashboardItemFramerate(const ghoul::Dictionary& dictionary)
    : DashboardItem(dictionary)
    , _fontName(FontNameInfo, KeyFontMono)
    , _fontSize(FontSizeInfo, DefaultFontSize, 6.f, 144.f, 1.f)
    , _frametimeType(FrametimeInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "DashboardItemFramerate"
    );

    if (dictionary.hasKey(FontNameInfo.identifier)) {
        _fontName = dictionary.value<std::string>(FontNameInfo.identifier);
    }
    _fontName.onChange([this]() {
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

    _frametimeType.addOptions({
        { static_cast<int>(FrametimeType::DtTimeAvg), "Average Deltatime" },
        { static_cast<int>(FrametimeType::FPS), "Frames per second" },
        { static_cast<int>(FrametimeType::FPSAvg), "Average frames per second" },
        { static_cast<int>(FrametimeType::None), "None" }
    });

    if (dictionary.hasKey(FrametimeInfo.identifier)) {
        const std::string& v = dictionary.value<std::string>(FrametimeInfo.identifier);
        if (v == "Average Deltatime") {
            _frametimeType = static_cast<int>(FrametimeType::DtTimeAvg);
        }
        else if (v == "Frames per second") {
            _frametimeType = static_cast<int>(FrametimeType::FPS);
        }
        else if (v == "Average frames per second") {
            _frametimeType = static_cast<int>(FrametimeType::FPSAvg);
        }
        else {
            _frametimeType = static_cast<int>(FrametimeType::None);
        }
    }
    else {
        _frametimeType = static_cast<int>(FrametimeType::FPSAvg);
    }
    addProperty(_frametimeType);

    _font = global::fontManager.font(_fontName, _fontSize);
}

void DashboardItemFramerate::render(glm::vec2& penPosition) {
    FrametimeType frametimeType = FrametimeType(_frametimeType.value());
    switch (frametimeType) {
        case FrametimeType::DtTimeAvg:
            penPosition.y -= _font->height();
            RenderFont(
                *_font,
                penPosition,
                fmt::format(
                    "Avg. Frametime: {:.5f}", global::windowDelegate.averageDeltaTime()
                )
            );
            break;
        case FrametimeType::FPS:
            penPosition.y -= _font->height();
            RenderFont(
                *_font,
                penPosition,
                fmt::format("FPS: {:3.2f}", 1.0 / global::windowDelegate.deltaTime())
            );
            break;
        case FrametimeType::FPSAvg:
            penPosition.y -= _font->height();
            RenderFont(
                *_font,
                penPosition,
                fmt::format(
                    "Avg. FPS: {:3.2f}", 1.0 / global::windowDelegate.averageDeltaTime()
                )
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
                fmt::format(
                    "Avg. Frametime: {:.5f}", global::windowDelegate.averageDeltaTime()
                )
            ).boundingBox;
        case FrametimeType::FPS:
            return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                *_font,
                fmt::format(
                    "FPS: {:3.2f}", 1.0 / global::windowDelegate.deltaTime()
                )
            ).boundingBox;
        case FrametimeType::FPSAvg:
            return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
                *_font,
                fmt::format(
                    "Avg. FPS: %3.2f", 1.0 / global::windowDelegate.averageDeltaTime()
                )
            ).boundingBox;
        case FrametimeType::None:
            return { 0.f, 0.f };
        default:
            return { 0.f, 0.f };
    }
}

} // namespace openspace
