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

    std::string formatDt() {
        return fmt::format(
            "Avg. Frametime: {:.2f} ms",
            openspace::global::windowDelegate.averageDeltaTime() * 1000.0
        );
    }

    std::string formatDtExtremes() {
        return fmt::format(
            "Frametimes between: {:.2f} and {:.2f} ms",
            openspace::global::windowDelegate.minDeltaTime() * 1000.0,
            openspace::global::windowDelegate.maxDeltaTime() * 1000.0
        );
    }

    std::string formatDtStandardDeviation() {
        return fmt::format(
            "Frametime standard deviation : {:.2f} ms",
            openspace::global::windowDelegate.deltaTimeStandardDeviation() * 1000.0
        );
    }

    std::string formatDtCoefficientOfVariation() {
        return fmt::format(
            "Frametime coefficient of variation : {:.2f} %",
            openspace::global::windowDelegate.deltaTimeStandardDeviation() /
            openspace::global::windowDelegate.averageDeltaTime() * 100.0
        );
    }

    std::string formatFps() {
        return fmt::format(
            "FPS: {:3.2f}",
            1.0 / openspace::global::windowDelegate.deltaTime()
        );
    }

    std::string formatAverageFps() {
        return fmt::format(
            "Avg. FPS: {:3.2f}",
            1.0 / openspace::global::windowDelegate.averageDeltaTime()
        );
    }

    std::string format(openspace::DashboardItemFramerate::FrametimeType frametimeType) {
        using namespace openspace;
        switch (frametimeType) {
        case DashboardItemFramerate::FrametimeType::DtTimeAvg:
            return formatDt();
        case DashboardItemFramerate::FrametimeType::DtTimeExtremes:
            return formatDtExtremes();
        case DashboardItemFramerate::FrametimeType::DtStandardDeviation:
            return formatDtStandardDeviation();
        case DashboardItemFramerate::FrametimeType::DtCoefficientOfVariation:
            return formatDtCoefficientOfVariation();
        case DashboardItemFramerate::FrametimeType::FPS:
            return formatFps();
        case DashboardItemFramerate::FrametimeType::FPSAvg:
            return formatAverageFps();
        default:
            return "";
        }
    }

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
                    "Average Deltatime", "Deltatime Extremes", "Deltatime standard deviation",
                    "Deltatime coefficient of variation", "Frames per second",
                    "Average frames per second", "None"
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
        { static_cast<int>(FrametimeType::DtTimeExtremes), "Deltatime Extremes" },
        {
            static_cast<int>(FrametimeType::DtStandardDeviation),
            "Deltatime standard deviation"
        },
        {
            static_cast<int>(FrametimeType::DtCoefficientOfVariation),
            "Deltatime coefficient of variation"
        },
        { static_cast<int>(FrametimeType::FPS), "Frames per second" },
        { static_cast<int>(FrametimeType::FPSAvg), "Average frames per second" },
        { static_cast<int>(FrametimeType::None), "None" }
    });

    if (dictionary.hasKey(FrametimeInfo.identifier)) {
        const std::string& v = dictionary.value<std::string>(FrametimeInfo.identifier);
        if (v == "Average Deltatime") {
            _frametimeType = static_cast<int>(FrametimeType::DtTimeAvg);
        }
        else if (v == "Deltatime Extremes") {
            _frametimeType = static_cast<int>(FrametimeType::DtTimeExtremes);
        }
        else if (v == "Deltatime standard deviation") {
            _frametimeType =
            static_cast<int>(FrametimeType::DtStandardDeviation);
        }
        else if (v == "Deltatime coefficient of variation") {
            _frametimeType =
            static_cast<int>(FrametimeType::DtCoefficientOfVariation);
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

    const std::string output = format(frametimeType);

    int nLines = output.empty() ? 0 :
        (std::count(output.begin(), output.end(), '\n') + 1);

    penPosition.y -= _font->height() * static_cast<float>(nLines);

    ghoul::fontrendering::FontRenderer::defaultRenderer().render(
        *_font,
        penPosition,
        output
    );
}

glm::vec2 DashboardItemFramerate::size() const {
    FrametimeType frametimeType = FrametimeType(_frametimeType.value());
    const std::string output = format(frametimeType);

    if (output.empty()) {
        return { 0.f, 0.f };
    }

    return ghoul::fontrendering::FontRenderer::defaultRenderer().boundingBox(
        *_font,
        output
    ).boundingBox;
}

} // namespace openspace
