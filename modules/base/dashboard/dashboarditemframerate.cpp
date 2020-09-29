/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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
#include <ghoul/misc/profiling.h>

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

    constexpr openspace::properties::Property::PropertyInfo ClearCacheInfo = {
        "ClearCache",
        "Clear Cache",
        "Clears the cache of this DashboardItemFramerate item. If the selected option "
        "does not use any caching, this trigger does not do anything."
    };

    constexpr const char* ValueDtAvg = "Average Deltatime";
    constexpr const char* ValueDtExtremes = "Deltatime extremes";
    constexpr const char* ValueDtStandardDeviation = "Deltatime standard deviation";
    constexpr const char* ValueDtCov = "Deltatime coefficient of variation";
    constexpr const char* ValueFps = "Frames per second";
    constexpr const char* ValueFpsAvg = "Average frames per second";
    constexpr const char* ValueNone = "None";

    [[ nodiscard ]] char* formatDt(std::vector<char>& buffer) {
        return fmt::format_to(
            buffer.data(),
            "Avg. Frametime: {:.2f} ms\0",
            openspace::global::windowDelegate.averageDeltaTime() * 1000.0
        );
    }

    [[ nodiscard ]] char* formatDtExtremes(std::vector<char>& buffer,
                                           double minFrametimeCache,
                                           double maxFrametimeCache)
    {
        return fmt::format_to(
            buffer.data(),
            "Last frametimes between: {:.2f} and {:.2f} ms\n"
            "Overall between: {:.2f} and {:.2f} ms\0",
            openspace::global::windowDelegate.minDeltaTime() * 1000.0,
            openspace::global::windowDelegate.maxDeltaTime() * 1000.0,
            minFrametimeCache,
            maxFrametimeCache
        );
    }

    [[ nodiscard ]] char* formatDtStandardDeviation(std::vector<char>& buffer) {
        return fmt::format_to(
            buffer.data(),
            "Frametime standard deviation : {:.2f} ms\0",
            openspace::global::windowDelegate.deltaTimeStandardDeviation() * 1000.0
        );
    }

    [[ nodiscard ]] char* formatDtCoefficientOfVariation(std::vector<char>& buffer) {
        return fmt::format_to(
            buffer.data(),
            "Frametime coefficient of variation : {:.2f} %\0",
            openspace::global::windowDelegate.deltaTimeStandardDeviation() /
            openspace::global::windowDelegate.averageDeltaTime() * 100.0
        );
    }

    [[ nodiscard ]] char* formatFps(std::vector<char>& buffer) {
        return fmt::format_to(
            buffer.data(),
            "FPS: {:3.2f}\0",
            1.0 / openspace::global::windowDelegate.deltaTime()
        );
    }

    [[ nodiscard ]] char* formatAverageFps(std::vector<char>& buffer) {
        return fmt::format_to(
            buffer.data(),
            "Avg. FPS: {:3.2f}\0",
            1.0 / openspace::global::windowDelegate.averageDeltaTime()
        );
    }

    [[ nodiscard ]] char* format(std::vector<char>& buffer,
                           openspace::DashboardItemFramerate::FrametimeType frametimeType,
                                       double minFrametimeCache, double maxFrametimeCache)
    {
        using namespace openspace;
        switch (frametimeType) {
            case DashboardItemFramerate::FrametimeType::DtTimeAvg:
                return formatDt(buffer);
            case DashboardItemFramerate::FrametimeType::DtTimeExtremes:
                return formatDtExtremes(buffer, minFrametimeCache, maxFrametimeCache);
            case DashboardItemFramerate::FrametimeType::DtStandardDeviation:
                return formatDtStandardDeviation(buffer);
            case DashboardItemFramerate::FrametimeType::DtCoefficientOfVariation:
                return formatDtCoefficientOfVariation(buffer);
            case DashboardItemFramerate::FrametimeType::FPS:
                return formatFps(buffer);
            case DashboardItemFramerate::FrametimeType::FPSAvg:
                return formatAverageFps(buffer);
            default:
                throw ghoul::MissingCaseException();
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
                new StringInListVerifier({ ValueDtAvg, ValueDtExtremes,
                    ValueDtStandardDeviation, ValueDtCov, ValueFps, ValueFpsAvg, ValueNone
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
    , _clearCache(ClearCacheInfo)
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
        { static_cast<int>(FrametimeType::DtTimeAvg), ValueDtAvg },
        { static_cast<int>(FrametimeType::DtTimeExtremes), ValueDtExtremes },
        {
            static_cast<int>(FrametimeType::DtStandardDeviation),
            ValueDtStandardDeviation
        },
        {
            static_cast<int>(FrametimeType::DtCoefficientOfVariation),
            ValueDtCov
        },
        { static_cast<int>(FrametimeType::FPS), ValueFps },
        { static_cast<int>(FrametimeType::FPSAvg), ValueFpsAvg },
        { static_cast<int>(FrametimeType::None), ValueNone }
    });

    if (dictionary.hasKey(FrametimeInfo.identifier)) {
        const std::string& v = dictionary.value<std::string>(FrametimeInfo.identifier);
        if (v == ValueDtAvg) {
            _frametimeType = static_cast<int>(FrametimeType::DtTimeAvg);
        }
        else if (v == ValueDtExtremes) {
            _frametimeType = static_cast<int>(FrametimeType::DtTimeExtremes);
        }
        else if (v == ValueDtStandardDeviation) {
            _frametimeType =
                static_cast<int>(FrametimeType::DtStandardDeviation);
        }
        else if (v == ValueDtCov) {
            _frametimeType =
                static_cast<int>(FrametimeType::DtCoefficientOfVariation);
        }
        else if (v == ValueFps) {
            _frametimeType = static_cast<int>(FrametimeType::FPS);
        }
        else if (v == ValueFpsAvg) {
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

    _clearCache.onChange([this]() {
        _shouldClearCache = true;
    });
    addProperty(_clearCache);

    _font = global::fontManager.font(_fontName, _fontSize);

    _buffer.resize(128);
}

void DashboardItemFramerate::render(glm::vec2& penPosition) {
    ZoneScoped

    if (_shouldClearCache) {
        _minDeltaTimeCache = 1.0;
        _maxDeltaTimeCache = -1.0;
        _shouldClearCache = false;
    }

    _minDeltaTimeCache = std::min(
        _minDeltaTimeCache,
        global::windowDelegate.minDeltaTime() * 1000.0
    );
    _maxDeltaTimeCache = std::max(
        _maxDeltaTimeCache,
        global::windowDelegate.maxDeltaTime() * 1000.0
    );

    FrametimeType frametimeType = FrametimeType(_frametimeType.value());

    std::fill(_buffer.begin(), _buffer.end(), 0);
    char* end = format(
        _buffer,
        frametimeType,
        _minDeltaTimeCache,
        _maxDeltaTimeCache
    );
    std::string_view output = std::string_view(_buffer.data(), end - _buffer.data());

    int nLines = output.empty() ? 0 :
        static_cast<int>((std::count(output.begin(), output.end(), '\n') + 1));

    penPosition.y -= _font->height() * static_cast<float>(nLines);

    ghoul::fontrendering::FontRenderer::defaultRenderer().render(
        *_font,
        penPosition,
        output
    );
}

glm::vec2 DashboardItemFramerate::size() const {
    ZoneScoped

    const FrametimeType t = FrametimeType(_frametimeType.value());
    format(_buffer, t, _minDeltaTimeCache, _maxDeltaTimeCache);
    std::string_view output = _buffer.data();

    if (output.empty()) {
        return { 0.f, 0.f };
    }

    return _font->boundingBox(output);
}

} // namespace openspace
