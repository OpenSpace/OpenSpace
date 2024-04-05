/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <optional>

namespace {
    enum FrametimeType {
        DtTimeAvg = 0,
        DtTimeExtremes,
        DtStandardDeviation,
        DtCoefficientOfVariation,
        FPS,
        FPSAvg,
        None
    };

    constexpr openspace::properties::Property::PropertyInfo FrametimeInfo = {
        "FrametimeType",
        "Type of the frame time display",
        "This value determines the units in which the frame time is displayed",
        // @VISIBILITY(2.75)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ClearCacheInfo = {
        "ClearCache",
        "Clear Cache",
        "Clears the cache of this DashboardItemFramerate item. If the selected option "
        "does not use any caching, this trigger does not do anything",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    [[ nodiscard ]] char* formatDt(std::vector<char>& buffer) {
        return std::format_to(
            buffer.data(),
            "Avg. Frametime: {:.2f} ms\0",
            openspace::global::windowDelegate->averageDeltaTime() * 1000.0
        );
    }

    [[ nodiscard ]] char* formatDtExtremes(std::vector<char>& buffer,
                                           double minFrametimeCache,
                                           double maxFrametimeCache)
    {
        return std::format_to(
            buffer.data(),
            "Last frametimes between: {:.2f} and {:.2f} ms\n"
            "Overall between: {:.2f} and {:.2f} ms\0",
            openspace::global::windowDelegate->minDeltaTime() * 1000.0,
            openspace::global::windowDelegate->maxDeltaTime() * 1000.0,
            minFrametimeCache,
            maxFrametimeCache
        );
    }

    [[ nodiscard ]] char* formatDtStandardDeviation(std::vector<char>& buffer) {
        return std::format_to(
            buffer.data(),
            "Frametime standard deviation : {:.2f} ms\0",
            openspace::global::windowDelegate->deltaTimeStandardDeviation() * 1000.0
        );
    }

    [[ nodiscard ]] char* formatDtCoefficientOfVariation(std::vector<char>& buffer) {
        return std::format_to(
            buffer.data(),
            "Frametime coefficient of variation : {:.2f} %\0",
            openspace::global::windowDelegate->deltaTimeStandardDeviation() /
            openspace::global::windowDelegate->averageDeltaTime() * 100.0
        );
    }

    [[ nodiscard ]] char* formatFps(std::vector<char>& buffer) {
        return std::format_to(
            buffer.data(),
            "FPS: {:3.2f}\0",
            1.0 / openspace::global::windowDelegate->deltaTime()
        );
    }

    [[ nodiscard ]] char* formatAverageFps(std::vector<char>& buffer) {
        return std::format_to(
            buffer.data(),
            "Avg. FPS: {:3.2f}\0",
            1.0 / openspace::global::windowDelegate->averageDeltaTime()
        );
    }

    [[ nodiscard ]] char* format(std::vector<char>& buffer, FrametimeType frametimeType,
                                       double minFrametimeCache, double maxFrametimeCache)
    {
        using namespace openspace;
        switch (frametimeType) {
            case FrametimeType::DtTimeAvg:
                return formatDt(buffer);
            case FrametimeType::DtTimeExtremes:
                return formatDtExtremes(buffer, minFrametimeCache, maxFrametimeCache);
            case FrametimeType::DtStandardDeviation:
                return formatDtStandardDeviation(buffer);
            case FrametimeType::DtCoefficientOfVariation:
                return formatDtCoefficientOfVariation(buffer);
            case FrametimeType::FPS:
                return formatFps(buffer);
            case FrametimeType::FPSAvg:
                return formatAverageFps(buffer);
            default:
                throw ghoul::MissingCaseException();
        }
    }

    struct [[codegen::Dictionary(DashboardItemFramerate)]] Parameters {
        enum class [[codegen::map(FrametimeType)]] Type {
            DtTimeAvg [[codegen::key("Average Deltatime")]],
            DtTimeExtremes [[codegen::key("Deltatime extremes")]],
            DtStandardDeviation [[codegen::key("Deltatime standard deviation")]],
            DtCoefficientOfVariation
                [[codegen::key("Deltatime coefficient of variation")]],
            FPS [[codegen::key("Frames per second")]],
            FPSAvg [[codegen::key("Average frames per second")]]
        };

        // [[codegen::verbatim(FrametimeInfo.description)]]
        std::optional<Type> frametimeType;
    };
#include "dashboarditemframerate_codegen.cpp"

} // namespace

namespace openspace {

documentation::Documentation DashboardItemFramerate::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_framerate",
        DashboardTextItem::Documentation()
    );
}

DashboardItemFramerate::DashboardItemFramerate(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _frametimeType(FrametimeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _clearCache(ClearCacheInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _frametimeType.addOptions({
        { static_cast<int>(FrametimeType::DtTimeAvg), "Average Deltatime" },
        { static_cast<int>(FrametimeType::DtTimeExtremes), "Deltatime extremes" },
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

    if (p.frametimeType.has_value()) {
        _frametimeType = codegen::map<FrametimeType>(*p.frametimeType);
    }
    else {
        _frametimeType = static_cast<int>(FrametimeType::FPSAvg);
    }
    addProperty(_frametimeType);

    _clearCache.onChange([this]() {
        _shouldClearCache = true;
    });
    addProperty(_clearCache);

    _buffer.resize(128);
}

void DashboardItemFramerate::render(glm::vec2& penPosition) {
    ZoneScoped;

    if (_shouldClearCache) {
        _minDeltaTimeCache = 1.0;
        _maxDeltaTimeCache = -1.0;
        _shouldClearCache = false;
    }

    _minDeltaTimeCache = std::min(
        _minDeltaTimeCache,
        global::windowDelegate->minDeltaTime() * 1000.0
    );
    _maxDeltaTimeCache = std::max(
        _maxDeltaTimeCache,
        global::windowDelegate->maxDeltaTime() * 1000.0
    );

    const FrametimeType frametimeType = FrametimeType(_frametimeType.value());

    std::fill(_buffer.begin(), _buffer.end(), char(0));
    char* end = format(
        _buffer,
        frametimeType,
        _minDeltaTimeCache,
        _maxDeltaTimeCache
    );
    const std::string_view text = std::string_view(_buffer.data(), end - _buffer.data());

    const int nLines = text.empty() ?
        0 :
        static_cast<int>((std::count(text.begin(), text.end(), '\n') + 1));

    ghoul::fontrendering::FontRenderer::defaultRenderer().render(
        *_font,
        penPosition,
        text
    );
    penPosition.y -= _font->height() * static_cast<float>(nLines);
}

glm::vec2 DashboardItemFramerate::size() const {
    ZoneScoped;

    const FrametimeType t = FrametimeType(_frametimeType.value());
    char* end = format(_buffer, t, _minDeltaTimeCache, _maxDeltaTimeCache);
    const std::string_view res = std::string_view(_buffer.data(), end - _buffer.data());

    if (res.empty()) {
        return { 0.f, 0.f };
    }

    return _font->boundingBox(res);
}

} // namespace openspace
