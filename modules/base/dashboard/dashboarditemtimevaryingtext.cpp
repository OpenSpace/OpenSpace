/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/base/dashboard/dashboarditemtimevaryingtext.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/misc/exception.h>
#include <algorithm>
#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <utility>

namespace {
    using namespace openspace;

    constexpr Property::PropertyInfo FormatStringInfo = {
        "FormatString",
        "Format string",
        "The format text describing how this dashboard item renders its text. This text "
        "must contain exactly one {} which is a placeholder that will be replaced "
        "with the values read from the file provided in `DataFile`.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo DataFileInfo = {
        "DataFile",
        "Data file path",
        "The file path to the JSON data.",
        Property::Visibility::User
    };

    constexpr std::string_view DefaultFallbackMode = "Clear";

    DashboardItemTimeVaryingText::FallbackMode parseFallbackMode(std::string_view mode) {
        if (mode == "HoldLastValue") {
            return DashboardItemTimeVaryingText::FallbackMode::HoldLastValue;
        }
        if (mode == "MissingText") {
            return DashboardItemTimeVaryingText::FallbackMode::MissingText;
        }
        if (mode == "Clear") {
            return DashboardItemTimeVaryingText::FallbackMode::Clear;
        }

        throw ghoul::RuntimeError(std::format(
            "Unsupported DashboardItemTimeVaryingText fallback mode '{}'. Supported "
            "values are 'HoldLastValue', 'Clear', and 'MissingText'",
            mode
        ));
    }

    std::string formatValue(std::string_view formatString, const nlohmann::json& value) {
        switch (value.type()) {
            case nlohmann::json::value_t::boolean: {
                const bool v = value.get<bool>();
                return std::vformat(formatString, std::make_format_args(v));
            }
            case nlohmann::json::value_t::string: {
                const std::string v = value.get<std::string>();
                return std::vformat(formatString, std::make_format_args(v));
            }
            case nlohmann::json::value_t::number_integer: {
                const double v = value.get<double>();
                return std::vformat(formatString, std::make_format_args(v));
            }
            case nlohmann::json::value_t::number_unsigned: {
                const double v = value.get<double>();
                return std::vformat(formatString, std::make_format_args(v));
            }
            case nlohmann::json::value_t::number_float: {
                const double v = value.get<double>();
                return std::vformat(formatString, std::make_format_args(v));
            }
            case nlohmann::json::value_t::object:
            case nlohmann::json::value_t::array: {
                const std::string v = nlohmann::to_string(value);
                return std::vformat(formatString, std::make_format_args(v));
            }
            case nlohmann::json::value_t::null:
            case nlohmann::json::value_t::discarded:
            case nlohmann::json::value_t::binary:
                break;
        }

        return std::string();
    }

    struct [[codegen::Dictionary(DashboardItemTimeVaryingText)]] Parameters {
        // [[codegen::verbatim(FormatStringInfo.description)]]
        std::optional<std::string> formatString;

        // [[codegen::verbatim(DataFileInfo.description)]]
        std::filesystem::path dataFile;

        std::optional<bool> useKpColoring [[codegen::key("UseKpColoring")]];

        std::optional<bool> timestampsAreIntervalEnd
            [[codegen::key("TimestampsAreIntervalEnd")]];

        std::optional<std::string> fallbackMode [[codegen::key("FallbackMode")]];

        std::optional<std::string> missingText [[codegen::key("MissingText")]];
    };
} // namespace
#include "dashboarditemtimevaryingtext_codegen.cpp"

namespace openspace {

Documentation DashboardItemTimeVaryingText::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_timevaryingtext",
        DashboardTextItem::Documentation()
    );
}

DashboardItemTimeVaryingText::DashboardItemTimeVaryingText(
                                                    const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _formatString(FormatStringInfo, "{}")
    , _dataFile(DataFileInfo, "")
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _formatString = p.formatString.value_or(_formatString);
    addProperty(_formatString);

    _useKpColoring = p.useKpColoring.value_or(_useKpColoring);
    _timestampsAreIntervalEnd =
        p.timestampsAreIntervalEnd.value_or(_timestampsAreIntervalEnd);
    _fallbackMode = parseFallbackMode(
        p.fallbackMode.value_or(std::string(DefaultFallbackMode))
    );
    _missingText = p.missingText.value_or(_missingText);

    _dataFile.onChange([this]() { loadDataFromJson(_dataFile.value()); });
    _dataFile = p.dataFile.string();
    addProperty(_dataFile);

    loadDataFromJson(_dataFile.value());
}

void DashboardItemTimeVaryingText::update() {
    if (_timestamps.empty()) {
        applyFallback();
        return;
    }

    const double currentTime = global::timeManager->time().j2000Seconds();
    const int newIndex = valueIndex(currentTime);
    if (newIndex < 0) {
        _activeValueIndex = -1;
        applyFallback();
        return;
    }

    if (newIndex == _activeValueIndex && !_buffer.empty()) {
        return;
    }

    _activeValueIndex = newIndex;
    updateBufferForValue(_values[_activeValueIndex]);
}

void DashboardItemTimeVaryingText::render(glm::vec2& penPosition) {
    if (_buffer.empty()) {
        update();
    }

    if (_buffer.empty()) {
        return;
    }

    std::string_view text = _buffer;
    size_t end = text.find('\n');
    while (end != std::string_view::npos) {
        penPosition.y -= _font->height();
        RenderFont(*_font, penPosition, text.substr(0, end), _textColor);
        text.remove_prefix(end + 1);
        end = text.find('\n');
    }

    penPosition.y -= _font->height();
    RenderFont(*_font, penPosition, text, _textColor);
}

void DashboardItemTimeVaryingText::loadDataFromJson(const std::string& filePath) {
    std::ifstream file(filePath);
    if (!file.is_open()) {
        throw ghoul::RuntimeError(std::format(
            "Time varying text, '{}' is not a valid JSON file",
            filePath
        ));
    }

    nlohmann::json jsonData;
    file >> jsonData;

    auto it = jsonData.find("data");
    if (it == jsonData.end() || !it->is_array()) {
        throw ghoul::RuntimeError(std::format(
            "Error loading JSON file '{}'. Expected a 'data' array",
            filePath
        ));
    }

    std::vector<std::pair<double, nlohmann::json>> entries;
    entries.reserve(it->size());

    for (size_t i = 0; i < it->size(); ++i) {
        const nlohmann::json& item = (*it)[i];
        if (!item.is_array() || item.size() < 2) {
            throw ghoul::RuntimeError(std::format(
                "Error loading JSON file '{}'. Entry {} must contain at least [time, value]",
                filePath,
                i
            ));
        }

        if (!item[0].is_string()) {
            throw ghoul::RuntimeError(std::format(
                "Error loading JSON file '{}'. Entry {} time value must be a string",
                filePath,
                i
            ));
        }

        const std::string timeString = item[0].get<std::string>();
        entries.emplace_back(Time::convertTime(timeString), item[1]);
    }

    std::sort(
        entries.begin(),
        entries.end(),
        [](const auto& lhs, const auto& rhs) { return lhs.first < rhs.first; }
    );

    _timestamps.clear();
    _values.clear();
    _timestamps.reserve(entries.size());
    _values.reserve(entries.size());

    for (const auto& [timestamp, value] : entries) {
        _timestamps.push_back(timestamp);
        _values.push_back(value);
    }

    _activeValueIndex = -1;
    _hasLastResolvedValue = false;
    _buffer.clear();
    _textColor = glm::vec4(1.f);

    if (_timestamps.size() > 1) {
        _averageInterval =
            (_timestamps.back() - _timestamps.front()) /
            static_cast<double>(_timestamps.size() - 1);
    }
    else {
        _averageInterval = 0.0;
    }

    update();
}

void DashboardItemTimeVaryingText::updateBufferForValue(const nlohmann::json& value) {
    if (value.is_null() || value.is_discarded()) {
        applyFallback();
        return;
    }

    if (value.is_binary()) {
        LWARNINGC("DashboardItemTimeVaryingText", "Binary data is not supported");
        applyFallback();
        return;
    }

    try {
        _buffer = formatValue(_formatString.value(), value);
        _textColor = colorForValue(value);
        _lastResolvedColor = _textColor;
        _hasLastResolvedValue = true;
    }
    catch (const std::format_error&) {
        LERRORC("DashboardItemTimeVaryingText", "Illegal format string");
        applyFallback();
    }
}

void DashboardItemTimeVaryingText::applyFallback() {
    switch (_fallbackMode) {
        case FallbackMode::HoldLastValue:
            if (_hasLastResolvedValue) {
                _textColor = _lastResolvedColor;
                return;
            }
            if (!_missingText.empty()) {
                _buffer = _missingText;
                _textColor = glm::vec4(1.f);
                return;
            }
            _buffer.clear();
            _textColor = glm::vec4(1.f);
            return;

        case FallbackMode::MissingText:
            _buffer = _missingText;
            _textColor = glm::vec4(1.f);
            return;

        case FallbackMode::Clear:
            _buffer.clear();
            _textColor = glm::vec4(1.f);
            return;
    }
}

int DashboardItemTimeVaryingText::valueIndex(double currentTime) const {
    if (_timestamps.empty()) {
        return -1;
    }

    if (_timestampsAreIntervalEnd) {
        const double earliestTime = _timestamps.front() - _averageInterval;
        if (currentTime < earliestTime || currentTime > _timestamps.back()) {
            return -1;
        }

        auto it = std::lower_bound(_timestamps.begin(), _timestamps.end(), currentTime);
        if (it == _timestamps.end()) {
            return -1;
        }
        return static_cast<int>(std::distance(_timestamps.begin(), it));
    }

    const double latestTime = _timestamps.back() + _averageInterval;
    if (currentTime < _timestamps.front()) {
        return -1;
    }
    if (_timestamps.size() > 1 && currentTime >= latestTime) {
        return -1;
    }

    auto it = std::upper_bound(_timestamps.begin(), _timestamps.end(), currentTime);
    if (it == _timestamps.begin()) {
        return -1;
    }
    return static_cast<int>(std::distance(_timestamps.begin(), it) - 1);
}

glm::vec4 DashboardItemTimeVaryingText::colorForValue(const nlohmann::json& value) const {
    if (!_useKpColoring || !value.is_number()) {
        return glm::vec4(1.f);
    }

    const double kpValue = value.get<double>();
    if (kpValue > 8.0) {
        return glm::vec4(1.f, 0.f, 0.f, 1.f);
    }
    if (kpValue > 7.0) {
        return glm::vec4(1.f, 150.f / 255.f, 0.f, 1.f);
    }
    if (kpValue > 6.0) {
        return glm::vec4(1.f, 200.f / 255.f, 0.f, 1.f);
    }
    if (kpValue > 5.0) {
        return glm::vec4(246.f / 255.f, 235.f / 255.f, 20.f / 255.f, 1.f);
    }
    return glm::vec4(146.f / 255.f, 208.f / 255.f, 80.f / 255.f, 1.f);
}

} // namespace openspace
