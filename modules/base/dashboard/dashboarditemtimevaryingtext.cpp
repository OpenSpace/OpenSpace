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

#include <modules/base/dashboard/dashboarditemtimevaryingtext.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/json.h>
#include <openspace/util/timemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <fstream>

namespace {
    constexpr openspace::properties::Property::PropertyInfo FormatStringInfo = {
        "FormatString",
        "Format String",
        "The format text describing how this dashboard item renders its text. This text "
        "must contain exactly one {} which is a placeholder that will be replaced "
        "with the values read from the file provided in `DataFile`",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DataFileInfo = {
        "DataFile",
        "Data File Path",
        "The file path to the JSON data.",
        openspace::properties::Property::Visibility::User
    };

    // This `DashboardItem` displays text based on the content of a provided data file.
    // The value that is displayed depends on the current in-game simulation time.
    //
    // The JSON must contain a 'data' array with timestamp-value pairs. Example format:
    // {\"data\": [[\"2024-05-10T00:00:00Z\", 2.33], [\"2024-05-10T03:00:00Z\", 3.0]]}
    struct [[codegen::Dictionary(DashboardItemTimeVaryingText)]] Parameters {
        // [[codegen::verbatim(FormatStringInfo.description)]]
        std::optional<std::string> formatString;

        // [[codegen::verbatim(DataFileInfo.description)]]
        std::filesystem::path dataFile;
    };
#include "dashboarditemtimevaryingtext_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemTimeVaryingText::Documentation() {
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

    _dataFile.onChange([this]() { loadDataFromJson(_dataFile); });
    _dataFile = p.dataFile.string();
    addProperty(_dataFile);
}

void DashboardItemTimeVaryingText::update() {
    if (_startTimes.empty()) {
        _buffer.clear();
        return;
    }

    double current = global::timeManager->time().j2000Seconds();
    double first = _startTimes.front();
    double last = _sequenceEndTime;

    if (current >= first && current < last) {
        int newIdx = updateActiveTriggerTimeIndex(current);
        if (newIdx != _activeTriggerTimeIndex) {
            _activeTriggerTimeIndex = newIdx;
            double timeKey = _startTimes[_activeTriggerTimeIndex];
            const nlohmann::json value = _data[timeKey];
            try {
                switch (value.type()) {
                    case nlohmann::json::value_t::null:
                    case nlohmann::json::value_t::discarded:
                        break;
                    case nlohmann::json::value_t::boolean: {
                        const bool v = value.get<bool>();
                        _buffer = std::vformat(
                            _formatString.value(),
                            std::make_format_args(v)
                        );
                        break;
                    }
                    case nlohmann::json::value_t::string: {
                        const std::string v = value.get<std::string>();
                        _buffer = std::vformat(
                            _formatString.value(),
                            std::make_format_args(v)
                        );
                        break;
                    }
                    case nlohmann::json::value_t::number_integer: {
                        const int v = value.get<int>();
                        _buffer = std::vformat(
                            _formatString.value(),
                            std::make_format_args(v)
                        );
                        break;
                    }
                    case nlohmann::json::value_t::number_unsigned: {
                        const unsigned v = value.get<unsigned>();
                        _buffer = std::vformat(
                            _formatString.value(),
                            std::make_format_args(v)
                        );
                        break;
                    }
                    case nlohmann::json::value_t::number_float: {
                        const double v = value.get<double>();
                        _buffer = std::vformat(
                            _formatString.value(),
                            std::make_format_args(v)
                        );
                        break;
                    }
                    case nlohmann::json::value_t::object:
                    case nlohmann::json::value_t::array: {
                        const std::string v = nlohmann::to_string(value);
                        _buffer = std::vformat(
                            _formatString.value(),
                            std::make_format_args(v)
                        );
                        break;
                    }
                    case nlohmann::json::value_t::binary: {
                        LWARNINGC(
                            "DashboardItemTimeVaryingText",
                            "Binary data is not supported"
                        );
                        break;
                    }
                }

            }
            catch (const std::format_error&) {
                LERRORC("DashboardItemTimeVaryingText", "Illegal format string");
            }
        }
    }
    else {
        _activeTriggerTimeIndex = -1;
        _buffer.clear();
    }
}

void DashboardItemTimeVaryingText::loadDataFromJson(const std::string& filePath) {
    std::ifstream file = std::ifstream(filePath);
    if (!file.is_open()) {
        throw ghoul::RuntimeError(std::format(
            "Time varying text, '{}' is not a valid JSON file",
            filePath
        ));
        return;
    }

    nlohmann::json jsonData;
    file >> jsonData;

    if (jsonData.find("data") == jsonData.end()) {
        throw ghoul::RuntimeError(std::format(
            "Error loading JSON file. No 'data' was found in '{}'", filePath
        ));
    }

    _data.clear();
    _startTimes.clear();

    for (const nlohmann::json& item : jsonData["data"]) {
        const std::string& timeString = item[0].get<std::string>();
        double j2000Time = Time::convertTime(timeString);
        _data[j2000Time] = item[1];
        _startTimes.push_back(j2000Time);
    }

    std::sort(_startTimes.begin(), _startTimes.end());
    computeSequenceEndTime();
}

void DashboardItemTimeVaryingText::computeSequenceEndTime() {
    if (_startTimes.size() > 1) {
        double first = _startTimes.front();
        double last = _startTimes.back();
        double avgDuration = (last - first) / static_cast<double>(_startTimes.size() - 1);
        // Extend end time so the last value remains visible for one more interval
        _sequenceEndTime = last + avgDuration;
    }
}

int DashboardItemTimeVaryingText::updateActiveTriggerTimeIndex(double currentTime) const {
    auto it = std::upper_bound(_startTimes.begin(), _startTimes.end(), currentTime);
    if (it != _startTimes.end()) {
        if (it != _startTimes.begin()) {
            return static_cast<int>(std::distance(_startTimes.begin(), it)) - 1;
        }
        return 0;
    }
    return static_cast<int>(_startTimes.size()) - 1;
}
}// namespace openspace
