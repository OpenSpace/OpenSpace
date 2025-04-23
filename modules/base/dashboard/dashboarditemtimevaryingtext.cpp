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
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/timemanager.h>
#include <openspace/json.h>
#include <fstream>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextInfo = {
        "Text",
        "Text",
        "The text to be displayed.",
        openspace::properties::Property::Visibility::User
    };
    constexpr openspace::properties::Property::PropertyInfo DataFileInfo = {
        "DataFile",
        "Data File Path",
        "The file path to the JSON data.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(DashboardItemTimeVaryingText)]] Parameters {
        // [[codegen::verbatim(TextInfo.description)]]
        std::optional<std::string> text;
        std::string dataFile;
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

DashboardItemTimeVaryingText::DashboardItemTimeVaryingText(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _text(TextInfo, "")
    , _dataFile(DataFileInfo, "")
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _text = p.text.value_or(_text);
    addProperty(_text);
    _dataFile = absPath(p.dataFile).string();
    addProperty(_dataFile);
    loadDataFromJson(_dataFile);
}

void DashboardItemTimeVaryingText::update() {
    if (_startTimes.empty()) {
        _buffer = "";
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
            double value = _data[timeKey];

            std::ostringstream oss;
            oss << value;
            _text = oss.str();
            _buffer = _text.value();
        }
    }
    else {
        _activeTriggerTimeIndex = -1;
        _buffer = "";
    }
}

void DashboardItemTimeVaryingText::loadDataFromJson(const std::string& filePath) {
    std::ifstream file(filePath);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to open JSON file: " + filePath);
    }

    nlohmann::json jsonData;
    file >> jsonData;

    _data.clear();
    _startTimes.clear();

    for (const auto& item : jsonData["data"]) {
        const std::string& timeString = item[0].get<std::string>();
        double j2000Time = Time::convertTime(timeString);
        double value = item[1].get<double>();
        _data[j2000Time] = value;
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
