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

#include <modules/base/dashboard/dashboarditemtext.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/util/timemanager.h>
#include <ghoul/font/font.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/profiling.h>
#include <optional>

#include <json/json.hpp>
#include <map>
#include <iostream>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TextInfo = {
        "Text",
        "Text",
        "The text to be displayed.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(DashboardItemText)]] Parameters {
        // [[codegen::verbatim(TextInfo.description)]]
        std::optional<std::string> text;
    };
#include "dashboarditemtext_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation DashboardItemText::Documentation() {
    return codegen::doc<Parameters>(
        "base_dashboarditem_text",
        DashboardTextItem::Documentation()
    );
}

void DashboardItemText::loadDataFromJson(const std::string& filePath) {
    std::ifstream file(filePath);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to open JSON file: " + filePath);
    }

    nlohmann::json jsonData;
    file >> jsonData;

    _data.clear();

    for (const auto& item : jsonData["data"]) {
        std::string time = item[0].get<std::string>();
        double value = item[1].get<double>();
        _data[time] = value;
    }
}

DashboardItemText::DashboardItemText(const ghoul::Dictionary& dictionary)
    : DashboardTextItem(dictionary)
    , _text(TextInfo, "")
{
    const Parameters p = codegen::bake<Parameters>(dictionary);
    _text = p.text.value_or(_text);
    addProperty(_text);

    loadDataFromJson("C:/Users/alundkvi/Documents/work/OpenSpace/user/data/assets/aurorasaurus/KPjson/observed_data.json");
}

std::string formatTimeForData(std::string_view timeStr) {
    std::string formattedTime(timeStr);

    // Convert to the format YYYY-MM-DDTHH:MM:00Z
    std::replace(formattedTime.begin(), formattedTime.end(), 'T', ' ');
    std::replace(formattedTime.begin(), formattedTime.end(), '.', ' '); // Remove milliseconds

    std::tm tm = {};
    std::istringstream ss(formattedTime);
    ss >> std::get_time(&tm, "%Y %b %d %H:%M:%S");

    std::ostringstream oss;
    oss << std::put_time(&tm, "%Y-%m-%dT%H:%M:00Z");
    return oss.str();
}

double DashboardItemText::getValueForCurrentTime() const {
    std::string_view currentTimeStr = global::timeManager->time().UTC();
    std::string formattedTime = formatTimeForData(currentTimeStr);

    // Check if the formatted time exists in the data
    auto it = _data.find(formattedTime);
    if (it != _data.end()) {
        // Exact match found, update last value
        _lastValue = it->second;
        return _lastValue;
    }

    // If no exact match is found, return the last value
    return _lastValue;
}


void DashboardItemText::render(glm::vec2& penPosition) {
    ZoneScoped;

    double value = getValueForCurrentTime();

    _text = "KP Index: " + std::to_string(value);

    glm::vec4 color = { 1.f, 1.f, 1.f, 1.f };

    if (value > 6)
    {
        color = { 1.f, 0.f, 0.f, 1.f };
    }
    else if (value > 4)
    {
        color = { 1.f, 1.f, 0.f, 1.f };
    }
    else
    {
        color = { 0.f, 1.f, 0.f, 1.f };
    }

    penPosition.y -= _font->height();
    RenderFont(*_font, penPosition, _text.value(), color);
}

glm::vec2 DashboardItemText::size() const {
    ZoneScoped;

    return _font->boundingBox(_text.value());
}

} // namespace openspace
