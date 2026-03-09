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

#include <modules/iswa/util/dataprocessorjson.h>

#include <openspace/json.h>
#include <openspace/properties/misc/selectionproperty.h>
#include <ghoul/misc/assert.h>
#include <algorithm>
#include <iterator>
#include <utility>

namespace openspace {

std::vector<std::string> DataProcessorJson::readMetadata(const std::string& data,
                                                         glm::size3_t& dimensions)
{
    if (data.empty()) {
        return std::vector<std::string>();
    }

    std::vector<std::string> options = std::vector<std::string>();
    const nlohmann::json& j = nlohmann::json::parse(data);
    nlohmann::json variables = j["variables"];

    for (nlohmann::json::iterator it = variables.begin(); it != variables.end(); it++) {
        std::string option = it.key();
        if (option == "ep") {
            const nlohmann::json& row = it.value();
            const nlohmann::json& col = row.at(0);
            dimensions = glm::size3_t(col.size(), row.size(), 1);
        }

        if (_coordinateVariables.find(option) == _coordinateVariables.end()) {
            options.push_back(std::move(option));
        }
    }
    return options;
}

void DataProcessorJson::addDataValues(const std::string& data,
                                      SelectionProperty& dataOptions)
{
    int numOptions = static_cast<int>(dataOptions.options().size());
    initializeVectors(numOptions);

    if (data.empty()) {
        return;
    }

    const nlohmann::json& j = nlohmann::json::parse(data);
    nlohmann::json variables = j["variables"];

    std::vector<float> sum(numOptions, 0.f);
    std::vector<std::vector<float>> optionValues =
        std::vector<std::vector<float>>(numOptions, std::vector<float>());
    const std::vector<std::string>& options = dataOptions.options();

    for (int i = 0; i < numOptions; i++) {
        const nlohmann::json& row = variables[options[i]];

        for (size_t y = 0; y < row.size(); y++) {
            const nlohmann::json& col = row.at(y);

            for (size_t x = 0; x < col.size(); x++) {
                const float value = col.at(x).get<float>();
                optionValues[i].push_back(value);
                _min[i] = std::min(_min[i], value);
                _max[i] = std::max(_max[i], value);
                sum[i] += value;
            }
        }
    }

    add(optionValues, sum);
}

std::vector<std::vector<float>> DataProcessorJson::processData(const std::string& data,
                                                            SelectionProperty& optionProp,
                                                                 glm::size3_t& dimensions)
{
    if (data.empty()) {
        return std::vector<std::vector<float>>();
    }

    const nlohmann::json& j = nlohmann::json::parse(data);
    nlohmann::json variables = j["variables"];

    const std::set<std::string>& selectedOptions = optionProp;
    const std::vector<std::string>& options = optionProp.options();
    std::vector<int> selectedOptionsIndices;
    for (const std::string& option : selectedOptions) {
        auto it = std::find(options.begin(), options.end(), option);
        ghoul_assert(it != options.end(), "Selected option must be in all options");
        int idx = static_cast<int>(std::distance(options.begin(), it));
        selectedOptionsIndices.push_back(idx);
    }

    std::vector<std::vector<float>> dataOptions(options.size());
    for (int option : selectedOptionsIndices) {
        dataOptions[option] = std::vector<float>(dimensions.x * dimensions.y, 0.f);

        nlohmann::json row = variables[options[option]];
        const int rowsize = static_cast<int>(row.size());

        for (int y = 0; y < rowsize; y++) {
            nlohmann::json col = row.at(y);

            for (size_t x = 0; x < col.size(); x++) {
                const float value = col.at(x).get<float>();
                const size_t i = x + y * col.size();

                dataOptions[option][i] = processDataPoint(value, option);
            }
        }
    }

    calculateFilterValues(selectedOptionsIndices);
    return dataOptions;
}

} // namespace openspace
