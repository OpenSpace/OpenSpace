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

#include <modules/iswa/util/dataprocessortext.h>

#include <openspace/properties/selectionproperty.h>
#include <openspace/util/histogram.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <sstream>

namespace openspace {

DataProcessorText::DataProcessorText() : DataProcessor() {}

DataProcessorText::~DataProcessorText() {}

std::vector<std::string> DataProcessorText::readMetadata(const std::string& data,
                                                         glm::size3_t& dimensions)
{
    if (data.empty()) {
        return std::vector<std::string>();
    }

    //The intresting part of the file looks like this:
    //# Output data: field with 61x61=3721 elements
    //# x           y           z           N           V_x         B_x

    // The string where the interesting data begins
    constexpr std::string_view info = "# Output data: field with ";
    std::vector<std::string> options;
    std::string line;
    std::stringstream memorystream(data);
    while (ghoul::getline(memorystream, line)) {
        if (line.find(info) == 0) {
            line = line.substr(info.size());
            std::stringstream ss(line);

            std::string token;
            ghoul::getline(ss, token, 'x');
            const int x = std::stoi(token);

            ghoul::getline(ss, token, '=');
            const int y = std::stoi(token);

            dimensions = glm::size3_t(x, y, 1);

            ghoul::getline(memorystream, line);
            line = line.substr(1); //because of the # char

            ss = std::stringstream(line);
            std::string option;
            while (ss >> option) {
                if (_coordinateVariables.find(option) == _coordinateVariables.end()) {
                    options.push_back(option);
                }
            }
        }
    }
    return options;
}

void DataProcessorText::addDataValues(const std::string& data,
                                      properties::SelectionProperty& dataOptions)
{
    int numOptions = static_cast<int>(dataOptions.options().size());
    initializeVectors(numOptions);

    if (data.empty()) {
        return;
    }

    std::string line;
    std::stringstream memorystream(data);

    // for standard diviation in the add() function
    std::vector<float> sum(numOptions, 0.f);
    std::vector<std::vector<float>> optionValues(numOptions);

    // for each data point
    while (ghoul::getline(memorystream, line)) {
        if (!line.empty() && line[0] == '#') {
            continue;
        }

        std::vector<float> values;
        std::istringstream ss(line);
        std::string val;
        int skip = 0;
        //for each data option (variable)
        while (ss >> val) {
            // first three values are coordinates
            if (skip < 3) {
                skip++;
                continue;
            }

            float v = std::stof(val);
            // Some values are "NaN", use 0 instead
            values.push_back(std::isnan(v) ? v : 0.f);
            val.clear();
        }

        if (values.empty()) {
            continue;
        }

        for (int i = 0; i < numOptions; i++) {
            const float value = values[i];

            optionValues[i].push_back(value);
            _min[i] = std::min(_min[i], value);
            _max[i] = std::max(_max[i], value);
            sum[i] += value;
        }
    }

    add(optionValues, sum);
}

std::vector<float*> DataProcessorText::processData(const std::string& data,
                                                   properties::SelectionProperty& options,
                                                                 glm::size3_t& dimensions)
{
    // The update of the selection properties broke this and we don't have the data to
    // actually test whether this update works. So if you are getting a crash around here
    // this is why

    if (data.empty()) {
        return std::vector<float*>();
    }

    std::string line;
    std::stringstream memorystream(data);

    const std::set<std::string>& selectedOptions = options.value();
    const std::vector<std::string>& allOptions = options.options();
    std::vector<int> selectedOptionsIndices;

    std::vector<float*> dataOptions(options.options().size(), nullptr);
    for (const std::string& o : selectedOptions) {
        auto it = std::find(allOptions.begin(), allOptions.end(), o);
        ghoul_assert(
            it != allOptions.end(),
            "Selected option must be in list of all options"
        );
        int idx = static_cast<int>(std::distance(allOptions.begin(), it));
        selectedOptionsIndices.push_back(idx);
        dataOptions[idx] = new float[dimensions.x * dimensions.y] { 0.f };
    }

    int numValues = 0;
    while (ghoul::getline(memorystream, line)) {
        if (!line.empty() && line[0] == '#') {
            continue;
        }

        int last = 0;
        int option = -3;
        int lineSize = static_cast<int>(line.size());

        while (last < lineSize) {
            const int first = static_cast<int>(line.find_first_not_of(" \t", last));
            last = static_cast<int>(line.find_first_of(" \t", first));
            last = (last > 0)? last : lineSize;

            const auto it = std::find(
                selectedOptionsIndices.begin(),
                selectedOptionsIndices.end(),
                option
            );
            if (option >= 0 && it != selectedOptionsIndices.end()) {
                const float value = std::stof(line.substr(first, last));
                dataOptions[option][numValues] = processDataPoint(value, option);
            }

            option++;
        }

        numValues++;
    }

    calculateFilterValues(selectedOptionsIndices);

    return dataOptions;
//#endif
}

} //namespace openspace
