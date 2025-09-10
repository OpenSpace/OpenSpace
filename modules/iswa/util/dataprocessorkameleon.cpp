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

#include <modules/iswa/util/dataprocessorkameleon.h>

#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/properties/misc/selectionproperty.h>
#include <openspace/util/histogram.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <algorithm>
#include <filesystem>

namespace openspace {

DataProcessorKameleon::DataProcessorKameleon() : DataProcessor() {}

DataProcessorKameleon::~DataProcessorKameleon() {}

std::vector<std::string> DataProcessorKameleon::readMetadata(const std::string& path,
                                                             glm::size3_t&)
{
    if (path.empty()) {
        return std::vector<std::string>();
    }

    if ((path != _kwPath) || !_kw) {
        initializeKameleonWrapper(path);
    }

    std::vector<std::string> opts = _kw->variables();
    opts.erase(
        std::remove_if(
            opts.begin(),
            opts.end(),
            [this](const std::string& opt) {
                    return (opt.size() > 3 ||
                        _coordinateVariables.find(opt) != _coordinateVariables.end());
                }
        ),
        opts.end()
    );
    return opts;
}

void DataProcessorKameleon::addDataValues(const std::string& path,
                                          properties::SelectionProperty& dataOptions)
{
    int numOptions = static_cast<int>(dataOptions.options().size());
    initializeVectors(numOptions);

    if (path.empty()) {
        return;
    }

    if ((path != _kwPath) || !_kw) {
        initializeKameleonWrapper(path);
    }

    std::vector<float> sum(numOptions, 0.f);
    std::vector<std::vector<float>> optionValues(numOptions, std::vector<float>());
    const std::vector<std::string>& options = dataOptions.options();

    const int numValues = static_cast<int>(_dimensions.x * _dimensions.y * _dimensions.z);

    for (int i = 0; i < numOptions; i++) {
        //0.5 to gather interesting values for the normalization/histograms.
        float* values = _kw->uniformSliceValues(
            options[i],
            _dimensions,
            0.5f
        );

        for (int j=0; j<numValues; j++) {
            const float value = values[j];

            optionValues[i].push_back(value);
            _min[i] = std::min(_min[i], value);
            _max[i] = std::max(_max[i], value);
            sum[i] += value;
        }
    }

    add(optionValues, sum);
}

std::vector<float*> DataProcessorKameleon::processData(const std::string& path,
                                                properties::SelectionProperty& optionProp,
                                                                 glm::size3_t& dimensions)
{
    const int numOptions = static_cast<int>(optionProp.options().size());

    if (path.empty()) {
        return std::vector<float*>(numOptions, nullptr);
    }

    if ((path != _kwPath) || !_kw) {
        initializeKameleonWrapper(path);
    }

    const std::set<std::string>& selectedOptions = optionProp;
    const std::vector<std::string>& options = optionProp.options();
    std::vector<int> selectedOptionsIndices;
    for (const std::string& option : selectedOptions) {
        auto it = std::find(options.begin(), options.end(), option);
        ghoul_assert(it != options.end(), "Selected option must be in all options");
        int idx = static_cast<int>(std::distance(options.begin(), it));
        selectedOptionsIndices.push_back(idx);
    }

    const int numValues = static_cast<int>(glm::compMul(dimensions));

    std::vector<float*> dataOptions(numOptions, nullptr);
    for (int option : selectedOptionsIndices) {
        dataOptions[option] = _kw->uniformSliceValues(
            options[option],
            dimensions,
            _slice
        );

        for (int i = 0; i < numValues; i++) {
            const float value = dataOptions[option][i];
            dataOptions[option][i] = processDataPoint(value, option);
        }
    }

    calculateFilterValues(selectedOptionsIndices);
    return dataOptions;
}

void DataProcessorKameleon::setSlice(float slice) {
    _slice = slice;
}

void DataProcessorKameleon::setDimensions(glm::size3_t dimensions) {
    _dimensions = std::move(dimensions);
}

void DataProcessorKameleon::initializeKameleonWrapper(std::string path) {
    std::filesystem::path extension = std::filesystem::path(absPath(path)).extension();
    if (std::filesystem::is_regular_file(absPath(path)) && extension == ".cdf") {
        if (_kw) {
            _kw->close();
        }

        _kwPath = std::move(path);
        _kw = std::make_shared<KameleonWrapper>(absPath(_kwPath).string());
    }
}

} //namespace openspace
