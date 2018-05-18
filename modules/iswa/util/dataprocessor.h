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

#ifndef __OPENSPACE_MODULE_ISWA___DATAPROCESSOR___H__
#define __OPENSPACE_MODULE_ISWA___DATAPROCESSOR___H__

#include <ghoul/glm.h>
#include <glm/gtx/std_based_type.hpp>
#include <memory>
#include <set>
#include <string>
#include <vector>

namespace openspace {

namespace properties { class SelectionProperty; }

class Histogram;

class DataProcessor {
    //friend class IswaBaseGroup;

public:
    DataProcessor() = default;
    virtual ~DataProcessor() = default;

    virtual std::vector<std::string> readMetadata(const std::string& data,
        glm::size3_t& dimensions) = 0;

    virtual void addDataValues(const std::string& data,
        properties::SelectionProperty& dataOptions) = 0;

    virtual std::vector<float*> processData(const std::string& data,
        properties::SelectionProperty& dataOptions, glm::size3_t& dimensions) = 0;

    void useLog(bool useLog);
    void useHistogram(bool useHistogram);
    void normValues(glm::vec2 normValues);
    glm::size3_t dimensions() const;
    glm::vec2 filterValues() const;

    void clear();

protected:
    float processDataPoint(float value, int option);

    float normalizeWithStandardScore(float value, float mean, float sd,
        const glm::vec2& normalizationValues = glm::vec2(1.f, 1.f));

    float unnormalizeWithStandardScore(float value, float mean, float sd,
        const glm::vec2& normalizationValues = glm::vec2(1.f, 1.f));

    void initializeVectors(int numOptions);
    void calculateFilterValues(const std::vector<int>& selectedOptions);
    void add(const std::vector<std::vector<float>>& optionValues,
        const std::vector<float>& sum);

    glm::size3_t _dimensions;
    bool _useLog = false;
    bool _useHistogram = false;
    glm::vec2 _normValues = glm::vec2(1.f);
    glm::vec2 _filterValues = glm::vec2(0.f);

    std::vector<float> _min;
    std::vector<float> _max;
    std::vector<float> _sum;
    std::vector<float> _standardDeviation;
    std::vector<float> _numValues;
    std::vector<std::unique_ptr<Histogram>> _histograms;
    std::set<std::string> _coordinateVariables = { "x", "y", "z", "phi", "theta" };

    glm::vec2 _histNormValues = glm::vec2(10.f, 10.f);
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_ISWA___DATAPROCESSOR___H__
