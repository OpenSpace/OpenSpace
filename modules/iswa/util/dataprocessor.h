/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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

#ifndef __DATAPROCESSOR_H__
#define __DATAPROCESSOR_H__

#include <openspace/properties/propertyowner.h>
#include <openspace/properties/scalarproperty.h>
#include <openspace/properties/selectionproperty.h>
#include <ghoul/glm.h>
#include <ghoul/opengl/texture.h>
#include <set>
#include <openspace/util/histogram.h>

namespace openspace{
class DataProcessor{
    friend class IswaBaseGroup;
public:
    DataProcessor();
    ~DataProcessor();

    virtual std::vector<std::string> readMetadata(std::string data, glm::size3_t& dimensions) = 0;
    virtual void addDataValues(std::string data, properties::SelectionProperty& dataOptions) = 0;
    virtual std::vector<float*> processData(std::string data, properties::SelectionProperty& dataOptions, glm::size3_t& dimensions) = 0;

    void useLog(bool useLog);
    void useHistogram(bool useHistogram);
    void normValues(glm::vec2 normValues);
    glm::size3_t dimensions();
    glm::vec2 filterValues();

    void clear();
protected:
    float processDataPoint(float value, int option);
    float normalizeWithStandardScore(float value, float mean, float sd);

    void initializeVectors(int numOptions);
    void calculateFilterValues(std::vector<int> selectedOptions);
    void add(std::vector<std::vector<float>>& optionValues, std::vector<float>& sum);

    glm::size3_t _dimensions;
    bool _useLog;
    bool _useHistogram;
    glm::vec2 _normValues;
    glm::vec2 _filterValues;

    std::vector<float> _min; 
    std::vector<float> _max;
    std::vector<float> _sum;
    std::vector<float> _standardDeviation;
    std::vector<float> _numValues;
    std::vector<std::shared_ptr<Histogram>> _histograms;
    std::set<std::string> _coordinateVariables;
};

} // namespace openspace
#endif //__DATAPROCESSOR_H__