/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include <openspace/properties/selectionproperty.h>
#include <ghoul/glm.h>
#include <glm/gtx/std_based_type.hpp>
#include <set>
#include <openspace/util/histogram.h>

namespace openspace{
class DataProcessor{
    friend class IswaBaseGroup;
public:
    DataProcessor();
    ~DataProcessor();

    /**
     * Reads and returns the necessary metadata from the data file.
     * 
     * @param data The data file content to read metadata from
     * @param dimensions the dimensions of the data, will be modified
     * 
     * @return An array with data variable options
     */
    virtual std::vector<std::string> readMetadata(const std::string& data, glm::size3_t& dimensions) = 0;

    /**
     * Function that initializes the dataprocessor for a new data variable by 
     * calculating its min & max value, standard deviation, sum and histogram for a 
     * given data file. This file will then be used as reference when normalizing, filtering
     * and give auto-contrast 
     * 
     * @param data The data file content to add data from
     * @param dataOptions a list of dataoptions that you want to pre-process
     */
    virtual void addDataValues(const std::string& data, const properties::SelectionProperty& dataOptions) = 0;

    /**
     * Will take raw input data as a string and return it as a vector of 
     * float arrays (one float array per data variable that is selected).
     * Before returning it, the data is processed with normalization and 
     * an optional histogram equalization. 
     * 
     * @param data input data. directly from data file (file contents)
     * @param dataOptions Pass in the dataOptions property from the Renderable
     * @param dimensions Dimensions of the data
     * @return Processed data
     */
    virtual std::vector<float*> processData(const std::string& data, const properties::SelectionProperty& dataOptions, const glm::size3_t& dimensions) = 0;

    void useLog(bool useLog);
    void useHistogram(bool useHistogram);
    void normValues(glm::vec2 normValues);
    glm::size3_t dimensions();
    glm::vec2 filterValues();

    void clear();
protected:
    float processDataPoint(float value, int option);
    void initializeVectors(int numOptions);
    void calculateFilterValues(std::vector<int> selectedOptions);
    void add(std::vector<std::vector<float>>& optionValues, std::vector<float>& sum);


    std::vector<float> _min; 
    std::vector<float> _max;
    std::set<std::string> _coordinateVariables;
    glm::size3_t _dimensions;

    //glm::vec2 _histNormValues;
private:
    float normalizeWithStandardScore(float value, float mean, float sd, glm::vec2 normalizationValues = glm::vec2(1.0f, 1.0f));
    float unnormalizeWithStandardScore(float value, float mean, float sd, glm::vec2 normalizationValues = glm::vec2(1.0f, 1.0f));

    bool _useLog;
    bool _useHistogram;
    
    glm::vec2 _normValues;
    glm::vec2 _filterValues;
    
    std::vector<float> _sum;
    std::vector<float> _standardDeviation;
    std::vector<float> _numValues;
    std::vector<float> _fitValues;

    std::vector<std::shared_ptr<Histogram>> _histograms;

    std::vector<std::shared_ptr<Histogram>> _unNormalizedhistograms;
};

} // namespace openspace
#endif //__DATAPROCESSOR_H__