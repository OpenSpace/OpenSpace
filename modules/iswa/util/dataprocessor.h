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

namespace openspace{
class DataProcessor{
    friend class IswaGroup;
public:
    DataProcessor(bool useLog, bool useHistogram, glm::vec2 normValues);
    ~DataProcessor();

    void useLog(bool useLog){
        _useLog = useLog;
    }

    void useHistogram(bool useHistogram){
        _useHistogram = useHistogram;
    }

    void normValues(glm::vec2 normValues){
        _normValues = normValues;
    }

    glm::size3_t dimensions(){
        return _dimensions;
    }

    std::vector<std::string> readHeader(std::string& dataBuffer);
    std::vector<float*> readData(std::string& dataBuffer, properties::SelectionProperty dataOptions);

    std::vector<std::string> readJSONHeader(std::string& dataBuffer);
    std::vector<float*> readJSONData(std::string& dataBuffer, properties::SelectionProperty dataOptions);

    std::vector<float*> processKameleonData(std::vector<float*> kdata, glm::size3_t dimensions, properties::SelectionProperty dataOptions);

    glm::vec2 filterValues();
    
private:
    void processData(
        float* outputData, // Where you want your processed data to go 
        std::vector<float>& inputData, //data that needs processing 
        float min, // min value of the input data
        float max, // max valye of the input data
        float sum // sum of the input data 
    );

    float normalizeWithStandardScore(float value, float mean, float sd);

    glm::size3_t _dimensions;
    bool _useLog;
    bool _useHistogram;
    glm::vec2 _normValues;
    glm::vec2 _filterValues;

    std::set<std::string> _coordinateVariables;
};

} // namespace openspace
#endif //__DATAPROCESSOR_H__