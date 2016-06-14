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
#include <modules/iswa/util/dataprocessor.h>
#include <openspace/util/histogram.h>

#include <fstream>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <modules/iswa/util/iswamanager.h>
#include <modules/iswa/ext/json/json.hpp>

namespace {
	const std::string _loggerCat = "DataProcessor";
    using json = nlohmann::json;
}

namespace openspace {
DataProcessor::DataProcessor()
    :_useLog(false)
    ,_useHistogram(false)
    ,_normValues(glm::vec2(1.0))
    ,_filterValues(glm::vec2(0.0))
{
    _coordinateVariables = {"x", "y", "z", "phi", "theta"};
}

DataProcessor::~DataProcessor(){};

void DataProcessor::useLog(bool useLog){
    _useLog = useLog;
}

void DataProcessor::useHistogram(bool useHistogram){
    _useHistogram = useHistogram;
}

void DataProcessor::normValues(glm::vec2 normValues){
    _normValues = normValues;
}

glm::size3_t DataProcessor::dimensions(){
    return _dimensions;
}

glm::vec2 DataProcessor::filterValues(){
    return _filterValues;
}

void DataProcessor::clear(){
    _min.clear();
    _max.clear();
    _sum.clear();
    _standardDeviation.clear();
    _histograms.clear();
    _numValues.clear();
}


float DataProcessor::processDataPoint(float value, int option){
    if(_numValues.empty()) return 0.0f;
    std::shared_ptr<Histogram> histogram = _histograms[option];
    float mean = (1.0 / _numValues[option]) * _sum[option];
    float sd = _standardDeviation[option];

    if(_useHistogram){
        sd = histogram->equalize(sd);
        mean = histogram->equalize(mean);
        value = histogram->equalize(value);
    }

    float v = normalizeWithStandardScore(value, mean, sd);
    return v;
}

float DataProcessor::normalizeWithStandardScore(float value, float mean, float sd){
    
    float zScoreMin = _normValues.x;
    float zScoreMax = _normValues.y;
    float standardScore = ( value - mean ) / sd;
    // Clamp intresting values
    standardScore = glm::clamp(standardScore, -zScoreMin, zScoreMax);
    //return and normalize
    return ( standardScore + zScoreMin )/(zScoreMin + zScoreMax );  
}

void DataProcessor::initializeVectors(int numOptions){
    if(_min.empty()) _min = std::vector<float>(numOptions, std::numeric_limits<float>::max());
    if(_max.empty()) _max = std::vector<float>(numOptions, std::numeric_limits<float>::min());
    if(_sum.empty()) _sum = std::vector<float>(numOptions, 0.0f);
    if(_standardDeviation.empty())  _standardDeviation = std::vector<float>(numOptions, 0.0f);
    if(_numValues.empty()) _numValues  = std::vector<float>(numOptions, 0.0f);
    if(_histograms.empty())_histograms = std::vector<std::shared_ptr<Histogram>>(numOptions, nullptr);
}

void DataProcessor::calculateFilterValues(std::vector<int> selectedOptions){
    int numSelected = selectedOptions.size();
    std::shared_ptr<Histogram> histogram;
    float mean, standardDeviation, filterMid, filterWidth;

    _filterValues = glm::vec2(0.0);
    if(numSelected <= 0) return;

    if(!_histograms.empty()){
        for(int option : selectedOptions){
            histogram = _histograms[option];
            mean = (1.0/_numValues[option])*_sum[option];
            standardDeviation = _standardDeviation[option];

            filterMid = histogram->highestBinValue(_useHistogram);
            filterWidth = mean+histogram->binWidth();

            if(_useHistogram){
                standardDeviation = histogram->equalize(standardDeviation);
                mean = histogram->equalize(mean);
                filterWidth = mean+1;
            }

            filterMid = normalizeWithStandardScore(filterMid, mean, standardDeviation);
            filterWidth = fabs(0.5-normalizeWithStandardScore(filterWidth, mean, standardDeviation));
            _filterValues += glm::vec2(filterMid, filterWidth);

        }
        _filterValues /= numSelected;   
    }
}

void DataProcessor::add(std::vector<std::vector<float>>& optionValues, std::vector<float>& sum){
    int numOptions = optionValues.size();
    int numValues;
    float mean, value, variance, standardDeviation;

    for(int i=0; i<numOptions; i++){
        if(!_histograms[i]){
             _histograms[i] = std::make_shared<Histogram>(_min[i], _max[i], 512);
        }
        else{
            _histograms[i]->changeRange(_min[i], _max[i]);
        }

        std::vector<float> values = optionValues[i];
        numValues = values.size();

        variance = 0;
        mean = (1.0f/numValues)*sum[i];

        for(int j=0; j<numValues; j++){
            value = values[j];
            variance +=  pow(value-mean, 2);
            _histograms[i]->add(value, 1);
        }

        standardDeviation = sqrt(variance/ numValues);

        _sum[i] += sum[i];
        _standardDeviation[i] = sqrt(pow(standardDeviation, 2) + pow(_standardDeviation[i], 2));
        _numValues[i] += numValues;

        _histograms[i]->generateEqualizer();
        // _histograms[i]->print();
    }
}

}