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
    ,_histNormValues(glm::vec2(4.f, 4.f))
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
    _fitValues.clear();
}


float DataProcessor::processDataPoint(float value, int option){
    if(_numValues.empty()) return 0.0f;
    std::shared_ptr<Histogram> histogram = _histograms[option];
    float mean = (1.0 / _numValues[option]) * _sum[option];
    float sd = _standardDeviation[option];

    float v;
    if(_useHistogram){
        v = histogram->equalize(normalizeWithStandardScore(value, mean, sd, glm::vec2(_fitValues[option],_fitValues[option])))/(float)512;
        // v = histogram->equalize(value)/(float)512;
    }else{
        v = normalizeWithStandardScore(value, mean, sd, _normValues);
    }
    return v;
}

float DataProcessor::normalizeWithStandardScore(float value, float mean, float sd, glm::vec2 normalizationValues){
    
    float zScoreMin = normalizationValues.x;
    float zScoreMax = normalizationValues.y;
    float standardScore = ( value - mean ) / sd;
    // Clamp intresting values
    standardScore = glm::clamp(standardScore, -zScoreMin, zScoreMax);
    //return and normalize
    return ( standardScore + zScoreMin )/(zScoreMin + zScoreMax );  
}

float DataProcessor::unnormalizeWithStandardScore(float standardScore, float mean, float sd, glm::vec2 normalizationValues){
    float zScoreMin = normalizationValues.x;
    float zScoreMax = normalizationValues.y; 

    float value = standardScore*(zScoreMax+zScoreMin)-zScoreMin;
    value = value*sd+mean; 
    
    return value; 
}

void DataProcessor::initializeVectors(int numOptions){
    if(_min.empty()) _min = std::vector<float>(numOptions, std::numeric_limits<float>::max());
    if(_max.empty()) _max = std::vector<float>(numOptions, std::numeric_limits<float>::min());
    if(_sum.empty()) _sum = std::vector<float>(numOptions, 0.0f);
    if(_standardDeviation.empty())  _standardDeviation = std::vector<float>(numOptions, 0.0f);
    if(_numValues.empty()) _numValues  = std::vector<float>(numOptions, 0.0f);
    if(_fitValues.empty()) _fitValues  = std::vector<float>(numOptions, 0.0f);
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
            if(!_useHistogram){
                mean = (1.0/_numValues[option])*_sum[option];
                standardDeviation = _standardDeviation[option];
                histogram = _histograms[option];
                
                filterMid = histogram->highestBinValue(_useHistogram);
                filterWidth = mean+histogram->binWidth();
                
                filterMid = normalizeWithStandardScore(filterMid, mean, standardDeviation, _normValues);
                filterWidth = fabs(0.5-normalizeWithStandardScore(filterWidth, mean, standardDeviation, _normValues));
            }else{
                Histogram hist = _histograms[option]->equalize();
                filterMid = hist.highestBinValue(true);
                std::cout << filterMid << std::endl;
                filterWidth = 1.f/512.f;
            }

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

        std::vector<float> values = optionValues[i];
        numValues = values.size();

        variance = 0;
        mean = (1.0f/numValues)*sum[i];

        for(int j=0; j<numValues; j++){
            value = values[j];
            variance +=  pow(value-mean, 2);
        }

        standardDeviation = sqrt(variance/ numValues);

        float oldStandardDeviation = _standardDeviation[i];
        float oldMean = (1.0f/_numValues[i])*_sum[i];

        _sum[i] += sum[i];
        _standardDeviation[i] = sqrt(pow(standardDeviation, 2) + pow(_standardDeviation[i], 2));
        _numValues[i] += numValues;
        

        float fit = 100.0f*pow(_standardDeviation[i]/(_max[i]-_min[i]),2);
        float oldFit = _fitValues[i];
        _fitValues[i] = fit;

        // std::cout << "Fit: " << fit << std::endl;
        // std::cout << "Old fit: " << oldFit << std::endl << std::endl;

        mean = (1.0f/_numValues[i])*_sum[i];
        // float min = normalizeWithStandardScore(_min[i], mean, _standardDeviation[i], _histNormValues);
        // float max = normalizeWithStandardScore(_max[i], mean, _standardDeviation[i], _histNormValues);

        float min = normalizeWithStandardScore(_min[i], mean, _standardDeviation[i], glm::vec2(_fitValues[i],_fitValues[i]));
        float max = normalizeWithStandardScore(_max[i], mean, _standardDeviation[i], glm::vec2(_fitValues[i],_fitValues[i]));
        
        // float max = _max[i];
        // float min = _min[i];

        if(!_histograms[i]){
             _histograms[i] = std::make_shared<Histogram>(min, max, 512);
        }
        else{

            const float* histData = _histograms[i]->data();
            float histMin = _histograms[i]->minValue();
            float histMax = _histograms[i]->maxValue();
            int numBins = _histograms[i]->numBins();

            // float unNormHistMin = unnormalizeWithStandardScore(histMin, oldMean, oldStandardDeviation, _histNormValues);
            // float unNormHistMax = unnormalizeWithStandardScore(histMax, oldMean, oldStandardDeviation, _histNormValues);
            
            float unNormHistMin = unnormalizeWithStandardScore(histMin, oldMean, oldStandardDeviation, glm::vec2(oldFit, oldFit));
            float unNormHistMax = unnormalizeWithStandardScore(histMax, oldMean, oldStandardDeviation, glm::vec2(oldFit, oldFit));

            std::shared_ptr<Histogram> newHist = std::make_shared<Histogram>(
                // std::min(min, normalizeWithStandardScore(unNormHistMin, mean, _standardDeviation[i], _histNormValues)), 
                // std::min(max, normalizeWithStandardScore(unNormHistMax, mean, _standardDeviation[i], _histNormValues)),
                std::min(min, normalizeWithStandardScore(unNormHistMin, mean, _standardDeviation[i], glm::vec2(_fitValues[i],_fitValues[i]))), 
                std::min(max, normalizeWithStandardScore(unNormHistMax, mean, _standardDeviation[i], glm::vec2(_fitValues[i],_fitValues[i]))),
                // std::min(min, histMin),
                // std::max(max, histMax),
                numBins
            );

            for(int j=0; j<numBins; j++){
                value = j*(histMax-histMin)+histMin;
                value = unnormalizeWithStandardScore(value, oldMean, oldStandardDeviation, glm::vec2(oldFit, oldFit));
                _histograms[i]->add(normalizeWithStandardScore(value, mean, _standardDeviation[i], glm::vec2(_fitValues[i],_fitValues[i])), histData[j]);
                // _histograms[i]->add(value, histData[j]);
            }
            _histograms[i] = newHist;
        }

        for(int j=0; j<numValues; j++){
            value = values[j];
            _histograms[i]->add(normalizeWithStandardScore(value, mean, _standardDeviation[i], glm::vec2(_fitValues[i],_fitValues[i])), 1);
            // _histograms[i]->add(value, 1);
        }

        _histograms[i]->generateEqualizer();
        
        // std::cout << std::endl;

        // float histMin = _histograms[i]->minValue();
        // float histMax = _histograms[i]->maxValue();
        // int numBins = _histograms[i]->numBins();
        // int numValues = _histograms[i]->numValues();

        // float normalizedMean = (mean - histMin) / (histMax - histMin);      // [0.0, 1.0]
        // int binMeanIndex = std::min( (float)floor(normalizedMean * numBins), numBins - 1.0f ); // [0, _numBins - 1]

        // float normalizedSd = (_standardDeviation[i] - histMin) / (histMax - histMin);      // [0.0, 1.0]
        // int binSpanSd = std::min( (float)floor(normalizedSd * numBins), numBins - 1.0f ); // [0, _numBins - 1]

        // std::cout << binSpanSd << ", " << binMeanIndex << std::endl;
        // std::cout << std::endl;

        std::cout << "Normal Histogram: " << std::endl;
        _histograms[i]->print();
        std::cout << std::endl << std::endl;

        std::cout << "Equalized Histogram: " << std::endl;
        Histogram hist = _histograms[i]->equalize();
        hist.print();
        std::cout << std::endl;

        std::cout << "Norm value: " << _fitValues[i] << std::endl << std::endl;  
        // std::cout << "Min: " << histMin;   
        // std::cout << ", Max: " << histMax;   
        // std::cout << ", Sd: " << _standardDeviation[i];
        // std::cout << ", Mean: " << mean;
        // std::cout << ", Num: " << numValues;
        // std::cout << ", Bins: " << numBins << std::endl << std::endl;
    }
}

}