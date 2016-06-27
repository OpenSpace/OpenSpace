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
#include <modules/iswa/util/iswamanager.h>
#include <fstream> 
namespace {
	const std::string _loggerCat = "DataProcessor";
    const int NumBins = 512;
}

namespace openspace {
DataProcessor::DataProcessor()
    :_useLog(false)
    ,_useHistogram(false)
    ,_normValues(glm::vec2(1.0))
    ,_filterValues(glm::vec2(0.0))
{
    _coordinateVariables = {"x", "y", "z", "phi", "theta"};
    // _normValues = glm::vec2(IswaManager::ref().fit()); //for testing
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
    _unNormalizedhistograms.clear();
}


float DataProcessor::processDataPoint(float value, int option){
    if(_numValues.empty()) return 0.0f;
    std::shared_ptr<Histogram> histogram = _histograms[option];
    float mean = (1.0 / _numValues[option]) * _sum[option];
    float sd = _standardDeviation[option];

    float v;
    if(_useHistogram){
        v = histogram->equalize(normalizeWithStandardScore(value, mean, sd, glm::vec2(_fitValues[option])))/(float)NumBins;
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
    if(_unNormalizedhistograms.empty())_unNormalizedhistograms = std::vector<std::shared_ptr<Histogram>>(numOptions, nullptr);
    if(_buildData.empty())_buildData = std::vector<std::vector<float>>(numOptions, std::vector<float>());
}

void DataProcessor::calculateFilterValues(std::vector<int> selectedOptions){
    int numSelected = selectedOptions.size();
    std::shared_ptr<Histogram> histogram;
    float mean, standardDeviation, filterMid, filterWidth;

    _filterValues = glm::vec2(0.0);
    if (numSelected <= 0) return;

    if (!_histograms.empty()) {
        for (int option : selectedOptions) {
            if (!_useHistogram) {
                mean = (1.0/_numValues[option])*_sum[option];
                standardDeviation = _standardDeviation[option];
                histogram = _unNormalizedhistograms[option];
                
                filterMid = histogram->highestBinValue(_useHistogram);
                filterWidth = histogram->binWidth();

                //at least one pixel value width. 1/numBins above mid and 1/numBins below mid => 1/(numBins/2) filtered
                // filterWidth = std::max(filterWidth, 1.0f/(float)NumBins);

                filterMid = normalizeWithStandardScore(filterMid, mean, standardDeviation, _normValues);
                filterWidth = fabs(0.5-normalizeWithStandardScore(mean+filterWidth, mean, standardDeviation, _normValues));

            }else{
                Histogram hist = _histograms[option]->equalize();
                filterMid = hist.highestBinValue(true);
                filterWidth = std::min(1.f / (float)NumBins, 1.0f/(float)NumBins);
            }
            _filterValues += glm::vec2(filterMid-filterWidth, filterMid+filterWidth);

        }
        _filterValues /= numSelected;
    }
}

void DataProcessor::add(std::vector<std::vector<float>>& optionValues, std::vector<float>& sum){
    int numOptions = optionValues.size();
    int numValues;
    float mean, value, variance, standardDeviation, low, high;

    // std::ofstream ofs[4];
    // ofs[0].open ("norm-entropy_n", std::ofstream::out | std::ofstream::app);
    // ofs[1].open ("norm-entropy_v", std::ofstream::out | std::ofstream::app);
    // ofs[2].open ("norm-entropy_bz", std::ofstream::out | std::ofstream::app);

    // ofs[0].open ("norm-entropy_eave", std::ofstream::out | std::ofstream::app);
    // ofs[1].open ("norm-entropy_eflux", std::ofstream::out | std::ofstream::app);
    // ofs[2].open ("norm-entropy_ep", std::ofstream::out | std::ofstream::app);
    // ofs[3].open ("norm-entropy_jr", std::ofstream::out | std::ofstream::app);

    for(int i=0; i<numOptions; i++){
        std::vector<float> values = optionValues[i];
        numValues = values.size();

        //set min, max for the unnormalized histogram
        if(!_unNormalizedhistograms[i]){
             _unNormalizedhistograms[i] = std::make_shared<Histogram>(_min[i], _max[i], NumBins);
        }else{   
            _unNormalizedhistograms[i]->changeRange(_min[i], _max[i]);
        }

        variance = 0;
        mean = (1.0f/numValues)*sum[i];

        //add values to unnormalized histogram and calculate variance
        for(int j=0; j<numValues; j++){
            value = values[j];
            variance +=  pow(value-mean, 2);
            _unNormalizedhistograms[i]->add(value, 1);
            _buildData[i].push_back(value);
        }
        _unNormalizedhistograms[i]->generateEqualizer();

        standardDeviation = sqrt(variance/ numValues);
        _sum[i] += sum[i];
        _standardDeviation[i] = sqrt(pow(standardDeviation, 2) + pow(_standardDeviation[i], 2));
        _numValues[i] += numValues;
        mean = (1.0f/_numValues[i])*_sum[i];

        const float* histData = _unNormalizedhistograms[i]->data();
        float histMin = _unNormalizedhistograms[i]->minValue();
        float histMax = _unNormalizedhistograms[i]->maxValue();
        int numBins = _unNormalizedhistograms[i]->numBins();

        // float fit = IswaManager::ref().fit();
        // float fit = _unNormalizedhistograms[i]->entropy()/IswaManager::ref().fit();

        // the E (entropy) method
        float fit = _unNormalizedhistograms[i]->entropy();
        _fitValues[i] = fit;


        float max = normalizeWithStandardScore(histMax, mean, _standardDeviation[i], glm::vec2(_fitValues[i]));
        float min = normalizeWithStandardScore(histMin, mean, _standardDeviation[i], glm::vec2(_fitValues[i]));

        std::shared_ptr<Histogram> newHist = std::make_shared<Histogram>(min, max, numBins);

        int length = _buildData[i].size();
        for(int j=0; j<length; j++){
            newHist->add(normalizeWithStandardScore(_buildData[i][j], mean, _standardDeviation[i], glm::vec2(_fitValues[i])), 1);
        }

        _histograms[i] = newHist;
        _histograms[i]->generateEqualizer(true);

        // _unNormalizedhistograms[i]->print();
        // ofs[i] << _fitValues[i] << " " << _histograms[i]->entropy() << std::endl;
        // std::cout << _fitValues[i] << " " << _histograms[i]->entropy() << std::endl;
        
    }


    // ofs[0].close();
    // ofs[1].close();
    // ofs[2].close();
    // ofs[3].close();

}

void DataProcessor::clearBuildData(){
    for(int i=0; _buildData.size(); i++){
        _buildData[i].clear();
    }
    _unNormalizedhistograms.clear();
}

}