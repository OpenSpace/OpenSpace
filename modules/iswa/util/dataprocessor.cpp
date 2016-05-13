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

namespace {
	const std::string _loggerCat = "DataPlane";
}

namespace openspace {
DataProcessor::DataProcessor(bool useLog, bool useHistogram, glm::vec2 normValues)
    :_useLog(useLog)
    ,_useHistogram(useHistogram)
    ,_normValues(normValues)
{};
DataProcessor::~DataProcessor(){};

std::vector<std::string> DataProcessor::readHeader(std::string& dataBuffer){
    std::vector<std::string> options = std::vector<std::string>();
    if(!dataBuffer.empty()){    
        std::stringstream memorystream(dataBuffer);
        std::string line;

        while(getline(memorystream,line)){
            if(line.find("#") == 0){
                if(line.find("# Output data:") == 0){

                    line = line.substr(26);
                    std::stringstream ss(line);

                    std::string token;
                    getline(ss, token, 'x');
                    int x = std::stoi(token);

                    getline(ss, token, '=');
                    int y = std::stoi(token);

                    _dimensions = glm::size3_t(x, y, 1);

                    getline(memorystream, line);
                    line = line.substr(1);

                    ss = std::stringstream(line);
                    std::string option;
                    while(ss >> option){
                        if(option != "x" && option != "y" && option != "z"){
                            options.push_back(option);
                        }
                    }
                }
            }else{
                break;
            }
        }
    }
    return options;
}

std::vector<float*> DataProcessor::readData(std::string& dataBuffer, properties::SelectionProperty dataOptions){
    if(!dataBuffer.empty()){
        // if(!_dataOptions.options().size()) // load options for value selection
        //     readHeader(dataBuffer);
        
        std::stringstream memorystream(dataBuffer);
        std::string line;

        std::vector<int> selectedOptions = dataOptions.value();

        int numSelected = selectedOptions.size();

        std::vector<float> min(numSelected, std::numeric_limits<float>::max()); 
        std::vector<float> max(numSelected, std::numeric_limits<float>::min());

        std::vector<float> sum(numSelected, 0.0f);
        std::vector<std::vector<float>> optionValues(numSelected, std::vector<float>());

        std::vector<float*> data(dataOptions.options().size(), nullptr);
        for(int option : selectedOptions){
            data[option] = new float[_dimensions.x*_dimensions.y]{0.0f};
        }
        
        int numValues = 0;
        while(getline(memorystream, line)){
            if(line.find("#") == 0){ //part of the header
                continue;
            }

            std::stringstream ss(line); 
            std::vector<float> value;
            float v;
            while(ss >> v){
                value.push_back(v);
            }

            if(value.size()){
                for(int i=0; i<numSelected; i++){

                    float v = value[selectedOptions[i]+3]; //+3 because "options" x, y and z.

                    if(_useLog){
                        int sign = (v>0)? 1:-1;
                        if(v != 0){
                            v = sign*log(fabs(v));
                        }
                    }

                    optionValues[i].push_back(v); 

                    min[i] = std::min(min[i], v);
                    max[i] = std::max(max[i], v);

                    sum[i] += v;
                }
                numValues++;
            }
        }
        // std::cout << "Actual size: " << numValues << " Expected: " << _dimensions.x*_dimensions.y   << std::endl;
        if(numValues != _dimensions.x*_dimensions.y){
            LWARNING("Number of values read and expected are not the same");
            return std::vector<float*>();
        }
        
        // FOR TESTING
        // ===========
        // std::chrono::time_point<std::chrono::system_clock> start, end;
        // start = std::chrono::system_clock::now();
        // ===========

        for(int i=0; i<numSelected; i++){
            processData(data[ selectedOptions[i] ], optionValues[i], min[i], max[i], sum[i]);
        }
        
        // FOR TESTING
        // ===========
        // end = std::chrono::system_clock::now();
        // _numOfBenchmarks++;
        // std::chrono::duration<double> elapsed_seconds = end-start;
        // _avgBenchmarkTime = ( (_avgBenchmarkTime * (_numOfBenchmarks-1)) + elapsed_seconds.count() ) / _numOfBenchmarks;
        // std::cout << " readData():" << std::endl;
        // std::cout << "avg elapsed time: " << _avgBenchmarkTime << "s\n";
        // std::cout << "num Benchmarks: " << _numOfBenchmarks << "\n";
        // ===========

        return data;
        
    } 
    else {
    //     LWARNING("Nothing in memory buffer, are you connected to the information super highway?");
        return std::vector<float*>();
    }
}

void DataProcessor::processData(float* outputData, std::vector<float>& inputData, float min, float max,float sum){
    
    // HISTOGRAM
    // number of levels/bins/values
    const int levels = 512;    
    // Normal Histogram where "levels" is the number of steps/bins
    std::vector<int> histogram = std::vector<int>(levels, 0);
    // Maps the old levels to new ones. 
    std::vector<float> newLevels = std::vector<float>(levels, 0.0f);
    
    const int numValues = inputData.size(); 
    
    //FOR TESTING ONLY
    //================
    // float entropyBefore;
    // float entropyAfter;
    // std::vector<int> histogramAfter = std::vector<int>(levels, 0);
    // auto calulateEntropy = [levels, numValues](std::vector<int> histogram){
    //     float entropy;
    //     for(auto frequency : histogram){
    //         if(frequency != 0)
    //             entropy -= ((float)frequency/numValues) * log2((float)frequency/numValues);
    //     }
    //     return entropy;
    // };
    //================
    
    // maps the data values to the histogram bin/index/level
    auto mapToHistogram = [levels](float val, float varMin, float varMax) {
        float probability = (val-varMin)/(varMax-varMin);
        float mappedValue = probability * levels;
        return glm::clamp(mappedValue, 0.0f, static_cast<float>(levels - 1));
    };
    
    
    //Calculate the mean
    float mean = (1.0 / numValues) * sum;
    //Calculate the Standard Deviation
    float standardDeviation = sqrt (((pow(sum, 2.0)) - ((1.0/numValues) * (pow(sum,2.0)))) / (numValues - 1.0));
    //calulate log mean
    // logmean /= numValues;   
    
    //HISTOGRAM FUNCTIONALITY
    //======================
    if(_useHistogram){
        for(int i = 0; i < numValues; i++){
            float v = inputData[i];
            float pixelVal = mapToHistogram(v, min, max);       
            histogram[(int)pixelVal]++;
            inputData[i] = pixelVal;
        }
        
        // Map mean and standard deviation to histogram levels
        mean = mapToHistogram(mean , min, max);
        // logmean = mapToHistogram(logmean , min, max);
        standardDeviation = mapToHistogram(standardDeviation, min, max);
        min = 0.0f;
        max = levels - 1.0f;
        
        //FOR TESTING
        //entropyBefore = calulateEntropy(histogram);

        //Calculate the cumulative distributtion function (CDF)
        float previousCdf = 0.0f;
        for(int i = 0; i < levels; i++){
            
            float probability = histogram[i] / (float)numValues; 
            float cdf  = previousCdf + probability;
            cdf = glm::clamp(cdf, 0.0f, 1.0f); //just in case
            newLevels[i] = cdf * (levels-1);
            previousCdf = cdf;
        }
    }
    //======================
    
    for(int i=0; i< numValues; i++){
        
        float v = inputData[i];
   
        // if use histogram get the equalized values
        if(_useHistogram){
            v = newLevels[(int)v];
            
            // FOR TESTING
            //histogramAfter[(int)v]++;

            // Map mean and standard deviation to new histogram levels
            mean =  newLevels[(int) mean];
            // logmean =  newLevels[(int) logmean];
            standardDeviation = newLevels[(int) standardDeviation];
        }
        
        v = normalizeWithStandardScore(v, mean, standardDeviation);         
        outputData[i] += v;

    }
    
    // FOR TESTING
    // ===========
    // entropyAfter = calulateEntropy(histogramAfter);
    // std::cout << "Entropy Before: "<< entropyBefore << std::endl;
    // std::cout << "Entropy After: "<< entropyAfter << std::endl;
    // ===========
    
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
}