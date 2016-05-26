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
	const std::string _loggerCat = "DataPlane";
    using json = nlohmann::json;
}

namespace openspace {
DataProcessor::DataProcessor(bool useLog, bool useHistogram, glm::vec2 normValues)
    :_useLog(useLog)
    ,_useHistogram(useHistogram)
    ,_normValues(normValues)
    ,_filterValues(glm::vec2(0))
    ,_numValues(0)
{
    _coordinateVariables = {"x", "y", "z", "phi", "theta"};
};
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
                        if(_coordinateVariables.find(option) == _coordinateVariables.end()){
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

std::vector<std::string> DataProcessor::readJSONHeader(std::string& dataBuffer){
    std::vector<std::string> options = std::vector<std::string>();
    if(!dataBuffer.empty()){
        json j = json::parse(dataBuffer);
        json var = j["variables"];
        for (json::iterator it = var.begin(); it != var.end(); ++it) {
            std::string option = it.key();
            if(option == "x"){
                json lon = it.value();
                json lat = lon.at(0);

                _dimensions = glm::size3_t(lat.size(), lon.size(), 1);
            }
            if(_coordinateVariables.find(option) == _coordinateVariables.end()){
                options.push_back(option);
            }
        }
    }
    return options;
}

void DataProcessor::addValues(std::string& dataBuffer, properties::SelectionProperty dataOptions){
    int numOptions = dataOptions.options().size();
    
    if(_min.empty()) _min = std::vector<float>(numOptions, std::numeric_limits<float>::max());
    if(_max.empty()) _max = std::vector<float>(numOptions, std::numeric_limits<float>::min());
    if(_sum.empty()) _sum = std::vector<float>(numOptions, 0.0f);
    if(_sd.empty()) _sd = std::vector<float>(numOptions, 0.0f);
    if(_numValues.empty()) _numValues= std::vector<float>(numOptions, 0.0f);
    if(_histograms.empty())_histograms = std::vector<std::shared_ptr<Histogram>>(numOptions, nullptr);

    if(!dataBuffer.empty()){

        std::stringstream memorystream(dataBuffer);
        std::string line;
        std::vector<float> sum(numOptions, 0.0f);
        std::vector<std::vector<float>> values(numOptions, std::vector<float>());

        int numValues = 0;
        while(getline(memorystream, line)){
            if(line.find("#") == 0) continue;

            std::stringstream ss(line); 
            std::vector<float> value;
            float v;
            while(ss >> v){
                value.push_back(v);
            }

            if(value.size()){
                for(int i=0; i<numOptions; i++){
                    float v = value[i+3];

                    values[i].push_back(v); 
                    _min[i] = std::min(_min[i], v);
                    _max[i] = std::max(_max[i], v);
                    sum[i] += v;
                }
                numValues++;
            }
        }

        for(int i=0; i<numOptions; i++){
            if(!_histograms[i]){
                _histograms[i] = std::make_shared<Histogram>(_min[i], _max[i], 512);
            }else{
                _histograms[i]->changeRange(_min[i], _max[i]);
            }
            int numValues = values[i].size();
            float mean = (1.0/numValues)*sum[i];
        
            float var = 0;
            for(int j=0; j<numValues; j++){
                var += pow(values[i][j] - mean, 2);
                _histograms[i]->add(values[i][j], 1);
            }
            float sd = sqrt(var / numValues);

            _sum[i] += sum[i];
            _sd[i] = sqrt(pow(_sd[i],2) + pow(sd, 2));
            _numValues[i] += numValues;
            _histograms[i]->generateEqualizer();
        }
    }
}



std::vector<float*> DataProcessor::readData(std::string& dataBuffer, properties::SelectionProperty dataOptions){
    if(!dataBuffer.empty()){
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
                        v = sign*log(fabs(v) + 1);
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


std::vector<float*> DataProcessor::readData2(std::string& dataBuffer, properties::SelectionProperty dataOptions){
    if(!dataBuffer.empty()){
        std::stringstream memorystream(dataBuffer);
        std::string line;

        std::vector<int> selectedOptions = dataOptions.value();
        int numSelected = selectedOptions.size();

        std::vector<std::vector<float>> values(selectedOptions.size(), std::vector<float>());
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
                for(int option : selectedOptions){
                    float v = value[option+3]; //+3 because "options" x, y and z.
                    data[option][numValues] = processDataPoint(v, option);
                }
            }
            numValues++;
        }

        if(numValues != _dimensions.x*_dimensions.y){
            LWARNING("Number of values read and expected are not the same");
            return std::vector<float*>();
        }


        _filterValues = glm::vec2(0.0f);
        if(!_histograms.empty()){
            for(int option : selectedOptions){
                std::shared_ptr<Histogram> histogram = _histograms[option];
                float mean = (1.0 / _numValues[option]) * _sum[option];
                float sd = _sd[option];

                float filterMid = histogram->highestBinValue(_useHistogram);
                float filterWidth = mean+histogram->binWidth();

                if(_useHistogram) {
                    sd = histogram->equalize(sd);
                    mean = histogram->equalize(mean);
                    filterWidth = mean+1.0;
                }

                filterMid = normalizeWithStandardScore(filterMid, mean, sd);
                filterWidth = fabs(0.5-normalizeWithStandardScore(filterWidth, mean, sd));
                _filterValues += glm::vec2(filterMid, filterWidth);
            }
        }
        
        if(numSelected>0){
            _filterValues.x /= numSelected;
            _filterValues.y /= numSelected;
        }else{
            _filterValues = glm::vec2(0.0, 1.0);
        }

        return data;

    }else{
        return std::vector<float*>();
    }
}

std::vector<float*> DataProcessor::readJSONData(std::string& dataBuffer, properties::SelectionProperty dataOptions){
    if(!dataBuffer.empty()){
        json j = json::parse(dataBuffer);
        json var = j["variables"];

        std::vector<int> selectedOptions = dataOptions.value();
        int numSelected = selectedOptions.size();

        std::vector<float> min(numSelected, std::numeric_limits<float>::max()); 
        std::vector<float> max(numSelected, std::numeric_limits<float>::min());

        std::vector<float> sum(numSelected, 0.0f);
        std::vector<std::vector<float>> optionValues(numSelected, std::vector<float>());

        auto options = dataOptions.options();        

        std::vector<float*> data(options.size(), nullptr);
        int i = 0; 
        for(int option : selectedOptions){

            data[option] = new float[_dimensions.x*_dimensions.y]{0.0f};

            std::string optionName = options[option].description;

            json valueArray = var[optionName];
            int ySize = valueArray.size();

            for(int y=0; y<valueArray.size(); y++){
                json values = valueArray.at(y);
                for(int x=0; x<values.size(); x++){
                    float v = values.at(x);
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
            }
            i++;
        }

        for(int i=0; i<numSelected; i++){
            processData(data[ selectedOptions[i] ], optionValues[i], min[i], max[i], sum[i]);
        }
        
        return data;
    } 
    else {
    //     LWARNING("Nothing in memory buffer, are you connected to the information super highway?");
        return std::vector<float*>();
    }
}

void DataProcessor::addValuesFromJSON(std::string& dataBuffer, properties::SelectionProperty dataOptions){
    int numOptions = dataOptions.options().size();
    
    if(_min.empty()) _min = std::vector<float>(numOptions, std::numeric_limits<float>::max());
    if(_max.empty()) _max = std::vector<float>(numOptions, std::numeric_limits<float>::min());
    if(_sum.empty()) _sum = std::vector<float>(numOptions, 0.0f);
    if(_sd.empty()) _sd = std::vector<float>(numOptions, 0.0f);
    if(_numValues.empty()) _numValues= std::vector<float>(numOptions, 0.0f);
    if(_histograms.empty())_histograms = std::vector<std::shared_ptr<Histogram>>(numOptions, nullptr);


    if(!dataBuffer.empty()){
        json j = json::parse(dataBuffer);
        json var = j["variables"];

        std::vector<int> selectedOptions = dataOptions.value();
        int numSelected = selectedOptions.size();
        std::vector<float> sum(numOptions, 0.0f);

        std::vector<std::vector<float>> values(numOptions, std::vector<float>());
        auto options = dataOptions.options();        
        std::vector<float*> data(options.size(), nullptr);
        int i = 0;

        for(int i=0; i<numOptions; i++){
            // std::stringstream memorystream();
            std::string optionName = options[i].description;
            // getline(memorystream, optionName, '/');
            // getline(memorystream, optionName, '/');

            json valueArray = var[optionName];
            int ySize = valueArray.size();

            for(int y=0; y<valueArray.size(); y++){
                json value = valueArray.at(y);
                for(int x=0; x<value.size(); x++){

                    float v = value.at(x);
                    values[i].push_back(v);

                    _min[i] = std::min(_min[i],v);
                    _max[i] = std::max(_max[i],v);
                    sum[i] += v;
                }
            }
        }
    //    //  //    for(int i=0; i<numOptions; i++){
    //    //  //     if(!_histograms[i]){
    //    //  //         _histograms[i] = std::make_shared<Histogram>(_min[i], _max[i], 512);
    //    //  //     }else{
    //    //  //         //_histogram[option]->changeRange();
    //    //  //     }
    //    //  //      int numValues = values[i].size();
    //    //  //     float mean = (1.0/numValues)*sum[i];

    //    //  //     float var = 0;
    //    //  //     for(int j=0; j<numValues; j++){
    //    //  //         var += pow(values[i][j] - mean, 2);
    //    //  //         _histograms[i]->add(values[i][j], 1);
    //    //  //     }
    //    //  //     float sd = sqrt(var / numValues);

    //    //  //     _sum[i] += sum[i];
    //    //  //     _sd[i] = sqrt(pow(_sd[i],2) + pow(sd, 2));
    //    //  //     _numValues[i] += numValues;
    //    //  //     _histograms[i]->generateEqualizer();
    //    //  // }


       for(int i=0; i<numOptions; i++){
            if(!_histograms[i]){
                _histograms[i] = std::make_shared<Histogram>(_min[i], _max[i], 512);
            }else{
                _histograms[i]->changeRange(_min[i], _max[i]);
            }
            int numValues = values[i].size();
            float mean = (1.0/numValues)*sum[i];
        
            float var = 0;
            for(int j=0; j<numValues; j++){
                var += pow(values[i][j] - mean, 2);
                _histograms[i]->add(values[i][j], 1);
            }

            float sd = sqrt(var / numValues);

            _sum[i] += sum[i];
            _sd[i] = sqrt(pow(_sd[i],2) + pow(sd, 2));
            _numValues[i] += numValues;
            _histograms[i]->generateEqualizer();
        }
    }
}


std::vector<float*> DataProcessor::readJSONData2(std::string& dataBuffer, properties::SelectionProperty dataOptions){
    if(!dataBuffer.empty()){
        json j = json::parse(dataBuffer);
        json var = j["variables"];

        std::vector<int> selectedOptions = dataOptions.value();
        int numSelected = selectedOptions.size();

        std::vector<float> sum(numSelected, 0.0f);
        std::vector<std::vector<float>> values(numSelected, std::vector<float>());
        auto options = dataOptions.options();        

        std::vector<float*> data(options.size(), nullptr);

        _filterValues = glm::vec2(0.0f);

        for(int option : selectedOptions){

            data[option] = new float[_dimensions.x*_dimensions.y]{0.0f};

            // std::stringstream memorystream();
            std::string optionName = options[option].description;
            // getline(memorystream, optionName, '/');
            // getline(memorystream, optionName, '/');

            json yArray = var[optionName];
            for(int y=0; y<yArray.size(); y++){
                json xArray = yArray.at(y);
                for(int x=0; x<xArray.size(); x++){

                    int i = x + y*xArray.size();
                    // std::cout << _dimensions.x*_dimensions.y << " " << i << std::endl;
                    float v = xArray.at(x);
                    data[option][i] = processDataPoint(v, option);

                }
            }

            if(!_histograms.empty()){
                float mean = (1.0 / _numValues[option]) * _sum[option];
                float sd = _sd[option];

                std::shared_ptr<Histogram> histogram = _histograms[option];
                float filterMid = histogram->highestBinValue(_useHistogram);
                float filterWidth = mean+histogram->binWidth();

                if(_useHistogram) {
                    sd = histogram->equalize(sd);
                    mean = histogram->equalize(mean);
                    filterWidth = mean+1.0;
                }

                filterMid = normalizeWithStandardScore(filterMid, mean, sd);
                filterWidth = fabs(0.5-normalizeWithStandardScore(filterWidth, mean, sd));
                _filterValues += glm::vec2(filterMid, filterWidth);
            }
        }

        if(numSelected>0){
            _filterValues.x /= numSelected;
            _filterValues.y /= numSelected;
        }else{
            _filterValues = glm::vec2(0.0, 1.0);
        }

        return data;
    } 
    else {
        // LWARNING("Nothing in memory buffer, are you connected to the information super highway?");
        return std::vector<float*>();
    }
}


void DataProcessor::addValuesFromKameleonData(float* kdata, glm::size3_t dimensions, int numOptions, int option){
    if(_min.empty()) _min = std::vector<float>(numOptions, std::numeric_limits<float>::max());
    if(_max.empty()) _max = std::vector<float>(numOptions, std::numeric_limits<float>::min());
    if(_sum.empty()) _sum= std::vector<float>(numOptions, 0.0f);
    if(_sd.empty()) _sd= std::vector<float>(numOptions, 0.0f);
    if(_numValues.empty()) _numValues= std::vector<float>(numOptions, 0.0f);
    if(_histograms.empty())_histograms = std::vector<std::shared_ptr<Histogram>>(numOptions, nullptr);


    int numValues = dimensions.x*dimensions.y*dimensions.z;
    float sum = 0;

    for(int i=0; i<numValues; i++){
        float v = kdata[i];
        _min[option] = std::min(_min[option],v);
        _max[option] = std::max(_max[option],v);
        sum += v;
    }

    int i = option;
    // for(int i=0; i<numOptions; i++){
    if(!_histograms[i]){
        _histograms[i] = std::make_shared<Histogram>(_min[i], _max[i], 512);
    }else{
        _histograms[i]->changeRange(_min[i], _max[i]);
    }
    // int numValues = values[i].size();
    float mean = (1.0/numValues)*sum;

    float var = 0;
    for(int j=0; j<numValues; j++){
        var += pow(kdata[j] - mean, 2);
        _histograms[i]->add(kdata[j], 1);
    }
    float sd = sqrt(var / numValues);

    _sum[i] += sum;
    _sd[i] = sqrt(pow(_sd[i],2) + pow(sd, 2));
    _numValues[i] += numValues;
    _histograms[i]->generateEqualizer();
}

std::vector<float*> DataProcessor::processKameleonData2(std::vector<float*> kdata, glm::size3_t dimensions, properties::SelectionProperty dataOptions){
    std::vector<int> selectedOptions = dataOptions.value();
    int numSelected = selectedOptions.size();

    std::vector<std::vector<float>> values(selectedOptions.size(), std::vector<float>());
    std::vector<float*> data(dataOptions.options().size(), nullptr);
    int numValues = dimensions.x*dimensions.y*dimensions.z;

    _filterValues = glm::vec2(0.0f);

    for(int option : selectedOptions){
        data[option] = new float[numValues]{0.0f};

        float mean = (1.0 / _numValues[option]) * _sum[option];
        float sd = _sd[option];

        for(int i=0; i<numValues; i++){
            float v = kdata[option][i];
            data[option][i] = processDataPoint(v, option);
        }

        std::shared_ptr<Histogram> histogram = _histograms[option];
        float filterMid = histogram->highestBinValue(_useHistogram);
        float filterWidth = mean+histogram->binWidth();

        if(_useHistogram) {
            sd = histogram->equalize(sd);
            mean = histogram->equalize(mean);
            filterWidth = mean+1.0;
        }

        filterMid = normalizeWithStandardScore(filterMid, mean, sd);
        filterWidth = fabs(0.5-normalizeWithStandardScore(filterWidth, mean, sd));
        _filterValues += glm::vec2(filterMid, filterWidth);
    }
    if(numSelected>0){
        _filterValues.x /= numSelected;
        _filterValues.y /= numSelected;
    }else{
        _filterValues = glm::vec2(0.0, 1.0);
    }

    return data;
}

std::vector<float*> DataProcessor::processKameleonData(std::vector<float*> kdata, glm::size3_t dimensions, properties::SelectionProperty dataOptions){
    std::vector<int> selectedOptions = dataOptions.value();
    int numSelected = selectedOptions.size();
    auto options = dataOptions.options();        
    int numOptions = options.size();

    if(_min.empty()){
        _min = std::vector<float>(numOptions, std::numeric_limits<float>::max());
    }

    if(_max.empty()){
        _max = std::vector<float>(numOptions, std::numeric_limits<float>::min());
    }

    if(_sum.empty()){
        _sum= std::vector<float>(numOptions, 0.0f);
    }

    if(_sd.empty()){
        _sd= std::vector<float>(numOptions, 0.0f);
    }

    if(_histograms.empty()){
        _histograms = std::vector<std::shared_ptr<Histogram>>(numOptions, nullptr);
    }


    std::vector<float> min(numSelected, std::numeric_limits<float>::max());
    std::vector<float> max(numSelected, std::numeric_limits<float>::min());

    std::vector<float> sum(numSelected, 0.0f);
    std::vector<std::vector<float>> optionValues(numSelected, std::vector<float>());


    std::vector<float*> data(options.size(), nullptr);
    int numValues = dimensions.x*dimensions.y*dimensions.z;
    int i = 0; 

    for(int option : selectedOptions){
        bool calculateMin = (_min[option] == std::numeric_limits<float>::max());
        bool calculateMax = (_max[option] == std::numeric_limits<float>::min());
        bool claculateSum = (_sum[option] == 0.0f);

        data[option] = new float[numValues]{0.0f};

        for(int j=0; j<numValues; j++){
            float v = kdata[option][j];

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

            if(calculateMin)
                _min[option] = std::min(_min[option],v);
            if(calculateMax)
                _max[option] = std::max(_max[option],v);
            if(claculateSum)
                _sum[option] += v;
        }
        i++;
        // if(calculateMin)
        //     std::cout << _min[option] << std::endl;
    }

    for(int i=0; i<numSelected; i++){
        int selected = selectedOptions[i];
        processData(data[ selected ], optionValues[i], _min[selected], _max[selected], _sum[selected], selected);
    }
        
    return data;
}

void DataProcessor::processData(float* outputData, std::vector<float>& inputData, float min, float max,float sum, int selected){
    const int numValues = inputData.size(); 
    Histogram histogram(min, max, 512); 

    //Calculate the mean
    float mean = (1.0 / numValues) * sum;

    //Calculate the Standard Deviation 
    float var = 0;
    for(auto dataValue : inputData){
        var += pow(dataValue - mean, 2);
    }
    float standardDeviation = sqrt ( var / numValues );

    // Histogram functionality
    if(_useHistogram){
        for(auto dataValue : inputData){
            histogram.add(dataValue, 1);
        }
        histogram.generateEqualizer();
        standardDeviation = histogram.equalize(standardDeviation);
        mean = histogram.equalize(mean);
    }

    // Normalize and equalize
    for(int i=0; i < numValues; i++){
        float v = inputData[i];
        if(_useHistogram){
            v = histogram.equalize(v);
        }
        v = normalizeWithStandardScore(v, mean, standardDeviation); 
        outputData[i] += v;
    }

    if(_useHistogram){
        float val = histogram.highestBinValue(_useHistogram);
        val = normalizeWithStandardScore(val, mean, standardDeviation);
        float width = normalizeWithStandardScore(1, mean, standardDeviation);
        _filterValues = glm::vec2( val, width);
    }

    // Histogram equalized = histogram.equalize();
    // histogram.print();
    // equalized.print();
}

float DataProcessor::processDataPoint(float value, int option){
    if(_numValues.empty()) return 0.0f;
    std::shared_ptr<Histogram> histogram = _histograms[option];
    float mean = (1.0 / _numValues[option]) * _sum[option];
    float sd = _sd[option];

    if(_useHistogram){
        // std::cout << sd << " " << 
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


glm::vec2 DataProcessor::filterValues(){
    return _filterValues;
}

void DataProcessor::clear(){
    _min.clear();
    _max.clear();
    _sum.clear();
    _sd.clear();
    _histograms.clear();
    _numValues.clear();
}

}