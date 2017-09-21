/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/iswa/util/dataprocessorjson.h>

#include <algorithm>
#include <iterator>
#include <modules/iswa/ext/json.h>

namespace {
    using json = nlohmann::json;
} // namespace

namespace openspace {

DataProcessorJson::DataProcessorJson()
    : DataProcessor()
{}

DataProcessorJson::~DataProcessorJson() {}

std::vector<std::string> DataProcessorJson::readMetadata(std::string data, glm::size3_t& dimensions){
    std::vector<std::string> options = std::vector<std::string>();
    if(!data.empty()){
        json j = json::parse(data);
        json variables = j["variables"];

        for(json::iterator it = variables.begin(); it != variables.end(); ++it){
            std::string option = it.key();
            if(option == "ep"){
                json row = it.value();
                json col = row.at(0);

                dimensions = glm::size3_t(col.size(), row.size(), 1);
            }

            if(_coordinateVariables.find(option) == _coordinateVariables.end()){
                options.push_back(option);
            }
        }
    }
    return options;
}

void DataProcessorJson::addDataValues(std::string data, properties::SelectionProperty& dataOptions){
    int numOptions = dataOptions.options().size();
    initializeVectors(numOptions);

    if(!data.empty()){
        json j = json::parse(data);
        json variables = j["variables"];

        std::vector<float> sum(numOptions, 0.0f);
        std::vector<std::vector<float>> optionValues(numOptions, std::vector<float>());
        auto options = dataOptions.options();

        float value;

        for(int i=0; i<numOptions; i++){
            json row = variables[options[i].description];
//            int rowsize = row.size();

            for(int y=0; y<row.size(); y++){
                json col = row.at(y);
                int colsize = col.size();

                for(int x=0; x<colsize; x++){
                    value = col.at(x);

                    optionValues[i].push_back(value);
                    _min[i] = std::min(_min[i], value);
                    _max[i] = std::max(_max[i], value);
                    sum[i] += value;
                }
            }
        }

        add(optionValues, sum);
    }
}

std::vector<float*> DataProcessorJson::processData(std::string data, properties::SelectionProperty& dataOptions,  glm::size3_t& dimensions){
    if(!data.empty()){
        json j = json::parse(data);
        json variables = j["variables"]; 

        std::vector<int> selectedOptions = dataOptions.value();
//        int numSelected = selectedOptions.size();
        
        auto options = dataOptions.options();
        int numOptions = options.size();

        float value;
        int rowsize, colsize, i;

        std::vector<float*> dataOptions(numOptions, nullptr);
        for(int option : selectedOptions){
            dataOptions[option] = new float[dimensions.x*dimensions.y]{0.0f};

            json row = variables[options[option].description];
            rowsize = row.size();

            for(int y=0; y<rowsize; y++){
                json col = row.at(y);
                colsize = col.size();

                for(int x=0; x<colsize; x++){
                    value = col.at(x);
                    i = x+y*colsize;

                    dataOptions[option][i] = processDataPoint(value, option);
                }
            }
        }

        calculateFilterValues(selectedOptions);
        return dataOptions;
    }
    return std::vector<float*>();
}

} //namespace openspace
