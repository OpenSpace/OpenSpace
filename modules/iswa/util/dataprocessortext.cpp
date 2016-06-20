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
#include <modules/iswa/util/dataprocessortext.h>
//#include <algorithm>
//#include <boost/iostreams/device/mapped_file.hpp>
//
//#include <boost/config/warning_disable.hpp>
//#include <boost/spirit/include/qi.hpp>
//#include <boost/spirit/include/phoenix_core.hpp>
//#include <boost/spirit/include/phoenix_operator.hpp>
//#include <boost/spirit/include/phoenix_stl.hpp>

namespace {
    const std::string _loggerCat = "DataProcessorText";
}

namespace openspace{

DataProcessorText::DataProcessorText()
    :DataProcessor()
{}

DataProcessorText::~DataProcessorText(){}

std::vector<std::string> DataProcessorText::readMetadata(std::string data, glm::size3_t& dimensions){
    //The intresting part of the file looks like this:
    //# Output data: field with 61x61=3721 elements
    //# x           y           z           N           V_x         B_x

    std::vector<std::string> options = std::vector<std::string>();
    std::string info = "# Output data: field with "; //The string where the interesting data begins
    if(!data.empty()){
        std::string line;
        std::stringstream memorystream(data);

        while(getline(memorystream, line)){
            if(line.find(info) == 0){
                line = line.substr(info.size());
                std::stringstream ss(line);

                std::string token;
                getline(ss, token, 'x');
                int x = std::stoi(token);

                getline(ss, token, '=');
                int y = std::stoi(token);

                dimensions = glm::size3_t(x, y, 1);

                getline(memorystream, line);
                line = line.substr(1); //because of the # char

                ss = std::stringstream(line);
                std::string option;
                while(ss >> option){
                    if(_coordinateVariables.find(option) == _coordinateVariables.end())
                        options.push_back(option);
                }
            }
        }
    }

    return options;
}

void DataProcessorText::addDataValues(std::string data, properties::SelectionProperty& dataOptions){
    int numOptions = dataOptions.options().size(); 
    initializeVectors(numOptions);

    if(!data.empty()){
        std::string line;
        std::stringstream memorystream(data);

        std::vector<float> sum(numOptions, 0.0f); //for standard diviation in the add() function
        std::vector<std::vector<float>> optionValues(numOptions, std::vector<float>());

        std::vector<float> values;
        float value;

        int first, last, option, lineSize;


        // for each data point
        while(getline(memorystream, line)){
            if(line.find("#") == 0) continue;
            values = std::vector<float>();
            std::istringstream ss(line);
            std::string val;
            int skip = 0;
            //for each data option (variable)
            while(ss >> val){
                // first three values are coordinates
                if( skip < 3 ){
                    skip++;
                    continue;
                }

                float v = std::stof(val);
                // Some values are "NaN", use 0 instead
                if(!std::isnan(v)){
                    values.push_back(v);
                } else {
                    values.push_back(0.0f);
                }
                val.clear();
            }

            if(values.size() <= 0) continue;

            for(int i=0; i<numOptions; i++){
                value = values[i];

                optionValues[i].push_back(value);
                _min[i] = std::min(_min[i], value);
                _max[i] = std::max(_max[i], value);
                sum[i] += value;
            }
        }

        add(optionValues, sum);
    }
}

std::vector<float*> DataProcessorText::processData(std::string data, properties::SelectionProperty& dataOptions, glm::size3_t& dimensions){
    if(!data.empty()){

        std::string line;
        std::stringstream memorystream(data);

        std::vector<int> selectedOptions = dataOptions.value();
        int numSelected = selectedOptions.size();
        int numOptions  = dataOptions.options().size();

        std::vector<float> values;
        float value;

        int first, last, option, lineSize;
        
        std::vector<float*> dataOptions(numOptions, nullptr);
        for (int option : selectedOptions) {
            dataOptions[option] = new float[dimensions.x*dimensions.y]{0.0f};
        }

        int numValues = 0;
        while (getline(memorystream, line)) {
            if (line.find("#") == 0) continue;

            
            // ----------- OLD METHODS ------------------------
            // values = std::vector<float>();
            // std::stringstream ss(line);
            // float v;
            // while(ss >> v){
            //     values.push_back(v);
            // }

            // copy(
            //     std::istream_iterator<float> (ss),
            //     std::istream_iterator<float> (),
            //     back_inserter(values)
            // );

            // copy(
            //     std::next( std::istream_iterator<float> (ss), 3 ), //+3 because options x, y and z in the file
            //     std::istream_iterator<float> (),
            //     back_inserter(values)
            // );

            // for(int option : selectedOptions){
            //     value = values[option]; 
            //     //value = values[option+3]; //+3 because options x, y and z in the file
            //     dataOptions[option][numValues] = processDataPoint(value, option);
            // }
            // ----------- OLD METHODS ------------------------

            first = 0; 
            last = 0;
            option = -3;
            lineSize = line.size();

            while(last < lineSize){

                first = line.find_first_not_of(" \t", last);
                last =  line.find_first_of(" \t", first);
                last = (last > 0)? last : lineSize;
                
                if(option >= 0 && std::find(selectedOptions.begin(), selectedOptions.end(), option) != selectedOptions.end()){
                    // boost::spirit::qi::parse(&line[first], &line[last], boost::spirit::qi::float_, value);                
                    value = std::stof(line.substr(first, last));
                    dataOptions[option][numValues] = processDataPoint(value, option);
                }

                option++;
            }

            numValues++;
        }

        calculateFilterValues(selectedOptions);

        return dataOptions;
    }
    return std::vector<float*>();
}

}//namespace openspace