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

#include <modules/iswa/util/dataprocessorkameleon.h>
//#include <algorithm>
#include <ghoul/filesystem/filesystem.h>

namespace {
    const std::string _loggerCat = "DataProcessorKameleon";
}

namespace openspace {

DataProcessorKameleon::DataProcessorKameleon()
    : DataProcessor()
    , _kwPath("")
    , _kw(nullptr)
    , _initialized(false)
    , _slice(0.5)
{}

DataProcessorKameleon::~DataProcessorKameleon(){}

std::vector<std::string> DataProcessorKameleon::readMetadata(std::string path, glm::size3_t& dimensions){

    if(!path.empty()){
        if(path != _kwPath || !_kw){

            initializeKameleonWrapper(path);
        }

        std::vector<std::string> opts = _kw->getVariables();
        opts.erase( std::remove_if(
            opts.begin(), 
            opts.end(), 
            [this](std::string opt){ return (opt.size() > 3 || _coordinateVariables.find(opt) != _coordinateVariables.end());}
            ), 
        opts.end()
        );
        return opts;
    }

    return std::vector<std::string>();
}

void DataProcessorKameleon::addDataValues(std::string path, properties::SelectionProperty& dataOptions){
    int numOptions = dataOptions.options().size();
    initializeVectors(numOptions);

    if(!path.empty()){
        if(path != _kwPath || !_kw)
            initializeKameleonWrapper(path);

        std::vector<float> sum(numOptions, 0.0f);
        std::vector<std::vector<float>> optionValues(numOptions, std::vector<float>());
        auto options = dataOptions.options();
        
        int numValues = _dimensions.x*_dimensions.y*_dimensions.z;

        float* values;
        float value;

        for(int i=0; i<numOptions; i++){
            //0.5 to gather interesting values for the normalization/histograms.
            values = _kw->getUniformSliceValues(options[i].description, _dimensions, 0.5f);

            for(int j=0; j<numValues; j++){
                value = values[j];

                optionValues[i].push_back(value);
                _min[i] = std::min(_min[i], value);
                _max[i] = std::max(_max[i], value);
                sum[i] += value;
            }
        }

        add(optionValues, sum);
    }
}
std::vector<float*> DataProcessorKameleon::processData(std::string path, properties::SelectionProperty& dataOptions, glm::size3_t& dimensions, float slice){
    _slice = slice;
    // _dimensions = dimensions; 
    return processData(path, dataOptions, dimensions);
}

std::vector<float*> DataProcessorKameleon::processData(std::string path, properties::SelectionProperty& dataOptions,  glm::size3_t& dimensions){
    int numOptions =  dataOptions.options().size();
    
    if(!path.empty()){
        if(path != _kwPath || !_kw)
            initializeKameleonWrapper(path);

        std::vector<int> selectedOptions = dataOptions.value();
//        int numSelected = selectedOptions.size();

        auto options = dataOptions.options();
        int numOptions = options.size();

        int numValues = dimensions.x*dimensions.y*dimensions.z;

        float value;

        std::vector<float*> dataOptions(numOptions, nullptr);
        for(int option : selectedOptions){
            dataOptions[option] = _kw->getUniformSliceValues(options[option].description, dimensions, _slice);

            for(int i=0; i<numValues; i++){
                value = dataOptions[option][i];
                dataOptions[option][i] = processDataPoint(value, option);
            }
        }

        calculateFilterValues(selectedOptions);
        return dataOptions;
    }
    return std::vector<float*>(numOptions, nullptr);
}

void DataProcessorKameleon::initializeKameleonWrapper(std::string path){
    const std::string& extension = ghoul::filesystem::File(absPath(path)).fileExtension();
    if(FileSys.fileExists(absPath(path)) && extension == "cdf"){
        if(_kw) _kw->close();

        _kwPath = path;
        _kw = std::make_shared<KameleonWrapper>(absPath(_kwPath));
    }
}

} //namespace openspace
