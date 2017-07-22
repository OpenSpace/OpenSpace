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

#include <modules/iswa/rendering/datacygnet.h>

#include <fstream>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>
#include <modules/iswa/util/dataprocessortext.h>

#include <ghoul/opengl/programobject.h>

namespace {
    const char* _loggerCat = "DataCygnet";
} // namespace

namespace openspace {

DataCygnet::DataCygnet(const ghoul::Dictionary& dictionary)
    : IswaCygnet(dictionary)
    , _dataProcessor(nullptr)
    , _dataOptions({ "DataOptions", "Data Options", "" }) // @TODO Missing documentation
    , _useLog({ "UseLog","Use Logarithm", "" }, false) // @TODO Missing documentation
    , _useHistogram({ "UseHistogram", "Auto Contrast", "" }, false) // @TODO Missing documentation
    , _autoFilter({ "AutoFilter", "Auto Filter", "" }, true) // @TODO Missing documentation
    , _normValues({ "NormValues", "Normalize Values", "" }, glm::vec2(1.0, 1.0), glm::vec2(0), glm::vec2(5.0)) // @TODO Missing documentation
    , _backgroundValues({ "BackgroundValues", "Background Values", "" }, glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0)) // @TODO Missing documentation
    , _transferFunctionsFile({ "Transferfunctions", "Transfer Functions", "" }, "${SCENE}/iswa/tfs/default.tf") // @TODO Missing documentation
    //FOR TESTING
    , _numOfBenchmarks(0)
    , _avgBenchmarkTime(0.0f)
{
    addProperty(_dataOptions);
    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_autoFilter);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_transferFunctionsFile);
    registerProperties();
}

DataCygnet::~DataCygnet(){}

bool DataCygnet::updateTexture(){
    std::vector<float*> data = textureData();

    if(data.empty())
        return false;
    
    bool texturesReady = false;
    std::vector<int> selectedOptions = _dataOptions.value();

    for(int option: selectedOptions){
        float* values = data[option];
        if(!values) continue;

        if(!_textures[option]){
            std::unique_ptr<ghoul::opengl::Texture> texture =  std::make_unique<ghoul::opengl::Texture>(
                                                                    values, 
                                                                    _textureDimensions,
                                                                    ghoul::opengl::Texture::Format::Red,
                                                                    GL_RED, 
                                                                    GL_FLOAT,
                                                                    ghoul::opengl::Texture::FilterMode::Linear,
                                                                    ghoul::opengl::Texture::WrappingMode::ClampToEdge
                                                                );

            if(texture){
                texture->uploadTexture();
                texture->setFilter(ghoul::opengl::Texture::FilterMode::Linear);
                _textures[option] = std::move(texture);
            }
        }else{
            _textures[option]->setPixelData(values);
            _textures[option]->uploadTexture();
        }
        texturesReady = true;
    }
    return texturesReady;
}

bool DataCygnet::downloadTextureResource(double timestamp){
    if(_futureObject.valid())
        return false;

    std::future<DownloadManager::MemoryFile> future = IswaManager::ref().fetchDataCygnet(_data->id, timestamp);

    if(future.valid()){
        _futureObject = std::move(future);
        return true;
    }

    return false;
}

bool DataCygnet::updateTextureResource(){
    DownloadManager::MemoryFile dataFile = _futureObject.get();

     if(dataFile.corrupted)
        return false;

    _dataBuffer = std::string(dataFile.buffer, dataFile.size);
    delete[] dataFile.buffer;

    return true;
}

bool DataCygnet::readyToRender() const{
    return (!_textures.empty() && !_transferFunctions.empty());
}

/**
 * Set both transfer function textures and data textures in same function so that they bind to 
 * the right texture units. If separate in to two functions a list of ghoul::TextureUnit
 * needs to be passed as an argument to both.
 */
void DataCygnet::setTextureUniforms(){
    std::vector<int> selectedOptions = _dataOptions.value();
    int activeTextures = std::min((int)selectedOptions.size(), MAX_TEXTURES);
    int activeTransferfunctions = std::min((int)_transferFunctions.size(), MAX_TEXTURES);    

    // Set Textures
    ghoul::opengl::TextureUnit txUnits[MAX_TEXTURES];
    int j = 0;
    for(int option : selectedOptions){
        if(_textures[option]){
            txUnits[j].activate();
            _textures[option]->bind();
            _shader->setUniform(
                "textures[" + std::to_string(j) + "]",
                txUnits[j]
            );

            j++;
            if(j >= MAX_TEXTURES) break;
        }
    }

    if(activeTextures > 0 && selectedOptions.back()>=(int)_transferFunctions.size())
            activeTransferfunctions = 1;

    ghoul::opengl::TextureUnit tfUnits[MAX_TEXTURES];
    j = 0;

    if((activeTransferfunctions == 1)){
        tfUnits[0].activate();
        _transferFunctions[0]->bind();
        _shader->setUniform(
            "transferFunctions[0]",
            tfUnits[0]
        );
    }else{
        for(int option : selectedOptions){

            if(_transferFunctions[option]){
                tfUnits[j].activate();
                _transferFunctions[option]->bind();
                _shader->setUniform(
                "transferFunctions[" + std::to_string(j) + "]",
                tfUnits[j]
                );

                j++;
                if(j >= MAX_TEXTURES) break;
            }
        }
    }

    _shader->setUniform("numTransferFunctions", activeTransferfunctions);
    _shader->setUniform("numTextures", activeTextures);
}


void DataCygnet::readTransferFunctions(std::string tfPath){
    std::string line;
    std::ifstream tfFile(absPath(tfPath));

    std::vector<std::shared_ptr<TransferFunction>> tfs;

    if(tfFile.is_open()){
        while(getline(tfFile, line)){
            std::shared_ptr<TransferFunction> tf = std::make_shared<TransferFunction>(absPath(line));
            if(tf){
                tfs.push_back(tf);
            }
        }

        tfFile.close();
    }
    
    if(!tfs.empty()){
        _transferFunctions.clear();
        _transferFunctions = tfs;
    }
}

void DataCygnet::fillOptions(std::string& source){
    std::vector<std::string> options = _dataProcessor->readMetadata(source, _textureDimensions);
     
    for(int i=0; i<options.size(); i++){
        _dataOptions.addOption({i, options[i]});
        _textures.push_back(nullptr);
    }

    if(_group){
        std::dynamic_pointer_cast<IswaDataGroup>(_group)->registerOptions(_dataOptions.options());
        _dataOptions.setValue(std::dynamic_pointer_cast<IswaDataGroup>(_group)->dataOptionsValue());
    } else {
        _dataOptions.setValue(std::vector<int>(1,0));
    }
}

void DataCygnet::setPropertyCallbacks(){
    _normValues.onChange([this](){
        _dataProcessor->normValues(_normValues.value());
        updateTexture();
    });
    
    _useLog.onChange([this](){
        _dataProcessor->useLog(_useLog.value());
        updateTexture();
    });

    _useHistogram.onChange([this](){
        _dataProcessor->useHistogram(_useHistogram.value());
        updateTexture();
        if(_autoFilter.value())
            _backgroundValues.setValue(_dataProcessor->filterValues());
    });

    _dataOptions.onChange([this](){ 
        if(_dataOptions.value().size() > MAX_TEXTURES)
            LWARNING("Too many options chosen, max is " + std::to_string(MAX_TEXTURES));
        updateTexture();
    });

    _transferFunctionsFile.onChange([this](){
        readTransferFunctions(_transferFunctionsFile.value());
    });
}

void DataCygnet::subscribeToGroup(){
    auto groupEvent = _group->groupEvent();

    groupEvent->subscribe(name(), "dataOptionsChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event dataOptionsChanged");
        std::vector<int> values;
        bool success = dict.getValue<std::vector<int> >("dataOptions", values);
        if(success){
            _dataOptions.setValue(values);            
        }
    });

    groupEvent->subscribe(name(), "normValuesChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event normValuesChanged");
        glm::vec2 values;
        bool success = dict.getValue("normValues", values);
        if(success){
            _normValues.setValue(values);            
        }
    });

    groupEvent->subscribe(name(), "backgroundValuesChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event backgroundValuesChanged");
        glm::vec2 values;
        bool success = dict.getValue("backgroundValues", values);
        if(success){
            _backgroundValues.setValue(values);            
        }
    });

    groupEvent->subscribe(name(), "transferFunctionsChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event transferFunctionsChanged");
        _transferFunctionsFile.setValue(dict.value<std::string>("transferFunctions"));
    });

    groupEvent->subscribe(name(), "useLogChanged", [&](const ghoul::Dictionary& dict){
        LDEBUG(name() + " Event useLogChanged");
        _useLog.setValue(dict.value<bool>("useLog"));
    });

    groupEvent->subscribe(name(), "useHistogramChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event useHistogramChanged");
        _useHistogram.setValue(dict.value<bool>("useHistogram"));
    });

    groupEvent->subscribe(name(), "autoFilterChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event autoFilterChanged");
        _autoFilter.setValue(dict.value<bool>("autoFilter"));
    });

    groupEvent->subscribe(name(), "updateGroup", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event updateGroup");
        if(_autoFilter.value())
            _backgroundValues.setValue(_dataProcessor->filterValues());
        updateTexture();
    });

}

} //namespace openspace
