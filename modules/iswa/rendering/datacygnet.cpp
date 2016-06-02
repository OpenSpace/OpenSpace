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

#include <modules/iswa/rendering/datacygnet.h>

#include <fstream>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/filesystem/filesystem.h>
#include <modules/iswa/util/dataprocessortext.h>

namespace {
    const std::string _loggerCat = "DataCygnet";
}

namespace openspace{

DataCygnet::DataCygnet(const ghoul::Dictionary& dictionary)
    :IswaCygnet(dictionary)
    ,_dataProcessor(nullptr)
    ,_dataOptions("dataOptions", "Data Options")
{
    addProperty(_dataOptions);
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

bool DataCygnet::downloadTextureResource(){
    if(_futureObject.valid())
        return false;

    std::future<DownloadManager::MemoryFile> future = IswaManager::ref().fetchDataCygnet(_data->id);

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

    _dataBuffer = "";
    _dataBuffer.append(dataFile.buffer, dataFile.size);
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

    //Set Transfer Functions
    if(activeTextures > 0){
        if(selectedOptions.back()>=activeTransferfunctions)
            activeTransferfunctions = 1;
    }

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
    std::vector<std::string> options = _dataProcessor->readMetadata(source);
     
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


} //namespace openspace