// /*****************************************************************************************
//  *                                                                                       *
//  * OpenSpace                                                                             *
//  *                                                                                       *
//  * Copyright (c) 2014-2016                                                               *
//  *                                                                                       *
//  * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
//  * software and associated documentation files (the "Software"), to deal in the Software *
//  * without restriction, including without limitation the rights to use, copy, modify,    *
//  * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
//  * permit persons to whom the Software is furnished to do so, subject to the following   *
//  * conditions:                                                                           *
//  *                                                                                       *
//  * The above copyright notice and this permission notice shall be included in all copies *
//  * or substantial portions of the Software.                                              *
//  *                                                                                       *
//  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
//  * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
//  * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
//  * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
//  * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
//  * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
//  ****************************************************************************************/
#include <modules/iswa/rendering/dataplane.h>

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

namespace {
    const std::string _loggerCat = "DataPlane";
}

namespace openspace {

DataPlane::DataPlane(const ghoul::Dictionary& dictionary)
    :CygnetPlane(dictionary)
    ,_useLog("useLog","Use Logarithm", false)
    ,_useHistogram("useHistogram", "Use Histogram", true)
    ,_normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    ,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/hot.tf")
    ,_dataOptions("dataOptions", "Data Options")
{     
    std::string name;
    dictionary.getValue("Name", name);
    setName(name);

    registerProperties();

    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_transferFunctionsFile);
    addProperty(_dataOptions);

    if(_data->groupName.empty()){
        OsEng.gui()._iswa.registerProperty(&_useLog);
        OsEng.gui()._iswa.registerProperty(&_useHistogram);
        OsEng.gui()._iswa.registerProperty(&_normValues);
        OsEng.gui()._iswa.registerProperty(&_backgroundValues);
        OsEng.gui()._iswa.registerProperty(&_transferFunctionsFile);
        OsEng.gui()._iswa.registerProperty(&_dataOptions);
    }

    setTransferFunctions(_transferFunctionsFile.value());

    _dataProcessor = std::make_shared<DataProcessor>(
        _useLog.value(),
        _useHistogram.value(),
        _normValues
    );

    _normValues.onChange([this](){
        // FOR TESTING (should be done on all onChange)
        // _avgBenchmarkTime = 0.0;
        // _numOfBenchmarks = 0;
        _dataProcessor->normValues(_normValues.value());
        loadTexture();
    });
    _useLog.onChange([this](){
        _dataProcessor->useLog(_useLog.value());
        loadTexture();
    });

    _useHistogram.onChange([this](){
        _dataProcessor->useHistogram(_useHistogram.value());
        loadTexture();
    });

    _dataOptions.onChange([this](){ loadTexture();} );

    _transferFunctionsFile.onChange([this](){
        setTransferFunctions(_transferFunctionsFile.value());
    });

    _type = IswaManager::CygnetType::Data;

}

DataPlane::~DataPlane(){}

void DataPlane::useLog(bool useLog){ _useLog.setValue(useLog); };
void DataPlane::normValues(glm::vec2 normValues){  _normValues.setValue(normValues); };
void DataPlane::useHistogram(bool useHistogram){ _useHistogram.setValue(useHistogram); };
void DataPlane::dataOptions(std::vector<int> options){ _dataOptions.setValue(options); };
void DataPlane::transferFunctionsFile(std::string tfPath){ _transferFunctionsFile.setValue(tfPath); };
void DataPlane::backgroundValues(glm::vec2 backgroundValues){ _backgroundValues.setValue(backgroundValues); };

bool DataPlane::loadTexture() {
    
    // if The future is done then get the new dataFile
    if(_futureObject.valid() && DownloadManager::futureReady(_futureObject)){
         DownloadManager::MemoryFile dataFile = _futureObject.get();

         if(dataFile.corrupted)
            return false;

        _dataBuffer = "";
        _dataBuffer.append(dataFile.buffer, dataFile.size);
    }

    // if the buffer in the datafile is empty, do not proceed
    if(_dataBuffer.empty())
        return false;

    if(!_dataOptions.options().size()){ // load options for value selection
        std::vector<std::string> options = _dataProcessor->readHeader(_dataBuffer);
        for(int i=0; i<options.size(); i++){
            _dataOptions.addOption({i, name()+"/"+options[i]});
            _textures.push_back(nullptr);
        }
        _dataOptions.setValue(std::vector<int>(1,0));
        if(!_data->groupName.empty())
            IswaManager::ref().registerOptionsToGroup(_data->groupName, _dataOptions.options());
    }

    std::vector<float*> data = _dataProcessor->readData(_dataBuffer, _dataOptions);

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
                                                                    _dataProcessor->dimensions(),
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

bool DataPlane::updateTexture(){
    if(_futureObject.valid())
        return false;

    std::future<DownloadManager::MemoryFile> future = IswaManager::ref().fetchDataCygnet(_data->id);

    if(future.valid()){
        _futureObject = std::move(future);
        return true;
    }

    return false;
}

bool DataPlane::readyToRender(){
    return (!_textures.empty());
}

void DataPlane::setUniformAndTextures(){    
    std::vector<int> selectedOptions = _dataOptions.value();
    int activeTextures = selectedOptions.size();
    int activeTransferfunctions = _transferFunctions.size();

    ghoul::opengl::TextureUnit txUnits[10];
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
        }
    }

    if(activeTextures > 0){
        if(selectedOptions.back()>=activeTransferfunctions)
            activeTransferfunctions = 1;
    }

    ghoul::opengl::TextureUnit tfUnits[10];
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
            // std::cout << option << std::endl;
            // if(option >= activeTransferfunctions){
            //     // LWARNING("No transfer function for this value.");
            //     break;
            // }

            if(_transferFunctions[option]){
                tfUnits[j].activate();
                _transferFunctions[option]->bind();
                _shader->setUniform(
                "transferFunctions[" + std::to_string(j) + "]",
                tfUnits[j]
                );

                j++;
            }
        }
    }

    _shader->setUniform("numTextures", activeTextures);
    _shader->setUniform("numTransferFunctions", activeTransferfunctions);
    _shader->setUniform("backgroundValues", _backgroundValues.value());
}

bool DataPlane::createShader(){
    if (_shader == nullptr) {
        // DatePlane Program
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram("DataPlaneProgram",
            "${MODULE_ISWA}/shaders/dataplane_vs.glsl",
            "${MODULE_ISWA}/shaders/dataplane_fs.glsl"
            );
        if (!_shader) return false;
    }
}

void DataPlane::setTransferFunctions(std::string tfPath){
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
    }

    if(!tfs.empty()){
        _transferFunctions.clear();
        _transferFunctions = tfs;
    }

}

}// namespace openspace