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
#include <modules/iswa/rendering/iswagroup.h>

namespace {
    const std::string _loggerCat = "DataPlane";
    const int MAX_TEXTURES = 6;
}

namespace openspace {

DataPlane::DataPlane(const ghoul::Dictionary& dictionary)
    :CygnetPlane(dictionary)
    ,_useLog("useLog","Use Logarithm", false)
    ,_useHistogram("useHistogram", "Use Histogram", true)
    ,_autoFilter("autoFilter", "Auto Filter", true)
    ,_normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    ,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/hot.tf")
    ,_dataOptions("dataOptions", "Data Options")
    ,_dataProcessor(nullptr)
{     
    std::string name;
    dictionary.getValue("Name", name);
    setName(name);

    registerProperties();

    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_autoFilter);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_transferFunctionsFile);
    addProperty(_dataOptions);

    _type = IswaManager::CygnetType::Data;
}

DataPlane::~DataPlane(){}

bool DataPlane::initialize(){
    IswaCygnet::initialize();

    if(_group){
        _dataProcessor = _group->dataProcessor();

        _groupEvent->subscribe(name(), "useLogChanged", [&](const ghoul::Dictionary& dict){
            LDEBUG(name() + " Event useLogChanged");
            _useLog.setValue(dict.value<bool>("useLog"));
        });

        _groupEvent->subscribe(name(), "normValuesChanged", [&](ghoul::Dictionary dict){
            LDEBUG(name() + " Event normValuesChanged");
            std::shared_ptr<glm::vec2> values;
            bool success = dict.getValue("normValues", values);
            if(success){
                _normValues.setValue(*values);            
            }
        });

        _groupEvent->subscribe(name(), "useHistogramChanged", [&](ghoul::Dictionary dict){
            LDEBUG(name() + " Event useHistogramChanged");
            _useHistogram.setValue(dict.value<bool>("useHistogram"));
        });

        _groupEvent->subscribe(name(), "dataOptionsChanged", [&](ghoul::Dictionary dict){
            LDEBUG(name() + " Event dataOptionsChanged");
            std::shared_ptr<std::vector<int> > values;
            bool success = dict.getValue("dataOptions", values);
            if(success){
                _dataOptions.setValue(*values);            
            }
        });

        _groupEvent->subscribe(name(), "transferFunctionsChanged", [&](ghoul::Dictionary dict){
            LDEBUG(name() + " Event transferFunctionsChanged");
            _transferFunctionsFile.setValue(dict.value<std::string>("transferFunctions"));
        });

        _groupEvent->subscribe(name(), "backgroundValuesChanged", [&](ghoul::Dictionary dict){
            LDEBUG(name() + " Event backgroundValuesChanged");
            std::shared_ptr<glm::vec2> values;
            bool success = dict.getValue("backgroundValues", values);
            if(success){
                _backgroundValues.setValue(*values);            
            }
        });

        _groupEvent->subscribe(name(), "autoFilterChanged", [&](ghoul::Dictionary dict){
            LDEBUG(name() + " Event autoFilterChanged");
            _autoFilter.setValue(dict.value<bool>("autoFilter"));
        });

        _groupEvent->subscribe(name(), "updateGroup", [&](ghoul::Dictionary dict){
            LDEBUG(name() + " Event updateGroup");
            loadTexture();
        });

    }else{
        OsEng.gui()._iswa.registerProperty(&_useLog);
        OsEng.gui()._iswa.registerProperty(&_useHistogram);
        OsEng.gui()._iswa.registerProperty(&_autoFilter);
        OsEng.gui()._iswa.registerProperty(&_normValues);
        OsEng.gui()._iswa.registerProperty(&_backgroundValues);
        OsEng.gui()._iswa.registerProperty(&_transferFunctionsFile);
        OsEng.gui()._iswa.registerProperty(&_dataOptions);
        _dataProcessor = std::make_shared<DataProcessor>(
            _useLog.value(),
            _useHistogram.value(),
            _normValues
        );
    }

    setTransferFunctions(_transferFunctionsFile.value());

    _normValues.onChange([this](){
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

    _dataOptions.onChange([this](){ 
        if(_dataOptions.value().size() > MAX_TEXTURES)
            LWARNING("Too many options chosen, max is " + std::to_string(MAX_TEXTURES));
        loadTexture();
    });

    _transferFunctionsFile.onChange([this](){
        setTransferFunctions(_transferFunctionsFile.value());
    });

    return true;
}

bool DataPlane::loadTexture() {

    // if The future is done then get the new dataFile
    if(_futureObject.valid() && DownloadManager::futureReady(_futureObject)){
         DownloadManager::MemoryFile dataFile = _futureObject.get();

         if(dataFile.corrupted)
            return false;

        _dataBuffer = "";
        _dataBuffer.append(dataFile.buffer, dataFile.size);
        delete[] dataFile.buffer;
    }

    // if the buffer in the datafile is empty, do not proceed
    if(_dataBuffer.empty())
        return false;

    if(!_dataOptions.options().size()){ // load options for value selection
        fillOptions();
        _dataProcessor->addValues(_dataBuffer, _dataOptions);
        _group->updateGroup();
    }

    std::vector<float*> data = _dataProcessor->readData2(_dataBuffer, _dataOptions);

    if(data.empty())
        return false;

    if(_autoFilter.value())
        _backgroundValues.setValue(_dataProcessor->filterValues());
    
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
    int activeTextures = std::min((int)selectedOptions.size(), MAX_TEXTURES);
    int activeTransferfunctions = std::min((int)_transferFunctions.size(), MAX_TEXTURES);

    ghoul::opengl::TextureUnit txUnits[6];
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

    if(activeTextures > 0){
        if(selectedOptions.back()>=activeTransferfunctions)
            activeTransferfunctions = 1;
    }

    ghoul::opengl::TextureUnit tfUnits[6];
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
                if(j >= MAX_TEXTURES) break;
            }
        }
    }

    _shader->setUniform("numTextures", activeTextures);
    _shader->setUniform("numTransferFunctions", activeTransferfunctions);
    _shader->setUniform("backgroundValues", _backgroundValues.value());
    _shader->setUniform("transparency", _alpha.value());
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

        tfFile.close();
    }

    
    if(!tfs.empty()){
        _transferFunctions.clear();
        _transferFunctions = tfs;
    }
}

void DataPlane::fillOptions(){
    std::vector<std::string> options = _dataProcessor->readHeader(_dataBuffer);
    for(int i=0; i<options.size(); i++){
        _dataOptions.addOption({i, options[i]});
        _textures.push_back(nullptr);
    }
    _dataOptions.setValue(std::vector<int>(1,0));
    if(_group)
        _group->registerOptions(_dataOptions.options());
}

}// namespace openspace