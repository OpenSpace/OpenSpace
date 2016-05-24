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

#include <modules/iswa/rendering/kameleonplane.h>
#include <ghoul/filesystem/filesystem>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <modules/kameleon/include/kameleonwrapper.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <fstream>
#include <modules/iswa/ext/json/json.hpp>
#include <modules/iswa/rendering/iswagroup.h>


namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "KameleonPlane";
}

namespace openspace {

KameleonPlane::KameleonPlane(const ghoul::Dictionary& dictionary)
    :CygnetPlane(dictionary)
    ,_useLog("useLog","Use Logarithm", false)
    ,_useHistogram("useHistogram", "Use Histogram", false)
    ,_autoFilter("autoFilter", "Auto Filter", true)
    ,_normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    ,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/hot.tf")
    ,_dataOptions("dataOptions", "Data Options")
    ,_fieldlines("fieldlineSeedsIndexFile", "Fieldline Seedpoints")
    ,_resolution("resolution", "Resolutionx100", 1, 1, 5)
    ,_slice("slice", "Slice", 0.0, 0.0, 1.0)
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
    addProperty(_resolution);
    addProperty(_slice);
    addProperty(_transferFunctionsFile);
    addProperty(_dataOptions);
    addProperty(_fieldlines);


    _type = IswaManager::CygnetType::Data;

    dictionary.getValue("kwPath", _kwPath);
    
    std::string fieldlineIndexFile;
    dictionary.getValue("fieldlineSeedsIndexFile", fieldlineIndexFile);
    readFieldlinePaths(absPath(fieldlineIndexFile));

    _fieldlines.onChange([this](){ updateFieldlineSeeds();} );

    std::string axis;
    dictionary.getValue("axisCut", axis);

    OsEng.gui()._iswa.registerProperty(&_fieldlines);
    OsEng.gui()._iswa.registerProperty(&_slice);

    if(axis == "x"){
        _scale = _data->scale.x;
        _data->scale.x = 0;
        _data->offset.x = 0;

        _slice.setValue(0.8);
    }else if(axis == "y"){
        _scale = _data->scale.y;
        _data->scale.y = 0;
        // _data->offset.y = 0;

        _slice.setValue((_data->offset.y -_data->gridMin.y)/_scale);
    }else{
        _scale = _data->scale.z;
        _data->scale.z = 0;
        // _data->offset.z = 0;

        _slice.setValue((_data->offset.z - _data->gridMin.z)/_scale);
    }
}

KameleonPlane::~KameleonPlane(){
    _kw = nullptr;
}

bool KameleonPlane::initialize(){
    _kw = std::make_shared<KameleonWrapper>(absPath(_kwPath));
    // IswaCygnet::initialize();
    _textures.push_back(nullptr);
    

    if(!_data->groupName.empty()){
        _groupEvent = IswaManager::ref().groupEvent(_data->groupName, _type);
        std::cout << "Register groupEvent: " << (_groupEvent != nullptr) << std::endl;
        _group = IswaManager::ref().registerToGroup(_data->groupName, _type);
        std::cout << "Register group " << (_group != nullptr) << std::endl;
    }
    
    initializeTime();
    createGeometry();
    createShader();

    if(_group){
        _dataProcessor = _group->dataProcessor();
    }else{
        OsEng.gui()._iswa.registerProperty(&_useLog);
        OsEng.gui()._iswa.registerProperty(&_useHistogram);
        OsEng.gui()._iswa.registerProperty(&_autoFilter);
        OsEng.gui()._iswa.registerProperty(&_normValues);
        OsEng.gui()._iswa.registerProperty(&_backgroundValues);
        OsEng.gui()._iswa.registerProperty(&_resolution);
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

    _transferFunctionsFile.onChange([this](){
        setTransferFunctions(_transferFunctionsFile.value());
    });

    _resolution.onChange([this](){
        for(int i=0; i<_textures.size(); i++){
            _textures[i] = std::move(nullptr);
        }
        _dataProcessor->clear();
        updateTexture();
    });

    _slice.onChange([this](){
        updateTexture();
    });

    _transferFunctionsFile.onChange([this](){
        LDEBUG(name() + " Event setTransferFunctionsFileChanged");
        setTransferFunctions(_transferFunctionsFile.value());
    });

    _groupEvent->subscribe(name(), "enabledChanged", [&](const ghoul::Dictionary& dict){
        LDEBUG(name() + " Event enabledChanged");
        _enabled.setValue(dict.value<bool>("enabled"));
    });


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

    _groupEvent->subscribe(name(), "clearGroup", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event clearGroup");
        OsEng.scriptEngine().queueScript("openspace.removeSceneGraphNode('" + name() + "')");
    });

    fillOptions();
    updateTexture();

	return true;
}

bool KameleonPlane::loadTexture() {
    std::vector<int> selectedOptions = _dataOptions.value();
    auto options = _dataOptions.options();
    
    for(int option : selectedOptions){
        if(!_dataSlices[option]){

            std::stringstream memorystream(options[option].description);
            std::string optionName;
            getline(memorystream, optionName, '/');
            getline(memorystream, optionName, '/');
            // std::cout << options[option].description << std::endl;
            _dataSlices[option] = _kw->getUniformSliceValues(optionName, _dimensions, _slice.value());
            if(!_textures[option])
                _dataProcessor->addValuesFromKameleonData(_dataSlices[option], _dimensions, options.size(), option);
        }
    }

    std::vector<float*> data = _dataProcessor->processKameleonData2(_dataSlices, _dimensions, _dataOptions);

    if(data.empty())
        return false;
    
    if(_autoFilter.value())
        _backgroundValues.setValue(_dataProcessor->filterValues());
    
    bool texturesReady = false;
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

bool KameleonPlane::updateTexture(){
    _dimensions = glm::size3_t(_resolution.value()*100);
    if(_data->scale.x == 0){
        _dimensions.x = 1;
        _dimensions.z = (int) _dimensions.y * (_data->scale.y/_data->scale.z);
        _textureDimensions = glm::size3_t(_dimensions.y, _dimensions.z, 1);

        _data->offset.x = _data->gridMin.x+_slice.value()*_scale;

    }else if(_data->scale.y == 0){
        _dimensions.y = 1;
        _dimensions.z = (int) _dimensions.x * (_data->scale.x/_data->scale.z);
        _textureDimensions = glm::size3_t(_dimensions.x, _dimensions.z, 1);

        _data->offset.y = _data->gridMin.y+_slice.value()*_scale;
    }else{
        _dimensions.z = 1;
        _dimensions.y = (int) _dimensions.x * (_data->scale.x/_data->scale.y); 
        _textureDimensions = glm::size3_t(_dimensions.x, _dimensions.y, 1);

        _data->offset.z = _data->gridMin.z+_slice.value()*_scale;
    }

    for(int i=0; i<_dataSlices.size(); ++i){
        float* slice = _dataSlices[i];
        if(slice){
            _dataSlices[i] = nullptr;
            delete slice;
        }
    }

    _textureDirty = true;

    return true;
}


bool KameleonPlane::readyToRender(){
    return (!_textures.empty() && !_transferFunctions.empty());
}

void KameleonPlane::setUniformAndTextures(){
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
    _shader->setUniform("transparency", _alpha.value());
}

bool KameleonPlane::createShader(){
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

void KameleonPlane::setTransferFunctions(std::string tfPath){
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

void KameleonPlane::fillOptions(){
    std::vector<std::string> options = _kw->getVariables();
    int numOptions = 0;

    for(std::string option : options){
        if(option.size() < 4 && option != "x" && option != "y" && option != "z"){
            _dataOptions.addOption({numOptions, name()+"/"+option});
            _dataSlices.push_back(nullptr);
            _textures.push_back(nullptr);
            numOptions++;
        }
    }
    _dataOptions.setValue(std::vector<int>(1,0));
    if(_group)
        IswaManager::ref().registerOptionsToGroup(_data->groupName, _dataOptions.options());
    _dataOptions.onChange([this](){loadTexture();});
}

void KameleonPlane::updateFieldlineSeeds(){
    std::vector<int> selectedOptions = _fieldlines.value();

    // SeedPath == map<int selectionValue, tuple< string name, string path, bool active > >
    for (auto& seedPath: _fieldlineState) {
        // if this option was turned off
        if( std::find(selectedOptions.begin(), selectedOptions.end(), seedPath.first)==selectedOptions.end() && std::get<2>(seedPath.second)){
            LDEBUG("Removed fieldlines: " + std::get<0>(seedPath.second));
            OsEng.scriptEngine().queueScript("openspace.removeSceneGraphNode('" + std::get<0>(seedPath.second) + "')");
            std::get<2>(seedPath.second) = false;
        // if this option was turned on
        } else if( std::find(selectedOptions.begin(), selectedOptions.end(), seedPath.first)!=selectedOptions.end() && !std::get<2>(seedPath.second)) {
            LDEBUG("Created fieldlines: " + std::get<0>(seedPath.second));
            IswaManager::ref().createFieldline(std::get<0>(seedPath.second), _kwPath, std::get<1>(seedPath.second));
            std::get<2>(seedPath.second) = true;
        }
    }
}

void KameleonPlane::readFieldlinePaths(std::string indexFile){
    LINFO("Reading seed points paths from file '" << indexFile << "'");

    // Read the index file from disk
    std::ifstream seedFile(indexFile);
    if (!seedFile.good())
        LERROR("Could not open seed points file '" << indexFile << "'");
    else {
        std::string line;
        std::string fileContent;
        while (std::getline(seedFile, line)) {
            fileContent += line;
        }

        try{
            //Parse and add each fieldline as an selection
            json fieldlines = json::parse(fileContent);
            int i = 0;
            for (json::iterator it = fieldlines.begin(); it != fieldlines.end(); ++it) {
                _fieldlines.addOption({i, it.key()});
                _fieldlineState[i] = std::make_tuple(it.key(), it.value(), false);
                i++;
            }
        } catch(const std::exception& e) {
            LERROR("Error when reading json file with paths to seedpoints: " + std::string(e.what()));
        }
    }
}

}// namespace openspace