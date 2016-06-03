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
#include <modules/iswa/util/dataprocessorkameleon.h>

namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "KameleonPlane";
    const int MAX_TEXTURES = 6;
}

namespace openspace {

KameleonPlane::KameleonPlane(const ghoul::Dictionary& dictionary)
    :CygnetPlane(dictionary)
    ,_useLog("useLog","Use Logarithm", false)
    ,_useHistogram("useHistogram", "Auto Contrast", false)
    ,_autoFilter("autoFilter", "Auto Filter", true)
    ,_normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    ,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/default.tf")
    ,_dataOptions("dataOptions", "Data Options")
    ,_fieldlines("fieldlineSeedsIndexFile", "Fieldline Seedpoints")
    ,_resolution("resolution", "Resolution%", 100.0f, 10.0f, 200.0f)
    ,_slice("slice", "Slice", 0.0, 0.0, 1.0)
{       

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

    dictionary.getValue("kwPath", _kwPath);
    
    std::string fieldlineIndexFile;
    dictionary.getValue("fieldlineSeedsIndexFile", _fieldlineIndexFile);

    std::string axis;
    dictionary.getValue("axisCut", axis);

    OsEng.gui()._iswa.registerProperty(&_slice);


    if(axis == "x")         _cut = 0;
    else if (axis == "y")   _cut = 1;
    else                    _cut = 2;

    _origOffset = _data->offset;

    _scale = _data->scale[_cut];
    _data->scale[_cut] = 0;
    _data->offset[_cut] = 0;

    _slice.setValue((_data->offset[_cut] -_data->gridMin[_cut])/_scale);

    setDimensions();

    _programName = "DataPlaneProgram";
    _vsPath = "${MODULE_ISWA}/shaders/dataplane_vs.glsl";
    _fsPath = "${MODULE_ISWA}/shaders/dataplane_fs.glsl";
}

KameleonPlane::~KameleonPlane(){}

bool KameleonPlane::deinitialize(){
    IswaCygnet::deinitialize();
    _fieldlines.set(std::vector<int>());
    return true;
}

bool KameleonPlane::initialize(){
    if(!_data->groupName.empty()){
        initializeGroup();
    }
    
    initializeTime();
    createGeometry();
    createShader();

    readFieldlinePaths(absPath(_fieldlineIndexFile));

    if(_group){
        _dataProcessor = _group->dataProcessor();
        subscribeToGroup();
    }else{
        OsEng.gui()._iswa.registerProperty(&_useLog);
        OsEng.gui()._iswa.registerProperty(&_useHistogram);
        OsEng.gui()._iswa.registerProperty(&_autoFilter);
        OsEng.gui()._iswa.registerProperty(&_normValues);
        OsEng.gui()._iswa.registerProperty(&_backgroundValues);
        OsEng.gui()._iswa.registerProperty(&_resolution);
        OsEng.gui()._iswa.registerProperty(&_transferFunctionsFile);
        OsEng.gui()._iswa.registerProperty(&_fieldlines);
        OsEng.gui()._iswa.registerProperty(&_dataOptions);
    
        _dataProcessor = std::make_shared<DataProcessorKameleon>();
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
        // _dataProcessor->clear();
        setDimensions();
        updateTexture();
    });

    _slice.onChange([this](){
        updateTexture();
    });
    
    _fieldlines.onChange([this](){ 
        updateFieldlineSeeds();
    });

    fillOptions();

    // Has to be done after fillOptions()
    _dataOptions.onChange([this](){
        if(_dataOptions.value().size() > MAX_TEXTURES)
            LWARNING("Too many options chosen, max is " + std::to_string(MAX_TEXTURES));
        loadTexture();
    });

    std::dynamic_pointer_cast<DataProcessorKameleon>(_dataProcessor)->dimensions(_dimensions);
    _dataProcessor->addDataValues(_kwPath, _dataOptions);

    updateTexture();

	return true;
}

bool KameleonPlane::loadTexture() {
    std::vector<float*> data = std::dynamic_pointer_cast<DataProcessorKameleon> (_dataProcessor)->processData(_kwPath,  _dataOptions, _slice, _dimensions);

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
    _data->offset[_cut] = _data->gridMin[_cut]+_slice.value()*_scale;
    _textureDirty = true;

    return true;
}


bool KameleonPlane::readyToRender() const {
    return (!_textures.empty() && !_transferFunctions.empty());
}

void KameleonPlane::setUniforms(){
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

    // if(activeTextures > MAX_TEXTURES){
    //     if(selectedOptions.back()>=(int)_transferFunctions.size())
    if(activeTextures > 0 && selectedOptions.back()>=(int)_transferFunctions.size())
            activeTransferfunctions = 1;

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
        tfFile.close();
    }

    if(!tfs.empty()){
        _transferFunctions.clear();
        _transferFunctions = tfs;
    }
}

void KameleonPlane::fillOptions(){
    std::vector<std::string> options = _dataProcessor->readMetadata(_kwPath);

    for(int i=0; i<options.size(); i++){
        _dataOptions.addOption({i, options[i]});
        _textures.push_back(nullptr);
    }
    if(_group){
        std::dynamic_pointer_cast<IswaKameleonGroup> (_group)->registerOptions(_dataOptions.options());
        _dataOptions.setValue(std::dynamic_pointer_cast<IswaKameleonGroup> (_group)->dataOptionsValue());
    } else {
        _dataOptions.setValue(std::vector<int>(1,0));
    }
}

void KameleonPlane::updateFieldlineSeeds(){
    std::vector<int> selectedOptions = _fieldlines.value();

    // SeedPath == map<int selectionValue, tuple< string name, string path, bool active > >
    for (auto& seedPath: _fieldlineState) {
        // if this option was turned off
        if( std::find(selectedOptions.begin(), selectedOptions.end(), seedPath.first)==selectedOptions.end() && std::get<2>(seedPath.second)){
            if(OsEng.renderEngine().scene()->sceneGraphNode(std::get<0>(seedPath.second)) == nullptr) return;
            
            LDEBUG("Removed fieldlines: " + std::get<0>(seedPath.second));
            OsEng.scriptEngine().queueScript("openspace.removeSceneGraphNode('" + std::get<0>(seedPath.second) + "')");
            std::get<2>(seedPath.second) = false;
        // if this option was turned on
        } else if( std::find(selectedOptions.begin(), selectedOptions.end(), seedPath.first)!=selectedOptions.end() && !std::get<2>(seedPath.second)) {
            if(OsEng.renderEngine().scene()->sceneGraphNode(std::get<0>(seedPath.second)) != nullptr) return;
            LDEBUG("Created fieldlines: " + std::get<0>(seedPath.second));
            IswaManager::ref().createFieldline(std::get<0>(seedPath.second), _kwPath, std::get<1>(seedPath.second));
            std::get<2>(seedPath.second) = true;
        }
    }
}

void KameleonPlane::readFieldlinePaths(std::string indexFile){
    LINFO("Reading seed points paths from file '" << indexFile << "'");
    if(_group){
        std::dynamic_pointer_cast<IswaKameleonGroup>(_group)->setFieldlineInfo(indexFile, _kwPath);
        return;
    }

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
            std::string fullName = name();
            std::string partName = fullName.substr(0,fullName.find_last_of("-"));
            std::cout << fullName << std::endl;
            std::cout << partName << std::endl;
            for (json::iterator it = fieldlines.begin(); it != fieldlines.end(); ++it) {


                _fieldlines.addOption({i, it.key()});
                _fieldlineState[i] = std::make_tuple(partName+"/"+it.key(), it.value(), false);
                i++;
            }

        } catch(const std::exception& e) {
            LERROR("Error when reading json file with paths to seedpoints: " + std::string(e.what()));
        }
    }
}

void KameleonPlane::subscribeToGroup(){
    auto groupEvent = _group->groupEvent();

    groupEvent->subscribe(name(), "useLogChanged", [&](const ghoul::Dictionary& dict){
        LDEBUG(name() + " Event useLogChanged");
        _useLog.setValue(dict.value<bool>("useLog"));
    });

    groupEvent->subscribe(name(), "normValuesChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event normValuesChanged");
        std::shared_ptr<glm::vec2> values;
        bool success = dict.getValue("normValues", values);
        if(success){
            _normValues.setValue(*values);            
        }
    });

    groupEvent->subscribe(name(), "useHistogramChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event useHistogramChanged");
        _useHistogram.setValue(dict.value<bool>("useHistogram"));
    });

    groupEvent->subscribe(name(), "dataOptionsChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event dataOptionsChanged");
        std::shared_ptr<std::vector<int> > values;
        bool success = dict.getValue("dataOptions", values);
        if(success){
            _dataOptions.setValue(*values);            
        }
    });

    groupEvent->subscribe(name(), "transferFunctionsChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event transferFunctionsChanged");
        _transferFunctionsFile.setValue(dict.value<std::string>("transferFunctions"));
    });

    groupEvent->subscribe(name(), "backgroundValuesChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event backgroundValuesChanged");
        std::shared_ptr<glm::vec2> values;
        bool success = dict.getValue("backgroundValues", values);
        if(success){
            _backgroundValues.setValue(*values);            
        }
    });

    groupEvent->subscribe(name(), "autoFilterChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event autoFilterChanged");
        _autoFilter.setValue(dict.value<bool>("autoFilter"));
    });

    groupEvent->subscribe(name(), "updateGroup", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event updateGroup");
        loadTexture();
    });

    groupEvent->subscribe(name(), "resolutionChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event resolutionChanged");
        float resolution;
        bool success = dict.getValue("resolution", resolution);
        if(success){
            _resolution.setValue(resolution);
        }
    });

    groupEvent->subscribe(name(), "cdfChanged", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event cdfChanged");
        std::string path;
        bool success = dict.getValue("path", path);
        if(success){
            changeKwPath(path);
        }

        loadTexture();
    });
}

void KameleonPlane::setDimensions(){
    // the cdf files has an offset of 0.5 in normali resolution.
    // with lower resolution the offset increases. 
    _data->offset = _origOffset - 0.5f*(100.0f/_resolution.value());
    _dimensions = glm::size3_t(_data->scale*((float)_resolution.value()/100.f));
    _dimensions[_cut] = 1;

    if(_cut == 0){
        _textureDimensions = glm::size3_t(_dimensions.y, _dimensions.z, 1);
    }else if(_cut == 1){
        _textureDimensions = glm::size3_t(_dimensions.x, _dimensions.z, 1);
    }else{
        _textureDimensions = glm::size3_t(_dimensions.x, _dimensions.y, 1);
    }
}

void KameleonPlane::changeKwPath(std::string kwPath){
    _kwPath = kwPath;
}

}// namespace openspace