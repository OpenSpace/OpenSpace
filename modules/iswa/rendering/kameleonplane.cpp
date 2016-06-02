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

#include <fstream>
#include <modules/iswa/rendering/kameleonplane.h>
#include <modules/iswa/util/dataprocessorkameleon.h>
#include <ghoul/filesystem/filesystem>
#include <modules/iswa/ext/json/json.hpp>
#include <openspace/engine/openspaceengine.h>
#include <openspace/scene/scene.h>

namespace {
    using json = nlohmann::json;
    const std::string _loggerCat = "KameleonPlane";
}

namespace openspace {

KameleonPlane::KameleonPlane(const ghoul::Dictionary& dictionary)
    :DataCygnet(dictionary)
    ,_useLog("useLog","Use Logarithm", false)
    ,_useHistogram("useHistogram", "Auto Contrast", false)
    ,_autoFilter("autoFilter", "Auto Filter", true)
    ,_normValues("normValues", "Normalize Values", glm::vec2(1.0,1.0), glm::vec2(0), glm::vec2(5.0))
    ,_backgroundValues("backgroundValues", "Background Values", glm::vec2(0.0), glm::vec2(0), glm::vec2(1.0))
    ,_transferFunctionsFile("transferfunctions", "Transfer Functions", "${SCENE}/iswa/tfs/hot.tf")
    ,_fieldlines("fieldlineSeedsIndexFile", "Fieldline Seedpoints")
    ,_resolution("resolution", "Resolutionx100", 1, 1, 5)
    ,_slice("slice", "Slice", 0.0, 0.0, 1.0)
{       

    addProperty(_useLog);
    addProperty(_useHistogram);
    addProperty(_autoFilter);
    addProperty(_normValues);
    addProperty(_backgroundValues);
    addProperty(_resolution);
    addProperty(_slice);
    addProperty(_transferFunctionsFile);
    addProperty(_fieldlines);

    dictionary.getValue("kwPath", _kwPath);
    
    std::string fieldlineIndexFile;
    dictionary.getValue("fieldlineSeedsIndexFile", _fieldlineIndexFile);

    std::string axis;
    dictionary.getValue("axisCut", axis);

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

        //If autofiler is on, background values property should be hidden
        _autoFilter.onChange([this](){
            // If autofiler is selected, use _dataProcessor to set backgroundValues 
            // and unregister backgroundvalues property.
            if(_autoFilter.value()){
                _backgroundValues.setValue(_dataProcessor->filterValues());
                OsEng.gui()._iswa.unregisterProperty(&_backgroundValues); 
            // else if autofilter is turned off, register backgroundValues 
            } else {
                OsEng.gui()._iswa.registerProperty(&_backgroundValues, &_autoFilter);            
            }
        });
    }
    
    readTransferFunctions(_transferFunctionsFile.value());

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
    });

    _transferFunctionsFile.onChange([this](){
        readTransferFunctions(_transferFunctionsFile.value());
    });

    _resolution.onChange([this](){
        for(int i=0; i<_textures.size(); i++){
            _textures[i] = std::move(nullptr);
        }
        _dataProcessor->clear();
        updateTextureResource();
    });

    _slice.onChange([this](){
        updateTextureResource();
    });
    
    _fieldlines.onChange([this](){ 
        updateFieldlineSeeds();
    });

    _dimensions = glm::size3_t(_resolution.value()*100);
    if(_data->scale.x == 0){
        _dimensions.x = 1;
        _dimensions.z = (int) _dimensions.y * (_data->scale.y/_data->scale.z);
        _textureDimensions = glm::size3_t(_dimensions.y, _dimensions.z, 1);

    }else if(_data->scale.y == 0){
        _dimensions.y = 1;
        _dimensions.z = (int) _dimensions.x * (_data->scale.x/_data->scale.z);
        _textureDimensions = glm::size3_t(_dimensions.x, _dimensions.z, 1);
    }else{
        _dimensions.z = 1;
        _dimensions.y = (int) _dimensions.x * (_data->scale.x/_data->scale.y); 
        _textureDimensions = glm::size3_t(_dimensions.x, _dimensions.y, 1);
    }

    fillOptions(_kwPath);
    // Has to be done after fillOptions
    _dataOptions.onChange([this](){
        if(_dataOptions.value().size() > MAX_TEXTURES)
            LWARNING("Too many options chosen, max is " + std::to_string(MAX_TEXTURES));
        updateTexture();
    });

    std::dynamic_pointer_cast<DataProcessorKameleon>(_dataProcessor)->dimensions(_dimensions);
    _dataProcessor->addDataValues(_kwPath, _dataOptions);
    updateTextureResource();

	return true;
}

bool KameleonPlane::createGeometry() {
    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    
    // ============================
    //         GEOMETRY (quad)
    // ============================
    // GLfloat x,y, z;
    float s = _data->spatialScale.x;
    const GLfloat x = s*_data->scale.x/2.0;
    const GLfloat y = s*_data->scale.y/2.0;
    const GLfloat z = s*_data->scale.z/2.0;
    const GLfloat w = _data->spatialScale.w;

    const GLfloat vertex_data[] = { // square of two triangles (sigh)
        //      x      y     z     w     s     t
        -x, -y,             -z,  w, 0, 1,
         x,  y,              z,  w, 1, 0,
        -x,  ((x>0)?y:-y),   z,  w, 0, 0,
        -x, -y,             -z,  w, 0, 1,
         x,  ((x>0)?-y:y),  -z,  w, 1, 1,
         x,  y,              z,  w, 1, 0,
    };

    glBindVertexArray(_quad); // bind array
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer); // bind buffer
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, reinterpret_cast<void*>(sizeof(GLfloat) * 4));

    return true;
}

bool KameleonPlane::destroyGeometry(){
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    return true;
}

void KameleonPlane::renderGeometry() const {
    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
}

std::vector<float*> KameleonPlane::textureData() {
    return std::dynamic_pointer_cast<DataProcessorKameleon>(_dataProcessor)->processData(_kwPath,  _dataOptions, _slice, _dimensions);
};

bool KameleonPlane::updateTextureResource(){

    if (_data->scale.x == 0){
        _data->offset.x = _data->gridMin.x+_slice.value()*_scale;
    } else if (_data->scale.y == 0){
        _data->offset.y = _data->gridMin.y+_slice.value()*_scale;
    } else {
        _data->offset.z = _data->gridMin.z+_slice.value()*_scale;
    }

    _textureDirty = true;

    return true;
}

void KameleonPlane::setUniforms(){
    setTextureUniforms();
    _shader->setUniform("backgroundValues", _backgroundValues.value());
    _shader->setUniform("transparency", _alpha.value());
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
        try{
            //Parse and add each fieldline as an selection
            json fieldlines = json::parse(seedFile);
            int i = 0;
            std::string fullName = name();
            std::string partName = fullName.substr(0,fullName.find_last_of("-"));
            for (json::iterator it = fieldlines.begin(); it != fieldlines.end(); ++it) {

                _fieldlines.addOption({i, name()+"/"+it.key()});
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
        updateTexture();
    });
}

}// namespace openspace