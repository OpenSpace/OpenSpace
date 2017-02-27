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
    ,_fieldlines("fieldlineSeedsIndexFile", "Fieldline Seedpoints")
    ,_resolution("resolution", "Resolution%", 100.0f, 10.0f, 200.0f)
    ,_slice("slice", "Slice", 0.0, 0.0, 1.0)
{       

    addProperty(_resolution);
    addProperty(_slice);
    addProperty(_fieldlines);

    dictionary.getValue("kwPath", _kwPath);
    
    std::string fieldlineIndexFile;
    dictionary.getValue("fieldlineSeedsIndexFile", _fieldlineIndexFile);

    std::string axis;
    dictionary.getValue("axisCut", axis);

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
        _dataProcessor = std::make_shared<DataProcessorKameleon>();

        //If autofiler is on, background values property should be hidden
        _autoFilter.onChange([this](){
            // If autofiler is selected, use _dataProcessor to set backgroundValues 
            // and unregister backgroundvalues property.
            if(_autoFilter.value()){
                _backgroundValues.setValue(_dataProcessor->filterValues());
                _backgroundValues.setVisibility(properties::Property::Visibility::None);
                //_backgroundValues.setVisible(false);
            // else if autofilter is turned off, register backgroundValues 
            } else {
                _backgroundValues.setVisibility(properties::Property::Visibility::All);
                //_backgroundValues.setVisible(true);
            }
        });
    }
    
    fillOptions(_kwPath);

    readTransferFunctions(_transferFunctionsFile.value());

    // Set Property Callbacks of DataCygnet (must be called after fillOptions)
    setPropertyCallbacks();
    
    // Set Property callback specific to KameleonPlane
    _resolution.onChange([this](){
        for(int i=0; i<_textures.size(); i++){
            _textures[i] = std::move(nullptr);
        }

        updateTextureResource();
        setDimensions();

    });

    _slice.onChange([this](){
        updateTextureResource();
    });
    
    _fieldlines.onChange([this](){ 
        updateFieldlineSeeds();
    });

    std::dynamic_pointer_cast<DataProcessorKameleon>(_dataProcessor)->dimensions(_dimensions);
    _dataProcessor->addDataValues(_kwPath, _dataOptions);
    // if this datacygnet has added new values then reload texture
    // for the whole group, including this datacygnet, and return after.
    if(_group){
        _group->updateGroup();
    }
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
    return std::dynamic_pointer_cast<DataProcessorKameleon>(_dataProcessor)->processData(_kwPath,  _dataOptions, _dimensions, _slice);
};

bool KameleonPlane::updateTextureResource(){

    _data->offset[_cut] = _data->gridMin[_cut]+_slice.value()*_scale;
    // _textureDirty = true;
    updateTexture();
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
            OsEng.scriptEngine().queueScript(
                "openspace.removeSceneGraphNode('" + std::get<0>(seedPath.second) + "')",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
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
    // Subscribe to DataCygnet events
    DataCygnet::subscribeToGroup();

    //Add additional Events specific to KameleonPlane
    auto groupEvent = _group->groupEvent();
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
        updateTexture();
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