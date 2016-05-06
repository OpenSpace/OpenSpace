/*****************************************************************************************
*                                                                                       *
* OpenSpace                                                                             *
*                                                                                       *
* Copyright (c) 2014-2015                                                               *
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
#include <modules/iswa/rendering/iswacygnet.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/time.h>

namespace openspace{

ISWACygnet::ISWACygnet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _delete("delete", "Delete")
    , _shader(nullptr)
    // , _texture(nullptr)
    , _memorybuffer("")
    ,_type(ISWAManager::CygnetType::NoType)
    ,_futureObject(nullptr)
    // ,_transferFunction(nullptr)
{
    _data = std::make_shared<Metadata>();

    // dict.getValue can only set strings in _data directly
    float renderableId;
    float updateTime;
    glm::vec3 min, max;
    glm::vec4 spatialScale;

    dictionary.getValue("Id", renderableId);
    dictionary.getValue("UpdateTime", updateTime);
    dictionary.getValue("SpatialScale", spatialScale);
    dictionary.getValue("GridMin", min);
    dictionary.getValue("GridMax", max);
    dictionary.getValue("Frame",_data->frame);
    dictionary.getValue("CoordinateType", _data->coordinateType);
    
    _data->id = (int) renderableId;
    _data->updateTime = (int) updateTime;
    _data->spatialScale = spatialScale;
    _data->gridMin = min;
    _data->gridMax = max;


    glm::vec3 scale;
    glm::vec3 offset;

    scale = glm::vec3(
        (max.x - min.x),
        (max.y - min.y),
        (max.z - min.z)
    );

    offset = glm::vec3(
        (min.x + (std::abs(min.x)+std::abs(max.x))/2.0f),
        (min.y + (std::abs(min.y)+std::abs(max.y))/2.0f),
        (min.z + (std::abs(min.z)+std::abs(max.z))/2.0f)
    );

    _data->scale = scale;
    _data->offset = offset;

    addProperty(_delete);

    float groupId = -1;
    // if(dictionary.hasValue<float>("Group")){
        dictionary.getValue("Group", groupId);
    // }
    _data->groupId = groupId;
    // std::cout << _data->id << std::endl;
    // std::cout << _data->frame << std::endl;
    // std::cout << std::to_string(_data->offset) << std::endl;
    // std::cout << std::to_string(_data->scale) << std::endl;
    // std::cout << std::to_string(_data->max) << std::endl;
    // std::cout << std::to_string(_data->min) << std::endl;
    // std::cout << std::to_string(_data->spatialScale) << std::endl;

    _delete.onChange([this](){ISWAManager::ref().deleteISWACygnet(name());});

    // _textures.push_back(nullptr);
    // _transferFunctions.push_back(nullptr);
}

ISWACygnet::~ISWACygnet(){}
bool ISWACygnet::initialize(){
    _textures.push_back(nullptr);
    
    initializeTime();
    createGeometry();
    createShader();
    updateTexture();

    if(_data->groupId > 0)
        ISWAManager::ref().registerToGroup(_data->groupId, _type, this);

    // return isReady();
}

bool ISWACygnet::deinitialize(){
    if(_data->groupId > 0)
        ISWAManager::ref().unregisterFromGroup(_data->groupId, this);

    unregisterProperties();
    destroyGeometry();
    destroyShader();
    _memorybuffer = "";

    return true;
}

bool ISWACygnet::isReady() const{
    bool ready = true;
    if (!_shader)
        ready &= false;
    return ready;
}

void ISWACygnet::render(const RenderData& data){
    if(!readyToRender()) return;
    
    psc position = data.position;
    glm::mat4 transform = glm::mat4(1.0);

    glm::mat4 rot = glm::mat4(1.0);
    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 3; j++){
            transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }

    position += transform*glm::vec4(_data->spatialScale.x*_data->offset, _data->spatialScale.w);
    
    // Activate shader
    _shader->activate();
    glEnable(GL_ALPHA_TEST);
    glDisable(GL_CULL_FACE);


    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", transform);

    setPscUniforms(*_shader.get(), data.camera, position);

    setUniformAndTextures();
    renderGeometry();

    glEnable(GL_CULL_FACE);
    _shader->deactivate();
}

void ISWACygnet::update(const UpdateData& data){
    _openSpaceTime = Time::ref().currentTime();
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());


     _stateMatrix = ISWAManager::ref().getTransform(_data->frame, "GALACTIC", _openSpaceTime);
    // glm::dmat3 spiceMatrix    = SpiceManager::ref().positionTransformMatrix("J2000", "GALACTIC", _openSpaceTime);
     // = spiceMatrix*kameleonMatrix;

    bool timeToUpdate = (fabs(_openSpaceTime-_lastUpdateOpenSpaceTime) >= _data->updateTime &&
                        (_realTime.count()-_lastUpdateRealTime.count()) > _minRealTimeUpdateInterval);
    if( _data->updateTime != 0 && (Time::ref().timeJumped() || timeToUpdate )){
        updateTexture();

        _lastUpdateRealTime = _realTime;
        _lastUpdateOpenSpaceTime = _openSpaceTime;
    }

    if(_futureObject && _futureObject->isFinished){
        if(loadTexture())
            _futureObject = nullptr;
    }

    if(!_transferFunctions.empty())
        for(auto tf : _transferFunctions)
            tf->update();
}


bool ISWACygnet::destroyShader(){
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }
}

void ISWACygnet::registerProperties(){
    OsEng.gui()._iSWAproperty.registerProperty(&_enabled);
    OsEng.gui()._iSWAproperty.registerProperty(&_delete);
}

void ISWACygnet::unregisterProperties(){
    OsEng.gui()._iSWAproperty.unregisterProperties(name());
}

void ISWACygnet::initializeTime(){
    _openSpaceTime = Time::ref().currentTime();
    _lastUpdateOpenSpaceTime = _openSpaceTime;

    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;

    _minRealTimeUpdateInterval = 100;
}

}//namespace openspac