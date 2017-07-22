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

#include <modules/iswa/rendering/iswacygnet.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/time.h>
#include <openspace/util/transformationmanager.h>
#include <modules/iswa/rendering/iswabasegroup.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/opengl/programobject.h>

namespace {
    const char* _loggerCat = "IswaCygnet";
} // namespace

namespace openspace {

IswaCygnet::IswaCygnet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _delete({ "Delete", "Delete", "" }) // @TODO Missing documentation
    , _alpha({ "Alpha", "Alpha", "" }, 0.9f, 0.0f, 1.0f) // @TODO Missing documentation
    , _shader(nullptr)
    , _group(nullptr)
    , _textureDirty(false)
    , _rotation(glm::mat4(1.0f))
{
    std::string name;
    dictionary.getValue("Name", name);
    setName(name);

    _data = std::make_shared<Metadata>();

    // dict.getValue can only set strings in _data directly
    float renderableId;
    float updateTime;
    float xOffset;
    glm::vec3 min, max;
    glm::vec4 spatialScale;

    dictionary.getValue("Id", renderableId);
    dictionary.getValue("UpdateTime", updateTime);
    dictionary.getValue("SpatialScale", spatialScale);
    dictionary.getValue("GridMin", min);
    dictionary.getValue("GridMax", max);
    dictionary.getValue("Frame",_data->frame);
    dictionary.getValue("CoordinateType", _data->coordinateType);
    dictionary.getValue("XOffset", xOffset);
    
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
        (min.x + (std::abs(min.x)+std::abs(max.x))/2.0f)+xOffset,
        (min.y + (std::abs(min.y)+std::abs(max.y))/2.0f),
        (min.z + (std::abs(min.z)+std::abs(max.z))/2.0f)
    );

    _data->scale = scale;
    _data->offset = offset;

    addProperty(_alpha);
    addProperty(_delete);

    dictionary.getValue("Group", _data->groupName);

}

IswaCygnet::~IswaCygnet(){}

bool IswaCygnet::initialize(){
    _textures.push_back(nullptr);
    
    if(!_data->groupName.empty()){
        initializeGroup();
    }else{
        _delete.onChange([this](){
            deinitialize();
            OsEng.scriptEngine().queueScript(
                "openspace.removeSceneGraphNode('" + name() + "')",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        });
    }
    
    initializeTime();
    createGeometry();
    createShader();
    downloadTextureResource();

    return true;
}

bool IswaCygnet::deinitialize(){
     if(!_data->groupName.empty())
        _group->groupEvent()->unsubscribe(name());

    unregisterProperties();
    destroyGeometry();
    destroyShader();

    return true;
}

bool IswaCygnet::isReady() const{
    bool ready = true;
    if (!_shader)
        ready &= false;
    return ready;
}

void IswaCygnet::render(const RenderData& data, RendererTasks&){
    if(!readyToRender()) return;
    
    psc position = data.position;
    glm::mat4 transform = glm::mat4(1.0);

    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 3; j++){
            transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }
    transform = transform*_rotation;

    position += transform*glm::vec4(_data->spatialScale.x*_data->offset, _data->spatialScale.w);
    
    // Activate shader
    _shader->activate();
    glEnable(GL_ALPHA_TEST);
    glDisable(GL_CULL_FACE);


    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", transform);

    setPscUniforms(*_shader.get(), data.camera, position);

    setUniforms();
    renderGeometry();

    glEnable(GL_CULL_FACE);
    _shader->deactivate();
}

void IswaCygnet::update(const UpdateData&) {
    if (!_enabled) {
        return;
    }

    // the texture resource is downloaded ahead of time, so we need to
    // now if we are going backwards or forwards
    double clockwiseSign = (OsEng.timeManager().time().deltaTime()>0) ? 1.0 : -1.0;
    _openSpaceTime = OsEng.timeManager().time().j2000Seconds();
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    _stateMatrix = TransformationManager::ref().frameTransformationMatrix(_data->frame, "GALACTIC", _openSpaceTime);

    bool timeToUpdate = (fabs(_openSpaceTime-_lastUpdateOpenSpaceTime) >= _data->updateTime &&
                        (_realTime.count()-_lastUpdateRealTime.count()) > _minRealTimeUpdateInterval);

    if (_futureObject.valid() && DownloadManager::futureReady(_futureObject)) {
        bool success = updateTextureResource();
        if (success) {
            _textureDirty = true;
        }
    }

    if (_textureDirty && _data->updateTime != 0 && timeToUpdate) {
        updateTexture();
        _textureDirty = false;

        downloadTextureResource(_openSpaceTime + clockwiseSign*_data->updateTime);
        _lastUpdateRealTime = _realTime;
        _lastUpdateOpenSpaceTime =_openSpaceTime;
    }

    if (!_transferFunctions.empty()) {
        for (const std::shared_ptr<TransferFunction>& tf : _transferFunctions) {
            tf->update();
        }
    }
}

bool IswaCygnet::destroyShader() {
    RenderEngine& renderEngine = OsEng.renderEngine();
    if (_shader) {
        renderEngine.removeRenderProgram(_shader);
        _shader = nullptr;
    }
    return true;
}

void IswaCygnet::registerProperties() {}

void IswaCygnet::unregisterProperties() {}

void IswaCygnet::initializeTime() {
    _openSpaceTime = OsEng.timeManager().time().j2000Seconds();
    _lastUpdateOpenSpaceTime = 0.0;

    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::system_clock::now().time_since_epoch());
    _lastUpdateRealTime = _realTime;

    _minRealTimeUpdateInterval = 100;
}

bool IswaCygnet::createShader() {
    if (_shader == nullptr) {
        RenderEngine& renderEngine = OsEng.renderEngine();
        _shader = renderEngine.buildRenderProgram(_programName,
            _vsPath,
            _fsPath
        );
        if (!_shader) {
            return false;
        }
    }
    return true;
}

void IswaCygnet::initializeGroup() {
    _group = IswaManager::ref().iswaGroup(_data->groupName);

    //Subscribe to enable and delete property
    auto groupEvent = _group->groupEvent();
    groupEvent->subscribe(name(), "enabledChanged", [&](const ghoul::Dictionary& dict){
        LDEBUG(name() + " Event enabledChanged");
        _enabled.setValue(dict.value<bool>("enabled"));
    });

    groupEvent->subscribe(name(), "alphaChanged", [&](const ghoul::Dictionary& dict){
        LDEBUG(name() + " Event alphaChanged");
        _alpha.setValue(dict.value<float>("alpha"));
    });

    groupEvent->subscribe(name(), "clearGroup", [&](ghoul::Dictionary dict){
        LDEBUG(name() + " Event clearGroup");
        OsEng.scriptEngine().queueScript(
            "openspace.removeSceneGraphNode('" + name() + "')",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    });
}

} //namespace openspace
