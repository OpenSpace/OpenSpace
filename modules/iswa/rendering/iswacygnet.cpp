/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <modules/iswa/rendering/iswabasegroup.h>
#include <modules/iswa/util/iswamanager.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/powerscaledcoordinate.h>
#include <openspace/util/time.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/transformationmanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/designpattern/event.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>

namespace {
    constexpr const char* _loggerCat = "IswaCygnet";

    constexpr openspace::properties::Property::PropertyInfo DeleteInfo = {
        "Delete",
        "Delete",
        "" // @TODO Missing documentation
    };
    constexpr openspace::properties::Property::PropertyInfo AlphaInfo = {
        "Alpha",
        "Alpha",
        "" // @TODO Missing documentation
    };
} // namespace

namespace openspace {

IswaCygnet::IswaCygnet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _alpha(AlphaInfo, 0.9f, 0.f, 1.f)
    , _delete(DeleteInfo)
{
    // This changed from setIdentifier to setGuiName, 2018-03-14 ---abock
    std::string name;
    dictionary.getValue("Name", name);
    setGuiName(name);

    float renderableId;
    dictionary.getValue("Id", renderableId);
    float updateTime;
    dictionary.getValue("UpdateTime", updateTime);
    glm::vec4 spatialScale;
    dictionary.getValue("SpatialScale", spatialScale);
    glm::vec3 min;
    dictionary.getValue("GridMin", min);
    glm::vec3 max;
    dictionary.getValue("GridMax", max);
    dictionary.getValue("Frame",_data.frame);
    dictionary.getValue("CoordinateType", _data.coordinateType);
    float xOffset;
    dictionary.getValue("XOffset", xOffset);

    dictionary.getValue("Group", _data.groupName);

    _data.id = static_cast<int>(renderableId);
    _data.updateTime = static_cast<int>(updateTime);
    _data.spatialScale = spatialScale;
    _data.gridMin = min;
    _data.gridMax = max;

    glm::vec3 scale = glm::vec3((max.x - min.x), (max.y - min.y), (max.z - min.z));
    _data.scale = scale;

    glm::vec3 offset = glm::vec3(
        (min.x + (std::abs(min.x) + std::abs(max.x)) / 2.f) + xOffset,
        (min.y + (std::abs(min.y) + std::abs(max.y)) / 2.f),
        (min.z + (std::abs(min.z) + std::abs(max.z)) / 2.f)
    );
    _data.offset = offset;

    addProperty(_alpha);
    addProperty(_delete);
}

IswaCygnet::~IswaCygnet() {}

void IswaCygnet::initializeGL() {
    _textures.push_back(nullptr);

    if (!_data.groupName.empty()) {
        initializeGroup();
    } else {
        _delete.onChange([this]() {
            deinitialize();
            global::scriptEngine.queueScript(
                "openspace.removeSceneGraphNode('" + identifier() + "')",
                scripting::ScriptEngine::RemoteScripting::Yes
            );
        });
    }

    initializeTime();
    createGeometry();
    downloadTextureResource(global::timeManager.time().j2000Seconds());
}

void IswaCygnet::deinitializeGL() {
    if (!_data.groupName.empty()) {
        _group->groupEvent().unsubscribe(identifier());
    }

    unregisterProperties();
    destroyGeometry();

    if (_shader) {
        global::renderEngine.removeRenderProgram(_shader.get());
        _shader = nullptr;
    }
}

bool IswaCygnet::isReady() const {
    return !_shader;
}

void IswaCygnet::render(const RenderData& data, RendererTasks&) {
    if (!readyToRender()) {
        return;
    }

    glm::mat4 transform = glm::mat4(1.0);
    for (int i = 0; i < 3; i++){
        for (int j = 0; j < 3; j++){
            transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }
    transform = transform * _rotation;

    psc position = data.position + transform * glm::vec4(
        _data.spatialScale.x * _data.offset,
        _data.spatialScale.w
    );

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
    _openSpaceTime = global::timeManager.time().j2000Seconds();
    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch()
    );
    _stateMatrix = TransformationManager::ref().frameTransformationMatrix(
        _data.frame,
        "GALACTIC",
        _openSpaceTime
    );

    const bool timeToUpdate =
        (fabs(_openSpaceTime - _lastUpdateOpenSpaceTime) >= _data.updateTime &&
        (_realTime.count() - _lastUpdateRealTime.count()) > _minRealTimeUpdateInterval);

    if (_futureObject.valid() && DownloadManager::futureReady(_futureObject)) {
        const bool success = updateTextureResource();
        if (success) {
            _textureDirty = true;
        }
    }

    if (_textureDirty && _data.updateTime != 0 && timeToUpdate) {
        updateTexture();
        _textureDirty = false;

        double clockwiseSign = (global::timeManager.deltaTime() > 0) ? 1.0 : -1.0;
        downloadTextureResource(_openSpaceTime + clockwiseSign * _data.updateTime);
        _lastUpdateRealTime = _realTime;
        _lastUpdateOpenSpaceTime = _openSpaceTime;
    }

    if (!_transferFunctions.empty()) {
        for (TransferFunction& tf : _transferFunctions) {
            tf.update();
        }
    }
}

void IswaCygnet::enabled(bool enabled) {
    _enabled = enabled;
}

void IswaCygnet::registerProperties() {}

void IswaCygnet::unregisterProperties() {}

void IswaCygnet::initializeTime() {
    _openSpaceTime = global::timeManager.time().j2000Seconds();
    _lastUpdateOpenSpaceTime = 0.0;

    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch()
    );
    _lastUpdateRealTime = _realTime;

    _minRealTimeUpdateInterval = 100;
}

void IswaCygnet::initializeGroup() {
    _group = IswaManager::ref().iswaGroup(_data.groupName);

    //Subscribe to enable and delete property
    ghoul::Event<ghoul::Dictionary>& groupEvent = _group->groupEvent();

    groupEvent.subscribe(
        identifier(),
        "enabledChanged",
        [&](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event enabledChanged");
            _enabled = dict.value<bool>("enabled");
        }
    );

    groupEvent.subscribe(
        identifier(),
        "alphaChanged",
        [&](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event alphaChanged");
            _alpha = dict.value<float>("alpha");
        }
    );

    groupEvent.subscribe(identifier(), "clearGroup", [&](ghoul::Dictionary) {
        LDEBUG(identifier() + " Event clearGroup");
        global::scriptEngine.queueScript(
            "openspace.removeSceneGraphNode('" + identifier() + "')",
            scripting::ScriptEngine::RemoteScripting::Yes
        );
    });
}

} //namespace openspace
