/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/iswa/rendering/renderableiswacygnet.h>

#include <modules/iswa/rendering/iswabasegroup.h>
#include <modules/iswa/util/iswamanager.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
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
    constexpr std::string_view _loggerCat = "IswaCygnet";

    constexpr openspace::properties::Property::PropertyInfo DeleteInfo = {
        "Delete",
        "Delete",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::Developer
    };
    constexpr openspace::properties::Property::PropertyInfo AlphaInfo = {
        "Alpha",
        "Alpha",
        "", // @TODO Missing documentation
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableIswaCygnet)]] Parameters {
        int id;
        int updateTime;
        std::optional<glm::vec4> spatialScale;
        std::optional<glm::vec3> gridMin;
        std::optional<glm::vec3> gridMax;
        std::optional<std::string> frame;
        std::optional<std::string> coordinateType;
        std::optional<std::string> group;

        std::optional<double> xOffset;
    };
#include "renderableiswacygnet_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableIswaCygnet::Documentation() {
    return codegen::doc<Parameters>("iswa_renderable_iswacygnet");
}

RenderableIswaCygnet::RenderableIswaCygnet(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _alpha(AlphaInfo, 0.9f, 0.f, 1.f)
    , _delete(DeleteInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::string name;
    if (dictionary.hasValue<std::string>("Name")) {
        name = dictionary.value<std::string>("Name");
    }
    setGuiName(name);

    _data.id = p.id;
    _data.updateTime = p.updateTime;
    _data.spatialScale = p.spatialScale.value_or(_data.spatialScale);
    _data.gridMin = p.gridMin.value_or(_data.gridMin);
    _data.gridMax = p.gridMax.value_or(_data.gridMax);
    _data.frame = p.frame.value_or(_data.frame);
    _data.coordinateType = p.frame.value_or(_data.coordinateType);
    _data.groupName = p.group.value_or(_data.groupName);

    double xOffset = p.xOffset.value_or(0.0);

    glm::vec3 scale = glm::vec3(
        _data.gridMax.x - _data.gridMin.x,
        _data.gridMax.y - _data.gridMin.y,
        _data.gridMax.z - _data.gridMin.z
    );
    _data.scale = scale;

    glm::vec3 offset = glm::vec3(
        (_data.gridMin.x +
            (std::abs(_data.gridMin.x) + std::abs(_data.gridMax.x)) / 2.f) + xOffset,
        (_data.gridMin.y +
            (std::abs(_data.gridMin.y) + std::abs(_data.gridMax.y)) / 2.f),
        (_data.gridMin.z +
            (std::abs(_data.gridMin.z) + std::abs(_data.gridMax.z)) / 2.f)
    );
    _data.offset = offset;

    addProperty(_alpha);
    addProperty(_delete);
}

RenderableIswaCygnet::~RenderableIswaCygnet() {}

void RenderableIswaCygnet::initializeGL() {
    _textures.push_back(nullptr);

    if (!_data.groupName.empty()) {
        initializeGroup();
    }
    else {
        _delete.onChange([this]() {
            deinitialize();
            global::scriptEngine->queueScript(std::format(
                "openspace.removeSceneGraphNode('{}')", identifier()
            ));
        });
    }

    initializeTime();
    createGeometry();
    downloadTextureResource(global::timeManager->time().j2000Seconds());
}

void RenderableIswaCygnet::deinitializeGL() {
    if (!_data.groupName.empty()) {
        _group->groupEvent().unsubscribe(identifier());
    }

    unregisterProperties();
    destroyGeometry();

    if (_shader) {
        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
    }
}

bool RenderableIswaCygnet::isReady() const {
    return !_shader;
}

void RenderableIswaCygnet::render(const RenderData& data, RendererTasks&) {
    if (!readyToRender()) {
        return;
    }

    glm::mat4 transform = glm::mat4(1.f);
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            transform[i][j] = static_cast<float>(_stateMatrix[i][j]);
        }
    }
    transform = transform * _rotation;

    glm::vec4 pposition =
        static_cast<glm::vec4>(glm::dvec4(data.modelTransform.translation, 0.0)) +
        transform * glm::vec4(
            _data.spatialScale.x * _data.offset,
            _data.spatialScale.w
        );
    glm::vec3 position =
        glm::vec3(pposition) * static_cast<float>(pow(10.f, pposition.w));

    // Activate shader
    _shader->activate();
    glEnable(GL_ALPHA_TEST);
    glDisable(GL_CULL_FACE);

    _shader->setUniform("ViewProjection", data.camera.viewProjectionMatrix());
    _shader->setUniform("ModelTransform", transform);

    _shader->setUniform("campos", glm::vec4(data.camera.positionVec3(), 1.f));
    _shader->setUniform("objpos", glm::vec4(position, 0.f));
    _shader->setUniform("camrot", glm::mat4(data.camera.viewRotationMatrix()));
    _shader->setUniform("scaling", glm::vec2(1.f, 0.f));


    setUniforms();
    renderGeometry();

    glEnable(GL_CULL_FACE);
    _shader->deactivate();
}

void RenderableIswaCygnet::update(const UpdateData&) {
    if (!_enabled) {
        return;
    }

    // the texture resource is downloaded ahead of time, so we need to
    // now if we are going backwards or forwards
    _openSpaceTime = global::timeManager->time().j2000Seconds();
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

        double clockwiseSign = (global::timeManager->deltaTime() > 0) ? 1.0 : -1.0;
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

void RenderableIswaCygnet::enabled(bool enabled) {
    _enabled = enabled;
}

void RenderableIswaCygnet::registerProperties() {}

void RenderableIswaCygnet::unregisterProperties() {}

void RenderableIswaCygnet::initializeTime() {
    _openSpaceTime = global::timeManager->time().j2000Seconds();
    _lastUpdateOpenSpaceTime = 0.0;

    _realTime = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch()
    );
    _lastUpdateRealTime = _realTime;

    _minRealTimeUpdateInterval = 100;
}

void RenderableIswaCygnet::initializeGroup() {
    _group = IswaManager::ref().iswaGroup(_data.groupName);

    //Subscribe to enable and delete property
    ghoul::Event<ghoul::Dictionary>& groupEvent = _group->groupEvent();

    groupEvent.subscribe(
        identifier(),
        "enabledChanged",
        [this](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event enabledChanged");
            _enabled = dict.value<bool>("enabled");
        }
    );

    groupEvent.subscribe(
        identifier(),
        "alphaChanged",
        [this](const ghoul::Dictionary& dict) {
            LDEBUG(identifier() + " Event alphaChanged");
            _alpha = static_cast<float>(dict.value<double>("alpha"));
        }
    );

    groupEvent.subscribe(
        identifier(),
        "clearGroup",
        [this](ghoul::Dictionary) {
            LDEBUG(identifier() + " Event clearGroup");
            global::scriptEngine->queueScript(
                "openspace.removeSceneGraphNode('" + identifier() + "')"
            );
        }
    );
}

} //namespace openspace
