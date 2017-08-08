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

#include <modules/toyvolume/rendering/renderabletoyvolume.h>
#include <modules/toyvolume/rendering/toyvolumeraycaster.h>

#include <openspace/rendering/renderable.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/raycastermanager.h>
#include <ghoul/glm.h>
#include <glm/gtc/matrix_transform.hpp>
#include <ghoul/opengl/ghoul_gl.h>

namespace openspace {

RenderableToyVolume::RenderableToyVolume(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _scalingExponent({ "ScalingExponent", "Scaling Exponent", "" }, 1, -10, 20)
    , _stepSize({ "StepSize", "Step Size", "" }, 0.02f, 0.01f, 1.f) // @TODO Missing documentation
    , _scaling({ "Scaling", "Scaling", "" }, glm::vec3(1.f, 1.f, 1.f), glm::vec3(0.f), glm::vec3(10.f)) // @TODO Missing documentation
    , _translation({ "Translation", "Translation", "" }, glm::vec3(0.f, 0.f, 0.f), glm::vec3(0.f), glm::vec3(10.f)) // @TODO Missing documentation
    , _rotation({ "Rotation", "Euler rotation", "" }, glm::vec3(0.f, 0.f, 0.f), glm::vec3(0), glm::vec3(6.28f)) // @TODO Missing documentation
    , _color({ "Color", "Color", "" }, glm::vec4(1.f, 0.f, 0.f, 0.1f), glm::vec4(0.f), glm::vec4(1.f)) // @TODO Missing documentation
{
    float stepSize;
    int scalingExponent;
    glm::vec3 scaling, translation, rotation;
    glm::vec4 color;
    if (dictionary.getValue("ScalingExponent", scalingExponent)) {
        _scalingExponent = scalingExponent;
    }
    if (dictionary.getValue("Scaling", scaling)) {
        _scaling = scaling;
    }
    if (dictionary.getValue("Translation", translation)) {
        _translation = translation;
    }
    if (dictionary.getValue("Rotation", rotation)) {
        _rotation = rotation;
    }
    if (dictionary.getValue("Color", color)) {
        _color = color;
    }
    if (dictionary.getValue("StepSize", stepSize)) {
        _stepSize = stepSize;
    }
}
    
RenderableToyVolume::~RenderableToyVolume() {}

void RenderableToyVolume::initialize() {
    _raycaster = std::make_unique<ToyVolumeRaycaster>(_color);
    _raycaster->initialize();

    OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());

    std::function<void(bool)> onChange = [&](bool enabled) {
        if (enabled) {
            OsEng.renderEngine().raycasterManager().attachRaycaster(*_raycaster.get());
        }
        else {
            OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        }
    };

    onEnabledChange(onChange);

    addProperty(_scaling);
    addProperty(_scalingExponent);
    addProperty(_stepSize);
    addProperty(_translation);
    addProperty(_rotation);
    addProperty(_color);
}
    
void RenderableToyVolume::deinitialize() {
    if (_raycaster) {
        OsEng.renderEngine().raycasterManager().detachRaycaster(*_raycaster.get());
        _raycaster = nullptr;
    }
}
    
bool RenderableToyVolume::isReady() const {
    // @TODO isReady function needs to be filled
    return true;
}
    
void RenderableToyVolume::update(const UpdateData& data) {
    if (_raycaster) {
        glm::mat4 transform = glm::translate(glm::mat4(1.0), static_cast<glm::vec3>(_translation) * std::pow(10.0f, static_cast<float>(_scalingExponent)));
        glm::vec3 eulerRotation = static_cast<glm::vec3>(_rotation);
        transform = glm::rotate(transform, eulerRotation.x, glm::vec3(1, 0, 0));
        transform = glm::rotate(transform, eulerRotation.y, glm::vec3(0, 1, 0));
        transform = glm::rotate(transform, eulerRotation.z,  glm::vec3(0, 0, 1));
        transform = glm::scale(transform, static_cast<glm::vec3>(_scaling) * std::pow(10.0f, static_cast<float>(_scalingExponent)));
        
        _raycaster->setColor(_color);
        _raycaster->setStepSize(_stepSize);
        _raycaster->setModelTransform(transform);
        _raycaster->setTime(data.time.j2000Seconds());
    }
}

void RenderableToyVolume::render(const RenderData& data, RendererTasks& tasks) {
    RaycasterTask task{ _raycaster.get(), data };
    tasks.raycasterTasks.push_back(task);
}
       
} // namespace openspace
