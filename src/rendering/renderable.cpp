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

#include <openspace/rendering/renderable.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/spicemanager.h>
#include <openspace/util/updatestructures.h>

#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>
#include <ghoul/opengl/programobject.h>

namespace {
    const char* _loggerCat = "Renderable";
    const char* keyStart = "StartTime";
    const char* keyEnd = "EndTime";
    const char* KeyType = "Type";
    const char* KeyTag = "Tag";
} // namespace

namespace openspace {

documentation::Documentation Renderable::Documentation() {
    using namespace openspace::documentation;

    return {
        "Renderable",
        "renderable",
        {
        {
            KeyType,
            new StringAnnotationVerifier("A valid Renderable created by a factory"),
            "This key specifies the type of Renderable that gets created. It has to be one"
            "of the valid Renderables that are available for creation (see the "
            "FactoryDocumentation for a list of possible Renderables), which depends on "
            "the configration of the application",
            Optional::No
        }
        }
    };
}

std::unique_ptr<Renderable> Renderable::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    // The name is passed down from the SceneGraphNode
    ghoul_assert(
        dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyName),
        "The SceneGraphNode did not set the 'name' key"
    );
    std::string name = dictionary.value<std::string>(SceneGraphNode::KeyName);

    documentation::testSpecificationAndThrow(Documentation(), dictionary, "Renderable");

    std::string renderableType = dictionary.value<std::string>(KeyType);

    auto factory = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(factory, "Renderable factory did not exist");
    std::unique_ptr<Renderable> result = factory->create(renderableType, dictionary);
    if (result == nullptr) {
        LERROR("Failed to create a Renderable object of type '" << renderableType << "'");
        return nullptr;
    }

    return result;
}

Renderable::Renderable()
    : properties::PropertyOwner("renderable")
    , _enabled("enabled", "Is Enabled", true)
    , _renderBin(RenderBin::Opaque)
    , _startTime("")
    , _endTime("")
    , _hasTimeInterval(false)
{}

Renderable::Renderable(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner("renderable")
    , _enabled("enabled", "Is Enabled", true)
    , _renderBin(RenderBin::Opaque)
    , _startTime("")
    , _endTime("")
    , _hasTimeInterval(false)
{
    ghoul_assert(
        dictionary.hasKeyAndValue<std::string>(SceneGraphNode::KeyName),
        std::string("SceneGraphNode must specify '") + SceneGraphNode::KeyName + "'"
    );

    dictionary.getValue(keyStart, _startTime);
    dictionary.getValue(keyEnd, _endTime);

    if (dictionary.hasKeyAndValue<std::string>(KeyTag)) {
        std::string tagName = dictionary.value<std::string>(KeyTag);
        if (!tagName.empty()) {
            addTag(std::move(tagName));
        }
    } else if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeyTag)) {
        ghoul::Dictionary tagNames = dictionary.value<ghoul::Dictionary>(KeyTag);
        std::vector<std::string> keys = tagNames.keys();
        std::string tagName;
        for (const std::string& key : keys) {
            tagName = tagNames.value<std::string>(key);
            if (!tagName.empty()) {
                addTag(std::move(tagName));
            }
        }
    }

    if (_startTime != "" && _endTime != "") {
        _hasTimeInterval = true;
    }

    addProperty(_enabled);
}

Renderable::~Renderable() {}

void Renderable::setBoundingSphere(float boundingSphere) {
    _boundingSphere = boundingSphere;
}

float Renderable::boundingSphere() const {
    return _boundingSphere;
}

void Renderable::update(const UpdateData&) {}

void Renderable::render(const RenderData& data, RendererTasks&) {
    render(data);
}

void Renderable::render(const RenderData&) {}

SurfacePositionHandle Renderable::calculateSurfacePositionHandle(
                                                       const glm::dvec3& targetModelSpace)
{
    glm::dvec3 directionFromCenterToTarget = glm::normalize(targetModelSpace);
    return {
        directionFromCenterToTarget * static_cast<double>(boundingSphere()),
        directionFromCenterToTarget,
        0.0
    };
}

void Renderable::setPscUniforms(ghoul::opengl::ProgramObject& program,
                                const Camera& camera,
                                const PowerScaledCoordinate& position)
{
    program.setUniform("campos", camera.position().vec4());
    program.setUniform("objpos", position.vec4());
    program.setUniform("camrot", glm::mat4(camera.viewRotationMatrix()));
    program.setUniform("scaling", camera.scaling());
}

Renderable::RenderBin Renderable::renderBin() const {
    return _renderBin;
}

void Renderable::setRenderBin(RenderBin bin) {
    _renderBin = bin;
}

bool Renderable::matchesRenderBinMask(int binMask) {
    return binMask & static_cast<int>(renderBin());
}

bool Renderable::isVisible() const {
    return _enabled;
}

bool Renderable::hasTimeInterval() {
    return _hasTimeInterval;
}

bool Renderable::getInterval(double& start, double& end) {
    if (_startTime != "" && _endTime != "") {
        start = SpiceManager::ref().ephemerisTimeFromDate(_startTime);
        end = SpiceManager::ref().ephemerisTimeFromDate(_endTime);
        return true;
    }
    else {
        return false;
    }
}

bool Renderable::isReady() const {
    return true;
}

bool Renderable::isEnabled() const {
    return _enabled;
}

void Renderable::onEnabledChange(std::function<void(bool)> callback) {
    _enabled.onChange([&] () {
        callback(isEnabled());
    });
}

}  // namespace openspace
