/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <openspace/engine/globals.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <optional>

namespace {
    constexpr const char* KeyType = "Type";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this object will be visible or not."
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "This value determines the opacity of this renderable. A value of 0 means "
        "completely transparent."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderableTypeInfo = {
        "Type",
        "Renderable Type",
        "This tells the type of the renderable.",
        openspace::properties::Property::Visibility::Hidden
    };

    struct [[codegen::Dictionary(Renderable)]] Parameters {
        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // [[codegen::verbatim(OpacityInfo.description)]]
        std::optional<float> opacity [[codegen::inrange(0.0, 1.0)]];

        // A single tag or a list of tags that this renderable will respond to when
        // setting properties
        std::optional<std::variant<std::vector<std::string>, std::string>> tag;

        // [[codegen::verbatim(RenderableTypeInfo.description)]]
        std::optional<std::string> type;
    };
#include "renderable_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation Renderable::Documentation() {
    return codegen::doc<Parameters>("renderable");
}

ghoul::mm_unique_ptr<Renderable> Renderable::createFromDictionary(
                                                             ghoul::Dictionary dictionary)
{
    if (!dictionary.hasKey(KeyType)) {
        throw ghoul::RuntimeError("Tried to create Renderable but no 'Type' was found");
    }

    // This should be done in the constructor instead with noexhaustive
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "Renderable");

    std::string renderableType = dictionary.value<std::string>(KeyType);
    auto factory = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(factory, "Renderable factory did not exist");
    Renderable* result = factory->create(
        renderableType,
        dictionary,
        &global::memoryManager->PersistentMemory
    );
    return ghoul::mm_unique_ptr<Renderable>(result);
}


Renderable::Renderable(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "Renderable" })
    , _enabled(EnabledInfo, true)
    , _opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , _renderableType(RenderableTypeInfo, "Renderable")
{
    ZoneScoped

    // I can't come up with a good reason not to do this for all renderables
    registerUpdateRenderBinFromOpacity();

    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.tag.has_value()) {
        if (std::holds_alternative<std::string>(*p.tag)) {
            if (!std::get<std::string>(*p.tag).empty()) {
                addTag(std::get<std::string>(*p.tag));
            }
        }
        else {
            ghoul_assert(std::holds_alternative<std::vector<std::string>>(*p.tag), "");
            for (std::string tag : std::get<std::vector<std::string>>(*p.tag)) {
                addTag(std::move(tag));
            }
        }
    }

    _enabled = p.enabled.value_or(_enabled);
    addProperty(_enabled);

    _opacity = p.opacity.value_or(_opacity);
    // We don't add the property here as subclasses should decide on their own whether
    // they to expose the opacity or not

    // set type for UI
    _renderableType = p.type.value_or(_renderableType);
    addProperty(_renderableType);
}

void Renderable::initialize() {}

void Renderable::initializeGL() {}

void Renderable::deinitialize() {}

void Renderable::deinitializeGL() {}

void Renderable::update(const UpdateData&) {}

void Renderable::render(const RenderData&, RendererTasks&) {}

void Renderable::setBoundingSphere(double boundingSphere) {
    _boundingSphere = boundingSphere;
}

double Renderable::boundingSphere() const {
    return _boundingSphere;
}

void Renderable::setInteractionSphere(double interactionSphere) {
    _interactionSphere = interactionSphere;
}

double Renderable::interactionSphere() const {
    return _interactionSphere;
}

SurfacePositionHandle Renderable::calculateSurfacePositionHandle(
                                                 const glm::dvec3& targetModelSpace) const
{
    const glm::dvec3 directionFromCenterToTarget = glm::normalize(targetModelSpace);
    return {
        directionFromCenterToTarget * _parent->interactionSphere(),
        directionFromCenterToTarget,
        0.0
    };
}

bool Renderable::renderedWithDesiredData() const {
    return true;
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

bool Renderable::isReady() const {
    return true;
}

bool Renderable::isEnabled() const {
    return _enabled;
}

bool Renderable::shouldUpdateIfDisabled() const {
    return _shouldUpdateIfDisabled;
}

void Renderable::onEnabledChange(std::function<void(bool)> callback) {
    _enabled.onChange([this, c = std::move(callback)]() {
        c(isEnabled());
    });
}

void Renderable::setRenderBinFromOpacity() {
    if (_renderBin != Renderable::RenderBin::PostDeferredTransparent) {
        if (_opacity >= 0.f && _opacity < 1.f) {
            setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
        }
        else {
            setRenderBin(Renderable::RenderBin::Opaque);
        }
    }
}

void Renderable::registerUpdateRenderBinFromOpacity() {
    _opacity.onChange([this](){
        if ((_renderBin != Renderable::RenderBin::PostDeferredTransparent) &&
            (_renderBin != Renderable::RenderBin::Overlay))
        {
            if (_opacity >= 0.f && _opacity < 1.f) {
                setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
            }
            else {
                setRenderBin(Renderable::RenderBin::Opaque);
            }
        }
    });
}

}  // namespace openspace
