/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <openspace/camera/camera.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/events/event.h>
#include <openspace/events/eventengine.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/factorymanager.h>
#include <openspace/util/memorymanager.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <optional>

namespace {
    constexpr std::string_view KeyType = "Type";

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this object will be visible or not"
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "This value determines the opacity of this renderable. A value of 0 means "
        "completely transparent"
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInfo = {
        "Fade",
        "Fade",
        "This value is used by the system to be able to fade out renderables "
        "independently from the Opacity value selected by the user. This value should "
        "not be directly manipulated through a user interface, but instead used by other "
        "components of the system programmatically",
        // The Developer mode should be used once the properties in the UI listen to this
        // openspace::properties::Property::Visibility::Developer
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo RenderableTypeInfo = {
        "Type",
        "Renderable Type",
        "This tells the type of the renderable",
        openspace::properties::Property::Visibility::Hidden
    };

    constexpr openspace::properties::Property::PropertyInfo RenderableRenderBinModeInfo =
    {
        "RenderBinMode",
        "Render Bin Mode",
        "This value specifies if the renderable should be rendered in the Background,"
        "Opaque, Pre/PostDeferredTransparency, or Overlay rendering step",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo DimInAtmosphereInfo = {
        "DimInAtmosphere",
        "Dim In Atmosphere",
        "Enables/Disables if the object should be dimmed when the camera is in the "
        "sunny part of an atmosphere",
        openspace::properties::Property::Visibility::Developer
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

        // Fragile! Keep in sync with documentation
        enum class [[codegen::map(openspace::Renderable::RenderBin)]] RenderBinMode {
            Background,
            Opaque,
            PreDeferredTransparent,
            PostDeferredTransparent,
            Overlay
        };

        // [[codegen::verbatim(RenderableRenderBinModeInfo.description)]]
        std::optional<RenderBinMode> renderBinMode;

        // [[codegen::verbatim(DimInAtmosphereInfo.description)]]
        std::optional<bool> dimInAtmosphere;
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
    ghoul::TemplateFactory<Renderable>* factory =
        FactoryManager::ref().factory<Renderable>();
    ghoul_assert(factory, "Renderable factory did not exist");
    Renderable* result = factory->create(
        renderableType,
        dictionary,
        &global::memoryManager->PersistentMemory
    );
    result->_type = renderableType;
    return ghoul::mm_unique_ptr<Renderable>(result);
}



Renderable::Renderable(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "Renderable" })
    , _enabled(EnabledInfo, true)
    , _opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , _fade(FadeInfo, 1.f, 0.f, 1.f)
    , _renderableType(RenderableTypeInfo, "Renderable")
    , _dimInAtmosphere(DimInAtmosphereInfo, false)
{
    ZoneScoped;

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
    _enabled.onChange([this]() {
        if (isEnabled()) {
            global::eventEngine->publishEvent<events::EventRenderableEnabled>(_parent);
        }
        else {
            global::eventEngine->publishEvent<events::EventRenderableDisabled>(_parent);
        }
    });

    _opacity = p.opacity.value_or(_opacity);
    // We don't add the property here as subclasses should decide on their own whether
    // they to expose the opacity or not

    addProperty(_fade);

    // set type for UI
    _renderableType = p.type.value_or(_renderableType);
    addProperty(_renderableType);

    // only used by a few classes such as RenderableTrail and RenderableSphere
    if (p.renderBinMode.has_value()) {
        setRenderBin(codegen::map<Renderable::RenderBin>(*p.renderBinMode));
    }

    _dimInAtmosphere = p.dimInAtmosphere.value_or(_dimInAtmosphere);
    addProperty(_dimInAtmosphere);
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

std::string_view Renderable::typeAsString() const {
    return _renderableType;
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

void Renderable::setFade(float fade) {
    _fade = fade;
}

bool Renderable::isVisible() const {
    return _enabled && _opacity > 0.f && _fade > 0.f;
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
    if ((_renderBin != Renderable::RenderBin::PostDeferredTransparent) &&
        (_renderBin != Renderable::RenderBin::Overlay))
    {
        const float v = opacity();
        if (v >= 0.f && v < 1.f) {
            setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
        }
        else {
            setRenderBin(Renderable::RenderBin::Opaque);
        }
    }
}

void Renderable::registerUpdateRenderBinFromOpacity() {
    _opacity.onChange([this]() { setRenderBinFromOpacity(); });
    _fade.onChange([this]() { setRenderBinFromOpacity(); });
}

float Renderable::opacity() const {
    // Rendering should depend on if camera is in the atmosphere and if camera is at the
    // dark part of the globe
    const float dimming = _dimInAtmosphere ?
        global::navigationHandler->camera()->atmosphereDimmingFactor() :
        1.f;
    return _opacity * _fade * dimming;
}

}  // namespace openspace
