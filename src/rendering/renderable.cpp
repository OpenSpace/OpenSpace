/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

namespace {
    constexpr const char* KeyType = "Type";
    constexpr const char* KeyTag = "Tag";

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

    constexpr openspace::properties::Property::PropertyInfo BoundingSphereInfo = {
        "BoundingSphere",
        "Bounding Sphere",
        "The size of the bounding sphere radius."
    };

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
                Optional::No,
                "This key specifies the type of Renderable that gets created. It has to "
                "be one of the valid Renderables that are available for creation (see "
                "the FactoryDocumentation for a list of possible Renderables), which "
                "depends on the configration of the application"
            },
            {
                EnabledInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                EnabledInfo.description
            },
            {
                OpacityInfo.identifier,
                new DoubleInRangeVerifier(0.0, 1.0),
                Optional::Yes,
                OpacityInfo.description
            }
        }
    };
}

ghoul::mm_unique_ptr<Renderable> Renderable::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    documentation::testSpecificationAndThrow(Documentation(), dictionary, "Renderable");

    std::string renderableType = dictionary.value<std::string>(KeyType);

    auto factory = FactoryManager::ref().factory<Renderable>();
    ghoul_assert(factory, "Renderable factory did not exist");
    Renderable* result = factory->create(
        renderableType,
        dictionary,
        &global::memoryManager.PersistentMemory
    );
    return ghoul::mm_unique_ptr<Renderable>(result);
}


Renderable::Renderable(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "Renderable" })
    , _enabled(EnabledInfo, true)
    , _opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , _boundingSphere(BoundingSphereInfo, 0.f, 0.f, 3e10f)
    , _renderableType(RenderableTypeInfo, "Renderable")
{
    ZoneScoped

    // I can't come up with a good reason not to do this for all renderables
    registerUpdateRenderBinFromOpacity();

    if (dictionary.hasKeyAndValue<std::string>(KeyTag)) {
        std::string tagName = dictionary.value<std::string>(KeyTag);
        if (!tagName.empty()) {
            addTag(std::move(tagName));
        }
    }
    else if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeyTag)) {
        const ghoul::Dictionary& tagNames = dictionary.value<ghoul::Dictionary>(KeyTag);
        const std::vector<std::string>& keys = tagNames.keys();
        for (const std::string& key : keys) {
            std::string tagName = tagNames.value<std::string>(key);
            if (!tagName.empty()) {
                addTag(std::move(tagName));
            }
        }
    }

    if (dictionary.hasKey(EnabledInfo.identifier)) {
        _enabled = dictionary.value<bool>(EnabledInfo.identifier);
    }

    if (dictionary.hasKey(OpacityInfo.identifier)) {
        _opacity = static_cast<float>(dictionary.value<double>(
            OpacityInfo.identifier)
       );
    }

    addProperty(_enabled);

    //set type for UI
    if (dictionary.hasKey(RenderableTypeInfo.identifier)) {
        _renderableType = dictionary.value<std::string>(
            RenderableTypeInfo.identifier
       );
    }

    if (dictionary.hasKey(BoundingSphereInfo.identifier)) {
        _boundingSphere = static_cast<float>(
            dictionary.value<double>(BoundingSphereInfo.identifier)
       );
    }

    addProperty(_renderableType);
    addProperty(_boundingSphere);
}

void Renderable::initialize() {}

void Renderable::initializeGL() {}

void Renderable::deinitialize() {}

void Renderable::deinitializeGL() {}

void Renderable::update(const UpdateData&) {}

void Renderable::render(const RenderData&, RendererTasks&) {}

void Renderable::setBoundingSphere(float boundingSphere) {
    _boundingSphere = boundingSphere;
}

float Renderable::boundingSphere() const {
    return _boundingSphere;
}

SurfacePositionHandle Renderable::calculateSurfacePositionHandle(
                                                 const glm::dvec3& targetModelSpace) const
{
    const glm::dvec3 directionFromCenterToTarget = glm::normalize(targetModelSpace);
    return {
        directionFromCenterToTarget * static_cast<double>(boundingSphere()),
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
        if (_renderBin != Renderable::RenderBin::PostDeferredTransparent) {
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
