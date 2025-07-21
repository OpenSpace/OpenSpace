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

#include <modules/base/rendering/screenspacerenderablerenderable.h>

#include <openspace/camera/camera.h>
#include <openspace/rendering/renderable.h>
#include <openspace/scene/rotation.h>
#include <openspace/scene/scale.h>
#include <openspace/scene/translation.h>
#include <openspace/util/updatestructures.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo TimeInfo = {
        "Time",
        "Time",
        "The time (in J2000 seconds) that is used to calculate transformations and the "
        "renderable's data."
    };

    constexpr openspace::properties::Property::PropertyInfo CameraPositionInfo = {
        "CameraPosition",
        "Camera Position",
        "Specifies the location of the virtual camera that is showing the renderable "
        "class. This position is provided in meters."
    };

    constexpr openspace::properties::Property::PropertyInfo CameraCenterInfo = {
        "CameraCenter",
        "Camera Center",
        "The location of the camera's focal point. The camera's view direction will "
        "always be pointing at the provided center location. This position is provided "
        "in meters."
    };

    constexpr openspace::properties::Property::PropertyInfo CameraUpInfo = {
        "CameraUp",
        "Camera Up",
        "The direction that is 'up' for the provided camera. This value does not have "
        "any units."
    };

    constexpr openspace::properties::Property::PropertyInfo CameraFovInfo = {
        "CameraFov",
        "Camera Field of view",
        "The camera's field of view in degrees."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo TransformInfo = {
        "Transform",
        "Transform",
        "The Translation, Rotation, and Scale that are applied to the rendered "
        "Renderable."
    };

    // This [ScreenSpaceRenderable](#core_screenspacerenderable) object can render any
    // [Renderable](#renderable) type into an image that is shown in screen space. This
    // can be used to display a rendered object as an overlay in front of the regular 3D
    // rendering of the scene.
    //
    // Note that to use this `ScreenSpaceRenderable`, it might be necessary to specify the
    // `size` parameter, which determines the resolution of the inset window into which
    // the Renderable is rendered. For many use cases, the default should suffice,
    // however.
    //
    // A possible use-case for the ScreenSpaceRenderable would be to show a 3D model of a
    // spacecraft without the need to place it at a position in the 3D scene with the need
    // to fly to that object to talk about it.
    struct [[codegen::Dictionary(ScreenSpaceRenderableRenderable)]] Parameters {
        std::optional<std::string> identifier [[codegen::private()]];

        // The [Renderable](#renderable) object that is shown in this ScreenSpace object.
        // See the list of creatable renderable objects for options that can be used for
        // this type.
        ghoul::Dictionary renderable [[codegen::reference("renderable")]];

        struct Transform {
            // The [Translation](#core_transform_translation) object that is used for the
            // provided [Renderable](#renderable). If no value is specified, a
            // [StaticTranslation](#base_transform_translation_static) is created instead.
            std::optional<ghoul::Dictionary> translation
                [[codegen::reference("core_transform_translation")]];

            // The [Rotation](#core_transform_rotation) object that is used for the
            // provided [Renderable](#renderable). If no value is specified, a
            // [StaticRotation](#base_transform_rotation_static) is created instead.
            std::optional<ghoul::Dictionary> rotation
                [[codegen::reference("core_transform_rotation")]];

            // The [Scale](#core_transform_scale) object that is used for the provided
            // [Renderable](#renderable). If no value is specified, a
            // [StaticScale](#base_transform_scale_static) is created instead.
            std::optional<ghoul::Dictionary> scale
                [[codegen::reference("core_transform_scale")]];
        };
        // The collection of transformations that are applied to the
        // [Renderable](#renderable) before it is shown on screen.
        std::optional<Transform> transform;

        // Specifies the start date that is used to control the renderable and the
        // transforms. If no value is specified the date of 2000 JAN 01 12:00:00 is used
        // instead.
        std::optional<std::string> time [[codegen::datetime()]];

        // [[codegen::verbatim(CameraPositionInfo.description)]]
        std::optional<glm::vec3> cameraPosition;

        // [[codegen::verbatim(CameraCenterInfo.description)]]
        std::optional<glm::vec3> cameraCenter;

        // [[codegen::verbatim(CameraUpInfo.description)]]
        std::optional<glm::vec3> cameraUp;

        // [[codegen::verbatim(CameraFovInfo.description)]]
        std::optional<float> cameraFov;
    };
#include "screenspacerenderablerenderable_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceRenderableRenderable::Documentation() {
    return codegen::doc<Parameters>(
        "base_screenspace_renderable",
        ScreenSpaceFramebuffer::Documentation()
    );
}

ScreenSpaceRenderableRenderable::ScreenSpaceRenderableRenderable(
                                                      const ghoul::Dictionary& dictionary)
    : ScreenSpaceFramebuffer(dictionary)
    , _time(
        TimeInfo,
        0.0,
        -std::numeric_limits<double>::max(),
        std::numeric_limits<double>::max()
    )
    , _cameraPosition(
        CameraPositionInfo,
        glm::vec3(0.f, 2.f, 2.f),
        glm::vec3(-10.f, -10.f, -10.f),
        glm::vec3(10.f, 10.f, 10.f)
    )
    , _cameraCenter(CameraCenterInfo, glm::vec3(0.f), glm::vec3(-1.f), glm::vec3(1.f))
    , _cameraUp(CameraUpInfo, glm::vec3(0.f, 1.f, 0.f), glm::vec3(-1.f), glm::vec3(1.f))
    , _cameraFov(CameraFovInfo, 45.f, 5.f, 90.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::string identifier = p.identifier.value_or("ScreenSpaceRenderableRenderable");
    setIdentifier(makeUniqueIdentifier(std::move(identifier)));

    if (p.time.has_value()) {
        _time = Time(*p.time).j2000Seconds();
    }
    _time.onChange([this]() { _previousTime = _time; });
    addProperty(_time);

    _cameraPosition = p.cameraPosition.value_or(_cameraPosition);
    addProperty(_cameraPosition);
    _cameraCenter = p.cameraCenter.value_or(_cameraCenter);
    addProperty(_cameraCenter);
    _cameraUp = p.cameraUp.value_or(_cameraUp);
    addProperty(_cameraUp);
    _cameraFov = p.cameraFov.value_or(_cameraFov);
    addProperty(_cameraFov);

    _renderable = Renderable::createFromDictionary(p.renderable);
    addPropertySubOwner(_renderable.get());

    _transform.parent = ghoul::mm_unique_ptr<properties::PropertyOwner>(
        new properties::PropertyOwner(TransformInfo)
    );
    addPropertySubOwner(_transform.parent.get());

    if (p.transform.has_value() && p.transform->translation.has_value()) {
        _transform.translation =
            Translation::createFromDictionary(*p.transform->translation);
    }
    else {
        ghoul::Dictionary translation;
        translation.setValue("Type", std::string("StaticTranslation"));
        translation.setValue("Position", glm::dvec3(0.0));
        _transform.translation = Translation::createFromDictionary(translation);
    }
    _transform.parent->addPropertySubOwner(_transform.translation.get());

    if (p.transform.has_value() && p.transform->rotation.has_value()) {
        _transform.rotation = Rotation::createFromDictionary(*p.transform->rotation);
    }
    else {
        ghoul::Dictionary rotation;
        rotation.setValue("Type", std::string("StaticRotation"));
        rotation.setValue("Rotation", glm::dvec3(0.0));
        _transform.rotation = Rotation::createFromDictionary(rotation);
    }
    _transform.parent->addPropertySubOwner(_transform.rotation.get());

    if (p.transform.has_value() && p.transform->scale.has_value()) {
        _transform.scale = Scale::createFromDictionary(*p.transform->scale);
    }
    else {
        ghoul::Dictionary scale;
        scale.setValue("Type", std::string("StaticScale"));
        scale.setValue("Scale", 1.0);
        _transform.scale = Scale::createFromDictionary(scale);
    }
    _transform.parent->addPropertySubOwner(_transform.scale.get());
}

ScreenSpaceRenderableRenderable::~ScreenSpaceRenderableRenderable() {}

void ScreenSpaceRenderableRenderable::initialize() {
    ScreenSpaceFramebuffer::initialize();
    _transform.translation->initialize();
    _transform.rotation->initialize();
    _transform.scale->initialize();
    _renderable->initialize();
}

void ScreenSpaceRenderableRenderable::initializeGL() {
    ScreenSpaceFramebuffer::initializeGL();

    _renderable->initializeGL();

    addRenderFunction([this]() {
        glm::vec4 bg = _backgroundColor;
        glClearColor(bg.r, bg.g, bg.b, bg.a);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        Camera camera;
        // @TODO (2025-03-24, abock): These two lines can be removed once #3573 is fixed
        camera.setPositionVec3(glm::dvec3(0.0, 0.0, 0.0));
        camera.setRotation(glm::dvec3(0.0, 0.0, 0.0));

        glm::mat4 view = glm::lookAt(
            _cameraPosition.value(),
            _cameraCenter.value(),
            glm::normalize(_cameraUp.value())
        );
        camera.sgctInternal.setViewMatrix(view);

        glm::mat4 proj = glm::perspectiveFov(
            glm::radians(_cameraFov.value()),
            _size.value().x,
            _size.value().y,
            0.1f,
            20.f
        );
        camera.sgctInternal.setProjectionMatrix(proj);

        openspace::RenderData renderData = {
            .camera = camera,
            .time = Time(_time),
            .modelTransform = {
                .translation = _transform.translation->position(),
                .rotation = _transform.rotation->matrix(),
                .scale = _transform.scale->scaleValue()
            }
        };
        RendererTasks tasks;
        _renderable->render(renderData, tasks);
    });
}

void ScreenSpaceRenderableRenderable::deinitializeGL() {
    _renderable->deinitializeGL();
    _renderable->deinitialize();

    ScreenSpaceFramebuffer::deinitializeGL();
}

void ScreenSpaceRenderableRenderable::update() {
    UpdateData updateData = {
        .time = Time(_time),
        .previousFrameTime = Time(_previousTime)
    };

    _transform.translation->update(updateData);
    updateData.modelTransform.translation = _transform.translation->position();

    _transform.rotation->update(updateData);
    updateData.modelTransform.rotation = _transform.rotation->matrix();

    _transform.scale->update(updateData);
    updateData.modelTransform.scale = _transform.scale->scaleValue();

    _renderable->update(updateData);
}

} // namespace openspace
