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
#include <openspace/util/updatestructures.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo CameraPositionInfo = {
        "CameraPosition",
        "Camera Position",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo CameraCenterInfo = {
        "CameraCenter",
        "Camera Center",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo CameraUpInfo = {
        "CameraUp",
        "Camera Up",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo CameraFovInfo = {
        "CameraFov",
        "Camera Field-of-view",
        ""
    };

    struct [[codegen::Dictionary(ScreenSpaceRenderableRenderable)]] Parameters {
        std::optional<std::string> identifier [[codegen::private()]];

        ghoul::Dictionary renderable [[codegen::reference("renderable")]];
    };
#include "screenspacerenderablerenderable_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceRenderableRenderable::Documentation() {
    return codegen::doc<Parameters>("base_screenspace_renderable");
}

ScreenSpaceRenderableRenderable::ScreenSpaceRenderableRenderable(
                                                      const ghoul::Dictionary& dictionary)
    : ScreenSpaceFramebuffer(dictionary)
    , _cameraPosition(
        CameraPositionInfo,
        glm::vec3(0.f, 0.f, 1.f),
        glm::vec3(-50.f, -50.f, -50.f),
        glm::vec3(50.f, 50.f, 50.f)
    )
    , _cameraCenter(CameraCenterInfo, glm::vec3(0.f), glm::vec3(-1.f), glm::vec3(1.f))
    , _cameraUp(CameraUpInfo, glm::vec3(0.f, 1.f, 0.f), glm::vec3(-1.f), glm::vec3(1.f))
    , _cameraFov(CameraFovInfo, 45.f, 5.f, 90.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    std::string identifier = p.identifier.value_or("ScreenSpaceRenderableRenderable");
    setIdentifier(makeUniqueIdentifier(std::move(identifier)));

    addProperty(_cameraPosition);
    addProperty(_cameraCenter);
    addProperty(_cameraUp);
    addProperty(_cameraFov);

    _renderable = Renderable::createFromDictionary(p.renderable);
    _renderable->initialize();
    addPropertySubOwner(_renderable.get());
}

ScreenSpaceRenderableRenderable::~ScreenSpaceRenderableRenderable() {}

bool ScreenSpaceRenderableRenderable::initializeGL() {
    ScreenSpaceFramebuffer::initializeGL();

    _renderable->initializeGL();

    addRenderFunction([this]() {
        glClearColor(0.f, 0.f, 0.f, 1.f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        Camera camera;
        glm::mat4 view = glm::lookAt(
            _cameraPosition.value(),
            _cameraCenter.value(),
            _cameraUp.value()
        );
        camera.sgctInternal.setViewMatrix(view);

        glm::mat4 proj = glm::perspectiveFov(
            glm::radians(_cameraFov.value()),
            _size.value().z,
            _size.value().w,
            0.1f,
            20.f
        );
        camera.sgctInternal.setProjectionMatrix(proj);

        openspace::RenderData renderData = {
            .camera = camera,
            .time = Time(0)
        };
        RendererTasks tasks;
        _renderable->render(renderData, tasks);
    });
    return true;
}

bool ScreenSpaceRenderableRenderable::deinitializeGL() {
    _renderable->deinitializeGL();
    _renderable->deinitialize();
    return ScreenSpaceFramebuffer::deinitializeGL();
}

void ScreenSpaceRenderableRenderable::update() {
    UpdateData updateData;
    _renderable->update(updateData);
}

} // namespace openspace
