/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/base/rendering/renderableplane.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/glm.h>
#include <glm/gtx/string_cast.hpp>
#include <optional>
#include <variant>

namespace {
    enum BlendMode {
        Normal = 0,
        Additive
    };

    constexpr openspace::properties::Property::PropertyInfo BillboardInfo = {
        "Billboard",
        "Billboard Mode",
        "Specifies whether the plane should be a billboard, which means that it is "
        "always facing the camera. If it is not, it can be oriented using other "
        "transformations.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MirrorBacksideInfo = {
        "MirrorBackside",
        "Mirror Backside of Image Plane",
        "If false, the image plane will not be mirrored when viewed from the backside. "
        "This is usually desirable when the image shows data at a specific location, but "
        "not if it is displaying text for example.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "The size of the plane in meters.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AutoScaleInfo = {
        "AutoScale",
        "Auto Scale",
        "Decides whether the plane should automatically adjust in size to match the "
        "aspect ratio of the content. Otherwise it will remain in the given size."
    };

    constexpr openspace::properties::Property::PropertyInfo BlendModeInfo = {
        "BlendMode",
        "Blending Mode",
        "Determines the blending mode that is applied to this plane.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MultiplyColorInfo = {
        "MultiplyColor",
        "Multiply Color",
        "An RGB color to multiply with the plane's texture. Useful for applying "
        "a color to grayscale images.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderablePlane)]] Parameters {
        // [[codegen::verbatim(BillboardInfo.description)]]
        std::optional<bool> billboard;

        // [[codegen::verbatim(MirrorBacksideInfo.description)]]
        std::optional<bool> mirrorBackside;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::variant<float, glm::vec2> size;

        // [[codegen::verbatim(AutoScaleInfo.description)]]
        std::optional<bool> autoScale;

        enum class [[codegen::map(BlendMode)]] BlendMode {
            Normal,
            Additive
        };
        // [[codegen::verbatim(BlendModeInfo.description)]]
        std::optional<BlendMode> blendMode;

        // [[codegen::verbatim(MultiplyColorInfo.description)]]
        std::optional<glm::vec3> multiplyColor [[codegen::color()]];
    };
#include "renderableplane_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderablePlane::Documentation() {
    return codegen::doc<Parameters>("base_renderable_plane");
}

RenderablePlane::RenderablePlane(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary, { .automaticallyUpdateRenderBin = false })
    , _blendMode(BlendModeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _billboard(BillboardInfo, false)
    , _mirrorBackside(MirrorBacksideInfo, false)
    , _size(SizeInfo, glm::vec2(10.f), glm::vec2(0.f), glm::vec2(1e25f))
    , _autoScale(AutoScaleInfo, false)
    , _multiplyColor(MultiplyColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
{
    Parameters p = codegen::bake<Parameters>(dictionary);

    _opacity.onChange([this]() {
        if (_blendMode == static_cast<int>(BlendMode::Normal)) {
            setRenderBinFromOpacity();
        }
    });
    addProperty(Fadeable::_opacity);

    if (std::holds_alternative<float>(p.size)) {
        _size = glm::vec2(std::get<float>(p.size));
    }
    else {
        _size = std::get<glm::vec2>(p.size);
    }
    _size.setExponent(15.f);
    _size.onChange([this]() { _planeIsDirty = true; });
    addProperty(_size);

    _blendMode.addOptions({
        { static_cast<int>(BlendMode::Normal), "Normal" },
        { static_cast<int>(BlendMode::Additive), "Additive"}
    });
    _blendMode.onChange([this]() {
        const BlendMode m = static_cast<BlendMode>(_blendMode.value());
        switch (m) {
            case BlendMode::Normal:
                setRenderBinFromOpacity();
                break;
            case BlendMode::Additive:
                setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
                break;
        }
    });

    if (p.blendMode.has_value()) {
        _blendMode = codegen::map<BlendMode>(*p.blendMode);
    }
    addProperty(_blendMode);

    _billboard = p.billboard.value_or(_billboard);
    addProperty(_billboard);

    _mirrorBackside = p.mirrorBackside.value_or(_mirrorBackside);
    addProperty(_mirrorBackside);

    _autoScale = p.autoScale.value_or(_autoScale);
    addProperty(_autoScale);

    _multiplyColor = p.multiplyColor.value_or(_multiplyColor);
    _multiplyColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_multiplyColor);

    setBoundingSphere(glm::compMax(_size.value()));
}

bool RenderablePlane::isReady() const {
    return _shader != nullptr;
}

void RenderablePlane::initializeGL() {
    ZoneScoped;

    glGenVertexArrays(1, &_quad); // generate array
    glGenBuffers(1, &_vertexPositionBuffer); // generate buffer
    createPlane();

    _shader = BaseModule::ProgramObjectManager.request(
        "Plane",
        []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return global::renderEngine->buildRenderProgram(
                "Plane",
                absPath("${MODULE_BASE}/shaders/plane_vs.glsl"),
                absPath("${MODULE_BASE}/shaders/plane_fs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
}

void RenderablePlane::deinitializeGL() {
    ZoneScoped;

    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    BaseModule::ProgramObjectManager.release(
        "Plane",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _shader = nullptr;
}

void RenderablePlane::render(const RenderData& data, RendererTasks&) {
    ZoneScoped;

    _shader->activate();
    _shader->setUniform(_uniformCache.opacity, opacity());

    _shader->setUniform(_uniformCache.mirrorBackside, _mirrorBackside);

    const glm::dvec3 objPosWorld = glm::dvec3(
        glm::translate(
            glm::dmat4(1.0),
            data.modelTransform.translation) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );

    const glm::dvec3 normal = glm::normalize(data.camera.positionVec3() - objPosWorld);
    const glm::dvec3 newRight = glm::normalize(
        glm::cross(data.camera.lookUpVectorWorldSpace(), normal)
    );
    const glm::dvec3 newUp = glm::cross(normal, newRight);

    glm::dmat4 cameraOrientedRotation = glm::dmat4(1.0);
    cameraOrientedRotation[0] = glm::dvec4(newRight, 0.0);
    cameraOrientedRotation[1] = glm::dvec4(newUp, 0.0);
    cameraOrientedRotation[2] = glm::dvec4(normal, 0.0);

    const glm::dmat4 rotationTransform = _billboard ?
        cameraOrientedRotation :
        glm::dmat4(data.modelTransform.rotation);

    auto [modelTransform, modelViewTransform, modelViewProjectionTransform] =
        calcAllTransforms(data, { .rotation = rotationTransform });

    _shader->setUniform(
        _uniformCache.modelViewProjection,
        glm::mat4(modelViewProjectionTransform)
    );
    _shader->setUniform(_uniformCache.modelViewTransform, glm::mat4(modelViewTransform));

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    bindTexture();
    defer { unbindTexture(); };

    _shader->setUniform(_uniformCache.colorTexture, unit);

    _shader->setUniform(_uniformCache.multiplyColor, _multiplyColor);

    const bool additiveBlending = (_blendMode == static_cast<int>(BlendMode::Additive));
    if (additiveBlending) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }
    glDisable(GL_CULL_FACE);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    if (additiveBlending) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    }

    _shader->deactivate();
}

void RenderablePlane::bindTexture() {}

void RenderablePlane::unbindTexture() {}

void RenderablePlane::update(const UpdateData&) {
    ZoneScoped;

    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
    }

    if (_planeIsDirty) {
        createPlane();
    }
}

void RenderablePlane::createPlane() {
    const GLfloat sizeX = _size.value().x;
    const GLfloat sizeY = _size.value().y;
    const std::array<GLfloat, 36> vertexData = {
        //   x       y    z    w    s    t
        -sizeX, -sizeY, 0.f, 0.f, 0.f, 0.f,
         sizeX,  sizeY, 0.f, 0.f, 1.f, 1.f,
        -sizeX,  sizeY, 0.f, 0.f, 0.f, 1.f,
        -sizeX, -sizeY, 0.f, 0.f, 0.f, 0.f,
         sizeX, -sizeY, 0.f, 0.f, 1.f, 0.f,
         sizeX,  sizeY, 0.f, 0.f, 1.f, 1.f
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, sizeof(GLfloat) * 6, nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 6,
        reinterpret_cast<void*>(sizeof(GLfloat) * 4)
    );
    glBindVertexArray(0);
}

} // namespace openspace
