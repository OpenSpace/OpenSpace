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

#include <modules/base/rendering/renderableplane.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/windowdelegate.h>
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
    enum RenderOption {
        ViewDirection = 0,
        PositionNormal,
        FixedRotation
    };

    enum BlendMode {
        Normal = 0,
        Additive
    };

    constexpr openspace::properties::Property::PropertyInfo OrientationRenderOptionInfo =
    {
        "OrientationRenderOption",
        "Orientation Render Option",
        "Controls how the plane will be oriented. \"Camera View Direction\" rotates the "
        "plane so that it is orthogonal to the viewing direction of the camera (useful "
        "for planar displays), and \"Camera Position Normal\" rotates the plane towards "
        "the position of the camera (useful for spherical displays, like dome theaters). "
        "In both these cases the plane will be billboarded towards the camera but in a "
        "slightly different way. In contrast, \"Fixed Rotation\" does not rotate the "
        "plane at all based on the camera and should be used the plane should be "
        "oriented in a fixed way.",
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

    constexpr openspace::properties::Property::PropertyInfo ScaleByDistanceInfo = {
        "ScaleByDistance",
        "Scale By Distance",
        "Decides whether the plane should automatically adjust in size to based on "
        "the distance to the camera. Otherwise it will remain in the given size.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ApparentSizeMultiplierInfo = {
        "ApparentSizeMultiplier",
        "Apparent Size Multiplier",
        "Value that controls the visual size of the object when using distance scaling."
        "A value of 1.0 results in a natural angular size based on camera distance and "
        "field of view. Smaller values (e.g., 0.01) make the object appear smaller, while"
        " larger values make it appear bigger.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleByDistanceMaxHeightInfo =
    {
        "ScaleByDistanceMaxHeight",
        "Scale By Distance Max Height",
        "The maximum height in meters a plane can get when using distance scaling.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleByDistanceMinHeightInfo =
    {
        "ScaleByDistanceMinHeight",
        "Scale By Distance Min Height",
        "The minimum height in meters a plane can get when using distance scaling.",
        openspace::properties::Property::Visibility::AdvancedUser
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

    // A `RenderablePlane` is a renderable that will shows some form of contents projected
    // on a two-dimensional plane, which in turn is placed in three-dimensional space as
    // any other `Renderable`. It is possible to specify the `Size` of the plane, whether
    // it should always face the camera (`Billboard`), and other parameters shown below.
    struct [[codegen::Dictionary(RenderablePlane)]] Parameters {
        enum class [[codegen::map(RenderOption)]] RenderOption {
            ViewDirection [[codegen::key("Camera View Direction")]],
            PositionNormal [[codegen::key("Camera Position Normal")]],
            FixedRotation [[codegen::key("Fixed Rotation")]]
        };

        // Controls whether the plane will be oriented as a billboard. Setting this value
        // to `true` is the same as setting it to \"Camera Position Normal\", setting it
        // to `false` is the same as setting it to \"Fixed Rotation\". If the value is not
        // specified, the default value of `false` is used instead.
        //
        // \"Camera View Direction\" rotates the plane so that it is orthogonal to the
        // viewing direction of the camera (useful for planar displays), and \"Camera
        // Position Normal\" rotates the plane towards the position of the camera (useful
        // for spherical displays, like dome theaters). In both these cases the plane will
        // be billboarded towards the camera but in a slightly different way. In contrast,
        // \"Fixed Rotation\" does not rotate the plane at all based on the camera and
        // should be used the plane should be oriented in a fixed way.
        std::optional<std::variant<bool, RenderOption>> billboard;

        // [[codegen::verbatim(MirrorBacksideInfo.description)]]
        std::optional<bool> mirrorBackside;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::variant<float, glm::vec2> size;

        // [[codegen::verbatim(AutoScaleInfo.description)]]
        std::optional<bool> autoScale;

        struct DistanceScalingSettings {
            // [[codegen::verbatim(ScaleByDistanceInfo.description)]]
            std::optional<bool> scaleByDistance;

            // [[codegen::verbatim(ApparentSizeMultiplierInfo.description)]]
            std::optional<float> apparentSizeMultiplier [[codegen::greater(0.f)]];;

            // [[codegen::verbatim(ScaleByDistanceMaxHeightInfo.description)]]
            std::optional<float> scaleByDistanceMaxHeight [[codegen::greater(0.f)]];;

            // [[codegen::verbatim(ScaleByDistanceMinHeightInfo.description)]]
            std::optional<float> scaleByDistanceMinHeight [[codegen::greater(0.f)]];;
        };

        // Settings for scaling points based on camera distance
        std::optional<DistanceScalingSettings> distanceScalingSettings;

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

RenderablePlane::DistanceScalingSettings::DistanceScalingSettings(
    const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "DistanceScaling", "Distance Scaling", "" })
    , scaleByDistance(ScaleByDistanceInfo, false)
    , apparentSizeMultiplier(ApparentSizeMultiplierInfo, 1.f)
    , scaleByDistanceMaxHeight(ScaleByDistanceMaxHeightInfo, 2000.f)
    , scaleByDistanceMinHeight(ScaleByDistanceMinHeightInfo, 100.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.distanceScalingSettings.has_value()) {
        const Parameters::DistanceScalingSettings settings = *p.distanceScalingSettings;

        scaleByDistance = settings.scaleByDistance.value_or(scaleByDistance);
        apparentSizeMultiplier =
            settings.apparentSizeMultiplier.value_or(apparentSizeMultiplier);
        scaleByDistanceMaxHeight =
            settings.scaleByDistanceMaxHeight.value_or(scaleByDistanceMaxHeight);
        scaleByDistanceMinHeight =
            settings.scaleByDistanceMinHeight.value_or(scaleByDistanceMinHeight);
    }

    addProperty(scaleByDistance);
    addProperty(apparentSizeMultiplier);
    addProperty(scaleByDistanceMaxHeight);
    addProperty(scaleByDistanceMinHeight);
}

RenderablePlane::RenderablePlane(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary, { .automaticallyUpdateRenderBin = false })
    , _blendMode(BlendModeInfo)
    , _renderOption(OrientationRenderOptionInfo)
    , _mirrorBackside(MirrorBacksideInfo, false)
    , _size(SizeInfo, glm::vec2(10.f), glm::vec2(0.f), glm::vec2(1e25f))
    , _autoScale(AutoScaleInfo, false)
    , _multiplyColor(MultiplyColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _distanceScalingSettings(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

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
        { static_cast<int>(BlendMode::Additive), "Additive" }
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

    _renderOption.addOption(RenderOption::ViewDirection, "Camera View Direction");
    _renderOption.addOption(RenderOption::PositionNormal, "Camera Position Normal");
    _renderOption.addOption(RenderOption::FixedRotation, "Fixed Rotation");

    if (p.billboard.has_value()) {
        ghoul_assert(
            std::holds_alternative<bool>(*p.billboard) ||
            std::holds_alternative<Parameters::RenderOption>(*p.billboard),
            "Wrong type"
        );

        if (std::holds_alternative<bool>(*p.billboard)) {
            _renderOption = std::get<bool>(*p.billboard) ?
                RenderOption::ViewDirection :
                RenderOption::FixedRotation;
        }
        else {
            _renderOption = codegen::map<RenderOption>(
                std::get<Parameters::RenderOption>(*p.billboard)
            );
        }
    }
    else {
        _renderOption = RenderOption::FixedRotation;
    }
    addProperty(_renderOption);

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

    if (_distanceScalingSettings.scaleByDistance) {
        if (global::windowDelegate->isFisheyeRendering()) {
            LWARNINGC("RenderablePlane",
                "Distance scaling is disabled in Fisheye rendering mode.");
            _distanceScalingSettings.scaleByDistance = false;
        }
        else {
            const glm::dvec3 cameraPosition = data.camera.positionVec3();
            const glm::dvec3 modelPosition = data.modelTransform.translation;

            const double fovDegrees = static_cast<double>(
                                             global::windowDelegate->horizFieldOfView(0));
            const double fovRadians = glm::radians(fovDegrees);
            const double halfFovTan = std::tan(fovRadians * 0.5);

            const double distance = glm::distance(cameraPosition, modelPosition);

            double projectedHeight = 2.0 * distance * halfFovTan *
                static_cast<double>(_distanceScalingSettings.apparentSizeMultiplier);

            projectedHeight = std::clamp(
                projectedHeight,
                static_cast<double>(
                               _distanceScalingSettings.scaleByDistanceMinHeight.value()),
                static_cast<double>(
                                _distanceScalingSettings.scaleByDistanceMaxHeight.value())
            );

            const glm::vec2 currentSize = _size.value();
            if (currentSize.y > 0.f) {
                const double scaleFactor = projectedHeight / static_cast<double>(
                                                                           currentSize.y);
                const glm::vec2 scaledSize = currentSize * static_cast<float>(
                                                                             scaleFactor);
                _size.setValue(scaledSize);
            }
        }
    }

    glm::dmat4 rotationTransform = rotationMatrix(data);
    auto [modelTransform, modelViewTransform, modelViewProjectionTransform] =
        calcAllTransforms(data, { .rotation = std::move(rotationTransform) });

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

    if (_shader->isDirty()) [[unlikely]] {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache);
    }

    if (_planeIsDirty) [[unlikely]] {
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

glm::dmat4 RenderablePlane::rotationMatrix(const RenderData& data) const {
    switch (_renderOption.value()) {
        case RenderOption::ViewDirection:
        {
            glm::dvec3 cameraViewDirectionWorld = -data.camera.viewDirectionWorldSpace();
            glm::dvec3 cameraUpDirectionWorld = data.camera.lookUpVectorWorldSpace();
            glm::dvec3 orthoRight = glm::normalize(
                glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
            );
            if (orthoRight == glm::dvec3(0.0)) {
                glm::dvec3 otherVector = glm::vec3(
                    cameraUpDirectionWorld.y,
                    cameraUpDirectionWorld.x,
                    cameraUpDirectionWorld.z
                );
                orthoRight = glm::normalize(
                    glm::cross(otherVector, cameraViewDirectionWorld)
                );
            }
            glm::dvec3 orthoUp = glm::normalize(
                glm::cross(cameraViewDirectionWorld, orthoRight)
            );

            glm::dmat4 cameraOrientedRotation = glm::dmat4(1.0);
            cameraOrientedRotation[0] = glm::dvec4(orthoRight, 0.0);
            cameraOrientedRotation[1] = glm::dvec4(orthoUp, 0.0);
            cameraOrientedRotation[2] = glm::dvec4(cameraViewDirectionWorld, 0.0);
            return cameraOrientedRotation;
        }
        case RenderOption::PositionNormal:
        {
            const glm::dvec3 objPosWorld = glm::dvec3(
                glm::translate(
                    glm::dmat4(1.0),
                    data.modelTransform.translation
                ) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
            );

            const glm::dvec3 normal = glm::normalize(
                data.camera.positionVec3() - objPosWorld
            );
            const glm::dvec3 newRight = glm::normalize(
                glm::cross(data.camera.lookUpVectorWorldSpace(), normal)
            );
            const glm::dvec3 newUp = glm::cross(normal, newRight);

            glm::dmat4 cameraOrientedRotation = glm::dmat4(1.0);
            cameraOrientedRotation[0] = glm::dvec4(newRight, 0.0);
            cameraOrientedRotation[1] = glm::dvec4(newUp, 0.0);
            cameraOrientedRotation[2] = glm::dvec4(normal, 0.0);
            return cameraOrientedRotation;
        }
        case RenderOption::FixedRotation:
            return glm::dmat4(data.modelTransform.rotation);
    }

    throw std::logic_error("Missing case exception");
}

} // namespace openspace
