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

#include <openspace/rendering/screenspacerenderable.h>

#include <openspace/camera/camera.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <optional>
#include <variant>

namespace {
    constexpr std::array<const char*, 9> UniformNames = {
        "color", "opacity", "blackoutFactor", "mvpMatrix", "tex", "backgroundColor",
        "gamma", "borderColor", "borderWidth"
    };

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "This setting determines whether this sceen space plane will be visible or not",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderDuringBlackoutInfo = {
        "RenderDuringBlackout",
        "Render during Blackout",
        "If this value is 'true', this screenspace renderable is going to ignore the "
        "global blackout factor from the Render Engine and will always render at full "
        "opacity. If it is 'false', it will adhere to the factor and fade out like the "
        "rest of the 3D rendering",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo
        UseRadiusAzimuthElevationInfo =
    {
        "UseRadiusAzimuthElevation",
        "Use Radius Azimuth and Elevation",
        "This value determines whether the location of this screen space plane will be "
        "specified using radius, azimuth and elevation (if this is set to 'true') or "
        "using cartesian coordinates. By switching this value, the correct property will "
        "be shown or hidden. The Cartesian coordinate system is useful if a regular "
        "rendering is applied, whereas the radius azimuth elevation are most useful in a "
        "planetarium environment",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo UsePerspectiveProjectionInfo =
    {
        "UsePerspectiveProjection",
        "Use Perspective Projection",
        "Determines whetether the z/radius values affects the size of the plane or not",
        // @VISIBILITY(3.25)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo CartesianPositionInfo = {
        "CartesianPosition",
        "Cartesian Coordinates",
        "This value determines the position of this screen space plane in Cartesian "
        "three-dimensional coordinates (meters)",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusAzimuthElevationInfo = {
        "RadiusAzimuthElevation",
        "Radius Azimuth Elevation",
        "This value determines the position of this screen space plane in a "
        "coordinate system based on radius (meters), azimuth (radians) and elevation "
        "(radians)",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Scale Value",
        "This value determines a scale factor for the plane. The default size of a plane "
        "is determined by the concrete instance and reflects, for example, the size of "
        "the image being displayed",
        // @VISIBILITY(1.6)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo LocalRotationInfo = {
        "Rotation",
        "Local Rotation",
        "An euler rotation (x, y, z) to apply to the plane",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MultiplyColorInfo = {
        "MultiplyColor",
        "Multiply Color",
        "If set, the plane's texture is multiplied with this color. Useful for applying "
        "a color grayscale images",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo BackgroundColorInfo = {
        "BackgroundColor",
        "Background Color",
        "The fixed color that is combined with the screen space renderable to create the "
        "final color. The actual color of the screen space renderable is alpha-blended "
        "with the background color to produce the final result",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DeleteInfo = {
        "Delete",
        "Delete",
        "If this property is triggered, this screen space plane is removed from the "
        "scene",
        // @VISIBILITY(2.25)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FaceCameraInfo = {
        "FaceCamera",
        "Face Camera",
        "If enabled, the local rotation is applied after the plane is rotated to face "
        "the camera",
        // @VISIBILITY(1.25)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo GammaInfo = {
        "Gamma",
        "Gamma Correction",
        "Sets the gamma correction of the texture",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BorderWidthInfo = {
        "BorderWidth",
        "Border Width",
        "The width of the border",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo BorderColorInfo = {
        "BorderColor",
        "Border Color",
        "Sets the color of the border",
        openspace::properties::Property::Visibility::NoviceUser
    };

    float wrap(float value, float min, float max) {
        return glm::mod(value - min, max - min) + min;
    }

    struct [[codegen::Dictionary(ScreenSpaceRenderable)]] Parameters {
        // The type of the Screenspace renderable that is to be created. The available
        // types of Screenspace renderable depend on the configuration of the application
        // and can be written to disk on application startup into the FactoryDocumentation
        std::string type
            [[codegen::annotation("Must name a valid Screenspace renderable")]];

        // Specifies the name of this screenspace renderable. This does not have to be
        // unique to the scene, but it is recommended to be
        std::optional<std::string> name;

        // This is the unique identifier for this screenspace renderable. It has to be
        // unique amongst all existing screenspace nodes that already have been added to
        // the scene.
        std::optional<std::string> identifier [[codegen::identifier()]];

        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // [[codegen::verbatim(RenderDuringBlackoutInfo.description)]]
        std::optional<bool> renderDuringBlackout;

        // [[codegen::verbatim(UseRadiusAzimuthElevationInfo.description)]]
        std::optional<bool> useRadiusAzimuthElevation;

        // [[codegen::verbatim(FaceCameraInfo.description)]]
        std::optional<bool> faceCamera;

        // [[codegen::verbatim(CartesianPositionInfo.description)]]
        std::optional<glm::vec3> cartesianPosition;

        // [[codegen::verbatim(GammaInfo.description)]]
        std::optional<glm::vec3> radiusAzimuthElevation;

        // [[codegen::verbatim(BorderWidthInfo.description)]]
        std::optional<float> borderWidth [[codegen::greater(0.0)]];

        // [[codegen::verbatim(BorderColorInfo.description)]]
        std::optional<glm::vec3> borderColor [[codegen::color()]];

        // [[codegen::verbatim(ScaleInfo.description)]]
        std::optional<float> scale;

        // [[codegen::verbatim(UseRadiusAzimuthElevationInfo.description)]]
        std::optional<float> gamma;

        // [[codegen::verbatim(UsePerspectiveProjectionInfo.description)]]
        std::optional<bool> usePerspectiveProjection;

        // [[codegen::verbatim(MultiplyColorInfo.description)]]
        std::optional<glm::vec3> multiplyColor [[codegen::color()]];

        // [[codegen::verbatim(BackgroundColorInfo.description)]]
        std::optional<glm::vec4> backgroundColor [[codegen::color()]];

        // This value determines the opacity of the screen space plane. If this value
        // is 1, the plane is completely opaque, if this value is 0, the plane is
        // completely transparent
        std::optional<float> opacity [[codegen::inrange(0.f, 1.f)]];

        // Defines either a single or multiple tags that apply to this
        // ScreenSpaceRenderable, thus making it possible to address multiple, separate
        // Renderables with a single property change
        std::optional<std::variant<std::string, std::vector<std::string>>> tag;
    };
#include "screenspacerenderable_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceRenderable::Documentation() {
    return codegen::doc<Parameters>("core_screenspacerenderable");
}

std::unique_ptr<ScreenSpaceRenderable> ScreenSpaceRenderable::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    ScreenSpaceRenderable* ssr =
        FactoryManager::ref().factory<ScreenSpaceRenderable>()->create(
            p.type,
            dictionary
        );
    ssr->_type = p.type;
    return std::unique_ptr<ScreenSpaceRenderable>(ssr);
}

std::string ScreenSpaceRenderable::makeUniqueIdentifier(std::string name) {
    std::vector<ScreenSpaceRenderable*> rs =
        global::renderEngine->screenSpaceRenderables();

    auto nameTaken = [&rs](const std::string& n) {
        const bool taken = std::any_of(
            rs.cbegin(),
            rs.cend(),
            [&n](ScreenSpaceRenderable* r) { return r->identifier() == n; }
        );
        return taken;
    };

    std::string baseName = name;
    int i = 1;
    while (nameTaken(name)) {
        name = baseName + std::to_string(i);
        i++;
    }
    return name;
}

ScreenSpaceRenderable::ScreenSpaceRenderable(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "" })
    , _enabled(EnabledInfo, true)
    , _renderDuringBlackout(RenderDuringBlackoutInfo, false)
    , _usePerspectiveProjection(UsePerspectiveProjectionInfo, false)
    , _useRadiusAzimuthElevation(UseRadiusAzimuthElevationInfo, false)
    , _faceCamera(FaceCameraInfo, true)
    , _cartesianPosition(
        CartesianPositionInfo,
        glm::vec3(0.f, 0.f, -2.f),
        glm::vec3(-4.f, -4.f, -10.f),
        glm::vec3(4.f, 4.f, 0.f)
    )
    , _raePosition(
        RadiusAzimuthElevationInfo,
        glm::vec3(2.f, 0.f, 0.f),
        glm::vec3(0.f, -glm::pi<float>(), -glm::half_pi<float>()),
        glm::vec3(10.f, glm::pi<float>(), glm::half_pi<float>())
    )
    , _localRotation(
        LocalRotationInfo,
        glm::vec3(0.f),
        glm::vec3(-glm::pi<float>()),
        glm::vec3(glm::pi<float>())
    )
    , _borderWidth(BorderWidthInfo, 0.f, 0.f, 1000.f)
    , _borderColor(BorderColorInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _scale(ScaleInfo, 0.25f, 0.f, 2.f)
    , _gamma(GammaInfo, 1.f, 0.000001f, 10.f)
    , _multiplyColor(MultiplyColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _backgroundColor(
        BackgroundColorInfo,
        glm::vec4(0.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _delete(DeleteInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.identifier.has_value()) {
        setIdentifier(*p.identifier);
    }

    if (p.name.has_value()) {
        setGuiName(*p.name);
    }

    addProperty(_enabled);
    _renderDuringBlackout = p.renderDuringBlackout.value_or(_renderDuringBlackout);
    addProperty(_renderDuringBlackout);
    addProperty(_useRadiusAzimuthElevation);
    addProperty(_usePerspectiveProjection);
    addProperty(_faceCamera);
    addProperty(_cartesianPosition);
    addProperty(_raePosition);
    addProperty(_gamma);

    // Setting spherical/euclidean onchange handler
    _useRadiusAzimuthElevation.onChange([this]() {
        if (_useRadiusAzimuthElevation) {
            _raePosition = sphericalToRae(cartesianToSpherical(_cartesianPosition));
        }
        else {
            _cartesianPosition = sphericalToCartesian(raeToSpherical(_raePosition));
        }
    });

    addProperty(_scale);
    addProperty(_multiplyColor);
    addProperty(_backgroundColor);
    addProperty(Fadeable::_opacity);
    addProperty(Fadeable::_fade);
    addProperty(_localRotation);

    addProperty(_borderColor);
    addProperty(_borderWidth);

    _borderWidth = p.borderWidth.value_or(_borderWidth);

    _borderColor = p.borderColor.value_or(_borderColor);
    _borderColor.setViewOption(properties::Property::ViewOptions::Color);

    _multiplyColor = p.multiplyColor.value_or(_multiplyColor);
    _multiplyColor.setViewOption(properties::Property::ViewOptions::Color);

    _backgroundColor = p.backgroundColor.value_or(_backgroundColor);
    _backgroundColor.setViewOption(properties::Property::ViewOptions::Color);

    _enabled = p.enabled.value_or(_enabled);
    _gamma = p.gamma.value_or(_gamma);

    _useRadiusAzimuthElevation =
        p.useRadiusAzimuthElevation.value_or(_useRadiusAzimuthElevation);

    if (_useRadiusAzimuthElevation) {
        _raePosition = p.radiusAzimuthElevation.value_or(_raePosition);
    }
    else {
        _cartesianPosition = p.cartesianPosition.value_or(_cartesianPosition);
    }

    _scale = p.scale.value_or(_scale);
    _opacity = p.opacity.value_or(_opacity);
    _usePerspectiveProjection =
        p.usePerspectiveProjection.value_or(_usePerspectiveProjection);

    _faceCamera = p.faceCamera.value_or(_faceCamera);

    if (p.tag.has_value()) {
        if (std::holds_alternative<std::string>(*p.tag)) {
            addTag(std::get<std::string>(*p.tag));
        }
        else if (std::holds_alternative<std::vector<std::string>>(*p.tag)) {
            for (const std::string& t : std::get<std::vector<std::string>>(*p.tag)) {
                if (!t.empty()) {
                    addTag(t);
                }
            }
        }
        else {
            throw ghoul::MissingCaseException();
        }
    }

    _delete.onChange([this](){
        std::string script =
            "openspace.removeScreenSpaceRenderable('" + identifier() + "');";
        // No sync or send because this is already inside a Lua script that was triggered
        // when this triggerProperty was pressed in the gui, therefor it has already been
        // synced and sent to the connected nodes and peers
        global::scriptEngine->queueScript(
            script,
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    });
    addProperty(_delete);
}

ScreenSpaceRenderable::~ScreenSpaceRenderable() {}

bool ScreenSpaceRenderable::initialize() {
    return true;
}

bool ScreenSpaceRenderable::initializeGL() {
    createShaders();
    return isReady();
}

bool ScreenSpaceRenderable::deinitialize() {
    return true;
}

bool ScreenSpaceRenderable::deinitializeGL() {
    if (_shader) {
        global::renderEngine->removeRenderProgram(_shader.get());
        _shader = nullptr;
    }

    return true;
}

void ScreenSpaceRenderable::render(float blackoutFactor) {
    ZoneScoped;

    glm::mat4 mat =
        globalRotationMatrix() *
        translationMatrix() *
        localRotationMatrix() *
        scaleMatrix();
    draw(mat, blackoutFactor);
}

bool ScreenSpaceRenderable::isReady() const {
    return _shader != nullptr;
}

void ScreenSpaceRenderable::update() {}

bool ScreenSpaceRenderable::isEnabled() const {
    return _enabled;
}

bool ScreenSpaceRenderable::isUsingRaeCoords() const {
    return _useRadiusAzimuthElevation;
}

bool ScreenSpaceRenderable::isFacingCamera() const {
    return _faceCamera;
}

void ScreenSpaceRenderable::setEnabled(bool isEnabled) {
    _enabled = isEnabled;
}

float ScreenSpaceRenderable::depth() {
    return _useRadiusAzimuthElevation ?
        _raePosition.value().x :
        cartesianToSpherical(_cartesianPosition).x;
}

float ScreenSpaceRenderable::scale() const {
    return _scale;
}

void ScreenSpaceRenderable::createShaders() {
    ghoul::Dictionary dict = ghoul::Dictionary();

    auto res = global::windowDelegate->currentDrawBufferResolution();
    ghoul::Dictionary rendererData;
    rendererData.setValue(
        "fragmentRendererPath",
        std::string("${SHADERS}/framebuffer/renderframebuffer.frag")
    );
    rendererData.setValue("windowWidth", res.x);
    rendererData.setValue("windowHeight", res.y);
    rendererData.setValue(
        "hdrExposure",
        static_cast<double>(global::renderEngine->hdrExposure())
    );
    rendererData.setValue("disableHDR", true);

    dict.setValue("rendererData", rendererData);
    dict.setValue(
        "fragmentPath",
        std::string("${MODULE_BASE}/shaders/screenspace_fs.glsl")
    );
    _shader = ghoul::opengl::ProgramObject::Build(
        "ScreenSpaceProgram",
        absPath("${MODULE_BASE}/shaders/screenspace_vs.glsl"),
        absPath("${SHADERS}/render.frag"),
        dict
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
}

glm::mat4 ScreenSpaceRenderable::scaleMatrix() {
    // to scale the plane
    float textureRatio =
        static_cast<float>(_objectSize.y) / static_cast<float>(_objectSize.x);

    glm::mat4 scale = glm::scale(
        glm::mat4(1.f),
        glm::vec3(_scale, textureRatio*_scale, 1.f)
    );

    return scale;
}

glm::vec2 ScreenSpaceRenderable::screenSpacePosition() {
    return glm::vec2(_cartesianPosition.value());
}

glm::vec2 ScreenSpaceRenderable::screenSpaceDimensions() {
    float ratio = static_cast<float>(_objectSize.x) / static_cast<float>(_objectSize.y);
    return glm::vec2(2.f * _scale * ratio, 2.f * _scale);
}

glm::vec2 ScreenSpaceRenderable::upperRightCornerScreenSpace() {
    return screenSpacePosition() + (screenSpaceDimensions() / 2.0f);
}

glm::vec2 ScreenSpaceRenderable::lowerLeftCornerScreenSpace() {
    return screenSpacePosition() - (screenSpaceDimensions() / 2.0f);
}

bool ScreenSpaceRenderable::isIntersecting(glm::vec2 coord) {
    bool isUnderTopBorder = coord.x < upperRightCornerScreenSpace().x;
    bool isLeftToRightBorder = coord.y < upperRightCornerScreenSpace().y;
    bool isRightToLeftBorder = coord.x > lowerLeftCornerScreenSpace().x;
    bool isOverBottomBorder = coord.y > lowerLeftCornerScreenSpace().y;

    return  isUnderTopBorder && isLeftToRightBorder &&
            isRightToLeftBorder && isOverBottomBorder;
}

void ScreenSpaceRenderable::translate(glm::vec2 translation, glm::vec2 position) {
    glm::mat4 translationMatrix = glm::translate(
        glm::mat4(1.f),
        glm::vec3(translation, 0.0f)
    );
    glm::vec4 origin = glm::vec4(position, _cartesianPosition.value().z, 1.0f);
    _cartesianPosition = translationMatrix * origin;
}

void ScreenSpaceRenderable::setCartesianPosition(const glm::vec3& position) {
    _cartesianPosition = position;
}

void ScreenSpaceRenderable::setRaeFromCartesianPosition(const glm::vec3& position) {
    _raePosition = cartesianToRae(position);
}

glm::vec3 ScreenSpaceRenderable::raePosition() const {
    return _raePosition;
}

glm::mat4 ScreenSpaceRenderable::globalRotationMatrix() {
    // We do not want the screen space planes to be affected by
    // 1) The global rotation of the view applied in the render engine
    // 2) sgct's scene matrix (also called model matrix by sgct)

    glm::mat4 inverseRotation = glm::inverse(
        global::renderEngine->globalRotation() *
        global::windowDelegate->modelMatrix()
    );

    // The rotation of all screen space renderables is adjustable in the render engine:
    return global::renderEngine->screenSpaceRotation() * inverseRotation;
}

glm::mat4 ScreenSpaceRenderable::localRotationMatrix() {
    glm::mat4 rotation = glm::mat4(1.f);
    if (_faceCamera) {
        glm::vec3 translation = _useRadiusAzimuthElevation ?
            sphericalToCartesian(raeToSpherical(_raePosition)) :
            _cartesianPosition;

        rotation = glm::inverse(glm::lookAt(
            glm::vec3(0.f),
            glm::normalize(translation),
            glm::vec3(0.f, 1.f, 0.f)
        ));
    }

    float roll = _localRotation.value().x;
    float pitch = _localRotation.value().y;
    float yaw = _localRotation.value().z;
    return rotation * glm::mat4(glm::quat(glm::vec3(pitch, yaw, roll)));
}

glm::vec3 ScreenSpaceRenderable::raeToCartesian(const glm::vec3& rae) const {
    return sphericalToCartesian(raeToSpherical(rae));
}

glm::vec3 ScreenSpaceRenderable::cartesianToRae(const glm::vec3& cartesian) const {
    return sphericalToRae(cartesianToSpherical(cartesian));
}

glm::mat4 ScreenSpaceRenderable::translationMatrix() {
    glm::vec3 translation = _useRadiusAzimuthElevation ?
        sphericalToCartesian(raeToSpherical(_raePosition)) :
        _cartesianPosition;

    return glm::translate(glm::mat4(1.f), translation);
}

void ScreenSpaceRenderable::draw(glm::mat4 modelTransform, float blackoutFactor) {
    glDisable(GL_CULL_FACE);

    _shader->activate();
    // Calculate the border from pixels to UV coordinates
    glm::vec2 borderUV = glm::vec2(
        _borderWidth / static_cast<float>(_objectSize.x),
        _borderWidth / static_cast<float>(_objectSize.y)
    );

    _shader->setUniform(_uniformCache.color, _multiplyColor);
    _shader->setUniform(_uniformCache.opacity, opacity());
    _shader->setUniform(
        _uniformCache.blackoutFactor,
        _renderDuringBlackout ? 1.f : blackoutFactor
    );
    _shader->setUniform(_uniformCache.backgroundColor, _backgroundColor);
    _shader->setUniform(_uniformCache.gamma, _gamma);
    _shader->setUniform(_uniformCache.borderWidth, borderUV);
    _shader->setUniform(_uniformCache.borderColor, _borderColor);
    _shader->setUniform(
        _uniformCache.mvp,
        global::renderEngine->scene()->camera()->viewProjectionMatrix() * modelTransform
    );

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    bindTexture();
    _shader->setUniform(_uniformCache.texture, unit);

    glBindVertexArray(rendering::helper::vertexObjects.square.vao);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    glEnable(GL_CULL_FACE);

    _shader->deactivate();
    unbindTexture();
}

void ScreenSpaceRenderable::unbindTexture() {}

glm::vec3 ScreenSpaceRenderable::sanitizeSphericalCoordinates(glm::vec3 spherical) const {
    const float r = spherical.x;
    float phi = spherical.z;

    // Sanitize coordinates.
    float theta = wrap(spherical.y, 0.f, glm::two_pi<float>());
    if (theta > glm::pi<float>()) {
        theta = glm::two_pi<float>() - theta;
        phi += glm::pi<float>();
    }

    return glm::vec3(r, theta, phi);
}

glm::vec3 ScreenSpaceRenderable::sphericalToCartesian(glm::vec3 spherical) const {
    // First convert to ISO convention spherical coordinates according to
    // https://en.wikipedia.org/wiki/Spherical_coordinate_system
    // (radius, theta, phi), where theta is the polar angle from the z axis,
    // and phi is the azimuth.

    const glm::vec3 sanitized = sanitizeSphericalCoordinates(std::move(spherical));
    const float x = sanitized[0] * sin(sanitized[1]) * cos(sanitized[2]);
    const float y = sanitized[0] * sin(sanitized[1]) * sin(sanitized[2]);
    const float z = sanitized[0] * cos(sanitized[1]);

    // Now, convert rotate the coordinate system, so that z maps to y,
    // and y maps to -z. We want the pole to be in y instead of z.
    return glm::vec3(x, -z, y);
}

glm::vec3 ScreenSpaceRenderable::cartesianToSpherical(const glm::vec3& cartesian) const {
    // Rotate cartesian coordinates.
    glm::vec3 rotated = glm::vec3(cartesian.x, cartesian.z, -cartesian.y);

    const float r = sqrt(
        pow(rotated.x, 2.f) + pow(rotated.y, 2.f) + pow(rotated.z, 2.f)
    );
    const float theta = acos(rotated.z / r);
    const float phi = atan2(rotated.y, rotated.x);
    return sanitizeSphericalCoordinates(glm::vec3(r, theta, phi));
}

// Radius, azimiuth, elevation to spherical coordinates.
glm::vec3 ScreenSpaceRenderable::raeToSpherical(glm::vec3 rae) const {
    //return rae;
    const float r = rae.x;

    // Polar angle, theta, is elevation + pi/2.
    const float theta = rae.z + glm::half_pi<float>();

    // Azimuth in ISO spherical coordiantes (phi) is angle from x,
    // as opposed to from negative y on screen.
    const float phi = rae.y - glm::half_pi<float>();

    return glm::vec3(r, theta, phi);
}

// Spherical coordinates to radius, azimuth and elevation.
glm::vec3 ScreenSpaceRenderable::sphericalToRae(glm::vec3 spherical) const {
    //return spherical;
    const float r = spherical.x;

    // Azimuth on screen is angle from negative y, as opposed to from x.
    float azimuth = spherical.z + glm::half_pi<float>();

    // Elevation is polar angle - pi/2
    float elevation = wrap(
        spherical.y - glm::half_pi<float>(),
        -glm::pi<float>(),
        glm::pi<float>()
    );

    return glm::vec3(
        r,
        wrap(azimuth, -glm::pi<float>(), glm::pi<float>()),
        wrap(elevation, -glm::pi<float>(), glm::pi<float>())
    );
}

} // namespace openspace
