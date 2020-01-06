/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/scene/scene.h>
#include <openspace/util/camera.h>
#include <openspace/util/factorymanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>

namespace {
    constexpr const char* KeyType = "Type";
    constexpr const char* KeyTag = "Tag";

    constexpr const std::array<const char*, 4> UniformNames = {
        "Alpha", "ModelTransform", "ViewProjectionMatrix", "texture1"
    };

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this sceen space plane will be visible or not."
    };

    constexpr openspace::properties::Property::PropertyInfo
    UseRadiusAzimuthElevationInfo = {
        "UseRadiusAzimuthElevation",
        "Use Radius Azimuth and Elevation",
        "This value determines whether the location of this screen space plane will be "
        "specified using radius, azimuth and elevation (if this is set to 'true') or "
        "using cartesian coordinates. By switching this value, the correct property will "
        "be shown or hidden. The Cartesian coordinate system is useful if a regular "
        "rendering is applied, whereas the radius azimuth elevation are most useful in a "
        "planetarium environment."
    };

    constexpr openspace::properties::Property::PropertyInfo
    UsePerspectiveProjectionInfo = {
        "UsePerspectiveProjection",
        "Use Perspective Projection",
        "Determines whetether the z/radius values affects the size of the plane or not."
    };

    constexpr openspace::properties::Property::PropertyInfo CartesianPositionInfo = {
        "CartesianPosition",
        "Cartesian coordinates",
        "This value determines the position of this screen space plane in Cartesian "
        "three-dimensional coordinates (meters)."
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusAzimuthElevationInfo = {
        "RadiusAzimuthElevation",
        "Radius Azimuth Elevation",
        "This value determines the position of this screen space plane in a "
        "coordinate system based on radius (meters), azimuth (radians) and elevation "
        "(radians)."
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Scale value",
        "This value determines a scale factor for the plane. The default size of a plane "
        "is determined by the concrete instance and reflects, for example, the size of "
        "the image being displayed."
    };

    constexpr openspace::properties::Property::PropertyInfo LocalRotationInfo = {
        "Rotation",
        "Local rotation",
        "An euler rotation (x, y, z) to apply to the plane."
    };


    constexpr openspace::properties::Property::PropertyInfo AlphaInfo = {
        "Alpha",
        "Transparency",
        "This value determines the transparency of the screen space plane. If this value "
        "is 1, the plane is completely opaque, if this value is 0, the plane is "
        "completely transparent."
    };

    constexpr openspace::properties::Property::PropertyInfo DeleteInfo = {
        "Delete",
        "Delete",
        "If this property is triggered, this screen space plane is removed from the "
        "scene."
    };

    constexpr openspace::properties::Property::PropertyInfo FaceCameraInfo = {
        "FaceCamera",
        "Face Camera",
        "If enabled, the local rotation is applied after the plane is rotated to face "
        "the camera."
    };

    float wrap(float value, float min, float max) {
        return glm::mod(value - min, max - min) + min;
    }

    glm::vec3 sanitizeSphericalCoordinates(glm::vec3 spherical) {
        const float r = spherical.x;
        float phi = spherical.z;

        // Sanitize coordinates.
        float theta = wrap(spherical.y, 0.0, glm::two_pi<float>());
        if (theta > glm::pi<float>()) {
            theta = glm::two_pi<float>() - theta;
            phi += glm::pi<float>();
        }

        return glm::vec3(r, theta, phi);
    }


    glm::vec3 sphericalToCartesian(glm::vec3 spherical) {
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

    glm::vec3 cartesianToSpherical(const glm::vec3& cartesian) {
        // Rotate cartesian coordinates.
        glm::vec3 rotated = glm::vec3(cartesian.x, cartesian.z, -cartesian.y);

        const float r = sqrt(
            pow(rotated.x, 2.f) + pow(rotated.y, 2.f) + pow(rotated.z, 2.f)
        );
        const float theta = acos(rotated.z/r);
        const float phi = atan2(rotated.y, rotated.x);
        return sanitizeSphericalCoordinates(glm::vec3(r, theta, phi));
    }

    // Radius, azimiuth, elevation to spherical coordinates.
    glm::vec3 raeToSpherical(glm::vec3 rae) {
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
    glm::vec3 sphericalToRae(glm::vec3 spherical) {
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
} // namespace

namespace openspace {

documentation::Documentation ScreenSpaceRenderable::Documentation() {
    using namespace openspace::documentation;

    return {
        "Screenspace Renderable",
        "core_screenspacerenderable",
        {
            {
                KeyType,
                new StringAnnotationVerifier("Must name a valid Screenspace renderable"),
                Optional::No,
                "The type of the Screenspace renderable that is to be created. The "
                "available types of Screenspace renderable depend on the configuration of"
                "the application and can be written to disk on application startup into "
                "the FactoryDocumentation."
            },
            {
                KeyName,
                new StringVerifier,
                Optional::Yes,
                "Specifies the name of this screenspace renderable. This does not have "
                "to be unique to the scene, but it is recommended to be."
            },
            {
                KeyIdentifier,
                new StringVerifier,
                Optional::Yes,
                "This is the unique identifier for this screenspace renderable. It has "
                "to be unique amongst all existing screenspace nodes that already have "
                "been added to the scene. The identifier is not allowed to have any "
                "whitespace or '.' and must not be empty."
            },
            {
                EnabledInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                EnabledInfo.description
            },
            {
                UseRadiusAzimuthElevationInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                UseRadiusAzimuthElevationInfo.description
            },
            {
                FaceCameraInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                FaceCameraInfo.description
            },
            {
                CartesianPositionInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                CartesianPositionInfo.description
            },
            {
                RadiusAzimuthElevationInfo.identifier,
                new DoubleVector3Verifier,
                Optional::Yes,
                RadiusAzimuthElevationInfo.description
            },
            {
                ScaleInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                ScaleInfo.description
            },
            {
                AlphaInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                AlphaInfo.description
            },
            {
                KeyTag,
                new OrVerifier({ new StringVerifier, new StringListVerifier }),
                Optional::Yes,
                "Defines either a single or multiple tags that apply to this "
                "ScreenSpaceRenderable, thus making it possible to address multiple, "
                "seprate Renderables with a single property change."
            }
        }
    };
}

std::unique_ptr<ScreenSpaceRenderable> ScreenSpaceRenderable::createFromDictionary(
                                                      const ghoul::Dictionary& dictionary)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "ScreenSpaceRenderable"
    );

    const std::string& renderableType = dictionary.value<std::string>(KeyType);
    return FactoryManager::ref().factory<ScreenSpaceRenderable>()->create(
        renderableType,
        dictionary
    );
}

std::string ScreenSpaceRenderable::makeUniqueIdentifier(std::string name) {
    std::vector<ScreenSpaceRenderable*> r = global::renderEngine.screenSpaceRenderables();

    auto nameTaken = [&r](const std::string& name) {
        bool nameTaken = std::any_of(
            r.begin(),
            r.end(),
            [&name](ScreenSpaceRenderable* r) { return r->identifier() == name; }
        );
        return nameTaken;
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
    , _scale(ScaleInfo, 0.25f, 0.f, 2.f)
    , _alpha(AlphaInfo, 1.f, 0.f, 1.f)
    , _delete(DeleteInfo)
{
    if (dictionary.hasKey(KeyIdentifier)) {
        setIdentifier(dictionary.value<std::string>(KeyIdentifier));
    }

    if (dictionary.hasKey(KeyName)) {
        setGuiName(dictionary.value<std::string>(KeyName));
    }


    addProperty(_enabled);
    addProperty(_useRadiusAzimuthElevation);
    addProperty(_usePerspectiveProjection);
    addProperty(_faceCamera);
    addProperty(_cartesianPosition);
    addProperty(_raePosition);

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
    addProperty(_alpha);
    addProperty(_localRotation);

    if (dictionary.hasKey(EnabledInfo.identifier)) {
        _enabled = dictionary.value<bool>(EnabledInfo.identifier);
    }

    if (dictionary.hasKey(UseRadiusAzimuthElevationInfo.identifier)) {
        _useRadiusAzimuthElevation = dictionary.value<bool>(
            UseRadiusAzimuthElevationInfo.identifier
        );
    }

    if (_useRadiusAzimuthElevation) {
        if (dictionary.hasKey(RadiusAzimuthElevationInfo.identifier)) {
            _raePosition = dictionary.value<glm::vec3>(
                RadiusAzimuthElevationInfo.identifier
            );
        }
    }
    else {
        if (dictionary.hasKey(CartesianPositionInfo.identifier)) {
            _cartesianPosition = dictionary.value<glm::vec3>(
                CartesianPositionInfo.identifier
            );
        }
    }

    if (dictionary.hasKey(ScaleInfo.identifier)) {
        _scale = static_cast<float>(dictionary.value<double>(ScaleInfo.identifier));
    }

    if (dictionary.hasKey(AlphaInfo.identifier)) {
        _alpha = static_cast<float>(dictionary.value<double>(AlphaInfo.identifier));
    }

    if (dictionary.hasKey(UsePerspectiveProjectionInfo.identifier)) {
        _usePerspectiveProjection = static_cast<bool>(
            dictionary.value<bool>(UsePerspectiveProjectionInfo.identifier)
        );
    }

    if (dictionary.hasKey(FaceCameraInfo.identifier)) {
        _faceCamera = static_cast<bool>(
            dictionary.value<bool>(FaceCameraInfo.identifier)
        );
    }

    if (dictionary.hasKeyAndValue<std::string>(KeyTag)) {
        std::string tagName = dictionary.value<std::string>(KeyTag);
        if (!tagName.empty()) {
            addTag(std::move(tagName));
        }
    } else if (dictionary.hasKeyAndValue<ghoul::Dictionary>(KeyTag)) {
        const ghoul::Dictionary& tagNames = dictionary.value<ghoul::Dictionary>(KeyTag);
        const std::vector<std::string>& keys = tagNames.keys();
        for (const std::string& key : keys) {
            std::string tagName = tagNames.value<std::string>(key);
            if (!tagName.empty()) {
                addTag(std::move(tagName));
            }
        }
    }

    _delete.onChange([this](){
        std::string script =
            "openspace.removeScreenSpaceRenderable('" + identifier() + "');";
        global::scriptEngine.queueScript(
            script,
            scripting::ScriptEngine::RemoteScripting::No
        );
    });
    addProperty(_delete);
}

ScreenSpaceRenderable::~ScreenSpaceRenderable() {} // NOLINT

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
        global::renderEngine.removeRenderProgram(_shader.get());
        _shader = nullptr;
    }

    return true;
}

void ScreenSpaceRenderable::render() {
    draw(
        globalRotationMatrix() *
        translationMatrix() *
        localRotationMatrix() *
        scaleMatrix()
    );
}

bool ScreenSpaceRenderable::isReady() const {
    return _shader != nullptr;
}

void ScreenSpaceRenderable::update() {}

bool ScreenSpaceRenderable::isEnabled() const {
    return _enabled;
}

float ScreenSpaceRenderable::depth() {
    return _useRadiusAzimuthElevation ?
        _raePosition.value().x :
        cartesianToSpherical(_cartesianPosition).x;
}

void ScreenSpaceRenderable::createShaders() {
    ghoul::Dictionary dict = ghoul::Dictionary();

    auto res = global::windowDelegate.currentWindowResolution();
    ghoul::Dictionary rendererData = {
        { "fragmentRendererPath", "${SHADERS}/framebuffer/renderframebuffer.frag" },
        { "windowWidth" , res.x },
        { "windowHeight" , res.y },
        { "hdrExposure", global::renderEngine.hdrExposure() },
        { "disableHDR", global::renderEngine.isHdrDisabled() }
    };

    dict.setValue("rendererData", rendererData);
    dict.setValue("fragmentPath", "${MODULE_BASE}/shaders/screenspace_fs.glsl");
    _shader = ghoul::opengl::ProgramObject::Build(
        "ScreenSpaceProgram",
        absPath("${MODULE_BASE}/shaders/screenspace_vs.glsl"),
        absPath("${SHADERS}/render.frag"),
        dict
    );

    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
}

glm::mat4 ScreenSpaceRenderable::scaleMatrix() {
    glm::vec2 resolution = global::windowDelegate.currentWindowResolution();

    //to scale the plane
    float textureRatio =
        static_cast<float>(_objectSize.y) / static_cast<float>(_objectSize.x);

    glm::mat4 scale = glm::scale(
        glm::mat4(1.f),
        glm::vec3(_scale, _scale * textureRatio, 1.f)
    );

    // Simulate orthographic projection by distance to plane.
    if (!_usePerspectiveProjection) {
        float distance = _useRadiusAzimuthElevation ?
            _raePosition.value().x :
            -_cartesianPosition.value().z;
        scale = glm::scale(scale, glm::vec3(distance));
    }

    return scale;
}

glm::mat4 ScreenSpaceRenderable::globalRotationMatrix() {
    // We do not want the screen space planes to be affected by
    // 1) The global rotation of the view applied in the render engine
    // 2) sgct's scene matrix (also called model matrix by sgct)

    glm::mat4 inverseRotation = glm::inverse(
        global::renderEngine.globalRotation() *
        global::windowDelegate.modelMatrix()
    );

    // The rotation of all screen space renderables is adjustable in the render engine:
    return global::renderEngine.screenSpaceRotation() * inverseRotation;
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

glm::mat4 ScreenSpaceRenderable::translationMatrix() {
    glm::vec3 translation = _useRadiusAzimuthElevation ?
        sphericalToCartesian(raeToSpherical(_raePosition)) :
        _cartesianPosition;

    return glm::translate(glm::mat4(1.f), translation);
}

void ScreenSpaceRenderable::draw(glm::mat4 modelTransform) {
    glDisable(GL_CULL_FACE);

    _shader->activate();

    _shader->setUniform(_uniformCache.alpha, _alpha);
    _shader->setUniform(_uniformCache.modelTransform, modelTransform);

    _shader->setUniform(
        _uniformCache.viewProj,
        global::renderEngine.scene()->camera()->viewProjectionMatrix()
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

void ScreenSpaceRenderable::unbindTexture() {
}


} // namespace openspace
