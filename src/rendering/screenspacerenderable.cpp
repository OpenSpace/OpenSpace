/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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
    constexpr const float PlaneDepth = -2.f;

    constexpr const std::array<const char*, 5> UniformNames = {
        "OcclusionDepth", "Alpha", "ModelTransform", "ViewProjectionMatrix", "texture1"
    };

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Is Enabled",
        "This setting determines whether this sceen space plane will be visible or not."
    };

    constexpr openspace::properties::Property::PropertyInfo FlatScreenInfo = {
        "FlatScreen",
        "Flat Screen specification",
        "This value determines whether the location of this screen space plane will be "
        "specified in a two-dimensional Euclidean plane (if this is set to 'true') or "
        "specified in spherical coordinates. By switching this value, the correct "
        "property will be shown or hidden. The Euclidean coordinate system is useful if "
        "a regular rendering is applied, whereas the spherical coordinates are most "
        "useful in a planetarium environment."
    };

    constexpr openspace::properties::Property::PropertyInfo EuclideanPositionInfo = {
        "EuclideanPosition",
        "Euclidean coordinates",
        "This value determines the position of this screen space plane in Euclidean "
        "two-dimensional coordinates."
    };

    constexpr openspace::properties::Property::PropertyInfo SphericalPositionInfo = {
        "SphericalPosition",
        "Spherical coordinates",
        "This value determines the position of this screen space plane in a spherical "
        "coordinate system."
    };

    constexpr openspace::properties::Property::PropertyInfo DepthInfo = {
        "Depth",
        "Depth value",
        "This value determines the depth of the plane. This value does not change the "
        "apparent size of the plane, but is only used to sort the planes correctly. The "
        "plane with a lower value will be shown in front of a plane with a higher depth "
        "value."
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Scale value",
        "This value determines a scale factor for the plane. The default size of a plane "
        "is determined by the concrete instance and reflects, for example, the size of "
        "the image being displayed."
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

    glm::vec2 toEuclidean(const glm::vec2& spherical, float r) {
        const float x = r * sin(spherical[0]) * sin(spherical[1]);
        const float y = r * cos(spherical[1]);

        return glm::vec2(x, y);
    }

    glm::vec3 toSpherical(const glm::vec2& euclidean) {
        const float r = -sqrt(
            pow(euclidean[0], 2) + pow(euclidean[1], 2) + pow(PlaneDepth, 2)
        );
        const float theta = atan2(-PlaneDepth, euclidean[0]) - glm::half_pi<float>();
        const float phi = acos(euclidean[1] / r);

        return glm::vec3(theta, phi, r);
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
                FlatScreenInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                FlatScreenInfo.description
            },
            {
                EuclideanPositionInfo.identifier,
                new DoubleVector2Verifier,
                Optional::Yes,
                EuclideanPositionInfo.description
            },
            {
                SphericalPositionInfo.identifier,
                new DoubleVector2Verifier,
                Optional::Yes,
                SphericalPositionInfo.description
            },
            {
                DepthInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                DepthInfo.description
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

ScreenSpaceRenderable::ScreenSpaceRenderable(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "" })
    , _enabled(EnabledInfo, true)
    , _useFlatScreen(FlatScreenInfo, true)
    , _euclideanPosition(
        EuclideanPositionInfo,
        glm::vec2(0.f),
        glm::vec2(-4.f),
        glm::vec2(4.f)
    )
    , _sphericalPosition(
        SphericalPositionInfo,
        glm::vec2(0.f, glm::half_pi<float>()),
        glm::vec2(-glm::pi<float>()),
        glm::vec2(glm::pi<float>())
    )
    , _depth(DepthInfo, 0.f, 0.f, 1.f)
    , _scale(ScaleInfo, 0.25f, 0.f, 2.f)
    , _alpha(AlphaInfo, 1.f, 0.f, 1.f)
    , _delete(DeleteInfo)
    , _radius(PlaneDepth)
{
    if (dictionary.hasKey(KeyIdentifier)) {
        setIdentifier(dictionary.value<std::string>(KeyIdentifier));
    }

    if (dictionary.hasKey(KeyName)) {
        setGuiName(dictionary.value<std::string>(KeyName));
    }


    addProperty(_enabled);
    addProperty(_useFlatScreen);
    addProperty(_euclideanPosition);

    // Setting spherical/euclidean onchange handler
    _useFlatScreen.onChange([this]() {
        if (_useFlatScreen) {
            addProperty(_euclideanPosition);
            removeProperty(_sphericalPosition);
        }
        else {
            removeProperty(_euclideanPosition);
            addProperty(_sphericalPosition);
        }
        useEuclideanCoordinates(_useFlatScreen);
    });

    addProperty(_depth);
    addProperty(_scale);
    addProperty(_alpha);

    if (dictionary.hasKey(EnabledInfo.identifier)) {
        _enabled = dictionary.value<bool>(EnabledInfo.identifier);
    }

    if (dictionary.hasKey(FlatScreenInfo.identifier)) {
        _useFlatScreen = dictionary.value<bool>(FlatScreenInfo.identifier);
    }
    useEuclideanCoordinates(_useFlatScreen);

    if (_useFlatScreen) {
        if (dictionary.hasKey(EuclideanPositionInfo.identifier)) {
            _euclideanPosition = dictionary.value<glm::vec2>(
                EuclideanPositionInfo.identifier
            );
        }
    }
    else {
        if (dictionary.hasKey(SphericalPositionInfo.identifier)) {
            _sphericalPosition = dictionary.value<glm::vec2>(
                SphericalPositionInfo.identifier
            );
        }
    }

    if (dictionary.hasKey(ScaleInfo.identifier)) {
        _scale = static_cast<float>(dictionary.value<double>(ScaleInfo.identifier));
    }

    if (dictionary.hasKey(DepthInfo.identifier)) {
        _depth = static_cast<float>(dictionary.value<double>(DepthInfo.identifier));
    }

    if (dictionary.hasKey(AlphaInfo.identifier)) {
        _alpha = static_cast<float>(dictionary.value<double>(AlphaInfo.identifier));
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
    _originalViewportSize = global::windowDelegate.currentWindowResolution();

    createPlane();
    createShaders();

    return isReady();
}

bool ScreenSpaceRenderable::deinitialize() {
    return true;
}

bool ScreenSpaceRenderable::deinitializeGL() {
    glDeleteVertexArrays(1, &_quad);
    _quad = 0;

    glDeleteBuffers(1, &_vertexPositionBuffer);
    _vertexPositionBuffer = 0;

    if (_shader) {
        global::renderEngine.removeRenderProgram(_shader.get());
        _shader = nullptr;
    }

    return true;
}

void ScreenSpaceRenderable::render() {
    draw(rotationMatrix() * translationMatrix() * scaleMatrix());
}

bool ScreenSpaceRenderable::isReady() const {
    return _shader != nullptr;
}

void ScreenSpaceRenderable::update() {}

bool ScreenSpaceRenderable::isEnabled() const {
    return _enabled;
}

glm::vec3 ScreenSpaceRenderable::euclideanPosition() const {
    return glm::vec3(_euclideanPosition.value(), _depth.value());
}

glm::vec3 ScreenSpaceRenderable::sphericalPosition() const {
    return glm::vec3(_sphericalPosition.value(), _depth.value());
}

float ScreenSpaceRenderable::depth() const {
    return _depth;
}

void ScreenSpaceRenderable::createPlane() {
    glGenVertexArrays(1, &_quad);
    glGenBuffers(1, &_vertexPositionBuffer);

    const GLfloat data[] = {
        // x     y    s    t
        -1.f, -1.f, 0.f, 0.f,
         1.f,  1.f, 1.f, 1.f,
        -1.f,  1.f, 0.f, 1.f,
        -1.f, -1.f, 0.f, 0.f,
         1.f, -1.f, 1.f, 0.f,
         1.f,  1.f, 1.f, 1.f,
    };

    glBindVertexArray(_quad);
    glBindBuffer(GL_ARRAY_BUFFER, _vertexPositionBuffer);
    glBufferData(GL_ARRAY_BUFFER, sizeof(data), data, GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 4,
        nullptr
    );

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 4,
        reinterpret_cast<void*>(sizeof(GLfloat) * 2)
    );
}

void ScreenSpaceRenderable::useEuclideanCoordinates(bool b) {
    _useEuclideanCoordinates = b;
    if (_useEuclideanCoordinates) {
        _euclideanPosition = toEuclidean(_sphericalPosition, _radius);
    } else {
        glm::vec3 sph = toSpherical(_euclideanPosition);
        _sphericalPosition = sph;
        _radius = sph.z;
    }
}

void ScreenSpaceRenderable::createShaders() {
    ghoul::Dictionary dict = ghoul::Dictionary();

    auto res = global::windowDelegate.currentWindowResolution();
    ghoul::Dictionary rendererData = {
        { "fragmentRendererPath", "${SHADERS}/framebuffer/renderframebuffer.frag" },
        { "windowWidth" , res.x },
        { "windowHeight" , res.y }
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

    float scalingRatioX = _originalViewportSize.x / resolution.x;
    float scalingRatioY = _originalViewportSize.y / resolution.y;
    return glm::scale(
        glm::mat4(1.f),
        glm::vec3(
            _scale * scalingRatioX,
            _scale * scalingRatioY * textureRatio,
            1.f
        )
    );
}

glm::mat4 ScreenSpaceRenderable::rotationMatrix() {
    // Get the scene transform
    glm::mat4 rotation = glm::inverse(global::windowDelegate.modelMatrix());
    if (!_useEuclideanCoordinates) {
        glm::vec2 position = _sphericalPosition.value();

        rotation = glm::rotate(rotation, position.x, glm::vec3(0.f, 1.f, 0.f));
        rotation = glm::rotate(
            rotation,
            position.y - glm::half_pi<float>(),
            glm::vec3(1.f, 0.f, 0.f)
        );
    }

    return rotation;
}

glm::mat4 ScreenSpaceRenderable::translationMatrix() {
    glm::mat4 translation(1.0);
    if (!_useEuclideanCoordinates) {
        translation = glm::translate(translation, glm::vec3(0.0f, 0.0f, PlaneDepth));
    } else {
        translation = glm::translate(
            glm::mat4(1.f),
            glm::vec3(_euclideanPosition.value(), PlaneDepth)
        );
    }

    return translation;
}

void ScreenSpaceRenderable::draw(glm::mat4 modelTransform) {
    glEnable(GL_DEPTH_TEST);
    glDisable(GL_CULL_FACE);

    _shader->activate();
    _shader->setUniform(_uniformCache.occlusionDepth, 1.f - _depth);
    _shader->setUniform(_uniformCache.alpha, _alpha);
    _shader->setUniform(_uniformCache.modelTransform, modelTransform);

    _shader->setUniform(
        _uniformCache.viewProj,
        global::renderEngine.scene()->camera()->viewProjectionMatrix()
    );

    ghoul::opengl::TextureUnit unit;
    unit.activate();
    bindTexture();
    defer { unbindTexture(); };
    _shader->setUniform(_uniformCache.texture, unit);

    glBindVertexArray(_quad);
    glDrawArrays(GL_TRIANGLES, 0, 6);

    glEnable(GL_CULL_FACE);

    _shader->deactivate();
}

void ScreenSpaceRenderable::bindTexture() {}

void ScreenSpaceRenderable::unbindTexture() {}


} // namespace openspace
