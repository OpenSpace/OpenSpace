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

#include <modules/base/rendering/renderablemodel.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/distanceconversion.h>
#include <openspace/util/time.h>
#include <openspace/util/timeconversion.h>
#include <openspace/util/updatestructures.h>
#include <openspace/scene/scene.h>
#include <openspace/scene/lightsource.h>
#include <ghoul/io/model/modelgeometry.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/invariants.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <filesystem>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "RenderableModel";
    constexpr std::string_view ProgramName = "ModelProgram";

    constexpr int DefaultBlending = 0;
    constexpr int AdditiveBlending = 1;
    constexpr int PointsAndLinesBlending = 2;
    constexpr int PolygonBlending = 3;
    constexpr int ColorAddingBlending = 4;

    std::map<std::string, int> BlendingMapping = {
        { "Default", DefaultBlending },
        { "Additive", AdditiveBlending },
        { "Points and Lines", PointsAndLinesBlending },
        { "Polygon", PolygonBlending },
        { "Color Adding", ColorAddingBlending }
    };

    constexpr glm::vec4 PosBufferClearVal = glm::vec4(1e32, 1e32, 1e32, 1.f);

    constexpr std::array<const char*, 26> UniformNames = {
        "modelViewTransform", "projectionTransform", "normalTransform", "meshTransform",
        "meshNormalTransform", "ambientIntensity", "diffuseIntensity",
        "specularIntensity", "performShading", "use_forced_color", "has_texture_diffuse",
        "has_texture_normal", "has_texture_specular", "has_color_specular",
        "texture_diffuse", "texture_normal", "texture_specular", "color_diffuse",
        "color_specular", "opacity", "nLightSources", "lightDirectionsViewSpace",
        "lightIntensities", "performManualDepthTest", "gBufferDepthTexture", "resolution"
    };

    constexpr std::array<const char*, 5> UniformOpacityNames = {
        "opacity", "colorTexture", "depthTexture", "viewport", "resolution"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableAnimationInfo = {
        "EnableAnimation",
        "Enable Animation",
        "Enable or disable the animation for the model if it has any",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo AmbientIntensityInfo = {
        "AmbientIntensity",
        "Ambient Intensity",
        "A multiplier for ambient lighting",
        // @VISIBILITY(2.4)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DiffuseIntensityInfo = {
        "DiffuseIntensity",
        "Diffuse Intensity",
        "A multiplier for diffuse lighting",
        // @VISIBILITY(2.4)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SpecularIntensityInfo = {
        "SpecularIntensity",
        "Specular Intensity",
        "A multiplier for specular lighting",
        // @VISIBILITY(2.4)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ShadingInfo = {
        "PerformShading",
        "Perform Shading",
        "This value determines whether this model should be shaded by using the position "
        "of the Sun",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableFaceCullingInfo = {
        "EnableFaceCulling",
        "Enable Face Culling",
        "Enable OpenGL automatic face culling optimization",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ModelTransformInfo = {
        "ModelTransform",
        "Model Transform",
        "This value specifies the model transform that is applied to the model before "
        "all other transformations are applied",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo PivotInfo = {
        "Pivot",
        "Pivot",
        "A vector that moves the place of origin for the model",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ModelScaleInfo = {
        "ModelScale",
        "Model Scale",
        "This value specifies the scale for the model. If a value for the ModelScale was "
        "provided in the asset file, you can see and change it here. If instead a unit "
        "name was provided in the asset, this is the value that that name represents. "
        "For example 'Centimeter' becomes 0.01. For more information see "
        "http://wiki.openspaceproject.com/docs/builders/models/model-scale.html",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RotationVecInfo = {
        "RotationVector",
        "Rotation Vector",
        "Rotation Vector using degrees",
        // @VISIBILITY(3.75)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourcesInfo = {
        "LightSources",
        "Light Sources",
        "A list of light sources that this model should accept light from",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EnableDepthTestInfo = {
        "EnableDepthTest",
        "Enable Depth Test",
        "Enable Depth Testing for the Model",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BlendingOptionInfo = {
        "BlendingOption",
        "Blending Options",
        "Changes the blending function used to calculate the colors of the model with "
        "respect to the opacity",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableModel)]] Parameters {
        // The file or files that should be loaded in this RenderableModel. The file can
        // contain filesystem tokens. This specifies the model that is rendered by
        // the Renderable.
        std::filesystem::path geometryFile;

        enum class [[codegen::map(openspace::DistanceUnit)]] ScaleUnit {
            Nanometer,
            Micrometer,
            Millimeter,
            Centimeter,
            Decimeter,
            Meter,
            Kilometer,
            Thou,
            Inch,
            Foot,
            Yard,
            Chain,
            Furlong,
            Mile
        };

        // The scale of the model. For example, if the model is in centimeters
        // then <code>ModelScale = 'Centimeter'</code> or <code>ModelScale = 0.01</code>.
        // The value that this needs to be in order for the model to be in the correct
        // scale relative to the rest of OpenSpace can be tricky to find.
        // Essentially it depends on the model software that the model was created
        // with and the original intention of the modeler. For more information see
        // our wiki page for this parameter:
        // http://wiki.openspaceproject.com/docs/builders/models/model-scale.html
        std::optional<std::variant<ScaleUnit, double>> modelScale;

        // By default the given ModelScale is used to scale the model down,
        // by setting this setting to true the model is instead scaled up with the
        // given ModelScale
        std::optional<bool> invertModelScale;

        // Set if invisible parts (parts with no textures or materials) of the model
        // should be forced to render or not.
        std::optional<bool> forceRenderInvisible;

        // [[codegen::verbatim(EnableAnimationInfo.description)]]
        std::optional<bool> enableAnimation;

        // The date and time that the model animation should start.
        // In format 'YYYY MM DD hh:mm:ss'.
        std::optional<std::string> animationStartTime [[codegen::datetime()]];

        enum class [[codegen::map(openspace::TimeUnit)]] AnimationTimeUnit {
            Nanosecond,
            Microsecond,
            Millisecond,
            Second,
            Minute
        };

        // The time scale for the animation relative to seconds.
        // Ex, if animation is in milliseconds then AnimationTimeScale = 0.001 or
        // AnimationTimeScale = Millisecond, default is Second
        std::optional<std::variant<AnimationTimeUnit, float>> animationTimeScale;

        enum class AnimationMode {
            Once,
            LoopFromStart,
            LoopInfinitely,
            BounceFromStart,
            BounceInfinitely
        };

        // The mode of how the animation should be played back.
        // Default is animation is played back once at the start time.
        // For a more detailed description see:
        // http://wiki.openspaceproject.com/docs/builders/model-animation
        std::optional<AnimationMode> animationMode;

        // [[codegen::verbatim(AmbientIntensityInfo.description)]]
        std::optional<float> ambientIntensity;

        // [[codegen::verbatim(DiffuseIntensityInfo.description)]]
        std::optional<float> diffuseIntensity;

        // [[codegen::verbatim(SpecularIntensityInfo.description)]]
        std::optional<float> specularIntensity;

        // [[codegen::verbatim(ShadingInfo.description)]]
        std::optional<bool> performShading;

        // [[codegen::verbatim(EnableFaceCullingInfo.description)]]
        std::optional<bool> enableFaceCulling;

        // [[codegen::verbatim(ModelTransformInfo.description)]]
        std::optional<glm::dmat4x4> modelTransform;

        // [[codegen::verbatim(PivotInfo.description)]]
        std::optional<glm::vec3> pivot;

        // [[codegen::verbatim(RotationVecInfo.description)]]
        std::optional<glm::dvec3> rotationVector;

        // [[codegen::verbatim(LightSourcesInfo.description)]]
        std::optional<std::vector<ghoul::Dictionary>> lightSources
            [[codegen::reference("core_light_source")]];

        // [[codegen::verbatim(EnableDepthTestInfo.description)]]
        std::optional<bool> enableDepthTest;

        // [[codegen::verbatim(BlendingOptionInfo.description)]]
        std::optional<std::string> blendingOption;

        // The path to the vertex shader program that is used instead of the default
        // shader.
        std::optional<std::filesystem::path> vertexShader;

        // The path to the fragment shader program that is used instead of the default
        // shader.
        std::optional<std::filesystem::path> fragmentShader;
    };
#include "renderablemodel_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableModel::Documentation() {
    return codegen::doc<Parameters>("base_renderable_model");
}

RenderableModel::RenderableModel(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary, { .automaticallyUpdateRenderBin = false })
    , _enableAnimation(EnableAnimationInfo, false)
    , _ambientIntensity(AmbientIntensityInfo, 0.2f, 0.f, 1.f)
    , _diffuseIntensity(DiffuseIntensityInfo, 1.f, 0.f, 1.f)
    , _specularIntensity(SpecularIntensityInfo, 1.f, 0.f, 1.f)
    , _performShading(ShadingInfo, true)
    , _enableFaceCulling(EnableFaceCullingInfo, true)
    , _modelTransform(
        ModelTransformInfo,
        glm::dmat4(1.0),
        glm::dmat4(-1.0),
        glm::dmat4(1.0)
    )
    , _pivot(
        PivotInfo,
        glm::vec3(0.f),
        glm::vec3(-std::numeric_limits<float>::max()),
        glm::vec3(std::numeric_limits<float>::max())
    )
    , _modelScale(ModelScaleInfo, 1.0, std::numeric_limits<double>::epsilon(), 4e+27)
    , _rotationVec(RotationVecInfo, glm::dvec3(0.0), glm::dvec3(0.0), glm::dvec3(360.0))
    , _enableDepthTest(EnableDepthTestInfo, true)
    , _blendingFuncOption(
        BlendingOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _lightSourcePropertyOwner({ "LightSources", "Light Sources" })
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    if (p.forceRenderInvisible.has_value()) {
        _forceRenderInvisible = *p.forceRenderInvisible;

        if (!_forceRenderInvisible) {
            // Asset file have specifically said to not render invisible parts,
            // do not notify in the log if invisible parts are detected and dropped
            _notifyInvisibleDropped = false;
        }
    }

    _file = absPath(p.geometryFile.string());
    if (!std::filesystem::exists(_file)) {
        throw ghoul::RuntimeError(std::format("Cannot find model file '{}'", _file));
    }

    _invertModelScale = p.invertModelScale.value_or(_invertModelScale);

    if (p.modelScale.has_value()) {
        if (std::holds_alternative<Parameters::ScaleUnit>(*p.modelScale)) {
            const Parameters::ScaleUnit scaleUnit =
                std::get<Parameters::ScaleUnit>(*p.modelScale);
            const DistanceUnit distanceUnit = codegen::map<DistanceUnit>(scaleUnit);
            _modelScale = toMeter(distanceUnit);
        }
        else if (std::holds_alternative<double>(*p.modelScale)) {
            _modelScale = std::get<double>(*p.modelScale);
        }
        else {
            throw ghoul::MissingCaseException();
        }

        if (_invertModelScale) {
            _modelScale = 1.0 / _modelScale;
        }
    }

    _modelTransform = p.modelTransform.value_or(_modelTransform);
    _pivot = p.pivot.value_or(_pivot);
    _animationStart = p.animationStartTime.value_or(_animationStart);
    _enableAnimation = p.enableAnimation.value_or(_enableAnimation);

    if (p.animationTimeScale.has_value()) {
        if (std::holds_alternative<float>(*p.animationTimeScale)) {
            _animationTimeScale = std::get<float>(*p.animationTimeScale);
        }
        else if (std::holds_alternative<Parameters::AnimationTimeUnit>(
                *p.animationTimeScale
            ))
        {
            const Parameters::AnimationTimeUnit animationTimeUnit =
                std::get<Parameters::AnimationTimeUnit>(*p.animationTimeScale);
            const TimeUnit timeUnit = codegen::map<TimeUnit>(animationTimeUnit);

            _animationTimeScale = static_cast<double>(
                convertTime(1.0, timeUnit, TimeUnit::Second)
            );
        }
        else {
            throw ghoul::MissingCaseException();
        }
    }

    if (p.animationMode.has_value()) {
        switch (*p.animationMode) {
            case Parameters::AnimationMode::LoopFromStart:
                _animationMode = AnimationMode::LoopFromStart;
                break;
            case Parameters::AnimationMode::LoopInfinitely:
                _animationMode = AnimationMode::LoopInfinitely;
                break;
            case Parameters::AnimationMode::BounceFromStart:
                _animationMode = AnimationMode::BounceFromStart;
                break;
            case Parameters::AnimationMode::BounceInfinitely:
                _animationMode = AnimationMode::BounceInfinitely;
                break;
            case Parameters::AnimationMode::Once:
                _animationMode = AnimationMode::Once;
                break;
        }
    }

    _ambientIntensity = p.ambientIntensity.value_or(_ambientIntensity);
    _diffuseIntensity = p.diffuseIntensity.value_or(_diffuseIntensity);
    _specularIntensity = p.specularIntensity.value_or(_specularIntensity);
    _performShading = p.performShading.value_or(_performShading);
    _enableDepthTest = p.enableDepthTest.value_or(_enableDepthTest);
    _enableFaceCulling = p.enableFaceCulling.value_or(_enableFaceCulling);

    if (p.vertexShader.has_value()) {
        _vertexShaderPath = p.vertexShader->string();
    }
    if (p.fragmentShader.has_value()) {
        _fragmentShaderPath = p.fragmentShader->string();
    }

    if (p.lightSources.has_value()) {
        const std::vector<ghoul::Dictionary> lightsources = *p.lightSources;

        for (const ghoul::Dictionary& lsDictionary : lightsources) {
            std::unique_ptr<LightSource> lightSource =
                LightSource::createFromDictionary(lsDictionary);
            _lightSourcePropertyOwner.addPropertySubOwner(lightSource.get());
            _lightSources.push_back(std::move(lightSource));
        }
    }

    addProperty(_enableAnimation);
    addPropertySubOwner(_lightSourcePropertyOwner);
    addProperty(_ambientIntensity);
    addProperty(_diffuseIntensity);
    addProperty(_specularIntensity);
    addProperty(_performShading);
    addProperty(_enableFaceCulling);
    addProperty(_enableDepthTest);
    addProperty(_modelTransform);
    addProperty(_pivot);
    addProperty(_rotationVec);

    addProperty(_modelScale);
    _modelScale.setExponent(20.f);

    _modelScale.onChange([this]() {
        if (!_geometry) {
            LWARNING(std::format(
                "Cannot set scale for model '{}': not loaded yet", _file
            ));
            return;
        }

        setBoundingSphere(_geometry->boundingRadius() * _modelScale);

        // Set Interaction sphere size to be 10% of the bounding sphere
        setInteractionSphere(boundingSphere() * 0.1);
    });

    _rotationVec.onChange([this]() {
        _modelTransform = glm::mat4_cast(glm::quat(glm::radians(_rotationVec.value())));
    });

    _enableAnimation.onChange([this]() {
        if (!_modelHasAnimation) {
            LWARNING(std::format(
                "Cannot enable animation for model '{}': it does not have any", _file
            ));
        }
        else if (_enableAnimation && _animationStart.empty()) {
            LWARNING(std::format(
                "Cannot enable animation for model '{}': it does not have a start time",
                _file
            ));
            _enableAnimation = false;
        }
        else {
            if (!_geometry) {
                LWARNING(std::format(
                    "Cannot enable animation for model '{}': not loaded yet", _file
                ));
                return;
            }
            _geometry->enableAnimation(_enableAnimation);
        }
    });

    if (p.rotationVector.has_value()) {
        _rotationVec = *p.rotationVector;
    }

    _blendingFuncOption.addOption(DefaultBlending, "Default");
    _blendingFuncOption.addOption(AdditiveBlending, "Additive");
    _blendingFuncOption.addOption(PolygonBlending, "Polygon");
    _blendingFuncOption.addOption(ColorAddingBlending, "Color Adding");

    addProperty(_blendingFuncOption);

    if (p.blendingOption.has_value()) {
        const std::string blendingOpt = *p.blendingOption;
        _blendingFuncOption = BlendingMapping[blendingOpt];
    }

    _originalRenderBin = renderBin();
}

bool RenderableModel::isReady() const {
    return _program && _quadProgram;
}

void RenderableModel::initialize() {
    ZoneScoped;

    for (const std::unique_ptr<LightSource>& ls : _lightSources) {
        ls->initialize();
    }
}

void RenderableModel::initializeGL() {
    ZoneScoped;

    // Load model
    _geometry = ghoul::io::ModelReader::ref().loadModel(
        _file,
        ghoul::io::ModelReader::ForceRenderInvisible(_forceRenderInvisible),
        ghoul::io::ModelReader::NotifyInvisibleDropped(_notifyInvisibleDropped)
    );
    _modelHasAnimation = _geometry->hasAnimation();

    // @TODO (abock, 2023-06-03) Leaving this here to address issue #2731. The
    // _modelHasAnimation has not been set to true in the constructor causing the
    // `enableAnimation` function not to be called
    if (_enableAnimation) {
        _geometry->enableAnimation(true);
    }

    if (!_modelHasAnimation) {
        if (!_animationStart.empty()) {
            LWARNING(std::format(
                "Animation start time given to model '{}' without animation", _file
            ));
        }

        if (_enableAnimation) {
            LWARNING(std::format(
                "Cannot enable animation for model '{}': it does not have any", _file
            ));
            _enableAnimation = false;
        }

        _enableAnimation.setReadOnly(true);
    }
    else {
        if (_enableAnimation && _animationStart.empty()) {
            LWARNING(std::format(
                "Cannot enable animation for model '{}': it does not have a start time",
                _file
            ));
        }
        else if (!_enableAnimation) {
            LINFO(std::format(
                "Model '{}' with deactivated animation was found. The animation can be "
                "activated by entering a start time in the asset file", _file
            ));
        }

        // Set animation settings
        _geometry->setTimeScale(static_cast<float>(_animationTimeScale));
    }

    // Initialize shaders
    std::string program = std::string(ProgramName);
    if (!_vertexShaderPath.empty()) {
        program += "|vs=" + _vertexShaderPath;
    }
    if (!_fragmentShaderPath.empty()) {
        program += "|fs=" + _fragmentShaderPath;
    }
    _program = BaseModule::ProgramObjectManager.request(
        program,
        [this, program]() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            const std::filesystem::path vs =
                _vertexShaderPath.empty() ?
                absPath("${MODULE_BASE}/shaders/model_vs.glsl") :
                std::filesystem::path(_vertexShaderPath);
            const std::filesystem::path fs =
                _fragmentShaderPath.empty() ?
                absPath("${MODULE_BASE}/shaders/model_fs.glsl") :
                std::filesystem::path(_fragmentShaderPath);

            return global::renderEngine->buildRenderProgram(program, vs, fs);
        }
    );
    // We don't really know what kind of shader the user provides us with, so we can't
    // make the assumption that we are going to use all uniforms
    _program->setIgnoreUniformLocationError(
        ghoul::opengl::ProgramObject::IgnoreError::Yes
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    _quadProgram = BaseModule::ProgramObjectManager.request(
        "ModelOpacityProgram",
        [&]() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            const std::filesystem::path vs =
                absPath("${MODULE_BASE}/shaders/modelOpacity_vs.glsl");
            const std::filesystem::path fs =
                absPath("${MODULE_BASE}/shaders/modelOpacity_fs.glsl");

            return global::renderEngine->buildRenderProgram(
                "ModelOpacityProgram",
                vs,
                fs
            );
        }
    );
    ghoul::opengl::updateUniformLocations(
        *_quadProgram,
        _uniformOpacityCache,
        UniformOpacityNames
    );

    // Screen quad VAO
    constexpr std::array<GLfloat, 24> QuadVtx = {
        // x     y     s     t
        -1.f, -1.f,  0.f,  0.f,
         1.f,  1.f,  1.f,  1.f,
        -1.f,  1.f,  0.f,  1.f,
        -1.f, -1.f,  0.f,  0.f,
         1.f, -1.f,  1.f,  0.f,
         1.f,  1.f,  1.f,  1.f
    };

    glGenVertexArrays(1, &_quadVao);
    glBindVertexArray(_quadVao);

    glGenBuffers(1, &_quadVbo);
    glBindBuffer(GL_ARRAY_BUFFER, _quadVbo);

    glBufferData(GL_ARRAY_BUFFER, sizeof(QuadVtx), QuadVtx.data(), GL_STATIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), nullptr);
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        2,
        GL_FLOAT,
        GL_FALSE,
        4 * sizeof(GLfloat),
        reinterpret_cast<void*>(2 * sizeof(GLfloat))
    );

    // Generate textures and the frame buffer
    glGenFramebuffers(1, &_framebuffer);

    // Bind textures to the framebuffer
    glBindFramebuffer(GL_FRAMEBUFFER, _framebuffer);
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT0,
        global::renderEngine->renderer().additionalColorTexture1(),
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT1,
        global::renderEngine->renderer().additionalColorTexture2(),
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_COLOR_ATTACHMENT2,
        global::renderEngine->renderer().additionalColorTexture3(),
        0
    );
    glFramebufferTexture(
        GL_FRAMEBUFFER,
        GL_DEPTH_ATTACHMENT,
        global::renderEngine->renderer().additionalDepthTexture(),
        0
    );

    if (glbinding::Binding::ObjectLabel.isResolved()) {
        glObjectLabel(GL_FRAMEBUFFER, _framebuffer, -1, "RenderableModel Framebuffer");
    }

    // Check status
    const GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
    if (status != GL_FRAMEBUFFER_COMPLETE) {
        LERROR("Framebuffer is not complete");
    }
    glBindFramebuffer(GL_FRAMEBUFFER, 0);

    // Initialize geometry
    _geometry->initialize();
    _geometry->calculateBoundingRadius();
    setBoundingSphere(_geometry->boundingRadius() * _modelScale);

    // Set Interaction sphere size to be 10% of the bounding sphere
    setInteractionSphere(boundingSphere() * 0.1);
}

void RenderableModel::deinitializeGL() {
    _geometry->deinitialize();
    _geometry.reset();

    glDeleteFramebuffers(1, &_framebuffer);

    glDeleteBuffers(1, &_quadVbo);
    glDeleteVertexArrays(1, &_quadVao);

    std::string program = std::string(ProgramName);
    if (!_vertexShaderPath.empty()) {
        program += "|vs=" + _vertexShaderPath;
    }
    if (!_fragmentShaderPath.empty()) {
        program += "|fs=" + _fragmentShaderPath;
    }
    BaseModule::ProgramObjectManager.release(
        program,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );

    BaseModule::ProgramObjectManager.release(
        "ModelOpacityProgram",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );

    _program = nullptr;
    _quadProgram = nullptr;
    ghoul::opengl::FramebufferObject::deactivate();
}

void RenderableModel::render(const RenderData& data, RendererTasks&) {
    const double distanceToCamera = glm::distance(
        data.camera.positionVec3(),
        data.modelTransform.translation
    );

    // This distance will be enough to render the model as one pixel if the field of
    // view is 'fov' radians and the screen resolution is 'res' pixels.
    // Formula from RenderableGlobe
    constexpr double tfov = 0.5773502691896257;
    constexpr int res = 2880;

    // @TODO (malej 13-APR-23): This should only use the boundingSphere function once
    // that takes the gui scale into account too for all renderables
    const double maxDistance =
        res * boundingSphere() * glm::compMax(data.modelTransform.scale) / tfov;

    // Don't render if model is too far away
    if (distanceToCamera >= maxDistance) {
        return;
    }

    _program->activate();

    // Model transform and view transform needs to be in double precision
    glm::dmat4 modelTransform = calcModelTransform(data);
    modelTransform = glm::translate(modelTransform, glm::dvec3(_pivot.value()));
    modelTransform *= glm::scale(_modelTransform.value(), glm::dvec3(_modelScale));
    const glm::dmat4 modelViewTransform = calcModelViewTransform(data, modelTransform);

    int nLightSources = 0;
    _lightIntensitiesBuffer.resize(_lightSources.size());
    _lightDirectionsViewSpaceBuffer.resize(_lightSources.size());
    for (const std::unique_ptr<LightSource>& lightSource : _lightSources) {
        if (!lightSource->isEnabled()) {
            continue;
        }
        _lightIntensitiesBuffer[nLightSources] = lightSource->intensity();
        _lightDirectionsViewSpaceBuffer[nLightSources] =
            lightSource->directionViewSpace(data);

        ++nLightSources;
    }

    if (_uniformCache.performShading != -1) {
        _program->setUniform(_uniformCache.performShading, _performShading);
    }

    if (_performShading) {
        _program->setUniform(
            _uniformCache.nLightSources,
            nLightSources
        );
        _program->setUniform(
            _uniformCache.lightIntensities,
            _lightIntensitiesBuffer
        );
        _program->setUniform(
            _uniformCache.lightDirectionsViewSpace,
            _lightDirectionsViewSpaceBuffer
        );

        _program->setUniform(_uniformCache.ambientIntensity, _ambientIntensity);
        _program->setUniform(_uniformCache.diffuseIntensity, _diffuseIntensity);
        _program->setUniform(_uniformCache.specularIntensity, _specularIntensity);
    }

    _program->setUniform(
        _uniformCache.modelViewTransform,
        glm::mat4(modelViewTransform)
    );

    const glm::dmat4 normalTransform = glm::transpose(glm::inverse(modelViewTransform));

    _program->setUniform(
        _uniformCache.normalTransform,
        glm::mat4(normalTransform)
    );

    _program->setUniform(
        _uniformCache.projectionTransform,
        data.camera.projectionMatrix()
    );

    if (!_enableFaceCulling) {
        glDisable(GL_CULL_FACE);
    }

    // Configure blending
    glEnablei(GL_BLEND, 0);
    switch (_blendingFuncOption) {
        case DefaultBlending:
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
            break;
        case AdditiveBlending:
            glBlendFunc(GL_ONE, GL_ONE);
            break;
        case PolygonBlending:
            glBlendFunc(GL_SRC_ALPHA_SATURATE, GL_ONE);
            break;
        case ColorAddingBlending:
            glBlendFunc(GL_SRC_COLOR, GL_DST_COLOR);
            break;
    };

    if (!_enableDepthTest) {
        glDisable(GL_DEPTH_TEST);
    }


    if (!_shouldRenderTwice) {
        // Reset manual depth test
        _program->setUniform(
            _uniformCache.performManualDepthTest,
            false
        );

        if (hasOverrideRenderBin()) {
            // If override render bin is set then use the opacity values as normal
            _program->setUniform(_uniformCache.opacity, opacity());
        }
        else {
            // Otherwise reset to 1
            _program->setUniform(_uniformCache.opacity, 1.f);
        }

        _geometry->render(*_program);
    }
    else {
        // Prepare framebuffer
        const GLint defaultFBO = ghoul::opengl::FramebufferObject::getActiveObject();
        glBindFramebuffer(GL_FRAMEBUFFER, _framebuffer);

        // Re-bind first texture to use the currently not used Ping-Pong texture in the
        // FramebufferRenderer
        glFramebufferTexture(
            GL_FRAMEBUFFER,
            GL_COLOR_ATTACHMENT0,
            global::renderEngine->renderer().additionalColorTexture1(),
            0
        );
        // Check status
        const GLenum status = glCheckFramebufferStatus(GL_FRAMEBUFFER);
        if (status != GL_FRAMEBUFFER_COMPLETE) {
            LERROR("Framebuffer is not complete");
        }

        constexpr std::array<GLenum, 3> ColorAttachmentArray = {
           GL_COLOR_ATTACHMENT0,
           GL_COLOR_ATTACHMENT1,
           GL_COLOR_ATTACHMENT2,
        };
        glDrawBuffers(3, ColorAttachmentArray.data());
        glClearColor(0.f, 0.f, 0.f, 0.f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glClearBufferfv(GL_COLOR, 1, glm::value_ptr(PosBufferClearVal));

        // Use a manuel depth test to make the models aware of the rest of the scene
        _program->setUniform(
            _uniformCache.performManualDepthTest,
            _enableDepthTest
        );

        // Bind the G-buffer depth texture for a manual depth test towards the rest
        // of the scene
        ghoul::opengl::TextureUnit gBufferDepthTextureUnit;
        gBufferDepthTextureUnit.activate();
        glBindTexture(
            GL_TEXTURE_2D,
            global::renderEngine->renderer().gBufferDepthTexture()
        );
        _program->setUniform(
            _uniformCache.gBufferDepthTexture,
            gBufferDepthTextureUnit
        );

        // Will also need the resolution to get a texture coordinate for the G-buffer
        // depth texture
        _program->setUniform(
            _uniformCache.resolution,
            glm::vec2(global::windowDelegate->currentDrawBufferResolution())
        );

        // Make sure opacity in first pass is always 1
        _program->setUniform(_uniformCache.opacity, 1.f);

        // Render Pass 1
        // Render all parts of the model into the new framebuffer without opacity
        _geometry->render(*_program);
        _program->deactivate();

        // Render pass 2
        // Render the whole model into the G-buffer with the correct opacity
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);

        // Screen-space quad should not be discarded due to depth test,
        // but we still want to be able to write to the depth buffer -> GL_ALWAYS
        glEnable(GL_DEPTH_TEST);
        glDepthFunc(GL_ALWAYS);

        _quadProgram->activate();

        _quadProgram->setUniform(_uniformOpacityCache.opacity, opacity());

        // Bind textures
        ghoul::opengl::TextureUnit colorTextureUnit;
        colorTextureUnit.activate();
        glBindTexture(
            GL_TEXTURE_2D,
            global::renderEngine->renderer().additionalColorTexture1()
        );
        _quadProgram->setUniform(_uniformOpacityCache.colorTexture, colorTextureUnit);

        ghoul::opengl::TextureUnit depthTextureUnit;
        depthTextureUnit.activate();
        glBindTexture(
            GL_TEXTURE_2D,
            global::renderEngine->renderer().additionalDepthTexture()
        );
        _quadProgram->setUniform(_uniformOpacityCache.depthTexture, depthTextureUnit);

        // Will also need the resolution and viewport to get a texture coordinate
        _quadProgram->setUniform(
            _uniformOpacityCache.resolution,
            glm::vec2(global::windowDelegate->currentDrawBufferResolution())
        );

        std::array<GLint, 4> vp = {};
        global::renderEngine->openglStateCache().viewport(vp.data());
        glm::ivec4 viewport = glm::ivec4(vp[0], vp[1], vp[2], vp[3]);
        _quadProgram->setUniform(
            _uniformOpacityCache.viewport,
            viewport[0],
            viewport[1],
            viewport[2],
            viewport[3]
        );

        // Draw
        glBindVertexArray(_quadVao);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        _quadProgram->deactivate();
    }

    // Reset
    if (!_enableFaceCulling) {
        glEnable(GL_CULL_FACE);
    }

    if (!_enableDepthTest) {
        glEnable(GL_DEPTH_TEST);
    }

    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
    glActiveTexture(GL_TEXTURE0);
}

void RenderableModel::update(const UpdateData& data) {
    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }

    if (_quadProgram->isDirty()) {
        _quadProgram->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(
            *_quadProgram,
            _uniformOpacityCache,
            UniformOpacityNames
        );
    }

    if (!hasOverrideRenderBin()) {
        // Only render two pass if the model is in any way transparent
        const float o = opacity();
        if ((o >= 0.f && o < 1.f) || _geometry->isTransparent()) {
            setRenderBin(Renderable::RenderBin::PostDeferredTransparent);
            _shouldRenderTwice = true;
        }
        else {
            setRenderBin(_originalRenderBin);
            _shouldRenderTwice = false;
        }
    }


    if (_geometry->hasAnimation() && !_animationStart.empty()) {
        double relativeTime = 0.0;
        const double now = data.time.j2000Seconds();
        const double startTime = Time::convertTime(_animationStart);
        const double duration = _geometry->animationDuration();

        // The animation works in a time range 0 to duration where 0 in the animation is
        // the given _animationStart time in OpenSpace. The time in OpenSpace then has to
        // be converted to the animation time range, so the animation knows which
        // keyframes it should interpolate between for each frame. The conversion is
        // done in different ways depending on the animation mode.
        // Explanation: s indicates start time, / indicates animation is played once
        // forwards, \ indicates animation is played once backwards, time moves to the
        // right.
        switch (_animationMode) {
            case AnimationMode::LoopFromStart:
                // Start looping from the start time
                // s//// ...
                relativeTime = std::fmod(now - startTime, duration);
                break;
            case AnimationMode::LoopInfinitely:
                // Loop both before and after the start time where the model is
                // in the initial position at the start time. std::fmod is not a
                // true modulo function, it just calculates the remainder of the division
                // which can be negative. To make it true modulo it is bumped up to
                // positive values when it is negative
                // //s// ...
                relativeTime = std::fmod(now - startTime, duration);
                if (relativeTime < 0.0) {
                    relativeTime += duration;
                }
                break;
            case AnimationMode::BounceFromStart:
                // Bounce from the start position. Bounce means to do the animation
                // and when it ends, play the animation in reverse to make sure the model
                // goes back to its initial position before starting again. Avoids a
                // visible jump from the last position to the first position when loop
                // starts again
                // s/\/\/\/\ ...
                relativeTime =
                    duration - std::abs(fmod(now - startTime, 2 * duration) - duration);
                break;
            case AnimationMode::BounceInfinitely: {
                // Bounce both before and after the start time where the model is
                // in the initial position at the start time
                // /\/\s/\/\ ...
                double modulo = fmod(now - startTime, 2 * duration);
                if (modulo < 0.0) {
                    modulo += 2 * duration;
                }
                relativeTime = duration - std::abs(modulo - duration);
                break;
            }
            case AnimationMode::Once:
                // Play animation once starting from the start time and stay at the
                // animation's last position when animation is over
                // s/
                relativeTime = now - startTime;
                if (relativeTime > duration) {
                    relativeTime = duration;
                }
                break;
        }
        _geometry->update(relativeTime);
    }
}

}  // namespace openspace
