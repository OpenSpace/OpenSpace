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

#include <modules/galaxy/rendering/renderablegalaxy.h>

#include <modules/galaxy/rendering/galaxyraycaster.h>
#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumereader.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/boxgeometry.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/format.h>
#include <ghoul/glm.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/stringhelper.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtx/component_wise.hpp>
#include <filesystem>
#include <fstream>
#include <optional>

namespace {
    constexpr int8_t CurrentCacheVersion = 1;

    constexpr std::string_view _loggerCat = "RenderableGalaxy";

    enum StarRenderingMethod {
        Points,
        Billboards
    };

    constexpr std::array<const char*, 4> UniformNamesPoints = {
        "modelMatrix", "viewProjectionMatrix", "eyePosition",
        "opacityCoefficient"
    };

    constexpr std::array<const char*, 5> UniformNamesBillboards = {
        "modelMatrix", "viewProjectionMatrix",
        "cameraUp", "eyePosition", "psfTexture"
    };

    constexpr openspace::properties::Property::PropertyInfo VolumeRenderingEnabledInfo = {
        "VolumeRenderingEnabled",
        "Volume Rendering",
        "If this value is enabled, the volume rendering component of the galaxy "
        "rendering is turned on. Otherwise, the volume rendering is skipped",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo StarRenderingEnabledInfo = {
        "StarRenderingEnabled",
        "Star Rendering",
        "If this value is enabled, the point-based star rendering component of the "
        "galaxy rendering is turned on. Otherwise, the volume rendering is skipped",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Step Size",
        "Determines the distance between steps taken in the volume rendering. The lower "
        "the number is, the better the rendering looks, but also takes more "
        "computational resources to render",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AbsorptionMultiplyInfo = {
        "AbsorptionMultiply",
        "Absorption Multiplier",
        "A unit-less scale factor for the probability of dust absorbing a light "
        "particle. The amount of absorption determines the spectrum of the light that is "
        "emitted from the galaxy",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EmissionMultiplyInfo = {
        "EmissionMultiply",
        "Emission Multiplier",
        "A unit-less scale factor for the amount of light being emitted by dust in the "
        "galaxy",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RotationInfo = {
        "Rotation",
        "Euler rotation",
        "The internal rotation of the volume rendering in Euler angles",
        openspace::properties::Property::Visibility::Developer
    };

    constexpr openspace::properties::Property::PropertyInfo StarRenderingMethodInfo = {
        "StarRenderingMethod",
        "Star Rendering Method",
        "This value determines which rendering method is used for visualization of the "
        "stars",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EnabledPointsRatioInfo = {
        "EnabledPointsRatio",
        "Enabled points",
        "The ratio of point-like stars that are rendered to produce the overall galaxy "
        "image. At a value of 0, no stars are rendered, at a value of 1 all points "
        "contained in the dataset are rendered. The specific value chosen is a "
        "compromise between image fidelity and rendering performance",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DownscaleVolumeRenderingInfo =
    {
        "Downscale",
        "Downscale Factor Volume Rendering",
        "This value sets the downscaling factor when rendering the current volume",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo NumberOfRayCastingStepsInfo =
    {
        "Steps",
        "Number of RayCasting Steps",
        "This value set the number of integration steps during the raycasting procedure",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableGalaxy)]] Parameters {
        // [[codegen::verbatim(VolumeRenderingEnabledInfo.description)]]
        std::optional<bool> volumeRenderingEnabled;

        // [[codegen::verbatim(StarRenderingEnabledInfo.description)]]
        std::optional<bool> starRenderingEnabled;

        // [[codegen::verbatim(StepSizeInfo.description)]]
        std::optional<float> stepSizeInfo;

        // [[codegen::verbatim(AbsorptionMultiplyInfo.description)]]
        std::optional<float> absorptionMultiply;

        // [[codegen::verbatim(EmissionMultiplyInfo.description)]]
        std::optional<float> emissionMultiply;

        // If this value is specified, the default raycasting shader is overwritten and
        // the shader found at the provided location is used instead
        std::optional<std::filesystem::path> raycastingShader;

        enum class [[codegen::map(StarRenderingMethod)]] StarRenderingMethod {
            Points,
            Billboards
        };
        // [[codegen::verbatim(StarRenderingMethodInfo.description)]]
        std::optional<StarRenderingMethod> starRenderingMethod;

        // [[codegen::verbatim(RotationInfo.description)]]
        std::optional<glm::vec3> rotation;

        struct Volume {
            std::filesystem::path filename;
            glm::ivec3 dimensions;
            glm::vec3 size;

            // [[codegen::verbatim(NumberOfRayCastingStepsInfo.description)]]
            std::optional<float> steps;

            // [[codegen::verbatim(DownscaleVolumeRenderingInfo.description)]]
            std::optional<float> downscale;
        };
        Volume volume;

        struct Points {
            std::filesystem::path filename;
            std::filesystem::path texture;

            // [[codegen::verbatim(EnabledPointsRatioInfo.description)]]
            std::optional<float> enabledPointsRatio;
        };
        Points points;
    };
#include "renderablegalaxy_codegen.cpp"


    void saveCachedFile(const std::filesystem::path& file,
                        const std::vector<glm::vec3>& positions,
                        const std::vector<glm::vec3>& colors, int64_t nPoints,
                        float pointsRatio)
    {
        std::ofstream fileStream(file, std::ofstream::binary);

        if (!fileStream.good()) {
            LERROR(std::format("Error opening file '{}' for save cache file", file));
            return;
        }

        fileStream.write(
            reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t)
        );
        fileStream.write(reinterpret_cast<const char*>(&nPoints), sizeof(int64_t));
        fileStream.write(reinterpret_cast<const char*>(&pointsRatio), sizeof(float));
        uint64_t nPositions = positions.size();
        fileStream.write(reinterpret_cast<const char*>(&nPositions), sizeof(uint64_t));
        fileStream.write(
            reinterpret_cast<const char*>(positions.data()),
            positions.size() * sizeof(glm::vec3)
        );
        uint64_t nColors = colors.size();
        fileStream.write(reinterpret_cast<const char*>(&nColors), sizeof(uint64_t));
        fileStream.write(
            reinterpret_cast<const char*>(colors.data()),
            colors.size() * sizeof(glm::vec3)
        );
    }

    float safeLength(const glm::vec3& vector) {
        const float maxComponent = std::max(
            std::max(std::abs(vector.x), std::abs(vector.y)), std::abs(vector.z)
        );
        return glm::length(vector / maxComponent) * maxComponent;
    }
} // namespace

namespace openspace {

documentation::Documentation RenderableGalaxy::Documentation() {
    return codegen::doc<Parameters>("galaxy_renderablegalaxy");
}

RenderableGalaxy::RenderableGalaxy(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _volumeRenderingEnabled(VolumeRenderingEnabledInfo, true)
    , _starRenderingEnabled(StarRenderingEnabledInfo, true)
    , _stepSize(StepSizeInfo, 0.01f, 0.001f, 0.05f, 0.001f)
    , _absorptionMultiply(AbsorptionMultiplyInfo, 40.f, 0.f, 200.f)
    , _emissionMultiply(EmissionMultiplyInfo, 200.f, 0.f, 1000.f)
    , _starRenderingMethod(
        StarRenderingMethodInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _enabledPointsRatio(EnabledPointsRatioInfo, 0.5f, 0.01f, 1.f)
    , _rotation(
        RotationInfo,
        glm::vec3(0.f),
        glm::vec3(0.f),
        glm::vec3(glm::two_pi<float>())
    )
    , _downScaleVolumeRendering(DownscaleVolumeRenderingInfo, 1.f, 0.1f, 1.f)
    , _numberOfRayCastingSteps(NumberOfRayCastingStepsInfo, 1000.f, 1.f, 1000.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _volumeRenderingEnabled = p.volumeRenderingEnabled.value_or(_volumeRenderingEnabled);
    _starRenderingEnabled = p.starRenderingEnabled.value_or(_starRenderingEnabled);
    _volumeRenderingEnabled = p.volumeRenderingEnabled.value_or(_volumeRenderingEnabled);
    _stepSize = p.stepSizeInfo.value_or(_stepSize);
    _absorptionMultiply = p.absorptionMultiply.value_or(_absorptionMultiply);
    _emissionMultiply = p.emissionMultiply.value_or(_emissionMultiply);
    _raycastingShader = p.raycastingShader.value_or(_raycastingShader);

    _starRenderingMethod.addOptions({
        { StarRenderingMethod::Points, "Points" },
        { StarRenderingMethod::Billboards, "Billboards" }
    });
    if (p.starRenderingMethod.has_value()) {
        _starRenderingMethod = codegen::map<StarRenderingMethod>(*p.starRenderingMethod);
    }

    _rotation = p.rotation.value_or(_rotation);

    _volumeFilename = p.volume.filename;
    _volumeDimensions = p.volume.dimensions;
    _volumeSize = p.volume.size;
    _numberOfRayCastingSteps = p.volume.steps.value_or(_numberOfRayCastingSteps);
    _downScaleVolumeRendering = p.volume.downscale.value_or(_downScaleVolumeRendering);

    _pointsFilename = p.points.filename;
    _enabledPointsRatio = p.points.enabledPointsRatio.value_or(_enabledPointsRatio);
    _pointSpreadFunctionTexturePath = p.points.texture;
    _pointSpreadFunctionFile = std::make_unique<ghoul::filesystem::File>(
        _pointSpreadFunctionTexturePath
    );

    auto onChange = [this](bool enabled) {
        if (enabled) {
            global::raycasterManager->attachRaycaster(*_raycaster);
        }
        else {
            global::raycasterManager->detachRaycaster(*_raycaster);
        }
    };

    onEnabledChange(onChange);

    addProperty(_volumeRenderingEnabled);
    addProperty(_starRenderingEnabled);
    addProperty(_stepSize);
    addProperty(_absorptionMultiply);
    addProperty(_emissionMultiply);
    addProperty(_starRenderingMethod);
    addProperty(_enabledPointsRatio);
    addProperty(_rotation);
    _downScaleVolumeRendering.setVisibility(properties::Property::Visibility::Developer);
    addProperty(_downScaleVolumeRendering);
    addProperty(_numberOfRayCastingSteps);

    // Use max component instead of length, to avoid problems with taking square
    // of huge value
    setBoundingSphere(glm::compMax(0.5f * _volumeSize));
}

void RenderableGalaxy::initialize() {
    ZoneScoped;

    // Aspect is currently hardcoded to cubic voxels
    const glm::vec3 d = _volumeDimensions;
    _aspect = d / glm::compMax(d);

    // The volume
    volume::RawVolumeReader<glm::tvec4<GLubyte>> reader(
        _volumeFilename,
        _volumeDimensions
    );
    _volume = reader.read();

    std::filesystem::path cachedPointsFile = FileSys.cacheManager()->cachedFilename(
        _pointsFilename
    );
    const bool hasCachedFile = std::filesystem::is_regular_file(cachedPointsFile);
    if (hasCachedFile) {
        LINFO(std::format("Cached file '{}' used for galaxy point file '{}'",
            cachedPointsFile, _pointsFilename
        ));

        Result res = loadCachedFile(cachedPointsFile);
        if (res.success) {
            _pointPositionsCache = std::move(res.positions);
            _pointColorsCache = std::move(res.color);
        }
        else {
            FileSys.cacheManager()->removeCacheFile(_pointsFilename);
            Result resPoint = loadPointFile();
            _pointPositionsCache = std::move(resPoint.positions);
            _pointColorsCache = std::move(resPoint.color);
            saveCachedFile(
                cachedPointsFile,
                _pointPositionsCache,
                _pointColorsCache,
                _nPoints,
                _enabledPointsRatio
            );
        }
    }
    else {
        Result res = loadPointFile();
        ghoul_assert(res.success, "Point file loading failed");
        _pointPositionsCache = std::move(res.positions);
        _pointColorsCache = std::move(res.color);
        saveCachedFile(
            cachedPointsFile,
            _pointPositionsCache,
            _pointColorsCache,
            _nPoints,
            _enabledPointsRatio
        );
    }
}

void RenderableGalaxy::initializeGL() {
    ZoneScoped;

    _texture = std::make_unique<ghoul::opengl::Texture>(
        _volumeDimensions,
        GL_TEXTURE_3D,
        ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge,
        ghoul::opengl::Texture::AllocateData::No,
        ghoul::opengl::Texture::TakeOwnership::No
    );

    _texture->setPixelData(
        reinterpret_cast<char*>(_volume->data()),
        ghoul::opengl::Texture::TakeOwnership::No
    );

    _texture->setDimensions(_volume->dimensions());
    _texture->uploadTexture();

    if (_raycastingShader.empty()) {
        _raycaster = std::make_unique<GalaxyRaycaster>(*_texture);
    }
    else {
        _raycaster = std::make_unique<GalaxyRaycaster>(*_texture, _raycastingShader);
    }
    _raycaster->initialize();

    // We no longer need the data
    _volume = nullptr;

    global::raycasterManager->attachRaycaster(*_raycaster);

    // initialize points.
    if (_pointsFilename.empty()) {
        return;
    }

    _pointsProgram = global::renderEngine->buildRenderProgram(
        "Galaxy points",
        absPath("${MODULE_GALAXY}/shaders/points_vs.glsl"),
        absPath("${MODULE_GALAXY}/shaders/points_fs.glsl")
    );
    _billboardsProgram = global::renderEngine->buildRenderProgram(
        "Galaxy billboard",
        absPath("${MODULE_GALAXY}/shaders/billboard_vs.glsl"),
        absPath("${MODULE_GALAXY}/shaders/billboard_fs.glsl"),
        absPath("${MODULE_GALAXY}/shaders/billboard_ge.glsl")
    );

    if (!_pointSpreadFunctionTexturePath.empty()) {
        _pointSpreadFunctionTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(_pointSpreadFunctionTexturePath),
            2
        );

        if (_pointSpreadFunctionTexture) {
            LDEBUG(std::format(
                "Loaded texture from '{}'", absPath(_pointSpreadFunctionTexturePath)
            ));
            _pointSpreadFunctionTexture->uploadTexture();
        }
        _pointSpreadFunctionTexture->setFilter(
            ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
        );

        _pointSpreadFunctionFile = std::make_unique<ghoul::filesystem::File>(
            _pointSpreadFunctionTexturePath
        );
    }

    ghoul::opengl::updateUniformLocations(
        *_pointsProgram,
        _uniformCachePoints,
        UniformNamesPoints
    );
    ghoul::opengl::updateUniformLocations(
        *_billboardsProgram,
        _uniformCacheBillboards,
        UniformNamesBillboards
    );

    glGenVertexArrays(1, &_pointsVao);
    glGenBuffers(1, &_positionVbo);
    glGenBuffers(1, &_colorVbo);

    glBindVertexArray(_pointsVao);
    glBindBuffer(GL_ARRAY_BUFFER, _positionVbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _pointPositionsCache.size() * sizeof(glm::vec3),
        _pointPositionsCache.data(),
        GL_STATIC_DRAW
    );
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 0, nullptr);
    _pointPositionsCache.clear();

    glBindBuffer(GL_ARRAY_BUFFER, _colorVbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _pointColorsCache.size() * sizeof(glm::vec3),
        _pointColorsCache.data(),
        GL_STATIC_DRAW
    );
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 0, nullptr);
    _pointColorsCache.clear();

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableGalaxy::deinitializeGL() {
    if (_raycaster) {
        global::raycasterManager->detachRaycaster(*_raycaster);
        _raycaster = nullptr;
    }

    global::renderEngine->removeRenderProgram(_pointsProgram.get());
    _pointsProgram = nullptr;
    global::renderEngine->removeRenderProgram(_billboardsProgram.get());
    _billboardsProgram = nullptr;

    glDeleteVertexArrays(1, &_pointsVao);
    glDeleteBuffers(1, &_positionVbo);
    glDeleteBuffers(1, &_colorVbo);
}

bool RenderableGalaxy::isReady() const {
    return true;
}

void RenderableGalaxy::update(const UpdateData& data) {
    if (!_raycaster) {
        return;
    }
    const glm::vec3 eulerRotation = static_cast<glm::vec3>(_rotation);
    glm::mat4 transform = glm::rotate(
        glm::mat4(1.f),
        eulerRotation.x,
        glm::vec3(1.f, 0.f, 0.f)
    );
    transform = glm::rotate(transform, eulerRotation.y, glm::vec3(0.f, 1.f, 0.f));
    transform = glm::rotate(transform, eulerRotation.z,  glm::vec3(0.f, 0.f, 1.f));

    const glm::mat4 volumeTransform = glm::scale(transform, _volumeSize);
    _pointTransform = transform;

    _raycaster->setDownscaleRender(_downScaleVolumeRendering);
    _raycaster->setMaxSteps(static_cast<int>(_numberOfRayCastingSteps));
    _raycaster->setStepSize(_stepSize);
    _raycaster->setAspect(_aspect);
    _raycaster->setModelTransform(volumeTransform);
    _raycaster->setAbsorptionMultiplier(_absorptionMultiply);
    _raycaster->setEmissionMultiplier(_emissionMultiply);
    _raycaster->setTime(data.time.j2000Seconds());
}

void RenderableGalaxy::render(const RenderData& data, RendererTasks& tasks) {
    // Render the volume
    if (_raycaster && _volumeRenderingEnabled) {
        const RaycasterTask task { _raycaster.get(), data };

        const glm::vec3 position = data.camera.positionVec3();
        const float length = safeLength(position);
        const glm::vec3 galaxySize = _volumeSize;

        const float maxDim = glm::compMax(galaxySize);

        const float lowerRampStart = maxDim * 0.01f;
        const float lowerRampEnd = maxDim * 0.1f;

        const float upperRampStart = maxDim * 2.f;
        const float upperRampEnd = maxDim * 10.f;

        float opacityCoefficient = 1.f;
        if (length < lowerRampStart) {
            opacityCoefficient = 0.f; // camera really close
        }
        else if (length < lowerRampEnd) {
            opacityCoefficient = (length - lowerRampStart) /
                                 (lowerRampEnd - lowerRampStart);
        }
        else if (length < upperRampStart) {
            opacityCoefficient = 1.f; // sweet spot (max)
        }
        else if (length < upperRampEnd) {
            opacityCoefficient = 1.f - (length - upperRampStart) /
                                 (upperRampEnd - upperRampStart); // fade out
        }
        else {
            opacityCoefficient = 0;
        }

        _opacityCoefficient = opacityCoefficient;
        ghoul_assert(
            _opacityCoefficient >= 0.f && _opacityCoefficient <= 1.f,
            "Opacity coefficient was not between 0 and 1"
        );
        if (opacityCoefficient > 0) {
            _raycaster->setOpacityCoefficient(_opacityCoefficient);
            tasks.raycasterTasks.push_back(task);
        }
    }

    if (!_starRenderingEnabled || _opacityCoefficient <= 0.f) {
        return;
    }

    if (_starRenderingMethod == 1) {
        if (_billboardsProgram) {
            renderBillboards(data);
        }
    }
    else {
        if (_pointsProgram) {
            renderPoints(data);
        }
    }
}

void RenderableGalaxy::renderPoints(const RenderData& data) {
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    glDisable(GL_DEPTH_TEST);

    _pointsProgram->activate();

    const glm::dmat4 rotMatrix = glm::rotate(
        glm::dmat4(1.0),
        glm::pi<double>(),
        glm::dvec3(1.0, 0.0, 0.0)) *
            glm::rotate(glm::dmat4(1.0), 3.1248, glm::dvec3(0.0, 1.0, 0.0)) *
            glm::rotate(glm::dmat4(1.0), 4.45741, glm::dvec3(0.0, 0.0, 1.0)
    );

    const AlternativeTransform altTransform = {
        .rotation = glm::dmat4(data.modelTransform.rotation) * rotMatrix
    };
    const glm::dmat4 modelTransform = calcModelTransform(data, altTransform);

    const glm::dmat4 cameraViewProjectionMatrix =
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix();

    _pointsProgram->setUniform(_uniformCachePoints.modelMatrix, modelTransform);
    _pointsProgram->setUniform(
        _uniformCachePoints.cameraViewProjectionMatrix,
        cameraViewProjectionMatrix
    );

    const glm::dvec3 eyePosition = glm::dvec3(
        glm::inverse(data.camera.combinedViewMatrix()) *
        glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );
    _pointsProgram->setUniform(_uniformCachePoints.eyePosition, eyePosition);
    _pointsProgram->setUniform(
        _uniformCachePoints.opacityCoefficient,
        _opacityCoefficient
    );

    glBindVertexArray(_pointsVao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_nPoints * _enabledPointsRatio));
    glBindVertexArray(0);

    _pointsProgram->deactivate();

    // Restores OpenGL Rendering State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderableGalaxy::renderBillboards(const RenderData& data) {
    // Change OpenGL Blending and Depth states
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    glDisable(GL_DEPTH_TEST);

    _billboardsProgram->activate();

    const glm::dmat4 rotMatrix = glm::rotate(
        glm::dmat4(1.0),
        glm::pi<double>(),
        glm::dvec3(1.0, 0.0, 0.0)) *
            glm::rotate(glm::dmat4(1.0), 3.1248, glm::dvec3(0.0, 1.0, 0.0)) *
            glm::rotate(glm::dmat4(1.0), 4.45741, glm::dvec3(0.0, 0.0, 1.0)
    );

    const AlternativeTransform altTransform = {
        .rotation = glm::dmat4(data.modelTransform.rotation) * rotMatrix
    };
    const glm::dmat4 modelTransform = calcModelTransform(data, altTransform);

    const glm::dmat4 cameraViewProjectionMatrix =
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix();

    _billboardsProgram->setUniform(_uniformCacheBillboards.modelMatrix, modelTransform);
    _billboardsProgram->setUniform(
        _uniformCacheBillboards.cameraViewProjectionMatrix,
        cameraViewProjectionMatrix
    );

    const glm::dvec3 eyePosition = glm::dvec3(
        glm::inverse(data.camera.combinedViewMatrix()) *
        glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );
    _billboardsProgram->setUniform(_uniformCacheBillboards.eyePosition, eyePosition);

    const glm::dvec3 cameraUp = data.camera.lookUpVectorWorldSpace();
    _billboardsProgram->setUniform(_uniformCacheBillboards.cameraUp, cameraUp);

    ghoul::opengl::TextureUnit psfUnit;
    psfUnit.activate();
    _pointSpreadFunctionTexture->bind();
    _billboardsProgram->setUniform(_uniformCacheBillboards.psfTexture, psfUnit);

    glBindVertexArray(_pointsVao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_nPoints * _enabledPointsRatio));
    glBindVertexArray(0);

    _billboardsProgram->deactivate();

    // Restores OpenGL Rendering State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

RenderableGalaxy::Result RenderableGalaxy::loadPointFile() {
    std::ifstream pointFile = std::ifstream(_pointsFilename, std::ios::in);

    // Read header for OFF (Object File Format)
    std::string line;
    ghoul::getline(pointFile, line);

    // Read point count
    ghoul::getline(pointFile, line);
    std::istringstream iss(line);
    int64_t nPoints = 0;
    iss >> nPoints;

    // Prepare point reading
    _nPoints = static_cast<size_t>(nPoints);

    std::vector<glm::vec3> pointPositions;
    std::vector<glm::vec3> pointColors;
    // Read points
    for (size_t i = 0;
        i < static_cast<size_t>(_nPoints * _enabledPointsRatio.maxValue()) + 1;
        i++)
    {
        float x = 0.f;
        float y = 0.f;
        float z = 0.f;
        float r = 0.f;
        float g = 0.f;
        float b = 0.f;
        float a = 0.f;
        ghoul::getline(pointFile, line);
        std::istringstream issp(line);
        issp >> x >> y >> z >> r >> g >> b >> a;

        // Convert kiloparsec to meters
        glm::vec3 position = glm::vec3(x, y, z);
        position *= (distanceconstants::Parsec * 100);

        pointPositions.emplace_back(position);
        pointColors.emplace_back(r, g, b);
    }

    Result res;
    res.success = true;
    res.positions = std::move(pointPositions);
    res.color = std::move(pointColors);
    return res;
}

RenderableGalaxy::Result RenderableGalaxy::loadCachedFile(
                                                        const std::filesystem::path& file)
{
    ZoneScoped;

    std::ifstream fileStream = std::ifstream(file, std::ifstream::binary);
    if (!fileStream.good()) {
        LERROR(std::format("Error opening file '{}' for loading cache file", file));
        return { false, {}, {} };
    }

    int8_t cacheVersion = 0;
    fileStream.read(reinterpret_cast<char*>(&cacheVersion), sizeof(int8_t));
    if (cacheVersion != CurrentCacheVersion) {
        LINFO(std::format("Removing cache file '{}' as the version changed", file));
        return { false, {}, {} };
    }

    int64_t nPoints = 0;
    fileStream.read(reinterpret_cast<char*>(&nPoints), sizeof(int64_t));
    _nPoints = static_cast<size_t>(nPoints);

    float enabledPointsRatio = false;
    fileStream.read(reinterpret_cast<char*>(&enabledPointsRatio), sizeof(float));
    _enabledPointsRatio = enabledPointsRatio;

    uint64_t nPositions = 0;
    fileStream.read(reinterpret_cast<char*>(&nPositions), sizeof(uint64_t));
    std::vector<glm::vec3> positions;
    positions.resize(nPositions);
    fileStream.read(
        reinterpret_cast<char*>(positions.data()),
        nPositions * sizeof(glm::vec3)
    );

    uint64_t nColors = 0;
    fileStream.read(reinterpret_cast<char*>(&nColors), sizeof(uint64_t));
    std::vector<glm::vec3> colors;
    colors.resize(nColors);
    fileStream.read(reinterpret_cast<char*>(colors.data()), nColors * sizeof(glm::vec3));

    Result result;
    result.success = true;
    result.positions = std::move(positions);
    result.color = std::move(colors);
    return result;
}

} // namespace openspace
