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

#include <modules/galaxy/rendering/renderablegalaxy.h>

#include <modules/galaxy/rendering/galaxyraycaster.h>
#include <modules/volume/rawvolume.h>
#include <modules/volume/rawvolumereader.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/raycastermanager.h>
#include <openspace/rendering/renderable.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/boxgeometry.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/fmt.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtc/matrix_transform.hpp>
#include <fstream>

namespace {
    constexpr int8_t CurrentCacheVersion = 1;

    constexpr const char* GlslRaycastPath =
        "${MODULE_GALAXY}/shaders/galaxyraycast.glsl";
    constexpr const char* GlslBoundsVsPath =
        "${MODULE_GALAXY}/shaders/raycasterbounds_vs.glsl";
    constexpr const char* GlslBoundsFsPath =
        "${MODULE_GALAXY}/shaders/raycasterbounds_fs.glsl";
    constexpr const char* _loggerCat = "Renderable Galaxy";

    constexpr const std::array<const char*, 4> UniformNamesPoints = {
        "modelMatrix", "cameraViewProjectionMatrix", "eyePosition",
        "opacityCoefficient"
    };

    constexpr const std::array<const char*, 5> UniformNamesBillboards = {
        "modelMatrix", "cameraViewProjectionMatrix",
        "cameraUp", "eyePosition", "psfTexture"
    };

    constexpr openspace::properties::Property::PropertyInfo VolumeRenderingEnabledInfo = {
        "VolumeRenderingEnabled",
        "Volume Rendering",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo StarRenderingEnabledInfo = {
        "StarRenderingEnabled",
        "Star Rendering",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo StepSizeInfo = {
        "StepSize",
        "Step Size",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo AbsorptionMultiplyInfo = {
        "AbsorptionMultiply",
        "Absorption Multiplier",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo EmissionMultiplyInfo = {
        "EmissionMultiply",
        "Emission Multiplier",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo TranslationInfo = {
        "Translation",
        "Translation",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo RotationInfo = {
        "Rotation",
        "Euler rotation",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo StarRenderingMethodInfo = {
        "StarRenderingMethod",
        "Star Rendering Method",
        "This value determines which rendering method is used for visualization of the "
        "stars."
    };

    constexpr openspace::properties::Property::PropertyInfo EnabledPointsRatioInfo = {
        "EnabledPointsRatio",
        "Enabled points",
        "" // @TODO Missing documentation
    };

    constexpr openspace::properties::Property::PropertyInfo DownscaleVolumeRenderingInfo =
    {
        "Downscale",
        "Downscale Factor Volume Rendering",
        "This value set the downscaling factor"
        " when rendering the current volume."
    };

    constexpr openspace::properties::Property::PropertyInfo NumberOfRayCastingStepsInfo =
    {
        "Steps",
        "Number of RayCasting Steps",
        "This value set the number of integration steps during the raycasting procedure."
    };

    void saveCachedFile(const std::string& file, const std::vector<glm::vec3>& positions,
                        const std::vector<glm::vec3>& colors, int64_t nPoints,
                        float pointsRatio)
    {
        std::ofstream fileStream(file, std::ofstream::binary);

        if (!fileStream.good()) {
            LERROR(fmt::format("Error opening file '{}' for save cache file", file));
            return;
        }

        fileStream.write(
            reinterpret_cast<const char*>(&CurrentCacheVersion),
            sizeof(int8_t)
        );
        fileStream.write(reinterpret_cast<const char*>(&nPoints), sizeof(int64_t));
        fileStream.write(reinterpret_cast<const char*>(&pointsRatio), sizeof(float));
        uint64_t nPositions = static_cast<uint64_t>(positions.size());
        fileStream.write(reinterpret_cast<const char*>(&nPositions), sizeof(uint64_t));
        fileStream.write(
            reinterpret_cast<const char*>(positions.data()),
            positions.size() * sizeof(glm::vec3)
        );
        uint64_t nColors = static_cast<uint64_t>(colors.size());
        fileStream.write(reinterpret_cast<const char*>(&nColors), sizeof(uint64_t));
        fileStream.write(
            reinterpret_cast<const char*>(colors.data()),
            colors.size() * sizeof(glm::vec3)
        );
    }

} // namespace

namespace openspace {

RenderableGalaxy::RenderableGalaxy(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _volumeRenderingEnabled(VolumeRenderingEnabledInfo, true)
    , _starRenderingEnabled(StarRenderingEnabledInfo, true)
    , _stepSize(StepSizeInfo, 0.01f, 0.0005f, 0.05f, 0.001f)
    , _absorptionMultiply(AbsorptionMultiplyInfo, 40.f, 0.0f, 200.0f)
    , _emissionMultiply(EmissionMultiplyInfo, 200.f, 0.0f, 1000.0f)
    , _starRenderingMethod(
        StarRenderingMethodInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _enabledPointsRatio(EnabledPointsRatioInfo, 0.5f, 0.01f, 1.0f)
    , _translation(TranslationInfo, glm::vec3(0.f), glm::vec3(0.f), glm::vec3(1.f))
    , _rotation(
        RotationInfo,
        glm::vec3(0.f),
        glm::vec3(0.f),
        glm::vec3(glm::two_pi<float>())
    )
    , _downScaleVolumeRendering(DownscaleVolumeRenderingInfo, 1.f, 0.1f, 1.f)
    , _numberOfRayCastingSteps(NumberOfRayCastingStepsInfo, 1000.f, 1.f, 1000.f)
{
    dictionary.getValue("VolumeRenderingEnabled", _volumeRenderingEnabled);
    dictionary.getValue("StarRenderingEnabled", _starRenderingEnabled);
    dictionary.getValue("StepSize", _stepSize);
    dictionary.getValue("AbsorptionMultiply", _absorptionMultiply);
    dictionary.getValue("EmissionMultiply", _emissionMultiply);
    dictionary.getValue("StarRenderingMethod", _starRenderingMethod);
    dictionary.getValue("EnabledPointsRatio", _enabledPointsRatio);
    dictionary.getValue("Translation", _translation);
    dictionary.getValue("Rotation", _rotation);

    if (dictionary.hasKeyAndValue<bool>(VolumeRenderingEnabledInfo.identifier)) {
        _volumeRenderingEnabled = dictionary.value<bool>(
            VolumeRenderingEnabledInfo.identifier
        );
    }

    if (dictionary.hasKeyAndValue<bool>(StarRenderingEnabledInfo.identifier)) {
        _starRenderingEnabled = static_cast<bool>(StarRenderingEnabledInfo.identifier);
    }

    if (dictionary.hasKeyAndValue<double>(StepSizeInfo.identifier)) {
        _stepSize = static_cast<float>(dictionary.value<double>(StepSizeInfo.identifier));
    }

    if (dictionary.hasKeyAndValue<double>(AbsorptionMultiplyInfo.identifier)) {
        _absorptionMultiply = static_cast<float>(
            dictionary.value<double>(AbsorptionMultiplyInfo.identifier)
        );
    }

    if (dictionary.hasKeyAndValue<double>(EmissionMultiplyInfo.identifier)) {
        _emissionMultiply = static_cast<float>(
            dictionary.value<double>(EmissionMultiplyInfo.identifier)
        );
    }

    _starRenderingMethod.addOptions({
        { 0, "Points" },
        { 1, "Billboards" }
    });
    if (dictionary.hasKey(StarRenderingMethodInfo.identifier)) {
        const std::string starRenderingMethod = dictionary.value<std::string>(
            StarRenderingMethodInfo.identifier
        );
        if (starRenderingMethod == "Points") {
            _starRenderingMethod = 0;
        }
        else if (starRenderingMethod == "Billboards") {
            _starRenderingMethod = 1;
        }
    }

    if (dictionary.hasKeyAndValue<glm::vec3>(TranslationInfo.identifier)) {
        _translation = dictionary.value<glm::vec3>(TranslationInfo.identifier);
    }

    if (dictionary.hasKeyAndValue<glm::vec3>(RotationInfo.identifier)) {
        _rotation = dictionary.value<glm::vec3>(RotationInfo.identifier);
    }

    if (!dictionary.hasKeyAndValue<ghoul::Dictionary>("Volume")) {
        LERROR("No volume dictionary specified.");
    }

    ghoul::Dictionary volumeDictionary = dictionary.value<ghoul::Dictionary>("Volume");

    std::string volumeFilename;
    if (volumeDictionary.getValue("Filename", volumeFilename)) {
        _volumeFilename = absPath(volumeFilename);
    }
    else {
        LERROR("No volume filename specified.");
    }
    glm::vec3 volumeDimensions = glm::vec3(0.f);
    if (volumeDictionary.getValue("Dimensions", volumeDimensions)) {
        _volumeDimensions = static_cast<glm::ivec3>(volumeDimensions);
    }
    else {
        LERROR("No volume dimensions specified.");
    }
    glm::vec3 volumeSize = glm::vec3(0.f);
    if (volumeDictionary.getValue("Size", volumeSize)) {
        _volumeSize = volumeSize;
    }
    else {
        LERROR("No volume dimensions specified.");
    }

    if (volumeDictionary.hasKey(NumberOfRayCastingStepsInfo.identifier)) {
        _numberOfRayCastingSteps = static_cast<float>(
            volumeDictionary.value<double>(NumberOfRayCastingStepsInfo.identifier)
        );
    }
    else {
        LINFO("Number of raycasting steps not specified. Using default value.");
    }

    _downScaleVolumeRendering.setVisibility(properties::Property::Visibility::Developer);
    if (volumeDictionary.hasKey(DownscaleVolumeRenderingInfo.identifier)) {
        _downScaleVolumeRendering =
            volumeDictionary.value<float>(DownscaleVolumeRenderingInfo.identifier);
    }

    if (!dictionary.hasKeyAndValue<ghoul::Dictionary>("Points")) {
        LERROR("No points dictionary specified.");
    }

    ghoul::Dictionary pointsDictionary = dictionary.value<ghoul::Dictionary>("Points");
    std::string pointsFilename;
    if (pointsDictionary.getValue("Filename", pointsFilename)) {
        _pointsFilename = absPath(pointsFilename);
    }
    else {
        LERROR("No points filename specified.");
    }

    if (pointsDictionary.hasKeyAndValue<double>(EnabledPointsRatioInfo.identifier)) {
        _enabledPointsRatio = static_cast<float>(
            pointsDictionary.value<double>(EnabledPointsRatioInfo.identifier)
        );
    }

    std::string pointSpreadFunctionTexturePath;
    if (pointsDictionary.getValue("Texture", pointSpreadFunctionTexturePath)) {
        _pointSpreadFunctionTexturePath = absPath(pointSpreadFunctionTexturePath);
        _pointSpreadFunctionFile = std::make_unique<ghoul::filesystem::File>(
            _pointSpreadFunctionTexturePath
        );
    }
    else {
        LERROR("No points filename specified.");
    }
}

void RenderableGalaxy::initializeGL() {
    // Aspect is currently hardcoded to cubic voxels.
    _aspect = static_cast<glm::vec3>(_volumeDimensions);
    _aspect /= std::max(std::max(_aspect.x, _aspect.y), _aspect.z);

    // The volume
    volume::RawVolumeReader<glm::tvec4<GLubyte>> reader(
        _volumeFilename,
        _volumeDimensions
    );
    _volume = reader.read();

    _texture = std::make_unique<ghoul::opengl::Texture>(
        _volumeDimensions,
        ghoul::opengl::Texture::Format::RGBA,
        GL_RGBA,
        GL_UNSIGNED_BYTE,
        ghoul::opengl::Texture::FilterMode::Linear,
        ghoul::opengl::Texture::WrappingMode::ClampToEdge
    );

    _texture->setPixelData(
        reinterpret_cast<char*>(_volume->data()),
        ghoul::opengl::Texture::TakeOwnership::No
    );

    _texture->setDimensions(_volume->dimensions());
    _texture->uploadTexture();

    _raycaster = std::make_unique<GalaxyRaycaster>(*_texture);
    _raycaster->initialize();

    global::raycasterManager.attachRaycaster(*_raycaster);

    auto onChange = [&](bool enabled) {
        if (enabled) {
            global::raycasterManager.attachRaycaster(*_raycaster);
        }
        else {
            global::raycasterManager.detachRaycaster(*_raycaster);
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
    addProperty(_translation);
    addProperty(_rotation);
    addProperty(_downScaleVolumeRendering);
    addProperty(_numberOfRayCastingSteps);

    // initialize points.
    if (_pointsFilename.empty()) {
        return;
    }

    _pointsProgram = global::renderEngine.buildRenderProgram(
        "Galaxy points",
        absPath("${MODULE_GALAXY}/shaders/points_vs.glsl"),
        absPath("${MODULE_GALAXY}/shaders/points_fs.glsl")
    );
    _billboardsProgram = global::renderEngine.buildRenderProgram(
        "Galaxy billboard",
        absPath("${MODULE_GALAXY}/shaders/billboard_vs.glsl"),
        absPath("${MODULE_GALAXY}/shaders/billboard_fs.glsl"),
        absPath("${MODULE_GALAXY}/shaders/billboard_ge.glsl")
    );

    if (!_pointSpreadFunctionTexturePath.empty()) {
        _pointSpreadFunctionTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(_pointSpreadFunctionTexturePath)
        );

        if (_pointSpreadFunctionTexture) {
            LDEBUG(fmt::format(
                "Loaded texture from '{}'",
                absPath(_pointSpreadFunctionTexturePath)
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

    _pointsProgram->setIgnoreUniformLocationError(
        ghoul::opengl::ProgramObject::IgnoreError::Yes
    );

    GLint positionAttrib = _pointsProgram->attributeLocation("in_position");
    GLint colorAttrib = _pointsProgram->attributeLocation("in_color");


    std::vector<glm::vec3> pointPositions;
    std::vector<glm::vec3> pointColors;

    std::string cachedPointsFile = FileSys.cacheManager()->cachedFilename(
        _pointsFilename,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );
    const bool hasCachedFile = FileSys.fileExists(cachedPointsFile);
    if (hasCachedFile) {
        LINFO(fmt::format("Cached file '{}' used for galaxy point file '{}'",
            cachedPointsFile, _pointsFilename
        ));

        Result res = loadCachedFile(cachedPointsFile);
        if (res.success) {
            pointPositions = std::move(res.positions);
            pointColors = std::move(res.color);
        }
        else {
            FileSys.cacheManager()->removeCacheFile(_pointsFilename);
            Result resPoint = loadPointFile(_pointsFilename);
            pointPositions = std::move(resPoint.positions);
            pointColors = std::move(resPoint.color);
            saveCachedFile(
                cachedPointsFile,
                pointPositions,
                pointColors,
                _nPoints,
                _enabledPointsRatio
            );
        }
    }
    else {
        Result res = loadPointFile(_pointsFilename);
        ghoul_assert(res.success, "Point file loading failed");
        pointPositions = std::move(res.positions);
        pointColors = std::move(res.color);
        saveCachedFile(
            cachedPointsFile,
            pointPositions,
            pointColors,
            _nPoints,
            _enabledPointsRatio
        );
    }

    glGenVertexArrays(1, &_pointsVao);
    glGenBuffers(1, &_positionVbo);
    glGenBuffers(1, &_colorVbo);

    glBindVertexArray(_pointsVao);
    glBindBuffer(GL_ARRAY_BUFFER, _positionVbo);
    glBufferData(GL_ARRAY_BUFFER,
        pointPositions.size() * sizeof(glm::vec3),
        pointPositions.data(),
        GL_STATIC_DRAW
    );

    glBindBuffer(GL_ARRAY_BUFFER, _colorVbo);
    glBufferData(GL_ARRAY_BUFFER,
        pointColors.size() * sizeof(glm::vec3),
        pointColors.data(),
        GL_STATIC_DRAW
    );

    glBindBuffer(GL_ARRAY_BUFFER, _positionVbo);
    glEnableVertexAttribArray(positionAttrib);
    glVertexAttribPointer(positionAttrib, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

    glBindBuffer(GL_ARRAY_BUFFER, _colorVbo);
    glEnableVertexAttribArray(colorAttrib);
    glVertexAttribPointer(colorAttrib, 3, GL_FLOAT, GL_FALSE, 0, nullptr);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
}

void RenderableGalaxy::deinitializeGL() {
    if (_raycaster) {
        global::raycasterManager.detachRaycaster(*_raycaster);
        _raycaster = nullptr;
    }

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
    //glm::mat4 transform = glm::translate(, static_cast<glm::vec3>(_translation));
    const glm::vec3 eulerRotation = static_cast<glm::vec3>(_rotation);
    glm::mat4 transform = glm::rotate(
        glm::mat4(1.f),
        eulerRotation.x,
        glm::vec3(1.f, 0.f, 0.f)
    );
    transform = glm::rotate(transform, eulerRotation.y, glm::vec3(0.f, 1.f, 0.f));
    transform = glm::rotate(transform, eulerRotation.z,  glm::vec3(0.f, 0.f, 1.f));

    glm::mat4 volumeTransform = glm::scale(transform, _volumeSize);
    _pointTransform = transform;
    //_pointTransform = glm::scale(transform, _pointScaling);

    const glm::vec4 translation = glm::vec4(_translation.value()*_volumeSize, 0.f);

    // Todo: handle floating point overflow, to actually support translation.

    volumeTransform[3] += translation;
    _pointTransform[3] += translation;

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
        RaycasterTask task { _raycaster.get(), data };

        const glm::vec3 position = data.camera.positionVec3();
        const float length = safeLength(position);
        const glm::vec3 galaxySize = _volumeSize;

        const float maxDim = std::max(std::max(galaxySize.x, galaxySize.y), galaxySize.z);

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
                                 (upperRampEnd - upperRampStart); //fade out
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

    // Render the stars
    if (_starRenderingEnabled && _opacityCoefficient > 0.f) {
        if (_starRenderingMethod == 1) {
            renderBillboards(data);
        }
        else {
            renderPoints(data);
        }
    }
}

void RenderableGalaxy::renderPoints(const RenderData& data) {
    if (!_pointsProgram) {
        return;
    }
    // Saving current OpenGL state
    GLenum blendEquationRGB;
    GLenum blendEquationAlpha;
    GLenum blendDestAlpha;
    GLenum blendDestRGB;
    GLenum blendSrcAlpha;
    GLenum blendSrcRGB;
    GLboolean depthMask;

    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    glGetBooleanv(GL_DEPTH_WRITEMASK, &depthMask);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    glDisable(GL_DEPTH_TEST);

    _pointsProgram->activate();

    glm::dmat4 rotMatrix = glm::rotate(
        glm::dmat4(1.0),
        glm::pi<double>(),
        glm::dvec3(1.0, 0.0, 0.0)) *
            glm::rotate(glm::dmat4(1.0), 3.1248, glm::dvec3(0.0, 1.0, 0.0)) *
            glm::rotate(glm::dmat4(1.0), 4.45741, glm::dvec3(0.0, 0.0, 1.0)
    );

    glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) * rotMatrix *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    glm::dmat4 projectionMatrix = glm::dmat4(data.camera.projectionMatrix());

    glm::dmat4 cameraViewProjectionMatrix = projectionMatrix *
        data.camera.combinedViewMatrix();

    _pointsProgram->setUniform(_uniformCachePoints.modelMatrix, modelMatrix);
    _pointsProgram->setUniform(
        _uniformCachePoints.cameraViewProjectionMatrix,
        cameraViewProjectionMatrix
    );

    glm::dvec3 eyePosition = glm::dvec3(
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

    glEnable(GL_DEPTH_TEST);
    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    // Restores OpenGL blending state
    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);
    glDepthMask(depthMask);
}

void RenderableGalaxy::renderBillboards(const RenderData& data) {
    if (!_billboardsProgram) {
        return;
    }

    // Saving current OpenGL state
    GLenum blendEquationRGB;
    GLenum blendEquationAlpha;
    GLenum blendDestAlpha;
    GLenum blendDestRGB;
    GLenum blendSrcAlpha;
    GLenum blendSrcRGB;
    GLboolean depthMask;

    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    glGetBooleanv(GL_DEPTH_WRITEMASK, &depthMask);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);
    glDisable(GL_DEPTH_TEST);

    _billboardsProgram->activate();

    glm::dmat4 rotMatrix = glm::rotate(
        glm::dmat4(1.0),
        glm::pi<double>(),
        glm::dvec3(1.0, 0.0, 0.0)) *
            glm::rotate(glm::dmat4(1.0), 3.1248, glm::dvec3(0.0, 1.0, 0.0)) *
            glm::rotate(glm::dmat4(1.0), 4.45741, glm::dvec3(0.0, 0.0, 1.0)
    );

    glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) * rotMatrix *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    glm::dmat4 projectionMatrix = glm::dmat4(data.camera.projectionMatrix());

    glm::dmat4 cameraViewProjectionMatrix = projectionMatrix *
        data.camera.combinedViewMatrix();

    _billboardsProgram->setUniform(_uniformCacheBillboards.modelMatrix, modelMatrix);
    _billboardsProgram->setUniform(
        _uniformCacheBillboards.cameraViewProjectionMatrix,
        cameraViewProjectionMatrix
    );

    glm::dvec3 eyePosition = glm::dvec3(
        glm::inverse(data.camera.combinedViewMatrix()) *
        glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );
    _billboardsProgram->setUniform(_uniformCacheBillboards.eyePosition, eyePosition);

    glm::dvec3 cameraUp = data.camera.lookUpVectorWorldSpace();
    _billboardsProgram->setUniform(_uniformCacheBillboards.cameraUp, cameraUp);

    ghoul::opengl::TextureUnit psfUnit;
    psfUnit.activate();
    _pointSpreadFunctionTexture->bind();
    _billboardsProgram->setUniform(_uniformCacheBillboards.psfTexture, psfUnit);

    glBindVertexArray(_pointsVao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_nPoints * _enabledPointsRatio));

    glBindVertexArray(0);

    _billboardsProgram->deactivate();

    glEnable(GL_DEPTH_TEST);
    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    // Restores OpenGL blending state
    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);
    glDepthMask(depthMask);
}

float RenderableGalaxy::safeLength(const glm::vec3& vector) const {
    const float maxComponent = std::max(
        std::max(std::abs(vector.x), std::abs(vector.y)), std::abs(vector.z)
    );
    return glm::length(vector / maxComponent) * maxComponent;
}

RenderableGalaxy::Result RenderableGalaxy::loadPointFile(const std::string&) {
    std::vector<glm::vec3> pointPositions;
    std::vector<glm::vec3> pointColors;
    int64_t nPoints;

    std::ifstream pointFile(_pointsFilename, std::ios::in);

    // Read header for OFF (Object File Format)
    std::string line;
    std::getline(pointFile, line);

    // Read point count
    std::getline(pointFile, line);
    std::istringstream iss(line);
    iss >> nPoints;

    // Prepare point reading
    _nPoints = static_cast<size_t>(nPoints);

    // Read points
    float x, y, z, r, g, b, a;
    for (size_t i = 0;
        i < static_cast<size_t>(_nPoints * _enabledPointsRatio.maxValue()) + 1;
        ++i)
    {
        std::getline(pointFile, line);
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

RenderableGalaxy::Result RenderableGalaxy::loadCachedFile(const std::string& file) {
    std::ifstream fileStream(file, std::ifstream::binary);
    if (!fileStream.good()) {
        LERROR(fmt::format("Error opening file '{}' for loading cache file", file));
        return { false, {}, {} };
    }

    int8_t cacheVersion;
    fileStream.read(reinterpret_cast<char*>(&cacheVersion), sizeof(int8_t));
    if (cacheVersion != CurrentCacheVersion) {
        LINFO(fmt::format("Removing cache file '{}' as the version changed"));
        return { false, {}, {} };
    }

    int64_t nPoints;
    fileStream.read(reinterpret_cast<char*>(&nPoints), sizeof(int64_t));
    _nPoints = static_cast<size_t>(nPoints);

    float enabledPointsRatio;
    fileStream.read(reinterpret_cast<char*>(&enabledPointsRatio), sizeof(float));
    _enabledPointsRatio = enabledPointsRatio;

    uint64_t nPositions;
    fileStream.read(reinterpret_cast<char*>(&nPositions), sizeof(uint64_t));
    std::vector<glm::vec3> positions;
    positions.resize(nPositions);
    fileStream.read(
        reinterpret_cast<char*>(positions.data()),
        nPositions * sizeof(glm::vec3)
    );

    uint64_t nColors;
    fileStream.read(reinterpret_cast<char*>(&nColors), sizeof(uint64_t));
    std::vector<glm::vec3> colors;
    colors.resize(nColors);
    fileStream.read(
        reinterpret_cast<char*>(colors.data()),
        nColors * sizeof(glm::vec3)
    );

    Result result;
    result.success = true;
    result.positions = std::move(positions);
    result.color = std::move(colors);
    return result;
}

} // namespace openspace
