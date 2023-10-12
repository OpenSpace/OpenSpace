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

#include <modules/digitaluniverse/rendering/renderablepointcloud.h>

#include <modules/digitaluniverse/digitaluniversemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/util/updatestructures.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/string_cast.hpp>
#include <array>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <locale>
#include <optional>
#include <string>

namespace {
    constexpr std::string_view _loggerCat = "RenderablePointCloud";

    constexpr std::array<const char*, 24> UniformNames = {
        "cameraViewProjectionMatrix", "modelMatrix", "cameraPosition", "cameraLookUp",
        "renderOption", "maxBillboardSize", "color", "alphaValue", "scaleExponent",
        "scaleFactor", "up", "right", "fadeInValue", "screenSize", "spriteTexture",
        "useColorMap", "colorMapTexture", "cmapRangeMin", "cmapRangeMax", "nanColor",
        "useNanColor", "hideOutsideRange", "enablePixelSizeControl", "hasDvarScaling"
    };

    enum RenderOption {
        ViewDirection = 0,
        PositionNormal
    };

    constexpr openspace::properties::Property::PropertyInfo SpriteTextureInfo = {
        "Texture",
        "Point Sprite Texture",
        "The path to the texture that should be used as the point sprite",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleExponentInfo = {
        "ScaleExponent",
        "Scale Exponent",
        "This value is used as in exponential scaling to set the absolute size of the "
        "point. In general, the larger distance the dataset covers, the larger this "
        "value should be.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor to adjust the size of the points, "
        "after the exponential scaling and any pixel-size control effects. Simply just "
        "increases or decreases the visual size of the points",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo PointColorInfo = {
        "FixedColor",
        "Fixed Color",
        "This value is used to define the color of the points when no color map is"
        "used",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the points",
        // @VISIBILITY(1.25)
        openspace::properties::Property::Visibility::NoviceUser
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo LabelsInfo = {
        "Labels",
        "Labels",
        "The labels for the points"
    };

    constexpr openspace::properties::Property::PropertyInfo SizeMappingEnabledInfo = {
        "Enabled",
        "Size Mapping Enabled",
        "If this value is set to 'true' ....", // @ TODO
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo SizeMappingOptionInfo = {
        "Parameter",
        "Parameter Option",
        "This value determines which paramenter (datavar) is used for scaling of the "
        "point", // @TODO: Clarify how it's applied
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Option wether the point billboards should face the camera or not. Used for "
        "non-linear display environments such as fisheye.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInDistancesInfo = {
        "FadeInDistances",
        "Fade-In Start and End Distances",
        "These values determine the initial and final distances from the center of "
        "our galaxy from which the object will start and end fading-in",
        // @VISIBILITY(3.25)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EnableDistanceFadeInfo = {
        "EnableDistanceFading",
        "Enable Distance-based Fading",
        "Enables/Disables the Fade-in effect based on camera distance. Automatically set "
        "to true if FadeInDistances are specified in the asset.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo PixelSizeControlInfo = {
        "EnablePixelSizeControl",
        "Enable Pixel Size Control",
        "If true, the Max Size in Pixels property will be used as an upper limit for the "
        "size of the point. Limits the maximum size of the points when navigating closely"
        "to them. Currenlty computed base don rectangular displays and might look weird "
        "in other projections",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseAdditiveBlendingInfo = {
        "UseAdditiveBlending",
        "Use Additive Blending",
        "If true (default), the color of points rendered on top of each other is "
        "blended additively, resulting in a brighter color where points overlap. "
        "If false, no such blending will take place and the color of the point "
        "will not be modified by blending. Note that this may lead to weird behaviors "
        "when the points are rendered with transparency.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BillboardMaxPixelSizeInfo = {
        "BillboardMaxPixelSize",
        "Billboard Max Size in Pixels",
        "The maximum size (in pixels) for the billboard representing the point.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderablePointCloud)]] Parameters {
        // The path to the SPECK file that contains information about the astronomical
        // object being rendered
        std::optional<std::string> file;

        // If the data file has a numeric value that corresponds to missing data points,
        // this setting can be used to interpret point with this value as having no data.
        // Note that this does however have its limitations, as the same value will be
        // used across all columns in the dataset.
        std::optional<float> missingDataValue;

        // [[codegen::verbatim(SpriteTextureInfo.description)]]
        std::optional<std::string> texture;

        // [[codegen::verbatim(DrawElementsInfo.description)]]
        std::optional<bool> drawElements;

        enum class [[codegen::map(RenderOption)]] RenderOption {
            ViewDirection [[codegen::key("Camera View Direction")]],
            PositionNormal [[codegen::key("Camera Position Normal")]]
        };
        // [[codegen::verbatim(RenderOptionInfo.description)]]
        std::optional<RenderOption> renderOption;

        // [[codegen::verbatim(UseAdditiveBlendingInfo.description)]]
        std::optional<bool> useAdditiveBlending;

        enum class [[codegen::map(openspace::DistanceUnit)]] Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            Gigalightyear [[codegen::key("Gly")]]
        };
        // The unit used for all distances. Must match the unit of any
        // distances/positions in the data files
        std::optional<Unit> unit;

        // [[codegen::verbatim(LabelsInfo.description)]]
        std::optional<ghoul::Dictionary> labels
            [[codegen::reference("labelscomponent")]];

        struct SizeSettings {
            // A list specifying all parameters that may be use for size mapping, i.e.
            // scaling the points based on the provided data columns
            std::optional<std::vector<std::string>> sizeMapping;

            // [[codegen::verbatim(ScaleExponentInfo.description)]]
            std::optional<float> scaleExponent;

            // [[codegen::verbatim(ScaleFactorInfo.description)]]
            std::optional<float> scaleFactor;

            // [[codegen::verbatim(PixelSizeControlInfo.description)]]
            std::optional<bool> enablePixelSizeControl;

            // [[codegen::verbatim(BillboardMaxPixelSizeInfo.description)]]
            std::optional<float> billboardMaxPixelSize;
        };
        // Settings related to the scale of the points, whether they should limit to
        // a certain pixel size, et. cetera
        std::optional<SizeSettings> sizeSettings;

        struct ColorSettings {
            // [[codegen::verbatim(PointColorInfo.description)]]
            std::optional<glm::vec3> fixedColor [[codegen::color()]];

            // Settings related to the choice of color map, parameters, et. cetera
            std::optional<ghoul::Dictionary> colorMap
                [[codegen::reference("digitaluniverse_colormapcomponent")]];
        };
        // Settings related to the coloring of the points, such as a fixed color,
        // color map, etc.
        std::optional<ColorSettings> coloring;

        // Transformation matrix to be applied to each astronomical object
        std::optional<glm::dmat4x4> transformationMatrix;

        // [[codegen::verbatim(FadeInDistancesInfo.description)]]
        std::optional<glm::dvec2> fadeInDistances;

        // [[codegen::verbatim(EnableDistanceFadeInfo.description)]]
        std::optional<bool> enableFadeIn;
    };

#include "renderablepointcloud_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderablePointCloud::Documentation() {
    return codegen::doc<Parameters>("digitaluniverse_renderablepointcloud");
}

RenderablePointCloud::SizeSettings::SizeSettings(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "Sizing", "Sizing", ""})
    , scaleExponent(ScaleExponentInfo, 1.f, 0.f, 60.f)
    , scaleFactor(ScaleFactorInfo, 1.f, 0.f, 50.f)
    , pixelSizeControl(PixelSizeControlInfo, false)
    , billboardMaxPixelSize(BillboardMaxPixelSizeInfo, 400.f, 0.f, 1000.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.sizeSettings.has_value()) {
        const Parameters::SizeSettings settings = *p.sizeSettings;

        scaleFactor = settings.scaleFactor.value_or(scaleFactor);
        scaleExponent = settings.scaleExponent.value_or(scaleExponent);
        pixelSizeControl = settings.enablePixelSizeControl.value_or(pixelSizeControl);
        billboardMaxPixelSize = settings.billboardMaxPixelSize.value_or(billboardMaxPixelSize);

        if (settings.sizeMapping.has_value()) {
            std::vector<std::string> opts = *settings.sizeMapping;
            for (size_t i = 0; i < opts.size(); ++i) {
                // Note that options are added in order
                sizeMapping.parameterOption.addOption(static_cast<int>(i), opts[i]);
            }
            sizeMapping.enabled = true;

            addPropertySubOwner(sizeMapping);
        }
    }

    addProperty(scaleFactor);

    scaleExponent.setExponent(2.f);
    addProperty(scaleExponent);

    addProperty(pixelSizeControl);
    addProperty(billboardMaxPixelSize);
}

RenderablePointCloud::SizeSettings::SizeMapping::SizeMapping()
    : properties::PropertyOwner({ "SizeMapping", "Size Mapping", "" })
    , enabled(SizeMappingEnabledInfo, false)
    , parameterOption(
        SizeMappingOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
{
    addProperty(enabled);
    addProperty(parameterOption);
}

RenderablePointCloud::ColorSettings::ColorSettings(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "Coloring", "Coloring", "" })
    , pointColor(PointColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.coloring.has_value()) {
        const Parameters::ColorSettings settings = *p.coloring;
        pointColor = settings.fixedColor.value_or(pointColor);

        if (settings.colorMap.has_value()) {
            colorMapComponent = std::make_unique<ColorMapComponent>(
                *settings.colorMap
           );
           addPropertySubOwner(colorMapComponent.get());
        }
    }
    pointColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(pointColor);
}

RenderablePointCloud::RenderablePointCloud(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _spriteTexturePath(SpriteTextureInfo)
    , _drawElements(DrawElementsInfo, true)
    , _fadeInDistances(
        FadeInDistancesInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(100.f)
    )
    , _fadeInDistanceEnabled(EnableDistanceFadeInfo, false)
    , _useAdditiveBlending(UseAdditiveBlendingInfo, true)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _sizeSettings(dictionary)
    , _colorSettings(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    if (p.file.has_value()) {
        _hasSpeckFile = true;
        _speckFile = absPath(*p.file).string();
    }

    if (p.missingDataValue.has_value()) {
        _missingDataValue = p.missingDataValue;
    }

    _drawElements = p.drawElements.value_or(_drawElements);
    addProperty(_drawElements);

    _renderOption.addOption(RenderOption::ViewDirection, "Camera View Direction");
    _renderOption.addOption(RenderOption::PositionNormal, "Camera Position Normal");

    if (p.renderOption.has_value()) {
        _renderOption = codegen::map<RenderOption>(*p.renderOption);
    }
    else {
        _renderOption = RenderOption::ViewDirection;
    }
    addProperty(_renderOption);

    _useAdditiveBlending = p.useAdditiveBlending.value_or(_useAdditiveBlending);
    addProperty(_useAdditiveBlending);

    if (p.unit.has_value()) {
        _unit = codegen::map<DistanceUnit>(*p.unit);
    }
    else {
        _unit = DistanceUnit::Meter;
    }

    if (p.texture.has_value()) {
        _spriteTexturePath = absPath(*p.texture).string();
        _spriteTexturePath.onChange([this]() { _spriteTextureIsDirty = true; });

        // @TODO (abock, 2021-01-31) I don't know why we only add this property if the
        // texture is given, but I think it's a bug
        // @TODO (emmbr, 2021-05-24) This goes for several properties in this renderable
        addProperty(_spriteTexturePath);
    }
    _hasSpriteTexture = p.texture.has_value();

    if (p.labels.has_value()) {
        _labels = std::make_unique<LabelsComponent>(*p.labels);
        _hasLabels = true;
        addPropertySubOwner(_labels.get());
        // Fading of the labels should also depend on the fading of the renderable
        _labels->setParentFadeable(this);
    }

    _transformationMatrix = p.transformationMatrix.value_or(_transformationMatrix);

    if (p.fadeInDistances.has_value()) {
        _fadeInDistances = *p.fadeInDistances;
        _fadeInDistances.setViewOption(properties::Property::ViewOptions::MinMaxRange);
        addProperty(_fadeInDistances);

        _fadeInDistanceEnabled = true;
        addProperty(_fadeInDistanceEnabled);
    }

    if (p.sizeSettings.has_value() && (*p.sizeSettings).sizeMapping.has_value()) {
        _sizeSettings.sizeMapping.parameterOption.onChange(
            [this]() { _dataIsDirty = true; }
        );
        _hasDatavarSize = true;
    }

    addPropertySubOwner(_sizeSettings);
    addPropertySubOwner(_colorSettings);

    if (p.coloring.has_value() && (*p.coloring).colorMap.has_value()) {
        _hasColorMapFile = true;

        _colorSettings.colorMapComponent->dataColumn.onChange(
            [this]() { _dataIsDirty = true; }
        );

        _colorSettings.colorMapComponent->setRangeFromData.onChange([this]() {
            int parameterIndex = currentColorParameterIndex();
            _colorSettings.colorMapComponent->valueRange = _dataset.findValueRange(
                parameterIndex
            );
        });
    }
}

bool RenderablePointCloud::isReady() const {
    bool isReady = _program && !_dataset.entries.empty();

    // If we have labels, they also need to be loaded
    if (_hasLabels) {
        isReady = isReady || _labels->isReady();
    }
    return isReady;
}

void RenderablePointCloud::initialize() {
    ZoneScoped;

    if (_hasSpeckFile) {
        dataloader::DataLoadSpecs specs = {
            .missingDataValue = _missingDataValue
        };
        _dataset = dataloader::data::loadFileWithCache(_speckFile, specs);

        if (_hasColorMapFile) {
            _colorSettings.colorMapComponent->initialize(_dataset);
        }
    }

    if (_hasLabels) {
        _labels->initialize();
        _labels->loadLabels();
    }

    setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
}

void RenderablePointCloud::initializeGL() {
    ZoneScoped;

    _program = DigitalUniverseModule::ProgramObjectManager.request(
        "RenderablePointCloud",
        []() {
            return global::renderEngine->buildRenderProgram(
                "RenderablePointCloud",
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpoint_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpoint_fs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpoint_gs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    if (_hasColorMapFile) {
        _colorSettings.colorMapComponent->initializeTexture();
    }
}

void RenderablePointCloud::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    DigitalUniverseModule::ProgramObjectManager.release(
        "RenderablePointCloud",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _program = nullptr;

    DigitalUniverseModule::TextureManager.release(_spriteTexture);
    _spriteTexture = nullptr;
}

void RenderablePointCloud::bindTextureForRendering() const {
    if (_spriteTexture) {
        _spriteTexture->bind();
    }
}

float RenderablePointCloud::computeDistanceFadeValue(const RenderData& data) const {
    float fadeValue = 1.f;
    if (_fadeInDistanceEnabled) {
        float distCamera = static_cast<float>(glm::length(data.camera.positionVec3()));
        const glm::vec2 fadeRange = _fadeInDistances;
        const float a = static_cast<float>(
            1.f / ((fadeRange.y - fadeRange.x) * toMeter(_unit))
        );
        const float b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        const float funcValue = a * distCamera + b;
        fadeValue *= std::min(1.f, funcValue);
    }

    return fadeValue;
}

void RenderablePointCloud::renderBillboards(const RenderData& data,
                                            const glm::dmat4& modelMatrix,
                                            const glm::dvec3& orthoRight,
                                            const glm::dvec3& orthoUp,
                                            float fadeInVariable)
{
    glEnablei(GL_BLEND, 0);

    if (_useAdditiveBlending) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    }
    else {
        // Normal blending, with transparency
        glDepthMask(true);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    }

    _program->activate();

    _program->setUniform(
        "screenSize",
        glm::vec2(global::renderEngine->renderingResolution())
    );

    _program->setUniform(_uniformCache.cameraPos, data.camera.positionVec3());
    _program->setUniform(
        _uniformCache.cameraLookup,
        glm::vec3(data.camera.lookUpVectorWorldSpace())
    );
    _program->setUniform(_uniformCache.renderOption, _renderOption.value());
    _program->setUniform(_uniformCache.modelMatrix, modelMatrix);
    _program->setUniform(
        _uniformCache.cameraViewProjectionMatrix,
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix()
    );

    _program->setUniform(_uniformCache.up, glm::vec3(orthoUp));
    _program->setUniform(_uniformCache.right, glm::vec3(orthoRight));
    _program->setUniform(_uniformCache.fadeInValue, fadeInVariable);
    _program->setUniform(_uniformCache.alphaValue, opacity());

    _program->setUniform(_uniformCache.scaleExponent, _sizeSettings.scaleExponent);
    _program->setUniform(_uniformCache.scaleFactor, _sizeSettings.scaleFactor);
    _program->setUniform(_uniformCache.enablePixelSizeControl, _sizeSettings.pixelSizeControl);
    _program->setUniform(_uniformCache.maxBillboardSize, _sizeSettings.billboardMaxPixelSize);
    _program->setUniform(_uniformCache.hasDvarScaling, _sizeSettings.sizeMapping.enabled);

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    _program->setUniform(_uniformCache.screenSize, glm::vec2(viewport[2], viewport[3]));

    ghoul::opengl::TextureUnit spriteTextureUnit;
    spriteTextureUnit.activate();
    bindTextureForRendering();
    _program->setUniform(_uniformCache.spriteTexture, spriteTextureUnit);

    _program->setUniform(_uniformCache.color, _colorSettings.pointColor);
    _program->setUniform(_uniformCache.nanColor, _colorSettings.colorMapComponent->nanColor);
    _program->setUniform(_uniformCache.useNanColor, _colorSettings.colorMapComponent->useNanColor);

    bool useColorMap = _hasColorMapFile && _colorSettings.colorMapComponent->enabled;
    _program->setUniform(_uniformCache.useColormap, useColorMap);
    if (useColorMap && _colorSettings.colorMapComponent->texture()) {
        ghoul::opengl::TextureUnit colorMapTextureUnit;
        colorMapTextureUnit.activate();
        _colorSettings.colorMapComponent->texture()->bind();

        _program->setUniform(_uniformCache.colorMapTexture, colorMapTextureUnit);

        const glm::vec2 range = _colorSettings.colorMapComponent->valueRange;
        _program->setUniform(_uniformCache.cmapRangeMin, range.x);
        _program->setUniform(_uniformCache.cmapRangeMax, range.y);
        _program->setUniform(
            _uniformCache.hideOutsideRange,
            _colorSettings.colorMapComponent->hideOutsideRange
        );

        // @TODO: NanColors
    }

    glBindVertexArray(_vao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_dataset.entries.size()));
    glBindVertexArray(0);
    _program->deactivate();

    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderablePointCloud::render(const RenderData& data, RendererTasks&) {
    float fadeInVar = computeDistanceFadeValue(data);

    if (fadeInVar < 0.01f) {
        return;
    }

    glm::dmat4 modelMatrix = calcModelTransform(data);
    glm::dmat4 modelViewProjectionMatrix =
        calcModelViewProjectionTransform(data, modelMatrix);

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
        orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
    }
    glm::dvec3 orthoUp = glm::normalize(glm::cross(cameraViewDirectionWorld, orthoRight));

    if (_hasSpeckFile && _drawElements) {
        renderBillboards(data, modelMatrix, orthoRight, orthoUp, fadeInVar);
    }

    if (_hasLabels) {
        _labels->render(data, modelViewProjectionMatrix, orthoRight, orthoUp, fadeInVar);
    }
}

void RenderablePointCloud::update(const UpdateData&) {
    ZoneScoped;

    if (_dataIsDirty) {
        updateBufferData();
    }

    if (_spriteTextureIsDirty) {
        updateSpriteTexture();
    }
}

int RenderablePointCloud::nAttributesPerPoint() const {
    int n = 4; // position
    n += _hasColorMapFile ? 1: 0;
    n += _hasDatavarSize ? 1: 0;
    return n;
}

void RenderablePointCloud::updateBufferData() {
    if (!_hasSpeckFile) {
        return;
    }

    ZoneScopedN("Data dirty");
    TracyGpuZone("Data dirty");
    LDEBUG("Regenerating data");

    std::vector<float> slice = createDataSlice();

    int size = static_cast<int>(slice.size());

    if (_vao == 0) {
        glGenVertexArrays(1, &_vao);
        LDEBUG(fmt::format("Generating Vertex Array id '{}'", _vao));
    }
    if (_vbo == 0) {
        glGenBuffers(1, &_vbo);
        LDEBUG(fmt::format("Generating Vertex Buffer Object id '{}'", _vbo));
    }

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, size * sizeof(float), slice.data(), GL_STATIC_DRAW);

    const int attibutesPerPoint = nAttributesPerPoint();
    int attributeOffset= 0;

    GLint positionAttrib = _program->attributeLocation("in_position");
    glEnableVertexAttribArray(positionAttrib);
    glVertexAttribPointer(
        positionAttrib,
        4,
        GL_FLOAT,
        GL_FALSE,
        attibutesPerPoint * sizeof(float),
        nullptr
    );
    attributeOffset += 4;

    if (_hasColorMapFile) {
        GLint colorParamAttrib = _program->attributeLocation("in_colorParameter");
        glEnableVertexAttribArray(colorParamAttrib);
        glVertexAttribPointer(
            colorParamAttrib,
            1,
            GL_FLOAT,
            GL_FALSE,
            attibutesPerPoint * sizeof(float),
            reinterpret_cast<void*>(attributeOffset * sizeof(float))
        );
        attributeOffset += 1;
    }

    if (_hasDatavarSize) {
        GLint scalingAttrib = _program->attributeLocation("in_scalingParameter");
        glEnableVertexAttribArray(scalingAttrib);
        glVertexAttribPointer(
            scalingAttrib,
            1,
            GL_FLOAT,
            GL_FALSE,
            attibutesPerPoint * sizeof(float),
            reinterpret_cast<void*>(attributeOffset * sizeof(float))
        );
        attributeOffset += 1;
    }

    glBindVertexArray(0);

    _dataIsDirty = false;
}

void RenderablePointCloud::updateSpriteTexture() {
    bool shouldUpdate = _hasSpriteTexture && _spriteTextureIsDirty &&
        !_spriteTexturePath.value().empty();

    if (!shouldUpdate) {
        return;
    }

    ZoneScopedN("Sprite texture");
    TracyGpuZone("Sprite texture");

    ghoul::opengl::Texture* texture = _spriteTexture;

    unsigned int hash = ghoul::hashCRC32File(_spriteTexturePath);

    _spriteTexture = DigitalUniverseModule::TextureManager.request(
        std::to_string(hash),
        [path = _spriteTexturePath]() -> std::unique_ptr<ghoul::opengl::Texture> {
            std::filesystem::path p = absPath(path);
            LINFO(fmt::format("Loaded texture from {}", p));
            std::unique_ptr<ghoul::opengl::Texture> t =
                ghoul::io::TextureReader::ref().loadTexture(p.string(), 2);
            t->uploadTexture();
            t->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            t->purgeFromRAM();
            return t;
        }
    );

    DigitalUniverseModule::TextureManager.release(texture);
    _spriteTextureIsDirty = false;
}

int RenderablePointCloud::currentColorParameterIndex() const {
    const properties::OptionProperty& property =
        _colorSettings.colorMapComponent->dataColumn;

    if (!_hasColorMapFile || property.options().empty()) {
        return 0;
    }

    return _dataset.index(property.option().description);
}

int RenderablePointCloud::currentSizeParameterIndex() const {
    const properties::OptionProperty& property =
        _sizeSettings.sizeMapping.parameterOption;

    if (!_hasDatavarSize || property.options().empty()) {
        return 0;
    }

    return _dataset.index(property.option().description);
}

std::vector<float> RenderablePointCloud::createDataSlice() {
    ZoneScoped;

    if (_dataset.entries.empty()) {
        return std::vector<float>();
    }

    std::vector<float> result;
    result.reserve(nAttributesPerPoint() * _dataset.entries.size());

    // what datavar in use for the index color
    int colorParamIndex = currentColorParameterIndex();

    // what datavar in use for the size scaling (if present)z
    int sizeParamIndex = currentSizeParameterIndex();

    double maxRadius = 0.0;
    double biggestCoord = -1.0;

    for (const dataloader::Dataset::Entry& e : _dataset.entries) {
        const double unitMeter = toMeter(_unit);
        glm::dvec4 position = glm::dvec4(glm::dvec3(e.position) * unitMeter, 1.0);
        position = _transformationMatrix * position;

        const double r = glm::length(position);
        maxRadius = std::max(maxRadius, r);

        // Positions
        for (int j = 0; j < 4; ++j) {
            result.push_back(static_cast<float>(position[j]));
        }

        // Colors
        if (_hasColorMapFile) {
            biggestCoord = std::max(biggestCoord, glm::compMax(position));
            result.push_back(e.data[colorParamIndex]);
        }

        // Size data
        if (_hasDatavarSize) {
            result.push_back(e.data[sizeParamIndex]);
        }
    }
    setBoundingSphere(maxRadius);
    _fadeInDistances.setMaxValue(glm::vec2(static_cast<float>(10.0 * biggestCoord)));
    return result;
}

} // namespace openspace
