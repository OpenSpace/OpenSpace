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

#include <modules/digitaluniverse/rendering/renderablebillboardscloud.h>

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
    constexpr std::string_view _loggerCat = "RenderableBillboardsCloud";

    constexpr std::array<const char*, 18> UniformNames = {
        "cameraViewProjectionMatrix", "modelMatrix", "cameraPosition", "cameraLookUp",
        "renderOption", "maxBillboardSize", "color", "alphaValue",
        "scaleExponent", "scaleFactor", "up", "right", "fadeInValue", "screenSize",
        "spriteTexture", "useColorMap", "enablePixelSizeControl", "hasDvarScaling"
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

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value is used to define the color of the points, when no color map is"
        "used",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorMapEnabledInfo = {
        "Enabled",
        "Color Map Enabled",
        "If this value is set to 'true', the provided color map is used (if one was "
        "provided in the configuration). If no color map was provided, changing this "
        "setting does not do anything",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorMapInfo = {
        "ColorMap",
        "Color Map File",
        "The path to the color map file to use for coloring the points",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorParameterInfo = {
        "DataColumn",
        "Data Column",
        "This value determines which paramenter is used for coloring the points based "
        "on the color map. The property is set based on predefined options specified in "
        "the asset file",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorRangeInfo = {
        "ValueRange",
        "Value Range",
        "This value changes the range of values to be mapped with the current color map",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SetRangeFromDataInfo = {
        "SetRangeFromData",
        "Set Data Range from Data",
        "Set the data range for the color mapping based on the available data for the "
        "curently selected data column",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo HideOutliersInfo = {
        "HideOutliers",
        "Hide Outliers",
        "If true, points with values outside the provided range for the coloring will be "
        "rendered as transparent, i.e. not shown. Otherwise, the values will be clamped "
        "to use the color at the max VS min limit of the color map, respectively.",
        openspace::properties::Property::Visibility::User
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
        "If this value is set to 'true' ....",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo SizeOptionInfo = {
        "SizeOption",
        "Size Option Variable",
        "This value determines which paramenter (datavar) is used for scaling of the "
        "point",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Option wether the billboards should face the camera or not. Used for non-linear "
        "display environments such as fisheye.",
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

    constexpr openspace::properties::Property::PropertyInfo BillboardMaxPixelSizeInfo = {
        "BillboardMaxPixelSize",
        "Billboard Max Size in Pixels",
        "The maximum size (in pixels) for the billboard representing the point.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableBillboardsCloud)]] Parameters {
        // The path to the SPECK file that contains information about the astronomical
        // object being rendered
        std::optional<std::string> file;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

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
            [[codegen::reference("space_labelscomponent")]];

        // [[codegen::verbatim(SizeOptionInfo.description)]]
        std::optional<std::vector<std::string>> sizeOption;

        struct ColorMapSettings {
            // [[codegen::verbatim(ColorMapEnabledInfo.description)]]
            std::optional<bool> enabled;

            // [[codegen::verbatim(ColorMapInfo.description)]]
            std::optional<std::string> colorMap;

            struct ColorMapParameter {
                // The key for the datavar to use for color
                std::string key;

                // An optional value range to use for coloring when this option is selected.
                // If not included, the range will be set from the min and max value in the
                // dataset
                std::optional<glm::vec2> range;
            };
            std::optional<std::vector<ColorMapParameter>> colorParameterOptions;

            // [[codegen::verbatim(HideOutliersInfo.description)]]
            std::optional<bool> hideOutliers;
        };
        // Settings related to the choice of color map, parameters, et. cetera
        std::optional<ColorMapSettings> colorMapSettings;

        struct SizeSettings {
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

        // Transformation matrix to be applied to each astronomical object
        std::optional<glm::dmat4x4> transformationMatrix;

        // [[codegen::verbatim(FadeInDistancesInfo.description)]]
        std::optional<glm::dvec2> fadeInDistances;

        // [[codegen::verbatim(EnableDistanceFadeInfo.description)]]
        std::optional<bool> enableFadeIn;
    };

#include "renderablebillboardscloud_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableBillboardsCloud::Documentation() {
    return codegen::doc<Parameters>("digitaluniverse_RenderableBillboardsCloud");
}

RenderableBillboardsCloud::SizeSettings::SizeSettings(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "SizeSettings", "Size Settings", ""})
    , scaleExponent(ScaleExponentInfo, 1.f, 0.f, 60.f)
    , scaleFactor(ScaleFactorInfo, 1.f, 0.f, 50.f)
    , pixelSizeControl(PixelSizeControlInfo, false)
    , billboardMaxPixelSize(BillboardMaxPixelSizeInfo, 400.f, 0.f, 1000.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.sizeSettings.has_value()) {
        const Parameters::SizeSettings settings = p.sizeSettings.value();

        scaleFactor = settings.scaleFactor.value_or(scaleFactor);
        scaleExponent = settings.scaleExponent.value_or(scaleExponent);
        pixelSizeControl = settings.enablePixelSizeControl.value_or(pixelSizeControl);
        billboardMaxPixelSize = settings.billboardMaxPixelSize.value_or(billboardMaxPixelSize);
    }

    addProperty(scaleFactor);

    scaleExponent.setExponent(2.f);
    addProperty(scaleExponent);

    addProperty(pixelSizeControl);
    addProperty(billboardMaxPixelSize);
}

RenderableBillboardsCloud::SizeFromData::SizeFromData(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "SizeFromData", "Size From Data", "" })
    , enabled(SizeMappingEnabledInfo, false)
    , datavarSizeOption(
        SizeOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    // TODO: read enabled from asset

    addProperty(enabled);
    addProperty(datavarSizeOption);
}

RenderableBillboardsCloud::ColorMapSettings::ColorMapSettings(
                                                      const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "ColorMap", "Color Map", "" })
    , enabled(ColorMapEnabledInfo, true)
    , dataColumn(ColorParameterInfo, properties::OptionProperty::DisplayType::Dropdown)
    , colorMapFile(ColorMapInfo)
    , valueRange(ColorRangeInfo, glm::vec2(0.f))
    , setRangeFromData(SetRangeFromDataInfo)
    , hideOutliers(HideOutliersInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.colorMapSettings.has_value()) {
        const Parameters::ColorMapSettings settings = p.colorMapSettings.value();

        enabled = settings.enabled.value_or(enabled);

        if (settings.colorParameterOptions.has_value()) {
            std::vector<Parameters::ColorMapSettings::ColorMapParameter> opts =
                *settings.colorParameterOptions;

            colorRangeData.reserve(opts.size());
            for (size_t i = 0; i < opts.size(); ++i) {
                dataColumn.addOption(static_cast<int>(i), opts[i].key);

                // TODO: set default value to be the data range
                colorRangeData.push_back(opts[i].range.value_or(glm::vec2(0.f)));
            }

            // Following DU behavior here. The last colormap variable
            // entry is the one selected by default.
            dataColumn.setValue(static_cast<int>(colorRangeData.size() - 1));
            valueRange = colorRangeData.back();
        }

        dataColumn.onChange([this]() {
            valueRange = colorRangeData[dataColumn.value()];
        });

        // TODO: read valueRange from asset if specified

        hideOutliers = settings.hideOutliers.value_or(hideOutliers);

        if (settings.colorMap.has_value()) {
            colorMapFile = absPath(*settings.colorMap).string();
        }
    }

    addProperty(enabled);
    addProperty(dataColumn);

    addProperty(valueRange);
    addProperty(setRangeFromData);

    addProperty(hideOutliers);

    colorMapFile.setReadOnly(true); // Currently this can't be changed
    addProperty(colorMapFile);
}

RenderableBillboardsCloud::RenderableBillboardsCloud(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _pointColor(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _spriteTexturePath(SpriteTextureInfo)
    , _drawElements(DrawElementsInfo, true)
    , _fadeInDistances(
        FadeInDistancesInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(100.f)
    )
    , _fadeInDistanceEnabled(EnableDistanceFadeInfo, false)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _sizeSettings(dictionary)
    , _sizeFromData(dictionary)
    , _colorMapSettings(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _pointColor = p.color.value_or(_pointColor);
    _pointColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_pointColor);

    if (p.file.has_value()) {
        _hasSpeckFile = true;
        _speckFile = absPath(*p.file).string();
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

    if (p.sizeOption.has_value()) {
        std::vector<std::string> opts = *p.sizeOption;
        for (size_t i = 0; i < opts.size(); ++i) {
            // Note that options are added in order
            _sizeFromData.datavarSizeOption.addOption(static_cast<int>(i), opts[i]);
        }

        _sizeFromData.datavarSizeOption.onChange([this]() { _dataIsDirty = true; });
        _sizeFromData.enabled = true;

        _hasDatavarSize = true;

        addPropertySubOwner(_sizeFromData);
    }

    addPropertySubOwner(_sizeSettings);

    if (p.colorMapSettings.has_value()) {
        _hasColorMapFile = true;

        _colorMapSettings.hideOutliers.onChange([this]() { _dataIsDirty = true; });
        _colorMapSettings.valueRange.onChange([this]() { _dataIsDirty = true; });
        _colorMapSettings.dataColumn.onChange([this]() { _dataIsDirty = true; });

        _colorMapSettings.setRangeFromData.onChange([this]() {
            int parameterIndex = currentColorParameterIndex();
            _colorMapSettings.valueRange = findValueRange(parameterIndex);
        });

        addPropertySubOwner(_colorMapSettings);
    }
}

bool RenderableBillboardsCloud::isReady() const {
    bool isReady = _program && !_dataset.entries.empty();

    // If we have labels, they also need to be loaded
    if (_hasLabels) {
        isReady = isReady || _labels->isReady();
    }
    return isReady;
}

void RenderableBillboardsCloud::initialize() {
    ZoneScoped;

    if (_hasSpeckFile) {
        _dataset = speck::data::loadFileWithCache(_speckFile);
    }

    if (_hasColorMapFile) {
        _colorMap = speck::color::loadFileWithCache(
            _colorMapSettings.colorMapFile.value()
        );
    }

    if (_hasLabels) {
        _labels->initialize();
        _labels->loadLabels();
    }

    setRenderBin(Renderable::RenderBin::PreDeferredTransparent);

    // Initialize empty colormap ranges based on dataset
    for (const properties::OptionProperty::Option& option :
            _colorMapSettings.dataColumn.options())
    {
        int optionIndex = option.value;
        int colorParameterIndex = _dataset.index(option.description);

        glm::vec2& range = _colorMapSettings.colorRangeData[optionIndex];
        if (glm::length(range) < glm::epsilon<float>()) {
            range = findValueRange(colorParameterIndex);
        }
    }

    // Set the value range again, to make sure that it's updated
    if (!_colorMapSettings.colorRangeData.empty()) {
        _colorMapSettings.valueRange = _colorMapSettings.colorRangeData.back();
    }
}

void RenderableBillboardsCloud::initializeGL() {
    ZoneScoped;

    _program = DigitalUniverseModule::ProgramObjectManager.request(
        "RenderableBillboardsCloud",
        []() {
            return global::renderEngine->buildRenderProgram(
                "RenderableBillboardsCloud",
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboard_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboard_fs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboard_gs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
}

void RenderableBillboardsCloud::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    DigitalUniverseModule::ProgramObjectManager.release(
        "RenderableBillboardsCloud",
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _program = nullptr;

    DigitalUniverseModule::TextureManager.release(_spriteTexture);
    _spriteTexture = nullptr;
}

void RenderableBillboardsCloud::bindTextureForRendering() const {
    if (_spriteTexture) {
        _spriteTexture->bind();
    }
}

float RenderableBillboardsCloud::computeDistanceFadeValue(const RenderData& data) const {
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

void RenderableBillboardsCloud::renderBillboards(const RenderData& data,
                                                 const glm::dmat4& modelMatrix,
                                                 const glm::dvec3& orthoRight,
                                                 const glm::dvec3& orthoUp,
                                                 float fadeInVariable)
{
    glDepthMask(false);

    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

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

    _program->setUniform(_uniformCache.maxBillboardSize, _sizeSettings.billboardMaxPixelSize);
    _program->setUniform(_uniformCache.color, _pointColor);
    _program->setUniform(_uniformCache.alphaValue, opacity());
    _program->setUniform(_uniformCache.scaleExponent, _sizeSettings.scaleExponent);
    _program->setUniform(_uniformCache.scaleFactor, _sizeSettings.scaleFactor);
    _program->setUniform(_uniformCache.up, glm::vec3(orthoUp));
    _program->setUniform(_uniformCache.right, glm::vec3(orthoRight));
    _program->setUniform(_uniformCache.fadeInValue, fadeInVariable);

    _program->setUniform(_uniformCache.enablePixelSizeControl, _sizeSettings.pixelSizeControl);

    _program->setUniform(_uniformCache.hasDvarScaling, _sizeFromData.enabled);

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    _program->setUniform(_uniformCache.screenSize, glm::vec2(viewport[2], viewport[3]));

    ghoul::opengl::TextureUnit textureUnit;
    textureUnit.activate();
    bindTextureForRendering();
    _program->setUniform(_uniformCache.spriteTexture, textureUnit);
    _program->setUniform(_uniformCache.useColormap, _hasColorMapFile && _colorMapSettings.enabled);

    glBindVertexArray(_vao);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_dataset.entries.size()));
    glBindVertexArray(0);
    _program->deactivate();

    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderableBillboardsCloud::render(const RenderData& data, RendererTasks&) {
    float fadeInVar = computeDistanceFadeValue(data);

    if (fadeInVar < 0.01f) {
        return;
    }

    glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    glm::dmat4 projectionMatrix = glm::dmat4(data.camera.projectionMatrix());

    glm::dmat4 modelViewProjectionMatrix = projectionMatrix * modelViewMatrix;

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

void RenderableBillboardsCloud::update(const UpdateData&) {
    ZoneScoped;

    if (_dataIsDirty) {
        updateBufferData();
    }

    if (_spriteTextureIsDirty) {
        updateSpriteTexture();
    }
}

int RenderableBillboardsCloud::nAttributesPerPoint() const {
    int n = 4; // position
    n += _hasColorMapFile ? 4: 0;
    n += _hasDatavarSize ? 1: 0;
    return n;
}

void RenderableBillboardsCloud::updateBufferData() {
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
        GLint colorMapAttrib = _program->attributeLocation("in_colormap");
        glEnableVertexAttribArray(colorMapAttrib);
        glVertexAttribPointer(
            colorMapAttrib,
            4,
            GL_FLOAT,
            GL_FALSE,
            attibutesPerPoint * sizeof(float),
            reinterpret_cast<void*>(attributeOffset * sizeof(float))
        );
        attributeOffset += 4;
    }

    if (_hasDatavarSize) {
        GLint dvarScalingAttrib = _program->attributeLocation("in_dvarScaling");
        glEnableVertexAttribArray(dvarScalingAttrib);
        glVertexAttribPointer(
            dvarScalingAttrib,
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

void RenderableBillboardsCloud::updateSpriteTexture() {
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

int RenderableBillboardsCloud::currentColorParameterIndex() const {
    bool hasOptions = _colorMapSettings.dataColumn.options().size() > 0;
    return _hasColorMapFile && hasOptions ?
        _dataset.index(_colorMapSettings.dataColumn.option().description) :
        0;
}

int RenderableBillboardsCloud::currentSizeParameterIndex() const {
    bool hasOptions = _sizeFromData.datavarSizeOption.options().size() > 0;
    return _hasDatavarSize && hasOptions ?
        _dataset.index(_sizeFromData.datavarSizeOption.option().description) :
        -1;
}

glm::vec2 RenderableBillboardsCloud::findValueRange(int parameterIndex) const {
    if (!_hasColorMapFile) {
        return glm::vec2(0.f);
    }

    float minValue = std::numeric_limits<float>::max();
    float maxValue = -std::numeric_limits<float>::max();
    for (const speck::Dataset::Entry& e : _dataset.entries) {
        if (e.data.size() > 0) {
            float value = e.data[parameterIndex];
            minValue = std::min(value, minValue);
            maxValue = std::max(value, maxValue);
        }
        else {
            minValue = 0.f;
            maxValue = 0.f;
        }
    }

    return { minValue, maxValue };
}

glm::vec4 RenderableBillboardsCloud::colorFromColorMap(float valueToColorFrom) const {
    glm::vec2 currentColorRange = _colorMapSettings.valueRange;
    float cmax = currentColorRange.y;
    float cmin = currentColorRange.x;

    float nColors = static_cast<float>(_colorMap.entries.size());

    if (_colorMapSettings.hideOutliers) {
        bool isOutsideRange = valueToColorFrom < cmin || valueToColorFrom > cmax;

        if (isOutsideRange) {
            return glm::vec4(0.f);
        }
    }

    // Nearest neighbor

    float normalization = (cmax != cmin) ? (nColors) / (cmax - cmin) : 0;
    int colorIndex = static_cast<int>((valueToColorFrom - cmin) * normalization);

    // Clamp color index to valid range
    colorIndex = std::max(colorIndex, 0);
    colorIndex = std::min(colorIndex, static_cast<int>(nColors) - 1);

    return _colorMap.entries[colorIndex];
}

std::vector<float> RenderableBillboardsCloud::createDataSlice() {
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

    for (const speck::Dataset::Entry& e : _dataset.entries) {
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
        if (_hasColorMapFile && !_colorMap.entries.empty()) {
            biggestCoord = std::max(biggestCoord, glm::compMax(position));
            // Note: if exact colormap option is not selected, the first color and the
            // last color in the colormap file are the outliers colors.
            float valueToColorFrom = e.data[colorParamIndex];

            glm::vec4 c = colorFromColorMap(valueToColorFrom);
            result.push_back(c.r);
            result.push_back(c.g);
            result.push_back(c.b);
            result.push_back(c.a);
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
