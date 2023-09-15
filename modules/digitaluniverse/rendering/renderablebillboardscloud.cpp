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

    constexpr std::array<const char*, 20> UniformNames = {
        "cameraViewProjectionMatrix", "modelMatrix", "cameraPosition", "cameraLookUp",
        "renderOption", "minBillboardSize", "maxBillboardSize",
        "correctionSizeEndDistance", "correctionSizeFactor", "color", "alphaValue",
        "scaleFactor", "up", "right", "fadeInValue", "screenSize", "spriteTexture",
        "useColorMap", "enabledRectSizeControl", "hasDvarScaling"
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

    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each point",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo UseColorMapInfo = {
        "Enabled",
        "Color Map Enabled",
        "If this value is set to 'true', the provided color map is used (if one was "
        "provided in the configuration). If no color map was provided, changing this "
        "setting does not do anything",
        // @VISIBILITY(2.75)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value is used to define the color of the astronomical object",
        // @VISIBILITY(1.5)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorMapInfo = {
        "ColorMap",
        "Color Map File",
        "The path to the color map file of the astronomical object",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the astronomical objects",
        // @VISIBILITY(1.25)
        openspace::properties::Property::Visibility::NoviceUser
    };

    static const openspace::properties::PropertyOwner::PropertyOwnerInfo LabelsInfo = {
        "Labels",
        "Labels",
        "The labels for the astronomical objects"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorOptionInfo = {
        "ColorOption",
        "Color Option",
        "This value determines which paramenter is used for default color of the "
        "astronomical objects",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OptionColorRangeInfo = {
        "OptionColorRange",
        "Option Color Range",
        "This value changes the range of values to be mapped with the current color map",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SizeOptionInfo = {
        "SizeOption",
        "Size Option Variable",
        "This value determines which paramenter (datavar) is used for scaling of the "
        "astronomical objects",
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
        "our galaxy from which the astronomical object will start and end "
        "fading-in",
        // @VISIBILITY(3.25)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInInfo = {
        "DisableFadeIn",
        "Disable Fade-in Effect",
        "Enables/Disables the Fade-in effect",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo PixelSizeControlInfo = {
        "EnablePixelSizeControl",
        "Enable Pixel Size Control",
        "Enable pixel size control for rectangular projections. If set to true, the "
        "billboard size is restricted by the min/max size in pixels property",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BillboardMinMaxSizeInfo = {
        "BillboardMinMaxSize",
        "Billboard Min/Max Size in Pixels",
        "The minimum and maximum size (in pixels) for the billboard representing the "
        "astronomical object",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo
        CorrectionSizeEndDistanceInfo =
    {
        "CorrectionSizeEndDistance",
        "Distance in 10^X meters where correction size stops acting",
        "Distance in 10^X meters where correction size stops acting",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo CorrectionSizeFactorInfo = {
        "CorrectionSizeFactor",
        "Control variable for distance size",
        "",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseLinearFilteringInfo = {
        "UseLinearFiltering",
        "Use Linear Filtering",
        "Determines whether the provided color map should be sampled nearest neighbor "
        "(=off) or linearly (=on)",
        // @VISIBILITY(3.25)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo IsColorMapExactInfo = {
        "ExactColorMap",
        "Exact Color Map",
        "Set a 1-to-1 relationship between the color index variable and the colormap "
        "entrered value, wihtout adjusting to the min/max vlaues. Overrides any other "
        "sampling option", // @TODO: Clarify.
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SetRangeFromDataInfo = {
        "SetRangeFromData",
        "Set Data Range from Data",
        "Set the data range based on the available data",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableBillboardsCloud)]] Parameters {
        // The path to the SPECK file that contains information about the astronomical
        // object being rendered
        std::optional<std::string> file;

        // [[codegen::verbatim(ColorInfo.description)]]
        glm::vec3 color [[codegen::color()]];

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

        // [[codegen::verbatim(ScaleFactorInfo.description)]]
        std::optional<float> scaleFactor;

        // [[codegen::verbatim(UseColorMapInfo.description)]]
        std::optional<bool> useColorMap;

        // [[codegen::verbatim(ColorMapInfo.description)]]
        std::optional<std::string> colorMap;

        // [[codegen::verbatim(ColorMapInfo.description)]]
        std::optional<bool> exactColorMap;

        // The number of sides for the polygon used to represent the astronomical object
        std::optional<int> polygonSides;

        // [[codegen::verbatim(LabelsInfo.description)]]
        std::optional<ghoul::Dictionary> labels
            [[codegen::reference("space_labelscomponent")]];

        // [[codegen::verbatim(ColorOptionInfo.description)]]
        std::optional<std::vector<std::string>> colorOption;

        // [[codegen::verbatim(SizeOptionInfo.description)]]
        std::optional<std::vector<std::string>> sizeOption;

        // This value determines the colormap ranges for the color parameters of the
        // astronomical objects
        std::optional<std::vector<glm::vec2>> colorRange;

        // Transformation matrix to be applied to each astronomical object
        std::optional<glm::dmat4x4> transformationMatrix;

        // [[codegen::verbatim(FadeInDistancesInfo.description)]]
        std::optional<glm::dvec2> fadeInDistances;

        // [[codegen::verbatim(DisableFadeInInfo.description)]]
        std::optional<bool> disableFadeIn;

        // [[codegen::verbatim(BillboardMinMaxSizeInfo.description)]]
        std::optional<glm::vec2> billboardMinMaxSize;

        // [[codegen::verbatim(CorrectionSizeEndDistanceInfo.description)]]
        std::optional<float> correctionSizeEndDistance;

        // [[codegen::verbatim(CorrectionSizeFactorInfo.description)]]
        std::optional<float> correctionSizeFactor;

        // [[codegen::verbatim(PixelSizeControlInfo.description)]]
        std::optional<bool> enablePixelSizeControl;

        // [[codegen::verbatim(UseLinearFilteringInfo.description)]]
        std::optional<bool> useLinearFiltering;
    };


    // TODO: bunch Size settings inputs etc into one

#include "renderablebillboardscloud_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableBillboardsCloud::Documentation() {
    return codegen::doc<Parameters>("digitaluniverse_RenderableBillboardsCloud");
}

RenderableBillboardsCloud::SizeSettings::SizeSettings(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "SizeSetting", "Size Settings", ""})
    , scaleFactor(ScaleFactorInfo, 1.f, 0.f, 600.f)
    , pixelSizeControl(PixelSizeControlInfo, false)
    , billboardMinMaxSize(
        BillboardMinMaxSizeInfo,
        glm::vec2(0.f, 400.f),
        glm::vec2(0.f),
        glm::vec2(1000.f)
    )
    , correctionSizeEndDistance(CorrectionSizeEndDistanceInfo, 17.f, 12.f, 25.f)
    , correctionSizeFactor(CorrectionSizeFactorInfo, 8.f, 0.f, 20.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    scaleFactor = p.scaleFactor.value_or(scaleFactor);
    addProperty(scaleFactor);

    pixelSizeControl = p.enablePixelSizeControl.value_or(pixelSizeControl);
    addProperty(pixelSizeControl);

    billboardMinMaxSize = p.billboardMinMaxSize.value_or(billboardMinMaxSize);
    billboardMinMaxSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(billboardMinMaxSize);

    // TODO: Make these clearer
    correctionSizeEndDistance = p.correctionSizeEndDistance.value_or(correctionSizeEndDistance);
    addProperty(correctionSizeEndDistance);
    correctionSizeFactor = p.correctionSizeFactor.value_or(correctionSizeFactor);
    addProperty(correctionSizeFactor);
}

RenderableBillboardsCloud::SizeFromData::SizeFromData(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "SizeFromData", "Size From Data", "" })
    , datavarSizeOption(
        SizeOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(datavarSizeOption);
}

RenderableBillboardsCloud::ColorMapSettings::ColorMapSettings(
                                                      const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "ColorMap", "Color Map", "" })
    , enabled(UseColorMapInfo, true)
    , colorOption(ColorOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , colorMapFile(ColorMapInfo)
    , optionColorRangeData(OptionColorRangeInfo, glm::vec2(0.f))
    , useLinearFiltering(UseLinearFilteringInfo, false)
    , setRangeFromData(SetRangeFromDataInfo)
    , isColorMapExact(IsColorMapExactInfo, false) // TODO: Fix docs
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    enabled = p.useColorMap.value_or(enabled);
    addProperty(enabled);

    addProperty(colorOption);

    if (p.colorMap.has_value()) {
        colorMapFile = absPath(*p.colorMap).string();
    }
    colorMapFile.setReadOnly(true); // Currently this can't be changed
    addProperty(colorMapFile);

    colorRangeData = p.colorRange.value_or(colorRangeData);
    if (!colorRangeData.empty()) {
        optionColorRangeData = colorRangeData[colorRangeData.size() - 1];
    }

    optionColorRangeData.onChange([this]() {
        const glm::vec2 colorRange = optionColorRangeData;
        colorRangeData[colorOption.value()] = colorRange;
    });

    if (p.colorOption.has_value()) {
        std::vector<std::string> opts = *p.colorOption;
        for (size_t i = 0; i < opts.size(); ++i) {
            colorOption.addOption(static_cast<int>(i), opts[i]);
        }
    }
    colorOption.onChange([this]() {
        const glm::vec2 colorRange = colorRangeData[colorOption.value()];
        optionColorRangeData = colorRange;
    });

    if (colorRangeData.size() > 0) {
        // Following DU behavior here. The last colormap variable
        // entry is the one selected by default.
        colorOption.setValue(static_cast<int>(colorRangeData.size() - 1));
    }

    addProperty(optionColorRangeData);
    addProperty(setRangeFromData);

    useLinearFiltering = p.useLinearFiltering.value_or(useLinearFiltering);
    addProperty(useLinearFiltering);

    isColorMapExact = p.exactColorMap.value_or(isColorMapExact);
    addProperty(isColorMapExact);
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
    , _disableFadeInDistance(DisableFadeInInfo, true)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _sizeSettings(dictionary)
    , _sizeFromData(dictionary)
    , _colorMapSettings(dictionary)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    if (p.file.has_value()) {
        _speckFile = absPath(*p.file).string();
    }
    _hasSpeckFile = p.file.has_value();

    _drawElements = p.drawElements.value_or(_drawElements);
    _drawElements.onChange([this]() { _hasSpeckFile = !_hasSpeckFile; });
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

    _pointColor = p.color;
    _pointColor.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_pointColor);

    _polygonSides = p.polygonSides.value_or(_polygonSides);
    _hasPolygon = p.polygonSides.has_value();

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

        _disableFadeInDistance = false;
        addProperty(_disableFadeInDistance);
    }

    if (p.sizeOption.has_value()) {
        std::vector<std::string> opts = *p.sizeOption;
        for (size_t i = 0; i < opts.size(); ++i) {
            // Note that options are added in order
            _sizeFromData.datavarSizeOption.addOption(static_cast<int>(i), opts[i]);
        }

        _sizeFromData.datavarSizeOption.onChange([this]() {
            _dataIsDirty = true;
        });

        _hasDatavarSize = true;

        addPropertySubOwner(_sizeFromData);
    }

    addPropertySubOwner(_sizeSettings);

    if (p.colorMap.has_value()) {
        _hasColorMapFile = true;

        _colorMapSettings.isColorMapExact.onChange([this]() { _dataIsDirty = true; });
        _colorMapSettings.useLinearFiltering.onChange([this]() { _dataIsDirty = true; });
        _colorMapSettings.optionColorRangeData.onChange([this]() { _dataIsDirty = true; });
        _colorMapSettings.colorOption.onChange([this]() { _dataIsDirty = true; });

        _colorMapSettings.setRangeFromData.onChange([this]() {
            const int colorMapInUse = _hasColorMapFile ?
                _dataset.index(_colorMapSettings.colorOption.option().description) : 0;

            float minValue = std::numeric_limits<float>::max();
            float maxValue = -std::numeric_limits<float>::max();
            for (const speck::Dataset::Entry& e : _dataset.entries) {
                float color = e.data[colorMapInUse];
                minValue = std::min(minValue, color);
                maxValue = std::max(maxValue, color);
            }

            _colorMapSettings.optionColorRangeData = glm::vec2(minValue, maxValue);
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

    _renderToPolygonProgram = DigitalUniverseModule::ProgramObjectManager.request(
        "RenderableBillboardsCloud_Polygon",
        []() {
            return ghoul::opengl::ProgramObject::Build(
                "RenderableBillboardsCloud_Polygon",
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_fs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_gs.glsl")
            );
        }
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    if (_hasPolygon) {
        createPolygonTexture();
    }
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

    DigitalUniverseModule::ProgramObjectManager.release(
        "RenderableBillboardsCloud_Polygon"
    );
    _renderToPolygonProgram = nullptr;

    DigitalUniverseModule::TextureManager.release(_spriteTexture);
    _spriteTexture = nullptr;

    if (_hasPolygon) {
        _polygonTexture = nullptr;
        glDeleteTextures(1, &_pTexture);
    }
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

    const float minBillboardSize = _sizeSettings.billboardMinMaxSize.value().x; // in pixels
    const float maxBillboardSize = _sizeSettings.billboardMinMaxSize.value().y; // in pixels
    _program->setUniform(_uniformCache.minBillboardSize, minBillboardSize);
    _program->setUniform(_uniformCache.maxBillboardSize, maxBillboardSize);
    _program->setUniform(_uniformCache.color, _pointColor);
    _program->setUniform(_uniformCache.alphaValue, opacity());
    _program->setUniform(_uniformCache.scaleFactor, _sizeSettings.scaleFactor);
    _program->setUniform(_uniformCache.up, glm::vec3(orthoUp));
    _program->setUniform(_uniformCache.right, glm::vec3(orthoRight));
    _program->setUniform(_uniformCache.fadeInValue, fadeInVariable);

    _program->setUniform(
        _uniformCache.correctionSizeEndDistance,
        _sizeSettings.correctionSizeEndDistance
    );
    _program->setUniform(_uniformCache.correctionSizeFactor, _sizeSettings.correctionSizeFactor);

    _program->setUniform(_uniformCache.enabledRectSizeControl, _sizeSettings.pixelSizeControl);

    _program->setUniform(_uniformCache.hasDvarScaling, _hasDatavarSize);

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    _program->setUniform(_uniformCache.screenSize, glm::vec2(viewport[2], viewport[3]));

    ghoul::opengl::TextureUnit textureUnit;
    textureUnit.activate();
    if (_hasPolygon) {
        glBindTexture(GL_TEXTURE_2D, _pTexture);
    }
    else if (_spriteTexture) {
        _spriteTexture->bind();
    }
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
    float fadeInVar = 1.f;
    if (!_disableFadeInDistance) {
        float distCamera = static_cast<float>(glm::length(data.camera.positionVec3()));
        const glm::vec2 fadeRange = _fadeInDistances;
        const float a = static_cast<float>(
            1.f / ((fadeRange.y - fadeRange.x) * toMeter(_unit))
        );
        const float b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        const float funcValue = a * distCamera + b;
        fadeInVar *= funcValue > 1.f ? 1.f : funcValue;

        if (funcValue < 0.01f) {
            return;
        }
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

    int attibutesPerPoint = 4;
    if (_hasColorMapFile) {
        attibutesPerPoint += 4;
    }
    if (_hasDatavarSize) {
        attibutesPerPoint += 1;
    }

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(GL_ARRAY_BUFFER, size * sizeof(float), slice.data(), GL_STATIC_DRAW);

    int attributeCount = 0;

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
    attributeCount += 4;

    if (_hasColorMapFile) {
        GLint colorMapAttrib = _program->attributeLocation("in_colormap");
        glEnableVertexAttribArray(colorMapAttrib);
        glVertexAttribPointer(
            colorMapAttrib,
            4,
            GL_FLOAT,
            GL_FALSE,
            attibutesPerPoint * sizeof(float),
            reinterpret_cast<void*>(attributeCount * sizeof(float))
        );
        attributeCount += 4;
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
            reinterpret_cast<void*>(attributeCount * sizeof(float))
        );
        attributeCount += 1;
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

std::vector<float> RenderableBillboardsCloud::createDataSlice() {
    ZoneScoped;

    if (_dataset.entries.empty()) {
        return std::vector<float>();
    }

    std::vector<float> result;
    if (_hasColorMapFile) {
        result.reserve(8 * _dataset.entries.size());
    }
    else {
        result.reserve(4 * _dataset.entries.size());
    }

    // what datavar in use for the index color
    int colorMapInUse = _hasColorMapFile ?
        _dataset.index(_colorMapSettings.colorOption.option().description) : 0;

    // what datavar in use for the size scaling (if present)
    int sizeScalingInUse = _hasDatavarSize ?
        _dataset.index(_sizeFromData.datavarSizeOption.option().description) : -1;

    float minColorIdx = std::numeric_limits<float>::max();
    float maxColorIdx = -std::numeric_limits<float>::max();
    for (const speck::Dataset::Entry& e : _dataset.entries) {
        if (e.data.size() > 0) {
            float color = e.data[colorMapInUse];
            minColorIdx = std::min(color, minColorIdx);
            maxColorIdx = std::max(color, maxColorIdx);
        } else {
            minColorIdx = 0;
            maxColorIdx = 0;
        }
    }

    double maxRadius = 0.0;

    float biggestCoord = -1.f;
    for (const speck::Dataset::Entry& e : _dataset.entries) {
        const double unitMeter = toMeter(_unit);
        glm::vec4 position = glm::vec4(_transformationMatrix * glm::dvec4(
            glm::dvec3(e.position) * unitMeter, 1.0
        ));

        const double r = glm::length(position);
        maxRadius = std::max(maxRadius, r);

        // Positions
        for (int j = 0; j < 4; ++j) {
            result.push_back(position[j]);
        }

        // Colors
        if (_hasColorMapFile && !_colorMap.entries.empty()) {
            biggestCoord = std::max(biggestCoord, glm::compMax(position));
            // Note: if exact colormap option is not selected, the first color and the
            // last color in the colormap file are the outliers colors.
            float variableColor = e.data[colorMapInUse];

            float cmax, cmin;
            if (_colorMapSettings.colorRangeData.empty()) {
                cmax = maxColorIdx; // Max value of datavar used for the index color
                cmin = minColorIdx; // Min value of datavar used for the index color
            }
            else {
                glm::vec2 currentColorRange =
                    _colorMapSettings.colorRangeData[_colorMapSettings.colorOption.value()];
                cmax = currentColorRange.y;
                cmin = currentColorRange.x;
            }

            if (_colorMapSettings.isColorMapExact) {
                int colorIndex = static_cast<int>(variableColor + cmin);
                for (int j = 0; j < 4; ++j) {
                    result.push_back(_colorMap.entries[colorIndex][j]);
                }
            }
            else if (_colorMapSettings.useLinearFiltering) {
                float valueT = (variableColor - cmin) / (cmax - cmin); // in [0, 1)
                valueT = std::clamp(valueT, 0.f, 1.f);

                const float idx = valueT * (_colorMap.entries.size() - 1);
                const int floorIdx = static_cast<int>(std::floor(idx));
                const int ceilIdx = static_cast<int>(std::ceil(idx));

                const glm::vec4 floorColor = _colorMap.entries[floorIdx];
                const glm::vec4 ceilColor = _colorMap.entries[ceilIdx];

                glm::vec4 c = floorColor;

                if (floorColor != ceilColor) {
                    c = floorColor + idx * (ceilColor - floorColor);
                }

                result.push_back(c.r);
                result.push_back(c.g);
                result.push_back(c.b);
                result.push_back(c.a);
            }
            else { // Nearest neighbor
                float ncmap = static_cast<float>(_colorMap.entries.size());
                float normalization = ((cmax != cmin) && (ncmap > 2.f)) ?
                    (ncmap - 2.f) / (cmax - cmin) : 0;
                int colorIndex = static_cast<int>(
                    (variableColor - cmin) * normalization + 1.f
                );
                colorIndex = colorIndex < 0 ? 0 : colorIndex;
                colorIndex = colorIndex >= ncmap ?
                    static_cast<int>(ncmap - 1.f) : colorIndex;

                for (int j = 0; j < 4; ++j) {
                    result.push_back(_colorMap.entries[colorIndex][j]);
                }
            }
        }

        // Size data
        if (_hasDatavarSize) {
            result.push_back(e.data[sizeScalingInUse]);
        }
    }
    setBoundingSphere(maxRadius);
    _fadeInDistances.setMaxValue(glm::vec2(10.f * biggestCoord));
    return result;
}

void RenderableBillboardsCloud::createPolygonTexture() {
    ZoneScoped;

    LDEBUG("Creating Polygon Texture");

    glGenTextures(1, &_pTexture);
    glBindTexture(GL_TEXTURE_2D, _pTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 256, 256, 0, GL_RGBA, GL_BYTE, nullptr);

    renderToTexture(_pTexture, 256, 256);
}

void RenderableBillboardsCloud::renderToTexture(GLuint textureToRenderTo,
                                                GLuint textureWidth, GLuint textureHeight)
{
    LDEBUG("Rendering to Texture");

    // Saves initial Application's OpenGL State
    GLint defaultFBO;
    GLint viewport[4];
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);
    glGetIntegerv(GL_VIEWPORT, viewport);

    GLuint textureFBO;
    glGenFramebuffers(1, &textureFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, textureFBO);
    GLenum drawBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, drawBuffers);

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, textureToRenderTo, 0);

    glViewport(viewport[0], viewport[1], textureWidth, textureHeight);

    loadPolygonGeometryForRendering();
    renderPolygonGeometry(_polygonVao);

    // Restores Applications' OpenGL State
    glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

    glDeleteBuffers(1, &_polygonVbo);
    glDeleteVertexArrays(1, &_polygonVao);
    glDeleteFramebuffers(1, &textureFBO);
}

void RenderableBillboardsCloud::loadPolygonGeometryForRendering() {
    glGenVertexArrays(1, &_polygonVao);
    glGenBuffers(1, &_polygonVbo);
    glBindVertexArray(_polygonVao);
    glBindBuffer(GL_ARRAY_BUFFER, _polygonVbo);

    constexpr std::array<GLfloat, 4> VertexData = {
        //      x      y     z     w
        0.f, 0.f, 0.f, 1.f,
    };

    glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData.data(), GL_STATIC_DRAW);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), nullptr);
    glEnableVertexAttribArray(0);
    glBindVertexArray(0);
}

void RenderableBillboardsCloud::renderPolygonGeometry(GLuint vao) {
    std::unique_ptr<ghoul::opengl::ProgramObject> program =
        ghoul::opengl::ProgramObject::Build(
            "RenderableBillboardsCloud_Polygon",
            absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_vs.glsl"),
            absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_fs.glsl"),
            absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_gs.glsl")
        );

    program->activate();
    constexpr glm::vec4 Black = glm::vec4(0.f, 0.f, 0.f, 0.f);
    glClearBufferfv(GL_COLOR, 0, glm::value_ptr(Black));

    program->setUniform("sides", _polygonSides);
    program->setUniform("polygonColor", _pointColor);

    glBindVertexArray(vao);
    glDrawArrays(GL_POINTS, 0, 1);
    glBindVertexArray(0);

    program->deactivate();
}

} // namespace openspace
