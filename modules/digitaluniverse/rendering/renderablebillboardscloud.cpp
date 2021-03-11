/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/glm.h>
#include <glm/gtx/string_cast.hpp>
#include <array>
#include <fstream>
#include <cstdint>
#include <locale>
#include <optional>
#include <string>

namespace {
    constexpr const char* _loggerCat = "RenderableBillboardsCloud";
    constexpr const char* ProgramObjectName = "RenderableBillboardsCloud";
    constexpr const char* RenderToPolygonProgram = "RenderableBillboardsCloud_Polygon";

    constexpr const std::array<const char*, 20> UniformNames = {
        "cameraViewProjectionMatrix", "modelMatrix", "cameraPosition", "cameraLookUp",
        "renderOption", "minBillboardSize", "maxBillboardSize",
        "correctionSizeEndDistance", "correctionSizeFactor", "color", "alphaValue",
        "scaleFactor", "up", "right", "fadeInValue", "screenSize", "spriteTexture",
        "hasColorMap", "enabledRectSizeControl", "hasDvarScaling"
    };

    constexpr int8_t CurrentCacheVersion = 1;
    constexpr double PARSEC = 0.308567756E17;

    constexpr const int RenderOptionViewDirection = 0;
    constexpr const int RenderOptionPositionNormal = 1;

    constexpr openspace::properties::Property::PropertyInfo SpriteTextureInfo = {
        "Texture",
        "Point Sprite Texture",
        "The path to the texture that should be used as the point sprite."
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each point."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value is used to define the color of the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorMapInfo = {
        "ColorMap",
        "Color Map File",
        "The path to the color map file of the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo TextColorInfo = {
        "TextColor",
        "Text Color",
        "The text color for the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo TextOpacityInfo = {
        "TextOpacity",
        "Text Opacity",
        "Determines the transparency of the text label, where 1 is completely opaque "
        "and 0 fully transparent."
    };

    constexpr openspace::properties::Property::PropertyInfo TextSizeInfo = {
        "TextSize",
        "Text Size",
        "The text size for the astronomical object labels."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMinSizeInfo = {
        "TextMinSize",
        "Text Min Size",
        "The minimal size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMaxSizeInfo = {
        "TextMaxSize",
        "Text Max Size",
        "The maximum size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorOptionInfo = {
        "ColorOption",
        "Color Option",
        "This value determines which paramenter is used for default color of the "
        "astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo OptionColorRangeInfo = {
        "OptionColorRange",
        "Option Color Range",
        "This value changes the range of values to be mapped with the current color map."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeOptionInfo = {
        "SizeOption",
        "Size Option Variable",
        "This value determines which paramenter (datavar) is used for scaling "
        "of the astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Debug option for rendering of billboards and texts."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInDistancesInfo = {
        "FadeInDistances",
        "Fade-In Start and End Distances",
        "These values determine the initial and final distances from the center of "
        "our galaxy from which the astronomical object will start and end "
        "fading-in."
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInInfo = {
        "DisableFadeIn",
        "Disable Fade-in effect",
        "Enables/Disables the Fade-in effect."
    };

    constexpr openspace::properties::Property::PropertyInfo BillboardMaxSizeInfo = {
        "BillboardMaxSize",
        "Billboard Max Size in Pixels",
        "The max size (in pixels) for the billboard representing the astronomical "
        "object."
    };

    constexpr openspace::properties::Property::PropertyInfo BillboardMinSizeInfo = {
        "BillboardMinSize",
        "Billboard Min Size in Pixels",
        "The min size (in pixels) for the billboard representing the astronomical "
        "object."
    };

    constexpr openspace::properties::Property::PropertyInfo
    CorrectionSizeEndDistanceInfo = {
        "CorrectionSizeEndDistance",
        "Distance in 10^X meters where correction size stops acting.",
        "Distance in 10^X meters where correction size stops acting."
    };

    constexpr openspace::properties::Property::PropertyInfo CorrectionSizeFactorInfo = {
        "CorrectionSizeFactor",
        "Control variable for distance size.",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo PixelSizeControlInfo = {
        "EnablePixelSizeControl",
        "Enable pixel size control.",
        "Enable pixel size control for rectangular projections."
    };

    constexpr openspace::properties::Property::PropertyInfo UseLinearFiltering = {
        "UseLinearFiltering",
        "Use Linear Filtering",
        "Determines whether the provided color map should be sampled nearest neighbor "
        "(=off) or linearly (=on"
    };

    constexpr openspace::properties::Property::PropertyInfo SetRangeFromData = {
        "SetRangeFromData",
        "Set Data Range from Data",
        "Set the data range based on the available data"
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

        enum class RenderOption {
            ViewDirection [[codegen::key("Camera View Direction")]],
            PositionNormal [[codegen::key("Camera Position Normal")]]
        };
        // [[codegen::verbatim(RenderOptionInfo.description)]]
        std::optional<RenderOption> renderOption;

        enum class Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            GigalightYears [[codegen::key("Gly")]]
        };
        std::optional<Unit> unit;

        // [[codegen::verbatim(ScaleFactorInfo.description)]]
        std::optional<float> scaleFactor;

        // [[codegen::verbatim(ColorMapInfo.description)]]
        std::optional<std::string> colorMap;

        // Set a 1 to 1 relationship between the color index variable and the colormap
        // entrered value
        std::optional<bool> exactColorMap;

        // The number of sides for the polygon used to represent the astronomical object
        std::optional<int> polygonSides;

        // [[codgen::verbatim(DrawLabelInfo.description)]]
        std::optional<bool> drawLabels;

        // [[codgen::verbatim(TextColorInfo.description)]]
        std::optional<glm::vec3> textColor [[codegen::color()]];

        // [[codgen::verbatim(TextOpacityInfo.description)]]
        std::optional<float> textOpacity;

        // [[codgen::verbatim(TextSizeInfo.description)]]
        std::optional<float> textSize;

        // The path to the label file that contains information about the astronomical
        // objects being rendered
        std::optional<std::string> labelFile;

        // [[codgen::verbatim(LabelMinSizeInfo.description)]]
        std::optional<float> textMinSize;

        // [[codgen::verbatim(LabelMaxSizeInfo.description)]]
        std::optional<float> textMaxSize;

        // [[codgen::verbatim(ColorOptionInfo.description)]]
        std::optional<std::vector<std::string>> colorOption;

        // [[codgen::verbatim(SizeOptionInfo.description)]]
        std::optional<std::vector<std::string>> sizeOption;

        // This value determines the colormap ranges for the color parameters of the
        // astronomical objects
        std::optional<std::vector<glm::vec2>> colorRange;

        // Transformation matrix to be applied to each astronomical object
        std::optional<glm::dmat4x4> transformationMatrix;

        // [[codgen::verbatim(FadeInDistancesInfo.description)]]
        std::optional<glm::dvec2> fadeInDistances;

        // [[codgen::verbatim(DisableFadeInInfo.description)]]
        std::optional<bool> disableFadeIn;

        // [[codgen::verbatim(BillboardMaxSizeInfo.description)]]
        std::optional<float> billboardMaxSize;

        // [[codgen::verbatim(BillboardMinSizeInfo.description)]]
        std::optional<float> billboardMinSize;

        // [[codgen::verbatim(CorrectionSizeEndDistanceInfo.description)]]
        std::optional<float> correctionSizeEndDistance;

        // [[codgen::verbatim(CorrectionSizeFactorInfo.description)]]
        std::optional<float> correctionSizeFactor;

        // [[codgen::verbatim(PixelSizeControlInfo.description)]]
        std::optional<bool> enablePixelSizeControl;

        // [[codgen::verbatim(UseLinearFiltering.description)]]
        std::optional<bool> useLinearFiltering;
    };
#include "renderablebillboardscloud_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableBillboardsCloud::Documentation() {
    documentation::Documentation doc = codegen::doc<Parameters>();
    doc.id = "digitaluniverse_RenderableBillboardsCloud";
    return doc;
}

RenderableBillboardsCloud::RenderableBillboardsCloud(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 600.f)
    , _pointColor(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _spriteTexturePath(SpriteTextureInfo)
    , _textColor(TextColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _textOpacity(TextOpacityInfo, 1.f, 0.f, 1.f)
    , _textSize(TextSizeInfo, 8.f, 0.5f, 24.f)
    , _textMinSize(LabelMinSizeInfo, 8.f, 0.5f, 24.f)
    , _textMaxSize(LabelMaxSizeInfo, 20.f, 0.5f, 100.f)
    , _drawElements(DrawElementsInfo, true)
    , _drawLabels(DrawLabelInfo, false)
    , _pixelSizeControl(PixelSizeControlInfo, false)
    , _colorOption(ColorOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _optionColorRangeData(OptionColorRangeInfo, glm::vec2(0.f))

    , _datavarSizeOption(
        SizeOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _fadeInDistance(
        FadeInDistancesInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(100.f)
    )
    , _disableFadeInDistance(DisableFadeInInfo, true)
    , _billboardMaxSize(BillboardMaxSizeInfo, 400.f, 0.f, 1000.f)
    , _billboardMinSize(BillboardMinSizeInfo, 0.f, 0.f, 100.f)
    , _correctionSizeEndDistance(CorrectionSizeEndDistanceInfo, 17.f, 12.f, 25.f)
    , _correctionSizeFactor(CorrectionSizeFactorInfo, 8.f, 0.f, 20.f)
    , _useLinearFiltering(UseLinearFiltering, false)
    , _setRangeFromData(SetRangeFromData)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.file.has_value()) {
        _speckFile = absPath(*p.file);
    }
    _hasSpeckFile = p.file.has_value();

    _drawElements = p.drawElements.value_or(_drawElements);
    _drawElements.onChange([&]() { _hasSpeckFile = !_hasSpeckFile; });
    addProperty(_drawElements);

    _renderOption.addOption(RenderOptionViewDirection, "Camera View Direction");
    _renderOption.addOption(RenderOptionPositionNormal, "Camera Position Normal");

    if (p.renderOption.has_value()) {
        switch (*p.renderOption) {
            case Parameters::RenderOption::ViewDirection:
                _renderOption = RenderOptionViewDirection;
                break;
            case Parameters::RenderOption::PositionNormal:
                _renderOption = RenderOptionPositionNormal;
                break;
        }
    }
    else {
        _renderOption = RenderOptionViewDirection;
    }
    addProperty(_renderOption);

    if (p.unit.has_value()) {
        switch (*p.unit) {
            case Parameters::Unit::Meter:
                _unit = Meter;
                break;
            case Parameters::Unit::Kilometer:
                _unit = Kilometer;
                break;
            case Parameters::Unit::Parsec:
                _unit = Parsec;
                break;
            case Parameters::Unit::Kiloparsec:
                _unit = Kiloparsec;
                break;
            case Parameters::Unit::Megaparsec:
                _unit = Megaparsec;
                break;
            case Parameters::Unit::Gigaparsec:
                _unit = Gigaparsec;
                break;
            case Parameters::Unit::GigalightYears:
                _unit = GigalightYears;
                break;
        }
    }
    else {
        LWARNING("No unit given for RenderableBillboardsCloud. Using meters as units.");
        _unit = Meter;
    }

    if (p.texture.has_value()) {
        _spriteTexturePath = absPath(*p.texture);
        _spriteTexturePath.onChange([&]() { _spriteTextureIsDirty = true; });

        // @TODO (abock, 2021-01-31) I don't know why we only add this property if the
        // texture is given, but I think it's a bug
        addProperty(_spriteTexturePath);

    }
    _hasSpriteTexture = p.texture.has_value();


    if (p.colorMap.has_value()) {
        _colorMapFile = absPath(*p.colorMap);
        _hasColorMapFile = true;

        if (p.colorOption.has_value()) {
            std::vector<std::string> opts = *p.colorOption;
            for (size_t i = 0; i < opts.size(); ++i) {
                _colorOption.addOption(static_cast<int>(i), opts[i]);
                _optionConversionMap.insert({ static_cast<int>(i), opts[i] });
                _colorOptionString = opts[i];
            }
        }
        _colorOption.onChange([&]() {
            _dataIsDirty = true;
            const glm::vec2 colorRange = _colorRangeData[_colorOption.value()];
            _optionColorRangeData = colorRange;
            _colorOptionString = _optionConversionMap[_colorOption.value()];
        });
        addProperty(_colorOption);

        _colorRangeData = p.colorRange.value_or(_colorRangeData);
        if (!_colorRangeData.empty()) {
            _optionColorRangeData = _colorRangeData[_colorRangeData.size() - 1];
        }
        _optionColorRangeData.onChange([&]() {
            const glm::vec2 colorRange = _optionColorRangeData;
            _colorRangeData[_colorOption.value()] = colorRange;
            _dataIsDirty = true;
        });
        addProperty(_optionColorRangeData);

        _isColorMapExact = p.exactColorMap.value_or(_isColorMapExact);
    }
    else {
        _pointColor = p.color;
        _pointColor.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_pointColor);
    }

    addProperty(_opacity);

    _scaleFactor = p.scaleFactor.value_or(_scaleFactor);
    addProperty(_scaleFactor);

    if (p.sizeOption.has_value()) {
        std::vector<std::string> opts = *p.sizeOption;
        for (size_t i = 0; i < opts.size(); ++i) {
            _datavarSizeOption.addOption(static_cast<int>(i), opts[i]);
            _optionConversionSizeMap.insert({ static_cast<int>(i), opts[i] });
            _datavarSizeOptionString = opts[i];
        }

        _datavarSizeOption.onChange([&]() {
            _dataIsDirty = true;
            _datavarSizeOptionString = _optionConversionSizeMap[_datavarSizeOption];
        });
        addProperty(_datavarSizeOption);

        _hasDatavarSize = true;
    }

    _polygonSides = p.polygonSides.value_or(_polygonSides);
    _hasPolygon = p.polygonSides.has_value();

    if (p.labelFile.has_value()) {
        _drawLabels = p.drawLabels.value_or(_drawLabels);
        addProperty(_drawLabels);

        _labelFile = absPath(*p.labelFile);
        _hasLabel = true;

        _textColor = p.textColor.value_or(_textColor);
        _hasLabel = p.textColor.has_value();
        _textColor.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_textColor);
        _textColor.onChange([&]() { _textColorIsDirty = true; });

        _textOpacity = p.textOpacity.value_or(_textOpacity);
        addProperty(_textOpacity);

        _textSize = p.textSize.value_or(_textSize);
        addProperty(_textSize);

        _textMinSize = p.textMinSize.value_or(_textMinSize);
        addProperty(_textMinSize);

        _textMaxSize = p.textMaxSize.value_or(_textMaxSize);
        addProperty(_textMaxSize);
    }

    _transformationMatrix = p.transformationMatrix.value_or(_transformationMatrix);

    if (p.fadeInDistances.has_value()) {
        _fadeInDistance = *p.fadeInDistances;
        addProperty(_fadeInDistance);

        _disableFadeInDistance = false;
        addProperty(_disableFadeInDistance);
    }

    _billboardMaxSize = p.billboardMaxSize.value_or(_billboardMaxSize);
    addProperty(_billboardMaxSize);

    _billboardMinSize = p.billboardMinSize.value_or(_billboardMinSize);
    addProperty(_billboardMinSize);

    _correctionSizeEndDistance =
        p.correctionSizeEndDistance.value_or(_correctionSizeEndDistance);
    addProperty(_correctionSizeEndDistance);

    _correctionSizeFactor = p.correctionSizeFactor.value_or(_correctionSizeFactor);
    if (p.correctionSizeFactor.has_value()) {
        addProperty(_correctionSizeFactor);
    }

    _pixelSizeControl = p.enablePixelSizeControl.value_or(_pixelSizeControl);
    if (p.enablePixelSizeControl.has_value()) {
        addProperty(_pixelSizeControl);
    }

    _setRangeFromData.onChange([this]() {
        const int colorMapInUse =
            _hasColorMapFile ? _variableDataPositionMap[_colorOptionString] : 0;

        float minValue = std::numeric_limits<float>::max();
        float maxValue = std::numeric_limits<float>::min();
        for (size_t i = 0; i < _fullData.size(); i += _nValuesPerAstronomicalObject) {
            float colorIdx = _fullData[i + 3 + colorMapInUse];
            maxValue = colorIdx >= maxValue ? colorIdx : maxValue;
            minValue = colorIdx < minValue ? colorIdx : minValue;
        }

        _optionColorRangeData = glm::vec2(minValue, maxValue);
    });
    addProperty(_setRangeFromData);

    _useLinearFiltering = p.useLinearFiltering.value_or(_useLinearFiltering);
    _useLinearFiltering.onChange([&]() { _dataIsDirty = true; });
    addProperty(_useLinearFiltering);
}

bool RenderableBillboardsCloud::isReady() const {
    return ((_program != nullptr) && (!_fullData.empty())) || (!_labelData.empty());
}

void RenderableBillboardsCloud::initialize() {
    ZoneScoped

    bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
    }

    if (!_colorOptionString.empty() && (_colorRangeData.size() > 1)) {
        // Following DU behavior here. The last colormap variable
        // entry is the one selected by default.
        _colorOption.setValue(static_cast<int>(_colorRangeData.size() - 1));
    }

    setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
}

void RenderableBillboardsCloud::initializeGL() {
    ZoneScoped

    _program = DigitalUniverseModule::ProgramObjectManager.request(
        ProgramObjectName,
        []() {
            return global::renderEngine->buildRenderProgram(
                ProgramObjectName,
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboard_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboard_fs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboard_gs.glsl")
            );
        }
    );

    _renderToPolygonProgram = DigitalUniverseModule::ProgramObjectManager.request(
        RenderToPolygonProgram,
        []() {
            return ghoul::opengl::ProgramObject::Build(
                RenderToPolygonProgram,
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

    if (_hasLabel) {
        if (_font == nullptr) {
            size_t _fontSize = 50;
            _font = global::fontManager->font(
                "Mono",
                static_cast<float>(_fontSize),
                ghoul::fontrendering::FontManager::Outline::Yes,
                ghoul::fontrendering::FontManager::LoadGlyphs::No
            );
        }
    }
}

void RenderableBillboardsCloud::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    DigitalUniverseModule::ProgramObjectManager.release(
        ProgramObjectName,
        [](ghoul::opengl::ProgramObject* p) {
            global::renderEngine->removeRenderProgram(p);
        }
    );
    _program = nullptr;

    DigitalUniverseModule::ProgramObjectManager.release(RenderToPolygonProgram);
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
        glm::mat4(
            glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix()
        )
    );
    _program->setUniform(_uniformCache.minBillboardSize, _billboardMinSize); // in pixels
    _program->setUniform(_uniformCache.maxBillboardSize, _billboardMaxSize); // in pixels
    _program->setUniform(_uniformCache.color, _pointColor);
    _program->setUniform(_uniformCache.alphaValue, _opacity);
    _program->setUniform(_uniformCache.scaleFactor, _scaleFactor);
    _program->setUniform(_uniformCache.up, glm::vec3(orthoUp));
    _program->setUniform(_uniformCache.right, glm::vec3(orthoRight));
    _program->setUniform(_uniformCache.fadeInValue, fadeInVariable);

    _program->setUniform(
        _uniformCache.correctionSizeEndDistance,
        _correctionSizeEndDistance
    );
    _program->setUniform(_uniformCache.correctionSizeFactor, _correctionSizeFactor);

    _program->setUniform(_uniformCache.enabledRectSizeControl, _pixelSizeControl);

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
    _program->setUniform(_uniformCache.hasColormap, _hasColorMapFile);

    glBindVertexArray(_vao);
    const GLsizei nAstronomicalObjects = static_cast<GLsizei>(
        _fullData.size() / _nValuesPerAstronomicalObject
    );
    glDrawArrays(GL_POINTS, 0, nAstronomicalObjects);

    glBindVertexArray(0);
    _program->deactivate();

    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderableBillboardsCloud::renderLabels(const RenderData& data,
                                             const glm::dmat4& modelViewProjectionMatrix,
                                             const glm::dvec3& orthoRight,
                                             const glm::dvec3& orthoUp,
                                             float fadeInVariable)
{
    float scale = 0.f;
    switch (_unit) {
        case Meter:
            scale = 1.f;
            break;
        case Kilometer:
            scale = 1e3f;
            break;
        case Parsec:
            scale = static_cast<float>(PARSEC);
            break;
        case Kiloparsec:
            scale = static_cast<float>(1e3 * PARSEC);
            break;
        case Megaparsec:
            scale = static_cast<float>(1e6 * PARSEC);
            break;
        case Gigaparsec:
            scale = static_cast<float>(1e9 * PARSEC);
            break;
        case GigalightYears:
            scale = static_cast<float>(306391534.73091 * PARSEC);
            break;
    }

    glm::vec4 textColor = glm::vec4(
        glm::vec3(_textColor),
        _textOpacity * fadeInVariable
    );

    ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo;
    labelInfo.orthoRight = orthoRight;
    labelInfo.orthoUp = orthoUp;
    labelInfo.minSize = static_cast<int>(_textMinSize);
    labelInfo.maxSize = static_cast<int>(_textMaxSize);
    labelInfo.cameraPos = data.camera.positionVec3();
    labelInfo.cameraLookUp = data.camera.lookUpVectorWorldSpace();
    labelInfo.renderType = _renderOption;
    labelInfo.mvpMatrix = modelViewProjectionMatrix;
    labelInfo.scale = pow(10.f, _textSize);
    labelInfo.enableDepth = true;
    labelInfo.enableFalseDepth = false;

    for (const std::pair<glm::vec3, std::string>& pair : _labelData) {
        //glm::vec3 scaledPos(_transformationMatrix * glm::dvec4(pair.first, 1.0));
        glm::vec3 scaledPos(pair.first);
        scaledPos *= scale;
        ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
            *_font,
            scaledPos,
            pair.second,
            textColor,
            labelInfo
        );
    }
}

void RenderableBillboardsCloud::render(const RenderData& data, RendererTasks&) {
    float scale = 0.f;
    switch (_unit) {
        case Meter:
            scale = 1.f;
            break;
        case Kilometer:
            scale = 1e3f;
            break;
        case Parsec:
            scale = static_cast<float>(PARSEC);
            break;
        case Kiloparsec:
            scale = static_cast<float>(1e3 * PARSEC);
            break;
        case Megaparsec:
            scale = static_cast<float>(1e6 * PARSEC);
            break;
        case Gigaparsec:
            scale = static_cast<float>(1e9 * PARSEC);
            break;
        case GigalightYears:
            scale = static_cast<float>(306391534.73091 * PARSEC);
            break;
    }

    float fadeInVariable = 1.f;
    if (!_disableFadeInDistance) {
        float distCamera = static_cast<float>(glm::length(data.camera.positionVec3()));
        const glm::vec2 fadeRange = _fadeInDistance;
        const float a = 1.f / ((fadeRange.y - fadeRange.x) * scale);
        const float b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        const float funcValue = a * distCamera + b;
        fadeInVariable *= funcValue > 1.f ? 1.f : funcValue;

        if (funcValue < 0.01f) {
            return;
        }
    }

    glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    glm::mat4 projectionMatrix = data.camera.projectionMatrix();

    glm::dmat4 modelViewProjectionMatrix = glm::dmat4(projectionMatrix) * modelViewMatrix;

    glm::dvec3 cameraViewDirectionWorld = -data.camera.viewDirectionWorldSpace();
    glm::dvec3 cameraUpDirectionWorld = data.camera.lookUpVectorWorldSpace();
    glm::dvec3 orthoRight = glm::normalize(
        glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
    );
    if (orthoRight == glm::dvec3(0.0)) {
        glm::dvec3 otherVector(
            cameraUpDirectionWorld.y,
            cameraUpDirectionWorld.x,
            cameraUpDirectionWorld.z
        );
        orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
    }
    glm::dvec3 orthoUp = glm::normalize(glm::cross(cameraViewDirectionWorld, orthoRight));

    if (_hasSpeckFile && _drawElements) {
        renderBillboards(
            data,
            modelMatrix,
            orthoRight,
            orthoUp,
            fadeInVariable
        );
    }

    if (_drawLabels && _hasLabel) {
        renderLabels(
            data,
            modelViewProjectionMatrix,
            orthoRight,
            orthoUp,
            fadeInVariable
        );
    }
}

void RenderableBillboardsCloud::update(const UpdateData&) {
    ZoneScoped

    if (_dataIsDirty && _hasSpeckFile) {
        ZoneScopedN("Data dirty")
        TracyGpuZone("Data dirty")
        LDEBUG("Regenerating data");

        createDataSlice();

        int size = static_cast<int>(_slicedData.size());

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
        glBufferData(
            GL_ARRAY_BUFFER,
            size * sizeof(float),
            &_slicedData[0],
            GL_STATIC_DRAW
        );
        GLint positionAttrib = _program->attributeLocation("in_position");

        if (_hasColorMapFile && _hasDatavarSize) {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribPointer(
                positionAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                sizeof(float) * 9,
                nullptr
            );

            GLint colorMapAttrib = _program->attributeLocation("in_colormap");
            glEnableVertexAttribArray(colorMapAttrib);
            glVertexAttribPointer(
                colorMapAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                sizeof(float) * 9,
                reinterpret_cast<void*>(sizeof(float) * 4)
            );

            GLint dvarScalingAttrib = _program->attributeLocation("in_dvarScaling");
            glEnableVertexAttribArray(dvarScalingAttrib);
            glVertexAttribPointer(
                dvarScalingAttrib,
                1,
                GL_FLOAT,
                GL_FALSE,
                sizeof(float) * 9,
                reinterpret_cast<void*>(sizeof(float) * 8)
            );
        }
        else if (_hasColorMapFile) {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribPointer(
                positionAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                sizeof(float) * 8,
                nullptr
            );

            GLint colorMapAttrib = _program->attributeLocation("in_colormap");
            glEnableVertexAttribArray(colorMapAttrib);
            glVertexAttribPointer(
                colorMapAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                sizeof(float) * 8,
                reinterpret_cast<void*>(sizeof(float) * 4)
            );
        }
        else if (_hasDatavarSize) {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribPointer(
                positionAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                sizeof(float) * 8,
                nullptr
            );

            GLint dvarScalingAttrib = _program->attributeLocation("in_dvarScaling");
            glEnableVertexAttribArray(dvarScalingAttrib);
            glVertexAttribPointer(
                dvarScalingAttrib,
                1,
                GL_FLOAT,
                GL_FALSE,
                sizeof(float) * 5,
                reinterpret_cast<void*>(sizeof(float) * 4)
            );
        }
        else {
            glEnableVertexAttribArray(positionAttrib);
            glVertexAttribPointer(
                positionAttrib,
                4,
                GL_FLOAT,
                GL_FALSE,
                0,
                nullptr
            );
        }

        glBindVertexArray(0);

        _dataIsDirty = false;
    }

    if (_hasSpriteTexture && _spriteTextureIsDirty && !_spriteTexturePath.value().empty())
    {
        ZoneScopedN("Sprite texture")
        TracyGpuZone("Sprite texture")

        ghoul::opengl::Texture* texture = _spriteTexture;

        unsigned int hash = ghoul::hashCRC32File(_spriteTexturePath);

        _spriteTexture = DigitalUniverseModule::TextureManager.request(
            std::to_string(hash),
            [path = _spriteTexturePath]() -> std::unique_ptr<ghoul::opengl::Texture> {
                LINFO(fmt::format("Loaded texture from '{}'", absPath(path)));
                std::unique_ptr<ghoul::opengl::Texture> t =
                    ghoul::io::TextureReader::ref().loadTexture(absPath(path));
                t->uploadTexture();
                t->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
                t->purgeFromRAM();
                return t;
            }
        );

        DigitalUniverseModule::TextureManager.release(texture);
        _spriteTextureIsDirty = false;
    }
}

bool RenderableBillboardsCloud::loadData() {
    bool success = true;

    success &= loadSpeckData();

    if (_hasColorMapFile) {
        if (!_hasSpeckFile) {
            success = true;
        }
        success &= readColorMapFile();
    }

    success &= loadLabelData();

    return success;
}

bool RenderableBillboardsCloud::loadSpeckData() {
    if (!_hasSpeckFile) {
        return true;
    }
    bool success = true;
    const std::string& cachedFile = FileSys.cacheManager()->cachedFilename(
        ghoul::filesystem::File(_speckFile),
        "RenderableDUMeshes|" + identifier(),
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    const bool hasCachedFile = FileSys.fileExists(cachedFile);
    if (hasCachedFile) {
        LINFO(fmt::format(
            "Cached file '{}' used for Speck file '{}'",
            cachedFile, _speckFile
        ));

        success = loadCachedFile(cachedFile);
        if (success) {
            return true;
        }
        else {
            FileSys.cacheManager()->removeCacheFile(_speckFile);
            // Intentional fall-through to the 'else' to generate the cache
            // file for the next run
        }
    }
    else {
        LINFO(fmt::format("Cache for Speck file '{}' not found", _speckFile));
    }
    LINFO(fmt::format("Loading Speck file '{}'", _speckFile));

    success = readSpeckFile();
    if (!success) {
        return false;
    }

    success &= saveCachedFile(cachedFile);
    return success;
}

bool RenderableBillboardsCloud::loadLabelData() {
    if (_labelFile.empty()) {
        return true;
    }
    bool success = true;
    // I disabled the cache as it didn't work on Mac --- abock
    const std::string& cachedFile = FileSys.cacheManager()->cachedFilename(
        ghoul::filesystem::File(_labelFile),
        ghoul::filesystem::CacheManager::Persistent::Yes
    );
    if (!_hasSpeckFile && !_hasColorMapFile) {
        success = true;
    }
    const bool hasCachedFile = FileSys.fileExists(cachedFile);
    if (hasCachedFile) {
        LINFO(fmt::format(
            "Cached file '{}' used for Label file '{}'",
            cachedFile, _labelFile
        ));

        success &= loadCachedFile(cachedFile);
        if (!success) {
            FileSys.cacheManager()->removeCacheFile(_labelFile);
            // Intentional fall-through to the 'else' to generate the cache
            // file for the next run
        }
    }
    else {
        LINFO(fmt::format("Cache for Label file '{}' not found", _labelFile));
        LINFO(fmt::format("Loading Label file '{}'", _labelFile));

        success &= readLabelFile();
        if (!success) {
            return false;
        }
    }

    return success;
}

bool RenderableBillboardsCloud::readSpeckFile() {
    std::ifstream file(_speckFile);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Speck file '{}'", _speckFile));
        return false;
    }

    _nValuesPerAstronomicalObject = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        std::getline(file, line);

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        if (line.empty() || line[0] == '#') {
            continue;
        }

        if (line.substr(0, 7) != "datavar" &&
            line.substr(0, 10) != "texturevar" &&
            line.substr(0, 7) != "texture" &&
            line.substr(0, 10) != "polyorivar" &&
            line.substr(0, 10) != "maxcomment")
        {
            // Started reading data
            break;
        }

        if (line.substr(0, 7) == "datavar") {
            // datavar lines are structured as follows:
            // datavar # description
            // where # is the index of the data variable; so if we repeatedly overwrite
            // the 'nValues' variable with the latest index, we will end up with the total
            // number of values (+3 since X Y Z are not counted in the Speck file index)
            std::stringstream str(line);

            std::string dummy;
            str >> dummy; // command
            str >> _nValuesPerAstronomicalObject; // variable index
            dummy.clear();
            str >> dummy; // variable name

            _variableDataPositionMap.insert({ dummy, _nValuesPerAstronomicalObject });

            // We want the number, but the index is 0 based
            _nValuesPerAstronomicalObject += 1;
        }
    }

    _nValuesPerAstronomicalObject += 3; // X Y Z are not counted in the Speck file indices



    do {
        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        if (line.empty()) {
            std::getline(file, line);
            continue;
        }
        else if (line[0] == '#') {
            std::getline(file, line);
            continue;
        }

        std::stringstream str(line);
        std::vector<float> values(_nValuesPerAstronomicalObject);

        for (int i = 0; i < _nValuesPerAstronomicalObject; ++i) {
            str >> values[i];
        }

        _fullData.insert(_fullData.end(), values.begin(), values.end());

        // reads new line
        std::getline(file, line);
    } while (!file.eof());

    return true;
}

bool RenderableBillboardsCloud::readColorMapFile() {
    std::string _file = _colorMapFile;
    std::ifstream file(_file);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Color Map file '{}'", _file));
        return false;
    }

    std::size_t numberOfColors = 0;

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        // std::streampos position = file.tellg();
        std::getline(file, line);

        if (line[0] == '#' || line.empty()) {
            continue;
        }

        // Initial number of colors
        std::locale loc;
        if (std::isdigit(line[0], loc)) {
            std::string::size_type sz;
            numberOfColors = std::stoi(line, &sz);
            break;
        }
        else if (file.eof()) {
            return false;
        }
    }

    for (size_t i = 0; i < numberOfColors; ++i) {
        std::getline(file, line);
        std::stringstream str(line);

        glm::vec4 color;
        // Each color in the colormap must be defined as (R,G,B,A)
        for (int j = 0; j < 4; ++j) {
            str >> color[j];
        }

        _colorMapData.push_back(color);
    }

    return true;
}

bool RenderableBillboardsCloud::readLabelFile() {
    std::string _file = _labelFile;
    std::ifstream file(_file);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Label file '{}'", _file));
        return false;
    }

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        std::streampos position = file.tellg();
        std::getline(file, line);

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        if (line.empty() || line[0] == '#') {
            continue;
        }

        if (line.substr(0, 9) != "textcolor") {
            // we read a line that doesn't belong to the header, so we have to jump back
            // before the beginning of the current line
            file.seekg(position);
            continue;
        }

        if (line.substr(0, 9) == "textcolor") {
            // textcolor lines are structured as follows:
            // textcolor # description
            // where # is color text defined in configuration file
            std::stringstream str(line);

            // TODO: handle cases of labels with different colors
            break;
        }
    }


    do {
        std::vector<float> values(_nValuesPerAstronomicalObject);

        std::getline(file, line);

        // Guard against wrong line endings (copying files from Windows to Mac) causes
        // lines to have a final \r
        if (!line.empty() && line.back() == '\r') {
            line = line.substr(0, line.length() - 1);
        }

        if (line.empty()) {
            continue;
        }

        std::stringstream str(line);

        glm::vec3 position = glm::vec3(0.f);
        for (int j = 0; j < 3; ++j) {
            str >> position[j];
        }

        std::string dummy;
        str >> dummy; // text keyword

        std::string label;
        str >> label;
        dummy.clear();

        while (str >> dummy) {
            if (dummy == "#") {
                break;
            }

            label += " " + dummy;
            dummy.clear();
        }

        glm::vec3 transformedPos = glm::vec3(
            _transformationMatrix * glm::dvec4(position, 1.0)
        );
        _labelData.emplace_back(std::make_pair(transformedPos, label));
    } while (!file.eof());

    return true;
}

bool RenderableBillboardsCloud::loadCachedFile(const std::string& file) {
    std::ifstream fileStream(file, std::ifstream::binary);
    if (!fileStream.good()) {
        LERROR(fmt::format("Error opening file '{}' for loading cache file", file));
        return false;
    }
    int8_t version = 0;
    fileStream.read(reinterpret_cast<char*>(&version), sizeof(int8_t));
    if (version != CurrentCacheVersion) {
        LINFO("The format of the cached file has changed: deleting old cache");
        fileStream.close();
        FileSys.deleteFile(file);
        return false;
    }

    int32_t nValues = 0;
    fileStream.read(reinterpret_cast<char*>(&nValues), sizeof(int32_t));
    fileStream.read(
        reinterpret_cast<char*>(&_nValuesPerAstronomicalObject),
        sizeof(int32_t)
    );

    _fullData.resize(nValues);
    fileStream.read(
        reinterpret_cast<char*>(&_fullData[0]),
        nValues * sizeof(_fullData[0])
    );

    if (_hasColorMapFile) {
        int32_t nItems = 0;
        fileStream.read(reinterpret_cast<char*>(&nItems), sizeof(int32_t));

        for (int i = 0; i < nItems; ++i) {
            int32_t keySize = 0;
            fileStream.read(reinterpret_cast<char*>(&keySize), sizeof(int32_t));
            std::vector<char> buffer(keySize);
            fileStream.read(buffer.data(), keySize);

            std::string key(buffer.begin(), buffer.end());
            int32_t value = 0;
            fileStream.read(reinterpret_cast<char*>(&value), sizeof(int32_t));

            _variableDataPositionMap.insert({ key, value });
        }
    }

    bool success = fileStream.good();
    return success;
}

bool RenderableBillboardsCloud::saveCachedFile(const std::string& file) const {
    std::ofstream fileStream(file, std::ofstream::binary);
    if (!fileStream.good()) {
        LERROR(fmt::format("Error opening file '{}' for save cache file", file));
        return false;
    }
    fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion), sizeof(int8_t));

    int32_t nValues = static_cast<int32_t>(_fullData.size());
    if (nValues == 0) {
        LERROR("Error writing cache: No values were loaded");
        return false;
    }
    fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

    int32_t nValuesPerAstronomicalObject = static_cast<int32_t>(
        _nValuesPerAstronomicalObject
    );
    fileStream.write(
        reinterpret_cast<const char*>(&nValuesPerAstronomicalObject),
        sizeof(int32_t)
    );

    size_t nBytes = nValues * sizeof(_fullData[0]);
    fileStream.write(reinterpret_cast<const char*>(&_fullData[0]), nBytes);

    if (_hasColorMapFile) {
        int32_t nItems = static_cast<int32_t>(_variableDataPositionMap.size());
        fileStream.write(reinterpret_cast<const char*>(&nItems), sizeof(int32_t));

        for (const std::pair<const std::string, int>& pair : _variableDataPositionMap) {
            int32_t keySize = static_cast<int32_t>(pair.first.size());
            fileStream.write(reinterpret_cast<const char*>(&keySize), sizeof(int32_t));
            fileStream.write(pair.first.data(), keySize);
            int32_t value = static_cast<int32_t>(pair.second);
            fileStream.write(reinterpret_cast<const char*>(&value), sizeof(int32_t));
        }
    }

    return fileStream.good();
}

void RenderableBillboardsCloud::createDataSlice() {
    ZoneScoped

    _slicedData.clear();
    if (_hasColorMapFile) {
        _slicedData.reserve(8 * (_fullData.size() / _nValuesPerAstronomicalObject));
    }
    else {
        _slicedData.reserve(4 * (_fullData.size() / _nValuesPerAstronomicalObject));
    }

    // what datavar in use for the index color
    int colorMapInUse =
        _hasColorMapFile ? _variableDataPositionMap[_colorOptionString] : 0;

    // what datavar in use for the size scaling (if present)
    int sizeScalingInUse = _hasDatavarSize ?
        _variableDataPositionMap[_datavarSizeOptionString] : -1;

    auto addDatavarSizeScalling = [&](size_t i, int datavarInUse) {
        _slicedData.push_back(_fullData[i + 3 + datavarInUse]);
    };

    auto addPosition = [&](const glm::vec4 &pos) {
        for (int j = 0; j < 4; ++j) {
            _slicedData.push_back(pos[j]);
        }
    };

    float minColorIdx = std::numeric_limits<float>::max();
    float maxColorIdx = std::numeric_limits<float>::min();

    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerAstronomicalObject) {
        float colorIdx = _fullData[i + 3 + colorMapInUse];
        maxColorIdx = colorIdx >= maxColorIdx ? colorIdx : maxColorIdx;
        minColorIdx = colorIdx < minColorIdx ? colorIdx : minColorIdx;
    }

    float biggestCoord = -1.f;
    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerAstronomicalObject) {
        glm::dvec4 transformedPos = _transformationMatrix * glm::dvec4(
            _fullData[i + 0],
            _fullData[i + 1],
            _fullData[i + 2],
            1.0
        );
        // W-normalization
        transformedPos /= transformedPos.w;
        glm::vec4 position(glm::vec3(transformedPos), static_cast<float>(_unit));

        if (_hasColorMapFile) {
            for (int j = 0; j < 4; ++j) {
                _slicedData.push_back(position[j]);
                biggestCoord = biggestCoord < position[j] ? position[j] : biggestCoord;
            }
            // Note: if exact colormap option is not selected, the first color and the
            // last color in the colormap file are the outliers colors.
            float variableColor = _fullData[i + 3 + colorMapInUse];

            float cmax, cmin;
            if (_colorRangeData.empty()) {
                cmax = maxColorIdx; // Max value of datavar used for the index color
                cmin = minColorIdx; // Min value of datavar used for the index color
            }
            else {
                glm::vec2 currentColorRange = _colorRangeData[_colorOption.value()];
                cmax = currentColorRange.y;
                cmin = currentColorRange.x;
            }

            if (_isColorMapExact) {
                int colorIndex = variableColor + cmin;
                for (int j = 0; j < 4; ++j) {
                    _slicedData.push_back(_colorMapData[colorIndex][j]);
                }
            }
            else {
                if (_useLinearFiltering) {
                    const float value = variableColor;

                    float valueT = (value - cmin) / (cmax - cmin); // in [0, 1)
                    valueT = std::clamp(valueT, 0.f, 1.f);

                    const float idx = valueT * (_colorMapData.size() - 1);
                    const int floorIdx = static_cast<int>(std::floor(idx));
                    const int ceilIdx = static_cast<int>(std::ceil(idx));

                    const glm::vec4 floorColor = _colorMapData[floorIdx];
                    const glm::vec4 ceilColor = _colorMapData[ceilIdx];

                    if (floorColor != ceilColor) {
                        const glm::vec4 c = floorColor + idx * (ceilColor - floorColor);
                        _slicedData.push_back(c.r);
                        _slicedData.push_back(c.g);
                        _slicedData.push_back(c.b);
                        _slicedData.push_back(c.a);
                    }
                    else {
                        _slicedData.push_back(floorColor.r);
                        _slicedData.push_back(floorColor.g);
                        _slicedData.push_back(floorColor.b);
                        _slicedData.push_back(floorColor.a);
                    }
                }
                else {
                    float ncmap = static_cast<float>(_colorMapData.size());
                    float normalization = ((cmax != cmin) && (ncmap > 2)) ?
                        (ncmap - 2) / (cmax - cmin) : 0;
                    int colorIndex = (variableColor - cmin) * normalization + 1;
                    colorIndex = colorIndex < 0 ? 0 : colorIndex;
                    colorIndex = colorIndex >= ncmap ? ncmap - 1 : colorIndex;

                    for (int j = 0; j < 4; ++j) {
                        _slicedData.push_back(_colorMapData[colorIndex][j]);
                    }
                }
            }

            if (_hasDatavarSize) {
                addDatavarSizeScalling(i, sizeScalingInUse);
            }
        }
        else if (_hasDatavarSize) {
            addDatavarSizeScalling(i, sizeScalingInUse);
            addPosition(position);
        }
        else {
            addPosition(position);
        }
    }
    _fadeInDistance.setMaxValue(glm::vec2(10.f * biggestCoord));
}

void RenderableBillboardsCloud::createPolygonTexture() {
    ZoneScoped

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

    const GLfloat vertex_data[] = {
        //      x      y     z     w
        0.f, 0.f, 0.f, 1.f,
    };

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertex_data), vertex_data, GL_STATIC_DRAW);
    glVertexAttribPointer(
        0,
        4,
        GL_FLOAT,
        GL_FALSE,
        sizeof(GLfloat) * 4,
        nullptr
    );
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
    static const float black[] = { 0.f, 0.f, 0.f, 0.f };
    glClearBufferfv(GL_COLOR, 0, black);

    program->setUniform("sides", _polygonSides);
    program->setUniform("polygonColor", _pointColor);

    glBindVertexArray(vao);
    glDrawArrays(GL_POINTS, 0, 1);
    glBindVertexArray(0);

    program->deactivate();
}

} // namespace openspace
