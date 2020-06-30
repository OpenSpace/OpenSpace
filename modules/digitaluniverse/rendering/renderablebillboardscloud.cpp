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

    constexpr const char* KeyFile = "File";
    constexpr const char* keyColor = "Color";
    constexpr const char* keyUnit = "Unit";
    constexpr const char* MeterUnit = "m";
    constexpr const char* KilometerUnit = "Km";
    constexpr const char* ParsecUnit = "pc";
    constexpr const char* KiloparsecUnit = "Kpc";
    constexpr const char* MegaparsecUnit = "Mpc";
    constexpr const char* GigaparsecUnit = "Gpc";
    constexpr const char* GigalightyearUnit = "Gly";

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

    constexpr openspace::properties::Property::PropertyInfo ExactColorMapInfo = {
        "ExactColorMap",
        "Exact Color Map File",
        "Set a 1 to 1 relationship between the color index variable and the colormap"
        " entrered value."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorRangeInfo = {
        "ColorRange",
        "Color Range",
        "This value determines the colormap ranges for the color parameters of the "
        "astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo PolygonSidesInfo = {
        "PolygonSides",
        "Polygon Sides",
        "The number of sides for the polygon used to represent the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo TextColorInfo = {
        "TextColor",
        "Text Color",
        "The text color for the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo TextSizeInfo = {
        "TextSize",
        "Text Size",
        "The text size for the astronomical object labels."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelFileInfo = {
        "LabelFile",
        "Label File",
        "The path to the label file that contains information about the astronomical "
        "objects being rendered."
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

    constexpr openspace::properties::Property::PropertyInfo SizeOptionInfo = {
        "SizeOption",
        "Size Option Variable",
        "This value determines which paramenter (datavar) is used for scaling "
        "of the astronomical objects."
    };

    constexpr openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to each astronomical object."
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
}  // namespace

namespace openspace {

documentation::Documentation RenderableBillboardsCloud::Documentation() {
    using namespace documentation;
    return {
        "RenderableBillboardsCloud",
        "digitaluniverse_RenderableBillboardsCloud",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableBillboardsCloud"),
                Optional::No
            },
            {
                KeyFile,
                new StringVerifier,
                Optional::Yes,
                "The path to the SPECK file that contains information about the "
                "astronomical object being rendered."
            },
            {
                keyColor,
                new Vector3Verifier<float>,
                Optional::No,
                "Astronomical Object Color (r,g,b)."
            },
            {
                SpriteTextureInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                SpriteTextureInfo.description
            },
            {
                ScaleFactorInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                ScaleFactorInfo.description
            },
            {
                ColorMapInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                ColorMapInfo.description
            },
            {
                ExactColorMapInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                ExactColorMapInfo.description
            },
            {
                PolygonSidesInfo.identifier,
                new IntVerifier,
                Optional::Yes,
                PolygonSidesInfo.description
            },
            {
                DrawLabelInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                DrawLabelInfo.description
            },
            {
                TextColorInfo.identifier,
                new DoubleVector4Verifier,
                Optional::Yes,
                TextColorInfo.description
            },
            {
                TextSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                TextSizeInfo.description
            },
            {
                LabelFileInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                LabelFileInfo.description
            },
            {
                LabelMinSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LabelMinSizeInfo.description
            },
            {
                LabelMaxSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LabelMaxSizeInfo.description
            },
            {
                ColorOptionInfo.identifier,
                new StringListVerifier,
                Optional::Yes,
                ColorOptionInfo.description
            },
            {
                SizeOptionInfo.identifier,
                new StringListVerifier,
                Optional::Yes,
                SizeOptionInfo.description
            },
            {
                ColorRangeInfo.identifier,
                new Vector2ListVerifier<float>,
                Optional::Yes,
                ColorRangeInfo.description
            },
            {
                TransformationMatrixInfo.identifier,
                new Matrix4x4Verifier<double>,
                Optional::Yes,
                TransformationMatrixInfo.description
            },
            {
                FadeInDistancesInfo.identifier,
                new Vector2Verifier<double>,
                Optional::Yes,
                FadeInDistancesInfo.description
            },
            {
                DisableFadeInInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                DisableFadeInInfo.description
            },
            {
                BillboardMaxSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                BillboardMaxSizeInfo.description
            },
            {
                BillboardMinSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                BillboardMinSizeInfo.description
            },
            {
                CorrectionSizeEndDistanceInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                CorrectionSizeEndDistanceInfo.description
            },
            {
                CorrectionSizeFactorInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                CorrectionSizeFactorInfo.description
            },
            {
                PixelSizeControlInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                PixelSizeControlInfo.description
            }
        }
    };
}

RenderableBillboardsCloud::RenderableBillboardsCloud(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 600.f)
    , _pointColor(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _spriteTexturePath(SpriteTextureInfo)
    , _textColor(
        TextColorInfo,
        glm::vec4(1.f, 1.f, 1.f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _textSize(TextSizeInfo, 8.f, 0.5f, 24.f)
    , _textMinSize(LabelMinSizeInfo, 8.f, 0.5f, 24.f)
    , _textMaxSize(LabelMaxSizeInfo, 20.f, 0.5f, 100.f)
    , _drawElements(DrawElementsInfo, true)
    , _drawLabels(DrawLabelInfo, false)
    , _pixelSizeControl(PixelSizeControlInfo, false)
    , _colorOption(ColorOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
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
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableBillboardsCloud"
    );

    if (dictionary.hasKey(KeyFile)) {
        _speckFile = absPath(dictionary.value<std::string>(KeyFile));
        _hasSpeckFile = true;
    }

    if (dictionary.hasKey(DrawElementsInfo.identifier)) {
        _drawElements = dictionary.value<bool>(DrawElementsInfo.identifier);
    }

    _drawElements.onChange([&]() { _hasSpeckFile = !_hasSpeckFile; });
    addProperty(_drawElements);

    _renderOption.addOption(RenderOptionViewDirection, "Camera View Direction");
    _renderOption.addOption(RenderOptionPositionNormal, "Camera Position Normal");

    _renderOption = RenderOptionPositionNormal;
    if (dictionary.hasKeyAndValue<std::string>(RenderOptionInfo.identifier)) {
        const std::string o = dictionary.value<std::string>(RenderOptionInfo.identifier);

        if (o == "Camera View Direction") {
            _renderOption = RenderOptionViewDirection;
        }
        else if (o == "Camera Position Normal") {
            _renderOption = RenderOptionPositionNormal;
        }
    }

    addProperty(_renderOption);

    if (dictionary.hasKey(keyUnit)) {
        std::string unit = dictionary.value<std::string>(keyUnit);
        if (unit == MeterUnit) {
            _unit = Meter;
        }
        else if (unit == KilometerUnit) {
            _unit = Kilometer;
        }
        else if (unit == ParsecUnit) {
            _unit = Parsec;
        }
        else if (unit == KiloparsecUnit) {
            _unit = Kiloparsec;
        }
        else if (unit == MegaparsecUnit) {
            _unit = Megaparsec;
        }
        else if (unit == GigaparsecUnit) {
            _unit = Gigaparsec;
        }
        else if (unit == GigalightyearUnit) {
            _unit = GigalightYears;
        }
        else {
            LWARNING(
                "No unit given for RenderableBillboardsCloud. Using meters as units."
            );
            _unit = Meter;
        }
    }

    if (dictionary.hasKey(SpriteTextureInfo.identifier)) {
        _spriteTexturePath = absPath(dictionary.value<std::string>(
            SpriteTextureInfo.identifier
        ));

        _spriteTexturePath.onChange([&]() { _spriteTextureIsDirty = true; });
        addProperty(_spriteTexturePath);

        _hasSpriteTexture = true;
    }

    if (dictionary.hasKey(ColorMapInfo.identifier)) {
        _colorMapFile = absPath(dictionary.value<std::string>(ColorMapInfo.identifier));
        _hasColorMapFile = true;

        if (dictionary.hasKey(ColorOptionInfo.identifier)) {
            ghoul::Dictionary colorOptionDataDic = dictionary.value<ghoul::Dictionary>(
                ColorOptionInfo.identifier
            );
            for (int i = 0; i < static_cast<int>(colorOptionDataDic.size()); ++i) {
                std::string colorMapInUseName(
                    colorOptionDataDic.value<std::string>(std::to_string(i + 1))
                );
                _colorOption.addOption(i, colorMapInUseName);
                _optionConversionMap.insert({ i, colorMapInUseName });
                _colorOptionString = colorMapInUseName;
            }
        }
        _colorOption.onChange([&]() {
            _dataIsDirty = true;
            _colorOptionString = _optionConversionMap[_colorOption.value()];
        });
        addProperty(_colorOption);

        if (dictionary.hasKey(ColorRangeInfo.identifier)) {
            ghoul::Dictionary rangeDataDict = dictionary.value<ghoul::Dictionary>(
                ColorRangeInfo.identifier
            );
            for (size_t i = 0; i < rangeDataDict.size(); ++i) {
                _colorRangeData.push_back(
                    rangeDataDict.value<glm::vec2>(std::to_string(i + 1))
                );
            }
        }

        if (dictionary.hasKey(ExactColorMapInfo.identifier)) {
            _isColorMapExact = dictionary.value<bool>(ExactColorMapInfo.identifier);
        }
    }
    else if (dictionary.hasKey(keyColor)) {
        _pointColor = dictionary.value<glm::vec3>(keyColor);
        _pointColor.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_pointColor);
    }

    addProperty(_opacity);

    if (dictionary.hasKey(ScaleFactorInfo.identifier)) {
        _scaleFactor = static_cast<float>(
            dictionary.value<double>(ScaleFactorInfo.identifier)
        );
    }
    addProperty(_scaleFactor);

    if (dictionary.hasKey(SizeOptionInfo.identifier)) {
        ghoul::Dictionary sizeOptionDataDic = dictionary.value<ghoul::Dictionary>(
            SizeOptionInfo.identifier
            );
        for (int i = 0; i < static_cast<int>(sizeOptionDataDic.size()); ++i) {
            std::string datavarSizeInUseName(
                sizeOptionDataDic.value<std::string>(std::to_string(i + 1))
            );
            _datavarSizeOption.addOption(i, datavarSizeInUseName);
            _optionConversionSizeMap.insert({ i, datavarSizeInUseName });
            _datavarSizeOptionString = datavarSizeInUseName;
        }

        _datavarSizeOption.onChange([&]() {
            _dataIsDirty = true;
            _datavarSizeOptionString = _optionConversionSizeMap[_datavarSizeOption];
        });
        addProperty(_datavarSizeOption);

        _hasDatavarSize = true;
    }

    if (dictionary.hasKey(PolygonSidesInfo.identifier)) {
        _polygonSides = static_cast<int>(
            dictionary.value<double>(PolygonSidesInfo.identifier)
        );
        _hasPolygon = true;
    }

    if (dictionary.hasKey(LabelFileInfo.identifier)) {
        if (dictionary.hasKey(DrawLabelInfo.identifier)) {
            _drawLabels = dictionary.value<bool>(DrawLabelInfo.identifier);
        }
        addProperty(_drawLabels);

        _labelFile = absPath(dictionary.value<std::string>(LabelFileInfo.identifier));
        _hasLabel = true;

        if (dictionary.hasKey(TextColorInfo.identifier)) {
            _textColor = dictionary.value<glm::vec4>(TextColorInfo.identifier);
            _hasLabel = true;
        }
        _textColor.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_textColor);
        _textColor.onChange([&]() { _textColorIsDirty = true; });


        if (dictionary.hasKey(TextSizeInfo.identifier)) {
            _textSize = dictionary.value<float>(TextSizeInfo.identifier);
        }
        addProperty(_textSize);

        if (dictionary.hasKey(LabelMinSizeInfo.identifier)) {
            _textMinSize = dictionary.value<float>(LabelMinSizeInfo.identifier);
        }
        addProperty(_textMinSize);

        if (dictionary.hasKey(LabelMaxSizeInfo.identifier)) {
            _textMaxSize = dictionary.value<float>(LabelMaxSizeInfo.identifier);
        }
        addProperty(_textMaxSize);
    }

    if (dictionary.hasKey(TransformationMatrixInfo.identifier)) {
        _transformationMatrix = dictionary.value<glm::dmat4>(
            TransformationMatrixInfo.identifier
        );
    }

    if (dictionary.hasKey(FadeInDistancesInfo.identifier)) {
        glm::vec2 v = dictionary.value<glm::vec2>(FadeInDistancesInfo.identifier);
        _fadeInDistance = v;
        _disableFadeInDistance = false;
        addProperty(_fadeInDistance);
        addProperty(_disableFadeInDistance);
    }

    if (dictionary.hasKey(BillboardMaxSizeInfo.identifier)) {
        _billboardMaxSize = static_cast<float>(
            dictionary.value<double>(BillboardMaxSizeInfo.identifier)
        );
    }
    addProperty(_billboardMaxSize);

    if (dictionary.hasKey(BillboardMinSizeInfo.identifier)) {
        _billboardMinSize = static_cast<float>(
            dictionary.value<double>(BillboardMinSizeInfo.identifier)
        );
    }
    addProperty(_billboardMinSize);

    if (dictionary.hasKey(CorrectionSizeEndDistanceInfo.identifier)) {
        _correctionSizeEndDistance = static_cast<float>(
            dictionary.value<double>(CorrectionSizeEndDistanceInfo.identifier)
        );
    }
    addProperty(_correctionSizeEndDistance);

    if (dictionary.hasKey(CorrectionSizeFactorInfo.identifier)) {
        _correctionSizeFactor = static_cast<float>(
            dictionary.value<double>(CorrectionSizeFactorInfo.identifier)
        );

        addProperty(_correctionSizeFactor);
    }

    if (dictionary.hasKey(PixelSizeControlInfo.identifier)) {
        _pixelSizeControl = dictionary.value<bool>(PixelSizeControlInfo.identifier);
        addProperty(_pixelSizeControl);
    }
}

bool RenderableBillboardsCloud::isReady() const {
    return ((_program != nullptr) && (!_fullData.empty())) || (!_labelData.empty());
}

void RenderableBillboardsCloud::initialize() {
    bool success = loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading data");
    }

    if (!_colorOptionString.empty() && (_colorRangeData.size() > 1)) {
        // Following DU behavior here. The last colormap variable
        // entry is the one selected by default.
        _colorOption.setValue(static_cast<int>(_colorRangeData.size() - 1));
    }

    setRenderBin(Renderable::RenderBin::Transparent);
}

void RenderableBillboardsCloud::initializeGL() {
    _program = DigitalUniverseModule::ProgramObjectManager.request(
        ProgramObjectName,
        []() {
            return global::renderEngine.buildRenderProgram(
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
            _font = global::fontManager.font(
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
            global::renderEngine.removeRenderProgram(p);
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
    glEnable(GL_DEPTH_TEST);

    // Saving current OpenGL state
    GLboolean blendEnabled = glIsEnabledi(GL_BLEND, 0);

    GLenum blendEquationRGB;
    glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);

    GLenum blendEquationAlpha;
    glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);

    GLenum blendDestAlpha;
    glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);

    GLenum blendDestRGB;
    glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);

    GLenum blendSrcAlpha;
    glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);

    GLenum blendSrcRGB;
    glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

    glEnablei(GL_BLEND, 0);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

    _program->activate();

    _program->setUniform(
        "screenSize",
        glm::vec2(global::renderEngine.renderingResolution())
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
    else {
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

    // Restores blending state
    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);

    if (!blendEnabled) {
        glDisablei(GL_BLEND, 0);
    }

    glDepthMask(true);

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

    glm::vec4 textColor = _textColor;
    textColor.a *= fadeInVariable;
    textColor.a *= _opacity;

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
    if (_dataIsDirty && _hasSpeckFile) {
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
        ghoul::opengl::Texture* t = _spriteTexture;

        unsigned int hash = ghoul::hashCRC32File(_spriteTexturePath);

        _spriteTexture = DigitalUniverseModule::TextureManager.request(
            std::to_string(hash),
            [path = _spriteTexturePath]() -> std::unique_ptr<ghoul::opengl::Texture> {
                LINFO(fmt::format("Loaded texture from '{}'", absPath(path)));
                std::unique_ptr<ghoul::opengl::Texture> t =
                    ghoul::io::TextureReader::ref().loadTexture(absPath(path));
                t->uploadTexture();
                t->setFilter(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
                return t;
            }
        );

        DigitalUniverseModule::TextureManager.release(t);
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
            int variableColor = static_cast<int>(_fullData[i + 3 + colorMapInUse]);
            int colorIndex = 0;

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
                colorIndex = variableColor + cmin;
            }
            else {
                float ncmap = static_cast<float>(_colorMapData.size());
                float normalization = ((cmax != cmin) && (ncmap > 2)) ?
                                      (ncmap - 2) / (cmax - cmin) : 0;
                colorIndex = (variableColor - cmin) * normalization + 1;
                colorIndex = colorIndex < 0 ? 0 : colorIndex;
                colorIndex = colorIndex >= ncmap ? ncmap - 1 : colorIndex;
            }

            for (int j = 0; j < 4; ++j) {
                _slicedData.push_back(_colorMapData[colorIndex][j]);
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
