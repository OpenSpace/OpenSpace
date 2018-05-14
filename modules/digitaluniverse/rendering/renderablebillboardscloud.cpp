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

#include <modules/digitaluniverse/rendering/renderablebillboardscloud.h>

#include <modules/digitaluniverse/digitaluniversemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/rendering/renderengine.h>

#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>

#include <glm/gtx/string_cast.hpp>
#include <ghoul/glm.h>

#include <array>
#include <fstream>
#include <stdint.h>
#include <locale>
#include <string>

namespace {
    constexpr const char* _loggerCat             = "RenderableBillboardsCloud";
    constexpr const char* ProgramObjectName      = "RenderableBillboardsCloud";
    constexpr const char* RenderToPolygonProgram = "RenderableBillboardsCloud_Polygon";

    constexpr const char* KeyFile           = "File";
    constexpr const char* keyColor          = "Color";
    constexpr const char* keyUnit           = "Unit";
    constexpr const char* MeterUnit         = "m";
    constexpr const char* KilometerUnit     = "Km";
    constexpr const char* ParsecUnit        = "pc";
    constexpr const char* KiloparsecUnit    = "Kpc";
    constexpr const char* MegaparsecUnit    = "Mpc";
    constexpr const char* GigaparsecUnit    = "Gpc";
    constexpr const char* GigalightyearUnit = "Gly";

    constexpr int8_t CurrentCacheVersion = 1;
    constexpr double PARSEC = 0.308567756E17;

    static const openspace::properties::Property::PropertyInfo SpriteTextureInfo = {
        "Texture",
        "Point Sprite Texture",
        "The path to the texture that should be used as the point sprite."
    };

    static const openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Transparency",
        "This value is a multiplicative factor that is applied to the transparency of "
        "all points."
    };

    static const openspace::properties::Property::PropertyInfo ScaleFactorInfo = {
        "ScaleFactor",
        "Scale Factor",
        "This value is used as a multiplicative factor that is applied to the apparent "
        "size of each point."
    };

    static const openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value is used to define the color of the astronomical object."
    };

    static const openspace::properties::Property::PropertyInfo ColorMapInfo = {
        "ColorMap",
        "Color Map File",
        "The path to the color map file of the astronomical object."
    };

    static const openspace::properties::Property::PropertyInfo PolygonSidesInfo = {
        "PolygonSides",
        "Polygon Sides",
        "The number of sides for the polygon used to represent the astronomical object."
    };

    static const openspace::properties::Property::PropertyInfo TextColorInfo = {
        "TextColor",
        "Text Color",
        "The text color for the astronomical object."
    };

    static const openspace::properties::Property::PropertyInfo TextSizeInfo = {
        "TextSize",
        "Text Size",
        "The text size for the astronomical object labels."
    };

    static const openspace::properties::Property::PropertyInfo LabelFileInfo = {
        "LabelFile",
        "Label File",
        "The path to the label file that contains information about the astronomical "
        "objects being rendered."
    };

    static const openspace::properties::Property::PropertyInfo LabelMinSizeInfo = {
        "TextMinSize",
        "Text Min Size",
        "The minimal size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    static const openspace::properties::Property::PropertyInfo LabelMaxSizeInfo = {
        "TextMaxSize",
        "Text Max Size",
        "The maximum size (in pixels) of the text for the labels for the astronomical "
        "objects being rendered."
    };

    static const openspace::properties::Property::PropertyInfo DrawElementsInfo = {
        "DrawElements",
        "Draw Elements",
        "Enables/Disables the drawing of the astronomical objects."
    };

    static const openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden."
    };

    static const openspace::properties::Property::PropertyInfo ColorOptionInfo = {
        "ColorOption",
        "Color Option",
        "This value determines which paramenter is used default color of the "
        "astronomical objects."
    };

    static const openspace::properties::Property::PropertyInfo ColorRangeInfo = {
        "ColorRange",
        "Color Range",
        "This value determines the color ranges for the color parameter of the "
        "astronomical objects."
    };

    static const openspace::properties::Property::PropertyInfo TransformationMatrixInfo =
    {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to each astronomical object."
    };

    static const openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOptionInfo",
        "Render Option",
        "Debug option for rendering of billboards and texts."
    };

    static const openspace::properties::Property::PropertyInfo FadeInDistancesInfo = {
        "FadeInDistances",
        "Fade-In Start and End Distances",
        "These values determine the initial and final distances from the center of "
        "our galaxy from which the astronomical object will start and end "
        "fading-in."
    };

    static const openspace::properties::Property::PropertyInfo DisableFadeInInfo = {
        "DisableFadeIn",
        "Disable Fade-in effect",
        "Enables/Disables the Fade-in effect."
    };

    static const openspace::properties::Property::PropertyInfo BillboardMaxSizeInfo = {
        "BillboardMaxSize",
        "Billboard Max Size in Pixels",
        "The max size (in pixels) for the billboard representing the astronomical "
        "object."
    };

    static const openspace::properties::Property::PropertyInfo BillboardMinSizeInfo = {
        "BillboardMinSize",
        "Billboard Min Size in Pixels",
        "The min size (in pixels) for the billboard representing the astronomical "
        "object."
    };

    static const openspace::properties::Property::PropertyInfo CorrectionSizeEndDistanceInfo = {
        "CorrectionSizeEndDistance",
        "Distance in 10^X meters where correction size stops acting.",
        "Distance in 10^X meters where correction size stops acting."
    };

    static const openspace::properties::Property::PropertyInfo CorrectionSizeFactorInfo = {
        "CorrectionSizeFactor",
        "Control variable for distance size.",
        ""
    };
    static const openspace::properties::Property::PropertyInfo PixelSizeControlInfo = {
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
                TransparencyInfo.identifier,
                new DoubleVerifier,
                Optional::No,
                TransparencyInfo.description
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
        , _hasSpeckFile(false)
        , _dataIsDirty(true)
        , _textColorIsDirty(true)
        , _hasSpriteTexture(false)
        , _spriteTextureIsDirty(true)
        , _hasColorMapFile(false)
        , _hasPolygon(false)
        , _hasLabel(false)
        , _labelDataIsDirty(true)
        , _polygonSides(0)
        , _pTexture(0)
        , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
        , _scaleFactor(ScaleFactorInfo, 1.f, 0.f, 600.f)
        , _pointColor(
            ColorInfo,
            glm::vec3(1.f, 0.4f, 0.2f),
            glm::vec3(0.f, 0.f, 0.f),
            glm::vec3(1.0f, 1.0f, 1.0f)
        )
        , _spriteTexturePath(SpriteTextureInfo)
        , _textColor(
            TextColorInfo,
            glm::vec4(1.0f, 1.0, 1.0f, 1.f),
            glm::vec4(0.f),
            glm::vec4(1.f)
        )
        , _textSize(TextSizeInfo, 8.0, 0.5, 24.0)
        , _textMinSize(LabelMinSizeInfo, 8.0, 0.5, 24.0)
        , _textMaxSize(LabelMaxSizeInfo, 500.0, 0.0, 1000.0)
        , _drawElements(DrawElementsInfo, true)
        , _drawLabels(DrawLabelInfo, false)
        , _pixelSizeControl(PixelSizeControlInfo, false)
        , _colorOption(ColorOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
        , _fadeInDistance(
            FadeInDistancesInfo,
            glm::vec2(0.0f),
            glm::vec2(0.0),
            glm::vec2(100.0)
        )
        , _disableFadeInDistance(DisableFadeInInfo, true)
        , _billboardMaxSize(BillboardMaxSizeInfo, 400.0, 0.0, 1000.0)
        , _billboardMinSize(BillboardMinSizeInfo, 0.0, 0.0, 100.0)
        , _correctionSizeEndDistance(CorrectionSizeEndDistanceInfo, 17.0, 12.0, 25.0)
        , _correctionSizeFactor(CorrectionSizeFactorInfo, 8, 0.0, 20.0)
        , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
        , _polygonTexture(nullptr)
        , _spriteTexture(nullptr)
        , _program(nullptr)
        , _font(nullptr)
        , _speckFile("")
        , _colorMapFile("")
        , _labelFile("")
        , _colorOptionString("")
        , _unit(Parsec)
        , _nValuesPerAstronomicalObject(0)
        , _transformationMatrix(glm::dmat4(1.0))
        , _vao(0)
        , _vbo(0)
        , _polygonVao(0)
        , _polygonVbo(0)
    {
        using File = ghoul::filesystem::File;

        documentation::testSpecificationAndThrow(
            Documentation(),
            dictionary,
            "RenderableBillboardsCloud"
        );

        if (dictionary.hasKey(KeyFile)) {
            _speckFile = absPath(dictionary.value<std::string>(KeyFile));
            _hasSpeckFile = true;
            _drawElements.onChange([&]() {
                _hasSpeckFile = _hasSpeckFile == true ? false : true; });
            addProperty(_drawElements);
        }

        // DEBUG:
        _renderOption.addOption(0, "Camera View Direction");
        _renderOption.addOption(1, "Camera Position Normal");
        _renderOption.set(1);
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
            _spriteTextureFile = std::make_unique<File>(_spriteTexturePath);

            _spriteTexturePath.onChange([&] { _spriteTextureIsDirty = true; });
            _spriteTextureFile->setCallback(
                [&](const File&) { _spriteTextureIsDirty = true; }
            );
            addProperty(_spriteTexturePath);


            _hasSpriteTexture = true;
        }

        if (dictionary.hasKey(ColorMapInfo.identifier)) {
            _colorMapFile = absPath(dictionary.value<std::string>(
                ColorMapInfo.identifier
                ));
            _hasColorMapFile = true;

            if (dictionary.hasKey(ColorOptionInfo.identifier)) {
                ghoul::Dictionary colorOptionDataDic = dictionary.value<ghoul::Dictionary>(
                    ColorOptionInfo.identifier
                    );
                for (int i = 0; i < static_cast<int>(colorOptionDataDic.size()); ++i) {
                    std::string colorMapInUseName(
                        colorOptionDataDic.value<std::string>(std::to_string(i + 1)));
                    _colorOption.addOption(i, colorMapInUseName);
                    _optionConversionMap.insert({ i, colorMapInUseName });
                    _colorOptionString = colorMapInUseName;
                }
            }
            _colorOption.onChange(
                [&] {
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
                        rangeDataDict.value<glm::vec2>(std::to_string(i + 1)));
                }
            }

        }
        else if (dictionary.hasKey(keyColor)) {
            _pointColor = dictionary.value<glm::vec3>(keyColor);
            addProperty(_pointColor);
        }

        if (dictionary.hasKey(TransparencyInfo.identifier)) {
            _alphaValue = static_cast<float>(
                dictionary.value<double>(TransparencyInfo.identifier)
                );
        }
        addProperty(_alphaValue);

        if (dictionary.hasKey(ScaleFactorInfo.identifier)) {
            _scaleFactor = static_cast<float>(
                dictionary.value<double>(ScaleFactorInfo.identifier)
                );
        }
        addProperty(_scaleFactor);

        if (dictionary.hasKey(PolygonSidesInfo.identifier)) {
            _polygonSides = static_cast<float>(
                dictionary.value<double>(PolygonSidesInfo.identifier)
                );
            _hasPolygon = true;
        }

        if (dictionary.hasKey(LabelFileInfo.identifier)) {
            if (dictionary.hasKey(DrawLabelInfo.identifier)) {
                _drawLabels = dictionary.value<bool>(DrawLabelInfo.identifier);
            }
            addProperty(_drawLabels);

            _labelFile = absPath(dictionary.value<std::string>(
                LabelFileInfo.identifier
                ));
            _hasLabel = true;

            if (dictionary.hasKey(TextColorInfo.identifier)) {
                _textColor = dictionary.value<glm::vec4>(TextColorInfo.identifier);
                _hasLabel = true;
            }
            _textColor.setViewOption(properties::Property::ViewOptions::Color);
            addProperty(_textColor);
            _textColor.onChange([&]() { _textColorIsDirty = true; });


            if (dictionary.hasKey(TextSizeInfo.identifier)) {
                _textSize = dictionary.value<double>(TextSizeInfo.identifier);
            }
            addProperty(_textSize);

            if (dictionary.hasKey(LabelMinSizeInfo.identifier)) {
                _textMinSize = static_cast<int>(
                    dictionary.value<float>(LabelMinSizeInfo.identifier)
                    );
            }
            addProperty(_textMinSize);

            if (dictionary.hasKey(LabelMaxSizeInfo.identifier)) {
                _textMaxSize = static_cast<int>(
                    dictionary.value<float>(LabelMaxSizeInfo.identifier)
                    );
            }
            addProperty(_textMaxSize);
        }

        if (dictionary.hasKey(TransformationMatrixInfo.identifier)) {
            _transformationMatrix = dictionary.value<glm::dmat4>(
                TransformationMatrixInfo.identifier
                );
        }

        if (dictionary.hasKey(FadeInDistancesInfo.identifier)) {
            glm::vec2 fadeInValue = dictionary.value<glm::vec2>(
                FadeInDistancesInfo.identifier
                );
            _fadeInDistance.set(fadeInValue);
            _disableFadeInDistance.set(false);
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
        }
        addProperty(_correctionSizeFactor);

        if (dictionary.hasKey(PixelSizeControlInfo.identifier)) {
            _pixelSizeControl = dictionary.value<bool>(PixelSizeControlInfo.identifier);
        }
        addProperty(_pixelSizeControl);
    }

    bool RenderableBillboardsCloud::isReady() const {
        return ((_program != nullptr) && (!_fullData.empty())) || (!_labelData.empty());
    }

    void RenderableBillboardsCloud::initialize() {
        bool success = loadData();
        if (!success) {
            throw ghoul::RuntimeError("Error loading data");
        }

        if (!_colorOptionString.empty()) {
            // Following DU behavior here. The last colormap variable
            // entry is the one selected by default.
            _colorOption.setValue(static_cast<int>(_colorRangeData.size() - 1));
        }
    }

    void RenderableBillboardsCloud::initializeGL() {
        _program = DigitalUniverseModule::ProgramObjectManager.requestProgramObject(
            ProgramObjectName,
            []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return OsEng.renderEngine().buildRenderProgram(
                ProgramObjectName,
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboard_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboard_fs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboard_gs.glsl")
            );
        }
        );

        _renderToPolygonProgram =
            DigitalUniverseModule::ProgramObjectManager.requestProgramObject(
                RenderToPolygonProgram,
                []() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
            return ghoul::opengl::ProgramObject::Build(
                RenderToPolygonProgram,
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_fs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_gs.glsl")
            );
        }
        );

        _uniformCache.cameraViewProjectionMatrix = _program->uniformLocation(
            "cameraViewProjectionMatrix"
        );

        _uniformCache.modelMatrix = _program->uniformLocation(
            "modelMatrix"
        );

        _uniformCache.cameraPos                 = _program->uniformLocation("cameraPosition");
        _uniformCache.cameraLookup              = _program->uniformLocation("cameraLookUp");
        _uniformCache.renderOption              = _program->uniformLocation("renderOption");
        _uniformCache.minBillboardSize          = _program->uniformLocation("minBillboardSize");
        _uniformCache.maxBillboardSize          = _program->uniformLocation("maxBillboardSize");
        _uniformCache.correctionSizeEndDistance = _program->uniformLocation("correctionSizeEndDistance");
        _uniformCache.correctionSizeFactor      = _program->uniformLocation("correctionSizeFactor");
        _uniformCache.color                     = _program->uniformLocation("color");
        _uniformCache.alphaValue                = _program->uniformLocation("alphaValue");
        _uniformCache.scaleFactor               = _program->uniformLocation("scaleFactor");
        _uniformCache.up                        = _program->uniformLocation("up");
        _uniformCache.right                     = _program->uniformLocation("right");
        _uniformCache.fadeInValue               = _program->uniformLocation("fadeInValue");
        _uniformCache.screenSize                = _program->uniformLocation("screenSize");
        _uniformCache.spriteTexture             = _program->uniformLocation("spriteTexture");
        _uniformCache.polygonTexture            = _program->uniformLocation("polygonTexture");
        _uniformCache.hasPolygon                = _program->uniformLocation("hasPolygon");
        _uniformCache.hasColormap               = _program->uniformLocation("hasColorMap");
        _uniformCache.enabledRectSizeControl    = _program->uniformLocation("enabledRectSizeControl");

        if (_hasPolygon) {
            createPolygonTexture();
        }

        if (_hasLabel) {
            if (_font == nullptr) {
                size_t _fontSize = 50;
                _font = OsEng.fontManager().font(
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

        DigitalUniverseModule::ProgramObjectManager.releaseProgramObject(
            ProgramObjectName,
            [](ghoul::opengl::ProgramObject* p) {
            OsEng.renderEngine().removeRenderProgram(p);
        }
        );
        _program = nullptr;

        DigitalUniverseModule::ProgramObjectManager.releaseProgramObject(
            RenderToPolygonProgram
        );
        _renderToPolygonProgram = nullptr;

        if (_hasSpriteTexture) {
            _spriteTexture = nullptr;
        }

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

        // Saving current OpenGL state
        GLboolean blendEnabled = glIsEnabledi(GL_BLEND, 0);
        GLenum blendEquationRGB;
        GLenum blendEquationAlpha;
        GLenum blendDestAlpha;
        GLenum blendDestRGB;
        GLenum blendSrcAlpha;
        GLenum blendSrcRGB;

        glGetIntegerv(GL_BLEND_EQUATION_RGB, &blendEquationRGB);
        glGetIntegerv(GL_BLEND_EQUATION_ALPHA, &blendEquationAlpha);
        glGetIntegerv(GL_BLEND_DST_ALPHA, &blendDestAlpha);
        glGetIntegerv(GL_BLEND_DST_RGB, &blendDestRGB);
        glGetIntegerv(GL_BLEND_SRC_ALPHA, &blendSrcAlpha);
        glGetIntegerv(GL_BLEND_SRC_RGB, &blendSrcRGB);

        glEnablei(GL_BLEND, 0);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
        //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

        _program->activate();

        glm::dmat4 projMatrix = glm::dmat4(data.camera.projectionMatrix());
        _program->setUniform(
            "screenSize",
            glm::vec2(OsEng.renderEngine().renderingResolution())
        );

        _program->setUniform(_uniformCache.cameraPos, data.camera.positionVec3());
        _program->setUniform(_uniformCache.cameraLookup, data.camera.lookUpVectorWorldSpace());
        _program->setUniform(_uniformCache.renderOption, _renderOption.value());
        _program->setUniform(_uniformCache.modelMatrix, modelMatrix);
        _program->setUniform(_uniformCache.cameraViewProjectionMatrix,
            glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix());
        _program->setUniform(_uniformCache.minBillboardSize, _billboardMinSize); // in pixels
        _program->setUniform(_uniformCache.maxBillboardSize, _billboardMaxSize); // in pixels
        _program->setUniform(_uniformCache.color, _pointColor);
        _program->setUniform(_uniformCache.alphaValue, _alphaValue);
        _program->setUniform(_uniformCache.scaleFactor, _scaleFactor);
        _program->setUniform(_uniformCache.up, orthoUp);
        _program->setUniform(_uniformCache.right, orthoRight);
        _program->setUniform(_uniformCache.fadeInValue, fadeInVariable);

        _program->setUniform(_uniformCache.correctionSizeEndDistance, _correctionSizeEndDistance);
        _program->setUniform(_uniformCache.correctionSizeFactor, _correctionSizeFactor);

        _program->setUniform(_uniformCache.enabledRectSizeControl, _pixelSizeControl);

        GLint viewport[4];
        glGetIntegerv(GL_VIEWPORT, viewport);
        _program->setUniform(_uniformCache.screenSize, glm::vec2(viewport[2], viewport[3]));

        ghoul::opengl::TextureUnit spriteTextureUnit;
        if (_hasSpriteTexture) {
            spriteTextureUnit.activate();
            _spriteTexture->bind();
            _program->setUniform(_uniformCache.spriteTexture, spriteTextureUnit);
        }

        ghoul::opengl::TextureUnit polygonTextureUnit;
        if (_hasPolygon) {
            polygonTextureUnit.activate();
            glBindTexture(GL_TEXTURE_2D, _pTexture);
            _program->setUniform(_uniformCache.polygonTexture, polygonTextureUnit);
            _program->setUniform(_uniformCache.hasPolygon, _hasPolygon);
        }

        _program->setUniform(_uniformCache.hasColormap, _hasColorMapFile);

        glBindVertexArray(_vao);
        const GLsizei nAstronomicalObjects = static_cast<GLsizei>(_fullData.size() /
            _nValuesPerAstronomicalObject);
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
        float scale = 0.0;
        switch (_unit) {
        case Meter:
            scale = 1.0;
            break;
        case Kilometer:
            scale = 1e3;
            break;
        case Parsec:
            scale = PARSEC;
            break;
        case Kiloparsec:
            scale = 1e3 * PARSEC;
            break;
        case Megaparsec:
            scale = 1e6 * PARSEC;
            break;
        case Gigaparsec:
            scale = 1e9 * PARSEC;
            break;
        case GigalightYears:
            scale = 306391534.73091 * PARSEC;
            break;
        }

        glm::vec4 textColor = _textColor;
        textColor.a *= fadeInVariable;
        for (const std::pair<glm::vec3, std::string>& pair : _labelData) {
            //glm::vec3 scaledPos(_transformationMatrix * glm::dvec4(pair.first, 1.0));
            glm::vec3 scaledPos(pair.first);
            scaledPos *= scale;
            ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
                *_font,
                scaledPos,
                textColor,
                pow(10.0, _textSize.value()),
                _textMinSize,
                _textMaxSize,
                modelViewProjectionMatrix,
                orthoRight,
                orthoUp,
                data.camera.positionVec3(),
                data.camera.lookUpVectorWorldSpace(),
                _renderOption.value(),
                "%s",
                pair.second.c_str()
            );
        }
    }

    void RenderableBillboardsCloud::render(const RenderData& data, RendererTasks&) {

        float scale = 0.0;
        switch (_unit) {
        case Meter:
            scale = 1.0;
            break;
        case Kilometer:
            scale = 1e3;
            break;
        case Parsec:
            scale = PARSEC;
            break;
        case Kiloparsec:
            scale = 1e3 * PARSEC;
            break;
        case Megaparsec:
            scale = 1e6 * PARSEC;
            break;
        case Gigaparsec:
            scale = 1e9 * PARSEC;
            break;
        case GigalightYears:
            scale = 306391534.73091 * PARSEC;
            break;
        }

        float fadeInVariable = 1.0f;
        if (!_disableFadeInDistance) {
            float distCamera = glm::length(data.camera.positionVec3());
            glm::vec2 fadeRange = _fadeInDistance;
            float a = 1.0f / ((fadeRange.y - fadeRange.x) * scale);
            float b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
            float funcValue = a * distCamera + b;
            fadeInVariable *= funcValue > 1.0 ? 1.0 : funcValue;

            if (funcValue < 0.01) {
                return;
            }
        }


        glm::dmat4 modelMatrix =
            glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
            glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
            glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

        glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
        glm::mat4 projectionMatrix = data.camera.projectionMatrix();

        glm::dmat4 modelViewProjectionMatrix = glm::dmat4(projectionMatrix) *
            modelViewMatrix;

        glm::dvec3 cameraViewDirectionWorld = glm::normalize(data.camera.viewDirectionWorldSpace());
        glm::dvec3 cameraUpDirectionWorld = glm::normalize(data.camera.lookUpVectorWorldSpace());
        glm::dvec3 orthoRight = glm::dvec3(glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld));
        if (orthoRight == glm::dvec3(0.0)) {
            glm::dvec3 otherVector(cameraUpDirectionWorld.y, cameraUpDirectionWorld.x, cameraUpDirectionWorld.z);
            orthoRight = glm::dvec3(glm::cross(otherVector, cameraViewDirectionWorld));
        }
        glm::dvec3 orthoUp = cameraUpDirectionWorld;

        if (_hasSpeckFile) {
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

            if (_hasColorMapFile) {
                /*const size_t nAstronomicalObjects = _fullData.size() /
                _nValuesPerAstronomicalObject;
                const size_t nValues = _slicedData.size() / nAstronomicalObjects;
                GLsizei stride = static_cast<GLsizei>(sizeof(float) * nValues);*/

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

        if (_hasSpriteTexture && _spriteTextureIsDirty) {
            LDEBUG("Reloading Sprite Texture");
            _spriteTexture = nullptr;
            if (_spriteTexturePath.value() != "") {
                _spriteTexture = ghoul::io::TextureReader::ref().loadTexture(
                    absPath(_spriteTexturePath)
                );
                if (_spriteTexture) {
                    LINFO(fmt::format(
                        "Loaded texture from '{}'",
                        absPath(_spriteTexturePath)
                    ));
                    _spriteTexture->uploadTexture();
                }
                _spriteTexture->setFilter(
                    ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
                );

                _spriteTextureFile = std::make_unique<ghoul::filesystem::File>(
                    _spriteTexturePath);
                _spriteTextureFile->setCallback(
                    [&](const ghoul::filesystem::File&) { _spriteTextureIsDirty = true; }
                );
            }
            _spriteTextureIsDirty = false;
        }

        if (_hasLabel && _labelDataIsDirty) {

            _labelDataIsDirty = false;
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
        bool success = true;
        if (_hasSpeckFile) {
            std::string _file = _speckFile;
            // I disabled the cache as it didn't work on Mac --- abock

            std::string cachedFile = FileSys.cacheManager()->cachedFilename(
                ghoul::filesystem::File(_file),
                "RenderableDUMeshes|" + identifier(),
                ghoul::filesystem::CacheManager::Persistent::Yes
            );

            bool hasCachedFile = FileSys.fileExists(cachedFile);
            if (hasCachedFile) {
                LINFO(fmt::format(
                    "Cached file '{}' used for Speck file '{}'",
                    cachedFile,
                    _file
                ));

                success = loadCachedFile(cachedFile);
                if (success) {
                    return true;
                }
                else {
                    FileSys.cacheManager()->removeCacheFile(_file);
                    // Intentional fall-through to the 'else' to generate the cache
                    // file for the next run
                }
            }
            else {
                LINFO(fmt::format("Cache for Speck file '{}' not found", _file));
            }
            LINFO(fmt::format("Loading Speck file '{}'", _file));

            success = readSpeckFile();
            if (!success) {
                return false;
            }

            success &= saveCachedFile(cachedFile);
        }
        return success;
    }

    bool RenderableBillboardsCloud::loadLabelData() {
        bool success = true;
        std::string labelFile = _labelFile;
        if (!labelFile.empty()) {
            // I disabled the cache as it didn't work on Mac --- abock
            std::string cachedFile = FileSys.cacheManager()->cachedFilename(
                ghoul::filesystem::File(labelFile),
                ghoul::filesystem::CacheManager::Persistent::Yes
            );
            if (!_hasSpeckFile && !_hasColorMapFile) {
                success = true;
            }
            bool hasCachedFile = FileSys.fileExists(cachedFile);
            if (hasCachedFile) {
                LINFO(fmt::format(
                    "Cached file '{}' used for Label file '{}'",
                    cachedFile,
                    labelFile
                ));

                success &= loadCachedFile(cachedFile);
                if (!success) {
                    FileSys.cacheManager()->removeCacheFile(labelFile);
                    // Intentional fall-through to the 'else' to generate the cache
                    // file for the next run
                }
            }
            else {
                LINFO(fmt::format("Cache for Label file '{}' not found", labelFile));
                LINFO(fmt::format("Loading Label file '{}'", labelFile));

                success &= readLabelFile();
                if (!success) {
                    return false;
                }
            }
        }

        return success;
    }


    bool RenderableBillboardsCloud::readSpeckFile() {
        std::string _file = _speckFile;
        std::ifstream file(_file);
        if (!file.good()) {
            LERROR(fmt::format("Failed to open Speck file '{}'", _file));
            return false;
        }

        _nValuesPerAstronomicalObject = 0;

        // The beginning of the speck file has a header that either contains comments
        // (signaled by a preceding '#') or information about the structure of the file
        // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
        std::string line = "";
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

            if (line.substr(0, 7) != "datavar" &&
                line.substr(0, 10) != "texturevar" &&
                line.substr(0, 7) != "texture" &&
                line.substr(0, 10) != "polyorivar" &&
                line.substr(0, 10) != "maxcomment")
            {
                // we read a line that doesn't belong to the header, so we have to jump back
                // before the beginning of the current line
                file.seekg(position);
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

            for (int i = 0; i < _nValuesPerAstronomicalObject; ++i) {
                str >> values[i];
            }

            _fullData.insert(_fullData.end(), values.begin(), values.end());
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
        std::string line = "";
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
        std::string line = "";
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

            glm::vec3 position;
            for (auto j = 0; j < 3; ++j) {
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
            _labelData.push_back(std::make_pair(transformedPos, label));
        } while (!file.eof());

        return true;
    }

    bool RenderableBillboardsCloud::loadCachedFile(const std::string& file) {
        std::ifstream fileStream(file, std::ifstream::binary);
        if (fileStream.good()) {
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
            fileStream.read(reinterpret_cast<char*>(&_fullData[0]),
                nValues * sizeof(_fullData[0]));

            if (_hasColorMapFile) {
                int32_t nItems = 0;
                fileStream.read(reinterpret_cast<char*>(&nItems), sizeof(int32_t));

                for (int i = 0; i < nItems; ++i) {
                    int32_t keySize = 0;
                    fileStream.read(reinterpret_cast<char*>(&keySize), sizeof(int32_t));
                    std::string key;
                    for (int c = 0; c < keySize; ++c) {
                        char t;
                        fileStream.read(&t, sizeof(char));
                        key.append(1, t);
                    }
                    int32_t value = 0;
                    fileStream.read(reinterpret_cast<char*>(&value), sizeof(int32_t));

                    _variableDataPositionMap.insert({ key, value });
                }
            }

            bool success = fileStream.good();
            return success;
        }
        else {
            LERROR(fmt::format("Error opening file '{}' for loading cache file", file));
            return false;
        }
    }

    bool RenderableBillboardsCloud::saveCachedFile(const std::string& file) const {
        std::ofstream fileStream(file, std::ofstream::binary);
        if (fileStream.good()) {
            fileStream.write(reinterpret_cast<const char*>(&CurrentCacheVersion),
                sizeof(int8_t));

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

                for (auto pair : _variableDataPositionMap) {
                    int32_t keySize = static_cast<int32_t>(pair.first.size());
                    fileStream.write(
                        reinterpret_cast<const char*>(&keySize),
                        sizeof(int32_t)
                    );
                    for (size_t c = 0; c < pair.first.size(); ++c) {
                        char keyChar = static_cast<int32_t>(pair.first[c]);
                        fileStream.write(&keyChar, sizeof(char));
                    }
                    int32_t value = static_cast<int32_t>(pair.second);
                    fileStream.write(reinterpret_cast<const char*>(&value), sizeof(int32_t));
                }
            }

            bool success = fileStream.good();
            return success;
        }
        else {
            LERROR(fmt::format("Error opening file '{}' for save cache file", file));
            return false;
        }
    }

    void RenderableBillboardsCloud::createDataSlice() {
        _slicedData.clear();
        if (_hasColorMapFile) {
            _slicedData.reserve(8 * (_fullData.size() / _nValuesPerAstronomicalObject));
        }
        else {
            _slicedData.reserve(4 * (_fullData.size() / _nValuesPerAstronomicalObject));
        }

        // Generate the color bins for the colomap
        int colorMapInUse = 0;
        std::vector<float> colorBins;
        if (_hasColorMapFile) {
            colorMapInUse = _variableDataPositionMap[_colorOptionString];
            glm::vec2 currentColorRange = _colorRangeData[_colorOption.value()];
            float colorMapBinSize = (currentColorRange.y - currentColorRange.x) /
                static_cast<float>(_colorMapData.size());
            float bin = colorMapBinSize;
            for (size_t i = 0; i < _colorMapData.size(); ++i) {
                colorBins.push_back(bin);
                bin += colorMapBinSize;
            }
        }

        float biggestCoord = -1.0f;
        for (size_t i = 0; i < _fullData.size(); i += _nValuesPerAstronomicalObject) {
            glm::dvec4 transformedPos = _transformationMatrix * glm::dvec4(
                _fullData[i + 0],
                _fullData[i + 1],
                _fullData[i + 2],
                1.0
            );
            glm::vec4 position(glm::vec3(transformedPos), static_cast<float>(_unit));

            if (_hasColorMapFile) {
                for (auto j = 0; j < 4; ++j) {
                    _slicedData.push_back(position[j]);
                    biggestCoord = biggestCoord < position[j] ? position[j] : biggestCoord;
                }
                // Finds from which bin to get the color.
                // Note: the first color in the colormap file
                // is the outliers color.
                glm::vec4 itemColor;
                float variableColor = _fullData[i + 3 + colorMapInUse];
                int c = static_cast<int>(colorBins.size() - 1);
                while (variableColor < colorBins[c]) {
                    --c;
                    if (c == 0)
                        break;
                }

                int colorIndex =
                    c == static_cast<int>(colorBins.size() - 1) ?
                    0 :
                    c + 1;

                for (auto j = 0; j < 4; ++j) {
                    _slicedData.push_back(_colorMapData[colorIndex][j]);
                }
            }
            else {
                for (auto j = 0; j < 4; ++j) {
                    _slicedData.push_back(position[j]);
                }
            }
        }
        _fadeInDistance.setMaxValue(glm::vec2(10.0f * biggestCoord));
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
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 256,
            256, 0, GL_RGBA, GL_BYTE, nullptr);

        renderToTexture(
            std::bind(
                &openspace::RenderableBillboardsCloud::loadPolygonGeometryForRendering,
                this
            ),
            std::bind(
                &openspace::RenderableBillboardsCloud::renderPolygonGeometry,
                this,
                std::placeholders::_1
            ),
            _pTexture,
            256,
            256
        );
    }

    void RenderableBillboardsCloud::renderToTexture(
        std::function<void(void)> geometryLoadingFunction,
        std::function<void(GLuint)> renderFunction,
        GLuint textureToRenderTo, GLuint textureWidth, GLuint textureHeight)
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

        glViewport(0, 0, textureWidth, textureHeight);

        geometryLoadingFunction();
        renderFunction(_polygonVao);

        // Restores Applications' OpenGL State
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
        glViewport(viewport[0], viewport[1], viewport[2], viewport[3]);

        if (_polygonVbo) {
            glDeleteBuffers(1, &_polygonVbo);
        }

        if (_polygonVao) {
            glDeleteVertexArrays(1, &_polygonVao);
        }
        glDeleteFramebuffers(1, &textureFBO);
    }

    void RenderableBillboardsCloud::loadPolygonGeometryForRendering() {
        glGenVertexArrays(1, &_polygonVao);
        glGenBuffers(1, &_polygonVbo);
        glBindVertexArray(_polygonVao);
        glBindBuffer(GL_ARRAY_BUFFER, _polygonVbo);

        const GLfloat vertex_data[] = {
            //      x      y     z     w
            0.0f, 0.0f, 0.0f, 1.0f,
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
            ghoul::opengl::ProgramObject::Build("RenderableBillboardsCloud_Polygon",
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_vs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_fs.glsl"),
                absPath("${MODULE_DIGITALUNIVERSE}/shaders/billboardpolygon_gs.glsl")
            );

        program->activate();
        static const float black[] = { 0.0f, 0.0f, 0.0f, 0.0f };
        glClearBufferfv(GL_COLOR, 0, black);

        program->setUniform("sides", _polygonSides);
        program->setUniform("polygonColor", _pointColor);

        glBindVertexArray(vao);
        glDrawArrays(GL_POINTS, 0, 1);
        glBindVertexArray(0);

        program->deactivate();
    }

} // namespace openspace
