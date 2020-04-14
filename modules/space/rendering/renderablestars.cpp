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

#include <modules/space/rendering/renderablestars.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <limits>
#include <type_traits>

namespace {
    constexpr const char* _loggerCat = "RenderableStars";

    constexpr const char* KeyFile = "File";
    constexpr const char* KeyStaticFilterValue = "StaticFilter";
    constexpr const char* KeyStaticFilterReplacement = "StaticFilterReplacement";

    constexpr const std::array<const char*, 17> UniformNames = {
        "modelMatrix", "cameraUp", "cameraViewProjectionMatrix",
        "colorOption", "magnitudeExponent", "eyePosition", "psfParamConf",
        "lumCent", "radiusCent", "brightnessCent", "colorTexture",
        "alphaValue", "psfTexture", "otherDataTexture", "otherDataRange",
        "filterOutOfRange", "fixedColor"
    };

    constexpr int8_t CurrentCacheVersion = 3;

    constexpr const int RenderOptionPointSpreadFunction = 0;
    constexpr const int RenderOptionTexture = 1;

    constexpr const int PsfMethodSpencer = 0;
    constexpr const int PsfMethodMoffat = 1;

    constexpr double PARSEC = 0.308567756E17;

    struct CommonDataLayout {
        std::array<float, 3> position;
        float value;
        float luminance;
        float absoluteMagnitude;
        float apparentMagnitude;
    };

    struct ColorVBOLayout : public CommonDataLayout {};

    struct VelocityVBOLayout : public CommonDataLayout {
        float vx; // v_x
        float vy; // v_y
        float vz; // v_z
    };

    struct SpeedVBOLayout : public CommonDataLayout {
        float speed;
    };

    struct OtherDataLayout : public CommonDataLayout {};

    constexpr openspace::properties::Property::PropertyInfo SpeckFileInfo = {
        "SpeckFile",
        "Speck File",
        "The speck file that is loaded to get the data for rendering these stars."
    };

    static const openspace::properties::Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "ColorBV Texture",
        "The path to the texture that is used to convert from the B-V value of the star "
        "to its color. The texture is used as a one dimensional lookup function."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorOptionInfo = {
        "ColorOption",
        "Color Option",
        "This value determines which quantity is used for determining the color of the "
        "stars."
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataOptionInfo = {
        "OtherData",
        "Other Data Column",
        "The index of the speck file data column that is used as the color input"
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataValueRangeInfo = {
        "OtherDataValueRange",
        "Range of the other data values",
        "This value is the min/max value range that is used to normalize the other data "
        "values so they can be used by the specified color map."
    };

    constexpr openspace::properties::Property::PropertyInfo FixedColorInfo = {
        "FixedColorValue",
        "Color used for fixed star colors",
        "The color that should be used if the 'Fixed Color' value is used."
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataColorMapInfo = {
        "OtherDataColorMap",
        "Other Data Color Map",
        "The color map that is used if the 'Other Data' rendering method is selected"
    };

    constexpr openspace::properties::Property::PropertyInfo FilterOutOfRangeInfo = {
        "FilterOutOfRange",
        "Filter Out of Range",
        "Determines whether other data values outside the value range should be visible "
        "or filtered away"
    };

    constexpr openspace::properties::Property::PropertyInfo EnableTestGridInfo = {
        "EnableTestGrid",
        "Enable Test Grid",
        "Set it to true for rendering the test grid."
    };

    // Old Method
    constexpr openspace::properties::Property::PropertyInfo PsfTextureInfo = {
        "Texture",
        "Point Spread Function Texture",
        "The path to the texture that should be used as a point spread function for the "
        "stars."
    };

    /*constexpr openspace::properties::Property::PropertyInfo ShapeTextureInfo = {
        "ShapeTexture",
        "Shape Texture to be convolved",
        "The path to the texture that should be used as the base shape for the stars."
    };*/

    constexpr openspace::properties::Property::PropertyInfo TransparencyInfo = {
        "Transparency",
        "Transparency",
        "This value is a multiplicative factor that is applied to the transparency of "
        "all stars."
    };

    // PSF
    constexpr openspace::properties::Property::PropertyInfo MagnitudeExponentInfo = {
        "MagnitudeExponent",
        "Magnitude Exponent",
        "Adjust star magnitude by 10^MagnitudeExponent. "
        "Stars closer than this distance are given full opacity. "
        "Farther away, stars dim proportionally to the logarithm of their distance."
    };

    constexpr openspace::properties::Property::PropertyInfo RenderMethodOptionInfo = {
        "RenderMethod",
        "Render Method",
        "Render method for the stars."
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo
    UserProvidedTextureOptionInfo =
    {
        "UserProvidedTexture",
        "User Provided Texture",
        ""
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo ParametersOwnerOptionInfo = {
        "ParametersOwner",
        "Parameters Options",
        ""
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo MoffatMethodOptionInfo = {
        "MoffatMethodOption",
        "Moffat Method",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo PSFMethodOptionInfo = {
        "PSFMethodOptionInfo",
        "PSF Method Option",
        "Debug option for PSF main function: Spencer or Moffat."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeCompositionOptionInfo = {
        "SizeComposition",
        "Size Composition Option",
        "Base multiplyer for the final stars' sizes."
    };

    constexpr openspace::properties::Property::PropertyInfo LumPercentInfo = {
        "LumPercent",
        "Luminosity Contribution",
        "Luminosity Contribution."
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusPercentInfo = {
        "RadiusPercent",
        "Radius Contribution",
        "Radius Contribution."
    };

    constexpr openspace::properties::Property::PropertyInfo BrightnessPercentInfo = {
        "BrightnessPercen",
        "App Brightness Contribution",
        "App Brightness Contribution."
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo SpencerPSFParamOwnerInfo = {
        "SpencerPSFParamOwner",
        "Spencer PSF Paramameters",
        "PSF parameters for Spencer"
    };

    constexpr openspace::properties::Property::PropertyInfo P0ParamInfo = {
        "P0Param",
        "P0",
        "P0 parameter contribution."
    };

    constexpr openspace::properties::Property::PropertyInfo P1ParamInfo = {
        "P1Param",
        "P1",
        "P1 parameter contribution."
    };

    constexpr openspace::properties::Property::PropertyInfo P2ParamInfo = {
        "P2Param",
        "P2",
        "P2 parameter contribution."
    };

    constexpr openspace::properties::Property::PropertyInfo AlphaConstInfo = {
        "AlphaConst",
        "Alpha",
        "Empirical Alpha Constant."
    };

    openspace::properties::PropertyOwner::PropertyOwnerInfo MoffatPSFParamOwnerInfo = {
        "MoffatPSFParam",
        "Moffat PSF Parameters",
        "PSF parameters for Moffat"
    };

    constexpr openspace::properties::Property::PropertyInfo FWHMInfo = {
        "FWHM",
        "FWHM",
        "Moffat's FWHM"
    };

    constexpr openspace::properties::Property::PropertyInfo BetaInfo = {
        "Beta",
        "Beta",
        "Moffat's Beta Constant."
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
}  // namespace

namespace openspace {

documentation::Documentation RenderableStars::Documentation() {
    using namespace documentation;
    return {
        "RenderableStars",
        "space_renderablestars",
        {
            {
                "Type",
                new StringEqualVerifier("RenderableStars"),
                Optional::No
            },
            {
                KeyFile,
                new StringVerifier,
                Optional::No,
                "The path to the SPECK file that contains information about the stars "
                "being rendered."
            },
            {
                ColorTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                ColorTextureInfo.description
            },
            /*{
                ShapeTextureInfo.identifier,
                new StringVerifier,
                Optional::No,
                ShapeTextureInfo.description
            },*/
            {
                ColorOptionInfo.identifier,
                new StringInListVerifier({
                    "Color", "Velocity", "Speed", "Other Data", "Fixed Color"
                }),
                Optional::Yes,
                ColorOptionInfo.description
            },
            {
                OtherDataOptionInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                OtherDataOptionInfo.description
            },
            {
                OtherDataColorMapInfo.identifier,
                new StringVerifier,
                Optional::Yes,
                OtherDataColorMapInfo.description
            },
            {
                FilterOutOfRangeInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                FilterOutOfRangeInfo.description
            },
            {
                KeyStaticFilterValue,
                new DoubleVerifier,
                Optional::Yes,
                "This value specifies a value that is always filtered out of the value "
                "ranges on loading. This can be used to trim the dataset's automatic "
                "value range."
            },
            {
                KeyStaticFilterReplacement,
                new DoubleVerifier,
                Optional::Yes,
                "This is the value that is used to replace statically filtered values. "
                "Setting this value only makes sense if 'StaticFilter' is 'true', as "
                "well."
            },
            {
                MagnitudeExponentInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                MagnitudeExponentInfo.description
            },
            {
                EnableTestGridInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                EnableTestGridInfo.description
            },
            {
                RenderMethodOptionInfo.identifier,
                new StringVerifier,
                Optional::No,
                RenderMethodOptionInfo.description
            },
            {
                SizeCompositionOptionInfo.identifier,
                new StringVerifier,
                Optional::No,
                SizeCompositionOptionInfo.description
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
        }
    };
}

RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _speckFile(SpeckFileInfo)
    , _colorTexturePath(ColorTextureInfo)
    //, _shapeTexturePath(ShapeTextureInfo)
    , _colorOption(ColorOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _otherDataOption(
        OtherDataOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _otherDataColorMapPath(OtherDataColorMapInfo)
    , _otherDataRange(
        OtherDataValueRangeInfo,
        glm::vec2(0.f, 1.f),
        glm::vec2(-10.f, -10.f),
        glm::vec2(10.f, 10.f)
    )
    , _fixedColor(FixedColorInfo, glm::vec4(1.f), glm::vec4(0.f), glm::vec4(1.f))
    , _filterOutOfRange(FilterOutOfRangeInfo, false)
    , _pointSpreadFunctionTexturePath(PsfTextureInfo)
    , _alphaValue(TransparencyInfo, 1.f, 0.f, 1.f)
    , _psfMethodOption(
        PSFMethodOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _psfMultiplyOption(
        SizeCompositionOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _lumCent(LumPercentInfo, 0.5f, 0.f, 3.f)
    , _radiusCent(RadiusPercentInfo, 0.5f, 0.f, 3.f)
    , _brightnessCent(BrightnessPercentInfo, 0.5f, 0.f, 3.f)
    , _magnitudeExponent(MagnitudeExponentInfo, 4.f, 0.f, 8.f)
    , _spencerPSFParamOwner(SpencerPSFParamOwnerInfo)
    , _p0Param(P0ParamInfo, 0.384f, 0.f, 1.f)
    , _p1Param(P1ParamInfo, 0.478f, 0.f, 1.f)
    , _p2Param(P2ParamInfo, 0.138f, 0.f, 1.f)
    , _spencerAlphaConst(AlphaConstInfo, 0.02f, 0.000001f, 5.f)
    , _moffatPSFParamOwner(MoffatPSFParamOwnerInfo)
    , _FWHMConst(FWHMInfo, 10.4f, -100.f, 1000.f)
    , _moffatBetaConst(BetaInfo, 4.765f, 0.f, 100.f)
    , _renderingMethodOption(
        RenderMethodOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _userProvidedTextureOwner(UserProvidedTextureOptionInfo)
    , _parametersOwner(ParametersOwnerOptionInfo)
    , _moffatMethodOwner(MoffatMethodOptionInfo)
    , _fadeInDistance(
        FadeInDistancesInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(100.f)
    )
    , _disableFadeInDistance(DisableFadeInInfo, true)
{
    using File = ghoul::filesystem::File;

    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableStars"
    );

    _speckFile = absPath(dictionary.value<std::string>(KeyFile));
    _speckFile.onChange([&]() { _speckFileIsDirty = true; });
    addProperty(_speckFile);

    _colorTexturePath = absPath(
        dictionary.value<std::string>(ColorTextureInfo.identifier)
    );
    _colorTextureFile = std::make_unique<File>(_colorTexturePath);

    /*_shapeTexturePath = absPath(dictionary.value<std::string>(
        ShapeTextureInfo.identifier
        ));
    _shapeTextureFile = std::make_unique<File>(_shapeTexturePath);*/

    if (dictionary.hasKey(OtherDataColorMapInfo.identifier)) {
        _otherDataColorMapPath = absPath(
            dictionary.value<std::string>(OtherDataColorMapInfo.identifier)
        );
    }

    _fixedColor.setViewOption(properties::Property::ViewOptions::Color, true);
    addProperty(_fixedColor);

    _colorOption.addOptions({
        { ColorOption::Color, "Color" },
        { ColorOption::Velocity, "Velocity" },
        { ColorOption::Speed, "Speed" },
        { ColorOption::OtherData, "Other Data" },
        { ColorOption::FixedColor, "Fixed Color" }
    });
    if (dictionary.hasKey(ColorOptionInfo.identifier)) {
        const std::string colorOption = dictionary.value<std::string>(
            ColorOptionInfo.identifier
        );
        if (colorOption == "Color") {
            _colorOption = ColorOption::Color;
        }
        else if (colorOption == "Velocity") {
            _colorOption = ColorOption::Velocity;
        }
        else if (colorOption == "Speed") {
            _colorOption = ColorOption::Speed;
        }
        else if (colorOption == "OtherData") {
            _colorOption = ColorOption::OtherData;
        }
        else {
            _colorOption = ColorOption::FixedColor;
        }
    }
    _colorOption.onChange([&] { _dataIsDirty = true; });
    addProperty(_colorOption);

    _colorTexturePath.onChange([&] { _colorTextureIsDirty = true; });
    _colorTextureFile->setCallback([&](const File&) {
        _colorTextureIsDirty = true;
    });
    addProperty(_colorTexturePath);

    /*_shapeTexturePath.onChange([&] { _shapeTextureIsDirty = true; });
    _shapeTextureFile->setCallback([&](const File&) {
        _shapeTextureIsDirty = true;
        });
    addProperty(_shapeTexturePath);*/

    if (dictionary.hasKey(EnableTestGridInfo.identifier)) {
        _enableTestGrid = dictionary.value<bool>(EnableTestGridInfo.identifier);
    }

    if (dictionary.hasKey(OtherDataOptionInfo.identifier)) {
        _queuedOtherData = dictionary.value<std::string>(OtherDataOptionInfo.identifier);
    }

    _otherDataOption.onChange([&]() { _dataIsDirty = true; });
    addProperty(_otherDataOption);

    addProperty(_otherDataRange);

    addProperty(_otherDataColorMapPath);
    _otherDataColorMapPath.onChange([&]() { _otherDataColorMapIsDirty = true; });

    if (dictionary.hasKey(KeyStaticFilterValue)) {
        _staticFilterValue = static_cast<float>(
            dictionary.value<double>(KeyStaticFilterValue)
        );
    }
    if (dictionary.hasKey(KeyStaticFilterReplacement)) {
        _staticFilterReplacementValue = static_cast<float>(
            dictionary.value<double>(KeyStaticFilterReplacement)
        );
    }

    addProperty(_filterOutOfRange);

    _renderingMethodOption.addOption(
        RenderOptionPointSpreadFunction,
        "Point Spread Function Based"
    );
    _renderingMethodOption.addOption(RenderOptionTexture, "Textured Based");
    addProperty(_renderingMethodOption);

    if (dictionary.hasKey(RenderMethodOptionInfo.identifier)) {
        std::string renderingMethod =
            dictionary.value<std::string>(RenderMethodOptionInfo.identifier);
        if (renderingMethod == "PSF") {
            _renderingMethodOption = RenderOptionPointSpreadFunction;
        }
        else if (renderingMethod == "Texture Based") {
            _renderingMethodOption = RenderOptionTexture;
        }
    }
    else {
        _renderingMethodOption = RenderOptionTexture;
    }

    _pointSpreadFunctionTexturePath = absPath(dictionary.value<std::string>(
        PsfTextureInfo.identifier
    ));
    _pointSpreadFunctionFile = std::make_unique<File>(_pointSpreadFunctionTexturePath);
    _pointSpreadFunctionTexturePath.onChange([&]() {
        _pointSpreadFunctionTextureIsDirty = true;
    });
    _pointSpreadFunctionFile->setCallback([&](const File&) {
        _pointSpreadFunctionTextureIsDirty = true;
    });
    _userProvidedTextureOwner.addProperty(_pointSpreadFunctionTexturePath);

    if (dictionary.hasKey(TransparencyInfo.identifier)) {
        _alphaValue = static_cast<float>(
            dictionary.value<double>(TransparencyInfo.identifier)
        );
    }
    _parametersOwner.addProperty(_alphaValue);

    _psfMethodOption.addOption(PsfMethodSpencer, "Spencer's Function");
    _psfMethodOption.addOption(PsfMethodMoffat, "Moffat's Function");
    _psfMethodOption = PsfMethodSpencer;
    _psfMethodOption.onChange([&]() { renderPSFToTexture(); });
    _parametersOwner.addProperty(_psfMethodOption);

    _psfMultiplyOption.addOption(0, "Use Star's Apparent Brightness");
    _psfMultiplyOption.addOption(1, "Use Star's Luminosity and Size");
    _psfMultiplyOption.addOption(2, "Luminosity, Size, App Brightness");
    _psfMultiplyOption.addOption(3, "Absolute Magnitude");
    _psfMultiplyOption.addOption(4, "Apparent Magnitude");
    _psfMultiplyOption.addOption(5, "Distance Modulus");

    if (dictionary.hasKey(MagnitudeExponentInfo.identifier)) {
        std::string sizeCompositionOption =
            dictionary.value<std::string>(SizeCompositionOptionInfo.identifier);

        if (sizeCompositionOption == "App Brightness") {
            _psfMultiplyOption = 0;
        }
        else if (sizeCompositionOption == "Lum and Size") {
            _psfMultiplyOption = 1;
        }
        else if (sizeCompositionOption == "Lum, Size and App Brightness") {
            _psfMultiplyOption = 2;
        }
        else if (sizeCompositionOption == "Abs Magnitude") {
            _psfMultiplyOption = 3;
        }
        else if (sizeCompositionOption == "App Maginitude") {
            _psfMultiplyOption = 4;
        }
        else if (sizeCompositionOption == "Distance Modulus") {
            _psfMultiplyOption = 5;
        }
    }
    else {
        _psfMultiplyOption = 5;
    }

    _parametersOwner.addProperty(_psfMultiplyOption);
    _parametersOwner.addProperty(_lumCent);
    _parametersOwner.addProperty(_radiusCent);
    _parametersOwner.addProperty(_brightnessCent);

    if (dictionary.hasKey(MagnitudeExponentInfo.identifier)) {
        _magnitudeExponent = static_cast<float>(
            dictionary.value<double>(MagnitudeExponentInfo.identifier)
        );
    }
    _parametersOwner.addProperty(_magnitudeExponent);

    auto renderPsf = [&]() { renderPSFToTexture(); };

    _spencerPSFParamOwner.addProperty(_p0Param);
    _p0Param.onChange(renderPsf);
    _spencerPSFParamOwner.addProperty(_p1Param);
    _p1Param.onChange(renderPsf);
    _spencerPSFParamOwner.addProperty(_p2Param);
    _p2Param.onChange(renderPsf);
    _spencerPSFParamOwner.addProperty(_spencerAlphaConst);
    _spencerAlphaConst.onChange(renderPsf);

    _moffatPSFParamOwner.addProperty(_FWHMConst);
    _FWHMConst.onChange(renderPsf);
    _moffatPSFParamOwner.addProperty(_moffatBetaConst);
    _moffatBetaConst.onChange(renderPsf);

    _parametersOwner.addPropertySubOwner(_spencerPSFParamOwner);
    _parametersOwner.addPropertySubOwner(_moffatPSFParamOwner);

    addPropertySubOwner(_userProvidedTextureOwner);
    addPropertySubOwner(_parametersOwner);
    addPropertySubOwner(_moffatMethodOwner);

    if (dictionary.hasKey(FadeInDistancesInfo.identifier)) {
        glm::vec2 v = dictionary.value<glm::vec2>(FadeInDistancesInfo.identifier);
        _fadeInDistance = v;
        _disableFadeInDistance = false;
        addProperty(_fadeInDistance);
        addProperty(_disableFadeInDistance);
    }
}

RenderableStars::~RenderableStars() {}

bool RenderableStars::isReady() const {
    return _program != nullptr;
}

void RenderableStars::initializeGL() {
    _program = global::renderEngine.buildRenderProgram(
        "Star",
        absPath("${MODULE_SPACE}/shaders/star_vs.glsl"),
        absPath("${MODULE_SPACE}/shaders/star_fs.glsl"),
        absPath("${MODULE_SPACE}/shaders/star_ge.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    loadData();

    if (!_queuedOtherData.empty()) {
        auto it = std::find(_dataNames.begin(), _dataNames.end(), _queuedOtherData);
        if (it == _dataNames.end()) {
            LERROR(fmt::format("Could not find other data column {}", _queuedOtherData));
        }
        else {
            _otherDataOption = static_cast<int>(std::distance(_dataNames.begin(), it));
            _queuedOtherData.clear();
        }
    }
    _speckFileIsDirty = false;

    LDEBUG("Creating Polygon Texture");

    glGenVertexArrays(1, &_psfVao);
    glGenBuffers(1, &_psfVbo);
    glBindVertexArray(_psfVao);
    glBindBuffer(GL_ARRAY_BUFFER, _psfVbo);

    const GLfloat vertexData[] = {
        //x      y     s     t
        -1.f, -1.f, 0.f, 0.f,
         1.f,  1.f, 1.f, 1.f,
        -1.f,  1.f, 0.f, 1.f,
        -1.f, -1.f, 0.f, 0.f,
         1.f, -1.f, 1.f, 0.f,
         1.f,  1.f, 1.f, 1.f
    };

    glBufferData(GL_ARRAY_BUFFER, sizeof(vertexData), vertexData, GL_STATIC_DRAW);
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

    glGenTextures(1, &_psfTexture);
    glBindTexture(GL_TEXTURE_2D, _psfTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA8,
        _psfTextureSize,
        _psfTextureSize,
        0,
        GL_RGBA,
        GL_BYTE,
        nullptr
    );


    LDEBUG("Creating Convolution Texture");

    glGenTextures(1, &_convolvedTexture);
    glBindTexture(GL_TEXTURE_2D, _convolvedTexture);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
    // Stopped using a buffer object for GL_PIXEL_UNPACK_BUFFER
    glBindBuffer(GL_PIXEL_UNPACK_BUFFER, 0);
    glTexImage2D(
        GL_TEXTURE_2D,
        0,
        GL_RGBA8,
        _convolvedfTextureSize,
        _convolvedfTextureSize,
        0,
        GL_RGBA,
        GL_BYTE,
        nullptr
    );

    //loadShapeTexture();

    renderPSFToTexture();
}

void RenderableStars::deinitializeGL() {
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    _colorTexture = nullptr;
    //_shapeTexture = nullptr;

    if (_program) {
        global::renderEngine.removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableStars::renderPSFToTexture() {
    // Saves current FBO first
    GLint defaultFBO;
    glGetIntegerv(GL_FRAMEBUFFER_BINDING, &defaultFBO);

    GLint m_viewport[4];
    glGetIntegerv(GL_VIEWPORT, m_viewport);

    // Saving current OpenGL state
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

    // Creates the FBO for the calculations
    GLuint psfFBO;
    glGenFramebuffers(1, &psfFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, psfFBO);
    GLenum drawBuffers[1] = { GL_COLOR_ATTACHMENT0 };
    glDrawBuffers(1, drawBuffers);

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _psfTexture, 0);

    glViewport(0, 0, _psfTextureSize, _psfTextureSize);

    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    std::unique_ptr<ghoul::opengl::ProgramObject> program =
        ghoul::opengl::ProgramObject::Build(
            "RenderStarPSFToTexture",
            absPath("${MODULE_SPACE}/shaders/psfToTexture_vs.glsl"),
            absPath("${MODULE_SPACE}/shaders/psfToTexture_fs.glsl")
        );

    program->activate();
    constexpr const float black[] = { 0.f, 0.f, 0.f, 0.f };
    glClearBufferfv(GL_COLOR, 0, black);

    program->setUniform("psfMethod", _psfMethodOption.value());
    program->setUniform("p0Param", _p0Param);
    program->setUniform("p1Param", _p1Param);
    program->setUniform("p2Param", _p2Param);
    program->setUniform("alphaConst", _spencerAlphaConst);
    program->setUniform("FWHM", _FWHMConst);
    program->setUniform("betaConstant", _moffatBetaConst);

    // Draws psf to texture
    glBindVertexArray(_psfVao);
    glDrawArrays(GL_TRIANGLES, 0, 6);
    glBindVertexArray(0);

    program->deactivate();

    // JCC: Convolution is disabled while FFT is not enabled
    //// Now convolves with a disc shape for final shape

    //GLuint convolveFBO;
    //glGenFramebuffers(1, &convolveFBO);
    //glBindFramebuffer(GL_FRAMEBUFFER, convolveFBO);
    //glDrawBuffers(1, drawBuffers);

    //glFramebufferTexture(
    //    GL_FRAMEBUFFER,
    //    GL_COLOR_ATTACHMENT0,
    //    _convolvedTexture,
    //    0
    //);

    //glViewport(0, 0, _convolvedfTextureSize, _convolvedfTextureSize);

    //std::unique_ptr<ghoul::opengl::ProgramObject> programConvolve =
    //    ghoul::opengl::ProgramObject::Build("ConvolvePSFandStarShape",
    //        absPath("${MODULE_SPACE}/shaders/convolution_vs.glsl"),
    //        absPath("${MODULE_SPACE}/shaders/convolution_fs.glsl")
    //    );

    //programConvolve->activate();
    //glClearBufferfv(GL_COLOR, 0, black);

    //ghoul::opengl::TextureUnit psfTextureUnit;
    //psfTextureUnit.activate();
    //glBindTexture(GL_TEXTURE_2D, _psfTexture);
    //programConvolve->setUniform("psfTexture", psfTextureUnit);
    //
    //ghoul::opengl::TextureUnit shapeTextureUnit;
    //shapeTextureUnit.activate();
    //_shapeTexture->bind();
    //programConvolve->setUniform("shapeTexture", shapeTextureUnit);

    //programConvolve->setUniform("psfTextureSize", _psfTextureSize);
    //programConvolve->setUniform(
    //    "convolvedfTextureSize",
    //    _convolvedfTextureSize
    //);

    //// Convolves to texture
    //glBindVertexArray(_psfVao);
    //glDrawArrays(GL_TRIANGLES, 0, 6);
    //glBindVertexArray(0);

    //programConvolve->deactivate();

    //// Restores system state
    //glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    //glViewport(
    //    m_viewport[0],
    //    m_viewport[1],
    //    m_viewport[2],
    //    m_viewport[3]
    //);
    //glDeleteFramebuffers(1, &psfFBO);
    //glDeleteFramebuffers(1, &convolveFBO);

    // Restores OpenGL blending state
    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB,  blendSrcAlpha, blendDestAlpha);
}

void RenderableStars::render(const RenderData& data, RendererTasks&) {
    if (_fullData.empty()) {
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

    _program->activate();

    glm::dvec3 eyePosition = glm::dvec3(
        glm::inverse(data.camera.combinedViewMatrix()) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );
    _program->setUniform(_uniformCache.eyePosition, eyePosition);

    glm::dvec3 cameraUp = data.camera.lookUpVectorWorldSpace();
    _program->setUniform(_uniformCache.cameraUp, cameraUp);

    glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) *
        glm::dmat4(data.modelTransform.rotation) *
        glm::dmat4(glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale)));

    glm::dmat4 projectionMatrix = glm::dmat4(data.camera.projectionMatrix());

    glm::dmat4 cameraViewProjectionMatrix = projectionMatrix *
                                            data.camera.combinedViewMatrix();

    _program->setUniform(_uniformCache.modelMatrix, modelMatrix);
    _program->setUniform(
        _uniformCache.cameraViewProjectionMatrix,
        cameraViewProjectionMatrix
    );
    _program->setUniform(_uniformCache.colorOption, _colorOption);
    _program->setUniform(_uniformCache.magnitudeExponent, _magnitudeExponent);

    _program->setUniform(_uniformCache.psfParamConf, _psfMultiplyOption.value());
    _program->setUniform(_uniformCache.lumCent, _lumCent);
    _program->setUniform(_uniformCache.radiusCent, _radiusCent);
    _program->setUniform(_uniformCache.brightnessCent, _brightnessCent);

    if (_colorOption == ColorOption::FixedColor) {
        if (_uniformCache.fixedColor == -1) {
            _uniformCache.fixedColor = _program->uniformLocation("fixedColor");
        }
        _program->setUniform(_uniformCache.fixedColor, _fixedColor);
    }

    float fadeInVariable = 1.f;
    if (!_disableFadeInDistance) {
        float distCamera = static_cast<float>(glm::length(data.camera.positionVec3()));
        const glm::vec2 fadeRange = _fadeInDistance;
        const double a = 1.f / ((fadeRange.y - fadeRange.x) * PARSEC);
        const double b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        const double funcValue = a * distCamera + b;
        fadeInVariable *= static_cast<float>(funcValue > 1.f ? 1.f : funcValue);

        _program->setUniform(_uniformCache.alphaValue, _alphaValue * fadeInVariable);
    }
    else {
        _program->setUniform(_uniformCache.alphaValue, _alphaValue);
    }

    ghoul::opengl::TextureUnit psfUnit;
    psfUnit.activate();

    if (_renderingMethodOption.value() == 0) { // PSF Based Methods
        glBindTexture(GL_TEXTURE_2D, _psfTexture);\
        // Convolutioned texture
        //glBindTexture(GL_TEXTURE_2D, _convolvedTexture);
    }
    else if (_renderingMethodOption.value() == 1) { // Textured based Method
        _pointSpreadFunctionTexture->bind();
    }

    _program->setUniform(_uniformCache.psfTexture, psfUnit);

    ghoul::opengl::TextureUnit colorUnit;
    if (_colorTexture) {
        colorUnit.activate();
        _colorTexture->bind();
        _program->setUniform(_uniformCache.colorTexture, colorUnit);
    }

    ghoul::opengl::TextureUnit otherDataUnit;
    if (_colorOption == ColorOption::OtherData && _otherDataColorMapTexture) {
        otherDataUnit.activate();
        _otherDataColorMapTexture->bind();
        _program->setUniform(_uniformCache.otherDataTexture, otherDataUnit);
    }
    else {
        // We need to set the uniform to something, or the shader doesn't work
        _program->setUniform(_uniformCache.otherDataTexture, colorUnit);
    }
    // Same here, if we don't set this value, the rendering disappears even if we don't
    // use this color mode --- abock 2018-11-19
    _program->setUniform(_uniformCache.otherDataRange, _otherDataRange);
    _program->setUniform(_uniformCache.filterOutOfRange, _filterOutOfRange);


    glBindVertexArray(_vao);
    const GLsizei nStars = static_cast<GLsizei>(_fullData.size() / _nValuesPerStar);
    glDrawArrays(GL_POINTS, 0, nStars);

    glBindVertexArray(0);
    _program->deactivate();

    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    // Restores OpenGL blending state
    glBlendEquationSeparate(blendEquationRGB, blendEquationAlpha);
    glBlendFuncSeparate(blendSrcRGB, blendDestRGB, blendSrcAlpha, blendDestAlpha);
    glDepthMask(depthMask);
}

void RenderableStars::update(const UpdateData&) {
    if (_speckFileIsDirty) {
        loadData();
        _speckFileIsDirty = false;
        _dataIsDirty = true;
    }

    if (_fullData.empty()) {
        return;
    }

    if (_dataIsDirty) {
        const int value = _colorOption;
        LDEBUG("Regenerating data");

        createDataSlice(ColorOption(value));

        int size = static_cast<int>(_slicedData.size());

        if (_vao == 0) {
            glGenVertexArrays(1, &_vao);
        }
        if (_vbo == 0) {
            glGenBuffers(1, &_vbo);
        }
        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glBufferData(
            GL_ARRAY_BUFFER,
            size * sizeof(GLfloat),
            _slicedData.data(),
            GL_STATIC_DRAW
        );

        GLint positionAttrib = _program->attributeLocation("in_position");
        // bvLumAbsMagAppMag = bv color, luminosity, abs magnitude and app magnitude
        GLint bvLumAbsMagAppMagAttrib = _program->attributeLocation(
            "in_bvLumAbsMagAppMag"
        );

        const size_t nStars = _fullData.size() / _nValuesPerStar;
        const size_t nValues = _slicedData.size() / nStars;

        GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * nValues);

        glEnableVertexAttribArray(positionAttrib);
        glEnableVertexAttribArray(bvLumAbsMagAppMagAttrib);
        const int colorOption = _colorOption;
        switch (colorOption) {
            case ColorOption::Color:
            case ColorOption::FixedColor:
                glVertexAttribPointer(
                    positionAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(ColorVBOLayout, position)
                );
                glVertexAttribPointer(
                    bvLumAbsMagAppMagAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(ColorVBOLayout, value))
                );

                break;
            case ColorOption::Velocity:
            {
                glVertexAttribPointer(
                    positionAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(VelocityVBOLayout, position)
                );
                glVertexAttribPointer(
                    bvLumAbsMagAppMagAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(VelocityVBOLayout, value))
                );

                GLint velocityAttrib = _program->attributeLocation("in_velocity");
                glEnableVertexAttribArray(velocityAttrib);
                glVertexAttribPointer(
                    velocityAttrib,
                    3,
                    GL_FLOAT,
                    GL_TRUE,
                    stride,
                    reinterpret_cast<void*>(offsetof(VelocityVBOLayout, vx)) // NOLINT
                );

                break;
            }
            case ColorOption::Speed:
            {
                glVertexAttribPointer(
                    positionAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(SpeedVBOLayout, position)
                );
                glVertexAttribPointer(
                    bvLumAbsMagAppMagAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(SpeedVBOLayout, value))
                );

                GLint speedAttrib = _program->attributeLocation("in_speed");
                glEnableVertexAttribArray(speedAttrib);
                glVertexAttribPointer(
                    speedAttrib,
                    1,
                    GL_FLOAT,
                    GL_TRUE,
                    stride,
                    reinterpret_cast<void*>(offsetof(SpeedVBOLayout, speed))
                );
                break;
            }
            case ColorOption::OtherData:
            {
                glVertexAttribPointer(
                    positionAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    nullptr // = offsetof(OtherDataLayout, position)
                );
                glVertexAttribPointer(
                    bvLumAbsMagAppMagAttrib,
                    4,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(OtherDataLayout, value)) // NOLINT
                );
            }
        }

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);

        _dataIsDirty = false;
    }

    if (_pointSpreadFunctionTextureIsDirty) {
        LDEBUG("Reloading Point Spread Function texture");
        _pointSpreadFunctionTexture = nullptr;
        if (!_pointSpreadFunctionTexturePath.value().empty()) {
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
            _pointSpreadFunctionFile->setCallback(
                [&](const ghoul::filesystem::File&) {
                    _pointSpreadFunctionTextureIsDirty = true;
                }
            );
        }
        _pointSpreadFunctionTextureIsDirty = false;
    }

    if (_colorTextureIsDirty) {
        LDEBUG("Reloading Color Texture");
        _colorTexture = nullptr;
        if (_colorTexturePath.value() != "") {
            _colorTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_colorTexturePath)
            );
            if (_colorTexture) {
                LDEBUG(fmt::format(
                    "Loaded texture from '{}'",
                    absPath(_colorTexturePath)
                ));
                _colorTexture->uploadTexture();
            }

            _colorTextureFile = std::make_unique<ghoul::filesystem::File>(
                _colorTexturePath
            );
            _colorTextureFile->setCallback(
                [&](const ghoul::filesystem::File&) { _colorTextureIsDirty = true; }
            );
        }
        _colorTextureIsDirty = false;
    }

    //loadShapeTexture();

    if (_otherDataColorMapIsDirty) {
        LDEBUG("Reloading Color Texture");
        _otherDataColorMapTexture = nullptr;
        if (!_otherDataColorMapPath.value().empty()) {
            _otherDataColorMapTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_otherDataColorMapPath)
            );
            if (_otherDataColorMapTexture) {
                LDEBUG(fmt::format(
                    "Loaded texture from '{}'",
                    absPath(_otherDataColorMapPath)
                ));
                _otherDataColorMapTexture->uploadTexture();
            }
        }
        _otherDataColorMapIsDirty = false;

    }

    if (_program->isDirty()) {
        _program->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);
    }
}

/*
void RenderableStars::loadShapeTexture() {
    if (_shapeTextureIsDirty) {
        LDEBUG("Reloading Shape Texture");
        _shapeTexture = nullptr;
        if (_shapeTexturePath.value() != "") {
            _shapeTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_shapeTexturePath)
            );
            if (_shapeTexture) {
                LDEBUG(fmt::format(
                    "Loaded texture from '{}'",
                    absPath(_shapeTexturePath)
                ));
                _shapeTexture->uploadTexture();
            }

            _shapeTextureFile = std::make_unique<ghoul::filesystem::File>(
                _shapeTexturePath
                );
            _shapeTextureFile->setCallback(
                [&](const ghoul::filesystem::File&) { _shapeTextureIsDirty = true; }
            );
        }
        _shapeTextureIsDirty = false;
    }
}
*/

void RenderableStars::loadData() {
    std::string _file = _speckFile;
    if (!FileSys.fileExists(absPath(_file))) {
        return;
    }

    std::string cachedFile = FileSys.cacheManager()->cachedFilename(
        _file,
        ghoul::filesystem::CacheManager::Persistent::Yes
    );

    _nValuesPerStar = 0;
    _slicedData.clear();
    _fullData.clear();
    _dataNames.clear();

    bool hasCachedFile = FileSys.fileExists(cachedFile);
    if (hasCachedFile) {
        LINFO(fmt::format("Cached file '{}' used for Speck file '{}'",
            cachedFile, _file
        ));

        bool success = loadCachedFile(cachedFile);
        if (success) {
            return;
        }
        else {
            FileSys.cacheManager()->removeCacheFile(_file);
            // Intentional fall-through to the 'else' computation to generate the cache
            // file for the next run
        }
    }
    else {
        LINFO(fmt::format("Cache for Speck file '{}' not found", _file));
    }
    LINFO(fmt::format("Loading Speck file '{}'", _file));

    readSpeckFile();

    LINFO("Saving cache");
    saveCachedFile(cachedFile);
}

void RenderableStars::readSpeckFile() {
    std::string _file = _speckFile;
    std::ifstream file(_file);
    if (!file.good()) {
        LERROR(fmt::format("Failed to open Speck file '{}'", _file));
        return;
    }

    // The beginning of the speck file has a header that either contains comments
    // (signaled by a preceding '#') or information about the structure of the file
    // (signaled by the keywords 'datavar', 'texturevar', and 'texture')
    std::string line;
    while (true) {
        std::streampos position = file.tellg();
        std::getline(file, line, '\n');

        if (line[0] == '#' || line.empty()) {
            continue;
        }

        if (line.substr(0, 7) != "datavar" &&
            line.substr(0, 10) != "texturevar" &&
            line.substr(0, 7) != "texture")
        {
            // we read a line that doesn't belong to the header, so we have to jump back
            // before the beginning of the current line
            if (_enableTestGrid) {
                file.seekg(position - std::streamoff(8));
            }
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
            str >> dummy;
            str >> _nValuesPerStar;

            std::string name;
            str >> name;
            _dataNames.push_back(name);

            // +3 because the position x, y, z
            if (name == "lum") {
                _lumArrayPos = _nValuesPerStar + 3;
            }
            else if (name == "absmag") {
                _absMagArrayPos = _nValuesPerStar + 3;
            }
            else if (name == "appmag") {
                _appMagArrayPos = _nValuesPerStar + 3;
            }
            else if (name == "colorb_v") {
                _bvColorArrayPos = _nValuesPerStar + 3;
            }
            else if (name == "vx") {
                _velocityArrayPos = _nValuesPerStar + 3;
            }
            else if (name == "speed") {
                _speedArrayPos = _nValuesPerStar + 3;
            }
            _nValuesPerStar += 1; // We want the number, but the index is 0 based
        }
    }

    _nValuesPerStar += 3; // X Y Z are not counted in the Speck file indices
    _otherDataOption.addOptions(_dataNames);

    float minLumValue = std::numeric_limits<float>::max();
    float maxLumValue = std::numeric_limits<float>::min();

    do {
        std::vector<float> values(_nValuesPerStar);
        std::stringstream str(line);

        for (int i = 0; i < _nValuesPerStar; ++i) {
            str >> values[i];
        }

        bool nullArray = true;
        for (float v : values) {
            if (v != 0.0) {
                nullArray = false;
                break;
            }
        }
        minLumValue = values[_lumArrayPos] < minLumValue ?
            values[_lumArrayPos] : minLumValue;
        maxLumValue = values[_lumArrayPos] > maxLumValue ?
            values[_lumArrayPos] : maxLumValue;
        if (!nullArray) {
            _fullData.insert(_fullData.end(), values.begin(), values.end());
        }

        std::getline(file, line, '\n');

    } while (!file.eof());

    // Normalize Luminosity:
    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerStar) {
        _fullData[i + _lumArrayPos] =
            (_fullData[i + _lumArrayPos] - minLumValue) / (maxLumValue - minLumValue);
    }
}

bool RenderableStars::loadCachedFile(const std::string& file) {
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
        fileStream.read(reinterpret_cast<char*>(&_nValuesPerStar), sizeof(int32_t));

        fileStream.read(reinterpret_cast<char*>(&_lumArrayPos), sizeof(int32_t));
        fileStream.read(reinterpret_cast<char*>(&_absMagArrayPos), sizeof(int32_t));
        fileStream.read(reinterpret_cast<char*>(&_appMagArrayPos), sizeof(int32_t));
        fileStream.read(reinterpret_cast<char*>(&_bvColorArrayPos), sizeof(int32_t));
        fileStream.read(reinterpret_cast<char*>(&_velocityArrayPos), sizeof(int32_t));
        fileStream.read(reinterpret_cast<char*>(&_speedArrayPos), sizeof(int32_t));

        for (int i = 0; i < _nValuesPerStar - 3; ++i) {
            uint16_t len;
            fileStream.read(reinterpret_cast<char*>(&len), sizeof(uint16_t));
            std::vector<char> buffer(len);
            fileStream.read(buffer.data(), len);
            std::string value(buffer.begin(), buffer.end());
            _dataNames.push_back(value);
        }
        _otherDataOption.addOptions(_dataNames);

        _fullData.resize(nValues);
        fileStream.read(reinterpret_cast<char*>(
            _fullData.data()),
            nValues * sizeof(_fullData[0])
        );

        bool success = fileStream.good();
        return success;
    }
    else {
        LERROR(fmt::format("Error opening file '{}' for loading cache file", file));
        return false;
    }
}

void RenderableStars::saveCachedFile(const std::string& file) const {
    std::ofstream fileStream(file, std::ofstream::binary);

    if (!fileStream.good()) {
        LERROR(fmt::format("Error opening file '{}' for save cache file", file));
        return;
    }

    fileStream.write(
        reinterpret_cast<const char*>(&CurrentCacheVersion),
        sizeof(int8_t)
    );

    int32_t nValues = static_cast<int32_t>(_fullData.size());
    if (nValues == 0) {
        throw ghoul::RuntimeError("Error writing cache: No values were loaded");
    }
    fileStream.write(reinterpret_cast<const char*>(&nValues), sizeof(int32_t));

    int32_t nValuesPerStar = static_cast<int32_t>(_nValuesPerStar);
    fileStream.write(reinterpret_cast<const char*>(&nValuesPerStar), sizeof(int32_t));
    fileStream.write(reinterpret_cast<const char*>(&_lumArrayPos), sizeof(int32_t));
    fileStream.write(reinterpret_cast<const char*>(&_absMagArrayPos), sizeof(int32_t));
    fileStream.write(reinterpret_cast<const char*>(&_appMagArrayPos), sizeof(int32_t));
    fileStream.write(reinterpret_cast<const char*>(&_bvColorArrayPos), sizeof(int32_t));
    fileStream.write(reinterpret_cast<const char*>(&_velocityArrayPos), sizeof(int32_t));
    fileStream.write(reinterpret_cast<const char*>(&_speedArrayPos), sizeof(int32_t));

    // -3 as we don't want to save the xyz values that are in the beginning of the file
    for (int i = 0; i < _nValuesPerStar - 3; ++i) {
        uint16_t len = static_cast<uint16_t>(_dataNames[i].size());
        fileStream.write(reinterpret_cast<const char*>(&len), sizeof(uint16_t));
        fileStream.write(_dataNames[i].c_str(), len);
    }

    size_t nBytes = nValues * sizeof(_fullData[0]);
    fileStream.write(reinterpret_cast<const char*>(_fullData.data()), nBytes);
}

void RenderableStars::createDataSlice(ColorOption option) {
    _slicedData.clear();

    _otherDataRange = glm::vec2(
        std::numeric_limits<float>::max(),
        -std::numeric_limits<float>::max()
    );

    for (size_t i = 0; i < _fullData.size(); i += _nValuesPerStar) {
        glm::vec3 position = glm::vec3(
            _fullData[i + 0],
            _fullData[i + 1],
            _fullData[i + 2]
        );
        position *= openspace::distanceconstants::Parsec;

        switch (option) {
            case ColorOption::Color:
            case ColorOption::FixedColor:
            {
                union {
                    ColorVBOLayout value;
                    std::array<float, sizeof(ColorVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = { { position[0], position[1], position[2] } };

                if (_enableTestGrid) {
                    float sunColor = 0.650f;
                    layout.value.value = sunColor;// _fullData[i + 3];
                }
                else {
                    layout.value.value = _fullData[i + _bvColorArrayPos];
                }

                layout.value.luminance = _fullData[i + _lumArrayPos];
                layout.value.absoluteMagnitude = _fullData[i + _absMagArrayPos];
                layout.value.apparentMagnitude = _fullData[i + _appMagArrayPos];

                _slicedData.insert(
                    _slicedData.end(),
                    layout.data.begin(),
                    layout.data.end());

                break;
            }
            case ColorOption::Velocity:
            {
                union {
                    VelocityVBOLayout value;
                    std::array<float, sizeof(VelocityVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = { { position[0], position[1], position[2] } };

                layout.value.value = _fullData[i + _bvColorArrayPos];
                layout.value.luminance = _fullData[i + _lumArrayPos];
                layout.value.absoluteMagnitude = _fullData[i + _absMagArrayPos];
                layout.value.apparentMagnitude = _fullData[i + _appMagArrayPos];

                layout.value.vx = _fullData[i + _velocityArrayPos];
                layout.value.vy = _fullData[i + _velocityArrayPos + 1];
                layout.value.vz = _fullData[i + _velocityArrayPos + 2];

                _slicedData.insert(
                    _slicedData.end(),
                    layout.data.begin(),
                    layout.data.end()
                );
                break;
            }
            case ColorOption::Speed:
            {
                union {
                    SpeedVBOLayout value;
                    std::array<float, sizeof(SpeedVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = { { position[0], position[1], position[2] } };

                layout.value.value = _fullData[i + _bvColorArrayPos];
                layout.value.luminance = _fullData[i + _lumArrayPos];
                layout.value.absoluteMagnitude = _fullData[i + _absMagArrayPos];
                layout.value.apparentMagnitude = _fullData[i + _appMagArrayPos];

                layout.value.speed = _fullData[i + _speedArrayPos];

                _slicedData.insert(
                    _slicedData.end(),
                    layout.data.begin(),
                    layout.data.end()
                );
                break;
            }
            case ColorOption::OtherData:
            {
                union {
                    OtherDataLayout value;
                    std::array<float, sizeof(OtherDataLayout)> data;
                } layout = {};

                layout.value.position = { { position[0], position[1], position[2] } };

                int index = _otherDataOption.value();
                // plus 3 because of the position
                layout.value.value = _fullData[i + index + 3];

                if (_staticFilterValue.has_value() &&
                    layout.value.value == _staticFilterValue)
                {
                    layout.value.value = _staticFilterReplacementValue;
                }

                glm::vec2 range = _otherDataRange.value();
                range.x = std::min(range.x, layout.value.value);
                range.y = std::max(range.y, layout.value.value);
                _otherDataRange = range;
                _otherDataRange.setMinValue(glm::vec2(range.x));
                _otherDataRange.setMaxValue(glm::vec2(range.y));

                layout.value.luminance = _fullData[i + _lumArrayPos];
                layout.value.absoluteMagnitude = _fullData[i + _absMagArrayPos];
                layout.value.apparentMagnitude = _fullData[i + _appMagArrayPos];

                _slicedData.insert(
                    _slicedData.end(),
                    layout.data.begin(),
                    layout.data.end()
                );

                break;
            }
        }
    }
}

} // namespace openspace
