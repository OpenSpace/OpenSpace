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

#include <modules/space/rendering/renderablestars.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/util/updatestructures.h>
#include <openspace/util/distanceconstants.h>
#include <openspace/engine/openspaceengine.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <array>
#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <limits>
#include <type_traits>

namespace {
    constexpr std::string_view _loggerCat = "RenderableStars";

    constexpr std::array<const char*, 17> UniformNames = {
        "modelMatrix", "cameraUp", "cameraViewProjectionMatrix", "colorOption",
        "magnitudeExponent", "eyePosition", "psfParamConf", "lumCent", "radiusCent",
        "brightnessCent", "colorTexture", "alphaValue", "psfTexture", "otherDataTexture",
        "otherDataRange", "filterOutOfRange", "fixedColor"
    };

    enum RenderMethod {
        PointSpreadFunction = 0,
        TextureBased
    };

    enum SizeComposition {
        AppBrightness = 0,
        LumSize,
        LumSizeAppBrightness,
        AbsMagnitude,
        AppMagnitude,
        DistanceModulus
    };

    constexpr int PsfMethodSpencer = 0;
    constexpr int PsfMethodMoffat = 1;

    constexpr int PsfTextureSize = 64;
    constexpr int ConvolvedfTextureSize = 257;

    constexpr double PARSEC = 0.308567756E17;

    struct ColorVBOLayout {
        std::array<float, 3> position;
        float value;
        float luminance;
        float absoluteMagnitude;
        float apparentMagnitude;
    };

    struct VelocityVBOLayout {
        std::array<float, 3> position;
        float value;
        float luminance;
        float absoluteMagnitude;
        float apparentMagnitude;

        float vx; // v_x
        float vy; // v_y
        float vz; // v_z
    };

    struct SpeedVBOLayout {
        std::array<float, 3> position;
        float value;
        float luminance;
        float absoluteMagnitude;
        float apparentMagnitude;

        float speed;
    };

    struct OtherDataLayout {
        std::array<float, 3> position;
        float value;
        float luminance;
        float absoluteMagnitude;
        float apparentMagnitude;
    };

    constexpr openspace::properties::Property::PropertyInfo SpeckFileInfo = {
        "SpeckFile",
        "Speck File",
        "The speck file that is loaded to get the data for rendering these stars",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "ColorBV Texture",
        "The path to the texture that is used to convert from the B-V value of the star "
        "to its color. The texture is used as a one dimensional lookup function",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingBvInfo = {
        "MappingBV",
        "Mapping (bv-color)",
        "The name of the variable in the speck file that is used as the b-v color "
        "variable",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingLuminanceInfo = {
        "MappingLuminance",
        "Mapping (luminance)",
        "The name of the variable in the speck file that is used as the luminance "
        "variable",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingAbsMagnitudeInfo = {
        "MappingAbsMagnitude",
        "Mapping (absolute magnitude)",
        "The name of the variable in the speck file that is used as the absolute "
        "magnitude variable",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingAppMagnitudeInfo = {
        "MappingAppMagnitude",
        "Mapping (apparent magnitude)",
        "The name of the variable in the speck file that is used as the apparent "
        "magnitude variable",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingVxInfo = {
        "MappingVx",
        "Mapping (vx)",
        "The name of the variable in the speck file that is used as the star velocity "
        "along the x-axis",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingVyInfo = {
        "MappingVy",
        "Mapping (vy)",
        "The name of the variable in the speck file that is used as the star velocity "
        "along the y-axis",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingVzInfo = {
        "MappingVz",
        "Mapping (vz)",
        "The name of the variable in the speck file that is used as the star velocity "
        "along the z-axis",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingSpeedInfo = {
        "MappingSpeed",
        "Mapping (speed)",
        "The name of the variable in the speck file that is used as the speed",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorOptionInfo = {
        "ColorOption",
        "Color Option",
        "This value determines which quantity is used for determining the color of the "
        "stars",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataOptionInfo = {
        "OtherData",
        "Other Data Column",
        "The index of the speck file data column that is used as the color input",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataValueRangeInfo = {
        "OtherDataValueRange",
        "Range of the other data values",
        "This value is the min/max value range that is used to normalize the other data "
        "values so they can be used by the specified color map",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FixedColorInfo = {
        "FixedColorValue",
        "Color used for fixed star colors",
        "The color that should be used if the 'Fixed Color' value is used",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataColorMapInfo = {
        "OtherDataColorMap",
        "Other Data Color Map",
        "The color map that is used if the 'Other Data' rendering method is selected",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilterOutOfRangeInfo = {
        "FilterOutOfRange",
        "Filter Out of Range",
        "Determines whether other data values outside the value range should be visible "
        "or filtered away",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    // Old Method
    constexpr openspace::properties::Property::PropertyInfo PsfTextureInfo = {
        "Texture",
        "Point Spread Function Texture",
        "The path to the texture that should be used as a point spread function for the "
        "stars",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    //constexpr openspace::properties::Property::PropertyInfo ShapeTextureInfo = {
    //    "ShapeTexture",
    //    "Shape Texture to be convolved",
    //    "The path to the texture that should be used as the base shape for the stars"
    //};

    // PSF
    constexpr openspace::properties::Property::PropertyInfo MagnitudeExponentInfo = {
        "MagnitudeExponent",
        "Magnitude Exponent",
        "Adjust star magnitude by 10^MagnitudeExponent. Stars closer than this distance "
        "are given full opacity. Farther away, stars dim proportionally to the "
        "logarithm of their distance",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo RenderMethodOptionInfo = {
        "RenderMethod",
        "Render Method",
        "Render method for the stars",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo
        UserProvidedTextureOptionInfo =
    {
        "UserProvidedTexture",
        "User Provided Texture",
        ""
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo
        ParametersOwnerOptionInfo =
    {
        "ParametersOwner",
        "Parameters Options",
        ""
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo MoffatMethodOptionInfo =
    {
        "MoffatMethodOption",
        "Moffat Method",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo PSFMethodOptionInfo = {
        "PSFMethodOptionInfo",
        "PSF Method Option",
        "Debug option for PSF main function: Spencer or Moffat",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo SizeCompositionOptionInfo = {
        "SizeComposition",
        "Size Composition Option",
        "Base multiplyer for the final stars' sizes",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LumPercentInfo = {
        "LumPercent",
        "Luminosity Contribution",
        "Luminosity Contribution",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusPercentInfo = {
        "RadiusPercent",
        "Radius Contribution",
        "Radius Contribution",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BrightnessPercentInfo = {
        "BrightnessPercent",
        "App Brightness Contribution",
        "App Brightness Contribution",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo
        SpencerPSFParamOwnerInfo =
    {
        "SpencerPSFParamOwner",
        "Spencer PSF Paramameters",
        "PSF parameters for Spencer"
    };

    constexpr openspace::properties::Property::PropertyInfo P0ParamInfo = {
        "P0Param",
        "P0",
        "P0 parameter contribution",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo P1ParamInfo = {
        "P1Param",
        "P1",
        "P1 parameter contribution",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo P2ParamInfo = {
        "P2Param",
        "P2",
        "P2 parameter contribution",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AlphaConstInfo = {
        "AlphaConst",
        "Alpha",
        "Empirical Alpha Constant",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo
        MoffatPSFParamOwnerInfo =
    {
        "MoffatPSFParam",
        "Moffat PSF Parameters",
        "PSF parameters for Moffat"
    };

    constexpr openspace::properties::Property::PropertyInfo FWHMInfo = {
        "FWHM",
        "FWHM",
        "Moffat's FWHM",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BetaInfo = {
        "Beta",
        "Beta",
        "Moffat's Beta Constant",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInDistancesInfo = {
        "FadeInDistances",
        "Fade-In Start and End Distances",
        "These values determine the initial and final distances from the center of "
        "our galaxy from which the astronomical object will start and end "
        "fading-in",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo DisableFadeInInfo = {
        "DisableFadeIn",
        "Disable Fade-in effect",
        "Enables/Disables the Fade-in effect",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableStars)]] Parameters {
        // The path to the SPECK file containing information about the stars being
        // rendered
        std::filesystem::path speckFile [[codegen::key("File")]];

        // [[codegen::verbatim(ColorTextureInfo.description)]]
        std::filesystem::path colorMap;

        enum class ColorOption {
            Color,
            Velocity,
            Speed,
            OtherData [[codegen::key("Other Data")]],
            FixedColor [[codegen::key("Fixed Color")]]
        };
        // [[codegen::verbatim(ColorOptionInfo.description)]]
        std::optional<ColorOption> colorOption;

        // [[codegen::verbatim(OtherDataOptionInfo.description)]]
        std::optional<std::string> otherData;

        // [[codegen::verbatim(OtherDataColorMapInfo.description)]]
        std::optional<std::string> otherDataColorMap;

        // [[codegen::verbatim(FilterOutOfRangeInfo.description)]]
        std::optional<bool> filterOutOfRange;

        // This value specifies a value that is always filtered out of the value ranges on
        // loading. This can be used to trim the dataset's automatic value range
        std::optional<float> staticFilter;

        // This is the value that is used to replace statically filtered values. Setting
        // this value only makes sense if 'StaticFilter' is 'true', as well
        std::optional<float> staticFilterReplacement;

        // [[codegen::verbatim(MagnitudeExponentInfo.description)]]
        std::optional<float> magnitudeExponent;

        enum class [[codegen::map(RenderMethod)]] RenderMethod {
            PointSpreadFunction [[codegen::key("PSF")]],
            TextureBased [[codegen::key("Texture Based")]]
        };

        // [[codegen::verbatim(RenderMethodOptionInfo.description)]]
        RenderMethod renderMethod;

        // [[codegen::verbatim(PsfTextureInfo.description)]]
        std::filesystem::path texture;

        enum class [[codegen::map(SizeComposition)]] SizeComposition {
            AppBrightness [[codegen::key("App Brightness")]],
            LumSize [[codegen::key("Lum and Size")]],
            LumSizeAppBrightness [[codegen::key("Lum, Size and App Brightness")]],
            AbsMagnitude [[codegen::key("Abs Magnitude")]],
            AppMagnitude [[codegen::key("App Magnitude")]],
            DistanceModulus [[codegen::key("Distance Modulus")]]
        };

        // [[codegen::verbatim(SizeCompositionOptionInfo.description)]]
        std::optional<SizeComposition> sizeComposition;

        struct DataMapping {
            // [[codegen::verbatim(MappingBvInfo.description)]]
            std::optional<std::string> bv;
            // [[codegen::verbatim(MappingLuminanceInfo.description)]]
            std::optional<std::string> luminance;
            // [[codegen::verbatim(MappingAbsMagnitudeInfo.description)]]
            std::optional<std::string> absoluteMagnitude;
            // [[codegen::verbatim(MappingAppMagnitudeInfo.description)]]
            std::optional<std::string> apparentMagnitude;
            // [[codegen::verbatim(MappingVxInfo.description)]]
            std::optional<std::string> vx;
            // [[codegen::verbatim(MappingVyInfo.description)]]
            std::optional<std::string> vy;
            // [[codegen::verbatim(MappingVzInfo.description)]]
            std::optional<std::string> vz;
            // [[codegen::verbatim(MappingSpeedInfo.description)]]
            std::optional<std::string> speed;
        };
        // The mappings between data values and the variable names specified in the speck
        // file
        DataMapping dataMapping;

        // [[codegen::verbatim(FadeInDistancesInfo.description)]]
        std::optional<glm::dvec2> fadeInDistances;

        // [[codegen::verbatim(DisableFadeInInfo.description)]]
        std::optional<bool> disableFadeIn;
    };
#include "renderablestars_codegen.cpp"
}  // namespace

namespace openspace {

documentation::Documentation RenderableStars::Documentation() {
    return codegen::doc<Parameters>("space_renderablestars");
}

RenderableStars::RenderableStars(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _speckFile(SpeckFileInfo)
    , _colorTexturePath(ColorTextureInfo)
    //, _shapeTexturePath(ShapeTextureInfo)
    , _dataMappingContainer({ "DataMapping", "Data Mapping" })
    , _dataMapping{
        properties::StringProperty(MappingBvInfo),
        properties::StringProperty(MappingLuminanceInfo),
        properties::StringProperty(MappingAbsMagnitudeInfo),
        properties::StringProperty(MappingAppMagnitudeInfo),
        properties::StringProperty(MappingVxInfo),
        properties::StringProperty(MappingVyInfo),
        properties::StringProperty(MappingVzInfo),
        properties::StringProperty(MappingSpeedInfo)
    }
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
    , _fixedColor(FixedColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _filterOutOfRange(FilterOutOfRangeInfo, false)
    , _pointSpreadFunctionTexturePath(PsfTextureInfo)
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
    , _magnitudeExponent(MagnitudeExponentInfo, 6.2f, 5.f, 8.f)
    , _spencerPSFParamOwner(SpencerPSFParamOwnerInfo)
    , _p0Param(P0ParamInfo, 0.384f, 0.f, 1.f)
    , _p1Param(P1ParamInfo, 0.478f, 0.f, 1.f)
    , _p2Param(P2ParamInfo, 0.138f, 0.f, 1.f)
    , _spencerAlphaConst(AlphaConstInfo, 0.02f, 0.000001f, 5.f)
    , _moffatPSFParamOwner(MoffatPSFParamOwnerInfo)
    , _FWHMConst(FWHMInfo, 10.4f, 0.f, 100.f)
    , _moffatBetaConst(BetaInfo, 4.765f, 0.f, 100.f)
    , _renderingMethodOption(
        RenderMethodOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _userProvidedTextureOwner(UserProvidedTextureOptionInfo)
    , _parametersOwner(ParametersOwnerOptionInfo)
    , _moffatMethodOwner(MoffatMethodOptionInfo)
    , _fadeInDistances(
        FadeInDistancesInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(100.f)
    )
    , _disableFadeInDistance(DisableFadeInInfo, true)
{
    using File = ghoul::filesystem::File;

    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _dataMapping.bvColor = p.dataMapping.bv.value_or("");
    _dataMapping.bvColor.onChange([this]() { _dataIsDirty = true; });
    _dataMappingContainer.addProperty(_dataMapping.bvColor);

    _dataMapping.luminance = p.dataMapping.luminance.value_or("");
    _dataMapping.luminance.onChange([this]() { _dataIsDirty = true; });
    _dataMappingContainer.addProperty(_dataMapping.luminance);

    _dataMapping.absoluteMagnitude = p.dataMapping.absoluteMagnitude.value_or("");
    _dataMapping.absoluteMagnitude.onChange([this]() { _dataIsDirty = true; });
    _dataMappingContainer.addProperty(_dataMapping.absoluteMagnitude);

    _dataMapping.apparentMagnitude = p.dataMapping.apparentMagnitude.value_or("");
    _dataMapping.apparentMagnitude.onChange([this]() { _dataIsDirty = true; });
    _dataMappingContainer.addProperty(_dataMapping.apparentMagnitude);

    _dataMapping.vx = p.dataMapping.vx.value_or("");
    _dataMapping.vx.onChange([this]() { _dataIsDirty = true; });
    _dataMappingContainer.addProperty(_dataMapping.vx);

    _dataMapping.vy = p.dataMapping.vy.value_or("");
    _dataMapping.vy.onChange([this]() { _dataIsDirty = true; });
    _dataMappingContainer.addProperty(_dataMapping.vy);

    _dataMapping.vz = p.dataMapping.vz.value_or("");
    _dataMapping.vz.onChange([this]() { _dataIsDirty = true; });
    _dataMappingContainer.addProperty(_dataMapping.vz);

    _dataMapping.speed = p.dataMapping.speed.value_or("");
    _dataMapping.speed.onChange([this]() { _dataIsDirty = true; });
    _dataMappingContainer.addProperty(_dataMapping.speed);

    addPropertySubOwner(_dataMappingContainer);

    _speckFile = p.speckFile.string();
    _speckFile.onChange([this]() { _speckFileIsDirty = true; });
    addProperty(_speckFile);

    _colorTexturePath = p.colorMap.string();
    _colorTextureFile = std::make_unique<File>(_colorTexturePath.value());

    //_shapeTexturePath = absPath(dictionary.value<std::string>(
    //    ShapeTextureInfo.identifier
    //    ));
    //_shapeTextureFile = std::make_unique<File>(_shapeTexturePath);

    if (p.otherDataColorMap.has_value()) {
        _otherDataColorMapPath = absPath(*p.otherDataColorMap).string();
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
    if (p.colorOption.has_value()) {
        switch (*p.colorOption) {
            case Parameters::ColorOption::Color:
                _colorOption = ColorOption::Color;
                break;
            case Parameters::ColorOption::Velocity:
                _colorOption = ColorOption::Velocity;
                break;
            case Parameters::ColorOption::Speed:
                _colorOption = ColorOption::Speed;
                break;
            case Parameters::ColorOption::OtherData:
                _colorOption = ColorOption::OtherData;
                break;
            case Parameters::ColorOption::FixedColor:
                _colorOption = ColorOption::FixedColor;
                break;
        }
    }
    _colorOption.onChange([&] { _dataIsDirty = true; });
    addProperty(_colorOption);

    _colorTexturePath.onChange([&] {
        if (std::filesystem::exists(_colorTexturePath.value())) {
            _colorTextureIsDirty = true;
        }
        else {
            LWARNING(std::format("File not found: {}", _colorTexturePath.value()));
        }
    });
    _colorTextureFile->setCallback([this]() { _colorTextureIsDirty = true; });
    addProperty(_colorTexturePath);

    _queuedOtherData = p.otherData.value_or(_queuedOtherData);

    _otherDataOption.onChange([this]() { _dataIsDirty = true; });
    addProperty(_otherDataOption);

    _otherDataRange.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_otherDataRange);

    addProperty(_otherDataColorMapPath);
    _otherDataColorMapPath.onChange([this]() {
        if (std::filesystem::exists(_otherDataColorMapPath.value())) {
            _otherDataColorMapIsDirty = true;
        }
        else {
            LWARNING(std::format("File not found: {}", _otherDataColorMapPath.value()));
        }
    });

    _staticFilterValue = p.staticFilter;
    _staticFilterReplacementValue =
        p.staticFilterReplacement.value_or(_staticFilterReplacementValue);

    addProperty(_filterOutOfRange);

    _renderingMethodOption.addOption(
        RenderMethod::PointSpreadFunction,
        "Point Spread Function Based"
    );
    _renderingMethodOption.addOption(RenderMethod::TextureBased, "Textured Based");
    addProperty(_renderingMethodOption);

    _renderingMethodOption = codegen::map<RenderMethod>(p.renderMethod);

    _pointSpreadFunctionTexturePath = absPath(p.texture.string()).string();
    _pointSpreadFunctionFile = std::make_unique<File>(
        _pointSpreadFunctionTexturePath.value()
    );
    _pointSpreadFunctionTexturePath.onChange([this]() {
        _pointSpreadFunctionTextureIsDirty = true;
    });
    _pointSpreadFunctionFile->setCallback([this]() {
        _pointSpreadFunctionTextureIsDirty = true;
    });
    _userProvidedTextureOwner.addProperty(_pointSpreadFunctionTexturePath);

    _psfMethodOption.addOption(PsfMethodSpencer, "Spencer's Function");
    _psfMethodOption.addOption(PsfMethodMoffat, "Moffat's Function");
    _psfMethodOption = PsfMethodSpencer;
    _psfMethodOption.onChange([this]() { renderPSFToTexture(); });
    _parametersOwner.addProperty(_psfMethodOption);

    _psfMultiplyOption.addOption(AppBrightness, "Use Star's Apparent Brightness");
    _psfMultiplyOption.addOption(LumSize, "Use Star's Luminosity and Size");
    _psfMultiplyOption.addOption(
        LumSizeAppBrightness,
        "Luminosity, Size, App Brightness"
    );
    _psfMultiplyOption.addOption(AbsMagnitude, "Absolute Magnitude");
    _psfMultiplyOption.addOption(AppMagnitude, "Apparent Magnitude");
    _psfMultiplyOption.addOption(DistanceModulus, "Distance Modulus");


    if (p.sizeComposition.has_value()) {
        _psfMultiplyOption =
            static_cast<int>(codegen::map<SizeComposition>(*p.sizeComposition));
    }
    else {
        _psfMultiplyOption = 5;
    }

    _parametersOwner.addProperty(_psfMultiplyOption);
    _parametersOwner.addProperty(_lumCent);
    _parametersOwner.addProperty(_radiusCent);
    _parametersOwner.addProperty(_brightnessCent);

    _magnitudeExponent = p.magnitudeExponent.value_or(_magnitudeExponent);
    _parametersOwner.addProperty(_magnitudeExponent);

    auto renderPsf = [this]() { renderPSFToTexture(); };

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

    if (p.fadeInDistances.has_value()) {
        _fadeInDistances = *p.fadeInDistances;
        _disableFadeInDistance = false;
        _fadeInDistances.setViewOption(properties::Property::ViewOptions::MinMaxRange);
        addProperty(_fadeInDistances);
        addProperty(_disableFadeInDistance);
    }
}

bool RenderableStars::isReady() const {
    return _program && _pointSpreadFunctionTexture;
}

void RenderableStars::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "Star",
        absPath("${MODULE_SPACE}/shaders/star_vs.glsl"),
        absPath("${MODULE_SPACE}/shaders/star_fs.glsl"),
        absPath("${MODULE_SPACE}/shaders/star_ge.glsl")
    );

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache, UniformNames);

    loadData();

    // We need to wait until after loading the data until we can see if the requested
    // data value actually exists or not.  Once we determine the index, we no longer
    // need the value and can clear it
    if (!_queuedOtherData.empty()) {
        const int idx = _dataset.index(_queuedOtherData);
        if (idx == -1) {
            LERROR(std::format("Could not find other data column {}", _queuedOtherData));
        }
        else {
            _otherDataOption = idx;
            _queuedOtherData.clear();
        }
    }
    _speckFileIsDirty = false;

    LDEBUG("Creating Polygon Texture");

    glGenVertexArrays(1, &_psfVao);
    glGenBuffers(1, &_psfVbo);
    glBindVertexArray(_psfVao);
    glBindBuffer(GL_ARRAY_BUFFER, _psfVbo);

    constexpr std::array<GLfloat, 24> VertexData = {
        //x      y     s     t
        -1.f, -1.f, 0.f, 0.f,
         1.f,  1.f, 1.f, 1.f,
        -1.f,  1.f, 0.f, 1.f,
        -1.f, -1.f, 0.f, 0.f,
         1.f, -1.f, 1.f, 0.f,
         1.f,  1.f, 1.f, 1.f
    };

    glBufferData(GL_ARRAY_BUFFER, sizeof(VertexData), VertexData.data(), GL_STATIC_DRAW);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), nullptr);
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
        PsfTextureSize,
        PsfTextureSize,
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
        ConvolvedfTextureSize,
        ConvolvedfTextureSize,
        0,
        GL_RGBA,
        GL_BYTE,
        nullptr
    );

    //loadShapeTexture();
    loadPSFTexture();
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
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableStars::loadPSFTexture() {
    _pointSpreadFunctionTexture = nullptr;
    if (!_pointSpreadFunctionTexturePath.value().empty() &&
        std::filesystem::exists(_pointSpreadFunctionTexturePath.value()))
    {
        _pointSpreadFunctionTexture = ghoul::io::TextureReader::ref().loadTexture(
            absPath(_pointSpreadFunctionTexturePath).string(),
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
            _pointSpreadFunctionTexturePath.value()
        );
        _pointSpreadFunctionFile->setCallback(
            [this]() { _pointSpreadFunctionTextureIsDirty = true; }
        );
    }
    _pointSpreadFunctionTextureIsDirty = false;
}

void RenderableStars::renderPSFToTexture() {
    // Creates the FBO for the calculations
    GLuint psfFBO = 0;
    glGenFramebuffers(1, &psfFBO);
    glBindFramebuffer(GL_FRAMEBUFFER, psfFBO);
    GLenum drawBuffers = GL_COLOR_ATTACHMENT0;
    glDrawBuffers(1, &drawBuffers);

    glFramebufferTexture(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, _psfTexture, 0);
    glViewport(0, 0, PsfTextureSize, PsfTextureSize);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    std::unique_ptr<ghoul::opengl::ProgramObject> program =
        ghoul::opengl::ProgramObject::Build(
            "RenderStarPSFToTexture",
            absPath("${MODULE_SPACE}/shaders/psfToTexture_vs.glsl"),
            absPath("${MODULE_SPACE}/shaders/psfToTexture_fs.glsl")
        );

    program->activate();
    constexpr std::array<float, 4> Black = { 0.f, 0.f, 0.f, 0.f };
    glClearBufferfv(GL_COLOR, 0, Black.data());

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
    glDeleteFramebuffers(1, &psfFBO);
    //glDeleteFramebuffers(1, &convolveFBO);

    // Restores OpenGL blending state
    global::renderEngine->openglStateCache().resetBlendState();
}

void RenderableStars::render(const RenderData& data, RendererTasks&) {
    if (_dataset.entries.empty()) {
        return;
    }

    glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glDepthMask(false);

    _program->activate();

    const glm::dvec3 eyePosition = glm::dvec3(
        glm::inverse(data.camera.combinedViewMatrix()) * glm::dvec4(0.0, 0.0, 0.0, 1.0)
    );
    _program->setUniform(_uniformCache.eyePosition, eyePosition);

    const glm::dvec3 cameraUp = data.camera.lookUpVectorWorldSpace();
    _program->setUniform(_uniformCache.cameraUp, cameraUp);

    const glm::dmat4 modelMatrix = calcModelTransform(data);

    const glm::dmat4 projectionMatrix = glm::dmat4(data.camera.projectionMatrix());

    const glm::dmat4 cameraViewProjectionMatrix =
        projectionMatrix * data.camera.combinedViewMatrix();

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
        const float distCamera = static_cast<float>(
            glm::length(data.camera.positionVec3())
        );
        const glm::vec2 fadeRange = _fadeInDistances;
        const double a = 1.f / ((fadeRange.y - fadeRange.x) * PARSEC);
        const double b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        const double funcValue = a * distCamera + b;
        fadeInVariable *= static_cast<float>(funcValue > 1.f ? 1.f : funcValue);

        _program->setUniform(_uniformCache.alphaValue, opacity() * fadeInVariable);
    }
    else {
        _program->setUniform(_uniformCache.alphaValue, opacity());
    }

    ghoul::opengl::TextureUnit psfUnit;
    psfUnit.activate();

    if (_renderingMethodOption.value() == RenderMethod::PointSpreadFunction) {
        glBindTexture(GL_TEXTURE_2D, _psfTexture);\
        // Convolutioned texture
        //glBindTexture(GL_TEXTURE_2D, _convolvedTexture);
    }
    else if (_renderingMethodOption.value() == RenderMethod::TextureBased) {
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
    const GLsizei nStars = static_cast<GLsizei>(_dataset.entries.size());
    glDrawArrays(GL_POINTS, 0, nStars);

    glBindVertexArray(0);
    _program->deactivate();

    // Restores OpenGL blending state
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}

void RenderableStars::update(const UpdateData&) {
    if (_speckFileIsDirty) {
        loadData();
        _speckFileIsDirty = false;
        _dataIsDirty = true;
    }

    if (_dataset.entries.empty()) {
        return;
    }

    if (_dataIsDirty) {
        const int value = _colorOption;
        LDEBUG("Regenerating data");

        std::vector<float> slice = createDataSlice(ColorOption(value));

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
            slice.size() * sizeof(GLfloat),
            slice.data(),
            GL_STATIC_DRAW
        );

        const GLint positionAttrib = _program->attributeLocation("in_position");
        // bvLumAbsMagAppMag = bv color, luminosity, abs magnitude and app magnitude
        const GLint bvLumAbsMagAppMagAttrib = _program->attributeLocation(
            "in_bvLumAbsMagAppMag"
        );

        const size_t nStars = _dataset.entries.size();
        const size_t nValues = slice.size() / nStars;

        const GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * nValues);

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

                const GLint velocityAttrib = _program->attributeLocation("in_velocity");
                glEnableVertexAttribArray(velocityAttrib);
                glVertexAttribPointer(
                    velocityAttrib,
                    3,
                    GL_FLOAT,
                    GL_TRUE,
                    stride,
                    reinterpret_cast<void*>(offsetof(VelocityVBOLayout, vx))
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

                const GLint speedAttrib = _program->attributeLocation("in_speed");
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
                    reinterpret_cast<void*>(offsetof(OtherDataLayout, value))
                );
            }
        }

        glBindBuffer(GL_ARRAY_BUFFER, 0);
        glBindVertexArray(0);

        _dataIsDirty = false;
    }

    if (_pointSpreadFunctionTextureIsDirty) {
        LDEBUG("Reloading Point Spread Function texture");
        loadPSFTexture();
    }

    if (_colorTextureIsDirty) {
        LDEBUG("Reloading Color Texture");
        _colorTexture = nullptr;
        if (!_colorTexturePath.value().empty()) {
            _colorTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_colorTexturePath).string(),
                1
            );
            if (_colorTexture) {
                LDEBUG(std::format(
                    "Loaded texture from '{}'", absPath(_colorTexturePath)
                ));
                _colorTexture->uploadTexture();
            }

            _colorTextureFile = std::make_unique<ghoul::filesystem::File>(
                _colorTexturePath.value()
            );
            _colorTextureFile->setCallback([this]() { _colorTextureIsDirty = true; });
        }
        _colorTextureIsDirty = false;
    }

    //loadShapeTexture();

    if (_otherDataColorMapIsDirty) {
        LDEBUG("Reloading Color Texture");
        _otherDataColorMapTexture = nullptr;
        if (!_otherDataColorMapPath.value().empty()) {
            _otherDataColorMapTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_otherDataColorMapPath).string(),
                1
            );
            if (_otherDataColorMapTexture) {
                LDEBUG(std::format(
                    "Loaded texture from '{}'", absPath(_otherDataColorMapPath)
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

void RenderableStars::loadData() {
    const std::filesystem::path file = absPath(_speckFile);
    if (!std::filesystem::is_regular_file(file)) {
        return;
    }

    _dataset = dataloader::data::loadFileWithCache(file);
    if (_dataset.entries.empty()) {
        return;
    }

    std::vector<std::string> variableNames;
    variableNames.reserve(_dataset.variables.size());
    for (const dataloader::Dataset::Variable& v : _dataset.variables) {
        variableNames.push_back(v.name);
    }
    _otherDataOption.addOptions(variableNames);

    const bool success = _dataset.normalizeVariable("lum");
    if (!success) {
        throw ghoul::RuntimeError("Could not find required variable 'luminosity'");
    }
}

std::vector<float> RenderableStars::createDataSlice(ColorOption option) {
    const int bvIdx = std::max(_dataset.index(_dataMapping.bvColor.value()), 0);
    const int lumIdx = std::max(_dataset.index(_dataMapping.luminance.value()), 0);
    const int absMagIdx = std::max(
        _dataset.index(_dataMapping.absoluteMagnitude.value()),
        0
    );
    const int appMagIdx = std::max(
        _dataset.index(_dataMapping.apparentMagnitude.value()),
        0
    );
    const int vxIdx = std::max(_dataset.index(_dataMapping.vx.value()), 0);
    const int vyIdx = std::max(_dataset.index(_dataMapping.vy.value()), 0);
    const int vzIdx = std::max(_dataset.index(_dataMapping.vz.value()), 0);
    const int speedIdx = std::max(_dataset.index(_dataMapping.speed.value()), 0);

    _otherDataRange = glm::vec2(
        std::numeric_limits<float>::max(),
        -std::numeric_limits<float>::max()
    );

    double maxRadius = 0.0;

    std::vector<float> result;
    // 7 for the default Color option of 3 positions + bv + lum + abs + app magnitude
    result.reserve(_dataset.entries.size() * 7);
    for (const dataloader::Dataset::Entry& e : _dataset.entries) {
        glm::dvec3 position = glm::dvec3(e.position) * distanceconstants::Parsec;
        maxRadius = std::max(maxRadius, glm::length(position));

        switch (option) {
            case ColorOption::Color:
            case ColorOption::FixedColor:
            {
                union {
                    ColorVBOLayout value;
                    std::array<float, sizeof(ColorVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = { {
                    static_cast<float>(position[0]),
                    static_cast<float>(position[1]),
                    static_cast<float>(position[2])
                }};

                layout.value.value = e.data[bvIdx];
                layout.value.luminance = e.data[lumIdx];
                layout.value.absoluteMagnitude = e.data[absMagIdx];
                layout.value.apparentMagnitude = e.data[appMagIdx];

                result.insert(result.end(), layout.data.begin(), layout.data.end());
                break;
            }
            case ColorOption::Velocity:
            {
                union {
                    VelocityVBOLayout value;
                    std::array<float, sizeof(VelocityVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = {{
                    static_cast<float>(position[0]),
                    static_cast<float>(position[1]),
                    static_cast<float>(position[2])
                }};

                layout.value.value = e.data[bvIdx];
                layout.value.luminance = e.data[lumIdx];
                layout.value.absoluteMagnitude = e.data[absMagIdx];
                layout.value.apparentMagnitude = e.data[appMagIdx];

                layout.value.vx = e.data[vxIdx];
                layout.value.vy = e.data[vyIdx];
                layout.value.vz = e.data[vzIdx];

                result.insert(result.end(), layout.data.begin(), layout.data.end());
                break;
            }
            case ColorOption::Speed:
            {
                union {
                    SpeedVBOLayout value;
                    std::array<float, sizeof(SpeedVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = {{
                    static_cast<float>(position[0]),
                    static_cast<float>(position[1]),
                    static_cast<float>(position[2])
                }};

                layout.value.value = e.data[bvIdx];
                layout.value.luminance = e.data[lumIdx];
                layout.value.absoluteMagnitude = e.data[absMagIdx];
                layout.value.apparentMagnitude = e.data[appMagIdx];
                layout.value.speed = e.data[speedIdx];

                result.insert(result.end(), layout.data.begin(), layout.data.end());
                break;
            }
            case ColorOption::OtherData:
            {
                union {
                    OtherDataLayout value;
                    std::array<float, sizeof(OtherDataLayout)> data;
                } layout = {};

                layout.value.position = {{
                    static_cast<float>(position[0]),
                    static_cast<float>(position[1]),
                    static_cast<float>(position[2])
                }};

                const int index = _otherDataOption.value();
                // plus 3 because of the position
                layout.value.value = e.data[index];

                if (_staticFilterValue.has_value() && e.data[index] == _staticFilterValue)
                {
                    layout.value.value = _staticFilterReplacementValue;
                }

                glm::vec2 range = _otherDataRange;
                range.x = std::min(range.x, layout.value.value);
                range.y = std::max(range.y, layout.value.value);
                _otherDataRange = range;
                _otherDataRange.setMinValue(glm::vec2(range.x));
                _otherDataRange.setMaxValue(glm::vec2(range.y));

                layout.value.luminance = e.data[lumIdx];
                layout.value.absoluteMagnitude = e.data[absMagIdx];
                layout.value.apparentMagnitude = e.data[appMagIdx];

                result.insert(result.end(), layout.data.begin(), layout.data.end());
                break;
            }
        }
    }

    setBoundingSphere(maxRadius);
    return result;
}

} // namespace openspace
