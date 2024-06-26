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

    constexpr std::array<const char*, 24> UniformNames = {
        "modelMatrix", "cameraViewProjectionMatrix", "cameraUp", "eyePosition",
        "colorOption", "magnitudeExponent", "sizeComposition", "lumCent", "radiusCent",
        "colorTexture", "opacity", "otherDataTexture", "otherDataRange",
        "filterOutOfRange", "fixedColor", "glareTexture", "glareMultiplier", "glareGamma",
        "glareScale", "hasCore", "coreTexture", "coreMultiplier", "coreGamma", "coreScale"
    };

    enum SizeComposition {
        DistanceModulus = 0,
        AppBrightness,
        LumSize,
        AbsMagnitude,
        AppMagnitude
    };

    constexpr double PARSEC = 0.308567756E17;

    struct ColorVBOLayout {
        std::array<float, 3> position;
        float value;
        float luminance;
        float absoluteMagnitude;
    };

    struct VelocityVBOLayout {
        std::array<float, 3> position;
        float value;
        float luminance;
        float absoluteMagnitude;

        float vx; // v_x
        float vy; // v_y
        float vz; // v_z
    };

    struct SpeedVBOLayout {
        std::array<float, 3> position;
        float value;
        float luminance;
        float absoluteMagnitude;

        float speed;
    };

    struct OtherDataLayout {
        std::array<float, 3> position;
        float value;
        float luminance;
        float absoluteMagnitude;
    };

    constexpr openspace::properties::Property::PropertyInfo SpeckFileInfo = {
        "SpeckFile",
        "SPECK File",
        "The path to the SPECK file containing the data for rendering these stars.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "ColorBV Texture",
        "The path to the texture that is used to convert from the B-V value of the star "
        "to its color. The texture is used as a one dimensional lookup function.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingBvInfo = {
        "MappingBV",
        "Mapping (bv-color)",
        "The name of the variable in the SPECK file that is used as the b-v color "
        "variable.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingLuminanceInfo = {
        "MappingLuminance",
        "Mapping (luminance)",
        "The name of the variable in the SPECK file that is used as the luminance "
        "variable.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingAbsMagnitudeInfo = {
        "MappingAbsMagnitude",
        "Mapping (absolute magnitude)",
        "The name of the variable in the SPECK file that is used as the absolute "
        "magnitude variable.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingVxInfo = {
        "MappingVx",
        "Mapping (vx)",
        "The name of the variable in the SPECK file that is used as the star velocity "
        "along the x-axis.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingVyInfo = {
        "MappingVy",
        "Mapping (vy)",
        "The name of the variable in the SPECK file that is used as the star velocity "
        "along the y-axis.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingVzInfo = {
        "MappingVz",
        "Mapping (vz)",
        "The name of the variable in the SPECK file that is used as the star velocity "
        "along the z-axis.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MappingSpeedInfo = {
        "MappingSpeed",
        "Mapping (speed)",
        "The name of the variable in the SPECK file that is used as the speed.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorOptionInfo = {
        "ColorOption",
        "Color Option",
        "This value determines which quantity is used for determining the color of the "
        "stars.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataOptionInfo = {
        "OtherData",
        "Other Data Column",
        "The index of the SPECK file data column that is used as the color input.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataValueRangeInfo = {
        "OtherDataValueRange",
        "Range of the other data values",
        "This value is the min/max value range that is used to normalize the other data "
        "values so they can be used by the specified color map.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FixedColorInfo = {
        "FixedColorValue",
        "Color used for fixed star colors",
        "The color that should be used if the 'Fixed Color' value is used.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OtherDataColorMapInfo = {
        "OtherDataColorMap",
        "Other Data Color Map",
        "The color map that is used if the 'Other Data' rendering method is selected.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FilterOutOfRangeInfo = {
        "FilterOutOfRange",
        "Filter Out of Range",
        "Determines whether other data values outside the value range should be visible "
        "or filtered away.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo CoreOwnerInfo = {
        "Core",
        "Core",
        "Settings for the central core portion of the star."
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo GlareOwnerInfo = {
        "Glare",
        "Glare",
        "Settings for the glare portion of the star."
    };

    constexpr openspace::properties::Property::PropertyInfo TextureInfo = {
        "Texture",
        "Texture Path",
        "The path to the texture that should be used.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo MultiplierInfo = {
        "Multiplier",
        "Multiplier",
        "An individual multiplication factor for this texture component. Using the "
        "multiplier and gamma values for both components, it is possible to fine tune "
        "the look of the stars or disable the contributions altogether by setting it to "
        "0.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo GammaInfo = {
        "Gamma",
        "Gamma",
        "An individual gamma exponent for this texture component. Using the multiplier "
        "and gamma values for both components, it is possible to finetune the look of "
        "the stars."
    };

    constexpr openspace::properties::Property::PropertyInfo MagnitudeExponentInfo = {
        "MagnitudeExponent",
        "Magnitude Exponent",
        "Adjust star magnitude by 10^MagnitudeExponent. Stars closer than this distance "
        "are given full opacity. Farther away, stars dim proportionally to the "
        "logarithm of their distance.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ScaleInfo = {
        "Scale",
        "Scale",
        "A uniform scale factor that determines how much of the total size of the star "
        "this component is using. If it is 0, it will be hidden. If it is 1, it will "
        "take the entire size.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    const openspace::properties::PropertyOwner::PropertyOwnerInfo SizeCompositionInfo = {
        "SizeComposition",
        "Size Composition",
        ""
    };

    constexpr openspace::properties::Property::PropertyInfo SizeCompositionMethodInfo = {
        "Method",
        "Method",
        "Method to determine the size for the stars.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo LumPercentInfo = {
        "LumPercent",
        "Luminosity Contribution",
        "Luminosity Contribution.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo RadiusPercentInfo = {
        "RadiusPercent",
        "Radius Contribution",
        "Radius Contribution.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FadeInDistancesInfo = {
        "FadeInDistances",
        "Fade-In Start and End Distances",
        "These values determine the initial and final distances from the center of "
        "our galaxy from which the astronomical object will start and end "
        "fading-in.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo EnableFadeInInfo = {
        "EnableFadeIn",
        "Enable Fade-in effect",
        "Enables/Disables the Fade-in effect.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableStars)]] Parameters {
        // [[codegen::verbatim(SpeckFileInfo.description)]]
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
        std::optional<std::filesystem::path> otherDataColorMap;

        // [[codegen::verbatim(FilterOutOfRangeInfo.description)]]
        std::optional<bool> filterOutOfRange;

        // Specifies a value that is always filtered out of the value ranges on load.
        // This can be used to trim the dataset's automatic value range.
        std::optional<float> staticFilter;

        // A value that is used to replace statically filtered values. Setting this value
        // only makes sense if `StaticFilter` is set as well.
        std::optional<float> staticFilterReplacement;

        struct Texture {
            // [[codegen::verbatim(TextureInfo.description)]]
            std::filesystem::path texture;

            // [[codegen::verbatim(MultiplierInfo.description)]]
            std::optional<float> multiplier;

            // [[codegen::verbatim(GammaInfo.description)]]
            std::optional<float> gamma;

            // [[codegen::verbatim(ScaleInfo.description)]]
            std::optional<float> scale;
        };

        // [[codegen::verbatim(GlareOwnerInfo.description)]]
        Texture glare;

        // [[codegen::verbatim(CoreOwnerInfo.description)]]
        std::optional<Texture> core;

        // [[codegen::verbatim(MagnitudeExponentInfo.description)]]
        std::optional<float> magnitudeExponent;

        enum class [[codegen::map(SizeComposition)]] SizeComposition {
            DistanceModulus [[codegen::key("Distance Modulus")]],
            AppBrightness [[codegen::key("App Brightness")]],
            LumSize [[codegen::key("Lum and Size")]],
            AbsMagnitude [[codegen::key("Abs Magnitude")]],
            AppMagnitude [[codegen::key("App Magnitude")]]
        };

        // [[codegen::verbatim(SizeCompositionMethodInfo.description)]]
        std::optional<SizeComposition> sizeComposition;

        struct DataMapping {
            // [[codegen::verbatim(MappingBvInfo.description)]]
            std::optional<std::string> bv;
            // [[codegen::verbatim(MappingLuminanceInfo.description)]]
            std::optional<std::string> luminance;
            // [[codegen::verbatim(MappingAbsMagnitudeInfo.description)]]
            std::optional<std::string> absoluteMagnitude;
            // [[codegen::verbatim(MappingVxInfo.description)]]
            std::optional<std::string> vx;
            // [[codegen::verbatim(MappingVyInfo.description)]]
            std::optional<std::string> vy;
            // [[codegen::verbatim(MappingVzInfo.description)]]
            std::optional<std::string> vz;
            // [[codegen::verbatim(MappingSpeedInfo.description)]]
            std::optional<std::string> speed;
        };
        // The mappings between data values and the variable names specified in the SPECK
        // file.
        DataMapping dataMapping;

        // [[codegen::verbatim(FadeInDistancesInfo.description)]]
        std::optional<glm::dvec2> fadeInDistances;

        // [[codegen::verbatim(EnableFadeInInfo.description)]]
        std::optional<bool> enableFadeIn;
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
    , _dataMapping{
        properties::PropertyOwner({ "DataMapping", "Data Mapping" }),
        properties::StringProperty(MappingBvInfo),
        properties::StringProperty(MappingLuminanceInfo),
        properties::StringProperty(MappingAbsMagnitudeInfo),
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
    , _core{
        properties::PropertyOwner(CoreOwnerInfo),
        properties::StringProperty(TextureInfo),
        properties::FloatProperty(MultiplierInfo, 1.f, 0.f, 20.f),
        properties::FloatProperty(GammaInfo, 1.f, 0.f, 5.f),
        properties::FloatProperty(ScaleInfo, 1.f, 0.f, 1.f)
    }
    , _glare{
        properties::PropertyOwner(GlareOwnerInfo),
        properties::StringProperty(TextureInfo),
        properties::FloatProperty(MultiplierInfo, 1.f, 0.f, 20.f),
        properties::FloatProperty(GammaInfo, 1.f, 0.f, 5.f),
        properties::FloatProperty(ScaleInfo, 1.f, 0.f, 1.f)
    }
    , _parameters{
        properties::PropertyOwner(SizeCompositionInfo),
        properties::OptionProperty(
            SizeCompositionMethodInfo,
            properties::OptionProperty::DisplayType::Dropdown
        ),
        properties::FloatProperty(LumPercentInfo, 0.5f, 0.f, 3.f),
        properties::FloatProperty(RadiusPercentInfo, 0.5f, 0.f, 3.f)
    }
    , _magnitudeExponent(MagnitudeExponentInfo, 6.2f, 5.f, 8.f)
    , _fadeInDistances(
        FadeInDistancesInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(100.f)
    )
    , _enableFadeInDistance(EnableFadeInInfo, false)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);


    _speckFile = p.speckFile.string();
    _speckFile.onChange([this]() { _speckFileIsDirty = true; });
    addProperty(_speckFile);


    _colorTextureFile = std::make_unique<ghoul::filesystem::File>(p.colorMap);
    _colorTextureFile->setCallback([this]() { _colorTextureIsDirty = true; });
    _colorTexturePath = p.colorMap.string();
    _colorTexturePath.onChange([&] {
        if (std::filesystem::exists(_colorTexturePath.value())) {
            _colorTextureIsDirty = true;
        }
        else {
            LWARNING(std::format("File not found: {}", _colorTexturePath.value()));
        }
    });
    addProperty(_colorTexturePath);


    _dataMapping.bvColor = p.dataMapping.bv.value_or(_dataMapping.bvColor);
    _dataMapping.bvColor.onChange([this]() { _dataIsDirty = true; });
    _dataMapping.container.addProperty(_dataMapping.bvColor);

    _dataMapping.luminance = p.dataMapping.luminance.value_or(_dataMapping.luminance);
    _dataMapping.luminance.onChange([this]() { _dataIsDirty = true; });
    _dataMapping.container.addProperty(_dataMapping.luminance);

    _dataMapping.absoluteMagnitude =
        p.dataMapping.absoluteMagnitude.value_or(_dataMapping.absoluteMagnitude);
    _dataMapping.absoluteMagnitude.onChange([this]() { _dataIsDirty = true; });
    _dataMapping.container.addProperty(_dataMapping.absoluteMagnitude);

    _dataMapping.vx = p.dataMapping.vx.value_or(_dataMapping.vx);
    _dataMapping.vx.onChange([this]() { _dataIsDirty = true; });
    _dataMapping.container.addProperty(_dataMapping.vx);

    _dataMapping.vy = p.dataMapping.vy.value_or(_dataMapping.vy);
    _dataMapping.vy.onChange([this]() { _dataIsDirty = true; });
    _dataMapping.container.addProperty(_dataMapping.vy);

    _dataMapping.vz = p.dataMapping.vz.value_or(_dataMapping.vz);
    _dataMapping.vz.onChange([this]() { _dataIsDirty = true; });
    _dataMapping.container.addProperty(_dataMapping.vz);

    _dataMapping.speed = p.dataMapping.speed.value_or(_dataMapping.speed);
    _dataMapping.speed.onChange([this]() { _dataIsDirty = true; });
    _dataMapping.container.addProperty(_dataMapping.speed);
    addPropertySubOwner(_dataMapping.container);


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
    _colorOption.onChange([this]() { _dataIsDirty = true; });
    addProperty(_colorOption);



    _otherDataOption.onChange([this]() { _dataIsDirty = true; });
    addProperty(_otherDataOption);


    _otherDataColorMapPath.onChange([this]() { _otherDataColorMapIsDirty = true; });
    if (p.otherDataColorMap.has_value()) {
        _otherDataColorMapPath = p.otherDataColorMap->string();
    }
    addProperty(_otherDataColorMapPath);


    _otherDataRange.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_otherDataRange);


    _fixedColor.setViewOption(properties::Property::ViewOptions::Color, true);
    addProperty(_fixedColor);


    addProperty(_filterOutOfRange);


    auto markTextureAsDirty = [this]() {_pointSpreadFunctionTextureIsDirty = true; };

    if (p.core.has_value()) {
        _core.texturePath = absPath(p.core->texture).string();
        _core.file = std::make_unique<ghoul::filesystem::File>(_core.texturePath.value());
        _core.file->setCallback(markTextureAsDirty);
        _core.multiplier = p.core->multiplier.value_or(_core.multiplier);
        _core.gamma = p.core->gamma.value_or(_core.gamma);
        _core.scale = p.core->scale.value_or(_core.scale);
    }
    _core.texturePath.onChange(markTextureAsDirty);
    _core.container.addProperty(_core.texturePath);
    _core.container.addProperty(_core.multiplier);
    _core.container.addProperty(_core.gamma);
    _core.container.addProperty(_core.scale);
    addPropertySubOwner(_core.container);

    _glare.texturePath = absPath(p.glare.texture).string();
    _glare.texturePath.onChange(markTextureAsDirty);
    _glare.file = std::make_unique<ghoul::filesystem::File>(_glare.texturePath.value());
    _glare.file->setCallback(markTextureAsDirty);
    _glare.container.addProperty(_glare.texturePath);
    _glare.multiplier = p.glare.multiplier.value_or(_glare.multiplier);
    _glare.container.addProperty(_glare.multiplier);
    _glare.gamma = p.glare.gamma.value_or(_glare.gamma);
    _glare.container.addProperty(_glare.gamma);
    _glare.scale = p.glare.scale.value_or(_glare.scale);
    _glare.container.addProperty(_glare.scale);
    addPropertySubOwner(_glare.container);

    _magnitudeExponent = p.magnitudeExponent.value_or(_magnitudeExponent);
    addProperty(_magnitudeExponent);


    _parameters.method.addOptions({
        { DistanceModulus, "Distance Modulus" },
        { AppBrightness, "Apparent Brightness" },
        { LumSize, "Luminosity and Size" },
        { AbsMagnitude, "Absolute Magnitude" },
        { AppMagnitude, "Apparent Magnitude" }
    });
    _parameters.method =
        p.sizeComposition.has_value() ?
        static_cast<int>(codegen::map<SizeComposition>(*p.sizeComposition)) :
        SizeComposition::DistanceModulus;
    _parameters.container.addProperty(_parameters.method);
    _parameters.container.addProperty(_parameters.lumCent);
    _parameters.container.addProperty(_parameters.radiusCent);
    addPropertySubOwner(_parameters.container);


    if (p.fadeInDistances.has_value()) {
        _fadeInDistances = *p.fadeInDistances;
        _enableFadeInDistance = true;
        _fadeInDistances.setViewOption(properties::Property::ViewOptions::MinMaxRange);
        addProperty(_fadeInDistances);
        addProperty(_enableFadeInDistance);
    }

    _queuedOtherData = p.otherData.value_or(_queuedOtherData);
    _staticFilterValue = p.staticFilter;
    _staticFilterReplacementValue =
        p.staticFilterReplacement.value_or(_staticFilterReplacementValue);
}

bool RenderableStars::isReady() const {
    return _program && _glare.texture;
}

void RenderableStars::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "Star",
        absPath("${MODULE_SPACE}/shaders/star_vs.glsl"),
        absPath("${MODULE_SPACE}/shaders/star_fs.glsl"),
        absPath("${MODULE_SPACE}/shaders/star_ge.glsl")
    );

    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBindVertexArray(0);

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

    loadPSFTexture();
}

void RenderableStars::deinitializeGL() {
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;

    glDeleteBuffers(1, &_vbo);
    _vbo = 0;

    _colorTexture = nullptr;

    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

void RenderableStars::loadPSFTexture() {
    auto markPsfTextureAsDirty = [this]() { _pointSpreadFunctionTextureIsDirty = true; };
    auto loadTexture = [markPsfTextureAsDirty](TextureComponent& component) {
        using Texture = ghoul::opengl::Texture;

        component.texture = nullptr;
        const std::string path = component.texturePath;
        if (path.empty() || !std::filesystem::exists(path)) {
            return;
        }

        component.texture = ghoul::io::TextureReader::ref().loadTexture(absPath(path), 2);

        if (!component.texture) {
            return;
        }

        LDEBUG(std::format("Loaded texture from '{}'", absPath(component.texturePath)));
        component.texture->uploadTexture();
        component.texture->setWrapping(Texture::WrappingMode::ClampToBorder);

        constexpr std::array<float, 4> border = { 0.f, 0.f, 0.f, 0.f };
        glTexParameterfv(GL_TEXTURE_2D, GL_TEXTURE_BORDER_COLOR, border.data());
        component.texture->setFilter(Texture::FilterMode::AnisotropicMipMap);

        component.file = std::make_unique<ghoul::filesystem::File>(path);
        component.file->setCallback(markPsfTextureAsDirty);
    };

    loadTexture(_core);
    loadTexture(_glare);

    _pointSpreadFunctionTextureIsDirty = false;
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
    _program->setUniform(_uniformCache.modelMatrix, modelMatrix);

    const glm::dmat4 viewProjectionMatrix =
        glm::dmat4(data.camera.projectionMatrix()) * data.camera.combinedViewMatrix();
    _program->setUniform(_uniformCache.cameraViewProjectionMatrix, viewProjectionMatrix);

    _program->setUniform(_uniformCache.colorOption, _colorOption);
    _program->setUniform(_uniformCache.magnitudeExponent, _magnitudeExponent);

    _program->setUniform(_uniformCache.sizeComposition, _parameters.method.value());
    _program->setUniform(_uniformCache.lumCent, _parameters.lumCent);
    _program->setUniform(_uniformCache.radiusCent, _parameters.radiusCent);

    if (_colorOption == ColorOption::FixedColor) {
        if (_uniformCache.fixedColor == -1) {
            _uniformCache.fixedColor = _program->uniformLocation("fixedColor");
        }
        _program->setUniform(_uniformCache.fixedColor, _fixedColor);
    }

    if (_enableFadeInDistance) {
        const double distCam = glm::length(data.camera.positionVec3());
        const glm::vec2 fadeRange = _fadeInDistances;
        const double a = 1.f / ((fadeRange.y - fadeRange.x) * PARSEC);
        const double b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        const float funcValue = static_cast<float>(a * distCam + b);
        const float fadeInValue = std::min(funcValue, 1.f);

        _program->setUniform(_uniformCache.opacity, opacity() * fadeInValue);
    }
    else {
        _program->setUniform(_uniformCache.opacity, opacity());
    }

    ghoul::opengl::TextureUnit glareUnit;
    glareUnit.activate();
    _glare.texture->bind();
    _program->setUniform(_uniformCache.glareTexture, glareUnit);
    _program->setUniform(_uniformCache.glareMultiplier, _glare.multiplier);
    _program->setUniform(_uniformCache.glareGamma, _glare.gamma);
    _program->setUniform(_uniformCache.glareScale, _glare.scale);

    ghoul::opengl::TextureUnit coreUnit;
    if (_core.texture) {
        coreUnit.activate();
        _core.texture->bind();
        _program->setUniform(_uniformCache.coreTexture, coreUnit);
        _program->setUniform(_uniformCache.coreMultiplier, _core.multiplier);
        _program->setUniform(_uniformCache.coreGamma, _core.gamma);
        _program->setUniform(_uniformCache.coreScale, _core.scale);
    }
    _program->setUniform(_uniformCache.hasCore, _core.texture != nullptr);

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
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_dataset.entries.size()));
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

        glBindVertexArray(_vao);
        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glBufferData(
            GL_ARRAY_BUFFER,
            slice.size() * sizeof(GLfloat),
            slice.data(),
            GL_STATIC_DRAW
        );

        const GLint positionAttrib = _program->attributeLocation("in_position");
        // in_bvLumAbsMag = bv color, luminosity, abs magnitude
        const GLint bvLumAbsMagAttrib = _program->attributeLocation("in_bvLumAbsMag");

        const size_t nStars = _dataset.entries.size();
        const size_t nValues = slice.size() / nStars;

        const GLsizei stride = static_cast<GLsizei>(sizeof(GLfloat) * nValues);

        glEnableVertexAttribArray(positionAttrib);
        glVertexAttribPointer(
            positionAttrib,
            3,
            GL_FLOAT,
            GL_FALSE,
            stride,
            nullptr
        );

        glEnableVertexAttribArray(bvLumAbsMagAttrib);
        const int colorOption = _colorOption;
        switch (colorOption) {
            case ColorOption::Color:
            case ColorOption::FixedColor:
                glVertexAttribPointer(
                    bvLumAbsMagAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(ColorVBOLayout, value))
                );

                break;
            case ColorOption::Velocity:
            {
                glVertexAttribPointer(
                    bvLumAbsMagAttrib,
                    3,
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
                    bvLumAbsMagAttrib,
                    3,
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
                glVertexAttribPointer(
                    bvLumAbsMagAttrib,
                    3,
                    GL_FLOAT,
                    GL_FALSE,
                    stride,
                    reinterpret_cast<void*>(offsetof(OtherDataLayout, value))
                );
                break;
        }

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
                absPath(_colorTexturePath),
                1
            );
            if (_colorTexture) {
                LDEBUG(std::format("Loaded texture '{}'", _colorTexturePath.value()));
                _colorTexture->uploadTexture();
            }

            _colorTextureFile = std::make_unique<ghoul::filesystem::File>(
                _colorTexturePath.value()
            );
            _colorTextureFile->setCallback([this]() { _colorTextureIsDirty = true; });
        }
        _colorTextureIsDirty = false;
    }

    if (_otherDataColorMapIsDirty) {
        LDEBUG("Reloading Color Texture");
        _otherDataColorMapTexture = nullptr;
        if (!_otherDataColorMapPath.value().empty()) {
            _otherDataColorMapTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_otherDataColorMapPath),
                1
            );
            if (_otherDataColorMapTexture) {
                LDEBUG(std::format(
                    "Loaded texture '{}'", _otherDataColorMapPath.value()
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
    const int bvIdx = std::max(_dataset.index(_dataMapping.bvColor), 0);
    const int lumIdx = std::max(_dataset.index(_dataMapping.luminance), 0);
    const int absMagIdx = std::max(_dataset.index(_dataMapping.absoluteMagnitude), 0);
    const int vxIdx = std::max(_dataset.index(_dataMapping.vx), 0);
    const int vyIdx = std::max(_dataset.index(_dataMapping.vy), 0);
    const int vzIdx = std::max(_dataset.index(_dataMapping.vz), 0);
    const int speedIdx = std::max(_dataset.index(_dataMapping.speed), 0);

    _otherDataRange = glm::vec2(
        std::numeric_limits<float>::max(),
        -std::numeric_limits<float>::max()
    );

    double maxRadius = 0.0;

    std::vector<float> result;
    // 6 for the default Color option of 3 positions + bv + lum + abs
    result.reserve(_dataset.entries.size() * 6);
    for (const dataloader::Dataset::Entry& e : _dataset.entries) {
        glm::dvec3 position = glm::dvec3(e.position) * distanceconstants::Parsec;
        glm::vec3 pos = position;
        maxRadius = std::max(maxRadius, glm::length(position));

        switch (option) {
            case ColorOption::Color:
            case ColorOption::FixedColor:
            {
                union {
                    ColorVBOLayout value;
                    std::array<float, sizeof(ColorVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = { pos.x, pos.y, pos.z };
                layout.value.value = e.data[bvIdx];
                layout.value.luminance = e.data[lumIdx];
                layout.value.absoluteMagnitude = e.data[absMagIdx];

                result.insert(result.end(), layout.data.begin(), layout.data.end());
                break;
            }
            case ColorOption::Velocity:
            {
                union {
                    VelocityVBOLayout value;
                    std::array<float, sizeof(VelocityVBOLayout) / sizeof(float)> data;
                } layout;

                layout.value.position = { pos.x, pos.y, pos.z };
                layout.value.value = e.data[bvIdx];
                layout.value.luminance = e.data[lumIdx];
                layout.value.absoluteMagnitude = e.data[absMagIdx];

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

                layout.value.position = { pos.x, pos.y, pos.z };
                layout.value.value = e.data[bvIdx];
                layout.value.luminance = e.data[lumIdx];
                layout.value.absoluteMagnitude = e.data[absMagIdx];
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

                layout.value.position = { pos.x, pos.y, pos.z };

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

                result.insert(result.end(), layout.data.begin(), layout.data.end());
                break;
            }
        }
    }

    setBoundingSphere(maxRadius);
    return result;
}

} // namespace openspace
