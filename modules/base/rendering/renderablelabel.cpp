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

#include <modules/base/rendering/renderablelabel.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/string_cast.hpp>
#include <optional>

namespace {
    constexpr std::string_view MeterUnit = "m";
    constexpr std::string_view KilometerUnit = "Km";
    constexpr std::string_view MegameterUnit = "Mm";
    constexpr std::string_view GigameterUnit = "Gm";
    constexpr std::string_view AstronomicalUnitUnit = "au";
    constexpr std::string_view TerameterUnit = "Tm";
    constexpr std::string_view PetameterUnit = "Pm";
    constexpr std::string_view ParsecUnit = "pc";
    constexpr std::string_view KiloparsecUnit = "Kpc";
    constexpr std::string_view MegaparsecUnit = "Mpc";
    constexpr std::string_view GigaparsecUnit = "Gpc";
    constexpr std::string_view GigalightyearUnit = "Gly";

    enum BlendMode {
        Normal = 0,
        Additive
    };

    enum Orientation {
        ViewDirection = 0,
        PositionNormal
    };

    enum Unit {
        Meter = 0,
        Kilometer,
        Megameter,
        Gigameter,
        AstronomicalUnit,
        Terameter,
        Petameter,
        Parsec,
        KiloParsec,
        MegaParsec,
        GigaParsec,
        GigaLightyear
    };

    constexpr double PARSEC = 0.308567756E17;

    constexpr openspace::properties::Property::PropertyInfo BlendModeInfo = {
        "BlendMode",
        "Blending Mode",
        "This determines the blending mode that is applied to the renderable.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The label text color.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "The font size (in points) for the label.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "Scales the size of the label, exponentially. The value is used as the exponent "
        "in a 10^x computation to scale the label size.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo TextInfo = {
        "Text",
        "Text",
        "The text that will be displayed on screen.",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo MinMaxSizeInfo = {
        "MinMaxSize",
        "Min and Max Size",
        "The minimum and maximum size (in pixels) of the label.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to the label.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo OrientationOptionInfo = {
        "OrientationOption",
        "Orientation Option",
        "Label orientation rendering mode.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo EnableFadingEffectInfo = {
        "EnableFading",
        "Enable/Disable Fade-in Effect",
        "Enable/Disable the Fade-in effect.",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FadeWidthsInfo = {
        "FadeWidths",
        "Fade Widths",
        "The distances over which the fading takes place, given in the specified unit. "
        "The first value is the distance before the closest distance and the second "
        "the one after the furthest distance. For example, with the unit Parsec (pc), "
        "a value of {1, 2} will make the label being fully faded out 1 Parsec before "
        "the closest distance and 2 Parsec away from the furthest distance.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FadeDistancesInfo = {
        "FadeDistances",
        "Fade Distances",
        "The distance range in which the labels should be fully opaque, specified in "
        "the chosen unit. The distance from the position of the label to the camera.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FadeUnitOptionInfo = {
        "FadeUnit",
        "Fade Distance Unit",
        "Distance unit for fade-in/-out distance calculations. Defaults to \"au\".",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableLabel)]] Parameters {
        enum class [[codegen::map(BlendMode)]] BlendMode {
            Normal,
            Additive
        };

        // [[codegen::verbatim(BlendModeInfo.description)]]
        std::optional<BlendMode> blendMode;

        enum class [[codegen::map(Orientation)]] Orientation {
            ViewDirection [[codegen::key("Camera View Direction")]],
            PositionNormal [[codegen::key("Camera Position Normal")]]
        };

        // [[codegen::verbatim(OrientationOptionInfo.description)]]
        std::optional<Orientation> orientationOption;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(TextInfo.description)]]
        std::optional<std::string> text;

        // [[codegen::verbatim(FontSizeInfo.description)]]
        std::optional<float> fontSize;

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(MinMaxSizeInfo.description)]]
        std::optional<glm::ivec2> minMaxSize;

        // [[codegen::verbatim(EnableFadingEffectInfo.description)]]
        std::optional<bool> enableFading;

        // [[codegen::verbatim(TransformationMatrixInfo.description)]]
        std::optional<glm::dmat4x4> transformationMatrix;

        enum class [[codegen::map(Unit)]] Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Megameter [[codegen::key("Mm")]],
            Gigameter [[codegen::key("Gm")]],
            Terameter [[codegen::key("Tm")]],
            Petameter [[codegen::key("Pm")]],
            AstronomicalUnit [[codegen::key("au")]],
            Parsec [[codegen::key("pc")]],
            KiloParsec [[codegen::key("Kpc")]],
            MegaParsec [[codegen::key("Mpc")]],
            GigaParsec [[codegen::key("Gpc")]],
            GigaLightyear [[codegen::key("Gly")]]
        };

        // [[codegen::verbatim(FadeUnitOptionInfo.description)]]
        std::optional<Unit> fadeUnit;

        // [[codegen::verbatim(FadeDistancesInfo.description)]]
        std::optional<glm::vec2> fadeDistances;

        // [[codegen::verbatim(FadeWidthsInfo.description)]]
        std::optional<glm::vec2> fadeWidths;
    };
#include "renderablelabel_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableLabel::Documentation() {
    return codegen::doc<Parameters>("base_renderable_labels");
}

RenderableLabel::RenderableLabel(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary, { .automaticallyUpdateRenderBin = false })
    , _blendMode(BlendModeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _fontSize(FontSizeInfo, 50.f, 1.f, 100.f)
    , _size(SizeInfo, 8.f, 0.5f, 30.f)
    , _minMaxSize(MinMaxSizeInfo, glm::ivec2(8, 20), glm::ivec2(0), glm::ivec2(100))
    , _text(TextInfo, "")
    , _enableFadingEffect(EnableFadingEffectInfo, false)
    , _fadeWidths(FadeWidthsInfo, glm::vec2(1.f), glm::vec2(0.f), glm::vec2(100.f))
    , _fadeDistances(FadeDistancesInfo, glm::vec2(1.f), glm::vec2(0.f), glm::vec2(100.f))
    , _fadeUnitOption(
        FadeUnitOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _orientationOption(
        OrientationOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(Fadeable::_opacity);

    _blendMode.addOptions({
        { BlendMode::Normal, "Normal" },
        { BlendMode::Additive, "Additive" }
    });
    _blendMode.onChange([this]() {
        switch (_blendMode) {
            case BlendMode::Normal:
                setRenderBinFromOpacity();
                break;
            case BlendMode::Additive:
                setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
                break;
        }
    });

    if (p.blendMode.has_value()) {
        _blendMode = codegen::map<BlendMode>(*p.blendMode);
    }

    addProperty(_blendMode);

    _orientationOption.addOption(ViewDirection, "Camera View Direction");
    _orientationOption.addOption(PositionNormal, "Camera Position Normal");

    _orientationOption = PositionNormal;
    if (p.orientationOption.has_value()) {
        _orientationOption = codegen::map<Orientation>(*p.orientationOption);
    }
    addProperty(_orientationOption);

    _text = p.text.value_or(_text);
    addProperty(_text);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    _fontSize = p.fontSize.value_or(_fontSize);
    _fontSize.onChange([this]() {
        _font = global::fontManager->font(
            "Mono",
            _fontSize,
            ghoul::fontrendering::FontManager::Outline::Yes,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    });
    addProperty(_fontSize);

    // @TODO (emmbr, 2021-05-31): Temporarily set as read only, to avoid errors from font
    // rendering/loading
    _fontSize.setReadOnly(true);

    _size = p.size.value_or(_size);
    addProperty(_size);

    _minMaxSize = p.minMaxSize.value_or(_minMaxSize);
    _minMaxSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_minMaxSize);

    _transformationMatrix = p.transformationMatrix.value_or(_transformationMatrix);

    _enableFadingEffect = p.enableFading.value_or(_enableFadingEffect);
    addProperty(_enableFadingEffect);

    _fadeUnitOption.addOption(Meter, std::string(MeterUnit));
    _fadeUnitOption.addOption(Kilometer, std::string(KilometerUnit));
    _fadeUnitOption.addOption(Megameter, std::string(MegameterUnit));
    _fadeUnitOption.addOption(Gigameter, std::string(GigameterUnit));
    _fadeUnitOption.addOption(AstronomicalUnit, std::string(AstronomicalUnitUnit));
    _fadeUnitOption.addOption(Terameter, std::string(TerameterUnit));
    _fadeUnitOption.addOption(Petameter, std::string(PetameterUnit));
    _fadeUnitOption.addOption(Parsec, std::string(ParsecUnit));
    _fadeUnitOption.addOption(KiloParsec, std::string(KiloparsecUnit));
    _fadeUnitOption.addOption(MegaParsec, std::string(MegaparsecUnit));
    _fadeUnitOption.addOption(GigaParsec, std::string(GigaparsecUnit));
    _fadeUnitOption.addOption(GigaLightyear, std::string(GigalightyearUnit));

    if (p.fadeUnit.has_value()) {
        _fadeUnitOption = codegen::map<Unit>(*p.fadeUnit);
    }
    else {
        _fadeUnitOption = AstronomicalUnit;
    }
    addProperty(_fadeUnitOption);

    _fadeDistances = p.fadeDistances.value_or(_fadeDistances);
    _fadeDistances.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_fadeDistances);

    _fadeWidths = p.fadeWidths.value_or(_fadeWidths);
    addProperty(_fadeWidths);
}

bool RenderableLabel::isReady() const {
    return true;
}

void RenderableLabel::initialize() {
    ZoneScoped;

    setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
}

void RenderableLabel::initializeGL() {
    if (_font == nullptr) {
        _font = global::fontManager->font(
            "Mono",
            _fontSize,
            ghoul::fontrendering::FontManager::Outline::Yes,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }
}

void RenderableLabel::deinitializeGL() {}

void RenderableLabel::render(const RenderData& data, RendererTasks&) {
    glDepthMask(true);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE);

    float fadeInVariable = 1.f;

    if (_enableFadingEffect) {
        const float distanceNodeToCamera = static_cast<float>(
            glm::distance(data.camera.positionVec3(), data.modelTransform.translation)
        );
        fadeInVariable = computeFadeFactor(distanceNodeToCamera);
    }

    const glm::dmat4 modelMatrix = glm::dmat4(1.0);
    const glm::dmat4 modelViewProjectionTransform =
        calcModelViewProjectionTransform(data, modelMatrix);

    const glm::dvec3 cameraViewDirectionWorld = -data.camera.viewDirectionWorldSpace();
    const glm::dvec3 cameraUpDirectionWorld = data.camera.lookUpVectorWorldSpace();
    glm::dvec3 orthoRight = glm::normalize(
        glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
    );
    if (orthoRight == glm::dvec3(0.0)) {
        const glm::dvec3 otherVector = glm::dvec3(
            cameraUpDirectionWorld.y,
            cameraUpDirectionWorld.x,
            cameraUpDirectionWorld.z
        );
        orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
    }
    const glm::dvec3 orthoUp = glm::normalize(
        glm::cross(cameraViewDirectionWorld, orthoRight)
    );

    renderLabels(data, modelViewProjectionTransform, orthoRight, orthoUp, fadeInVariable);

    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetDepthState();
}


void RenderableLabel::setLabelText(const std::string & newText) {
    _text = newText;
}

void RenderableLabel::renderLabels(const RenderData& data,
                                    const glm::dmat4& modelViewProjectionMatrix,
                                    const glm::dvec3& orthoRight,
                                    const glm::dvec3& orthoUp, float fadeInVariable)
{
    glm::vec4 textColor = glm::vec4(glm::vec3(_color), 1.f);

    textColor.a *= fadeInVariable;
    textColor.a *= opacity();

    ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo;

    labelInfo.orthoRight = orthoRight;
    labelInfo.orthoUp = orthoUp;
    labelInfo.minSize = _minMaxSize.value().x;
    labelInfo.maxSize = _minMaxSize.value().y;
    labelInfo.cameraPos = data.camera.positionVec3();
    labelInfo.cameraLookUp = data.camera.lookUpVectorWorldSpace();
    labelInfo.renderType = _orientationOption;
    labelInfo.mvpMatrix = modelViewProjectionMatrix;
    labelInfo.scale = powf(10.f, _size);
    labelInfo.enableDepth = true;
    labelInfo.enableFalseDepth = false;

    // We don't use spice rotation and scale
    const glm::vec3 transformedPos = glm::vec3(
        _transformationMatrix * glm::dvec4(data.modelTransform.translation, 1.0)
    );

    ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
        *_font,
        transformedPos,
        _text.value(),
        textColor,
        labelInfo
    );
}

float RenderableLabel::computeFadeFactor(float distanceNodeToCamera) const {
    const float distanceUnit = unit(_fadeUnitOption);

    const float x = distanceNodeToCamera;
    const float startX = _fadeDistances.value().x * distanceUnit;
    const float endX = _fadeDistances.value().y * distanceUnit;

    // The distances over which the fading should happen
    const float fadingStartDistance = _fadeWidths.value().x * distanceUnit;
    const float fadingEndDistance = _fadeWidths.value().y * distanceUnit;

    if (x <= startX) {
        const float f1 = 1.f - (startX - x) / fadingStartDistance;
        return std::clamp(f1, 0.f, 1.f);
    }
    else if (x > startX && x < endX) {
        return 1.f; // not faded
    }
    else { // x >= endX
        const float f2 = 1.f - (x - endX) / fadingEndDistance;
        return std::clamp(f2, 0.f, 1.f);
    }
}

float RenderableLabel::unit(int unit) const {
    switch (static_cast<Unit>(unit)) {
        case Meter:           return 1.f;
        case Kilometer:        return 1e3f;
        case Megameter:        return  1e6f;
        case Gigameter:        return 1e9f;
        case AstronomicalUnit: return 149597870700.f;
        case Terameter:        return 1e12f;
        case Petameter:        return 1e15f;
        case Parsec:           return static_cast<float>(PARSEC);
        case KiloParsec:       return static_cast<float>(1e3 * PARSEC);
        case MegaParsec:       return static_cast<float>(1e6 * PARSEC);
        case GigaParsec:       return static_cast<float>(1e9 * PARSEC);
        case GigaLightyear:    return static_cast<float>(306391534.73091 * PARSEC);
        default:               throw ghoul::MissingCaseException();
    }
}

std::string_view RenderableLabel::toString(int unit) const {
    switch (static_cast<Unit>(unit)) {
        case Meter:            return MeterUnit;
        case Kilometer:        return KilometerUnit;
        case Megameter:        return MegameterUnit;
        case Gigameter:        return GigameterUnit;
        case AstronomicalUnit: return AstronomicalUnitUnit;
        case Terameter:        return TerameterUnit;
        case Petameter:        return PetameterUnit;
        case Parsec:           return ParsecUnit;
        case KiloParsec:       return KiloparsecUnit;
        case MegaParsec:       return MegaparsecUnit;
        case GigaParsec:       return GigaparsecUnit;
        case GigaLightyear:    return GigalightyearUnit;
        default:               throw ghoul::MissingCaseException();
    }
}

} // namespace openspace
