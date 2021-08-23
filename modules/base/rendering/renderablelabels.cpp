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

#include <modules/base/rendering/renderablelabels.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/glm.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/defer.h>
#include <ghoul/misc/profiling.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/string_cast.hpp>
#include <optional>

namespace {
    constexpr const char* MeterUnit = "m";
    constexpr const char* KilometerUnit = "Km";
    constexpr const char* MegameterUnit = "Mm";
    constexpr const char* GigameterUnit = "Gm";
    constexpr const char* AstronomicalUnit = "au";
    constexpr const char* TerameterUnit = "Tm";
    constexpr const char* PetameterUnit = "Pm";
    constexpr const char* ParsecUnit = "pc";
    constexpr const char* KiloparsecUnit = "Kpc";
    constexpr const char* MegaparsecUnit = "Mpc";
    constexpr const char* GigaparsecUnit = "Gpc";
    constexpr const char* GigalightyearUnit = "Gly";

    enum BlendMode {
        BlendModeNormal = 0,
        BlendModeAdditive
    };

    constexpr const int ViewDirection   = 0;
    constexpr const int NormalDirection = 1;

    constexpr double PARSEC = 0.308567756E17;

    constexpr openspace::properties::Property::PropertyInfo BlendModeInfo = {
        "BlendMode",
        "Blending Mode",
        "This determines the blending mode that is applied to the renderable."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The label text color."
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "The font size (in points) for the label."
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "This value affects the size scale of the label."
    };

    constexpr openspace::properties::Property::PropertyInfo TextInfo = {
        "Text",
        "Text",
        "The text that will be displayed on screen."
    };

    constexpr openspace::properties::Property::PropertyInfo MinMaxSizeInfo = {
        "MinMaxSize",
        "Min and Max Size",
        "The minimum and maximum size (in pixels) of the label."
    };

    constexpr openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to the label."
    };

    constexpr openspace::properties::Property::PropertyInfo OrientationOptionInfo = {
        "OrientationOption",
        "Orientation Option",
        "Label orientation rendering mode."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableFadingEffectInfo = {
        "EnableFading",
        "Enable/Disable Fade-in Effect",
        "Enable/Disable the Fade-in effect."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeWidthsInfo = {
        "FadeWidths",
        "Fade Widths",
        "The distances over which the fading takes place, given in the specified unit. "
        "The first value is the distance before the closest distance and the second "
        "the one after the furthest distance. For example, with the unit Parsec (pc), "
        "a value of {1, 2} will make the label being fully faded out 1 Parsec before "
        "the closest distance and 2 Parsec away from the furthest distance."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeDistancesInfo = {
        "FadeDistances",
        "Fade Distances",
        "The distance range in which the labels should be fully opaque, specified in "
        "the chosen unit. The distance from the position of the label to the camera."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeUnitOptionInfo = {
        "FadeUnit",
        "Fade Distance Unit",
        "Distance unit for fade-in/-out distance calculations. Defaults to \"au\"."
    };

    struct [[codegen::Dictionary(RenderableLabels)]] Parameters {
        enum class BlendMode {
            Normal,
            Additive
        };

        // [[codegen::verbatim(BlendModeInfo.description)]]
        std::optional<BlendMode> blendMode;

        enum class Orientation {
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

        enum class Unit {
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
#include "renderablelabels_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableLabels::Documentation() {
    return codegen::doc<Parameters>("base_renderable_labels");
}

RenderableLabels::RenderableLabels(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
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

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    _blendMode.addOptions({
        { BlendModeNormal, "Normal" },
        { BlendModeAdditive, "Additive"}
    });
    _blendMode.onChange([&]() {
        switch (_blendMode) {
            case BlendModeNormal:
                setRenderBinFromOpacity();
                break;
            case BlendModeAdditive:
                setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    });

    if (p.blendMode.has_value()) {
        switch (*p.blendMode) {
            case Parameters::BlendMode::Normal:
                _blendMode = BlendModeNormal;
                break;
            case Parameters::BlendMode::Additive:
                _blendMode = BlendModeAdditive;
                break;
        }
    }

    addProperty(_blendMode);

    _orientationOption.addOption(ViewDirection, "Camera View Direction");
    _orientationOption.addOption(NormalDirection, "Camera Position Normal");

    _orientationOption = NormalDirection;
    if (p.orientationOption.has_value()) {
        switch (*p.orientationOption) {
            case Parameters::Orientation::ViewDirection:
                _orientationOption = ViewDirection;
                break;
            case Parameters::Orientation::PositionNormal:
                _orientationOption = NormalDirection;
                break;
        }
    }
    addProperty(_orientationOption);

    _text = p.text.value_or(_text);
    addProperty(_text);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    _fontSize = p.fontSize.value_or(_fontSize);
    _fontSize.onChange([&]() {
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

    _fadeUnitOption.addOption(Meter, MeterUnit);
    _fadeUnitOption.addOption(Kilometer, KilometerUnit);
    _fadeUnitOption.addOption(Megameter, MegameterUnit);
    _fadeUnitOption.addOption(Gigameter, GigameterUnit);
    _fadeUnitOption.addOption(AU, AstronomicalUnit);
    _fadeUnitOption.addOption(Terameter, TerameterUnit);
    _fadeUnitOption.addOption(Petameter, PetameterUnit);
    _fadeUnitOption.addOption(Parsec, ParsecUnit);
    _fadeUnitOption.addOption(Kiloparsec, KiloparsecUnit);
    _fadeUnitOption.addOption(Megaparsec, MegaparsecUnit);
    _fadeUnitOption.addOption(Gigaparsec, GigaparsecUnit);
    _fadeUnitOption.addOption(GigalightYears, GigalightyearUnit);

    if (p.fadeUnit.has_value()) {
        switch (*p.fadeUnit) {
            case Parameters::Unit::Meter:
                _fadeUnitOption = Meter;
                break;
            case Parameters::Unit::Kilometer:
                _fadeUnitOption = Kilometer;
                break;
            case Parameters::Unit::Megameter:
                _fadeUnitOption = Megameter;
                break;
            case Parameters::Unit::Gigameter:
                _fadeUnitOption = Gigameter;
                break;
            case Parameters::Unit::Terameter:
                _fadeUnitOption = Terameter;
                break;
            case Parameters::Unit::Petameter:
                _fadeUnitOption = Petameter;
                break;
            case Parameters::Unit::AstronomicalUnit:
                _fadeUnitOption = AU;
                break;
            case Parameters::Unit::Parsec:
                _fadeUnitOption = Parsec;
                break;
            case Parameters::Unit::KiloParsec:
                _fadeUnitOption = Kiloparsec;
                break;
            case Parameters::Unit::MegaParsec:
                _fadeUnitOption = Megaparsec;
                break;
            case Parameters::Unit::GigaParsec:
                _fadeUnitOption = Gigaparsec;
                break;
            case Parameters::Unit::GigaLightyear:
                _fadeUnitOption = GigalightYears;
                break;
        }
    }
    else {
        _fadeUnitOption = AU;
    }
    addProperty(_fadeUnitOption);

    _fadeDistances = p.fadeDistances.value_or(_fadeDistances);
    _fadeDistances.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_fadeDistances);

    _fadeWidths = p.fadeWidths.value_or(_fadeWidths);
    addProperty(_fadeWidths);
}

bool RenderableLabels::isReady() const {
    return true;
}

void RenderableLabels::initialize() {
    ZoneScoped

    setRenderBin(Renderable::RenderBin::PreDeferredTransparent);
}

void RenderableLabels::initializeGL() {
    if (_font == nullptr) {
        _font = global::fontManager->font(
            "Mono",
            _fontSize,
            ghoul::fontrendering::FontManager::Outline::Yes,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }
}

void RenderableLabels::deinitializeGL() {}

void RenderableLabels::render(const RenderData& data, RendererTasks&) {

    //bool additiveBlending = (_blendMode == BlendModeAdditive);
    //if (additiveBlending) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    //}

    float fadeInVariable = 1.f;

    if (_enableFadingEffect) {
        float distanceNodeToCamera = static_cast<float>(
            glm::distance(data.camera.positionVec3(), data.modelTransform.translation)
        );
        fadeInVariable = computeFadeFactor(distanceNodeToCamera);
    }

    glm::dmat4 modelMatrix(1.0);
    glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    glm::dmat4 projectionMatrix = glm::dmat4(data.camera.projectionMatrix());

    glm::dmat4 modelViewProjectionMatrix = projectionMatrix * modelViewMatrix;

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

    renderLabels(data, modelViewProjectionMatrix, orthoRight, orthoUp, fadeInVariable);

    //if (additiveBlending) {
        glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
        glDepthMask(true);
    //}
}


void RenderableLabels::setLabelText(const std::string & newText) {
    _text = newText;
}

void RenderableLabels::renderLabels(const RenderData& data,
                                    const glm::dmat4& modelViewProjectionMatrix,
                                    const glm::dvec3& orthoRight,
                                    const glm::dvec3& orthoUp, float fadeInVariable)
{
    glm::vec4 textColor = glm::vec4(glm::vec3(_color), 1.f);

    textColor.a *= fadeInVariable;
    textColor.a *= _opacity;

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
    glm::vec3 transformedPos(
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

float RenderableLabels::computeFadeFactor(float distanceNodeToCamera) const {
    float distanceUnit = unit(_fadeUnitOption);

    float x = distanceNodeToCamera;
    float startX = _fadeDistances.value().x * distanceUnit;
    float endX = _fadeDistances.value().y * distanceUnit;

    // The distances over which the fading should happen
    float fadingStartDistance = _fadeWidths.value().x * distanceUnit;
    float fadingEndDistance = _fadeWidths.value().y * distanceUnit;

    if (x <= startX) {
        float f1 = 1.f - (startX - x) / fadingStartDistance;
        return std::clamp(f1, 0.f, 1.f);
    }
    else if (x > startX && x < endX) {
        return 1.f; // not faded
    }
    else { // x >= endX
        float f2 = 1.f - (x - endX) / fadingEndDistance;
        return std::clamp(f2, 0.f, 1.f);
    }
}

float RenderableLabels::unit(int unit) const {
    switch (static_cast<Unit>(unit)) {
        case Meter: return 1.f;
        case Kilometer: return 1e3f;
        case Megameter: return  1e6f;
        case Gigameter: return 1e9f;
        case AU: return 149597870700.f;
        case Terameter: return 1e12f;
        case Petameter: return 1e15f;
        case Parsec: return static_cast<float>(PARSEC);
        case Kiloparsec: return static_cast<float>(1e3 * PARSEC);
        case Megaparsec: return static_cast<float>(1e6 * PARSEC);
        case Gigaparsec: return static_cast<float>(1e9 * PARSEC);
        case GigalightYears: return static_cast<float>(306391534.73091 * PARSEC);
        default: throw std::logic_error("Missing case label");
    }
}

std::string RenderableLabels::toString(int unit) const {
    switch (static_cast<Unit>(unit)) {
        case Meter: return MeterUnit;
        case Kilometer: return KilometerUnit;
        case Megameter: return  MegameterUnit;
        case Gigameter: return GigameterUnit;
        case AU: return AstronomicalUnit;
        case Terameter: return TerameterUnit;
        case Petameter: return PetameterUnit;
        case Parsec: return ParsecUnit;
        case Kiloparsec: return KiloparsecUnit;
        case Megaparsec: return MegaparsecUnit;
        case Gigaparsec: return GigaparsecUnit;
        case GigalightYears: return GigalightyearUnit;
        default: throw std::logic_error("Missing case label");
    }
}

} // namespace openspace
