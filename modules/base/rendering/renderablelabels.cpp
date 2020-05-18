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
#include <ghoul/misc/templatefactory.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <glm/gtx/string_cast.hpp>

namespace {
    constexpr const char* _loggerCat = "base::RenderableLabels";

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
        "This determines the blending mode that is applied to this plane."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelColorInfo = {
        "LabelColor",
        "Label Color",
        "The label color for the astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "The font size for the astronomical object labels."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelSizeInfo = {
        "LabelSize",
        "Label Size",
        "The label size for the astronomical object labels."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelTextInfo = {
        "LabelText",
        "Label Text",
        "The text that will be displayed on screen."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMinSizeInfo = {
        "LabelMinSize",
        "Label Min Size",
        "The minimal size (in pixels) of the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMaxSizeInfo = {
        "LabelMaxSize",
        "Label Max Size",
        "The maximum size (in pixels) of the labels for the astronomical "
        "objects being rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to each astronomical object."
    };

    constexpr openspace::properties::Property::PropertyInfo LabelOrientationOptionInfo = {
        "LabelOrientationOption",
        "Label Orientation Option",
        "Label orientation rendering mode."
    };

    constexpr openspace::properties::Property::PropertyInfo EnableFadingEffectInfo = {
        "EnableFading",
        "Enable/Disable Fade-in effect",
        "Enable/Disable the Fade-in effect."
    };

    constexpr openspace::properties::Property::PropertyInfo PixelSizeControlInfo = {
        "EnablePixelSizeControl",
        "Enable pixel size control.",
        "Enable pixel size control for rectangular projections."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeStartUnitOptionInfo = {
        "FadeStartUnit",
        "Fade-In/-Out Start Unit.",
        "Unit for fade-in/-out starting position calculation."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeEndUnitOptionInfo = {
        "FadeEndUnit",
        "Fade-In/-Out End Unit.",
        "Unit for fade-in/-out ending position calculation."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeStartDistInfo = {
        "FadeStartDistance",
        "Fade-In/-Out starting distance.",
        "Fade-In/-Out starting distance."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeEndDistInfo = {
        "FadeEndDistance",
        "Fade-In/-Out ending distance.",
        "Fade-In/-Out ending distance."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeStartSpeedInfo = {
        "FadeStartSpeed",
        "Fade-In/-Out starting speed.",
        "Fade-In/-Out starting speed."
    };

    constexpr openspace::properties::Property::PropertyInfo FadeEndSpeedInfo = {
        "FadeEndSpeed",
        "Fade-In/-Out ending speed.",
        "Fade-In/-Out ending speed."
    };
} // namespace

namespace openspace {

documentation::Documentation RenderableLabels::Documentation() {
    using namespace documentation;
    return {
        "Renderable Labels",
        "base_renderable_labels",
        {
            {
                BlendModeInfo.identifier,
                new StringInListVerifier({ "Normal", "Additive" }),
                Optional::Yes,
                BlendModeInfo.description, // + " The default value is 'Normal'.",
            },
            {
                LabelOrientationOptionInfo.identifier,
                new StringInListVerifier(
                    { "Camera View Direction", "Camera Position Normal" }
                ),
                Optional::Yes,
                LabelOrientationOptionInfo.description,
            },
            {
                LabelColorInfo.identifier,
                new DoubleVector4Verifier,
                Optional::Yes,
                LabelColorInfo.description,
            },
            {
                LabelColorInfo.identifier,
                new DoubleVector4Verifier,
                Optional::Yes,
                LabelColorInfo.description,
            },
            {
                LabelTextInfo.identifier,
                new StringVerifier,
                Optional::No,
                LabelTextInfo.description
            },
            {
                FontSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FontSizeInfo.description
            },
            {
                LabelSizeInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                LabelSizeInfo.description
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
                EnableFadingEffectInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                EnableFadingEffectInfo.description
            },
            {
                PixelSizeControlInfo.identifier,
                new BoolVerifier,
                Optional::Yes,
                PixelSizeControlInfo.description
            },
            {
                FadeStartUnitOptionInfo.identifier,
                new StringInListVerifier(
                    { "m", "Km", "Mm", "Gm", "au", "Tm", "Pm", "pc", "Kpc", "Mpc",
                      "Gpc", "Gly"}
                ),
                Optional::Yes,
                FadeStartUnitOptionInfo.description,
            },
            {
                FadeEndUnitOptionInfo.identifier,
                new StringInListVerifier(
                    {"m", "Km", "Mm", "Gm", "au", "Tm", "Pm", "pc", "Kpc", "Mpc",
                     "Gpc", "Gly"}
                ),
                Optional::Yes,
                FadeEndUnitOptionInfo.description,
            },
            {
                FadeStartDistInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FadeStartDistInfo.description
            },
            {
                FadeEndDistInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FadeEndDistInfo.description
            },
            {
                FadeStartSpeedInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FadeStartSpeedInfo.description
            },
            {
                FadeEndSpeedInfo.identifier,
                new DoubleVerifier,
                Optional::Yes,
                FadeEndSpeedInfo.description
            },
        }
    };
}

RenderableLabels::RenderableLabels(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _blendMode(BlendModeInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _labelColor(
        LabelColorInfo,
        glm::vec4(1.f, 1.f, 1.f, 1.f),
        glm::vec4(0.f),
        glm::vec4(1.f)
    )
    , _labelSize(LabelSizeInfo, 8.f, 0.5f, 30.f)
    , _fontSize(FontSizeInfo, 50.f, 1.f, 100.f)
    , _labelMinSize(LabelMinSizeInfo, 8.f, 0.5f, 24.f)
    , _labelMaxSize(LabelMaxSizeInfo, 20.f, 0.5f, 100.f)
    , _pixelSizeControl(PixelSizeControlInfo, false)
    , _enableFadingEffect(EnableFadingEffectInfo, false)
    , _labelText(LabelTextInfo)
    , _fadeStartDistance(FadeStartDistInfo, 1.f, 0.f, 100.f)
    , _fadeEndDistance(FadeEndDistInfo, 1.f, 0.f, 100.f)
    , _fadeStartSpeed(FadeStartSpeedInfo, 1.f, 1.f, 100.f)
    , _fadeEndSpeed(FadeEndSpeedInfo, 1.f, 1.f, 100.f)
    , _labelOrientationOption(
        LabelOrientationOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _fadeStartUnitOption(
        FadeStartUnitOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
    , _fadeEndUnitOption(
        FadeEndUnitOptionInfo,
        properties::OptionProperty::DisplayType::Dropdown
    )
{
    documentation::testSpecificationAndThrow(
        Documentation(),
        dictionary,
        "RenderableLabels"
    );

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
                setRenderBin(Renderable::RenderBin::Transparent);
                break;
            default:
                throw ghoul::MissingCaseException();
        }
    });

    if (dictionary.hasKey(BlendModeInfo.identifier)) {
        const std::string v = dictionary.value<std::string>(BlendModeInfo.identifier);
        if (v == "Normal") {
            _blendMode = BlendModeNormal;
        }
        else if (v == "Additive") {
            _blendMode = BlendModeAdditive;
        }
    }

    addProperty(_blendMode);

    _labelOrientationOption.addOption(ViewDirection, "Camera View Direction");
    _labelOrientationOption.addOption(NormalDirection, "Camera Position Normal");

    _labelOrientationOption = NormalDirection;
    if (dictionary.hasKeyAndValue<std::string>(LabelOrientationOptionInfo.identifier)) {
        const std::string o = dictionary.value<std::string>(
            LabelOrientationOptionInfo.identifier
            );

        if (o == "Camera View Direction") {
            _labelOrientationOption = ViewDirection;
        }
        else if (o == "Camera Position Normal") {
            _labelOrientationOption = NormalDirection;
        }
    }

    if (dictionary.hasKey(LabelTextInfo.identifier)) {
        _labelText = dictionary.value<std::string>(LabelTextInfo.identifier);
    }
    addProperty(_labelText);

    addProperty(_labelOrientationOption);

    _labelColor.setViewOption(properties::Property::ViewOptions::Color);
    if (dictionary.hasKey(LabelColorInfo.identifier)) {
        _labelColor = dictionary.value<glm::vec4>(LabelColorInfo.identifier);
    }
    addProperty(_labelColor);

    if (dictionary.hasKey(FontSizeInfo.identifier)) {
        _fontSize = dictionary.value<float>(FontSizeInfo.identifier);
    }
    _fontSize.onChange([&]() {
        _font = global::fontManager.font(
            "Mono",
            _fontSize,
            ghoul::fontrendering::FontManager::Outline::Yes,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    });
    addProperty(_fontSize);

    if (dictionary.hasKey(LabelSizeInfo.identifier)) {
        _labelSize = dictionary.value<float>(LabelSizeInfo.identifier);
    }
    addProperty(_labelSize);

    if (dictionary.hasKey(LabelMinSizeInfo.identifier)) {
        _labelMinSize = dictionary.value<float>(LabelMinSizeInfo.identifier);
    }
    addProperty(_labelMinSize);

    if (dictionary.hasKey(LabelMaxSizeInfo.identifier)) {
        _labelMaxSize = dictionary.value<float>(LabelMaxSizeInfo.identifier);
    }
    addProperty(_labelMaxSize);

    if (dictionary.hasKey(TransformationMatrixInfo.identifier)) {
        _transformationMatrix = dictionary.value<glm::dmat4>(
            TransformationMatrixInfo.identifier
            );
    }

    if (dictionary.hasKey(PixelSizeControlInfo.identifier)) {
        _pixelSizeControl = dictionary.value<bool>(PixelSizeControlInfo.identifier);
        addProperty(_pixelSizeControl);
    }

    if (dictionary.hasKey(EnableFadingEffectInfo.identifier)) {
        _enableFadingEffect = dictionary.value<bool>(EnableFadingEffectInfo.identifier);
    }
    addProperty(_enableFadingEffect);

    if (dictionary.hasKey(FadeStartDistInfo.identifier)) {
        _fadeStartDistance = dictionary.value<float>(FadeStartDistInfo.identifier);
    }

    addProperty(_fadeStartDistance);

    _fadeStartUnitOption.addOption(Meter, MeterUnit);
    _fadeStartUnitOption.addOption(Kilometer, KilometerUnit);
    _fadeStartUnitOption.addOption(Megameter, MegameterUnit);
    _fadeStartUnitOption.addOption(Gigameter, GigameterUnit);
    _fadeStartUnitOption.addOption(AU, AstronomicalUnit);
    _fadeStartUnitOption.addOption(Terameter, TerameterUnit);
    _fadeStartUnitOption.addOption(Petameter, PetameterUnit);
    _fadeStartUnitOption.addOption(Parsec, ParsecUnit);
    _fadeStartUnitOption.addOption(Kiloparsec, KiloparsecUnit);
    _fadeStartUnitOption.addOption(Megaparsec, MegaparsecUnit);
    _fadeStartUnitOption.addOption(Gigaparsec, GigaparsecUnit);
    _fadeStartUnitOption.addOption(GigalightYears, GigalightyearUnit);

    _fadeStartUnitOption = AU;

    if (dictionary.hasKey(FadeStartUnitOptionInfo.identifier)) {
        std::string unit = dictionary.value<std::string>(
            FadeStartUnitOptionInfo.identifier
        );
        if (unit == MeterUnit) {
            _fadeStartUnitOption = Meter;
        }
        else if (unit == KilometerUnit) {
            _fadeStartUnitOption = Kilometer;
        }
        else if (unit == MegameterUnit) {
            _fadeStartUnitOption = Megameter;
        }
        else if (unit == GigameterUnit) {
            _fadeStartUnitOption = Gigameter;
        }
        else if (unit == AstronomicalUnit) {
            _fadeStartUnitOption = AU;
        }
        else if (unit == TerameterUnit) {
            _fadeStartUnitOption = Terameter;
        }
        else if (unit == PetameterUnit) {
            _fadeStartUnitOption = Petameter;
        }
        else if (unit == ParsecUnit) {
            _fadeStartUnitOption = Parsec;
        }
        else if (unit == KiloparsecUnit) {
            _fadeStartUnitOption = Kiloparsec;
        }
        else if (unit == MegaparsecUnit) {
            _fadeStartUnitOption = Megaparsec;
        }
        else if (unit == GigaparsecUnit) {
            _fadeStartUnitOption = Gigaparsec;
        }
        else if (unit == GigalightyearUnit) {
            _fadeStartUnitOption = GigalightYears;
        }
        else {
            LWARNING(
                "No unit given for RenderableLabels. Using kilometer as units."
            );
            _fadeStartUnitOption = Kilometer;
        }
    }

    addProperty(_fadeStartUnitOption);

    if (dictionary.hasKey(FadeStartSpeedInfo.identifier)) {
        _fadeStartSpeed = dictionary.value<float>(FadeStartSpeedInfo.identifier);
    }

    addProperty(_fadeStartSpeed);

    if (dictionary.hasKey(FadeEndDistInfo.identifier)) {
        _fadeEndDistance = dictionary.value<float>(FadeEndDistInfo.identifier);
    }

    addProperty(_fadeEndDistance);

    _fadeEndUnitOption.addOption(Meter, MeterUnit);
    _fadeEndUnitOption.addOption(Kilometer, KilometerUnit);
    _fadeEndUnitOption.addOption(Megameter, MegameterUnit);
    _fadeEndUnitOption.addOption(Gigameter, GigameterUnit);
    _fadeEndUnitOption.addOption(AU, AstronomicalUnit);
    _fadeEndUnitOption.addOption(Terameter, TerameterUnit);
    _fadeEndUnitOption.addOption(Petameter, PetameterUnit);
    _fadeEndUnitOption.addOption(Parsec, ParsecUnit);
    _fadeEndUnitOption.addOption(Kiloparsec, KiloparsecUnit);
    _fadeEndUnitOption.addOption(Megaparsec, MegaparsecUnit);
    _fadeEndUnitOption.addOption(Gigaparsec, GigaparsecUnit);
    _fadeEndUnitOption.addOption(GigalightYears, GigalightyearUnit);

    _fadeEndUnitOption = AU;

    if (dictionary.hasKey(FadeEndUnitOptionInfo.identifier)) {
        std::string unit = dictionary.value<std::string>(
            FadeEndUnitOptionInfo.identifier
        );
        if (unit == MeterUnit) {
            _fadeEndUnitOption = Meter;
        }
        else if (unit == KilometerUnit) {
            _fadeEndUnitOption = Kilometer;
        }
        else if (unit == MegameterUnit) {
            _fadeEndUnitOption = Megameter;
        }
        else if (unit == GigameterUnit) {
            _fadeEndUnitOption = Gigameter;
        }
        else if (unit == AstronomicalUnit) {
            _fadeEndUnitOption = AU;
        }
        else if (unit == TerameterUnit) {
            _fadeEndUnitOption = Terameter;
        }
        else if (unit == PetameterUnit) {
            _fadeEndUnitOption = Petameter;
        }
        else if (unit == ParsecUnit) {
            _fadeEndUnitOption = Parsec;
        }
        else if (unit == KiloparsecUnit) {
            _fadeEndUnitOption = Kiloparsec;
        }
        else if (unit == MegaparsecUnit) {
            _fadeEndUnitOption = Megaparsec;
        }
        else if (unit == GigaparsecUnit) {
            _fadeEndUnitOption = Gigaparsec;
        }
        else if (unit == GigalightyearUnit) {
            _fadeEndUnitOption = GigalightYears;
        }
        else {
            LWARNING(
                "No unit given for RenderableLabels. Using kilometer as units."
            );
            _fadeEndUnitOption = Kilometer;
        }
    }

    addProperty(_fadeEndUnitOption);

    if (dictionary.hasKey(FadeEndSpeedInfo.identifier)) {
        _fadeEndSpeed = dictionary.value<float>(FadeEndSpeedInfo.identifier);
    }

    addProperty(_fadeEndSpeed);
}

bool RenderableLabels::isReady() const {
    return true;
}

void RenderableLabels::initialize() {
    bool success = true;// loadData();
    if (!success) {
        throw ghoul::RuntimeError("Error loading objects labels data.");
    }

    setRenderBin(Renderable::RenderBin::Transparent);
}

void RenderableLabels::initializeGL() {
    if (_font == nullptr) {
        //size_t _fontSize = 50;
        _font = global::fontManager.font(
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
        float sUnit = unit(_fadeStartUnitOption);
        float eUnit = unit(_fadeEndUnitOption);
        float startX = _fadeStartDistance * sUnit;
        float endX = _fadeEndDistance * eUnit;
        fadeInVariable = linearSmoothStepFunc(
            distanceNodeToCamera,
            startX,
            endX,
            sUnit,
            eUnit
        );
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
    _labelText = newText;
}

void RenderableLabels::renderLabels(const RenderData& data,
                                    const glm::dmat4& modelViewProjectionMatrix,
                                    const glm::dvec3& orthoRight,
                                    const glm::dvec3& orthoUp, float fadeInVariable)
{
    glm::vec4 textColor = _labelColor;

    textColor.a *= fadeInVariable;
    textColor.a *= _opacity;

    ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo;

    labelInfo.orthoRight       = orthoRight;
    labelInfo.orthoUp          = orthoUp;
    labelInfo.minSize          = static_cast<int>(_labelMinSize);
    labelInfo.maxSize          = static_cast<int>(_labelMaxSize);
    labelInfo.cameraPos        = data.camera.positionVec3();
    labelInfo.cameraLookUp     = data.camera.lookUpVectorWorldSpace();
    labelInfo.renderType       = _labelOrientationOption;
    labelInfo.mvpMatrix        = modelViewProjectionMatrix;
    labelInfo.scale            = powf(10.f, _labelSize);
    labelInfo.enableDepth      = true;
    labelInfo.enableFalseDepth = false;

    // We don't use spice rotation and scale
    glm::vec3 transformedPos(
        _transformationMatrix * glm::dvec4(data.modelTransform.translation, 1.0)
    );

    ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
        *_font,
        transformedPos,
        _labelText,
        textColor,
        labelInfo
    );
}

float RenderableLabels::changedPerlinSmoothStepFunc(float x, float startX,
                                                    float endX) const
{
    float f1 = 6.f * powf((x - startX), 5.f) - 15.f * powf((x - startX), 4.f) +
               10.f * powf((x - startX), 3.f);
    float f2 = -6.f * powf((x - endX), 5.f) + 15.f * powf((x - endX), 4.f) -
               10.f * powf((x - endX), 3.f) + 1.f;
    float f3 = 1.f;

    if (x <= startX) {
        return std::clamp(f1, 0.f, 1.f);
    }
    else if (x > startX && x < endX) {
        return f3;
    }
    else if (x >= endX) {
        return std::clamp(f2, 0.f, 1.f);
    }
    return x;
}

float RenderableLabels::linearSmoothStepFunc(float x, float startX, float endX,
                                             float sUnit, float eUnit) const
{
    float sdiv = 1.f / (sUnit * _fadeStartSpeed);
    float ediv = -1.f / (eUnit * _fadeEndSpeed);
    float f1 = sdiv * (x - startX) + 1.f;
    float f2 = ediv * (x - endX) + 1.f;
    float f3 = 1.f;

    if (x <= startX) {
        return std::clamp(f1, 0.f, 1.f);
    }
    else if (x > startX && x < endX) {
        return f3;
    }
    else if (x >= endX) {
        return std::clamp(f2, 0.f, 1.f);
    }
    return x;
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
