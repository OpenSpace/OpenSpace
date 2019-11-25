/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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

#include <modules/base/rendering/RenderableLabels.h>

#include <modules/base/basemodule.h>
#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/filesystem/cachemanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/templatefactory.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/misc/defer.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/glm.h>
#include <glm/gtx/string_cast.hpp>

namespace {
    constexpr const char* _loggerCat = "base::RenderableLabels";
    
    enum BlendMode {
        BlendModeNormal = 0,
        BlendModeAdditive
    };

    constexpr const int ViewDirection   = 0;
    constexpr const int NormalDirection = 1;

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

    constexpr openspace::properties::Property::PropertyInfo PixelSizeControlInfo = {
        "EnablePixelSizeControl",
        "Enable pixel size control.",
        "Enable pixel size control for rectangular projections."
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
            }
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
    , _fadeInDistance(
        FadeInDistancesInfo,
        glm::vec2(0.f),
        glm::vec2(0.f),
        glm::vec2(100.f)
    )
    , _disableFadeInDistance(DisableFadeInInfo, true)
    , _labelOrientationOption(LabelOrientationOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
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
        const std::string o = dictionary.value<std::string>(LabelOrientationOptionInfo.identifier);

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

    if (dictionary.hasKey(FadeInDistancesInfo.identifier)) {
        glm::vec2 v = dictionary.value<glm::vec2>(FadeInDistancesInfo.identifier);
        _fadeInDistance = v;
        _disableFadeInDistance = false;
        addProperty(_fadeInDistance);
        addProperty(_disableFadeInDistance);
    }

    if (dictionary.hasKey(PixelSizeControlInfo.identifier)) {
        _pixelSizeControl = dictionary.value<bool>(PixelSizeControlInfo.identifier);
        addProperty(_pixelSizeControl);
    }

    //setBoundingSphere(_size);
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

void RenderableLabels::deinitializeGL() {
}

void RenderableLabels::render(const RenderData& data, RendererTasks&) {

    //bool additiveBlending = (_blendMode == BlendModeAdditive);
    //if (additiveBlending) {
        glDepthMask(false);
        glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    //}

    float fadeInVariable = 1.f;
    if (!_disableFadeInDistance) {
        float distCamera = static_cast<float>(glm::length(data.camera.positionVec3()));
        const glm::vec2 fadeRange = _fadeInDistance;
        const float a = 1.f / ((fadeRange.y - fadeRange.x));
        const float b = -(fadeRange.x / (fadeRange.y - fadeRange.x));
        const float funcValue = a * distCamera + b;
        fadeInVariable *= funcValue > 1.f ? 1.f : funcValue;

        if (funcValue < 0.01f) {
            return;
        }
    }

    //glm::dmat4 modelMatrix =
    //    glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
    //    glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
    //    glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

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

void RenderableLabels::update(const UpdateData&) {
    // JCC: Change font size?
}

void RenderableLabels::renderLabels(const RenderData& data, 
                                    const glm::dmat4& modelViewProjectionMatrix,
                                    const glm::dvec3& orthoRight,
                                    const glm::dvec3& orthoUp,
                                    float fadeInVariable)
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
} // namespace openspace
