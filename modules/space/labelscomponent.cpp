/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/space/labelscomponent.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/documentation/documentation.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <optional>

namespace {
    constexpr std::string_view _loggerCat = "LabelsComponent";

    constexpr int RenderOptionFaceCamera = 0;
    constexpr int RenderOptionPositionNormal = 1;

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "File",
        "File",
        "The speck label file with the data for the labels"
    };

    constexpr openspace::properties::Property::PropertyInfo OpacityInfo = {
        "Opacity",
        "Opacity",
        "Determines the transparency of the labels, where 1 is completely opaque "
        "and 0 fully transparent"
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the labels"
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "The size of the labels in pixels"
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "Font size for the labels. This is different from the text size"
    };

    constexpr openspace::properties::Property::PropertyInfo MinMaxInfo = {
        "MinMaxSize",
        "Min/Max Size",
        "The minimum and maximum size (in pixels) of the labels"
    };

    constexpr openspace::properties::Property::PropertyInfo FaceCameraInfo = {
        "FaceCamera",
        "Face Camera",
        "If enabled, the labels will be rotated to face the camera. For non-linear "
        "display rendering (for example fisheye) this should be set to false."
    };

    struct [[codegen::Dictionary(LabelsComponent)]] Parameters {
        // [[codegen::verbatim(FileInfo.description)]]
        std::filesystem::path file;

        // [[codegen::verbatim(OpacityInfo.description)]]
        std::optional<float> opacity [[codegen::inrange(0.0, 1.0)]];

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(SizeInfo.description)]]
        std::optional<float> size;

        // [[codegen::verbatim(FontSizeInfo.description)]]
        std::optional<float> fontSize;

        // [[codegen::verbatim(MinMaxInfo.description)]]
        std::optional<glm::ivec2> minMaxSize;

        enum class [[codegen::map(openspace::DistanceUnit)]] Unit {
            Meter [[codegen::key("m")]],
            Kilometer [[codegen::key("Km")]],
            Parsec [[codegen::key("pc")]],
            Kiloparsec [[codegen::key("Kpc")]],
            Megaparsec [[codegen::key("Mpc")]],
            Gigaparsec [[codegen::key("Gpc")]],
            Gigalightyear [[codegen::key("Gly")]]
        };
        std::optional<Unit> unit;

        // [[codegen::verbatim(FaceCameraInfo.description)]]
        std::optional<bool> faceCamera;
    };
#include "labelscomponent_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation LabelsComponent::Documentation() {
    return codegen::doc<Parameters>("space_labelscomponent");
}

LabelsComponent::LabelsComponent(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "Labels" })
    , _opacity(OpacityInfo, 1.f, 0.f, 1.f)
    , _color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _size(SizeInfo, 8.f, 0.5f, 24.f)
    , _fontSize(FontSizeInfo, 50.f, 1.f, 300.f)
    , _minMaxSize(
        MinMaxInfo,
        glm::ivec2(8, 500),
        glm::ivec2(0),
        glm::ivec2(1000)
    )
    , _faceCamera(FaceCameraInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _labelFile = absPath(p.file);

    if (p.unit.has_value()) {
        _unit = codegen::map<DistanceUnit>(*p.unit);
    }
    else {
        _unit = DistanceUnit::Meter;
    }

    _opacity = p.opacity.value_or(_opacity);
    addProperty(_opacity);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    _size = p.size.value_or(_size);
    addProperty(_size);

    _fontSize = p.fontSize.value_or(_fontSize);
    _fontSize.onChange([this]() { initialize(); });
    addProperty(_fontSize);
    // @TODO (emmbr, 2021-05-31): Temporarily set as read only, to avoid errors from font
    // rendering (avoid filling font atlas)
    _fontSize.setReadOnly(true);

    _minMaxSize = p.minMaxSize.value_or(_minMaxSize);
    _minMaxSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
    addProperty(_minMaxSize);

    if (p.faceCamera.has_value()) {
        _faceCamera = *p.faceCamera;
    }
    else {
        // @TODO (abock. 2021-01-31) In the other DU classes, this is done with an enum,
        // and doing it based on the fisheye rendering seems a bit brittle?

        // (malej 2022-SEP-14)
        // For non-linear display rendering (for example fisheye) _faceCamera should be
        // false, otherwise true
        _faceCamera = !global::windowDelegate->isFisheyeRendering();
    }
    addProperty(_faceCamera);
}

speck::Labelset& LabelsComponent::labelSet() {
    return _labelset;
}

const speck::Labelset& LabelsComponent::labelSet() const {
    return _labelset;
}

void LabelsComponent::initialize() {
    _font = global::fontManager->font(
        "Mono",
        _fontSize,
        ghoul::fontrendering::FontManager::Outline::Yes,
        ghoul::fontrendering::FontManager::LoadGlyphs::No
    );
}

void LabelsComponent::loadLabels() {
    LINFO(fmt::format("Loading label file {}", _labelFile));
    _labelset = speck::label::loadFileWithCache(_labelFile);
}

bool LabelsComponent::isReady() const {
    return !(_labelset.entries.empty());
}

void LabelsComponent::render(const RenderData& data,
                             const glm::dmat4& modelViewProjectionMatrix,
                             const glm::vec3& orthoRight, const glm::vec3& orthoUp,
                             float fadeInVariable)
{
    float scale = static_cast<float>(toMeter(_unit));

    int renderOption = _faceCamera ? RenderOptionFaceCamera : RenderOptionPositionNormal;

    ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo;
    labelInfo.orthoRight = orthoRight;
    labelInfo.orthoUp = orthoUp;
    labelInfo.minSize = _minMaxSize.value().x;
    labelInfo.maxSize = _minMaxSize.value().y;
    labelInfo.cameraPos = data.camera.positionVec3();
    labelInfo.cameraLookUp = data.camera.lookUpVectorWorldSpace();
    labelInfo.renderType = renderOption;
    labelInfo.mvpMatrix = modelViewProjectionMatrix;
    labelInfo.scale = pow(10.f, _size);
    labelInfo.enableDepth = true;
    labelInfo.enableFalseDepth = false;

    glm::vec4 textColor = glm::vec4(glm::vec3(_color), _opacity * fadeInVariable);

    for (const speck::Labelset::Entry& e : _labelset.entries) {
        glm::vec3 scaledPos(e.position);
        scaledPos *= scale;
        ghoul::fontrendering::FontRenderer::defaultProjectionRenderer().render(
            *_font,
            scaledPos,
            e.text,
            textColor,
            labelInfo
        );
    }
}

} // namespace openspace
