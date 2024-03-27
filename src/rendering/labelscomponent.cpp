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

#include <openspace/rendering/labelscomponent.h>

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

    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "This setting determines whether the labels will be visible or not. They are "
        "disabled per default",
        // @VISIBILITY(?)
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "File",
        "File",
        "The speck label file with the data for the labels",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the labels",
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo SizeInfo = {
        "Size",
        "Size",
        "The size of the labels in pixels",
        // @VISIBILITY(2.5)
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo FontSizeInfo = {
        "FontSize",
        "Font Size",
        "Font size for the labels. This is different from the text size",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo MinMaxInfo = {
        "MinMaxSize",
        "Min/Max Size",
        "The minimum and maximum size (in pixels) of the labels",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo FaceCameraInfo = {
        "FaceCamera",
        "Face Camera",
        "If enabled, the labels will be rotated to face the camera. For non-linear "
        "display rendering (for example fisheye) this should be set to false.",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo TransformationMatrixInfo = {
        "TransformationMatrix",
        "Transformation Matrix",
        "Transformation matrix to be applied to the labels",
        // @VISIBILITY(?)
        openspace::properties::Property::Visibility::Developer
    };

    struct [[codegen::Dictionary(LabelsComponent)]] Parameters {
        // [[codegen::verbatim(EnabledInfo.description)]]
        std::optional<bool> enabled;

        // [[codegen::verbatim(FileInfo.description)]]
        std::optional<std::filesystem::path> file;

        // If true (default), the loaded labels file will be cached so that it can be
        // loaded faster at a later time. Note that this also means that changes in the
        // file will not be registered until the cached file is deleted. Set to false
        // to disable chaching and always do a fresh load of the label file
        std::optional<bool> useCaching;

        // The opacity of the labels
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

        // [[codegen::verbatim(TransformationMatrixInfo.description)]]
        std::optional<glm::dmat4x4> transformationMatrix;
    };
#include "labelscomponent_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation LabelsComponent::Documentation() {
    return codegen::doc<Parameters>("labelscomponent");
}

LabelsComponent::LabelsComponent(const ghoul::Dictionary& dictionary)
    : properties::PropertyOwner({ "Labels" })
    , _enabled(EnabledInfo, false)
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

    _labelFile = absPath(p.file.value_or(""));
    _useCache = p.useCaching.value_or(true);

    if (p.unit.has_value()) {
        _unit = codegen::map<DistanceUnit>(*p.unit);
    }
    else {
        _unit = DistanceUnit::Meter;
    }

    _enabled = p.enabled.value_or(_enabled);
    addProperty(_enabled);

    _opacity = p.opacity.value_or(_opacity);
    addProperty(Fadeable::_opacity);

    addProperty(Fadeable::_fade);

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

    _transformationMatrix = p.transformationMatrix.value_or(_transformationMatrix);
}

dataloader::Labelset& LabelsComponent::labelSet() {
    return _labelset;
}

const dataloader::Labelset& LabelsComponent::labelSet() const {
    return _labelset;
}

void LabelsComponent::initialize() {
    ZoneScoped;

    _font = global::fontManager->font(
        "Mono",
        _fontSize,
        ghoul::fontrendering::FontManager::Outline::Yes,
        ghoul::fontrendering::FontManager::LoadGlyphs::No
    );

    loadLabels();
}

void LabelsComponent::loadLabelsFromDataset(const dataloader::Dataset& dataset,
                                            DistanceUnit unit)
{
    ZoneScoped;

    LINFO("Loading labels from dataset");

    // The unit should match the one in the dataset, not the one that was included in the
    // asset (if any)
    _unit = unit;

    // Load the labelset directly based on the dataset, and keep track of that it has
    // already been loaded this way
    _labelset = dataloader::label::loadFromDataset(dataset);

    _createdFromDataset = true;
}

void LabelsComponent::loadLabels() {
    ZoneScoped;

    if (_createdFromDataset) {
        // The labelset should already have been loaded
        return;
    }

    LINFO(std::format("Loading label file '{}'", _labelFile));

    if (_useCache) {
        _labelset = dataloader::label::loadFileWithCache(_labelFile);
    }
    else {
        _labelset = dataloader::label::loadFile(_labelFile);
    }
}

bool LabelsComponent::isReady() const {
    return !(_labelset.entries.empty());
}

bool LabelsComponent::enabled() const {
    return _enabled;
}

void LabelsComponent::render(const RenderData& data,
                             const glm::dmat4& modelViewProjectionMatrix,
                             const glm::vec3& orthoRight, const glm::vec3& orthoUp,
                             float fadeInVariable)
{
    if (!_enabled) {
        return;
    }
    const float scale = static_cast<float>(toMeter(_unit));

    const int renderOption =
        _faceCamera ? RenderOptionFaceCamera : RenderOptionPositionNormal;

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

    const glm::vec4 textColor = glm::vec4(glm::vec3(_color), opacity() * fadeInVariable);

    for (const dataloader::Labelset::Entry& e : _labelset.entries) {
        if (!e.isEnabled) {
            continue;
        }

        // Transform and scale the labels
        const glm::vec3 transformedPos = glm::vec3(
            _transformationMatrix * glm::dvec4(e.position, 1.0)
        );
        const glm::vec3 scaledPos = glm::vec3(transformedPos * scale);

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
