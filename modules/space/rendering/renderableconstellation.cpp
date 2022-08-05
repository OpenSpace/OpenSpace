/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

#include <modules/space/rendering/renderableconstellation.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/font/fontmanager.h>
#include <ghoul/font/fontrenderer.h>
#include <ghoul/glm.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/misc.h>
#include <ghoul/opengl/programobject.h>
#include <fstream>
#include <optional>
#include "SpiceUsr.h"

namespace {
    constexpr int RenderOptionViewDirection = 0;
    constexpr int RenderOptionPositionNormal = 1;

    constexpr openspace::properties::Property::PropertyInfo TextColorInfo = {
       "TextColor",
       "Text Color",
       "The text color for the astronomical object"
    };

    constexpr openspace::properties::Property::PropertyInfo TextOpacityInfo = {
        "TextOpacity",
        "Text Opacity",
        "Determines the transparency of the text label, where 1 is completely opaque "
        "and 0 fully transparent"
    };

    constexpr openspace::properties::Property::PropertyInfo TextSizeInfo = {
        "TextSize",
        "Text Size",
        "The text size for the astronomical object labels"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelFileInfo = {
        "LabelFile",
        "Label File",
        "The path to the label file that contains information about the astronomical "
        "objects being rendered"
    };

    constexpr openspace::properties::Property::PropertyInfo LabelMinMaxSizeInfo = {
        "TextMinMaxSize",
        "Text Min/Max Size",
        "The minimum and maximum size (in pixels) of the text for the labels for the "
        "astronomical objects being rendered"
    };

    constexpr openspace::properties::Property::PropertyInfo ConstellationInfo = {
        "ConstellationFile",
        "Constellation File Path",
        "Specifies the file that contains the mapping between constellation "
        "abbreviations and full name of the constellation. If this value is empty, the "
        "abbreviations are used as the full names"
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The line width of the constellation "
    };

    constexpr openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden"
    };

    constexpr openspace::properties::Property::PropertyInfo RenderOptionInfo = {
        "RenderOption",
        "Render Option",
        "Debug option for rendering of billboards and texts"
    };

    constexpr openspace::properties::Property::PropertyInfo SelectionInfo = {
        "ConstellationSelection",
        "Constellation Selection",
        "The constellations that are selected are displayed on the celestial sphere"
    };

    struct [[codegen::Dictionary(RenderableConstellation)]] Parameters {
        // [[codegen::verbatim(DrawLabelInfo.description)]]
        std::optional<bool> drawLabels;

        // [[codegen::verbatim(ConstellationInfo.description)]]
        std::string constellationNamesFile;

        // [[codegen::verbatim(TextColorInfo.description)]]
        std::optional<glm::vec3> textColor [[codegen::color()]];

        // [[codegen::verbatim(TextOpacityInfo.description)]]
        std::optional<float> textOpacity;

        // [[codegen::verbatim(TextSizeInfo.description)]]
        std::optional<float> textSize;

        // [[codegen::verbatim(LabelFileInfo.description)]]
        std::optional<std::string> labelFile;

        // [[codegen::verbatim(LabelMinMaxSizeInfo.description)]]
        std::optional<glm::ivec2> textMinMaxSize;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(SelectionInfo.description)]]
        std::optional<std::vector<std::string>> constellationSelection;
    };
#include "renderableconstellation_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableConstellation::Documentation() {
    return codegen::doc<Parameters>("space_renderable_constellation");
}

RenderableConstellation::RenderableConstellation(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _textColor(TextColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _textOpacity(TextOpacityInfo, 1.f, 0.f, 1.f)
    , _textSize(TextSizeInfo, 8.f, 0.5f, 24.f)
    , _drawLabels(DrawLabelInfo, false)
    , _textMinMaxSize(
        LabelMinMaxSizeInfo,
        glm::ivec2(8, 500),
        glm::ivec2(0),
        glm::ivec2(1000)
    )
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 16.f)
    , _renderOption(RenderOptionInfo, properties::OptionProperty::DisplayType::Dropdown)
    , _constellationNamesFilename(ConstellationInfo)
    , _constellationSelection(SelectionInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    _renderOption.addOption(RenderOptionViewDirection, "Camera View Direction");
    _renderOption.addOption(RenderOptionPositionNormal, "Camera Position Normal");
    // @TODO (abock. 2021-01-31) In the other DU classes, this is done with an enum, and
    // doing it based on the fisheye rendering seems a bit brittle?
    if (global::windowDelegate->isFisheyeRendering()) {
        _renderOption = RenderOptionPositionNormal;
    }
    else {
        _renderOption = RenderOptionViewDirection;
    }
    addProperty(_renderOption);

    // Read all files in the initialize() instead, multithreaded
    _constellationNamesFilename = p.constellationNamesFile;
    _constellationNamesFilename.onChange([&]() { loadConstellationFile(); });
    addProperty(_constellationNamesFilename);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    if (p.labelFile.has_value()) {
        _labelFile = absPath(*p.labelFile).string();
        _hasLabel = true;

        _drawLabels = p.drawLabels.value_or(_drawLabels);
        addProperty(_drawLabels);

        _textColor = p.textColor.value_or(_textColor);
        _hasLabel = p.textColor.has_value();
        _textColor.setViewOption(properties::Property::ViewOptions::Color);
        addProperty(_textColor);
        _textColor.onChange([&]() { _textColorIsDirty = true; });

        _textOpacity = p.textOpacity.value_or(_textOpacity);
        addProperty(_textOpacity);

        _textSize = p.textSize.value_or(_textSize);
        addProperty(_textSize);

        _textMinMaxSize = p.textMinMaxSize.value_or(_textMinMaxSize);
        _textMinMaxSize.setViewOption(properties::Property::ViewOptions::MinMaxRange);
        addProperty(_textMinMaxSize);
    }

    _constellationSelection.onChange([this]() { selectionPropertyHasChanged(); });
    addProperty(_constellationSelection);

    _assetSelectedMeshes = p.constellationSelection.value_or(_assetSelectedMeshes);
}

std::string RenderableConstellation::constellationFullName(
                                                      const std::string& identifier) const
{
    try {
        return _constellationNamesTranslation.at(identifier);
    }
    catch (const std::out_of_range&) {
        std::string message = fmt::format(
            "Identifier '{}' could not be found in list of constellations", identifier
        );
        throw ghoul::RuntimeError(message, "RenderableConstellation");
    }
}

void RenderableConstellation::loadConstellationFile() {
    if (_constellationNamesFilename.value().empty()) {
        return;
    }

    // Reset
    _constellationSelection.clearOptions();
    _constellationNamesTranslation.clear();

    std::ifstream file;
    file.exceptions(std::ifstream::goodbit);
    file.open(absPath(_constellationNamesFilename));

    std::string line;
    int index = 0;
    while (file.good()) {
        std::getline(file, line);
        if (line.empty()) {
            continue;
        }

        std::string abbreviation;
        std::stringstream s(line);
        s >> abbreviation;

        std::string fullName;
        std::getline(s, fullName);
        ghoul::trimWhitespace(fullName);
        _constellationNamesTranslation[abbreviation] = fullName;

        ++index;
    }

    fillSelectionProperty();
}

void RenderableConstellation::fillSelectionProperty() {
    for (const std::pair<std::string, std::string>& pair : _constellationNamesTranslation) {
        _constellationSelection.addOption(pair.second);
    }
}

void RenderableConstellation::initialize() {
    loadConstellationFile();

    if (!_assetSelectedMeshes.empty()) {
        const std::vector<std::string> options = _constellationSelection.options();
        std::set<std::string> selectedConstellations;

        for (const std::string& s : _assetSelectedMeshes) {
            const auto it = std::find(options.begin(), options.end(), s);
            if (it == options.end()) {
                // The user has specified a mesh name that doesn't exist
                LWARNINGC(
                    "RenderableConstellation",
                    fmt::format("Option '{}' not found in list of meshes", s)
                );
            }
            else {
                selectedConstellations.insert(s);
            }
        }
        _constellationSelection = selectedConstellations;
    }

    if (!_hasLabel) {
        return;
    }

    if (!_font) {
        constexpr int FontSize = 50;
        _font = global::fontManager->font(
            "Mono",
            static_cast<float>(FontSize),
            ghoul::fontrendering::FontManager::Outline::Yes,
            ghoul::fontrendering::FontManager::LoadGlyphs::No
        );
    }

    std::string labelFile = _labelFile;
    if (!labelFile.empty()) {
        _labelset = speck::label::loadFileWithCache(_labelFile);
    }

    for (speck::Labelset::Entry& entry : _labelset.entries) {
        if (!entry.identifier.empty()) {
            entry.text = constellationFullName(entry.identifier);
        }
    }
}

void RenderableConstellation::render(const RenderData& data, RendererTasks&) {
    if (!_hasLabel || !_drawLabels) {
        return;
    }

    const glm::dmat4 modelMatrix =
        glm::translate(glm::dmat4(1.0), data.modelTransform.translation) * // Translation
        glm::dmat4(data.modelTransform.rotation) *  // Spice rotation
        glm::scale(glm::dmat4(1.0), glm::dvec3(data.modelTransform.scale));

    const glm::dmat4 modelViewMatrix = data.camera.combinedViewMatrix() * modelMatrix;
    const glm::dmat4 projectionMatrix = data.camera.projectionMatrix();
    const glm::dmat4 modelViewProjectionMatrix = projectionMatrix * modelViewMatrix;

    const glm::vec3 lookup = data.camera.lookUpVectorWorldSpace();
    const glm::vec3 viewDirection = data.camera.viewDirectionWorldSpace();
    glm::vec3 right = glm::cross(viewDirection, lookup);
    const glm::vec3 up = glm::cross(right, viewDirection);

    const glm::dmat4 worldToModelTransform = glm::inverse(modelMatrix);
    glm::vec3 orthoRight = glm::normalize(
        glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
    );

    if (orthoRight == glm::vec3(0.0)) {
        glm::vec3 otherVector(lookup.y, lookup.x, lookup.z);
        right = glm::cross(viewDirection, otherVector);
        orthoRight = glm::normalize(
            glm::vec3(worldToModelTransform * glm::vec4(right, 0.0))
        );
    }

    const glm::vec3 orthoUp = glm::normalize(
        glm::vec3(worldToModelTransform * glm::dvec4(up, 0.0))
    );
    renderLabels(data, modelViewProjectionMatrix, orthoRight, orthoUp);
}

void RenderableConstellation::renderLabels(const RenderData& data,
                                           const glm::dmat4& modelViewProjectionMatrix,
                                           const glm::vec3& orthoRight,
                                           const glm::vec3& orthoUp)
{
    float scale = static_cast<float>(toMeter(_labelUnit));

    ghoul::fontrendering::FontRenderer::ProjectedLabelsInformation labelInfo;
    labelInfo.orthoRight = orthoRight;
    labelInfo.orthoUp = orthoUp;
    labelInfo.minSize = _textMinMaxSize.value().x;
    labelInfo.maxSize = _textMinMaxSize.value().y;
    labelInfo.cameraPos = data.camera.positionVec3();
    labelInfo.cameraLookUp = data.camera.lookUpVectorWorldSpace();
    labelInfo.renderType = _renderOption;
    labelInfo.mvpMatrix = modelViewProjectionMatrix;
    labelInfo.scale = pow(10.f, _textSize);
    labelInfo.enableDepth = true;
    labelInfo.enableFalseDepth = false;

    glm::vec4 textColor = glm::vec4(glm::vec3(_textColor), _textOpacity);

    for (const speck::Labelset::Entry& e : _labelset.entries) {
        if (_constellationSelection.hasSelected() &&
           !_constellationSelection.isSelected(e.text))
        {
            continue;
        }

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
