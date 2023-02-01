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

#include <modules/space/rendering/renderableconstellationsbase.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/glm.h>
#include <ghoul/misc/misc.h>
#include <ghoul/opengl/programobject.h>
#include <fstream>
#include <optional>

namespace {
    constexpr openspace::properties::Property::PropertyInfo NamesFileInfo = {
        "NamesFile",
        "Constellation Names File Path",
        "Specifies the file that contains the mapping between constellation "
        "abbreviations and full names of the constellations. If this value is empty, the "
        "abbreviations are used as the full names"
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The line width of the constellation"
    };

    constexpr openspace::properties::Property::PropertyInfo DrawLabelInfo = {
        "DrawLabels",
        "Draw Labels",
        "Determines whether labels should be drawn or hidden"
    };

    constexpr openspace::properties::Property::PropertyInfo SelectionInfo = {
        "ConstellationSelection",
        "Constellation Selection",
        "The constellations that are selected are displayed on the celestial sphere"
    };

    const static openspace::properties::PropertyOwner::PropertyOwnerInfo LabelsInfo = {
        "Labels",
        "Labels",
        "The labels for the constellations"
    };

    struct [[codegen::Dictionary(RenderableConstellationsBase)]] Parameters {
        // [[codegen::verbatim(DrawLabelInfo.description)]]
        std::optional<bool> drawLabels;

        // [[codegen::verbatim(NamesFileInfo.description)]]
        std::optional<std::filesystem::path> namesFile;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;

        // [[codegen::verbatim(SelectionInfo.description)]]
        std::optional<std::vector<std::string>> selection;

        // [[codegen::verbatim(LabelsInfo.description)]]
        std::optional<ghoul::Dictionary> labels
            [[codegen::reference("space_labelscomponent")]];
    };
#include "renderableconstellationsbase_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableConstellationsBase::Documentation() {
    return codegen::doc<Parameters>("space_renderable_constellationsbase");
}

RenderableConstellationsBase::RenderableConstellationsBase(
                                                      const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _drawLabels(DrawLabelInfo, false)
    , _lineWidth(LineWidthInfo, 2.f, 1.f, 16.f)
    , _namesFilename(NamesFileInfo)
    , _selection(SelectionInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    // Avoid reading files here, instead do it in multithreaded initialize()
    if (p.namesFile.has_value()) {
        _namesFilename = absPath(p.namesFile.value().string()).string();
    }
    _namesFilename.onChange([&]() { loadConstellationFile(); });
    addProperty(_namesFilename);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    if (p.labels.has_value()) {
        _drawLabels = p.drawLabels.value_or(_drawLabels);
        addProperty(_drawLabels);

        _labels = std::make_unique<LabelsComponent>(*p.labels);
        _hasLabels = true;
        addPropertySubOwner(_labels.get());
    }

    _selection.onChange([this]() { selectionPropertyHasChanged(); });
    addProperty(_selection);

    _assetSelection = p.selection.value_or(_assetSelection);
}

std::string RenderableConstellationsBase::constellationFullName(
                                                      const std::string& identifier) const
{
    if (_namesTranslation.empty() || identifier.empty()) {
        std::string message = "List of constellations or the given identifier was empty";
        LWARNINGC("RenderableConstellationsBase", message);
        return "";
    }

    if (_namesTranslation.contains(identifier)) {
        return _namesTranslation.at(identifier);
    }

    std::string message = fmt::format(
        "Identifier '{}' could not be found in list of constellations", identifier
    );
    LERRORC("RenderableConstellationsBase", message);
    return "";
}

void RenderableConstellationsBase::loadConstellationFile() {
    if (_namesFilename.value().empty()) {
        return;
    }

    // Reset
    _selection.clearOptions();
    _namesTranslation.clear();

    // Load the constellation names file
    std::ifstream file;
    file.exceptions(std::ifstream::goodbit);
    file.open(absPath(_namesFilename));

    std::string line;
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
        _namesTranslation[abbreviation] = fullName;
    }

    fillSelectionProperty();
}

void RenderableConstellationsBase::fillSelectionProperty() {
    for (const std::pair<std::string, std::string>& pair : _namesTranslation) {
        _selection.addOption(pair.second);
    }
}

void RenderableConstellationsBase::initialize() {
    loadConstellationFile();

    if (!_hasLabels) {
        return;
    }

    _labels->initialize();
    _labels->loadLabels();

    for (speck::Labelset::Entry& entry : _labels->labelSet().entries) {
        if (!entry.identifier.empty()) {
            std::string fullName = constellationFullName(entry.identifier);
            if (!fullName.empty()) {
                entry.text = fullName;
            }
        }
    }
}

bool RenderableConstellationsBase::isReady() const {
    return _hasLabels ? _labels->isReady() : true;
}

void RenderableConstellationsBase::render(const RenderData& data, RendererTasks&) {
    if (!_hasLabels || !_drawLabels) {
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
        glm::vec3(worldToModelTransform * glm::vec4(right, 0.f))
    );

    if (orthoRight == glm::vec3(0.f)) {
        glm::vec3 otherVector(lookup.y, lookup.x, lookup.z);
        right = glm::cross(viewDirection, otherVector);
        orthoRight = glm::normalize(
            glm::vec3(worldToModelTransform * glm::vec4(right, 0.f))
        );
    }

    const glm::vec3 orthoUp = glm::normalize(
        glm::vec3(worldToModelTransform * glm::dvec4(up, 0.f))
    );
    _labels->render(data, modelViewProjectionMatrix, orthoRight, orthoUp);
}

} // namespace openspace
