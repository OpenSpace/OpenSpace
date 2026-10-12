/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <modules/exoplanetsexperttool/spatialselectionhandler.h>

#include <modules/exoplanetsexperttool/castselection/castselection.h>
#include <modules/exoplanetsexperttool/densityvolume.h>
#include <modules/exoplanetsexperttool/rendering/renderablespatialselectionvolume.h>
#include <modules/exoplanetsexperttool/rendering/selectionvolumemesh.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/misc/dictionary.h>
#include <openspace/misc/dictionaryluaformatter.h>
#include <openspace/query/query.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/util/distanceconstants.h>
#include <algorithm>
#include <cmath>
#include <format>
#include <unordered_set>

namespace {
    bool isPointInsideSphere(const glm::dvec3& point, const glm::dvec3& center,
                             double radius)
    {
        const glm::dvec3 diff = point - center;
        return glm::dot(diff, diff) <= (radius * radius);
    }

    bool isPointInsideBox(const glm::dvec3& point, const glm::dvec3& center,
                          const glm::dvec3& halfDim)
    {
        const bool isInX = std::abs(point.x - center.x) <= halfDim.x;
        const bool isInY = std::abs(point.y - center.y) <= halfDim.y;
        const bool isInZ = std::abs(point.z - center.z) <= halfDim.z;
        return isInX && isInY && isInZ;
    }

    bool isRaInRange(double ra, double minRa, double maxRa) {
        // Handle wrap-around when minRa > maxRa (e.g. 350 to 10 degrees)
        if (minRa <= maxRa) {
            return (ra >= minRa) && (ra <= maxRa);
        }
        else {
            return (ra >= minRa) || (ra <= maxRa);
        }
    }
} // namespace

namespace openspace::exoplanets {

std::vector<size_t> SpatialSelectionHandler::selectSphere(
                                            const std::vector<ExoplanetItem>& data,
                                            const std::vector<size_t>& candidates,
                                            const SphereVolume& sphere) const
{
    std::vector<size_t> result;
    result.reserve(candidates.size());

    for (size_t idx : candidates) {
        if (idx >= data.size()) {
            continue;
        }
        const ExoplanetItem& item = data[idx];
        if (item.position.has_value() &&
            isPointInsideSphere(*item.position, sphere.center, sphere.radius))
        {
            result.push_back(idx);
        }
    }

    return result;
}

std::vector<size_t> SpatialSelectionHandler::selectBox(
                                            const std::vector<ExoplanetItem>& data,
                                            const std::vector<size_t>& candidates,
                                            const BoxVolume& box) const
{
    std::vector<size_t> result;
    result.reserve(candidates.size());

    const glm::dvec3 halfDim = glm::abs(box.dimensions) * 0.5;

    for (size_t idx : candidates) {
        if (idx >= data.size()) {
            continue;
        }
        const ExoplanetItem& item = data[idx];
        if (item.position.has_value() &&
            isPointInsideBox(*item.position, box.center, halfDim))
        {
            result.push_back(idx);
        }
    }

    return result;
}

std::vector<size_t> SpatialSelectionHandler::selectSkyRect(
                                            const std::vector<ExoplanetItem>& data,
                                            const std::vector<size_t>& candidates,
                                            const SkyMapRect& rect,
                                            const DataSettings::DataMapping& mapping) const
{
    std::vector<size_t> result;
    result.reserve(candidates.size());

    const double decMin = std::min(rect.decMin, rect.decMax);
    const double decMax = std::max(rect.decMin, rect.decMax);

    for (size_t idx : candidates) {
        if (idx >= data.size()) {
            continue;
        }
        const ExoplanetItem& item = data[idx];

        const bool hasRaDecColumns = !mapping.positionRa.empty() &&
            !mapping.positionDec.empty() &&
            item.dataColumns.contains(mapping.positionRa) &&
            item.dataColumns.contains(mapping.positionDec);

        if (!hasRaDecColumns) {
            continue;
        }

        const std::variant<std::string, float>& raVal =
            item.dataColumns.at(mapping.positionRa);
        const std::variant<std::string, float>& decVal =
            item.dataColumns.at(mapping.positionDec);

        const bool areNumericValues = std::holds_alternative<float>(raVal) &&
            std::holds_alternative<float>(decVal);

        if (!areNumericValues) {
            continue;
        }

        const double ra = static_cast<double>(std::get<float>(raVal));
        const double dec = static_cast<double>(std::get<float>(decVal));

        const bool isInDecRange = (dec >= decMin) && (dec <= decMax);
        const bool isInAngularArea = isRaInRange(ra, rect.raMin, rect.raMax) &&
            isInDecRange;

        if (!isInAngularArea) {
            continue;
        }

        if (rect.useDistanceFilter) {
            const bool hasDistColumn = !mapping.positionDistance.empty() &&
                item.dataColumns.contains(mapping.positionDistance);
            if (hasDistColumn) {
                const std::variant<std::string, float>& distVal =
                    item.dataColumns.at(mapping.positionDistance);
                if (std::holds_alternative<float>(distVal)) {
                    const double dist = static_cast<double>(std::get<float>(distVal));
                    if ((dist < rect.distMin) || (dist > rect.distMax)) {
                        continue;
                    }
                }
            }
        }

        result.push_back(idx);
    }

    return result;
}

std::vector<size_t> SpatialSelectionHandler::selectDensity(
                                    const std::vector<ExoplanetItem>&,
                                    const std::vector<size_t>&,
                                    const DensitySelectionSettings&) const
{
    // @TODO: Implement density-based selection using spatial partitioning for efficiency.
    // Use cast-based methods from Tobias and Liyun
    // Density-based selection is disabled for now pending spatial partitioning
    // optimization to avoid O(N^2) search on the render thread.
    return {};
}

std::vector<size_t> SpatialSelectionHandler::selectMeshVolume(
                                            const std::vector<ExoplanetItem>& data,
                                            const std::vector<size_t>& candidates,
                                            const MeshVolume& volume) const
{
    std::vector<size_t> result;
    if (!volume.grid) {
        return result;
    }
    result.reserve(candidates.size());

    for (size_t idx : candidates) {
        if (idx >= data.size()) {
            continue;
        }
        const ExoplanetItem& item = data[idx];
        if (!item.position.has_value()) {
            continue;
        }
        const glm::dvec3 gridPos =
            glm::dvec3(volume.parsecToGrid * glm::dvec4(*item.position, 1.0));
        if (volume.grid->interpolate(gridPos) >= volume.isoValue) {
            result.push_back(idx);
        }
    }

    return result;
}

std::optional<MeshVolume> SpatialSelectionHandler::createMeshVolume(
                                                       const DensityVolume& volume,
                                                       const CastResult& result,
                                                       const glm::dmat4& volumeToWorld)
{
    const glm::uvec3 dims = volume.dimensions();
    const std::vector<float>& density = volume.data();
    const std::vector<uint8_t>& mask = result.nodeMask;
    // The interpolated field is 0 outside the mask, so a zero threshold would select all
    if (mask.size() != density.size() || !(result.threshold > 0.f)) {
        return std::nullopt;
    }

    glm::ivec3 low = glm::ivec3(dims);
    glm::ivec3 high = glm::ivec3(-1);
    for (unsigned int z = 0; z < dims.z; z++) {
        for (unsigned int y = 0; y < dims.y; y++) {
            for (unsigned int x = 0; x < dims.x; x++) {
                if (mask[volume.nodeIndex(glm::uvec3(x, y, z))]) {
                    const glm::ivec3 node = glm::ivec3(x, y, z);
                    low = glm::min(low, node);
                    high = glm::max(high, node);
                }
            }
        }
    }
    if (high.x < 0) {
        return std::nullopt;
    }

    // Crop to the selected nodes with one layer of zeros around them to close the surface
    const glm::ivec3 origin = low - glm::ivec3(1);
    auto grid = std::make_shared<VolumeGrid>();
    grid->dimensions = glm::uvec3(high - low + glm::ivec3(3));
    grid->values.resize(
        static_cast<size_t>(grid->dimensions.x) * grid->dimensions.y *
        grid->dimensions.z,
        0.f
    );
    for (unsigned int z = 0; z < grid->dimensions.z; z++) {
        for (unsigned int y = 0; y < grid->dimensions.y; y++) {
            for (unsigned int x = 0; x < grid->dimensions.x; x++) {
                const glm::ivec3 node = origin + glm::ivec3(x, y, z);
                const bool isInVolume =
                    glm::all(glm::greaterThanEqual(node, glm::ivec3(0))) &&
                    glm::all(glm::lessThan(node, glm::ivec3(dims)));
                if (!isInVolume) {
                    continue;
                }
                const size_t src = volume.nodeIndex(glm::uvec3(node));
                if (mask[src]) {
                    grid->values[x + static_cast<size_t>(grid->dimensions.x) *
                        (y + static_cast<size_t>(grid->dimensions.y) * z)] =
                        density[src];
                }
            }
        }
    }

    const glm::dvec3 spacing = glm::dvec3(volume.spacing());
    const glm::dmat4 gridToVolume =
        glm::translate(
            glm::dmat4(1.0),
            glm::dvec3(volume.boundsMin()) + glm::dvec3(origin) * spacing
        ) *
        glm::scale(glm::dmat4(1.0), spacing);
    const glm::dmat4 gridToParsec =
        glm::scale(glm::dmat4(1.0), glm::dvec3(1.0 / distanceconstants::Parsec)) *
        volumeToWorld * gridToVolume;

    return MeshVolume{
        .id = _nextMeshVolumeId++,
        .grid = std::move(grid),
        .isoValue = result.threshold,
        .gridToParsec = gridToParsec,
        .parsecToGrid = glm::inverse(gridToParsec),
        .nComponents = result.nComponents
    };
}

std::vector<size_t> SpatialSelectionHandler::select(
                                      const std::vector<ExoplanetItem>& data,
                                      const std::vector<size_t>& candidates,
                                      const SpatialSelectionQuery& query,
                                      const DataSettings::DataMapping& mapping) const
{
    if (const SphereVolume* sphere = std::get_if<SphereVolume>(&query)) {
        return selectSphere(data, candidates, *sphere);
    }
    if (const BoxVolume* box = std::get_if<BoxVolume>(&query)) {
        return selectBox(data, candidates, *box);
    }
    if (const SkyMapRect* rect = std::get_if<SkyMapRect>(&query)) {
        return selectSkyRect(data, candidates, *rect, mapping);
    }
    if (const MeshVolume* volume = std::get_if<MeshVolume>(&query)) {
        return selectMeshVolume(data, candidates, *volume);
    }
    return selectDensity(
        data,
        candidates,
        std::get<DensitySelectionSettings>(query)
    );
}

void SpatialSelectionHandler::addSavedSelection(std::string name, std::string summary,
                                                SpatialSelectionQuery query)
{
    if (name.empty()) {
        name = std::format("Selection {}", _savedSelections.size() + 1);
    }
    _savedSelections.push_back(SavedSelection{
        .id = _nextSavedSelectionId++,
        .name = std::move(name),
        .summary = std::move(summary),
        .query = std::move(query),
        .isEnabled = true
    });
}

void SpatialSelectionHandler::removeSavedSelection(size_t index) {
    if (index < _savedSelections.size()) {
        _savedSelections.erase(_savedSelections.begin() + index);
    }
}

void SpatialSelectionHandler::clearSavedSelections() {
    _savedSelections.clear();
}

std::vector<SavedSelection>& SpatialSelectionHandler::savedSelections() {
    return _savedSelections;
}

const std::vector<SavedSelection>& SpatialSelectionHandler::savedSelections() const {
    return _savedSelections;
}

std::vector<size_t> SpatialSelectionHandler::combinedSavedSelections(
                                      const std::vector<ExoplanetItem>& data,
                                      const std::vector<size_t>& candidates,
                                      const DataSettings::DataMapping& mapping) const
{
    std::unordered_set<size_t> combinedSet;
    for (const SavedSelection& sel : _savedSelections) {
        if (sel.isEnabled) {
            const std::vector<size_t> indices = select(
                data,
                candidates,
                sel.query,
                mapping
            );
            for (size_t idx : indices) {
                combinedSet.insert(idx);
            }
        }
    }

    std::vector<size_t> result(combinedSet.begin(), combinedSet.end());
    std::sort(result.begin(), result.end());
    return result;
}


void SpatialSelectionHandler::initializeRenderables() {
    using namespace std::string_literals;

    Dictionary selectionGui;
    selectionGui.setValue("Name", "Spatial Selection"s);
    selectionGui.setValue("Path", "/ExoplanetExplorer"s);

    Dictionary selectionRenderable;
    selectionRenderable.setValue("Type", "RenderableSpatialSelectionVolume"s);
    selectionRenderable.setValue("Enabled", false);
    selectionRenderable.setValue("Opacity", 0.08);
    selectionRenderable.setValue("RenderBinMode", "PostDeferredTransparent"s);

    Dictionary selectionNode;
    selectionNode.setValue("Identifier", std::string(SelectionVolumeIdentifier));
    selectionNode.setValue("Renderable", selectionRenderable);
    selectionNode.setValue("GUI", selectionGui);
    global::scriptEngine->queueScript(std::format(
        "if not openspace.hasSceneGraphNode('{}') then "
        "openspace.addSceneGraphNode({}) end",
        SelectionVolumeIdentifier, formatLua(selectionNode)
    ));

    _selectionVolumeIsInitialized = true;
    _selectionVolumeIsVisible = false;
    _selectionVolumeParameters.clear();
}

void SpatialSelectionHandler::hideSelectionVolume() {
    updateSelectionVolume(nullptr);
    _selectionVolumeParameters.clear();
}

void SpatialSelectionHandler::updateSelectionVolume(const SpatialSelectionQuery* query) {
    if (!_selectionVolumeIsInitialized || !global::windowDelegate->isMaster()) {
        return;
    }
    std::vector<double> parameters;
    if (query) {
        if (const SphereVolume* sphere = std::get_if<SphereVolume>(query)) {
            parameters = {
                0.0, sphere->center.x, sphere->center.y, sphere->center.z, sphere->radius
            };
        }
        else if (const BoxVolume* box = std::get_if<BoxVolume>(query)) {
            parameters = {
                1.0, box->center.x, box->center.y, box->center.z,
                box->dimensions.x, box->dimensions.y, box->dimensions.z
            };
        }
        else if (const SkyMapRect* sky = std::get_if<SkyMapRect>(query)) {
            parameters = {
                2.0, sky->raMin, sky->raMax, sky->decMin, sky->decMax,
                sky->useDistanceFilter ? 1.0 : 0.0,
                sky->useDistanceFilter ? sky->distMin : 0.0, sky->distMax
            };
        }
        else if (const MeshVolume* volume = std::get_if<MeshVolume>(query)) {
            parameters = { 3.0, static_cast<double>(volume->id) };
        }
    }
    for (double value : parameters) {
        if (!std::isfinite(value)) {
            parameters.clear();
            break;
        }
    }
    const bool isVisible = !parameters.empty();
    if (isVisible == _selectionVolumeIsVisible &&
        (!isVisible || parameters == _selectionVolumeParameters))
    {
        return;
    }
    std::string script = std::format(
        "if openspace.hasSceneGraphNode('{}') then ", SelectionVolumeIdentifier
    );
    const std::string path = std::format(
        "Scene.{}.Renderable", SelectionVolumeIdentifier
    );
    if (isVisible && parameters != _selectionVolumeParameters) {
        // The mesh is too large to pass through a property, so hand it over directly
        if (const MeshVolume* volume = std::get_if<MeshVolume>(query)) {
            SceneGraphNode* node = sceneGraphNode(SelectionVolumeIdentifier);
            auto* renderable = node ?
                dynamic_cast<RenderableSpatialSelectionVolume*>(node->renderable()) :
                nullptr;
            if (renderable) {
                renderable->setCustomMesh(selectionVolumeMesh(*volume));
            }
        }

        std::string values;
        for (double value : parameters) {
            if (!values.empty()) {
                values += ',';
            }
            values += std::format("{:.17g}", value);
        }
        script += std::format(
            "openspace.setPropertyValueSingle('{}.ShapeParameters', {{ {} }}); ",
            path, values
        );
    }
    script += std::format(
        "openspace.setPropertyValueSingle('{}.Enabled', {}) end", path, isVisible
    );
    global::scriptEngine->queueScript(
        std::move(script),
        ScriptEngine::ShouldBeLogged::No
    );
    _selectionVolumeIsVisible = isVisible;
    if (isVisible) {
        _selectionVolumeParameters = std::move(parameters);
    }
}

} // namespace openspace::exoplanets
