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

#include <modules/exoplanetsexperttool/rendering/renderablespatialselectionvolume.h>
#include <modules/exoplanetsexperttool/rendering/selectionvolumemesh.h>
#include <modules/exoplanetsexperttool/spatialselection/castselection.h>
#include <modules/exoplanetsexperttool/util/densityvolume.h>
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
    if (!volume.hulls) {
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
        const bool isInside = std::any_of(
            volume.hulls->begin(), volume.hulls->end(),
            [&item](const ConvexHull& hull) { return hull.contains(*item.position); }
        );
        if (isInside) {
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
    const std::vector<glm::vec3>& positions = volume.particlePositions();
    const std::vector<uint8_t>& mask = result.nodeMask;

    // Nothing selected, or the node mask doesn't match the volume's voxel count
    if (result.selectedParticles.empty() || mask.size() != volume.data().size()) {
        return std::nullopt;
    }

    // Build a per-particle lookup table of which particles are selected,
    // rejecting any out-of-range particle index
    std::vector<uint8_t> isSelected(positions.size(), 0);
    for (int i : result.selectedParticles) {
        if (i < 0 || static_cast<size_t>(i) >= positions.size()) {
            return std::nullopt;
        }
        isSelected[i] = 1;
    }

    // Transform from volume space to parsec-scaled world space
    // (volume -> world, then world units -> parsecs)
    const glm::dmat4 volumeToParsec =
        glm::scale(glm::dmat4(1.0), glm::dvec3(1.0 / distanceconstants::Parsec)) *
        volumeToWorld;

    // Split the node mask into connected components, so each separate selected
    // region gets its own hull instead of one hull spanning the gaps between them
    const Components components = labelComponents(mask, volume.dimensions());
    std::vector<ConvexHull> hulls;
    for (size_t label = 0; label < components.sizes.size(); label++) {
        // Find the selected particles that lie within this component
        const std::vector<int> particles = particlesInComponent(
            volume,
            components.labels,
            static_cast<int>(label),
            isSelected
        );
        // Transform this component's particle positions into parsec space
        std::vector<glm::dvec3> points;
        points.reserve(particles.size());
        for (int i : particles) {
            const glm::dvec4 position = glm::dvec4(glm::dvec3(positions[i]), 1.0);
            points.emplace_back(volumeToParsec * position);
        }
        // Compute the convex hull; this can fail for degenerate input (e.g. too few
        // or coplanar points), in which case the component is simply skipped
        std::optional<ConvexHull> hull = computeConvexHull(points);
        if (hull.has_value()) {
            hulls.push_back(std::move(*hull));
        }
    }
    if (hulls.empty()) {
        return std::nullopt;
    }

    return MeshVolume{
        .id = _nextMeshVolumeId++,
        .hulls = std::make_shared<const std::vector<ConvexHull>>(std::move(hulls))
    };
}

std::optional<IsoSurfaceVolume>
SpatialSelectionHandler::createIsoSurfaceVolume(const DensityVolume& volume,
                                                const CastResult& result,
                                                const glm::dmat4& volumeToWorld)
{
    // Grid cell size along each axis; a non-positive spacing makes the
    // world-to-grid conversion meaningless, so bail out early
    const glm::dvec3 spacing = volume.spacing();
    if (glm::any(glm::lessThanEqual(spacing, glm::dvec3(0.0)))) {
        return std::nullopt;
    }

    // Convert each selected particle position into grid (voxel) coordinates
    std::vector<glm::dvec3> points;
    const auto& positions = volume.particlePositions();
    for (int index : result.selectedParticles) {
        // Reject out-of-range particle indices
        if (index < 0 || static_cast<size_t>(index) >= positions.size()) {
            return std::nullopt;
        }
        // Position relative to the volume's minimum corner, in units of grid cells
        glm::dvec3 point = (glm::dvec3(positions[index]) -
            glm::dvec3(volume.boundsMin())) / spacing;

        // Largest valid grid coordinate along each axis
        const glm::dvec3 upper = glm::dvec3(volume.dimensions()) - 1.0;

        // Reject particles outside the grid (small epsilon tolerande for floating
        // point error at the boundaries)
        if (glm::any(glm::lessThan(point, glm::dvec3(-1.e-4))) ||
            glm::any(glm::greaterThan(point, upper + 1.e-4)))
        {
            return std::nullopt;
        }
        // Snap points within the tolerance band back onto the grid
        point = glm::clamp(point, glm::dvec3(0.0), upper);
        points.push_back(point);
    }

    // Generate a smooth scalar field/surface around the selection, using the
    // node mask and the selected points (in grid coordinates)
    std::optional<IsoSurface> field = smoothSelectionSurface(
        volume.dimensions(), result.nodeMask, points
    );
    if (!field) {
        return std::nullopt;
    }

    // Build the transform from the field's grid coordinates to parsec-scaled world space.
    // Applied right to left: scale by cell spacing -> translate to the field origin
    // (volume bounds min + field offset in cells) -> volume-to-world -> world to parsecs
    const glm::dmat4 gridToParsec =
        glm::scale(glm::dmat4(1.0), glm::dvec3(1.0 / distanceconstants::Parsec)) *
        volumeToWorld * glm::translate(glm::dmat4(1.0),
            glm::dvec3(volume.boundsMin()) + glm::dvec3(field->origin) * spacing) *
        glm::scale(glm::dmat4(1.0), spacing);

    // The transform must be invertible and finite, otherwise we can't map back
    const double determinant = glm::determinant(gridToParsec);
    if (!std::isfinite(determinant) || determinant == 0.0) {
        return std::nullopt;
    }

    // Store the inverse transform (parsec -> grid) so that world-space queries
    // can later be evaluated against the grid-based field
    auto geometry = std::make_shared<IsoSurfaceVolume::Geometry>();
    geometry->parsecToGrid = glm::inverse(gridToParsec);
    // Verify every matrix entry is finite after inversion
    for (int column = 0; column < 4; ++column) {
        for (int row = 0; row < 4; ++row) {
            if (!std::isfinite(geometry->parsecToGrid[column][row])) {
                return std::nullopt;
            }
        }
    }

    // Sanity check: every selected point must lie inside the generated surface
    // (points are expressed relative to the field's origin)
    for (const glm::dvec3& point : points) {
        if (!field->contains(point - glm::dvec3(field->origin))) {
            return std::nullopt;
        }
    }

    // Extract the triangle mesh (3 vertices per triangle)
    geometry->triangles = field->triangles();
    if (geometry->triangles.empty()) {
        return std::nullopt;
    }
    // Transform mesh vertices from grid space into parsec-scaled world space
    for (glm::dvec3& vertex : geometry->triangles) {
        vertex = glm::dvec3(gridToParsec * glm::dvec4(vertex, 1.0));
    }

    // A negative determinant means the transform mirrors the geometry, which
    // flips triangle winding; swap two vertices per triangle to restore the
    // original orientation (and thus correct face normals)
    if (determinant < 0.0) {
        for (size_t vertex = 0; vertex < geometry->triangles.size(); vertex += 3) {
            std::swap(geometry->triangles[vertex + 1], geometry->triangles[vertex + 2]);
        }
    }

    // Keep the field itself alongside the mesh
    geometry->field = std::move(*field);

    return IsoSurfaceVolume{
        .id = _nextMeshVolumeId++,
        .geometry = std::move(geometry)
    };
}

std::vector<size_t> SpatialSelectionHandler::selectIsoSurfaceVolume(
    const std::vector<ExoplanetItem>& data, const std::vector<size_t>& candidates,
    const IsoSurfaceVolume& volume) const
{
    std::vector<size_t> result;
    if (!volume.geometry) {
        return result;
    }
    for (size_t index : candidates) {
        if (index >= data.size() || !data[index].position) {
            continue;
        }
        const glm::dvec3 point(volume.geometry->parsecToGrid *
            glm::dvec4(*data[index].position, 1.0));
        if (volume.geometry->field.contains(point)) {
            result.push_back(index);
        }
    }
    return result;
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
    if (const IsoSurfaceVolume* volume = std::get_if<IsoSurfaceVolume>(&query)) {
        return selectIsoSurfaceVolume(data, candidates, *volume);
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
        else if (const IsoSurfaceVolume* volume = std::get_if<IsoSurfaceVolume>(query)) {
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
        if (std::holds_alternative<MeshVolume>(*query) ||
            std::holds_alternative<IsoSurfaceVolume>(*query))
        {
            SceneGraphNode* node = sceneGraphNode(SelectionVolumeIdentifier);
            auto* renderable = node ?
                dynamic_cast<RenderableSpatialSelectionVolume*>(node->renderable()) :
                nullptr;
            if (!renderable) {
                return;
            }
            renderable->setCustomMesh(selectionVolumeMesh(*query));
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
