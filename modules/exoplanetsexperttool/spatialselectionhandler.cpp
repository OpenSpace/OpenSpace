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

void SpatialSelectionHandler::addSavedSelection(std::string name, std::string summary,
                                                std::vector<size_t> indices)
{
    if (name.empty()) {
        name = std::format("Selection {}", _savedSelections.size() + 1);
    }
    _savedSelections.push_back(SavedSelection{
        .name = std::move(name),
        .summary = std::move(summary),
        .indices = std::move(indices),
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

std::vector<size_t> SpatialSelectionHandler::combinedSavedSelections() const {
    std::unordered_set<size_t> combinedSet;
    for (const SavedSelection& sel : _savedSelections) {
        if (sel.isEnabled) {
            for (size_t idx : sel.indices) {
                combinedSet.insert(idx);
            }
        }
    }

    std::vector<size_t> result(combinedSet.begin(), combinedSet.end());
    std::sort(result.begin(), result.end());
    return result;
}

} // namespace openspace::exoplanets
