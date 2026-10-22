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

#include <modules/exoplanetsexperttool/spatialselection/spacecast.h>

#include <modules/exoplanetsexperttool/util/densityvolume.h>
#include <algorithm>
#include <cmath>
#include <execution>
#include <numeric>

namespace {
    // Resolution of the raster used to measure projected areas, along its longest side
    constexpr float AreaRasterResolution = 256.f;

    // Caps the cost of splatting voxels that are very close to the camera
    constexpr float MaxSplatRadius = 8.f;
} // namespace

namespace openspace::exoplanets {

SpaceCast::SpaceCast(const DensityVolume& volume, const CastInput& input)
    : CastSelection(volume)
    , _lasso(input.stroke)
{
    if (!_lasso.isValid() || _lasso.diameter() <= 0.f) {
        return;
    }

    const ScreenProjection& projection = input.projection;
    const glm::uvec3 dims = _volume.dimensions();
    const size_t nVoxels = static_cast<size_t>(dims.x * dims.y * dims.z);
    const glm::vec2 lowBound = _lasso.boundsMin();
    const glm::vec2 highBound = _lasso.boundsMax();
    const float diameter = _lasso.diameter();

    _cellScreen.resize(nVoxels);
    _cellDepth.resize(nVoxels);
    _cellDelta.resize(nVoxels);
    const std::vector<size_t> voxels = indexSequence(nVoxels);

    // Project all grid voxels into screen space and compute their signed distance to the
    // lasso. Do this in parallel for all voxels
    std::for_each(
        std::execution::par,
        voxels.begin(), voxels.end(),
        [&](size_t i) {
            // Convert the linear index i into 3D grid coordinates (cell)
            const glm::uvec3 cellCoords = glm::uvec3(
                i % dims.x,
                (i / dims.x) % dims.y,
                i / (static_cast<size_t>(dims.x) * dims.y)
            );
            glm::vec2 screenPos = glm::vec2(0.f);
            const float w = projection.project(_volume.cellPosition(cellCoords), screenPos);
            _cellScreen[i] = screenPos;
            _cellDepth[i] = w;

            if (w <= 0.f) {
                _cellDelta[i] = -1.f;
            }
            else if (glm::any(glm::lessThan(screenPos, lowBound)) ||
                     glm::any(glm::greaterThan(screenPos, highBound)))
            {
                // Distance to the bounding box underestimates the magnitude, but the
                // sign is exact and the full test is too expensive for every node
                const float d = glm::distance(
                    screenPos,
                    glm::clamp(screenPos, lowBound, highBound)
                );
                _cellDelta[i] = -std::max(d, 1e-3f) / diameter;
            }
            else {
                _cellDelta[i] = _lasso.signedDistance(screenPos) / diameter;
            }
        }
    );

    const std::vector<float>& density = _volume.data();
    double sum = 0.0;
    double sumSquared = 0.0;
    size_t nInside = 0;
    for (size_t i = 0; i < nVoxels; i++) {
        if (_cellDelta[i] > 0.f) {
            sum += density[i];
            sumSquared += static_cast<double>(density[i]) * density[i];
            nInside++;
        }
    }
    if (nInside == 0) {
        return;
    }

    // Compute the mean and standard deviation of the density inside the lasso frustum.
    // Use the mean for the base threshold, and the standard deviation will be used later
    // to scale the density
    const double mean = sum / static_cast<double>(nInside);
    const double variance = sumSquared / static_cast<double>(nInside) - mean * mean;
    _baseThreshold = static_cast<float>(0.2 * mean);
    _densityStdDev = variance > 0.0 ? static_cast<float>(std::sqrt(variance)) : 1.f;

    const std::vector<glm::vec3>& positions = _volume.particlePositions();
    _particleInside.resize(positions.size());
    const std::vector<size_t> particles = indexSequence(positions.size());
    std::for_each(
        std::execution::par,
        particles.begin(), particles.end(),
        [&](size_t i) {
            glm::vec2 screenPos = glm::vec2(0.f);
            const float w = projection.project(positions[i], screenPos);
            _particleInside[i] = w > 0.f &&
                glm::all(glm::greaterThanEqual(screenPos, lowBound)) &&
                glm::all(glm::lessThanEqual(screenPos, highBound)) &&
                _lasso.contains(screenPos);
        }
    );

    _cellFootprint = projection.cellFootprint(_volume.spacing());
    _isValid = true;
}

CastResult SpaceCast::select(float exponent) const {
    CastResult result;
    if (!_isValid) {
        return result;
    }

    const float rho = threshold(exponent);
    result.threshold = rho;

    const glm::uvec3 dims = _volume.dimensions();
    const std::vector<float>& density = _volume.data();
    const size_t nVoxels = density.size();

    std::vector<uint8_t> mask(nVoxels);
    for (size_t i = 0; i < nVoxels; i++) {
        const float f = std::min((density[i] - rho) / _densityStdDev, _cellDelta[i]);
        mask[i] = f >= 0.f;
    }

    // @TODO Check this component computation. Do we want to keep it? Does it work correctly?
    const Components components = labelComponents(mask, dims);
    const size_t nComponents = components.sizes.size();
    result.nComponents = nComponents;
    if (nComponents == 0) {
        return result;
    }

    // Group the node indices by component
    std::vector<size_t> offsets(nComponents + 1, 0);
    for (int label : components.labels) {
        if (label >= 0) {
            offsets[label + 1]++;
        }
    }
    std::partial_sum(offsets.begin(), offsets.end(), offsets.begin());
    std::vector<size_t> ordered(offsets.back());
    {
        std::vector<size_t> cursor(offsets.begin(), offsets.end() - 1);
        for (size_t i = 0; i < nVoxels; i++) {
            const int label = components.labels[i];
            if (label >= 0) {
                ordered[cursor[label]++] = i;
            }
        }
    }

    // Raster over the lasso's bounding box used to measure the projected areas
    const glm::vec2 lowBound = _lasso.boundsMin();
    const glm::vec2 highBound = _lasso.boundsMax();
    const float cellSize = std::max(
        2.f,
        std::max(highBound.x - lowBound.x, highBound.y - lowBound.y) /
            AreaRasterResolution
    );
    const glm::ivec2 res =
        glm::ivec2(glm::ceil((highBound - lowBound) / cellSize)) + glm::ivec2(1);

    std::vector<uint8_t> lassoCells(static_cast<size_t>(res.x * res.y));

    for (int y = 0; y < res.y; y++) {
        for (int x = 0; x < res.x; x++) {
            const glm::vec2 center = lowBound + (glm::vec2(x, y) + 0.5f) * cellSize;
            lassoCells[y * res.x + x] = _lasso.contains(center);
        }
    }

    std::vector<size_t> overlap(nComponents, 0);
    std::vector<double> depth(nComponents, 0.0);
    std::vector<int> stamp(lassoCells.size(), -1);
    for (size_t k = 0; k < nComponents; k++) {
        const int label = static_cast<int>(k);
        double depthSum = 0.0;
        for (size_t n = offsets[k]; n < offsets[k + 1]; n++) {
            const size_t i = ordered[n];
            depthSum += _cellDepth[i];

            const float radius = std::min(
                0.5f * _cellFootprint / _cellDepth[i] / cellSize,
                MaxSplatRadius
            );
            const glm::vec2 c = (_cellScreen[i] - lowBound) / cellSize;
            const int x0 = std::max(static_cast<int>(std::floor(c.x - radius)), 0);
            const int x1 = std::min(static_cast<int>(std::floor(c.x + radius)), res.x - 1);
            const int y0 = std::max(static_cast<int>(std::floor(c.y - radius)), 0);
            const int y1 = std::min(static_cast<int>(std::floor(c.y + radius)), res.y - 1);
            for (int y = y0; y <= y1; y++) {
                for (int x = x0; x <= x1; x++) {
                    const size_t cell = static_cast<size_t>(y * res.x + x);
                    if (stamp[cell] != label) {
                        stamp[cell] = label;
                        overlap[k] += lassoCells[cell];
                    }
                }
            }
        }
        depth[k] = depthSum / static_cast<double>(components.sizes[k]);
    }

    const size_t best = static_cast<size_t>(std::distance(
        overlap.begin(),
        std::max_element(overlap.begin(), overlap.end())
    ));

    // Prefer a closer volume with a similar overlap, e.g. a small cluster in front of a
    // larger one that fills the whole lasso
    size_t chosen = best;
    for (size_t k = 0; k < nComponents; k++) {
        const bool similar =
            static_cast<double>(overlap[k]) > 0.8 * static_cast<double>(overlap[best]);
        if (similar && depth[k] < depth[chosen]) {
            chosen = k;
        }
    }
    const int chosenLabel = static_cast<int>(chosen);

    result.selectedVoxelMask.resize(nVoxels);
    for (size_t i = 0; i < nVoxels; i++) {
        result.selectedVoxelMask[i] = components.labels[i] == chosenLabel;
    }

    // A particle's own f is >= 0 if it passes the threshold and projects inside the lasso
    const std::vector<float>& particleDensities = _volume.particleDensities();
    std::vector<uint8_t> candidates(particleDensities.size());
    for (size_t i = 0; i < candidates.size(); i++) {
        candidates[i] = _particleInside[i] && particleDensities[i] >= rho;
    }
    result.selectedParticles = particlesInComponent(
        _volume,
        components.labels,
        chosenLabel,
        candidates
    );

    return result;
}

} // namespace openspace::exoplanets
