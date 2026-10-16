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

#include <modules/exoplanetsexperttool/spatialselection/castselection.h>

#include <modules/exoplanetsexperttool/spatialselection/spacecast.h>
#include <modules/exoplanetsexperttool/util/densityvolume.h>
#include <algorithm>
#include <cmath>
#include <execution>
#include <numeric>

namespace openspace::exoplanets {

float ScreenProjection::project(const glm::vec3& p, glm::vec2& screen) const {
    const glm::dvec4 clip = modelToClip * glm::dvec4(glm::dvec3(p), 1.0);
    if (clip.w > 0.0) {
        const glm::dvec2 ndc = glm::dvec2(clip) / clip.w;
        screen = glm::vec2(
            static_cast<float>((ndc.x * 0.5 + 0.5) * viewportSize.x),
            static_cast<float>((0.5 - ndc.y * 0.5) * viewportSize.y)
        );
    }
    return static_cast<float>(clip.w);
}

float ScreenProjection::cellFootprint(const glm::vec3& spacing) const {
    // Pixel extent of each cell axis at unit depth; the largest one bounds the footprint
    float footprint = 0.f;
    for (int k = 0; k < 3; k++) {
        const glm::dvec2 axis = glm::dvec2(
            modelToClip[k][0] * 0.5 * viewportSize.x,
            modelToClip[k][1] * 0.5 * viewportSize.y
        ) * static_cast<double>(spacing[k]);
        footprint = std::max(footprint, static_cast<float>(glm::length(axis)));
    }
    return footprint;
}

CastSelection::CastSelection(const DensityVolume& volume)
    : _volume(volume)
{}

const DensityVolume& CastSelection::volume() const {
    return _volume;
}

bool CastSelection::isValid() const {
    return _isValid;
}

float CastSelection::baseThreshold() const {
    return _baseThreshold;
}

float CastSelection::threshold(float exponent) const {
    return std::exp2(exponent) * _baseThreshold;
}

std::unique_ptr<CastSelection> createCastSelection(CastMethod method,
                                                   const DensityVolume& volume,
                                                   const CastInput& input)
{
    switch (method) {
        case CastMethod::SpaceCast:
            return std::make_unique<SpaceCast>(volume, input);
        case CastMethod::TraceCast:
        case CastMethod::PointCast:
            return nullptr;
    }
    return nullptr;
}

CastResult selectDensityVolume(const DensityVolume& volume, float exponent) {
    CastResult result;
    const std::vector<float>& density = volume.data();
    result.nodeMask.resize(density.size(), 0);
    if (density.empty()) {
        return result;
    }

    const double mean = std::accumulate(density.begin(), density.end(), 0.0) /
        static_cast<double>(density.size());
    result.threshold = std::exp2(exponent) * static_cast<float>(0.2 * mean);
    if (!std::isfinite(result.threshold) || result.threshold <= 0.f) {
        return result;
    }

    for (size_t node = 0; node < density.size(); node++) {
        result.nodeMask[node] = density[node] >= result.threshold;
    }
    Components components = labelComponents(result.nodeMask, volume.dimensions());
    result.nComponents = components.sizes.size();
    if (result.nComponents == 0) {
        return result;
    }
    for (int& label : components.labels) {
        if (label >= 0) {
            label = 0;
        }
    }

    const std::vector<float>& particleDensities = volume.particleDensities();
    std::vector<uint8_t> candidates(particleDensities.size());
    for (size_t particle = 0; particle < candidates.size(); particle++) {
        candidates[particle] = particleDensities[particle] >= result.threshold;
    }
    result.selectedParticles = particlesInComponent(
        volume, components.labels, 0, candidates
    );
    return result;
}

Components labelComponents(const std::vector<uint8_t>& mask, const glm::uvec3& dims) {
    Components result;
    result.labels.assign(mask.size(), -1);

    const size_t strideY = dims.x;
    const size_t strideZ = static_cast<size_t>(dims.x) * dims.y;

    std::vector<size_t> stack;
    for (size_t seed = 0; seed < mask.size(); seed++) {
        if (!mask[seed] || result.labels[seed] != -1) {
            continue;
        }

        const int label = static_cast<int>(result.sizes.size());
        size_t size = 0;
        result.labels[seed] = label;
        stack.push_back(seed);

        while (!stack.empty()) {
            const size_t i = stack.back();
            stack.pop_back();
            size++;

            const size_t x = i % dims.x;
            const size_t y = (i / strideY) % dims.y;
            const size_t z = i / strideZ;

            auto visit = [&](size_t neighbor) {
                if (mask[neighbor] && result.labels[neighbor] == -1) {
                    result.labels[neighbor] = label;
                    stack.push_back(neighbor);
                }
            };
            if (x > 0)          { visit(i - 1); }
            if (x + 1 < dims.x) { visit(i + 1); }
            if (y > 0)          { visit(i - strideY); }
            if (y + 1 < dims.y) { visit(i + strideY); }
            if (z > 0)          { visit(i - strideZ); }
            if (z + 1 < dims.z) { visit(i + strideZ); }
        }

        result.sizes.push_back(size);
    }

    return result;
}

std::vector<int> particlesInComponent(const DensityVolume& volume,
                                      const std::vector<int>& labels, int label,
                                      const std::vector<uint8_t>& candidates)
{
    const std::vector<glm::vec3>& positions = volume.particlePositions();
    const glm::vec3 volumeMin = volume.boundsMin();
    const glm::vec3 spacing = volume.spacing();
    const glm::ivec3 maxCell = glm::ivec3(volume.dimensions()) - glm::ivec3(2);

    std::vector<uint8_t> selected(positions.size(), 0);
    const std::vector<size_t> particles = indexSequence(positions.size());
    std::for_each(
        std::execution::par,
        particles.begin(), particles.end(),
        [&](size_t i) {
            if (!candidates[i]) {
                return;
            }
            const glm::ivec3 cell = glm::clamp(
                glm::ivec3(glm::floor((positions[i] - volumeMin) / spacing)),
                glm::ivec3(0),
                maxCell
            );
            for (int c = 0; c < 8; c++) {
                const glm::ivec3 corner = cell + glm::ivec3(c & 1, (c >> 1) & 1, c >> 2);
                if (labels[volume.nodeIndex(glm::uvec3(corner))] == label) {
                    selected[i] = 1;
                    return;
                }
            }
        }
    );

    std::vector<int> result;
    for (size_t i = 0; i < selected.size(); i++) {
        if (selected[i]) {
            result.push_back(static_cast<int>(i));
        }
    }
    return result;
}

std::vector<size_t> indexSequence(size_t n) {
    std::vector<size_t> result(n);
    std::iota(result.begin(), result.end(), size_t(0));
    return result;
}

} // namespace openspace::exoplanets
