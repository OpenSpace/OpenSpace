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

#include <modules/exoplanetsexperttool/util/densityvolume.h>

#include <openspace/logging/logmanager.h>
#include <openspace/misc/csvreader.h>
#include <openspace/misc/exception.h>
#include <algorithm>
#include <cmath>
#include <execution>
#include <format>
#include <limits>
#include <numeric>
#include <string>
#include <thread>

namespace {
    constexpr std::string_view _loggerCat = "DensityVolume";

    // 15 / (8 pi) normalizes the 3D Epanechnikov kernel 1 - x^2 over the unit ball
    constexpr double KernelNorm = 15.0 / (8.0 * 3.14159265358979323846);

    // Coordinate value at percentile q in [0, 1] along the given axis
    float percentile(const std::vector<glm::vec3>& positions, int axis, double q) {
        std::vector<float> values;
        values.reserve(positions.size());
        for (const glm::vec3& p : positions) {
            values.push_back(p[axis]);
        }
        const size_t k = static_cast<size_t>(
            std::round(q * static_cast<double>(values.size() - 1))
        );
        std::nth_element(values.begin(), values.begin() + k, values.end());
        return values[k];
    }

    std::vector<size_t> indices(size_t n) {
        std::vector<size_t> result(n);
        std::iota(result.begin(), result.end(), size_t(0));
        return result;
    }
} // namespace

namespace openspace::exoplanets {

DensityVolume::DensityVolume(std::vector<glm::vec3> positions, glm::uvec3 dimensions,
                             std::optional<Bounds> regionOfInterest)
    : _positions(std::move(positions))
    , _dimensions(dimensions)
{
    if (glm::any(glm::lessThan(_dimensions, glm::uvec3(2)))) {
        throw RuntimeError("Grid dimensions must be at least 2 in each direction");
    }

    if (regionOfInterest.has_value()) {
        const glm::vec3 lo = regionOfInterest->first;
        const glm::vec3 hi = regionOfInterest->second;
        std::erase_if(_positions, [&lo, &hi](const glm::vec3& p) {
            return glm::any(glm::lessThan(p, lo)) || glm::any(glm::greaterThan(p, hi));
        });
        _min = lo;
        _max = hi;
    }
    else if (!_positions.empty()) {
        _min = glm::vec3(std::numeric_limits<float>::max());
        _max = glm::vec3(std::numeric_limits<float>::lowest());
        for (const glm::vec3& p : _positions) {
            _min = glm::min(_min, p);
            _max = glm::max(_max, p);
        }
    }

    if (_positions.size() < 2) {
        throw RuntimeError(
            "At least two particles are required to compute a density volume"
        );
    }

    // Avoid zero-size cells for flat or collinear datasets
    const glm::vec3 extent = _max - _min;
    const float largest = std::max({ extent.x, extent.y, extent.z });
    const float pad = largest > 0.f ? 0.01f * largest : 0.5f;
    for (int k = 0; k < 3; k++) {
        if (extent[k] <= 0.f) {
            _min[k] -= pad;
            _max[k] += pad;
        }
    }

    _spacing = (_max - _min) / glm::vec3(_dimensions - glm::uvec3(1));

    compute();
}

DensityVolume DensityVolume::fromCsv(const std::filesystem::path& path,
                                     std::string_view xColumn, std::string_view yColumn,
                                     std::string_view zColumn, glm::uvec3 dimensions,
                                     std::optional<Bounds> regionOfInterest)
{
    const std::vector<std::vector<std::string>> rows = loadCSVFile(path, true);
    if (rows.empty()) {
        throw RuntimeError(
            std::format("CSV file '{}' is empty", path.string()),
            std::string(_loggerCat)
        );
    }

    const std::vector<std::string>& header = rows.front();
    auto columnIndex = [&header, &path](std::string_view name) {
        auto it = std::find(header.begin(), header.end(), name);
        if (it == header.end()) {
            throw RuntimeError(
                std::format(
                    "Column '{}' not found in CSV file '{}'", name, path.string()
                ),
                std::string(_loggerCat)
            );
        }
        return static_cast<size_t>(std::distance(header.begin(), it));
    };
    const size_t xIdx = columnIndex(xColumn);
    const size_t yIdx = columnIndex(yColumn);
    const size_t zIdx = columnIndex(zColumn);
    const size_t minRowSize = std::max({ xIdx, yIdx, zIdx }) + 1;

    std::vector<glm::vec3> positions;
    positions.reserve(rows.size() - 1);
    size_t nSkipped = 0;
    for (size_t i = 1; i < rows.size(); i++) {
        const std::vector<std::string>& row = rows[i];
        if (row.size() < minRowSize) {
            nSkipped++;
            continue;
        }
        try {
            const glm::vec3 p = glm::vec3(
                std::stof(row[xIdx]),
                std::stof(row[yIdx]),
                std::stof(row[zIdx])
            );
            if (!std::isfinite(p.x) || !std::isfinite(p.y) || !std::isfinite(p.z)) {
                nSkipped++;
                continue;
            }
            positions.push_back(p);
        }
        catch (const std::exception&) {
            nSkipped++;
        }
    }

    if (nSkipped > 0) {
        LWARNING(std::format(
            "Skipped {} rows with missing or invalid positions in '{}'",
            nSkipped, path.string()
        ));
    }

    return DensityVolume(std::move(positions), dimensions, std::move(regionOfInterest));
}

void DensityVolume::compute() {
    const size_t n = _positions.size();
    const double nParticles = static_cast<double>(n);

    // Global smoothing lengths: l_k = 2 (P80_k - P20_k) / ln N
    glm::vec3 length;
    for (int k = 0; k < 3; k++) {
        const float spread =
            percentile(_positions, k, 0.8) - percentile(_positions, k, 0.2);
        length[k] = spread > 0.f ?
            static_cast<float>(2.0 * spread / std::log(nParticles)) :
            _spacing[k];
    }

    // Pilot density on the grid
    const float pilotWeight = static_cast<float>(
        KernelNorm / (nParticles * length.x * length.y * length.z)
    );
    const std::vector<float> pilot = splat(
        std::vector<glm::vec3>(n, length),
        std::vector<float>(n, pilotWeight)
    );

    // Pilot density at each particle (parallelized). Each particle only reads the grid
    // and writes to its own output spot, so no conflict can happen.
    std::vector<float> pilotAtParticles(n);
    std::transform(
        std::execution::par,
        _positions.begin(), _positions.end(),
        pilotAtParticles.begin(),
        [this, &pilot](const glm::vec3& p) { return interpolate(pilot, p); }
    );

    // Compute the mean for the grid
    const double mean = std::reduce(
        std::execution::par,
        pilotAtParticles.begin(), pilotAtParticles.end(),
        0.0
    ) / nParticles;

    // Adaptive per-particle smoothing lengths, capped at 10 grid cells.
    // Again, parallelized as each particle only writes to its own output spot (i)
    const glm::vec3 maxLength = 10.f * _spacing;
    std::vector<glm::vec3> lengths(n);
    std::vector<float> weights(n);
    const std::vector<size_t> ids = indices(n);
    std::for_each(
        std::execution::par,
        ids.begin(),
        ids.end(),
        [&](size_t i) {
            const float rho = pilotAtParticles[i];
            const glm::vec3 l = rho > 0.f ?
                glm::min(
                    length * static_cast<float>(std::cbrt(mean / rho)),
                    maxLength
                ) :
                maxLength;
            lengths[i] = l;
            weights[i] = static_cast<float>(KernelNorm / (nParticles * l.x * l.y * l.z));
        }
    );

    // Compute new densities using the adapted smoothing lengths ("splat" the kernels
    // based on the weights)
    _density = splat(lengths, weights);

    const auto [minIt, maxIt] = std::minmax_element(_density.begin(), _density.end());
    _minDensity = *minIt;
    _maxDensity = *maxIt;

    _particleDensities.resize(n);
    std::transform(
        std::execution::par,
        _positions.begin(),
        _positions.end(),
        _particleDensities.begin(),
        [this](const glm::vec3& p) { return interpolate(_density, p); }
    );

    LDEBUG(std::format(
        "Computed density volume ({}x{}x{}) for {} particles, density range [{}, {}]",
        _dimensions.x, _dimensions.y, _dimensions.z, n, _minDensity, _maxDensity
    ));
}

std::vector<float> DensityVolume::splat(const std::vector<glm::vec3>& lengths,
                                        const std::vector<float>& weights) const
{
    const size_t nNodes = static_cast<size_t>(_dimensions.x) * _dimensions.y *
        _dimensions.z;
    const size_t nParticles = _positions.size();

    // Chunked splatting: the points are divided into chunks (up to the number of CPU
    // cores, and at least about 1024 points per chunk).
    // Each chunk accumulates into its own grid to avoid write races between threads
    const size_t nChunks = std::clamp<size_t>(
        std::thread::hardware_concurrency(),
        1,
        std::max<size_t>(nParticles / 1024, 1)
    );
    std::vector<std::vector<float>> partial(nChunks);
    const glm::ivec3 maxNode = glm::ivec3(_dimensions) - glm::ivec3(1);

    const std::vector<size_t> chunks = indices(nChunks);
    std::for_each(
        std::execution::par,
        chunks.begin(), chunks.end(),
        [&](size_t chunk) {
            std::vector<float>& grid = partial[chunk];
            grid.assign(nNodes, 0.f);

            // Determine the range of particles this chunk will process
            const size_t begin = chunk * nParticles / nChunks;
            const size_t end = (chunk + 1) * nParticles / nChunks;

            // Loop over the particles in this chunk and splat them into the grid
            for (size_t i = begin; i < end; i++) {
                const glm::vec3 p = _positions[i];
                const glm::vec3 l = lengths[i]; // Kernel radii along x, y and z
                const float w = weights[i]; // Peak height of the kernel

                // Compute the axis-aligned bounding box of the ellipsoid in grid
                // coordinates. Clamp it to the valid range of node indices
                const glm::ivec3 low = glm::max(
                    glm::ivec3(glm::ceil((p - l - _min) / _spacing)),
                    glm::ivec3(0)
                );
                const glm::ivec3 high = glm::min(
                    glm::ivec3(glm::floor((p + l - _min) / _spacing)),
                    maxNode
                );

                // Add this particle's contribution to every grid node inside its
                // bounding box. The kernel is an ellipsoid (Epanechnikov) centered on
                // the particle, with radii l.x, l.y and l.z. A node at normalized
                // squared distance r2 receives w * (1 - r2), and nodes with r2 >= 1
                // lie outside the ellipsoid and receive nothing.
                for (int z = low.z; z <= high.z; z++) {
                    // Normalized offset from the particle along z (1.0 = ellipsoid edge)
                    const float dz = (_min.z + z * _spacing.z - p.z) / l.z;
                    const float dz2 = dz * dz;
                    // The whole z slice is outside the ellipsoid, so skip it
                    if (dz2 >= 1.f) {
                        continue;
                    }
                    for (int y = low.y; y <= high.y; y++) {
                        const float dy = (_min.y + y * _spacing.y - p.y) / l.y;
                        // Partial squared distance, accumulated over z and y
                        const float dyz2 = dy * dy + dz2;
                        // The whole x row is outside the ellipsoid, so skip it
                        if (dyz2 >= 1.f) {
                            continue;
                        }
                        // Flat array index of the first node (x = 0) in this row. The
                        // grid is stored with x changing fastest, so node x in the row
                        // is at row + x
                        const size_t row = nodeIndex(glm::uvec3(0, y, z));
                        for (int x = low.x; x <= high.x; x++) {
                            const float dx = (_min.x + x * _spacing.x - p.x) / l.x;
                            // Full squared normalized distance to the particle
                            const float r2 = dx * dx + dyz2;
                            if (r2 < 1.f) {
                                grid[row + x] += w * (1.f - r2);
                            }
                        }
                    }
                }
            }
        }
    );

    // Merge the chunks into one
    std::vector<float> result = std::move(partial[0]);
    for (size_t c = 1; c < nChunks; c++) {
        std::transform(
            std::execution::par_unseq,
            result.begin(), result.end(),
            partial[c].begin(),
            result.begin(),
            std::plus<float>()
        );
    }
    return result;
}

float DensityVolume::interpolate(const std::vector<float>& grid,
                                 const glm::vec3& position) const
{
    if (glm::any(glm::lessThan(position, _min)) ||
        glm::any(glm::greaterThan(position, _max)))
    {
        return 0.f;
    }

    const glm::vec3 f = (position - _min) / _spacing;
    const glm::uvec3 i0 = glm::min(glm::uvec3(f), _dimensions - glm::uvec3(2));
    const glm::vec3 t = glm::clamp(f - glm::vec3(i0), 0.f, 1.f);

    auto at = [this, &grid, &i0](unsigned int dx, unsigned int dy, unsigned int dz) {
        return grid[nodeIndex(i0 + glm::uvec3(dx, dy, dz))];
    };

    const float c00 = glm::mix(at(0, 0, 0), at(1, 0, 0), t.x);
    const float c10 = glm::mix(at(0, 1, 0), at(1, 1, 0), t.x);
    const float c01 = glm::mix(at(0, 0, 1), at(1, 0, 1), t.x);
    const float c11 = glm::mix(at(0, 1, 1), at(1, 1, 1), t.x);
    const float c0 = glm::mix(c00, c10, t.y);
    const float c1 = glm::mix(c01, c11, t.y);
    return glm::mix(c0, c1, t.z);
}

float DensityVolume::densityAt(const glm::vec3& position) const {
    return interpolate(_density, position);
}

glm::uvec3 DensityVolume::dimensions() const {
    return _dimensions;
}

glm::vec3 DensityVolume::boundsMin() const {
    return _min;
}

glm::vec3 DensityVolume::boundsMax() const {
    return _max;
}

glm::vec3 DensityVolume::spacing() const {
    return _spacing;
}

const std::vector<float>& DensityVolume::data() const {
    return _density;
}

float DensityVolume::minDensity() const {
    return _minDensity;
}

float DensityVolume::maxDensity() const {
    return _maxDensity;
}

const std::vector<glm::vec3>& DensityVolume::particlePositions() const {
    return _positions;
}

const std::vector<float>& DensityVolume::particleDensities() const {
    return _particleDensities;
}

size_t DensityVolume::nodeIndex(const glm::uvec3& node) const {
    return node.x + static_cast<size_t>(_dimensions.x) *
        (node.y + static_cast<size_t>(_dimensions.y) * node.z);
}

glm::vec3 DensityVolume::nodePosition(const glm::uvec3& node) const {
    return _min + glm::vec3(node) * _spacing;
}

} // namespace openspace::exoplanets
