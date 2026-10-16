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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DENSITYVOLUME___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DENSITYVOLUME___H__

#include <openspace/glm.h>
#include <filesystem>
#include <optional>
#include <string_view>
#include <utility>
#include <vector>

namespace openspace::exoplanets {

/**
 * Scalar density field of a particle cloud, sampled on a uniform grid using the modified
 * Breiman kernel density estimator (MBE) with an adaptive Epanechnikov kernel, as
 * described in Yu et al., "CAST: Effective and Efficient User Interaction for
 * Context-Aware Selection in 3D Particle Clouds".
 *
 * The grid covers the axis-aligned bounding box of the particles (or the provided region
 * of interest). Grid data is stored with x varying fastest, then y, then z.
 */
class DensityVolume {
public:
    using Bounds = std::pair<glm::vec3, glm::vec3>;

    /**
     * Computes the density volume for the provided \p positions. Particles outside the
     * \p regionOfInterest, if provided, are discarded.
     *
     * \throw RuntimeError If fewer than two particles remain or a dimension is < 2
     */
    explicit DensityVolume(std::vector<glm::vec3> positions,
        glm::uvec3 dimensions = glm::uvec3(128),
        std::optional<Bounds> regionOfInterest = std::nullopt);

    /**
     * Loads positions from a CSV or SPECK file at \p path using the shared dataloader
     * and computes the density volume. For CSV, \p xColumn, \p yColumn, and \p zColumn
     * identify the case-sensitive header names. For SPECK, the first three values are
     * the position coordinates and the column names are ignored. Entries with
     * non-finite positions are skipped; other parsing follows the shared dataloader.
     *
     * \throw RuntimeError If the file cannot be read, fewer than two particles remain,
     *                     or a dimension is < 2
     */
    static DensityVolume fromFile(const std::filesystem::path& path,
        std::string_view xColumn = "x", std::string_view yColumn = "y",
        std::string_view zColumn = "z",
        glm::uvec3 dimensions = glm::uvec3(128),
        std::optional<Bounds> regionOfInterest = std::nullopt);

    glm::uvec3 dimensions() const;
    glm::vec3 boundsMin() const;
    glm::vec3 boundsMax() const;
    glm::vec3 spacing() const;

    const std::vector<float>& data() const;
    float minDensity() const;
    float maxDensity() const;

    /**
     * Returns the density at \p position by trilinear interpolation of the grid, or 0 if
     * the position lies outside the bounding box.
     */
    float densityAt(const glm::vec3& position) const;

    const std::vector<glm::vec3>& particlePositions() const;
    const std::vector<float>& particleDensities() const;

    size_t nodeIndex(const glm::uvec3& node) const;
    glm::vec3 nodePosition(const glm::uvec3& node) const;

private:
    void compute();

    // Sums the Epanechnikov kernels of all particles onto the grid. Each particle i uses
    // lengths[i] as its semi-axes and weights[i] as its normalization factor
    std::vector<float> splat(const std::vector<glm::vec3>& lengths,
        const std::vector<float>& weights) const;

    float interpolate(const std::vector<float>& grid, const glm::vec3& position) const;

    std::vector<glm::vec3> _positions;
    glm::uvec3 _dimensions;
    glm::vec3 _min = glm::vec3(0.f);
    glm::vec3 _max = glm::vec3(0.f);
    glm::vec3 _spacing = glm::vec3(0.f);

    std::vector<float> _density;
    std::vector<float> _particleDensities;
    float _minDensity = 0.f;
    float _maxDensity = 0.f;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___DENSITYVOLUME___H__
