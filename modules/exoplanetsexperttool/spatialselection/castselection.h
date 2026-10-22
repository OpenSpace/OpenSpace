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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___CASTSELECTION___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___CASTSELECTION___H__

#include <openspace/glm.h>
#include <cstdint>
#include <memory>
#include <vector>

namespace openspace::exoplanets {

class DensityVolume;

/**
 * The context-aware selection techniques from Yu et al., "CAST: Effective and Efficient
 * User Interaction for Context-Aware Selection in 3D Particle Clouds".
 */
enum class CastMethod {
    SpaceCast = 0,
    TraceCast,
    PointCast
};

/**
 * Maps volume coordinates to window pixel coordinates (origin top left, y down).
 */
struct ScreenProjection {
    glm::dmat4 modelToClip = glm::dmat4(1.0);
    glm::vec2 viewportSize = glm::vec2(0.f);

    /**
     * Writes the pixel position of \p p to \p screen and returns its clip space w, i.e.
     * the distance to the eye. If w <= 0 the point is behind the camera and \p screen is
     * left unchanged.
     */
    float project(const glm::vec3& p, glm::vec2& screen) const;

    /**
     * Approximate on-screen size in pixels of a grid cell with the given \p spacing at
     * unit depth; divide by the depth to get the size at that depth.
     */
    float cellFootprint(const glm::vec3& spacing) const;
};

/**
 * The user input for a cast selection.
 */
struct CastInput {
    /// The mouse positions in window pixels. A click is a stroke with a single point
    std::vector<glm::vec2> stroke;
    ScreenProjection projection;
};

struct CastResult {
    /// Indices of the selected particles in DensityVolume::particlePositions
    std::vector<int> selectedParticles;
    /// 1 for every grid voxel that is part of the selected volume
    std::vector<uint8_t> selectedVoxelMask;
    /// The density threshold that was used
    float threshold = 0.f;
    /// The number of disjoint volumes that passed the threshold
    size_t nComponents = 0;
};

/**
 * Base class of the cast selection techniques. The constructor of a technique performs
 * the input-dependent computations once so that the density threshold can be adjusted
 * interactively through #select.
 */
class CastSelection {
public:
    /**
     * \param volume The density volume; must outlive this object
     */
    explicit CastSelection(const DensityVolume& volume);
    virtual ~CastSelection() = default;

    const DensityVolume& volume() const;

    /**
     * Returns false if the input does not hit the volume, in which case #select returns
     * an empty result.
     */
    bool isValid() const;

    /**
     * The automatically computed density threshold rho_0.
     */
    float baseThreshold() const;

    /**
     * Selects a volume using the density threshold 2^exponent * baseThreshold().
     *
     * \param exponent The threshold exponent, in [-4, 4]
     */
    virtual CastResult select(float exponent) const = 0;

protected:
    float threshold(float exponent) const;

    const DensityVolume& _volume;
    float _baseThreshold = 0.f;
    bool _isValid = false;
};

/**
 * Creates the selection technique \p method for \p input, or returns nullptr if the
 * technique is not implemented.
 */
std::unique_ptr<CastSelection> createCastSelection(CastMethod method,
    const DensityVolume& volume, const CastInput& input);

CastResult selectDensityVolume(const DensityVolume& volume, float exponent);

struct Components {
    /// The component index for each grid cell, or -1 if the cell is not in the mask
    std::vector<int> labels;
    /// The number of voxels in each component
    std::vector<size_t> sizes;
};

/**
 * Splits the nonzero voxels of \p mask into 6-connected components. The mask uses the
 * same layout as DensityVolume (x varies fastest, then y, then z).
 */
Components labelComponents(const std::vector<uint8_t>& mask, const glm::uvec3& dims);

/**
 * Returns the indices of the particles for which \p candidates is nonzero and that lie
 * in a grid cell with at least one corner cell labeled \p label.
 */
std::vector<int> particlesInComponent(const DensityVolume& volume,
    const std::vector<int>& labels, int label, const std::vector<uint8_t>& candidates);

/**
 * Returns the sequence 0, 1, ..., n - 1, for use with parallel algorithms.
 */
std::vector<size_t> indexSequence(size_t n);

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___CASTSELECTION___H__
