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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SPATIALSELECTIONHANDLER___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SPATIALSELECTIONHANDLER___H__

#include <modules/exoplanetsexperttool/datastructures.h>

#include <openspace/glm.h>
#include <string>
#include <variant>
#include <vector>

namespace openspace::exoplanets {

/**
 * Geometric parameters for a 3D spherical selection volume.
 */
struct SphereVolume {
    /// Center coordinates of the sphere in Parsecs
    glm::dvec3 center = glm::dvec3(0.0);

    /// Radius of the sphere in Parsecs
    double radius = 100.0;
};

/**
 * Geometric parameters for an axis-aligned 3D box selection volume.
 */
struct BoxVolume {
    /// Center coordinates of the box in Parsecs
    glm::dvec3 center = glm::dvec3(0.0);

    /// Full width, height, and depth dimensions of the box in Parsecs
    glm::dvec3 dimensions = glm::dvec3(100.0);
};

/**
 * Bounds for a 2D celestial sky map selection rectangle in equatorial coordinates.
 */
struct SkyMapRect {
    /// Minimum Right Ascension in degrees [0, 360]
    double raMin = 0.0;

    /// Maximum Right Ascension in degrees [0, 360]
    double raMax = 360.0;

    /// Minimum Declination in degrees [-90, +90]
    double decMin = -90.0;

    /// Maximum Declination in degrees [-90, +90]
    double decMax = 90.0;

    /// Flag whether to additionally filter by distance
    bool useDistanceFilter = false;

    /// Minimum distance in Parsecs when distance filtering is enabled
    double distMin = 0.0;

    /// Maximum distance in Parsecs when distance filtering is enabled
    double distMax = 10000.0;
};

/**
 * Configuration parameters for density-based spatial selection.
 */
struct DensitySelectionSettings {
    /// Search radius around each point in Parsecs
    double searchRadius = 10.0;

    /// Minimum number of neighbors within the search radius
    int minNeighbors = 5;
};

using SpatialSelectionQuery = std::variant<
    SphereVolume,
    BoxVolume,
    SkyMapRect,
    DensitySelectionSettings
>;

/**
 * Represents a saved spatial query with user metadata.
 */
struct SavedSelection {
    /// Stable session-local identity
    size_t id = 0;

    /// User-defined display name
    std::string name;

    /// Human-readable summary of the selection parameters
    std::string summary;

    /// Reusable spatial query definition
    SpatialSelectionQuery query;

    /// Flag indicating if this selection is active when applying combined selections
    bool isEnabled = true;
};

/**
 * The SpatialSelectionHandler manages spatial intersection queries against 3D Cartesian
 * positions and 2D celestial coordinates, and maintains a collection of saved selections.
 */
class SpatialSelectionHandler {
public:
    constexpr static std::string_view SelectionVolumeIdentifier = "ExoplanetSelectionVolume";

    SpatialSelectionHandler() = default;

    /**
     * Selects candidate exoplanets whose 3D position falls within the given sphere.
     *
     * \param data The full dataset of exoplanet items
     * \param candidates Candidate item indices to test
     * \param sphere The sphere parameters in Parsecs
     * \return Indices of exoplanets located inside the sphere
     */
    std::vector<size_t> selectSphere(const std::vector<ExoplanetItem>& data,
        const std::vector<size_t>& candidates, const SphereVolume& sphere) const;

    /**
     * Selects candidate exoplanets whose 3D position falls within the given box.
     *
     * \param data The full dataset of exoplanet items
     * \param candidates Candidate item indices to test
     * \param box The box parameters in Parsecs
     * \return Indices of exoplanets located inside the box
     */
    std::vector<size_t> selectBox(const std::vector<ExoplanetItem>& data,
        const std::vector<size_t>& candidates, const BoxVolume& box) const;

    /**
     * Selects candidate exoplanets that fall within the given sky rectangle in RA/Dec.
     *
     * \param data The full dataset of exoplanet items
     * \param candidates Candidate item indices to test
     * \param rect The celestial rectangle bounds in degrees
     * \param mapping Column mapping used to look up RA, Dec, and distance
     * \return Indices of exoplanets located inside the sky rectangle
     */
    std::vector<size_t> selectSkyRect(const std::vector<ExoplanetItem>& data,
        const std::vector<size_t>& candidates, const SkyMapRect& rect,
        const DataSettings::DataMapping& mapping) const;

    /**
     * Selects candidate exoplanets located in dense spatial clusters.
     *
     * \param data The full dataset of exoplanet items
     * \param candidates Candidate item indices to test
     * \param settings Density parameters including search radius and neighbor threshold
     * \return Indices of exoplanets satisfying the density threshold
     */
    std::vector<size_t> selectDensity(const std::vector<ExoplanetItem>& data,
        const std::vector<size_t>& candidates,
        const DensitySelectionSettings& settings) const;

    /**
     * Evaluates a spatial query against the provided candidate rows.
     */
    std::vector<size_t> select(const std::vector<ExoplanetItem>& data,
        const std::vector<size_t>& candidates, const SpatialSelectionQuery& query,
        const DataSettings::DataMapping& mapping) const;

    /**
     * Adds a new saved selection to the list.
     *
     * \param name Display name for the selection
     * \param summary Text summary of selection parameters
      * \param query Reusable spatial query definition
     */
    void addSavedSelection(std::string name, std::string summary,
          SpatialSelectionQuery query);

    void removeSavedSelection(size_t index);

    void clearSavedSelections();
    std::vector<SavedSelection>& savedSelections();
    const std::vector<SavedSelection>& savedSelections() const;

    /**
     * Combines all currently enabled saved selections into a single deduplicated list.
     *
     * \return Sorted vector of unique indices from all enabled saved selections
     */
    std::vector<size_t> combinedSavedSelections(
        const std::vector<ExoplanetItem>& data,
        const std::vector<size_t>& candidates,
        const DataSettings::DataMapping& mapping) const;

    void initializeRenderables();

    void updateSelectionVolume(const SpatialSelectionQuery* query);
    void hideSelectionVolume();

private:
    std::vector<SavedSelection> _savedSelections;
    size_t _nextSavedSelectionId = 0;

    std::vector<double> _selectionVolumeParameters;
    bool _selectionVolumeIsVisible = false;
    bool _selectionVolumeIsInitialized = false;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SPATIALSELECTIONHANDLER___H__
