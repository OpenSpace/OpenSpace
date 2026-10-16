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

#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SPATIALSELECTIONVIEW___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SPATIALSELECTIONVIEW___H__

#include <modules/exoplanetsexperttool/datastructures.h>
#include <modules/exoplanetsexperttool/spatialselectionhandler.h>
#include <modules/exoplanetsexperttool/spatialselection/castselection.h>

#include <openspace/glm.h>
#include <openspace/util/keys.h>
#include <openspace/util/mouse.h>
#include <optional>
#include <string>
#include <vector>

namespace openspace::exoplanets {

class DataViewer;
class RenderableDensityPointCloud;

/**
 * The spatial geometry or method currently used for point selection.
 */
enum class SpatialSelectionMethod {
    /// 3D geometric shape selection (Sphere or Box)
    Shape3D = 0,

    /// 2D celestial sky map rectangle selection
    SkyMap,

    /// Lasso-based selection
    Lasso
};

/**
 * The specific 3D geometric shape type used within the 3D Shape tab.
 */
enum class Shape3DType {
    Sphere = 0,
    Box
};

/**
 * The SpatialSelectionView renders the ImGui window for configuring 3D and 2D spatial
 * point selections, toggling mouse interaction mode, and managing saved selections.
 */
class SpatialSelectionView {
public:
    SpatialSelectionView(DataViewer& dataViewer);

    SpatialSelectionHandler& handler();
    const SpatialSelectionHandler& handler() const;

    /**
     * Renders the Spatial Selection ImGui window.
     *
     * \param open Pointer to the boolean controlling window visibility
     */
    bool render(bool* open);

    /**
     * Evaluates the current spatial query against the provided candidate rows.
     *
     * \param candidates Candidate item indices to test
     * \return Candidate indices matching the current spatial query
     */
    std::vector<size_t> currentSpatialSelection(
        const std::vector<size_t>& candidates) const;

    std::optional<SpatialSelectionQuery> currentSpatialQuery() const;

    bool isLassoTabActive() const;

    bool isDrawingLasso() const;

    /**
     * The volume most recently generated from the lasso selection, if any.
     */
    const std::optional<SpatialSelectionQuery>& generatedVolume() const;

    /**
     * Starts or finishes a lasso stroke at \p mousePos (window pixels). Returns true if
     * the event was consumed.
     */
    bool lassoMouseButton(MouseButton button, MouseAction action, KeyModifier modifier,
        const glm::vec2& mousePos);

    void lassoMousePosition(const glm::vec2& mousePos);

private:
    bool renderMethodTabs();
    bool renderShape3DTab();
    bool renderSkyMapTab();
    bool renderLassoTab();
    bool renderCurrentSelectionActions();
    bool renderSavedSelectionsManager();

    RenderableDensityPointCloud* lassoTarget() const;
    void runLassoSelection(RenderableDensityPointCloud* target);
    void applyLassoSelection() const;
    void clearLassoSelection();
    void generateLassoVolume(RenderableDensityPointCloud* target, bool isIsosurface);
    void drawLasso() const;

    struct ConstellationLineSegment {
        std::vector<float> ra;
        std::vector<float> dec;
    };

    void initConstellationLines() const;

    void applyCurrentSelection();
    void applyCombinedSavedSelections();
    void loadSavedSelection(const SavedSelection& selection);
    void updateSkyMapCache() const;

    /// Reference to the main DataViewer coordinator
    DataViewer& _dataViewer;

    /// Active spatial selection category
    SpatialSelectionMethod _currentMethod = SpatialSelectionMethod::Shape3D;

    std::optional<SpatialSelectionMethod> _requestedTab;

    /// Active 3D shape type (Sphere or Box)
    Shape3DType _shape3DType = Shape3DType::Sphere;

    bool _centerInIcrs = false;
    glm::dvec2 _centerAngles = glm::dvec2(0.0);

    /// Parameters for 3D sphere selection
    SphereVolume _sphereVolume;

    /// Parameters for 3D box selection
    BoxVolume _boxVolume;

    /// Parameters for 2D sky map selection
    SkyMapRect _skyMapRect;

    /// Whether changing parameters immediately updates the active selection in DataViewer
    bool _liveUpdate = true;

    /// Whether constellation lines are rendered on the sky map
    bool _showConstellations = true;

    /// Whether constellation lines are rendered on top of planet points
    bool _showConstellationsOnTop = false;

    /// Text buffer for naming new saved selections
    char _saveNameBuffer[128] = "";

    /// Cached Right Ascension values for the 2D sky map plot
    mutable std::vector<float> _cachedRa;

    /// Cached Declination values for the 2D sky map plot
    mutable std::vector<float> _cachedDec;

    /// Cached point colors (as packed ImU32 RGBA) for the 2D sky map scatter plot
    mutable std::vector<unsigned int> _cachedColors;

    /// Cached 2D constellation line segments (in degrees RA/Dec)
    mutable std::vector<ConstellationLineSegment> _cachedConstellationLines;

    /// Cached zodiac constellation line segments (in degrees RA/Dec)
    mutable std::vector<ConstellationLineSegment> _cachedZodiacLines;

    /// Flag indicating whether constellation lines have been loaded
    mutable bool _constellationLinesLoaded = false;

    /// Dirty flag indicating if the 2D sky map cache needs updating
    mutable bool _skyMapCacheDirty = true;

    bool _lassoTabActive = false;
    bool _isDrawingLasso = false;
    // Set when a stroke is finished; the selection runs in the next GUI frame
    bool _lassoPending = false;
    std::vector<glm::vec2> _lassoStroke;
    std::string _lassoTargetNode;
    std::string _lassoStatus;
    std::string _lassoVolumeSummary;
    CastMethod _castMethod = CastMethod::SpaceCast;
    float _castThresholdExponent = 0.f;
    std::unique_ptr<CastSelection> _castSelection;
    CastResult _castResult;
    const DensityVolume* _densityPreviewVolume = nullptr;
    std::optional<SpatialSelectionQuery> _generatedVolume;
};

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SPATIALSELECTIONVIEW___H__
