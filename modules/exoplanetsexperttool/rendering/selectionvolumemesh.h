#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SELECTIONVOLUMEMESH___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SELECTIONVOLUMEMESH___H__

#include <modules/exoplanetsexperttool/spatialselectionhandler.h>
#include <openspace/glm.h>
#include <vector>

namespace openspace::exoplanets {

/**
 * Renderable geometry describing the outline and surface of a spatial selection volume.
 *
 * All positions are expressed in the same (galactic Cartesian) coordinate frame as the
 * selection volumes themselves.
 */
struct SelectionVolumeMesh {
    /// Triangle list (three consecutive vertices per triangle), used to draw the
    /// translucent surface of the volume. Winding is chosen so that triangles face
    /// outwards from the volume.
    std::vector<glm::dvec3> triangles;

    /// Line list (two consecutive vertices per segment), used to draw the volume's
    /// wireframe / edges.
    std::vector<glm::dvec3> lines;
};

/**
 * Builds a renderable mesh for the volume used in a spatial selection query.
 *
 * Dispatches on the type of volume held by \p query:
 *  - SphereVolume: tessellated sphere with great-circle outlines
 *  - BoxVolume: box faces and edges
 *  - SkyMapRect: RA/Dec/distance shell sector
 *  - MeshVolume: concatenated triangles of the convex hulls
 *  - IsoSurfaceVolume: the triangles of its geometry (if any)
 *  - any other volume type: an empty mesh
 *
 * \param query The spatial selection query whose volume should be visualized
 * \return The generated mesh. It is empty if the volume is invalid or unsupported
 */
SelectionVolumeMesh selectionVolumeMesh(const SpatialSelectionQuery& query);

} // namespace openspace::exoplanets

#endif // __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SELECTIONVOLUMEMESH___H__
