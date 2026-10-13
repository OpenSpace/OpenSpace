#include <modules/exoplanetsexperttool/rendering/selectionvolumemesh.h>

#include <openspace/util/coordinateconversion.h>
#include <algorithm>
#include <array>
#include <cmath>
#include <type_traits>
#include <variant>
#include <vector>

namespace openspace::exoplanets {

/**
 * Checks that all components of a vector are finite (neither NaN nor infinite).
 *
 * \param value The vector to check
 * \return `true` if x, y and z are all finite, `false` otherwise
 */
bool isFinite(const glm::dvec3& value) {
    return std::isfinite(value.x) && std::isfinite(value.y) &&
        std::isfinite(value.z);
}

/**
 * Appends a single triangle to \p mesh, oriented so that it faces outwards.
 *
 * The triangle is silently discarded if any vertex is non-finite or if the triangle is
 * degenerate (zero or near-zero area). The degeneracy test is performed on edges that
 * have been normalized by their largest component, so that it is independent of the
 * absolute size of the triangle (important as selection volumes can span huge distances).
 *
 * If the geometric normal of the triangle (given by the vertex order) points away from
 * \p outward, the last two vertices are swapped to flip the winding.
 *
 * \param mesh The mesh to append the triangle to
 * \param first The first vertex
 * \param second The second vertex
 * \param third The third vertex
 * \param outward A vector pointing in the desired outward direction. It does not need
 *        to be normalized, and only its direction relative to the triangle normal
 *        matters
 */
void triangle(SelectionVolumeMesh& mesh, glm::dvec3 first, glm::dvec3 second,
              glm::dvec3 third, const glm::dvec3& outward)
{
    if (!isFinite(first) || !isFinite(second) || !isFinite(third)) {
        return;
    }
    const glm::dvec3 edge = second - first;
    const glm::dvec3 other = third - first;
    const double scale = std::max(
        glm::compMax(glm::abs(edge)), glm::compMax(glm::abs(other))
    );
    if (!std::isfinite(scale) || scale <= 0.0) {
        return;
    }
    const glm::dvec3 normal = glm::cross(edge / scale, other / scale);
    if (glm::dot(normal, normal) <= 1.e-24) {
        return;
    }
    if (glm::dot(normal, outward) < 0.0) {
        std::swap(second, third);
    }
    mesh.triangles.insert(mesh.triangles.end(), { first, second, third });
}

/**
 * Appends a quad to \p mesh as two outward-facing triangles.
 *
 * The quad is split into the triangles (first, second, third) and (first, third,
 * fourth), so the vertices should be given in order around the perimeter. Each triangle
 * is passed through selectionvolume::triangle, meaning degenerate halves are dropped
 * individually.
 *
 * \param mesh The mesh to append to
 * \param first The first corner
 * \param second The second corner (adjacent to \p first)
 * \param third The third corner (opposite \p first)
 * \param fourth The fourth corner (adjacent to \p first and \p third)
 * \param outward A vector pointing in the desired outward direction of the quad
 */
void quad(SelectionVolumeMesh& mesh, const glm::dvec3& first, const glm::dvec3& second,
          const glm::dvec3& third, const glm::dvec3& fourth, const glm::dvec3& outward)
{
    triangle(mesh, first, second, third, outward);
    triangle(mesh, first, third, fourth, outward);
}

/**
 * Appends a line segment to \p mesh.
 *
 * The segment is ignored if either end point is non-finite, or if both end points are
 * identical (zero-length segment).
 *
 * \param mesh The mesh to append to
 * \param first The start point of the segment
 * \param second The end point of the segment
 */
void line(SelectionVolumeMesh& mesh, const glm::dvec3& first, const glm::dvec3& second) {
    if (isFinite(first) && isFinite(second) && first != second) {
        mesh.lines.insert(mesh.lines.end(), { first, second });
    }
}

/**
 * Builds the mesh for a spherical selection volume.
 *
 * The surface is a latitude/longitude tessellation (24 longitudes by 12 latitudes) with
 * outward-facing triangles. The wireframe consists of three orthogonal great circles
 * (one per coordinate axis plane), each made of 48 segments.
 *
 * \param volume The sphere to build a mesh for
 * \return The generated mesh, or an empty mesh if the center is non-finite or the
 *         radius is non-finite or not positive
 */
SelectionVolumeMesh sphere(const SphereVolume& volume) {
    SelectionVolumeMesh mesh;
    if (!isFinite(volume.center) || !std::isfinite(volume.radius) ||
        volume.radius <= 0.0)
    {
        return mesh;
    }
    constexpr int Longitudes = 24;
    constexpr int Latitudes = 12;
    const auto point = [&volume](int longitude, int latitude) {
        const double azimuth = glm::two_pi<double>() * longitude / Longitudes;
        const double elevation = glm::pi<double>() * latitude / Latitudes;
        // Force the ring radius to exactly zero at the poles so that all vertices there
        // coincide exactly (avoids tiny floating point offsets)
        const double ring = (latitude == 0 || latitude == Latitudes) ?
            0.0 : std::sin(elevation);
        return volume.center + volume.radius * glm::dvec3(
            ring * std::cos(azimuth), ring * std::sin(azimuth),
            std::cos(elevation)
        );
    };
    for (int latitude = 0; latitude < Latitudes; ++latitude) {
        for (int longitude = 0; longitude < Longitudes; ++longitude) {
            const glm::dvec3 first = point(longitude, latitude);
            const glm::dvec3 second = point(longitude + 1, latitude);
            const glm::dvec3 third = point(longitude + 1, latitude + 1);
            const glm::dvec3 fourth = point(longitude, latitude + 1);
            quad(mesh, first, second, third, fourth,
                (first + second + third + fourth) / 4.0 - volume.center);
        }
    }
    constexpr int Segments = 48;
    for (int axis = 0; axis < 3; ++axis) {
        for (int segment = 0; segment < Segments; ++segment) {
            const auto circlePoint = [&](int index) {
                const double angle = glm::two_pi<double>() * index / Segments;
                glm::dvec3 offset(0.0);
                offset[(axis + 1) % 3] = std::cos(angle);
                offset[(axis + 2) % 3] = std::sin(angle);
                return volume.center + volume.radius * offset;
            };
            line(mesh, circlePoint(segment), circlePoint(segment + 1));
        }
    }
    return mesh;
}

/**
 * Builds the mesh for an axis-aligned box selection volume.
 *
 * The surface consists of the six faces (two triangles each, facing outwards) and the
 * wireframe consists of the twelve box edges. Negative dimensions are treated as their
 * absolute value.
 *
 * \param volume The box to build a mesh for
 * \return The generated mesh, or an empty mesh if the center or dimensions are
 *         non-finite, or if any dimension is zero
 */
SelectionVolumeMesh box(const BoxVolume& volume) {
    SelectionVolumeMesh mesh;
    const glm::dvec3 half = glm::abs(volume.dimensions) / 2.0;

    if (!isFinite(volume.center) || !isFinite(half) ||
        half.x <= 0.0 || half.y <= 0.0 || half.z <= 0.0)
    {
        return mesh;
    }

    for (int axis = 0; axis < 3; ++axis) {
        // The two faces perpendicular to this axis
        for (int sign : { -1, 1 }) {
            glm::dvec3 outward(0.0);
            outward[axis] = static_cast<double>(sign);
            std::array<glm::dvec3, 4> corners;
            constexpr std::array<int, 4> FirstSigns = { -1, 1, 1, -1 };
            constexpr std::array<int, 4> SecondSigns = { -1, -1, 1, 1 };
            for (int corner = 0; corner < 4; ++corner) {
                glm::dvec3 offset = outward * half;
                offset[(axis + 1) % 3] = FirstSigns[corner] * half[(axis + 1) % 3];
                offset[(axis + 2) % 3] = SecondSigns[corner] * half[(axis + 2) % 3];
                corners[corner] = volume.center + offset;
            }
            quad(mesh, corners[0], corners[1], corners[2], corners[3], outward);
        }
        // The four edges that run parallel to this axis
        for (int firstSign : { -1, 1 }) {
            for (int secondSign : { -1, 1 }) {
                glm::dvec3 offset(0.0);
                offset[axis] = -half[axis];
                offset[(axis + 1) % 3] = firstSign * half[(axis + 1) % 3];
                offset[(axis + 2) % 3] = secondSign * half[(axis + 2) % 3];
                const glm::dvec3 first = volume.center + offset;
                offset[axis] = half[axis];
                line(mesh, first, volume.center + offset);
            }
        }
    }
    return mesh;
}

/**
 * Builds the mesh for a sky-map rectangle selection volume.
 *
 * The volume is the region of space bounded by a right ascension range, a declination
 * range and a distance range (a "spherical shell sector"). Directions are given in
 * ICRS (RA/Dec, in degrees) and converted to galactic Cartesian coordinates. The near
 * distance is `distMin` if the distance filter is enabled, otherwise 0.
 *
 * Two strategies are used:
 *  - A fast path for "simple" volumes (RA span and Dec range both below 180 degrees and
 *    not touching the poles), where the volume is approximated by a hexahedron with
 *    flat faces spanned by the four corner rays. If every expected face survives the
 *    degeneracy checks, this result is returned directly.
 *  - Otherwise (or if the fast path produced an unexpected triangle count), a general
 *    tessellation with roughly 5 degree cells is generated, with curved near and far
 *    caps and side walls along the RA/Dec boundaries. This path handles RA wrap-around,
 *    full-sky RA ranges and volumes reaching the poles.
 *
 * The RA range may wrap around 0/360 (i.e. `raMax < raMin`).
 *
 * \param volume The sky-map rectangle to build a mesh for
 * \return The generated mesh, or an empty mesh if any parameter is non-finite or out of
 *         range (RA outside [0, 360], Dec outside [-90, 90], negative near distance,
 *         far distance not greater than near), or if the volume has no extent
 */
SelectionVolumeMesh sky(const SkyMapRect& volume) {
    SelectionVolumeMesh mesh;
    const double near = volume.useDistanceFilter ? volume.distMin : 0.0;
    const double far = volume.distMax;
    const std::array<double, 6> parameters = {
        volume.raMin, volume.raMax, volume.decMin, volume.decMax, near, far
    };
    for (double parameter : parameters) {
        if (!std::isfinite(parameter)) {
            return mesh;
        }
    }

    if (near < 0.0 || far <= near || far <= 0.0 ||
        volume.raMin < 0.0 || volume.raMin > 360.0 ||
        volume.raMax < 0.0 || volume.raMax > 360.0)
    {
        return mesh;
    }

    // Angular extent in RA, accounting for wrap-around
    double span = volume.raMax - volume.raMin;
    if (span < 0.0) {
        span += 360.0;
    }
    const double bottom = std::min(volume.decMin, volume.decMax);
    const double top = std::max(volume.decMin, volume.decMax);
    if (span <= 0.0 || bottom < -90.0 || top > 90.0 || top <= bottom) {
        return mesh;
    }

    // Unit direction vector (galactic Cartesian) for an RA/Dec pair in degrees
    const auto direction = [](double ra, double dec) {
        return icrsToGalacticCartesian(std::fmod(ra, 360.0), dec, 1.0);
    };
    const double left = volume.raMin;
    const double right = left + span;
    const bool isSimple = span < 180.0 && top - bottom < 180.0 &&
        bottom > -90.0 && top < 90.0;

    if (isSimple) {
        const std::array<glm::dvec3, 4> rays = {
            direction(left, bottom), direction(right, bottom),
            direction(right, top), direction(left, top)
        };
        // Mean direction of the corner rays, used as the central axis of the volume
        const glm::dvec3 axis = (rays[0] + rays[1] + rays[2] + rays[3]) / 4.0;
        const glm::dvec3 interior = axis * (near / 2.0 + far / 2.0);
        quad(mesh, near * rays[0], near * rays[1], near * rays[2],
            near * rays[3], -axis);
        quad(mesh, far * rays[0], far * rays[1], far * rays[2],
            far * rays[3], axis);
        for (int corner = 0; corner < 4; ++corner) {
            const int next = (corner + 1) % 4;
            quad(mesh, near * rays[corner], near * rays[next], far * rays[next],
                far * rays[corner],
                (rays[corner] + rays[next]) * (near / 4.0 + far / 4.0) -
                    interior);
            line(mesh, near * rays[corner], near * rays[next]);
            line(mesh, far * rays[corner], far * rays[next]);
            line(mesh, near * rays[corner], far * rays[corner]);
        }
        // Sanity check: all expected triangles must be present. With a near distance of
        // zero the near face collapses to a point, leaving 6 triangles (18 vertices)
        // instead of 12 (36 vertices). Otherwise fall back to the general tessellation
        if (mesh.triangles.size() == (near == 0.0 ? 18 : 36)) {
            return mesh;
        }
        mesh = {};
    }

    // General case: tessellate into cells of roughly 5 degrees
    const int nRa = std::max(1, static_cast<int>(std::ceil(span / 5.0)));
    const int nDec = std::max(1, static_cast<int>(std::ceil((top - bottom) / 5.0)));
    const bool isFullRa = span == 360.0;

    // Unit direction at a given grid vertex. For a full RA range the last column is
    // snapped back to the first to close the seam, and at the poles RA is irrelevant
    const auto ray = [&](int raIndex, int decIndex) {
        const double ra = isFullRa && raIndex == nRa ? left :
            left + span * raIndex / nRa;
        const double dec = bottom + (top - bottom) * decIndex / nDec;
        return direction(std::abs(dec) == 90.0 ? 0.0 : ra, dec);
    };

    // Near and far caps
    for (int raIndex = 0; raIndex < nRa; ++raIndex) {
        for (int decIndex = 0; decIndex < nDec; ++decIndex) {
            const glm::dvec3 first = ray(raIndex, decIndex);
            const glm::dvec3 second = ray(raIndex + 1, decIndex);
            const glm::dvec3 third = ray(raIndex + 1, decIndex + 1);
            const glm::dvec3 fourth = ray(raIndex, decIndex + 1);
            const glm::dvec3 outward = first + second + third + fourth;
            quad(mesh, far * first, far * second, far * third, far * fourth,
                outward);
            quad(mesh, near * first, near * second, near * third, near * fourth,
                -outward);
        }
    }

    // Side wall between two neighboring rays. A wall surface is only emitted when
    // `hasWall` is true, but outline segments are always added at both distances
    const auto wall = [&](const glm::dvec3& first, const glm::dvec3& second,
                          const glm::dvec3& outward, bool hasWall)
    {
        if (hasWall) {
            quad(mesh, near * first, near * second, far * second, far * first,
                outward);
        }
        line(mesh, far * first, far * second);
        line(mesh, near * first, near * second);
    };

    // Walls along the declination boundaries (skipped at the poles, where they collapse)
    for (int raIndex = 0; raIndex < nRa; ++raIndex) {
        const double midpoint = left + span * (raIndex + 0.5) / nRa;
        wall(ray(raIndex, 0), ray(raIndex + 1, 0),
            direction(midpoint, bottom - 0.01) - direction(midpoint, bottom),
            bottom > -90.0);
        wall(ray(raIndex, nDec), ray(raIndex + 1, nDec),
            direction(midpoint, top + 0.01) - direction(midpoint, top),
            top < 90.0);
        // Full sky: add an equator-like outline ring (lines only, no surface)
        if (isFullRa && bottom == -90.0 && top == 90.0) {
            wall(ray(raIndex, nDec / 2), ray(raIndex + 1, nDec / 2),
                glm::dvec3(0.0), false);
        }
    }

    // Walls along the RA boundaries. For a full RA range there are no real boundaries,
    // so only four meridian outlines are drawn for visual reference
    for (int decIndex = 0; decIndex < nDec; ++decIndex) {
        const double midpoint = bottom + (top - bottom) * (decIndex + 0.5) / nDec;
        if (!isFullRa) {
            wall(ray(0, decIndex), ray(0, decIndex + 1),
                direction(left - 0.01, midpoint) - direction(left, midpoint), true);
            wall(ray(nRa, decIndex), ray(nRa, decIndex + 1),
                direction(right + 0.01, midpoint) - direction(right, midpoint), true);
        }
        else {
            for (int meridian : { 0, nRa / 4, nRa / 2, 3 * nRa / 4 }) {
                wall(ray(meridian, decIndex), ray(meridian, decIndex + 1),
                    glm::dvec3(0.0), false);
            }
        }
    }

    // Radial edges at the four corners of the angular rectangle
    if (!isFullRa) {
        for (int raIndex : { 0, nRa }) {
            for (int decIndex : { 0, nDec }) {
                line(mesh, near * ray(raIndex, decIndex), far * ray(raIndex, decIndex));
            }
        }
    }
    return mesh;
}

/**
 * Builds the mesh for a volume defined by a set of convex hulls.
 *
 * The triangles of all hulls are concatenated as-is, so the hulls are expected to
 * already have correct winding. No wireframe lines are generated.
 *
 * \param volume The mesh volume to build a mesh for
 * \return The combined triangles of all hulls, or an empty mesh if the volume has no
 *         hull data
 */
SelectionVolumeMesh meshVolume(const MeshVolume& volume) {
    SelectionVolumeMesh mesh;
    if (!volume.hulls) {
        return mesh;
    }
    for (const ConvexHull& hull : *volume.hulls) {
        mesh.triangles.insert(
            mesh.triangles.end(), hull.triangles.begin(), hull.triangles.end()
        );
    }
    return mesh;
}

SelectionVolumeMesh selectionVolumeMesh(const SpatialSelectionQuery& query) {
    return std::visit([](const auto& volume) -> SelectionVolumeMesh {
        using Volume = std::decay_t<decltype(volume)>;
        if constexpr (std::is_same_v<Volume, SphereVolume>) {
            return sphere(volume);
        }
        else if constexpr (std::is_same_v<Volume, BoxVolume>) {
            return box(volume);
        }
        else if constexpr (std::is_same_v<Volume, SkyMapRect>) {
            return sky(volume);
        }
        else if constexpr (std::is_same_v<Volume, MeshVolume>) {
            return meshVolume(volume);
        }
        else if constexpr (std::is_same_v<Volume, IsoSurfaceVolume>) {
            return volume.geometry ?
                SelectionVolumeMesh{ .triangles = volume.geometry->triangles } :
                SelectionVolumeMesh{};
        }
        else {
            return {};
        }
    }, query);
}

} // namespace openspace::exoplanets
