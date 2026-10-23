#include <modules/exoplanetsexperttool/spatialselection/isosurface.h>

#include <algorithm>
#include <array>
#include <cmath>
#include <cstdint>
#include <optional>
#include <utility>
#include <vector>

namespace {
    // Kuhn triangulation of a cube into six tetrahedra that all share the 0-7 diagonal.
    // Corner c of a cell has the offset (c & 1, (c >> 1) & 1, (c >> 2) & 1). Because every
    // cell is split the same way, the faces of neighboring cells match, so the extracted
    // surface is watertight.
    constexpr std::array<std::array<int, 4>, 6> Tetrahedra = { {
        { 0, 1, 3, 7 }, { 0, 3, 2, 7 }, { 0, 2, 6, 7 },
        { 0, 6, 4, 7 }, { 0, 4, 5, 7 }, { 0, 5, 1, 7 }
    } };

    // Radius (in nodes) of the Gaussian kernel used for smoothing, and the padding that
    // is added around the selection. The padding must be larger than the radius so that
    // the blurred values fade out completely before reaching the grid border.
    constexpr int KernelRadius = 3;
    constexpr int Padding = KernelRadius + 1;

    // Returns the offset (0 or 1 along each axis) of a cube corner from the cell origin
    glm::uvec3 corner(int index) {
        return glm::uvec3(index & 1, (index >> 1) & 1, (index >> 2) & 1);
    }

    bool finite(const glm::dvec3& point) {
        return std::isfinite(point.x) && std::isfinite(point.y) &&
            std::isfinite(point.z);
    }

    // Total number of nodes in a grid
    size_t nodeCount(const glm::uvec3& dimensions) {
        return static_cast<size_t>(dimensions.x) * dimensions.y * dimensions.z;
    }

    // Inverse of IsoSurface::index: converts a linear node index into a 3D position
    glm::ivec3 cellPosition(size_t node, const glm::uvec3& dimensions) {
        return glm::ivec3(
            node % dimensions.x,
            (node / dimensions.x) % dimensions.y,
            node / (static_cast<size_t>(dimensions.x) * dimensions.y)
        );
    }

    // Blurs the values of the surface with a separable Gaussian filter (sigma = 1 node).
    // Values outside of the grid are treated as zero.
    void gaussianBlur(openspace::exoplanets::IsoSurface& surface) {
        std::array<double, 2 * KernelRadius + 1> kernel;
        double total = 0.0;
        for (int offset = -KernelRadius; offset <= KernelRadius; ++offset) {
            kernel[offset + KernelRadius] = std::exp(-0.5 * offset * offset);
            total += kernel[offset + KernelRadius];
        }
        for (double& weight : kernel) {
            weight /= total;
        }

        std::vector<float> output(surface.values.size());
        for (int axis = 0; axis < 3; ++axis) {
            const int extent = static_cast<int>(surface.dimensions[axis]);
            for (size_t node = 0; node < surface.values.size(); ++node) {
                const glm::ivec3 position = cellPosition(node, surface.dimensions);
                double sample = 0.0;
                for (int offset = -KernelRadius; offset <= KernelRadius; ++offset) {
                    glm::ivec3 neighbor = position;
                    neighbor[axis] += offset;
                    if (neighbor[axis] >= 0 && neighbor[axis] < extent) {
                        sample += kernel[offset + KernelRadius] *
                            surface.values[surface.index(glm::uvec3(neighbor))];
                    }
                }
                output[node] = static_cast<float>(sample);
            }
            surface.values.swap(output);
        }
    }
} // namespace

namespace openspace::exoplanets {

size_t IsoSurface::index(const glm::uvec3& node) const {
    return node.x + static_cast<size_t>(dimensions.x) *
        (node.y + static_cast<size_t>(dimensions.y) * node.z);
}

double IsoSurface::valueAt(const glm::dvec3& position) const {
    // Positions outside of the grid (or a grid without any cells) have the value zero
    if (!finite(position) || glm::any(glm::lessThan(dimensions, glm::uvec3(2))) ||
        glm::any(glm::lessThan(position, glm::dvec3(0.0))) ||
        glm::any(glm::greaterThan(position, glm::dvec3(dimensions) - 1.0)))
    {
        return 0.0;
    }

    // Find the cell containing the position (clamped so that positions on the upper
    // boundary still belong to the last cell) and the position within that cell
    const glm::uvec3 cell = glm::min(
        glm::uvec3(position), dimensions - glm::uvec3(2)
    );
    const glm::dvec3 local = position - glm::dvec3(cell);

    // Each cell is split into six tetrahedra that all contain the diagonal from corner
    // (0,0,0) to corner (1,1,1). Which one contains the point depends on the order of
    // the local coordinates. Sort the axes from largest to smallest coordinate
    std::array<int, 3> axes = { 0, 1, 2 };
    std::sort(axes.begin(), axes.end(), [&](int a, int b) {
        return local[a] > local[b];
    });
    const double largest = local[axes[0]];
    const double middle = local[axes[1]];
    const double smallest = local[axes[2]];

    // The tetrahedron's corners are found by walking from (0,0,0) to (1,1,1), stepping
    // along the axes in sorted order. The interpolation weight of each corner is the
    // gap between consecutive sorted coordinates
    glm::uvec3 offset(0);
    double result = (1.0 - largest) * values[index(cell + offset)];
    offset[axes[0]] = 1;
    result += (largest - middle) * values[index(cell + offset)];
    offset[axes[1]] = 1;
    result += (middle - smallest) * values[index(cell + offset)];
    offset[axes[2]] = 1;
    result += smallest * values[index(cell + offset)];
    return result;
}

bool IsoSurface::contains(const glm::dvec3& position) const {
    return valueAt(position) >= threshold;
}

std::vector<glm::dvec3> IsoSurface::triangles() const {
    std::vector<glm::dvec3> result;

    // Adds a triangle, skipping degenerate ones and making sure that the winding order
    // is such that the normal points in the (approximate) outward direction
    const auto addTriangle = [&](glm::dvec3 first, glm::dvec3 second, glm::dvec3 third,
                                 const glm::dvec3& outward)
    {
        const glm::dvec3 normal = glm::cross(second - first, third - first);
        if (glm::dot(normal, normal) <= 1.e-24) {
            return;
        }
        if (glm::dot(normal, outward) < 0.0) {
            std::swap(second, third);
        }
        result.insert(result.end(), { first, second, third });
    };

    for (unsigned int z = 0; z + 1 < dimensions.z; ++z) {
        for (unsigned int y = 0; y + 1 < dimensions.y; ++y) {
            for (unsigned int x = 0; x + 1 < dimensions.x; ++x) {
                const glm::uvec3 cell(x, y, z);

                // Skip cells that are entirely inside or outside of the surface
                std::array<float, 8> samples;
                int insideCount = 0;
                for (int vertex = 0; vertex < 8; ++vertex) {
                    samples[vertex] = values[index(cell + corner(vertex))];
                    insideCount += samples[vertex] >= threshold;
                }
                if (insideCount == 0 || insideCount == 8) {
                    continue;
                }

                // Point on the edge between an inside and an outside corner where the
                // interpolated value equals the threshold (in grid coordinates)
                const auto crossingPoint = [&](int in, int out) {
                    const double fraction = (threshold - samples[in]) /
                        static_cast<double>(samples[out] - samples[in]);

                    return glm::dvec3(cell) + glm::mix(
                        glm::dvec3(corner(in)), glm::dvec3(corner(out)), fraction
                    );
                };

                for (const std::array<int, 4>&tetrahedron : Tetrahedra) {
                    // Classify the corners and sum up their positions to get centroids
                    std::array<int, 4> in;
                    std::array<int, 4> out;
                    int nIn = 0;
                    int nOut = 0;
                    glm::dvec3 inCentroid(0.0);
                    glm::dvec3 outCentroid(0.0);
                    for (int vertex : tetrahedron) {
                        if (samples[vertex] >= threshold) {
                            in[nIn++] = vertex;
                            inCentroid += glm::dvec3(corner(vertex));
                        }
                        else {
                            out[nOut++] = vertex;
                            outCentroid += glm::dvec3(corner(vertex));
                        }
                    }
                    if (nIn == 0 || nOut == 0) {
                        continue;
                    }

                    // The surface normal should point from the inside towards the outside
                    const glm::dvec3 outward = outCentroid / static_cast<double>(nOut) -
                        inCentroid / static_cast<double>(nIn);

                    if (nIn == 1) {
                        // One corner cut off => one triangle
                        addTriangle(
                            crossingPoint(in[0], out[0]),
                            crossingPoint(in[0], out[1]),
                            crossingPoint(in[0], out[2]),
                            outward
                        );
                    }
                    else if (nOut == 1) {
                        // One corner left => one triangle
                        addTriangle(
                            crossingPoint(in[0], out[0]),
                            crossingPoint(in[1], out[0]),
                            crossingPoint(in[2], out[0]),
                            outward
                        );
                    }
                    else {
                        // Two corners on each side => a quad, split into two triangles
                        const glm::dvec3 a = crossingPoint(in[0], out[0]);
                        const glm::dvec3 b = crossingPoint(in[0], out[1]);
                        const glm::dvec3 c = crossingPoint(in[1], out[1]);
                        const glm::dvec3 d = crossingPoint(in[1], out[0]);
                        addTriangle(a, b, c, outward);
                        addTriangle(a, c, d, outward);
                    }
                }
            }
        }
    }
    return result;
}

std::optional<IsoSurface> smoothSelectionSurface(const glm::uvec3& dimensions,
                                                 const std::vector<uint8_t>& mask,
                                                 const std::vector<glm::dvec3>& points)
{
    const size_t count = nodeCount(dimensions);
    if (glm::any(glm::lessThan(dimensions, glm::uvec3(2))) ||
        mask.size() != count || points.empty())
    {
        return std::nullopt;
    }

    // Find the cell containing each point, and the bounding box (in nodes) of
    // all selected points and masked nodes
    glm::ivec3 minimum(dimensions);
    glm::ivec3 maximum(-1);
    std::vector<glm::ivec3> cells;
    cells.reserve(points.size());

    for (const glm::dvec3& point : points) {
        if (!finite(point) || glm::any(glm::lessThan(point, glm::dvec3(0.0))) ||
            glm::any(glm::greaterThan(point, glm::dvec3(dimensions) - 1.0)))
        {
            return std::nullopt;
        }
        const glm::ivec3 cell = glm::min(
            glm::ivec3(glm::floor(point)), glm::ivec3(dimensions) - 2
        );
        cells.push_back(cell);
        minimum = glm::min(minimum, cell);
        maximum = glm::max(maximum, cell + 1);
    }

    for (size_t node = 0; node < count; ++node) {
        if (mask[node]) {
            const glm::ivec3 position = cellPosition(node, dimensions);
            minimum = glm::min(minimum, position);
            maximum = glm::max(maximum, position);
        }
    }

    // Create a new grid covering the bounding box, with padding on all sides so that
    // the smoothed values can fall off to zero
    IsoSurface surface;
    surface.origin = minimum - Padding;
    surface.dimensions = glm::uvec3(maximum - minimum + 2 * Padding + 1);
    surface.values.assign(nodeCount(surface.dimensions), 0.f);

    // Indices (in the new grid) of the eight corner nodes of a cell given in the
    // coordinates of the original grid
    const auto cornerNodes = [&surface](const glm::ivec3& cell) {
        std::array<size_t, 8> nodes;
        for (int vertex = 0; vertex < 8; ++vertex) {
            nodes[vertex] = surface.index(
                glm::uvec3(cell - surface.origin) + corner(vertex)
            );
        }
        return nodes;
    };

    // Mark the masked nodes and all corners of the cells containing a point
    for (size_t node = 0; node < count; ++node) {
        if (mask[node]) {
            const glm::ivec3 position = cellPosition(node, dimensions);
            surface.values[surface.index(glm::uvec3(position - surface.origin))] = 1.f;
        }
    }
    for (const glm::ivec3& cell : cells) {
        for (size_t node : cornerNodes(cell)) {
            surface.values[node] = 1.f;
        }
    }

    // Blur the values so that the surface gets smoother
    gaussianBlur(surface);

    // Find the lowest blurred value at any selected point. A threshold below that minimum
    // keeps every point inside. The points are shifted by the origin because valueAt
    // works in the surface's grid coordinates
    double minimumValue = 1.0;
    for (const glm::dvec3& point : points) {
        const glm::dvec3 local = point - glm::dvec3(surface.origin);
        minimumValue = std::min(minimumValue, surface.valueAt(local));
    }
    // Capped at 0.5, where a large flat selection's surface ends up after blurring the
    // 0/1 mask. The 0.99 factor is a small margin so rounding can't push a point
    // outside, while keeping the resulting volume as tight as possible around the
    // selected points
    surface.threshold = std::min(0.5, 0.99 * minimumValue);

    // A threshold of zero or less means a point has no blurred value, so no surface
    // can include all points. The negated comparison also rejects NaN
    if (!(surface.threshold > 0.f)) {
        return std::nullopt;
    }
    return surface;
}

} // namespace openspace::exoplanets
