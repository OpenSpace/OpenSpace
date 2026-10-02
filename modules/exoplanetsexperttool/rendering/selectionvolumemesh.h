#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SELECTIONVOLUMEMESH___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___SELECTIONVOLUMEMESH___H__

#include <modules/exoplanetsexperttool/spatialselectionhandler.h>
#include <openspace/util/coordinateconversion.h>
#include <algorithm>
#include <array>
#include <cmath>
#include <type_traits>
#include <vector>

namespace openspace::exoplanets {

struct SelectionVolumeMesh {
    std::vector<glm::dvec3> triangles;
    std::vector<glm::dvec3> lines;
};

namespace selectionvolume {

inline bool isFinite(const glm::dvec3& value) {
    return std::isfinite(value.x) && std::isfinite(value.y) &&
        std::isfinite(value.z);
}

inline void triangle(SelectionVolumeMesh& mesh, glm::dvec3 first,
                     glm::dvec3 second, glm::dvec3 third,
                     const glm::dvec3& outward)
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

inline void quad(SelectionVolumeMesh& mesh, const glm::dvec3& first,
                 const glm::dvec3& second, const glm::dvec3& third,
                 const glm::dvec3& fourth, const glm::dvec3& outward)
{
    triangle(mesh, first, second, third, outward);
    triangle(mesh, first, third, fourth, outward);
}

inline void line(SelectionVolumeMesh& mesh, const glm::dvec3& first,
                 const glm::dvec3& second)
{
    if (isFinite(first) && isFinite(second) && first != second) {
        mesh.lines.insert(mesh.lines.end(), { first, second });
    }
}

inline SelectionVolumeMesh sphere(const SphereVolume& volume) {
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

inline SelectionVolumeMesh box(const BoxVolume& volume) {
    SelectionVolumeMesh mesh;
    const glm::dvec3 half = glm::abs(volume.dimensions) / 2.0;
    if (!isFinite(volume.center) || !isFinite(half) ||
        half.x <= 0.0 || half.y <= 0.0 || half.z <= 0.0)
    {
        return mesh;
    }
    for (int axis = 0; axis < 3; ++axis) {
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

inline SelectionVolumeMesh sky(const SkyMapRect& volume) {
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
    double span = volume.raMax - volume.raMin;
    if (span < 0.0) {
        span += 360.0;
    }
    const double bottom = std::min(volume.decMin, volume.decMax);
    const double top = std::max(volume.decMin, volume.decMax);
    if (span <= 0.0 || bottom < -90.0 || top > 90.0 || top <= bottom) {
        return mesh;
    }
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
        if (mesh.triangles.size() == (near == 0.0 ? 18 : 36)) {
            return mesh;
        }
        mesh = {};
    }
    const int nRa = std::max(1, static_cast<int>(std::ceil(span / 5.0)));
    const int nDec = std::max(1, static_cast<int>(std::ceil((top - bottom) / 5.0)));
    const bool isFullRa = span == 360.0;
    const auto ray = [&](int raIndex, int decIndex) {
        const double ra = isFullRa && raIndex == nRa ? left :
            left + span * raIndex / nRa;
        const double dec = bottom + (top - bottom) * decIndex / nDec;
        return direction(std::abs(dec) == 90.0 ? 0.0 : ra, dec);
    };
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
    for (int raIndex = 0; raIndex < nRa; ++raIndex) {
        const double midpoint = left + span * (raIndex + 0.5) / nRa;
        wall(ray(raIndex, 0), ray(raIndex + 1, 0),
            direction(midpoint, bottom - 0.01) - direction(midpoint, bottom),
            bottom > -90.0);
        wall(ray(raIndex, nDec), ray(raIndex + 1, nDec),
            direction(midpoint, top + 0.01) - direction(midpoint, top),
            top < 90.0);
        if (isFullRa && bottom == -90.0 && top == 90.0) {
            wall(ray(raIndex, nDec / 2), ray(raIndex + 1, nDec / 2),
                glm::dvec3(0.0), false);
        }
    }
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
    if (!isFullRa) {
        for (int raIndex : { 0, nRa }) {
            for (int decIndex : { 0, nDec }) {
                line(mesh, near * ray(raIndex, decIndex), far * ray(raIndex, decIndex));
            }
        }
    }
    return mesh;
}

} // namespace selectionvolume

inline SelectionVolumeMesh selectionVolumeMesh(const SpatialSelectionQuery& query) {
    return std::visit([](const auto& volume) -> SelectionVolumeMesh {
        using Volume = std::decay_t<decltype(volume)>;
        if constexpr (std::is_same_v<Volume, SphereVolume>) {
            return selectionvolume::sphere(volume);
        }
        else if constexpr (std::is_same_v<Volume, BoxVolume>) {
            return selectionvolume::box(volume);
        }
        else if constexpr (std::is_same_v<Volume, SkyMapRect>) {
            return selectionvolume::sky(volume);
        }
        else {
            return {};
        }
    }, query);
}

} // namespace openspace::exoplanets

#endif