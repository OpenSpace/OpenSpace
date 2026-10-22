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

#include <modules/exoplanetsexperttool/spatialselection/selectionserialization.h>

#include <cmath>
#include <format>
#include <stdexcept>
#include <string>

namespace {
    constexpr const char* TypeSphere = "sphere";
    constexpr const char* TypeBox = "box";
    constexpr const char* TypeSkyMap = "skyMap";
    constexpr const char* TypeMesh = "mesh";
    constexpr const char* TypeIsoSurface = "isoSurface";

    template <typename Vec>
    nlohmann::json vecToJson(const Vec& v) {
        nlohmann::json result = nlohmann::json::array();
        for (int i = 0; i < Vec::length(); i++) {
            result.push_back(v[i]);
        }
        return result;
    }

    template <typename Vec>
    Vec vecFromJson(const nlohmann::json& json) {
        if (!json.is_array() || json.size() != static_cast<size_t>(Vec::length())) {
            throw std::runtime_error("Invalid vector in spatial selection");
        }
        Vec v;
        for (int i = 0; i < Vec::length(); i++) {
            v[i] = json[i].get<typename Vec::value_type>();
        }
        return v;
    }

    template <typename Vec>
    nlohmann::json vecListToJson(const std::vector<Vec>& list) {
        nlohmann::json result = nlohmann::json::array();
        for (const Vec& v : list) {
            result.push_back(vecToJson(v));
        }
        return result;
    }

    template <typename Vec>
    std::vector<Vec> vecListFromJson(const nlohmann::json& json) {
        std::vector<Vec> result;
        result.reserve(json.size());
        for (const nlohmann::json& v : json) {
            result.push_back(vecFromJson<Vec>(v));
        }
        return result;
    }

    nlohmann::json matToJson(const glm::dmat4& m) {
        nlohmann::json result = nlohmann::json::array();
        for (int column = 0; column < 4; column++) {
            result.push_back(vecToJson(m[column]));
        }
        return result;
    }

    glm::dmat4 matFromJson(const nlohmann::json& json) {
        if (!json.is_array() || json.size() != 4) {
            throw std::runtime_error("Invalid matrix in spatial selection");
        }
        glm::dmat4 m;
        for (int column = 0; column < 4; column++) {
            m[column] = vecFromJson<glm::dvec4>(json[column]);
        }
        return m;
    }
} // namespace

namespace openspace::exoplanets {

nlohmann::json spatialQueryToJson(const SpatialSelectionQuery& query) {
    if (const SphereVolume* sphere = std::get_if<SphereVolume>(&query)) {
        return {
            { "type", TypeSphere },
            { "center", vecToJson(sphere->center) },
            { "radius", sphere->radius }
        };
    }
    if (const BoxVolume* box = std::get_if<BoxVolume>(&query)) {
        return {
            { "type", TypeBox },
            { "center", vecToJson(box->center) },
            { "dimensions", vecToJson(box->dimensions) }
        };
    }
    if (const SkyMapRect* rect = std::get_if<SkyMapRect>(&query)) {
        return {
            { "type", TypeSkyMap },
            { "raMin", rect->raMin },
            { "raMax", rect->raMax },
            { "decMin", rect->decMin },
            { "decMax", rect->decMax },
            { "useDistanceFilter", rect->useDistanceFilter },
            { "distMin", rect->distMin },
            { "distMax", rect->distMax }
        };
    }
    if (const MeshVolume* mesh = std::get_if<MeshVolume>(&query)) {
        nlohmann::json hulls = nlohmann::json::array();
        if (mesh->hulls) {
            for (const ConvexHull& hull : *mesh->hulls) {
                hulls.push_back({
                    { "planes", vecListToJson(hull.planes) },
                    { "triangles", vecListToJson(hull.triangles) },
                    { "tolerance", hull.tolerance }
                });
            }
        }
        return { { "type", TypeMesh }, { "hulls", hulls } };
    }
    if (const IsoSurfaceVolume* iso = std::get_if<IsoSurfaceVolume>(&query)) {
        if (!iso->geometry) {
            throw std::runtime_error("Iso-surface selection has no geometry");
        }
        const IsoSurface& field = iso->geometry->field;
        return {
            { "type", TypeIsoSurface },
            { "dimensions", vecToJson(field.dimensions) },
            { "origin", vecToJson(field.origin) },
            { "values", field.values },
            { "threshold", field.threshold },
            { "parsecToGrid", matToJson(iso->geometry->parsecToGrid) }
        };
    }
    throw std::runtime_error("Unknown spatial selection type");
}

SpatialSelectionQuery spatialQueryFromJson(const nlohmann::json& json, size_t volumeId) {
    const std::string type = json.at("type").get<std::string>();

    if (type == TypeSphere) {
        return SphereVolume{
            .center = vecFromJson<glm::dvec3>(json.at("center")),
            .radius = json.at("radius").get<double>()
        };
    }
    if (type == TypeBox) {
        return BoxVolume{
            .center = vecFromJson<glm::dvec3>(json.at("center")),
            .dimensions = vecFromJson<glm::dvec3>(json.at("dimensions"))
        };
    }
    if (type == TypeSkyMap) {
        return SkyMapRect{
            .raMin = json.at("raMin").get<double>(),
            .raMax = json.at("raMax").get<double>(),
            .decMin = json.at("decMin").get<double>(),
            .decMax = json.at("decMax").get<double>(),
            .useDistanceFilter = json.value("useDistanceFilter", false),
            .distMin = json.value("distMin", 0.0),
            .distMax = json.value("distMax", 10000.0)
        };
    }
    if (type == TypeMesh) {
        std::vector<ConvexHull> hulls;
        for (const nlohmann::json& h : json.at("hulls")) {
            ConvexHull hull;
            hull.planes = vecListFromJson<glm::dvec4>(h.at("planes"));
            hull.triangles = vecListFromJson<glm::dvec3>(h.at("triangles"));
            hull.tolerance = h.value("tolerance", 0.0);
            if (hull.planes.empty() || hull.triangles.size() % 3 != 0) {
                throw std::runtime_error("Invalid convex hull in mesh selection");
            }
            hulls.push_back(std::move(hull));
        }
        if (hulls.empty()) {
            throw std::runtime_error("Mesh selection has no hulls");
        }
        return MeshVolume{
            .id = volumeId,
            .hulls = std::make_shared<const std::vector<ConvexHull>>(std::move(hulls))
        };
    }
    if (type == TypeIsoSurface) {
        auto geometry = std::make_shared<IsoSurfaceVolume::Geometry>();
        IsoSurface& field = geometry->field;
        field.dimensions = vecFromJson<glm::uvec3>(json.at("dimensions"));
        field.origin = vecFromJson<glm::ivec3>(json.at("origin"));
        field.values = json.at("values").get<std::vector<float>>();
        field.threshold = json.at("threshold").get<float>();
        geometry->parsecToGrid = matFromJson(json.at("parsecToGrid"));

        const size_t nValues = static_cast<size_t>(field.dimensions.x) *
            field.dimensions.y * field.dimensions.z;
        if (nValues == 0 || field.values.size() != nValues) {
            throw std::runtime_error("Iso-surface value count does not match dimensions");
        }

        const double determinant = glm::determinant(geometry->parsecToGrid);
        if (!std::isfinite(determinant) || determinant == 0.0) {
            throw std::runtime_error("Iso-surface transform is not invertible");
        }

        // Mirrors the mesh construction in SpatialSelectionHandler::createIsoSurfaceVolume
        const glm::dmat4 gridToParsec = glm::inverse(geometry->parsecToGrid);
        geometry->triangles = field.triangles();
        for (glm::dvec3& vertex : geometry->triangles) {
            vertex = glm::dvec3(gridToParsec * glm::dvec4(vertex, 1.0));
        }
        if (determinant < 0.0) {
            for (size_t v = 0; v + 2 < geometry->triangles.size(); v += 3) {
                std::swap(geometry->triangles[v + 1], geometry->triangles[v + 2]);
            }
        }

        return IsoSurfaceVolume{ .id = volumeId, .geometry = std::move(geometry) };
    }
    throw std::runtime_error(std::format("Unknown spatial selection type '{}'", type));
}

} // namespace openspace::exoplanets
