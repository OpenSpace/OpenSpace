/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/globebrowsing/src/geojson/globegeometryfeature.h>

#include <modules/globebrowsing/globebrowsingmodule.h>
#include <modules/globebrowsing/src/geojson/globegeometryhelper.h>
#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <geos/util/GEOSException.h>
#include <geos/triangulate/polygon/ConstrainedDelaunayTriangulator.h>
#include <geos/util/IllegalStateException.h>
#include <algorithm>
#include <filesystem>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "GlobeGeometryFeature";

    constexpr std::chrono::milliseconds HeightUpdateInterval(10000);
} // namespace

namespace openspace::globebrowsing {

void GlobeGeometryFeature::RenderFeature::initializeBuffers() {
    if (vaoId == 0) {
        glGenVertexArrays(1, &vaoId);
    }
    if (vboId == 0) {
        glGenBuffers(1, &vboId);
    }
}

GlobeGeometryFeature::GlobeGeometryFeature(const RenderableGlobe& globe,
                                           GeoJsonProperties& defaultProperties,
                                           GeoJsonOverrideProperties& overrideProperties)
    : _globe(globe)
    , _properties({ defaultProperties, overrideProperties })
    , _lastHeightUpdateTime(std::chrono::system_clock::now())
{}

std::string GlobeGeometryFeature::key() const {
    return _key;
}

void GlobeGeometryFeature::setOffsets(const glm::vec3& value) {
    _offsets = value;
}

void GlobeGeometryFeature::initializeGL(ghoul::opengl::ProgramObject* pointsProgram,
                                   ghoul::opengl::ProgramObject* linesAndPolygonsProgram)
{
    _pointsProgram = pointsProgram;
    _linesAndPolygonsProgram = linesAndPolygonsProgram;

    if (isPoints()) {
        updateTexture(true);
    }
}

void GlobeGeometryFeature::deinitializeGL() {
    for (const RenderFeature& r : _renderFeatures) {
        glDeleteVertexArrays(1, &r.vaoId);
        glDeleteBuffers(1, &r.vboId);
    }

    _pointTexture = nullptr;
}

bool GlobeGeometryFeature::isReady() const {
    bool shadersAreReady = (_linesAndPolygonsProgram != nullptr) &&
        (_pointsProgram != nullptr);
    bool textureIsReady = _hasTexture ? (_pointTexture != nullptr) : true;
    return shadersAreReady && textureIsReady;
}

bool GlobeGeometryFeature::isPoints() const {
    return _type == GeometryType::Point;
}

bool GlobeGeometryFeature::useHeightMap() const {
    return _properties.altitudeMode() ==
        GeoJsonProperties::AltitudeMode::RelativeToGround;
}

void GlobeGeometryFeature::updateTexture(bool isInitializeStep) {
    std::string texture = "";
    GlobeBrowsingModule* m = global::moduleEngine->module<GlobeBrowsingModule>();

    if (!isInitializeStep && _properties.hasOverrideTexture()) {
        // Here we don't necessarily have to update, since it should have been
        // created at initialization. Do nothing
        return;
    }
    else if (!_properties.pointTexture().empty()) {
        texture = _properties.pointTexture();
    }
    else if (m->hasDefaultGeoPointTexture()) {
        texture = m->defaultGeoPointTexture();
    }
    else {
        // No texture => render without texture
        _hasTexture = false;
        _pointTexture = nullptr;
        return;
    }

    if (isInitializeStep || !_pointTexture) {
        _pointTexture = std::make_unique<TextureComponent>(2);
        _pointTexture->setFilterMode(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
        _pointTexture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
    }

    std::filesystem::path texturePath = absPath(texture);
    if (std::filesystem::is_regular_file(texturePath)) {
        _hasTexture = true;
        _pointTexture->loadFromFile(texture);
        _pointTexture->uploadToGpu();
    }
    else {
        LERROR(fmt::format("Trying to use texture file that does not exist"));
    }
}

void GlobeGeometryFeature::createFromSingleGeosGeometry(const geos::geom::Geometry* geo,
                                                        int index)
{
    ghoul_assert(
        geo->isPuntal() || !geo->isCollection(),
        "Non-point-geometry can not be a collection"
    );

    switch (geo->getGeometryTypeId()) {
        case geos::geom::GEOS_POINT:
        case geos::geom::GEOS_MULTIPOINT: {
            _geoCoordinates.push_back(geometryhelper::geometryCoordsAsGeoVector(geo));
            _type = GeometryType::Point;
            break;
        }
        case geos::geom::GEOS_LINESTRING: {
            _geoCoordinates.push_back(geometryhelper::geometryCoordsAsGeoVector(geo));
            _type = GeometryType::LineString;
            break;
        }
        case geos::geom::GEOS_POLYGON: {
            try {
                const geos::geom::Polygon* p = dynamic_cast<const geos::geom::Polygon*>(geo);

                // Triangles
                // Note that Constrained Delaunay triangulation supports polygons with holes :)
                std::vector<geos::geom::Coordinate> triCoords;
                TriList<Tri> triangles;
                using geos::triangulate::polygon::ConstrainedDelaunayTriangulator;
                ConstrainedDelaunayTriangulator::triangulatePolygon(p, triangles);

                triCoords.reserve(3 * triangles.size());

                // Add three coordinates per triangle. Note flipped winding order
                // (want counter clockwise, but GEOS provides clockwise)
                for (const Tri* t : triangles) {
                    triCoords.push_back(t->getCoordinate(0));
                    triCoords.push_back(t->getCoordinate(2));
                    triCoords.push_back(t->getCoordinate(1));
                }
                _triangleCoordinates = geometryhelper::coordsToGeodetic(triCoords);

                // Boundaries / Lines

                // Normalize to make sure rings have correct orientation
                std::unique_ptr<Polygon> pNormalized = p->clone();
                pNormalized->normalize();

                const geos::geom::LinearRing* outerRing = pNormalized->getExteriorRing();
                std::vector<Geodetic3> outerBoundsGeoCoords =
                    geometryhelper::geometryCoordsAsGeoVector(outerRing);

                if (!outerBoundsGeoCoords.empty()) {
                    int nHoles = static_cast<int>(pNormalized->getNumInteriorRing());
                    _geoCoordinates.reserve(nHoles + 1);

                    // Outer bounds
                    _geoCoordinates.push_back(outerBoundsGeoCoords);

                    // Inner bounds (holes)
                    for (int i = 0; i < nHoles; ++i) {
                        const geos::geom::LinearRing* hole = pNormalized->getInteriorRingN(i);
                        std::vector<Geodetic3> ringGeoCoords =
                            geometryhelper::geometryCoordsAsGeoVector(hole);

                        _geoCoordinates.push_back(ringGeoCoords);
                    }
                }

                _type = GeometryType::Polygon;
            }
            catch (geos::util::IllegalStateException&) {
                LERROR("Non-simple (e.g. self-intersecting) polygons not supported yet");
                throw ghoul::MissingCaseException();

                // TODO: handle self-intersections points
                // https://www.sciencedirect.com/science/article/pii/S0304397520304199
            }
            break;
        }
        default:
            throw ghoul::MissingCaseException();
    }

    // Compute reference positions to use for checking if height map changes
    geos::geom::Coordinate centroid;
    geo->getCentroid(centroid);
    Geodetic3 geoCentroid = geometryhelper::coordsToGeodetic({ centroid }).front();
    _heightUpdateReferencePoints.push_back(std::move(geoCentroid));
;
    std::vector<Geodetic3> envelopeGeoCoords =
        geometryhelper::geometryCoordsAsGeoVector(geo->getEnvelope().get());

    _heightUpdateReferencePoints.insert(
        _heightUpdateReferencePoints.end(),
        envelopeGeoCoords.begin(),
        envelopeGeoCoords.end()
    );

    if (_properties.overrideValues.name.has_value()) {
        _key = *_properties.overrideValues.name;
    }
    else {
        _key = fmt::format("Feature {} - {}", index, geo->getGeometryType());
    }
}

void GlobeGeometryFeature::render(const RenderData& renderData, int pass,
                                 float mainOpacity,
                         const rendering::helper::LightSourceRenderData& lightSourceData)
{
    ghoul_assert(pass >= 0 && pass < 2, "Render pass variable out of accepted range");

    float opacity = mainOpacity * _properties.opacity();
    float fillOpacity = mainOpacity * _properties.fillOpacity();

    const glm::dmat4 globeModelTransform = _globe.modelTransform();
    const glm::dmat4 modelViewTransform =
        renderData.camera.combinedViewMatrix() * globeModelTransform;

    const glm::mat3 normalTransform = glm::mat3(
        glm::transpose(glm::inverse(modelViewTransform))
    );

    const glm::dmat4 projectionTransform = renderData.camera.projectionMatrix();

#ifndef __APPLE__
    glLineWidth(_properties.lineWidth());
#else
    glLineWidth(1.f);
#endif

    for (const RenderFeature& r : _renderFeatures) {
        if (r.isExtrusionFeature && !_properties.extrude()) {
            continue;
        }

        bool shouldRenderTwice = r.type == RenderType::Polygon &&
            fillOpacity < 1.0 && _properties.extrude();

        if (pass > 0 && !shouldRenderTwice) {
            continue;
        }

        ghoul::opengl::ProgramObject* shader = (r.type == RenderType::Points) ?
            _pointsProgram : _linesAndPolygonsProgram;

        shader->activate();
        shader->setUniform("opacity", opacity);
        shader->setUniform("modelTransform", globeModelTransform);
        shader->setUniform("viewTransform", renderData.camera.combinedViewMatrix());
        shader->setUniform("projectionTransform", projectionTransform);

        shader->setUniform("heightOffset", _offsets.z);
        shader->setUniform("useHeightMapData", useHeightMap());

        if (shader == _linesAndPolygonsProgram) {
            shader->setUniform("normalTransform", normalTransform);
            shader->setUniform("nLightSources", lightSourceData.nLightSources);
            shader->setUniform("lightIntensities", lightSourceData.intensitiesBuffer);
            shader->setUniform(
                "lightDirectionsViewSpace",
                lightSourceData.directionsViewSpaceBuffer
            );
        }
        else {
            // Points => render as billboard
            glm::dvec3 cameraViewDirectionWorld = -renderData.camera.viewDirectionWorldSpace();
            glm::dvec3 cameraUpDirectionWorld = renderData.camera.lookUpVectorWorldSpace();
            glm::dvec3 orthoRight = glm::normalize(
                glm::cross(cameraUpDirectionWorld, cameraViewDirectionWorld)
            );
            if (orthoRight == glm::dvec3(0.0)) {
                glm::dvec3 otherVector = glm::vec3(
                    cameraUpDirectionWorld.y,
                    cameraUpDirectionWorld.x,
                    cameraUpDirectionWorld.z
                );
                orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirectionWorld));
            }
            glm::dvec3 orthoUp = glm::normalize(glm::cross(cameraViewDirectionWorld, orthoRight));

            shader->setUniform("up", glm::vec3(orthoUp));
            shader->setUniform("right", glm::vec3(orthoRight));
        }

        glBindVertexArray(r.vaoId);

        switch (r.type) {
            case RenderType::Lines:
                renderLines(r);
                break;
            case RenderType::Points:
                renderPoints(r);
                break;
            case RenderType::Polygon: {
                shader->setUniform("opacity", fillOpacity);
                renderPolygons(r, shouldRenderTwice, pass);
                break;
            }
            default:
                throw ghoul::MissingCaseException();
                break;
        }

        shader->deactivate();
    }

    glBindVertexArray(0);

    // Reset after every geometry
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
}

void GlobeGeometryFeature::renderPoints(const RenderFeature& feature) const {
    ghoul_assert(feature.type == RenderType::Points, "Trying to render faulty geometry");
    _pointsProgram->setUniform("color", _properties.color());
    _pointsProgram->setUniform(
        "pointSize",
        0.001f * _properties.pointSize() * static_cast<float>(_globe.boundingSphere())
    );

    if (_pointTexture) {
        ghoul::opengl::TextureUnit unit;
        unit.activate();
        _pointTexture->bind();
        _pointsProgram->setUniform("pointTexture", unit);
        _pointsProgram->setUniform("hasTexture", true);

        float widthHeightRatio = static_cast<float>(_pointTexture->texture()->width()) /
            static_cast<float>(_pointTexture->texture()->height());
        _pointsProgram->setUniform("textureWidthFactor", widthHeightRatio);
    }
    else {
        glBindTexture(GL_TEXTURE_2D, 0);
        _pointsProgram->setUniform("hasTexture", false);
    }
    glEnable(GL_PROGRAM_POINT_SIZE);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(feature.nVertices));
    glDisable(GL_PROGRAM_POINT_SIZE);
}

void GlobeGeometryFeature::renderLines(const RenderFeature& feature) const
{
    ghoul_assert(feature.type == RenderType::Lines, "Trying to render faulty geometry");

    _linesAndPolygonsProgram->setUniform("color", _properties.color());
    _linesAndPolygonsProgram->setUniform("performShading", false);

    glEnable(GL_LINE_SMOOTH);
    glDrawArrays(GL_LINE_STRIP, 0, static_cast<GLsizei>(feature.nVertices));
    glDisable(GL_LINE_SMOOTH);
}

void GlobeGeometryFeature::renderPolygons(const RenderFeature& feature,
                                          bool shouldRenderTwice, int renderPass) const
{
    ghoul_assert(
        feature.type == RenderType::Polygon,
        "Trying to render faulty geometry"
    );

    _linesAndPolygonsProgram->setUniform("color", _properties.fillColor());
    _linesAndPolygonsProgram->setUniform("performShading", _properties.performShading());

    if (shouldRenderTwice) {
        glEnable(GL_CULL_FACE);
        if (renderPass == 0) {
            // First draw back faces
            glCullFace(GL_FRONT);
        }
        else {
            // Then front faces
            glCullFace(GL_BACK);
        }
    }
    else {
        glDisable(GL_CULL_FACE);
    }
    glDrawArrays(GL_TRIANGLES, 0, static_cast<GLsizei>(feature.nVertices));
}

bool GlobeGeometryFeature::shouldUpdateDueToHeightMapChange() const {
    if (_properties.altitudeMode() == GeoJsonProperties::AltitudeMode::RelativeToGround) {
        // Cap the update to a given time interval
        std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
        if (now - _lastHeightUpdateTime < HeightUpdateInterval) {
            return false;
        }

        // TODO: Change computation so that we return true immediately if even one height value is different

        // Check if last height values for the control positions have changed
        std::vector<double> newHeights = getCurrentReferencePointsHeights();

        bool isSame = std::equal(
            _lastControlHeights.begin(),
            _lastControlHeights.end(),
            newHeights.begin(),
            newHeights.end(),
            [](double a, double b) {
                return std::abs(a - b) < std::numeric_limits<double>::epsilon();
            }
        );

        if (!isSame) {
            return true;
        }
    }
    return false;
}

void GlobeGeometryFeature::update(bool dataIsDirty, bool preventHeightUpdates) {
    if (!preventHeightUpdates && shouldUpdateDueToHeightMapChange()) {
        updateHeightsFromHeightMap();
    }
    else if (dataIsDirty) {
        updateGeometry();
    }

    if (_pointTexture) {
        _pointTexture->update();
    }
}

void GlobeGeometryFeature::updateGeometry() {
    // Update vertex data and compute model coordinates based on globe
    _renderFeatures.clear();

    if (_type == GeometryType::Point) {
        createPointGeometry();
    }
    else {
        std::vector<std::vector<glm::vec3>> edgeVertices = createLineGeometry();
        createExtrudedGeometry(edgeVertices);
        createPolygonGeometry();
    }

    // Compute new heights - to see if height map changed
    _lastControlHeights = getCurrentReferencePointsHeights();
}

void GlobeGeometryFeature::updateHeightsFromHeightMap() {
    // @TODO: do the updating piece by piece, not all in one frame
    for (RenderFeature& f : _renderFeatures) {
        f.heights = geometryhelper::heightMapHeightsFromGeodetic2List(
            _globe,
            f.vertices
        );
        bufferDynamicHeightData(f);
    }

    _lastHeightUpdateTime = std::chrono::system_clock::now();
}

std::vector<std::vector<glm::vec3>> GlobeGeometryFeature::createLineGeometry() {
    std::vector<std::vector<glm::vec3>> resultPositions;
    resultPositions.reserve(_geoCoordinates.size());

    for (int i = 0; i < _geoCoordinates.size(); ++i) {
        std::vector<Vertex> vertices;
        std::vector<glm::vec3> positions;
        vertices.reserve(_geoCoordinates[i].size() * 3); // TODO: this is not correct anymore
        positions.reserve(_geoCoordinates[i].size() * 3); // TODO: this is not correct anymore

        glm::dvec3 lastPos = glm::dvec3(0.0);
        double lastHeightValue = 0.0;

        bool isFirst = true;
        for (const Geodetic3& geodetic : _geoCoordinates[i]) {
            glm::dvec3 v = geometryhelper::computeOffsetedModelCoordinate(
                geodetic,
                _globe,
                _offsets.x,
                _offsets.y
            );

            // If we are drawing points, just add and continue.
            // Note that we set the normal
            if (_type == GeometryType::Point) {
                glm::vec3 vf = static_cast<glm::vec3>(v);
                glm::vec3 normal = glm::normalize(vf);
                vertices.push_back({ vf.x, vf.y, vf.z, normal.x, normal.y, normal.z });
                positions.push_back(vf);
                continue;
            }
            // Else, we're rendering lines and need to subdivide

            if (isFirst) {
                lastPos = v;
                lastHeightValue = geodetic.height;
                isFirst = false;
                continue;
            }

            // Add extra vertices to fulfill MaxDistance criteria.
            // First determine how much to tesselate
            float stepSize = determineTesselationDistance(glm::distance(lastPos, v));

            std::vector<geometryhelper::PosHeightPair> subdividedPositions =
                geometryhelper::subdivideLine(
                    lastPos,
                    v,
                    lastHeightValue,
                    geodetic.height,
                    stepSize
                );

            for (const geometryhelper::PosHeightPair& pair : subdividedPositions) {
                glm::vec3 p = pair.position;
                vertices.push_back({ p.x, p.y, p.z, 0.f, 0.f, 0.f });
                positions.push_back(p);
            }

            lastPos = v;
            lastHeightValue = geodetic.height;
        }

        vertices.shrink_to_fit();

        RenderFeature feature;
        feature.nVertices = vertices.size();
        feature.type = RenderType::Lines;
        initializeRenderFeature(feature, vertices);
        _renderFeatures.push_back(std::move(feature));

        positions.shrink_to_fit();
        resultPositions.push_back(std::move(positions));
    }

    resultPositions.shrink_to_fit();
    return resultPositions;
}

void GlobeGeometryFeature::createPointGeometry() {
    if (_type != GeometryType::Point) {
        return;
    }

    for (int i = 0; i < _geoCoordinates.size(); ++i) {
        std::vector<Vertex> vertices;
        vertices.reserve(_geoCoordinates[i].size());

        std::vector<Vertex> extrudedLineVertices;
        extrudedLineVertices.reserve(2 * _geoCoordinates[i].size());

        for (const Geodetic3& geodetic : _geoCoordinates[i]) {
            glm::dvec3 v = geometryhelper::computeOffsetedModelCoordinate(
                geodetic,
                _globe,
                _offsets.x,
                _offsets.y
            );

            glm::vec3 vf = static_cast<glm::vec3>(v);
            glm::vec3 normal = glm::normalize(vf);
            vertices.push_back({ vf.x, vf.y, vf.z, normal.x, normal.y, normal.z });

            // Lines from center of the globe out to the point
            extrudedLineVertices.push_back({ 0.f, 0.f, 0.f, 0.f, 0.f, 0.f });
            extrudedLineVertices.push_back({ vf.x, vf.y, vf.z, 0.f, 0.f, 0.f });
        }

        vertices.shrink_to_fit();
        extrudedLineVertices.shrink_to_fit();

        RenderFeature feature;
        feature.nVertices = vertices.size();
        feature.type = RenderType::Points;
        initializeRenderFeature(feature, vertices);
        _renderFeatures.push_back(std::move(feature));

        // Create extrusion feature

        RenderFeature extrudeFeature;
        extrudeFeature.nVertices = extrudedLineVertices.size();
        extrudeFeature.type = RenderType::Lines;
        extrudeFeature.isExtrusionFeature = true;
        initializeRenderFeature(extrudeFeature, extrudedLineVertices);
        _renderFeatures.push_back(std::move(extrudeFeature));
    }
}

void GlobeGeometryFeature::createExtrudedGeometry(
                                 const std::vector<std::vector<glm::vec3>>& edgeVertices)
{
    if (edgeVertices.empty()) {
        return;
    }

    std::vector<Vertex> vertices =
        geometryhelper::createExtrudedGeometryVertices(edgeVertices);

    RenderFeature feature;
    feature.type = RenderType::Polygon;
    feature.nVertices = vertices.size();
    feature.isExtrusionFeature = true;
    initializeRenderFeature(feature, vertices);
    _renderFeatures.push_back(std::move(feature));
}

void GlobeGeometryFeature::initializeRenderFeature(RenderFeature& feature,
                                                   const std::vector<Vertex>& vertices)
{
    // Get height map heights
    feature.vertices = geometryhelper::geodetic2FromVertexList(_globe, vertices);
    feature.heights = geometryhelper::heightMapHeightsFromGeodetic2List(
        _globe,
        feature.vertices
    );

    // Generate buffers and buffer data
    feature.initializeBuffers();
    bufferVertexData(feature, vertices);
}

void GlobeGeometryFeature::createPolygonGeometry() {
    if (_triangleCoordinates.empty()) {
        return;
    }

    std::vector<Vertex> polyVertices;

    // Create polygon vertices from the triangle coordinates
    int triIndex = 0;
    std::array<glm::vec3, 3> triPositions;
    std::array<double, 3> triHeights;
    for (const Geodetic3& geodetic : _triangleCoordinates) {
        const glm::vec3 vert = geometryhelper::computeOffsetedModelCoordinate(
            geodetic,
            _globe,
            _offsets.x,
            _offsets.y
        );
        triPositions[triIndex] = vert;
        triHeights[triIndex] = geodetic.height;
        triIndex++;

        // Once we have a triangle, start subdividing
        if (triIndex == 3) {
            triIndex = 0;

            const glm::vec3 v0 = triPositions[0];
            const glm::vec3 v1 = triPositions[1];
            const glm::vec3 v2 = triPositions[2];

            double h0 = triHeights[0];
            double h1 = triHeights[1];
            double h2 = triHeights[2];

            if (_properties.shouldtesselate() && _properties.tesselationLevel() > 0.f) {
                // First determine how much to tesselate
                float longestSide = std::max(
                    std::max(glm::distance(v0, v1), glm::distance(v0, v2)),
                    glm::distance(v1, v2)
                );
                float stepSize = determineTesselationDistance(longestSide);

                std::vector<Vertex> verts = geometryhelper::subdivideTriangle(
                    v0, v1, v2,
                    h0, h1, h2,
                    stepSize,
                    _globe
                );
                polyVertices.insert(polyVertices.end(), verts.begin(), verts.end());
            }
            else {
                // Just add a triangle consisting of the three vertices
                const glm::vec3 n = -glm::normalize(glm::cross(v1 - v0, v2 - v0));
                polyVertices.push_back({ v0.x, v0.y, v0.z, n.x, n.y, n.z });
                polyVertices.push_back({ v1.x, v1.y, v1.z, n.x, n.y, n.z });
                polyVertices.push_back({ v2.x, v2.y, v2.z, n.x, n.y, n.z });
            }
        }
    }

    RenderFeature triFeature;
    triFeature.type = RenderType::Polygon;
    triFeature.nVertices = polyVertices.size();
    initializeRenderFeature(triFeature, polyVertices);
    _renderFeatures.push_back(std::move(triFeature));
}

std::vector<double> GlobeGeometryFeature::getCurrentReferencePointsHeights() const {
    std::vector<double> newHeights;
    newHeights.reserve(_heightUpdateReferencePoints.size());
    for (const Geodetic3& geo : _heightUpdateReferencePoints) {
        const glm::dvec3 p = geometryhelper::computeOffsetedModelCoordinate(
            geo,
            _globe,
            _offsets.x,
            _offsets.y
        );
        const SurfacePositionHandle handle = _globe.calculateSurfacePositionHandle(p);
        newHeights.push_back(handle.heightToSurface);
    }
    return newHeights;
}

float GlobeGeometryFeature::determineTesselationDistance(float objectSize) const {
    float tesselationFactor = 1.f / static_cast<float>(_properties.tesselationLevel());
    float distance = tesselationFactor * _properties.tesselationMaxDistance();

    // If object size is smaller than the allowed max distance, instead split the
    // distance into the number of steps given by the tesselationlevel property
    if (objectSize < _properties.tesselationMaxDistance()) {
        distance = tesselationFactor * objectSize;
    }
    // @TODO: verify that this will be ok. Maybe we shoud just simplify it?
    // Also, make something that takes the height into account
    return distance;
}

void GlobeGeometryFeature::bufferVertexData(const RenderFeature& feature,
                                            const std::vector<Vertex>& vertexData)
{
    ghoul_assert(_pointsProgram != nullptr, "Shader program must be initialized");
    ghoul_assert(_linesAndPolygonsProgram != nullptr, "Shader program must be initialized");

    ghoul::opengl::ProgramObject* program = _linesAndPolygonsProgram;
    if (feature.type == RenderType::Points) {
        program = _pointsProgram;
    }

    // Reserve space for both vertex and dynamic height information
    auto fullBufferSize = vertexData.size() * (sizeof(Vertex) +sizeof(float));

    glBindVertexArray(feature.vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, feature.vboId);
    glBufferData(
        GL_ARRAY_BUFFER,
        fullBufferSize,
        nullptr,
        GL_STATIC_DRAW
    );

    glBufferSubData(
        GL_ARRAY_BUFFER,
        0, // offset
        vertexData.size() * sizeof(Vertex), // size
        vertexData.data()
    );

    GLint positionAttrib = program->attributeLocation("in_position");
    glEnableVertexAttribArray(positionAttrib);
    glVertexAttribPointer(
        positionAttrib,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(Vertex),
        nullptr
    );

    GLint normalAttrib = program->attributeLocation("in_normal");
    glEnableVertexAttribArray(normalAttrib);
    glVertexAttribPointer(
        normalAttrib,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(Vertex),
        reinterpret_cast<void*>(3 * sizeof(float))
    );

    // Put height data after all vertex data in buffer
    unsigned long long endOfVertexData = vertexData.size() * sizeof(Vertex);
    glBindVertexArray(feature.vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, feature.vboId);
    glBufferSubData(
        GL_ARRAY_BUFFER,
        endOfVertexData, // offset
        feature.heights.size() * sizeof(float), // size of all height values
        feature.heights.data()
    );

    GLint heightAttrib = program->attributeLocation("in_height");
    glEnableVertexAttribArray(heightAttrib);
    glVertexAttribPointer(
        heightAttrib,
        1,
        GL_FLOAT,
        GL_FALSE,
        1 * sizeof(float), // stride (one height value)
        reinterpret_cast<void*>(endOfVertexData) // start position
    );

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
};

void GlobeGeometryFeature::bufferDynamicHeightData(const RenderFeature& feature) {
    ghoul_assert(_pointsProgram != nullptr, "Shader program must be initialized");
    ghoul_assert(_linesAndPolygonsProgram != nullptr, "Shader program must be initialized");

    ghoul::opengl::ProgramObject* program = _linesAndPolygonsProgram;
    if (feature.type == RenderType::Points) {
        program = _pointsProgram;
    }

    // Just update the height data

    glBindVertexArray(feature.vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, feature.vboId);
    glBufferSubData(
        GL_ARRAY_BUFFER,
        feature.nVertices * sizeof(Vertex), // offset
        feature.heights.size() * sizeof(float), // size
        feature.heights.data()
    );

    GLint heightAttrib = program->attributeLocation("in_height");
    glEnableVertexAttribArray(heightAttrib);
    glVertexAttribPointer(
        heightAttrib,
        1,
        GL_FLOAT,
        GL_FALSE,
        1 * sizeof(float), // stride
        reinterpret_cast<void*>(feature.nVertices * sizeof(Vertex)) // start position
    );

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
};

} // namespace openspace::globebrowsing