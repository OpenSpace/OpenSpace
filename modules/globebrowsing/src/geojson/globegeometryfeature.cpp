/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

void GlobeGeometryFeature::setOffsets(glm::vec3 offsets) {
    _offsets = std::move(offsets);
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
    const bool shadersAreReady = _linesAndPolygonsProgram && _pointsProgram;
    const bool textureIsReady = (!_hasTexture) || _pointTexture;
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
    std::string texture;
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
        _pointTexture->setFilterMode(
            ghoul::opengl::Texture::FilterMode::AnisotropicMipMap
        );
        _pointTexture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);
    }

    std::filesystem::path texturePath = absPath(texture);
    if (std::filesystem::is_regular_file(texturePath)) {
        _hasTexture = true;
        _pointTexture->loadFromFile(texture);
        _pointTexture->uploadToGpu();
    }
    else {
        LERROR(std::format(
            "Trying to use texture file that does not exist: {}", texturePath
        ));
    }
}

void GlobeGeometryFeature::createFromSingleGeosGeometry(const geos::geom::Geometry* geo,
                                                        int index, bool ignoreHeights)
{
    if (!geo) {
        throw std::logic_error("No geometry provided");
    }
    ghoul_assert(
        (geo && geo->isPuntal()) || (geo && !geo->isCollection()),
        "Non-point geometry can not be a collection"
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
                const auto p = dynamic_cast<const geos::geom::Polygon*>(geo);

                // Triangles
                // Note that Constrained Delaunay triangulation supports polygons with
                // holes :)
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
                const std::vector<Geodetic3> outerBoundsGeoCoords =
                    geometryhelper::geometryCoordsAsGeoVector(outerRing);

                if (!outerBoundsGeoCoords.empty()) {
                    const int nHoles = static_cast<int>(
                        pNormalized->getNumInteriorRing()
                    );
                    _geoCoordinates.reserve(nHoles + 1);

                    // Outer bounds
                    _geoCoordinates.push_back(outerBoundsGeoCoords);

                    // Inner bounds (holes)
                    for (int i = 0; i < nHoles; i++) {
                        const geos::geom::LinearRing* hole =
                            pNormalized->getInteriorRingN(i);
                        std::vector<Geodetic3> ringGeoCoords =
                            geometryhelper::geometryCoordsAsGeoVector(hole);
                        _geoCoordinates.push_back(std::move(ringGeoCoords));
                    }
                }

                _type = GeometryType::Polygon;
            }
            catch (geos::util::IllegalStateException& e) {
                throw ghoul::RuntimeError(std::format(
                    "Non-simple (e.g. self-intersecting) polygons not supported yet. "
                    "GEOS error: {}", e.what()
                ));

                // TODO: handle self-intersections points
                // https://www.sciencedirect.com/science/article/pii/S0304397520304199
            }
            catch (geos::util::GEOSException& e) {
                throw ghoul::RuntimeError(std::format(
                    "Unknown geos error: {}", e.what()
                ));
            }
            break;
        }
        default:
            throw ghoul::MissingCaseException();
    }

    // Reset height values if we don't care about them
    if (ignoreHeights) {
        for (std::vector<Geodetic3>& vec : _geoCoordinates) {
            for (Geodetic3& coord : vec) {
                coord.height = 0.0;
            }
        }
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
        _key = std::format("Feature {} - {}", index, geo->getGeometryType());
    }
}

void GlobeGeometryFeature::render(const RenderData& renderData, int pass,
                                  float mainOpacity,
                                  const ExtraRenderData& extraRenderData)
{
    ghoul_assert(pass >= 0 && pass < 2, "Render pass variable out of accepted range");

    const float opacity = mainOpacity * _properties.opacity();
    const float fillOpacity = mainOpacity * _properties.fillOpacity();

    const glm::dmat4 globeModelTransform = _globe.modelTransform();
    const glm::dmat4 modelViewTransform =
        renderData.camera.combinedViewMatrix() * globeModelTransform;

    const glm::mat3 normalTransform = glm::mat3(
        glm::transpose(glm::inverse(modelViewTransform))
    );

    const glm::dmat4 projectionTransform = renderData.camera.projectionMatrix();

#ifndef __APPLE__
    glLineWidth(_properties.lineWidth() * extraRenderData.lineWidthScale);
#else
    glLineWidth(1.f);
#endif

    for (const RenderFeature& r : _renderFeatures) {
        if (r.isExtrusionFeature && !_properties.extrude()) {
            continue;
        }

        const bool shouldRenderTwice = r.type == RenderType::Polygon &&
            fillOpacity < 1.f && _properties.extrude();

        if (pass > 0 && !shouldRenderTwice) {
            continue;
        }

        ghoul::opengl::ProgramObject* shader = (r.type == RenderType::Points) ?
            _pointsProgram : _linesAndPolygonsProgram;

        shader->activate();
        shader->setUniform("modelTransform", globeModelTransform);
        shader->setUniform("viewTransform", renderData.camera.combinedViewMatrix());
        shader->setUniform("projectionTransform", projectionTransform);

        shader->setUniform("heightOffset", _offsets.z);
        shader->setUniform("useHeightMapData", useHeightMap());

        if (shader == _linesAndPolygonsProgram) {
            const rendering::helper::LightSourceRenderData& ls =
                extraRenderData.lightSourceData;
            shader->setUniform("normalTransform", normalTransform);
            shader->setUniform("nLightSources", ls.nLightSources);
            shader->setUniform("lightIntensities", ls.intensitiesBuffer);
            shader->setUniform("lightDirectionsViewSpace", ls.directionsViewSpaceBuffer);
        }

        glBindVertexArray(r.vaoId);

        switch (r.type) {
            case RenderType::Lines:
                shader->setUniform(
                    "opacity",
                    r.isExtrusionFeature ? fillOpacity : opacity
                );
                renderLines(r);
                break;
            case RenderType::Points: {
                shader->setUniform("opacity", opacity);
                const float scale = extraRenderData.pointSizeScale;
                renderPoints(r, renderData, extraRenderData.pointRenderMode, scale);
                break;
            }
            case RenderType::Polygon: {
                shader->setUniform("opacity", fillOpacity);
                renderPolygons(r, shouldRenderTwice, pass);
                break;
            }
            default:
                throw ghoul::MissingCaseException();
        }

        shader->deactivate();
    }

    glBindVertexArray(0);

    // Reset when we're done rendering all the polygon features
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
}

void GlobeGeometryFeature::renderPoints(const RenderFeature& feature,
                                        const RenderData& renderData,
                                        const PointRenderMode& renderMode,
                                        float sizeScale) const
{
    ghoul_assert(feature.type == RenderType::Points, "Trying to render faulty geometry");
    _pointsProgram->setUniform("color", _properties.color());

    const float bs = static_cast<float>(_globe.boundingSphere());
    const float size = 0.001f * sizeScale * _properties.pointSize() * bs;
    _pointsProgram->setUniform("pointSize", size);

    _pointsProgram->setUniform("renderMode", static_cast<int>(renderMode));

    using TextureAnchor = GeoJsonProperties::PointTextureAnchor;
    _pointsProgram->setUniform(
        "useBottomAnchorPoint",
        _properties.pointTextureAnchor() != TextureAnchor::Center
    );

    // Points are rendered as billboards
    const glm::dvec3 cameraViewDirWorld = -renderData.camera.viewDirectionWorldSpace();
    const glm::dvec3 cameraUpDirWorld = renderData.camera.lookUpVectorWorldSpace();
    glm::dvec3 orthoRight = glm::normalize(
        glm::cross(cameraUpDirWorld, cameraViewDirWorld)
    );
    if (orthoRight == glm::dvec3(0.0)) {
        // For some reason, the up vector and camera view vector were the same. Use a
        // slightly different vector
        const glm::dvec3 otherVector = glm::vec3(
            cameraUpDirWorld.y,
            cameraUpDirWorld.x,
            cameraUpDirWorld.z
        );
        orthoRight = glm::normalize(glm::cross(otherVector, cameraViewDirWorld));
    }
    const glm::dvec3 orthoUp = glm::normalize(glm::cross(cameraViewDirWorld, orthoRight));

    _pointsProgram->setUniform("cameraUp", glm::vec3(orthoUp));
    _pointsProgram->setUniform("cameraRight", glm::vec3(orthoRight));

    const glm::dvec3 cameraPositionWorld = renderData.camera.positionVec3();
    _pointsProgram->setUniform("cameraPosition", cameraPositionWorld);
    _pointsProgram->setUniform("cameraLookUp", glm::vec3(cameraUpDirWorld));

    if (_pointTexture && _hasTexture) {
        ghoul::opengl::TextureUnit unit;
        unit.activate();
        _pointTexture->bind();
        _pointsProgram->setUniform("pointTexture", unit);
        _pointsProgram->setUniform("hasTexture", true);

        const float widthHeightRatio =
            static_cast<float>(_pointTexture->texture()->width()) /
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

void GlobeGeometryFeature::renderLines(const RenderFeature& feature) const {
    ghoul_assert(feature.type == RenderType::Lines, "Trying to render faulty geometry");

    const glm::vec3 color = feature.isExtrusionFeature ?
        _properties.fillColor() : _properties.color();

    _linesAndPolygonsProgram->setUniform("color", color);
    _linesAndPolygonsProgram->setUniform("performShading", false);

    glEnable(GL_LINE_SMOOTH);
    glDrawArrays(GL_LINE_STRIP, 0, static_cast<GLsizei>(feature.nVertices));
    glDisable(GL_LINE_SMOOTH);
}

void GlobeGeometryFeature::renderPolygons(const RenderFeature& feature,
                                          bool shouldRenderTwice, int renderPass) const
{
    ghoul_assert(renderPass == 0 || renderPass == 1, "Invalid render pass");
    ghoul_assert(
        feature.type == RenderType::Polygon,
        "Trying to render faulty geometry"
    );

    _linesAndPolygonsProgram->setUniform("color", _properties.fillColor());
    _linesAndPolygonsProgram->setUniform("performShading", _properties.performShading());

    if (shouldRenderTwice) {
        glEnable(GL_CULL_FACE);
        // First draw back faces, then front faces
        glCullFace(renderPass == 0 ? GL_FRONT : GL_BACK);
    }
    else {
        glDisable(GL_CULL_FACE);
    }
    glDrawArrays(GL_TRIANGLES, 0, static_cast<GLsizei>(feature.nVertices));
}

bool GlobeGeometryFeature::shouldUpdateDueToHeightMapChange() const {
    if (_properties.altitudeMode() == GeoJsonProperties::AltitudeMode::RelativeToGround) {
        // Cap the update to a given time interval
        const auto now = std::chrono::system_clock::now();
        if (now - _lastHeightUpdateTime < HeightUpdateInterval) {
            return false;
        }

        // TODO: Change computation so that we return true immediately if even one height
        // value is different

        // Check if last height values for the control positions have changed
        std::vector<double> newHeights = getCurrentReferencePointsHeights();

        const bool isSame = std::equal(
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
        const std::vector<std::vector<glm::vec3>> edgeVertices = createLineGeometry();
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
    for (const std::vector<Geodetic3>& coordinates : _geoCoordinates) {
        std::vector<Vertex> vertices;
        std::vector<glm::vec3> positions;
        // TODO: this is not correct anymore
        vertices.reserve(coordinates.size() * 3);
        // TODO: this is not correct anymore
        positions.reserve(coordinates.size() * 3);

        glm::dvec3 lastPos = glm::dvec3(0.0);
        double lastHeightValue = 0.0;

        bool isFirst = true;
        for (const Geodetic3& geodetic : coordinates) {
            const glm::dvec3 v = geometryhelper::computeOffsetedModelCoordinate(
                geodetic,
                _globe,
                _offsets.x,
                _offsets.y
            );

            const auto addLinePos = [&vertices, &positions](const glm::vec3& pos) {
                vertices.push_back({ pos.x, pos.y, pos.z, 0.f, 0.f, 0.f });
                positions.push_back(pos);
            };

            if (isFirst) {
                lastPos = v;
                lastHeightValue = geodetic.height;
                isFirst = false;
                addLinePos(glm::vec3(v));
                continue;
            }

            if (_properties.tessellationEnabled()) {
                // Tessellate.
                // But first, determine the step size for the tessellation (larger
                // features will not be tesselated)
                const float stepSize = tessellationStepSize();

                std::vector<geometryhelper::PosHeightPair> subdividedPositions =
                    geometryhelper::subdivideLine(
                        lastPos,
                        v,
                        lastHeightValue,
                        geodetic.height,
                        stepSize
                    );

                // Don't add the first position. Has been added as last in previous step
                for (size_t si = 1; si < subdividedPositions.size(); ++si) {
                    const geometryhelper::PosHeightPair& pair = subdividedPositions[si];
                    addLinePos(glm::vec3(pair.position));
                }
            }
            else {
                // Just add the line point
                addLinePos(glm::vec3(v));
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

    for (const std::vector<Geodetic3>& coordinates : _geoCoordinates) {
        std::vector<Vertex> vertices;
        vertices.reserve(coordinates.size());

        std::vector<Vertex> extrudedLineVertices;
        extrudedLineVertices.reserve(2 * coordinates.size());

        for (const Geodetic3& geodetic : coordinates) {
            const glm::dvec3 v = geometryhelper::computeOffsetedModelCoordinate(
                geodetic,
                _globe,
                _offsets.x,
                _offsets.y
            );

            const glm::vec3 vf = static_cast<glm::vec3>(v);
            // Normal is the out direction
            const glm::vec3 normal = glm::normalize(vf);

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

    const std::vector<Vertex> vertices = geometryhelper::createExtrudedGeometryVertices(
        edgeVertices
    );

    RenderFeature feature;
    feature.type = RenderType::Polygon;
    feature.nVertices = vertices.size();
    feature.isExtrusionFeature = true;
    initializeRenderFeature(feature, vertices);
    _renderFeatures.push_back(std::move(feature));
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

            const double h0 = triHeights[0];
            const double h1 = triHeights[1];
            const double h2 = triHeights[2];

            if (_properties.tessellationEnabled()) {
                // First determine the step size for the tessellation (larger features
                // will not be tesselated)
                const float stepSize = tessellationStepSize();

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

float GlobeGeometryFeature::tessellationStepSize() const {
    float distance = _properties.tessellationDistance();
    const bool shouldDivideDistance = _properties.useTessellationLevel() &&
        _properties.tessellationLevel() > 0;

    if (shouldDivideDistance) {
        distance /= static_cast<float>(_properties.tessellationLevel());
    }

    return distance;
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

void GlobeGeometryFeature::bufferVertexData(const RenderFeature& feature,
                                            const std::vector<Vertex>& vertexData)
{
    ghoul_assert(_pointsProgram, "Shader program must be initialized");
    ghoul_assert(_linesAndPolygonsProgram, "Shader program must be initialized");

    ghoul::opengl::ProgramObject* program = _linesAndPolygonsProgram;
    if (feature.type == RenderType::Points) {
        program = _pointsProgram;
    }

    // Reserve space for both vertex and dynamic height information
    auto fullBufferSize = vertexData.size() * (sizeof(Vertex) + sizeof(float));

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

    const GLint positionAttrib = program->attributeLocation("in_position");
    glEnableVertexAttribArray(positionAttrib);
    glVertexAttribPointer(
        positionAttrib,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(Vertex),
        nullptr
    );

    const GLint normalAttrib = program->attributeLocation("in_normal");
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
    const unsigned long long endOfVertexData = vertexData.size() * sizeof(Vertex);
    glBindVertexArray(feature.vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, feature.vboId);
    glBufferSubData(
        GL_ARRAY_BUFFER,
        endOfVertexData, // offset
        feature.heights.size() * sizeof(float), // size of all height values
        feature.heights.data()
    );

    const GLint heightAttrib = program->attributeLocation("in_height");
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
}

void GlobeGeometryFeature::bufferDynamicHeightData(const RenderFeature& feature) {
    ghoul_assert(_pointsProgram, "Shader program must be initialized");
    ghoul_assert(_linesAndPolygonsProgram, "Shader program must be initialized");

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

    const GLint heightAttrib = program->attributeLocation("in_height");
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
}

} // namespace openspace::globebrowsing
