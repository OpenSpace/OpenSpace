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
#include <geos/triangulate/DelaunayTriangulationBuilder.h>
#include <geos/triangulate/polygon/ConstrainedDelaunayTriangulator.h>
#include <geos/triangulate/quadedge/QuadEdgeSubdivision.h>
#include <geos/util/IllegalStateException.h>
#include <algorithm>
#include <filesystem>
#include <fstream>

namespace {
    constexpr const char* _loggerCat = "GlobeGeometryFeature";

    // Max distance between two coordinates in meter
    // TODO: Make a configurable property. Automatically generated from the size of the object
    const double MaxDistance = 10000.0;

} // namespace

namespace openspace::globebrowsing {

GlobeGeometryFeature::GlobeGeometryFeature(RenderableGlobe& globe,
                                           GeoJsonProperties& defaultProperties,
                                           GeoJsonOverrideProperties& overrideProperties)
    : properties::PropertyOwner({ "GlobeGeometryFeature" })
    , _globe(globe)
    , _properties({ defaultProperties, overrideProperties })
{}

std::string GlobeGeometryFeature::key() const {
    return _key;
}

bool GlobeGeometryFeature::enabled() const {
    return _isEnabled;
}

void GlobeGeometryFeature::setEnabled(bool value) {
    _isEnabled = value;
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
        GlobeBrowsingModule* m = global::moduleEngine->module<GlobeBrowsingModule>();
        if (m->hasDefaultGeoPointTexture()) {
            _pointTexture = std::make_unique<TextureComponent>(2);
            _pointTexture->setFilterMode(ghoul::opengl::Texture::FilterMode::AnisotropicMipMap);
            _pointTexture->setWrapping(ghoul::opengl::Texture::WrappingMode::ClampToEdge);

            // TODO: option to use texture from properties
            _pointTexture->loadFromFile(m->defaultGeoPointTexture());
            _pointTexture->uploadToGpu();
        }
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
    return (_linesAndPolygonsProgram != nullptr) && (_pointsProgram != nullptr);
}

bool GlobeGeometryFeature::isPoints() const {
    return _type == GeometryType::Point;
}

bool GlobeGeometryFeature::useHeightMap() const {
    return _properties.altitudeMode() ==
        GeoJsonProperties::AltitudeMode::RelativeToGround;
}

void GlobeGeometryFeature::createFromSingleGeosGeometry(const geos::geom::Geometry* geo,
                                                        int index)
{
    ghoul_assert(!geo->isCollection(), "Geometry can not be a collection");

    switch (geo->getGeometryTypeId()) {
        case geos::geom::GEOS_POINT:
        case geos::geom::GEOS_MULTIPOINT: {
            std::vector<geos::geom::Coordinate> coords;
            geo->getCoordinates()->toVector(coords);
            _geoCoordinates.push_back(geometryhelper::coordsToGeodetic(coords));
            _type = GeometryType::Point;
            break;
        }
        case geos::geom::GEOS_LINESTRING: {
            std::vector<geos::geom::Coordinate> coords;
            geo->getCoordinates()->toVector(coords);
            _geoCoordinates.push_back(geometryhelper::coordsToGeodetic(coords));
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

                std::vector<geos::geom::Coordinate> outerBounds;
                pNormalized->getExteriorRing()->getCoordinates()->toVector(outerBounds);

                if (!outerBounds.empty()) {
                    int nHoles = static_cast<int>(pNormalized->getNumInteriorRing());
                    _geoCoordinates.reserve(nHoles + 1);

                    // Outer bounds
                    _geoCoordinates.push_back(geometryhelper::coordsToGeodetic(outerBounds));

                    // Inner bounds (holes)
                    for (int i = 0; i < nHoles; ++i) {
                        std::vector<geos::geom::Coordinate> ringCoords;
                        pNormalized->getInteriorRingN(i)->getCoordinates()->toVector(ringCoords);
                        _geoCoordinates.push_back(geometryhelper::coordsToGeodetic(ringCoords));
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

    auto envelope = geo->getEnvelope();
    std::vector<geos::geom::Coordinate> coords;
    envelope->getCoordinates()->toVector(coords);

    std::vector<Geodetic3> envelopeGeoCoords = geometryhelper::coordsToGeodetic({ coords });
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

    if (!_isEnabled) {
        return;
    }

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

    // @TODO: optimise as to not loop through all objects twice (based on
    // the render pass), but only the ones for which it's actually needed
    for (const RenderFeature& r : _renderFeatures) {
        if (r.isExtrusionFeature && !_properties.extrude()) {
            continue;
        }

        ghoul::opengl::ProgramObject* shader = (r.type == RenderType::Points) ?
            _pointsProgram : _linesAndPolygonsProgram;

        shader->activate();
        shader->setUniform("modelViewTransform", modelViewTransform);
        shader->setUniform("projectionTransform", projectionTransform);
        shader->setUniform("normalTransform", normalTransform);

        shader->setUniform("nLightSources", lightSourceData.nLightSources);
        shader->setUniform("lightIntensities", lightSourceData.intensitiesBuffer);
        shader->setUniform(
            "lightDirectionsViewSpace",
            lightSourceData.directionsViewSpaceBuffer
        );

        shader->setUniform("opacity", opacity);

        glBindVertexArray(r.vaoId);

        switch (r.type) {
            case RenderType::Lines:
                renderLines(r, shader);
                break;
            case RenderType::Points:
                renderPoints(r, shader);
                break;
            case RenderType::Polygon: {
                shader->setUniform("opacity", fillOpacity);

                bool shouldRenderTwice = (fillOpacity < 1.0);
                renderPolygons(r, shader, shouldRenderTwice, pass);
                break;
            }
            default:
                throw ghoul::MissingCaseException();
                break;
        }

        shader->deactivate();
    }

    glBindVertexArray(0);

    // TODO: move to correct render function
    // Reset after every geometry
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
}

void GlobeGeometryFeature::renderPoints(const RenderFeature& feature,
                                        ghoul::opengl::ProgramObject* shader) const
{
    ghoul_assert(feature.type == RenderType::Points, "Trying to render faulty geometry");
    _pointsProgram->setUniform("color", _properties.color());
    _pointsProgram->setUniform("pointSize", _properties.pointSize());

    //if (_pointTexture) {
    //    ghoul::opengl::TextureUnit unit;
    //    unit.activate();
    //    _pointTexture->bind();
    //    shader->setUniform("texture", unit);
    //}
    //else {
    //    glBindTexture(GL_TEXTURE_2D, 0);
    //}
    glEnable(GL_PROGRAM_POINT_SIZE);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(feature.nVertices));
    glDisable(GL_PROGRAM_POINT_SIZE);
    // TODO: support sprites for the points
}

void GlobeGeometryFeature::renderLines(const RenderFeature& feature,
                                       ghoul::opengl::ProgramObject* shader) const
{
    ghoul_assert(feature.type == RenderType::Lines, "Trying to render faulty geometry");

    _linesAndPolygonsProgram->setUniform("color", _properties.color());
    _linesAndPolygonsProgram->setUniform("performShading", false);

    glEnable(GL_LINE_SMOOTH);
    glDrawArrays(GL_LINE_STRIP, 0, static_cast<GLsizei>(feature.nVertices));
    glDisable(GL_LINE_SMOOTH);
}

void GlobeGeometryFeature::renderPolygons(const RenderFeature& feature,
                                          ghoul::opengl::ProgramObject* shader,
                                          bool shouldRenderTwice, int renderPass) const
{
    ghoul_assert(
        feature.type == RenderType::Polygon,
        "Trying to render faulty geometry"
    );

    _linesAndPolygonsProgram->setUniform("color", _properties.fillColor());
    _linesAndPolygonsProgram->setUniform("performShading", true);

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
        // TODO: should just update height, not entire geometry
        //LINFO("Update from height map change"); // TODO: remove
        updateGeometry();
    }
    else if (dataIsDirty) {
        //LINFO("Update from data change"); // TODO: remove
        updateGeometry();
    }

    if (_pointTexture) {
        _pointTexture->update();
    }
}

void GlobeGeometryFeature::updateGeometry() {
    // Update vertex data and compute model coordinates based on globe
    _renderFeatures.clear();

    std::vector<std::vector<glm::vec3>> edgeVertices = createPointAndLineGeometry();
    createExtrudedGeometry(edgeVertices);
    createPolygonGeometry();

    // TODO: add extrusion for points

    // Compute new heights - to see if height map changed
    _lastControlHeights = getCurrentReferencePointsHeights();
}

std::vector<glm::vec3>
GlobeGeometryFeature::subdivideLine(const glm::dvec3& v0, const glm::dvec3& v1,
                                    double h0, double h1, double hOffset) const
{
    std::vector<glm::vec3> positions;

    double edgeLength = glm::distance(v1, v0);
    int nSegments = static_cast<int>(std::ceil(edgeLength / MaxDistance));

    for (int seg = 0; seg < nSegments; ++seg) {
        double t = static_cast<double>(seg) / static_cast<double>(nSegments);

        // Interpolate both position and height value
        glm::vec3 newV = glm::mix(v0, v1, t);
        double newHeight = glm::mix(h0, h1, t);

        // Get position with adjusted height value
        newV = static_cast<glm::vec3>(geometryhelper::adjustHeightOfModelCoordinate(
            newV,
            newHeight + hOffset,
            _globe,
            useHeightMap()
        ));

        positions.push_back(newV);
    }

    // Add final position
    positions.push_back(static_cast<glm::vec3>(
        geometryhelper::adjustHeightOfModelCoordinate(
            v1,
            h1 + hOffset,
            _globe,
            useHeightMap()
        )
    ));

    return positions;
}

std::vector<std::vector<glm::vec3>>
GlobeGeometryFeature::createPointAndLineGeometry()
{
    std::vector<std::vector<glm::vec3>> resultPositions;

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
                _offsets,
                useHeightMap()
            );

            // If we are drawing points, just add and continue
            if (_type == GeometryType::Point) {
                glm::vec3 vf = static_cast<glm::vec3>(v);
                vertices.push_back({ vf.x, vf.y, vf.z, 0.f, 0.f, 0.f });
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

            // Add extra vertices to fulfill MaxDistance criteria
            std::vector<glm::vec3> subdividedPositions = subdivideLine(
                lastPos,
                v,
                lastHeightValue,
                geodetic.height,
                _offsets.z
            );

            for (const glm::vec3& p : subdividedPositions) {
                vertices.push_back({ p.x, p.y, p.z, 0.f, 0.f, 0.f });
                positions.push_back(p);
            }

            lastPos = v;
            lastHeightValue = geodetic.height;
        }

        RenderFeature feature;
        feature.nVertices = vertices.size();

        // Figure out if we're rendering lines or points
        bool isPoints = _type == GeometryType::Point;
        feature.type = isPoints ? RenderType::Points : RenderType::Lines;

        // Generate buffers
        if (feature.vaoId == 0) {
            glGenVertexArrays(1, &feature.vaoId);
        }
        if (feature.vboId == 0) {
            glGenBuffers(1, &feature.vboId);
        }

        bufferVertexData(feature, vertices);

        _renderFeatures.push_back(std::move(feature));

        positions.shrink_to_fit();
        resultPositions.push_back(std::move(positions));
    }

    return resultPositions;
}

void GlobeGeometryFeature::createExtrudedGeometry(
                                 const std::vector<std::vector<glm::vec3>>& edgeVertices)
{
    std::vector<Vertex> vertices =
        geometryhelper::createExtrudedGeometryVertices(edgeVertices);

    RenderFeature feature;
    feature.type = RenderType::Polygon;
    feature.nVertices = vertices.size();
    feature.isExtrusionFeature = true;

    if (feature.vaoId == 0) {
        glGenVertexArrays(1, &feature.vaoId);
    }
    if (feature.vboId == 0) {
        glGenBuffers(1, &feature.vboId);
    }

    bufferVertexData(feature, vertices);
    _renderFeatures.push_back(std::move(feature));
}

void GlobeGeometryFeature::createPolygonGeometry() {
    std::vector<Vertex> vertices;
    vertices.reserve(_triangleCoordinates.size());

    int finalCount = 0;

    // Vertices for polygon triangles (OBS! Note that ordering matters! We want to
    // render the top polygon triangles last, to behave when rendering with
    // transparency. So, add them last)
    int triIndex = 0;
    std::array<glm::vec3, 3> triPositions;
    std::array<double, 3> triHeights;
    for (const Geodetic3& geodetic : _triangleCoordinates) {
        const glm::vec3 vert = geometryhelper::computeOffsetedModelCoordinate(
            geodetic,
            _globe,
            _offsets,
            useHeightMap()
        );
        triPositions[triIndex] = vert;
        triHeights[triIndex] = geodetic.height;
        triIndex++;

        // Once we have a triangle, start subdividing
        if (triIndex % 3 == 0) {
            const glm::vec3 v0 = triPositions[triIndex - 3];
            const glm::vec3 v1 = triPositions[triIndex - 2];
            const glm::vec3 v2 = triPositions[triIndex - 1];

            double h0 = triHeights[triIndex - 3];
            double h1 = triHeights[triIndex - 2];
            double h2 = triHeights[triIndex - 1];

            triIndex = 0;

            // Subdivide edges
            std::vector<glm::vec3> edge01 = subdivideLine(v0, v1, h0, h1, _offsets.z);
            std::vector<glm::vec3> edge02 = subdivideLine(v0, v2, h0, h2, _offsets.z);
            std::vector<glm::vec3> edge12 = subdivideLine(v1, v2, h1, h2, _offsets.z);

            size_t nSteps01 = edge01.size();
            size_t nSteps02 = edge02.size();
            size_t nSteps12 = edge12.size();
            size_t maxSteps = std::max(std::max(nSteps01, nSteps02), nSteps12);

            std::vector<Coordinate> pointCoords;

            pointCoords.reserve(3 * maxSteps + 1);

            // Add inside points
            const float lengthEdge01 = glm::length(v1 - v0);
            const float lengthEdge02 = glm::length(v2 - v0);
            for (size_t i = 1; i < nSteps01; ++i) {
                for (size_t j = 1; j < nSteps02; ++j) {
                    glm::vec3 comp01 = edge01[i] - v0;
                    glm::vec3 comp02 = edge02[j] - v0;

                    float w1 = glm::length(comp01) / lengthEdge01;
                    float w2 = glm::length(comp02) / lengthEdge02;

                    if (w1 + w2 > 1.f - std::numeric_limits<float>::epsilon()) {
                        continue; // Sum larger than 1.0 => Outside of triangle
                    }

                    glm::vec3 pos = v0 + comp01 + comp02;

                    Geodetic2 geo2 = _globe.ellipsoid().cartesianToGeodetic2(pos);
                    // TODO: include height from pos
                    Geodetic3 geo3 = { geo2, 0.0 };
                    pointCoords.push_back(geometryhelper::toGeosCoord(geo3));
                }
            }

            // Add egde positions
            for (size_t i = 0; i < maxSteps; ++i) {
                if (i < edge01.size() - 1) {
                    Geodetic2 geo2 = _globe.ellipsoid().cartesianToGeodetic2(edge01[i]);
                    // TODO: include height from pos
                    Geodetic3 geo3 = { geo2, 0.0 };
                    pointCoords.push_back(geometryhelper::toGeosCoord(geo3));
                }
                if (i < edge02.size() - 1) {
                    Geodetic2 geo2 = _globe.ellipsoid().cartesianToGeodetic2(edge02[i]);
                    // TODO: include height from pos
                    Geodetic3 geo3 = { geo2, 0.0 };
                    pointCoords.push_back(geometryhelper::toGeosCoord(geo3));
                }
                if (i < edge12.size() - 1) {
                    Geodetic2 geo2 = _globe.ellipsoid().cartesianToGeodetic2(edge12[i]);
                    // TODO: include height from pos
                    Geodetic3 geo3 = { geo2, 0.0 };
                    pointCoords.push_back(geometryhelper::toGeosCoord(geo3));
                }
            }

            // Also add the final position (not part of the subdivide step)
            Geodetic2 geo2 = _globe.ellipsoid().cartesianToGeodetic2(v2);
            // TODO: include height from pos
            Geodetic3 geo3 = { geo2, 0.0 };
            pointCoords.push_back(geometryhelper::toGeosCoord(geo3));

            pointCoords.shrink_to_fit();

            using namespace geos::geom;

            GeometryFactory::Ptr geometryFactory = GeometryFactory::create();
            MultiPoint* points = geometryFactory->createMultiPoint(pointCoords);

            // Create triangulation of points
            geos::triangulate::DelaunayTriangulationBuilder builder;
            builder.setSites(*points->getCoordinates());

            // Returns a list o triangles, as geos polygons
            GeometryCollection* triangleGeoms = builder.getTriangles(*geometryFactory).release();
            std::vector<Coordinate> triCoords;
            triangleGeoms->getCoordinates()->toVector(triCoords);

            vertices.reserve(vertices.size() + triCoords.size() + 1);

            int currentCount = 0;
            for (const Coordinate& coord : triCoords) {
                currentCount++;
                if (currentCount % 4 == 0) {
                    // Skip every 4th coord, as polygons has one extra coord per triangle
                    continue;
                }
                Geodetic3 geodetic = geometryhelper::toGeodetic(coord);
                glm::vec3 v = geometryhelper::computeOffsetedModelCoordinate(
                    geodetic,
                    _globe,
                    _offsets,
                    useHeightMap()
                );
                vertices.push_back({ v.x, v.y, v.z, 0.f, 0.f, 0.f });
                finalCount++;

                // Polygons: Every third set of coordinates is a triangle => update normal
                if (finalCount % 3 == 0) {
                    // Find previous vertices
                    Vertex& vert0 = vertices[finalCount - 3];
                    Vertex& vert1 = vertices[finalCount - 2];

                    const glm::vec3 v0 = glm::vec3(vert0.xyz[0], vert0.xyz[1], vert0.xyz[2]);
                    const glm::vec3 v1 = glm::vec3(vert1.xyz[0], vert1.xyz[1], vert1.xyz[2]);
                    const glm::vec3 n = -glm::normalize(glm::cross(v1 - v0, v - v0));

                    vert0.normal[0] = n.x;
                    vert0.normal[1] = n.y;
                    vert0.normal[2] = n.z;

                    vert1.normal[0] = n.x;
                    vert1.normal[1] = n.y;
                    vert1.normal[2] = n.z;

                    vertices.back().normal[0] = n.x;
                    vertices.back().normal[1] = n.y;
                    vertices.back().normal[2] = n.y;
                }
            }
        }
    }

    RenderFeature triFeature;
    triFeature.type = RenderType::Polygon;
    triFeature.nVertices = vertices.size();
    triFeature.isExtrusionFeature = false;

    if (triFeature.vaoId == 0) {
        glGenVertexArrays(1, &triFeature.vaoId);
    }
    if (triFeature.vboId == 0) {
        glGenBuffers(1, &triFeature.vboId);
    }

    bufferVertexData(triFeature, vertices);
    _renderFeatures.push_back(std::move(triFeature));
}

std::vector<double> GlobeGeometryFeature::getCurrentReferencePointsHeights() const {
    std::vector<double> newHeights;
    newHeights.reserve(_heightUpdateReferencePoints.size());
    for (const Geodetic3& geo : _heightUpdateReferencePoints) {
        const glm::dvec3 p = geometryhelper::computeOffsetedModelCoordinate(
            geo,
            _globe,
            _offsets,
            useHeightMap()
        );
        const SurfacePositionHandle handle = _globe.calculateSurfacePositionHandle(p);
        newHeights.push_back(handle.heightToSurface);
    }
    return newHeights;
}

void GlobeGeometryFeature::bufferVertexData(const RenderFeature& feature,
                                            const std::vector<Vertex>& vertexData)
{
    ghoul_assert(_pointsProgram != nullptr, "Shader program must be initialized");
    ghoul_assert(_linesAndPolygonsProgram != nullptr, "Shader program must be initialized");

    glBindVertexArray(feature.vaoId);
    glBindBuffer(GL_ARRAY_BUFFER, feature.vboId);
    glBufferData(
        GL_ARRAY_BUFFER,
        vertexData.size() * sizeof(Vertex),
        vertexData.data(),
        GL_STATIC_DRAW
    );

    if (feature.type == RenderType::Points) {
        GLint positionAttrib = _pointsProgram->attributeLocation("in_position");
        glEnableVertexAttribArray(positionAttrib);
        glVertexAttribPointer(
            positionAttrib,
            3,
            GL_FLOAT,
            GL_FALSE,
            6 * sizeof(float),
            nullptr
        );

        GLint normalAttrib = _pointsProgram->attributeLocation("in_normal");
        glEnableVertexAttribArray(normalAttrib);
        glVertexAttribPointer(
            normalAttrib,
            3,
            GL_FLOAT,
            GL_FALSE,
            6 * sizeof(float),
            reinterpret_cast<void*>(3 * sizeof(float))
        );
    }
    else {
        GLint positionAttrib =
            _linesAndPolygonsProgram->attributeLocation("in_position");
        glEnableVertexAttribArray(positionAttrib);
        glVertexAttribPointer(
            positionAttrib,
            3,
            GL_FLOAT,
            GL_FALSE,
            6 * sizeof(float),
            nullptr
        );

        GLint normalAttrib = _linesAndPolygonsProgram->attributeLocation("in_normal");
        glEnableVertexAttribArray(normalAttrib);
        glVertexAttribPointer(
            normalAttrib,
            3,
            GL_FLOAT,
            GL_FALSE,
            6 * sizeof(float),
            reinterpret_cast<void*>(3 * sizeof(float))
        );
    }

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);
};

} // namespace openspace::globebrowsing
