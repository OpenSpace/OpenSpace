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
        // TOOD: verify that this is updated correctly
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
        shader->setUniform("opacity", opacity);
        shader->setUniform("modelTransform", globeModelTransform);
        shader->setUniform("viewTransform", renderData.camera.combinedViewMatrix());
        shader->setUniform("projectionTransform", projectionTransform);

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
                bool shouldRenderTwice = (fillOpacity < 1.0);
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

    // TODO: move to correct render function
    // Reset after every geometry
    global::renderEngine->openglStateCache().resetPolygonAndClippingState();
}

void GlobeGeometryFeature::renderPoints(const RenderFeature& feature) const
{
    ghoul_assert(feature.type == RenderType::Points, "Trying to render faulty geometry");
    _pointsProgram->setUniform("color", _properties.color());
    _pointsProgram->setUniform(
        "pointSize",
        0.01f * _properties.pointSize() * static_cast<float>(_globe.boundingSphere())
    );

    if (_pointTexture) {
        ghoul::opengl::TextureUnit unit;
        unit.activate();
        _pointTexture->bind();
        _pointsProgram->setUniform("pointTexture", unit);
        _pointsProgram->setUniform("hasTexture", true);
    }
    else {
        glBindTexture(GL_TEXTURE_2D, 0);
        _pointsProgram->setUniform("hasTexture", false);
    }
    glEnable(GL_PROGRAM_POINT_SIZE);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(feature.nVertices));
    glDisable(GL_PROGRAM_POINT_SIZE);
    // TODO: support sprites for the points
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

            // If we are drawing points, just add and continue.
            // Note that we set the normal
            // TODO: make normal the out direction of the globe
            if (_type == GeometryType::Point) {
                glm::vec3 vf = static_cast<glm::vec3>(v);
                vertices.push_back({ vf.x, vf.y, vf.z, 1.f, 1.f, 1.f });
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
            std::vector<glm::vec3> subdividedPositions = geometryhelper::subdivideLine(
                lastPos,
                v,
                lastHeightValue,
                geodetic.height,
                _offsets.z,
                MaxDistance,
                _globe,
                useHeightMap()
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
    std::vector<Vertex> polyVertices;

    // Create polygon vertices from the triangle coordinates
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
        if (triIndex == 3) {
            triIndex = 0;

            const glm::vec3 v0 = triPositions[0];
            const glm::vec3 v1 = triPositions[1];
            const glm::vec3 v2 = triPositions[2];

            double h0 = triHeights[0];
            double h1 = triHeights[1];
            double h2 = triHeights[2];

            std::vector<Vertex> verts = geometryhelper::subdivideTriangle(
                v0, v1, v2,
                h0, h1, h2,
                MaxDistance,
                _offsets,
                _globe,
                useHeightMap()
            );
            polyVertices.insert(polyVertices.end(), verts.begin(), verts.end());
        }
    }

    RenderFeature triFeature;
    triFeature.type = RenderType::Polygon;
    triFeature.nVertices = polyVertices.size();
    triFeature.isExtrusionFeature = false;

    if (triFeature.vaoId == 0) {
        glGenVertexArrays(1, &triFeature.vaoId);
    }
    if (triFeature.vboId == 0) {
        glGenBuffers(1, &triFeature.vboId);
    }

    bufferVertexData(triFeature, polyVertices);
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
