/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <modules/globebrowsing/src/renderablegeojsonobject.h>

#include <modules/globebrowsing/src/renderableglobe.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/json.h>
#include <openspace/query/query.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <filesystem>
#include <fstream>
#include <optional>

namespace {
    constexpr const char* _loggerCat = "RenderableGeoJsonObject";

    constexpr openspace::properties::Property::PropertyInfo GlobeInfo = {
        "Globe",
        "Globe",
        "The globe on which this feature should be rendered."
    };

    constexpr openspace::properties::Property::PropertyInfo FileInfo = {
        "File",
        "File",
        "Path to the GeoJSON file to base the rendering on."
    }; // @TODO: Add documentation about what geometries are supported

    constexpr openspace::properties::Property::PropertyInfo HeightOffsetInfo = {
        "HeightOffset",
        "Height Offset",
        "A height offset value, in meters. Useful for moving a feature closer to or "
        "farther away from the surface."
    };

    constexpr openspace::properties::Property::PropertyInfo CoordinateOffsetInfo = {
        "CoordinateOffset",
        "Geographic Coordinate Offset",
        "A latitude and longitude offset value, in decimal degrees. Can be used to "
        "move the object on the surface and correct potential mismatches with other "
        "renderings."
    };

    constexpr openspace::properties::Property::PropertyInfo UseHeightmapInfo = {
        "UseHeightmap",
        "Use Heightmap",
        "If this value is 'true', the height offset property ('HeightOffset') will be "
        "treated as an offset from the heightmap. Otherwise, it will be an offset from "
        "the globe's reference ellipsoid. The default value is 'false'."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "The color of the rendered geometry."
    };

    constexpr openspace::properties::Property::PropertyInfo PointSizeInfo = {
        "PointSize",
        "Point Size",
        "The size of any rendered points."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line Width",
        "The width of any rendered lines."
    };

    constexpr openspace::properties::Property::PropertyInfo ForceUpdateDataInfo = {
        "ForceUpdateData",
        "Force Update Data",
        "Triggering this leads to a recomputation of the geometry positions. Can "
        "be used to for example update the poisition when the height map has loaded."
    };

    struct [[codegen::Dictionary(RenderableGeoJsonObject)]] Parameters {
        // [[codegen::verbatim(GlobeInfo.description)]]
        std::string globe;

        // [[codegen::verbatim(FileInfo.description)]]
        std::filesystem::path file;

        // [[codegen::verbatim(HeightOffsetInfo.description)]]
        std::optional<float> heightOffset;

        // [[codegen::verbatim(CoordinateOffsetInfo.description)]]
        std::optional<glm::vec2> coordinateOffset;

        // [[codegen::verbatim(UseHeightmapInfo.description)]]
        std::optional<bool> useHeightmap;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(PointSizeInfo.description)]]
        std::optional<float> pointSize;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth;
    };
#include "renderablegeojsonobject_codegen.cpp"
} // namespace

using json = nlohmann::json;

namespace openspace::globebrowsing {

void from_json(const nlohmann::json& j, RenderableGeoJsonObject::GeometryType& gt) {
    const std::string& type = j.get<std::string>();
    if (type == "LineString") {
        gt = RenderableGeoJsonObject::GeometryType::LineString;
    }
    else if (type == "Point") {
        gt = RenderableGeoJsonObject::GeometryType::Point;
    }
    else if (type == "MultiPoint") {
        gt = RenderableGeoJsonObject::GeometryType::MultiPoint;
    }
    else {
        LERROR(fmt::format("Non-supported geometry type: {}", type));
        throw ghoul::MissingCaseException();
    }
}

void from_json(const nlohmann::json& j, Geodetic2& g) {
    // OBS: ignoring the last value (height) for now, but a 3 value coordiante is still valid
    if (!j.is_array() || j.size() > 3) {
        throw ghoul::RuntimeError("Error parsing coordinate");
    }

    j[0].get_to(g.lon);
    j[1].get_to(g.lat);

    // Convert from decimal degrees to radians
    g.lon = glm::radians(g.lon);
    g.lat = glm::radians(g.lat);
}

void from_json(const nlohmann::json& j, RenderableGeoJsonObject::Geometry& g) {
    j.at("type").get_to(g.type);

    // If we have just a single point, handle it separately. Else just read the
    // entire array directly
    if (g.type == RenderableGeoJsonObject::GeometryType::Point) {
        Geodetic2 coordinate;
        j.at("coordinates").get_to(coordinate);
        g.coordinates = { coordinate };
    }
    else {
        j.at("coordinates").get_to(g.coordinates);
    }
}

documentation::Documentation RenderableGeoJsonObject::Documentation() {
    return codegen::doc<Parameters>("globebwowsing_renderablegeojsonfeature");
}

RenderableGeoJsonObject::RenderableGeoJsonObject(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _globe(GlobeInfo)
    , _geoJsonFile(FileInfo)
    , _heightOffset(HeightOffsetInfo, 0.f, -1e12f, 1e12f)
    , _latLongOffset(
        CoordinateOffsetInfo,
        glm::vec2(0.f),
        glm::vec2(-90.0),
        glm::vec2(90.f)
    )
    , _useHeightmap(UseHeightmapInfo, false)
    , _color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _pointSize(PointSizeInfo, 1.f, 0.f, 100.f)
    , _lineWidth(LineWidthInfo, 1.f, 0.f, 100.f)
    , _forceUpdateData(ForceUpdateDataInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    addProperty(_opacity);
    registerUpdateRenderBinFromOpacity();

    _globe = p.globe;
    _globe.setReadOnly(true);
    addProperty(_globe);

    _geoJsonFile = p.file.string();
    _geoJsonFile.setReadOnly(true);
    addProperty(_geoJsonFile);

    _heightOffset = p.heightOffset.value_or(_heightOffset);
    _heightOffset.onChange([this]() { _dataIsDirty = true; });
    addProperty(_heightOffset);

    _latLongOffset = p.coordinateOffset.value_or(_latLongOffset);
    _latLongOffset.onChange([this]() { _dataIsDirty = true; });
    addProperty(_latLongOffset);

    _useHeightmap = p.useHeightmap.value_or(_useHeightmap);
    _useHeightmap.onChange([this]() { _dataIsDirty = true; });
    addProperty(_useHeightmap);

    _color = p.color.value_or(_color);
    _color.setViewOption(properties::Property::ViewOptions::Color);
    addProperty(_color);

    _pointSize = p.pointSize.value_or(_pointSize);
    addProperty(_pointSize);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _forceUpdateData.onChange([this]() { _shouldForceUpdateData = true; });
    addProperty(_forceUpdateData);

    // Render after atmosphere
    setRenderBin(Renderable::RenderBin::PostDeferredTransparent);
}

void RenderableGeoJsonObject::findGlobe() {
    SceneGraphNode* n = sceneGraphNode(_globe);
    if (!n) {
        LERROR(fmt::format(
            "Could not find scenegraph node with identifier {}", _globe
        ));
    }
    else if (n->renderable() && dynamic_cast<RenderableGlobe*>(n->renderable())) {
        _globeNode = dynamic_cast<RenderableGlobe*>(n->renderable());
    }
    else {
        LERROR(
            "Could not set attached node as it does not have a RenderableGlobe"
        );
    }
}

void RenderableGeoJsonObject::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "GeoJsonLinesProgram",
        absPath("${MODULE_GLOBEBROWSING}/shaders/geojson_vs.glsl"),
        absPath("${MODULE_GLOBEBROWSING}/shaders/geojson_fs.glsl")
    );

    readFile();
    initializeGeometry();
}

void RenderableGeoJsonObject::deinitializeGL() {
    for (const Geometry& g : _geometryFeatures) {
        glDeleteVertexArrays(1, &g.vaoId);
        glDeleteBuffers(1, &g.vboId);
    }

    global::renderEngine->removeRenderProgram(_program.get());
    _program = nullptr;
}

bool RenderableGeoJsonObject::isReady() const {
    return _program != nullptr;
}

void RenderableGeoJsonObject::render(const RenderData& data, RendererTasks&) {
    if (!_globeNode) {
        findGlobe();
        return;
    }

    // @TODO: this shouldn't be needed, but currently it is. At least for assets
    // that are included during runtime
    if (!_dataIsInitialized) {
        //LERROR("Wasn't ready for render");
        return;
    }

    const glm::dmat4 globeModelTransform = _globeNode->modelTransform();
    const glm::dmat4 modelViewTransform =
        data.camera.combinedViewMatrix() * globeModelTransform;

    const glm::dmat4 projectionTransform = data.camera.projectionMatrix();
    renderGeometry(data, modelViewTransform, projectionTransform);
}

void RenderableGeoJsonObject::renderGeometry(const RenderData&,
                                             const glm::dmat4& modelViewMatrix,
                                             const glm::dmat4& projectionMatrix)
{
    _program->activate();

    _program->setUniform("modelViewTransform", modelViewMatrix);
    _program->setUniform("projectionTransform", projectionMatrix);

    _program->setUniform("opacity", _opacity);
    _program->setUniform("color", _color);
    _program->setUniform("pointSize", _pointSize);

    // Change GL state:
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnablei(GL_BLEND, 0);

#ifndef __APPLE__
    glLineWidth(_lineWidth);
#else
    glLineWidth(1.f);
#endif

    for (const Geometry& g : _geometryFeatures) {
        glBindVertexArray(g.vaoId);
        switch (g.type) {
            case GeometryType::LineString:
                glEnable(GL_LINE_SMOOTH);
                glDrawArrays(GL_LINE_STRIP, 0, static_cast<GLsizei>(g.coordinates.size()));
                glDisable(GL_LINE_SMOOTH);
                break;
            case GeometryType::Point:
            case GeometryType::MultiPoint:
                glEnable(GL_PROGRAM_POINT_SIZE);
                glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(g.coordinates.size()));
                glDisable(GL_PROGRAM_POINT_SIZE);
                // TODO: support sprites for the points
                break;
            default:
                break;
        }
    }

    glBindVertexArray(0);
    _program->deactivate();

    // Restore GL State
    global::renderEngine->openglStateCache().resetBlendState();
    global::renderEngine->openglStateCache().resetLineState();
}

void RenderableGeoJsonObject::update(const UpdateData&) {
    // @TODO (2021-10-21, emmbr): Figure out a neat solution to listen to changes 
    // in the height map, so that we don't have to use the force update property 
    // (or the worse alternative: recompute every frame, like the GlobeTranslation and 
    // GlobeRotation do)
    if (!_dataIsDirty && !_shouldForceUpdateData) {
        return;
    }

    _shouldForceUpdateData = false;

    if (!_globeNode) {
        // Find the globe and try to update next frame instead
        findGlobe();
        return;
    }

    double offsetLatRadians = glm::radians(_latLongOffset.value().x);
    double offsetLonRadians = glm::radians(_latLongOffset.value().y);

    // Update vertex data and compute model coordinates based on globe
    for (Geometry& g : _geometryFeatures) {
        g.vertices.clear();
        g.vertices.reserve(g.coordinates.size() * 3);

        for (const Geodetic2& geodetic : g.coordinates) {
            Geodetic2 adjustedGeodetic = geodetic;
            adjustedGeodetic.lat += offsetLatRadians;
            adjustedGeodetic.lon += offsetLonRadians;
            glm::vec3 v = calculateModelCoordinate(adjustedGeodetic);
            g.vertices.push_back(v.x);
            g.vertices.push_back(v.y);
            g.vertices.push_back(v.z);
        }

        // Buffer data
        glBindVertexArray(g.vaoId);
        glBindBuffer(GL_ARRAY_BUFFER, g.vboId);
        glBufferData(
            GL_ARRAY_BUFFER,
            g.vertices.size() * sizeof(float),
            g.vertices.data(),
            GL_STATIC_DRAW
        );

        GLint positionAttrib = _program->attributeLocation("in_position");
        glEnableVertexAttribArray(positionAttrib);
        glVertexAttribPointer(
            positionAttrib,
            3,
            GL_FLOAT,
            GL_FALSE,
            3 * sizeof(float),
            nullptr
        );
    }

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    _dataIsDirty = false;
    _dataIsInitialized = true;
}

void RenderableGeoJsonObject::readFile() {
    std::ifstream file(_geoJsonFile);

    if (!file.good()) {
        LERROR(fmt::format("Failed to open geoJson file: {}", _geoJsonFile));
        return;
    }

    _geometryFeatures.clear();

    const json jsonContent = json::parse(file);

    try {
        const std::string& objectType = jsonContent.at("type").get<std::string>();
        // TODO: also handle files with just a feature
        if (objectType == "FeatureCollection") {
            // TODO: verify better..
            const std::vector<json> features = jsonContent.at("features");
            _geometryFeatures.reserve(features.size());

            for (const json& featureJson : features) {
                if (featureJson["type"] != "Feature") {
                    LWARNING("Skipping invalid feature in collection (not type 'Feature')");
                    continue;
                }

                // Valid feature => get geometry
                Geometry g;
                featureJson.at("geometry").get_to(g);
                _geometryFeatures.push_back(g);
            }
        }
        else {
            throw ghoul::MissingCaseException();
        }
        _geometryFeatures.shrink_to_fit();
    }
    catch (const ghoul::MissingCaseException& e) {
        LERROR("Could not read geojson file -- missing case:");
        LERROR(e.what());
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR("Could not read geojson file -- runtime error:");
        LERROR(e.what());
    }
    catch (...) {
        LERROR("Could not read geojson file -- unknown error");
    }
}

void RenderableGeoJsonObject::initializeGeometry() {
    for (Geometry& g : _geometryFeatures) {
        GLuint vao;
        glGenVertexArrays(1, &vao);
        g.vaoId = vao;

        GLuint vbo;
        glGenBuffers(1, &vbo);
        g.vboId = vbo;

        glBindVertexArray(vao);
        glBindBuffer(GL_ARRAY_BUFFER, vao);
        glEnableVertexAttribArray(0);
        glBindVertexArray(0);
    }
}

glm::vec3 RenderableGeoJsonObject::calculateModelCoordinate(const Geodetic2& geodetic) {
    glm::dvec3 posModelSpaceZeroHeight =
        _globeNode->ellipsoid().cartesianSurfacePosition(geodetic);

    double heightToSurface = static_cast<double>(_heightOffset);

    if (_useHeightmap) {
        const SurfacePositionHandle posHandle =
            _globeNode->calculateSurfacePositionHandle(posModelSpaceZeroHeight);

        heightToSurface += posHandle.heightToSurface;
    }

    Geodetic3 geo3 = Geodetic3{ geodetic, heightToSurface };
    const glm::dvec3 posModelSpace = _globeNode->ellipsoid().cartesianPosition(geo3);
    return glm::vec3(posModelSpace);
}

} // namespace openspace::globebrowsing
