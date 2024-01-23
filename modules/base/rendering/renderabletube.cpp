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

#include <modules/base/rendering/renderabletube.h>

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/helper.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scene/lightsource.h>
#include <openspace/util/time.h>
#include <openspace/util/updatestructures.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/opengl/programobject.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/texture.h>
#include <glm/gtx/projection.hpp>
#include <optional>

using json = nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "RenderableTube";
    constexpr int8_t CurrentMajorVersion = 0;
    constexpr int8_t CurrentMinorVersion = 1;
    constexpr std::array<const char*, 14> UniformNames = {
        "modelViewTransform", "projectionTransform", "normalTransform", "color",
        "opacity", "hasTransferFunction", "transferFunction", "performShading",
        "nLightSources", "lightDirectionsViewSpace", "lightIntensities",
        "ambientIntensity", "diffuseIntensity", "specularIntensity"
    };

    constexpr openspace::properties::Property::PropertyInfo TransferFunctionInfo = {
        "TransferFunctionPath",
        "Transfer Function Path",
        "Specifies the transfer function file path",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ColorInfo = {
        "Color",
        "Color",
        "This value determines the RGB color for the tube",
        // @VISIBILITY(1.2)
        openspace::properties::Property::Visibility::NoviceUser
    };

    constexpr openspace::properties::Property::PropertyInfo EnableFaceCullingInfo = {
        "EnableFaceCulling",
        "Enable Face Culling",
        "Enable OpenGL automatic face culling optimization",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo ShadingEnabledInfo = {
        "PerformShading",
        "Perform Shading",
        "This value determines whether shading should be applied to the tube",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo AmbientIntensityInfo = {
        "AmbientIntensity",
        "Ambient Intensity",
        "A multiplier for ambient lighting for the shading of the tube",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DiffuseIntensityInfo = {
        "DiffuseIntensity",
        "Diffuse Intensity",
        "A multiplier for diffuse lighting for the shading of the tube",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo SpecularIntensityInfo = {
        "SpecularIntensity",
        "Specular Intensity",
        "A multiplier for specular lighting for the shading of the tube",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo LightSourcesInfo = {
        "LightSources",
        "Light Sources",
        "A list of light sources that this tube should accept light from",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo AddEdgesInfo = {
        "AddEdges",
        "Add Edges",
        "This value determines whether a bottom and top should b eadded to the tube",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo DrawWireframeInfo = {
        "DrawWireframe",
        "Wireframe",
        "If true, draw the wire frame of the tube",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo WireLineWidthInfo = {
        "WireLineWidth",
        "Wire Line Width",
        "The line width to use when the tube is rendered as a wireframe",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo UseSmoothNormalsInfo = {
        "UseSmoothNormals",
        "Use Smooth Normals",
        "If ture, the tube is shaded using smooth normals. If false, every triangle "
        "get its own normal, which can lead to harder shadows",
        openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableTube)]] Parameters {
        // The input file with data for the tube
        std::string file;

        // [[codegen::verbatim(TransferFunctionInfo.description)]]
        std::optional<std::string> transferFunction;

        // [[codegen::verbatim(ColorInfo.description)]]
        std::optional<glm::vec3> color [[codegen::color()]];

        // [[codegen::verbatim(EnableFaceCullingInfo.description)]]
        std::optional<bool> enableFaceCulling;

        // [[codegen::verbatim(ShadingEnabledInfo.description)]]
        std::optional<bool> performShading;

        // [[codegen::verbatim(AmbientIntensityInfo.description)]]
        std::optional<float> ambientIntensity [[codegen::inrange(0.f, 1.f)]];

        // [[codegen::verbatim(DiffuseIntensityInfo.description)]]
        std::optional<float> diffuseIntensity [[codegen::inrange(0.f, 1.f)]];

        // [[codegen::verbatim(SpecularIntensityInfo.description)]]
        std::optional<float> specularIntensity [[codegen::inrange(0.f, 1.f)]];

        // [[codegen::verbatim(LightSourcesInfo.description)]]
        std::optional<std::vector<ghoul::Dictionary>> lightSources
            [[codegen::reference("core_light_source")]];

        // [[codegen::verbatim(AddEdgesInfo.description)]]
        std::optional<bool> addEdges;

        // [[codegen::verbatim(DrawWireframeInfo.description)]]
        std::optional<bool> drawWireframe;

        // [[codegen::verbatim(WireLineWidthInfo.description)]]
        std::optional<float> wireLineWidth;

        // [[codegen::verbatim(UseSmoothNormalsInfo.description)]]
        std::optional<bool> useSmoothNormals;
    };
#include "renderabletube_codegen.cpp"
} // namespace

namespace openspace {

documentation::Documentation RenderableTube::Documentation() {
    return codegen::doc<Parameters>("base_renderable_tube");
}

RenderableTube::Shading::Shading()
    : properties::PropertyOwner({ "Shading" })
    , enabled(ShadingEnabledInfo, true)
    , ambientIntensity(AmbientIntensityInfo, 0.2f, 0.f, 1.f)
    , diffuseIntensity(DiffuseIntensityInfo, 1.f, 0.f, 1.f)
    , specularIntensity(SpecularIntensityInfo, 1.f, 0.f, 1.f)
{
    addProperty(enabled);
    addProperty(ambientIntensity);
    addProperty(diffuseIntensity);
    addProperty(specularIntensity);
}

RenderableTube::RenderableTube(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _transferFunctionPath(TransferFunctionInfo)
    , _color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _enableFaceCulling(EnableFaceCullingInfo, true)
    , _lightSourcePropertyOwner({ "LightSources", "Light Sources" })
    , _addEdges(AddEdgesInfo, true)
    , _drawWireframe(DrawWireframeInfo, false)
    , _wireLineWidth(WireLineWidthInfo, 1.f, 1.f, 10.f)
    , _useSmoothNormals(UseSmoothNormalsInfo, true)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _dataFile = p.file;

    if (p.transferFunction.has_value()) {
        _hasTransferFunction = true;
        _transferFunctionPath = absPath(*p.transferFunction).string();
        _transferFunction = std::make_shared<openspace::TransferFunction>(
            _transferFunctionPath,
            [](const openspace::TransferFunction&) {}
        );
    }
    addProperty(_transferFunctionPath);

    _color.setViewOption(properties::Property::ViewOptions::Color);
    _color = p.color.value_or(_color);
    addProperty(_color);

    _enableFaceCulling = p.enableFaceCulling.value_or(_enableFaceCulling);
    addProperty(_enableFaceCulling);

    _shading.enabled = p.performShading.value_or(_shading.enabled);
    _shading.ambientIntensity = p.ambientIntensity.value_or(_shading.ambientIntensity);
    _shading.diffuseIntensity = p.diffuseIntensity.value_or(_shading.diffuseIntensity);
    _shading.specularIntensity = p.specularIntensity.value_or(_shading.specularIntensity);
    addPropertySubOwner(_shading);

    if (p.lightSources.has_value()) {
        std::vector<ghoul::Dictionary> lightsources = *p.lightSources;

        for (const ghoul::Dictionary& lsDictionary : lightsources) {
            std::unique_ptr<LightSource> lightSource =
                LightSource::createFromDictionary(lsDictionary);
            _lightSourcePropertyOwner.addPropertySubOwner(lightSource.get());
            _lightSources.push_back(std::move(lightSource));
        }
    }

    _addEdges.onChange([this]() { _tubeIsDirty = true; });
    _addEdges = p.addEdges.value_or(_addEdges);
    addProperty(_addEdges);

    _drawWireframe = p.drawWireframe.value_or(_drawWireframe);
    addProperty(_drawWireframe);

    _wireLineWidth = p.wireLineWidth.value_or(_wireLineWidth);
    addProperty(_wireLineWidth);

    _useSmoothNormals.onChange([this]() { _tubeIsDirty = true; });
    _useSmoothNormals = p.useSmoothNormals.value_or(_useSmoothNormals);
    addProperty(_useSmoothNormals);

    addProperty(Fadeable::_opacity);
}

bool RenderableTube::isReady() const {
    return _shader != nullptr;
}

void RenderableTube::initialize() {
    readDataFile();
    updateTubeData();

    for (const std::unique_ptr<LightSource>& ls : _lightSources) {
        ls->initialize();
    }
}

void RenderableTube::initializeGL() {
    _shader = global::renderEngine->buildRenderProgram(
        "TubeProgram",
        absPath("${MODULE_BASE}/shaders/tube_vs.glsl"),
        absPath("${MODULE_BASE}/shaders/tube_fs.glsl")
    );
    ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);

    glGenVertexArrays(1, &_vaoId);
    glGenBuffers(1, &_vboId);
    glGenBuffers(1, &_iboId);

    glBindVertexArray(_vaoId);

    updateBufferData();

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(PolygonVertex), nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, normal))
    );

    glEnableVertexAttribArray(2);
    glVertexAttribPointer(
        2,
        1,
        GL_FLOAT,
        GL_FALSE,
        sizeof(PolygonVertex),
        reinterpret_cast<const GLvoid*>(offsetof(PolygonVertex, value))
    );

    glBindVertexArray(0);
}

void RenderableTube::deinitializeGL() {
    global::renderEngine->removeRenderProgram(_shader.get());
    _shader = nullptr;

    glDeleteVertexArrays(1, &_vaoId);
    _vaoId = 0;

    glDeleteBuffers(1, &_vboId);
    _vboId = 0;

    glDeleteBuffers(1, &_iboId);
    _iboId = 0;
}

void RenderableTube::readDataFile() {
    std::filesystem::path file = absPath(_dataFile);
    if (!std::filesystem::is_regular_file(file)) {
        LWARNING(fmt::format("The data file '{}' could not be found", file));
        return;
    }

    // Open file
    std::ifstream fileStream(file);
    if (!fileStream.good()) {
        LERROR(fmt::format("Failed to open data file '{}'", file));
        return;
    }

    // Read the entire file into a string
    constexpr size_t readSize = std::size_t(4096);
    fileStream.exceptions(std::ios_base::badbit);

    std::string data;
    std::string buf = std::string(readSize, '\0');
    while (fileStream.read(buf.data(), readSize)) {
        data.append(buf, 0, fileStream.gcount());
    }
    data.append(buf, 0, fileStream.gcount());
    fileStream.close();

    // Convert to a json object
    json jsonData = json::parse(data);

    // Check version
    bool foundVersion = false;
    if (auto version = jsonData.find("version"); version != jsonData.end()) {
        auto major = version->find("major");
        auto minor = version->find("minor");

        if (major != version->end() && minor != version->end()) {
            foundVersion = true;
            if (*major != CurrentMajorVersion || *minor != CurrentMinorVersion) {
                LWARNING(fmt::format(
                    "Unknown data version '{}.{}' found. The currently supported version "
                    "is {}.{}", major->dump(), minor->dump(), CurrentMajorVersion,
                    CurrentMinorVersion
                ));
            }
        }
    }

    if (!foundVersion) {
        LWARNING("Could not find version information, version might not be supported");
    }

    // Find polygons
    auto polygons = jsonData.find("polygons");
    if (polygons == jsonData.end() || polygons->size() < 1) {
        LERROR("Could not find any polygon in the data");
        return;
    }

    // Loop throught json object to fill the datastructure for the polygons
    for (auto it = polygons->begin(); it < polygons->end(); ++it) {
        TimePolygon timePolygon;

        // Timestamp
        auto time = it->find("time");
        if (time == it->end()) {
            LERROR("Could not find time for polygon in data");
            return;
        }
        std::string timeString = time->dump();
        timeString.erase(
            std::remove(timeString.begin(), timeString.end(), '\"'),
            timeString.end()
        );
        timePolygon.timestamp = Time::convertTime(timeString);

        // Center
        auto centerPt = it->find("center");
        if (centerPt == it->end()) {
            LERROR("Could not find center for polygon in data");
            return;
        }
        double x, y, z;
        centerPt->at("x").get_to(x);
        centerPt->at("y").get_to(y);
        centerPt->at("z").get_to(z);
        timePolygon.center = glm::dvec3(x, y, z);
        timePolygon.hasCenter = true;

        // Points
        auto points = it->find("points");
        if (points == it->end() || points->size() < 1) {
            LERROR("Could not find points for polygon in data");
            return;
        }
        for (auto pt = points->begin(); pt < points->end(); ++pt) {
            TimePolygonPoint timePolygonPoint;

            // Coordinates
            auto px = pt->find("x");
            auto py = pt->find("y");
            auto pz = pt->find("z");

            if (px == pt->end() || py == pt->end() || pz == pt->end()) {
                LERROR("Could not find coordinate component for polygon in data");
                return;
            }

            double x, y, z;
            pt->at("x").get_to(x);
            pt->at("y").get_to(y);
            pt->at("z").get_to(z);
            timePolygonPoint.coordinate = glm::dvec3(x, y, z);

            // Value (optional)
            auto v = pt->find("value");
            if (v != pt->end()) {
                float value;
                pt->at("value").get_to(value);
                timePolygonPoint.value = value;
            }

            timePolygon.points.push_back(timePolygonPoint);
        }
        _data.push_back(timePolygon);
    }
}

void RenderableTube::updateTubeData() {
    // Tube needs at least two polygons
    const size_t nPolygons = _data.size();
    if (nPolygons < 2) {
        LERROR("Tube is empty");
        return;
    }

    // Polygon needs at least 3 sides
    // NOTE: assumes all polygons have the same number of points
    const size_t nPoints = _data.front().points.size();
    if (nPoints < 3) {
        LERROR("Polygons need at least 3 edges");
        return;
    }

    _verticies.clear();
    _indicies.clear();

    if (_useSmoothNormals) {
        createSmoothTube(nPolygons, nPoints);
    }
    else {
        createLowPolyTube(nPolygons, nPoints);
    }
}

void RenderableTube::createSmoothTube(size_t nPolygons, size_t nPoints) {
    // Verticies
    // Calciulate the normals for the top and bottom
    glm::dvec3 bottomCenter = _data.front().center;
    glm::dvec3 topCenter = _data.back().center;
    glm::dvec3 bottomNormal = bottomCenter - topCenter;
    glm::dvec3 topNormal = topCenter - bottomCenter;

    // Add the bottom
    if (_addEdges) {
        addBottom(nPoints, bottomCenter, bottomNormal);
    }

    // Add all the polygons that will create the sides of the tube
    for (unsigned int polyIndex = 0; polyIndex < nPolygons; ++polyIndex) {
        for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
            bool isLast = polyIndex == nPolygons - 1;

            PolygonVertex sidePoint;
            sidePoint.position[0] = _data[polyIndex].points[pointIndex].coordinate.x;
            sidePoint.position[1] = _data[polyIndex].points[pointIndex].coordinate.y;
            sidePoint.position[2] = _data[polyIndex].points[pointIndex].coordinate.z;

            // Calculate normal
            glm::dvec3 centerLine = isLast ?
                _data[polyIndex - 1].center - _data[polyIndex].center :
                _data[polyIndex].center - _data[polyIndex + 1].center;
            glm::dvec3 normal = _data[polyIndex].points[pointIndex].coordinate -
                glm::proj(_data[polyIndex].points[pointIndex].coordinate, centerLine) - centerLine;

            sidePoint.normal[0] = normal.x;
            sidePoint.normal[1] = normal.y;
            sidePoint.normal[2] = normal.z;

            sidePoint.value = _data[polyIndex].points[pointIndex].value;
            _verticies.push_back(sidePoint);
        }
    }

    // Add the top
    if (_addEdges) {
        addTop(nPoints, topCenter, topNormal);
    }

    // Indicies
    unsigned int firstSideIndex = _addEdges ? nPoints + 1 : 0;

    // Indices for side triangles
    for (unsigned int polyIndex = 0; polyIndex < nPolygons - 1; ++polyIndex) {
        for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
            unsigned int vIndex = firstSideIndex + pointIndex + polyIndex * nPoints;
            bool isLast = pointIndex == nPoints - 1;

            unsigned int v0 = vIndex;
            unsigned int v1 = v0 + nPoints;
            unsigned int v2 = isLast ? v0 + 1 : v1 + 1;
            unsigned int v3 = isLast ? v0 + 1 - nPoints : v0 + 1;

            // 2 triangles per sector
            _indicies.push_back(v0);
            _indicies.push_back(v2);
            _indicies.push_back(v1);

            _indicies.push_back(v0);
            _indicies.push_back(v3);
            _indicies.push_back(v2);
        }
    }

    if (_addEdges) {
        unsigned int bottomCenterIndex = 0;
        unsigned int topCenterIndex = _verticies.size() - 1;

        // Indices for bottom
        for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
            unsigned int vIndex = pointIndex + 1;
            bool isLast = pointIndex == nPoints - 1;

            unsigned int v0 = bottomCenterIndex;
            unsigned int v1 = vIndex;
            unsigned int v2 = isLast ? v0 + 1 : v1 + 1;

            _indicies.push_back(v0);
            _indicies.push_back(v2);
            _indicies.push_back(v1);
        }

        // Indices for top
        for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
            unsigned int vIndex = topCenterIndex - pointIndex - 1;
            bool isLast = pointIndex == nPoints - 1;

            unsigned int v0 = topCenterIndex;
            unsigned int v1 = vIndex;
            unsigned int v2 = isLast ? v0 - 1 : v1 - 1;

            _indicies.push_back(v0);
            _indicies.push_back(v2);
            _indicies.push_back(v1);
        }
    }
}

void RenderableTube::createLowPolyTube(size_t nPolygons, size_t nPoints) {
    // Verticies
    // Calciulate the normals for the top and bottom
    glm::dvec3 bottomCenter = _data.front().center;
    glm::dvec3 topCenter = _data.back().center;
    glm::dvec3 bottomNormal = bottomCenter - topCenter;
    glm::dvec3 topNormal = topCenter - bottomCenter;

    // Add the bottom
    if (_addEdges) {
        addBottom(nPoints, bottomCenter, bottomNormal);
    }

    // Add all the polygons that will create the sides of the tube
    for (unsigned int polyIndex = 0; polyIndex < nPolygons - 1; ++polyIndex) {
        for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
            TimePolygon currentTimePolygon = _data[polyIndex];
            TimePolygon nextTimePolygon = _data[polyIndex + 1];
            bool isLast = pointIndex == nPoints - 1;

            // Identify all the points that are included in this section
            TimePolygonPoint v0 = currentTimePolygon.points[pointIndex];
            TimePolygonPoint v1 = nextTimePolygon.points[pointIndex];
            TimePolygonPoint v2 = isLast ?
                nextTimePolygon.points[pointIndex + 1 - nPoints] :
                nextTimePolygon.points[pointIndex + 1];
            TimePolygonPoint v3 = isLast ?
                currentTimePolygon.points[pointIndex + 1 - nPoints] :
                currentTimePolygon.points[pointIndex + 1];

            // Calculate normal of this section of the tube
            glm::dvec3 toNextPoly = glm::normalize(v1.coordinate - v0.coordinate);
            glm::dvec3 toNextPoint = glm::normalize(v3.coordinate - v0.coordinate);
            glm::dvec3 normal = glm::cross(toNextPoint, toNextPoly);

            // Create the Verticies for all points in this section
            PolygonVertex sidePointTriangleV0, sidePointTriangleV1, sidePointTriangleV2,
                sidePointTriangleV3;

            // Position
            sidePointTriangleV0.position[0] = v0.coordinate.x;
            sidePointTriangleV0.position[1] = v0.coordinate.y;
            sidePointTriangleV0.position[2] = v0.coordinate.z;

            sidePointTriangleV1.position[0] = v1.coordinate.x;
            sidePointTriangleV1.position[1] = v1.coordinate.y;
            sidePointTriangleV1.position[2] = v1.coordinate.z;

            sidePointTriangleV2.position[0] = v2.coordinate.x;
            sidePointTriangleV2.position[1] = v2.coordinate.y;
            sidePointTriangleV2.position[2] = v2.coordinate.z;

            sidePointTriangleV3.position[0] = v3.coordinate.x;
            sidePointTriangleV3.position[1] = v3.coordinate.y;
            sidePointTriangleV3.position[2] = v3.coordinate.z;

            // Value
            sidePointTriangleV0.value = v0.value;
            sidePointTriangleV1.value = v1.value;
            sidePointTriangleV2.value = v2.value;
            sidePointTriangleV3.value = v3.value;

            // Normal
            sidePointTriangleV0.normal[0] = normal.x;
            sidePointTriangleV0.normal[1] = normal.y;
            sidePointTriangleV0.normal[2] = normal.z;

            sidePointTriangleV1.normal[0] = normal.x;
            sidePointTriangleV1.normal[1] = normal.y;
            sidePointTriangleV1.normal[2] = normal.z;

            sidePointTriangleV2.normal[0] = normal.x;
            sidePointTriangleV2.normal[1] = normal.y;
            sidePointTriangleV2.normal[2] = normal.z;

            sidePointTriangleV3.normal[0] = normal.x;
            sidePointTriangleV3.normal[1] = normal.y;
            sidePointTriangleV3.normal[2] = normal.z;

            // Add all points to the list
            _verticies.push_back(sidePointTriangleV0);
            _verticies.push_back(sidePointTriangleV1);
            _verticies.push_back(sidePointTriangleV2);
            _verticies.push_back(sidePointTriangleV3);
        }
    }

    // Add the top
    if (_addEdges) {
        addTop(nPoints, topCenter, topNormal);
    }

    // Indicies
    unsigned int nPointsPerSection = 4;
    unsigned int vIndex = _addEdges ? nPoints + 1 : 0;

    // Indices for side triangles
    for (unsigned int polyIndex = 0; polyIndex < nPolygons - 1; ++polyIndex) {
        for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
            bool isLast = pointIndex == nPoints - 1;

            unsigned int v0 = vIndex;
            unsigned int v1 = v0 + 1;
            unsigned int v2 = v1 + 1;
            unsigned int v3 = v2 + 1;

            // 2 triangles per sector
            _indicies.push_back(v0);
            _indicies.push_back(v2);
            _indicies.push_back(v1);

            _indicies.push_back(v0);
            _indicies.push_back(v3);
            _indicies.push_back(v2);

            vIndex += nPointsPerSection;
        }
    }

    if (_addEdges) {
        unsigned int bottomCenterIndex = 0;
        unsigned int topCenterIndex = _verticies.size() - 1;

        // Indices for bottom
        for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
            unsigned int vIndex = pointIndex + 1;
            bool isLast = pointIndex == nPoints - 1;

            unsigned int v0 = bottomCenterIndex;
            unsigned int v1 = vIndex;
            unsigned int v2 = isLast ? v0 + 1 : vIndex + 1;

            _indicies.push_back(v0);
            _indicies.push_back(v2);
            _indicies.push_back(v1);
        }

        // Indices for the top
        for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
            unsigned int vIndex = topCenterIndex - pointIndex - 1;
            bool isLast = pointIndex == nPoints - 1;

            unsigned int v0 = topCenterIndex;
            unsigned int v1 = vIndex;
            unsigned int v2 = isLast ? v0 - 1 : vIndex - 1;

            _indicies.push_back(v0);
            _indicies.push_back(v1);
            _indicies.push_back(v2);
        }
    }
}

void RenderableTube::addBottom(size_t nPoints, const glm::dvec3& bottomCenter,
                               const glm::dvec3& bottomNormal)
{
    // Calculate the transfer function value for the center point of the bottom
    float bottomCenterValue = 0.f;
    for (const TimePolygonPoint& timePolygonPoint : _data.front().points) {
        bottomCenterValue += timePolygonPoint.value;
    }
    bottomCenterValue /= nPoints;

    // Add the bottom's center point
    PolygonVertex bottomCenterPoint;
    bottomCenterPoint.position[0] = bottomCenter.x;
    bottomCenterPoint.position[1] = bottomCenter.y;
    bottomCenterPoint.position[2] = bottomCenter.z;

    bottomCenterPoint.normal[0] = bottomNormal.x;
    bottomCenterPoint.normal[1] = bottomNormal.y;
    bottomCenterPoint.normal[2] = bottomNormal.z;

    bottomCenterPoint.value = bottomCenterValue;
    _verticies.push_back(bottomCenterPoint);

    // Add the bottom's sides with proper normals
    // This will ensure a hard shadow on the tube edge
    for (const TimePolygonPoint& timePolygonPoint : _data.front().points) {
        PolygonVertex bottomSidePoint;
        bottomSidePoint.position[0] = timePolygonPoint.coordinate.x;
        bottomSidePoint.position[1] = timePolygonPoint.coordinate.y;
        bottomSidePoint.position[2] = timePolygonPoint.coordinate.z;

        bottomSidePoint.normal[0] = bottomNormal.x;
        bottomSidePoint.normal[1] = bottomNormal.y;
        bottomSidePoint.normal[2] = bottomNormal.z;

        bottomSidePoint.value = timePolygonPoint.value;
        _verticies.push_back(bottomSidePoint);
    }
}

void RenderableTube::addTop(size_t nPoints, const glm::dvec3& topCenter,
                            const glm::dvec3& topNormal)
{
    // Calculate the transfer function value for the center point of the top
    float topCenterValue = 0.f;
    for (const TimePolygonPoint& timePolygonPoint : _data.back().points) {
        topCenterValue += timePolygonPoint.value;
    }
    topCenterValue /= nPoints;

    // Add the top's sides with proper normals
    // This will ensure a hard shadow on the tube edge
    for (const TimePolygonPoint& timePolygonPoint : _data.back().points) {
        PolygonVertex topSidePoint;
        topSidePoint.position[0] = timePolygonPoint.coordinate.x;
        topSidePoint.position[1] = timePolygonPoint.coordinate.y;
        topSidePoint.position[2] = timePolygonPoint.coordinate.z;

        topSidePoint.normal[0] = topNormal.x;
        topSidePoint.normal[1] = topNormal.y;
        topSidePoint.normal[2] = topNormal.z;

        topSidePoint.value = timePolygonPoint.value;
        _verticies.push_back(topSidePoint);
    }

    // Add the top's center point
    PolygonVertex topCenterPoint;
    topCenterPoint.position[0] = topCenter.x;
    topCenterPoint.position[1] = topCenter.y;
    topCenterPoint.position[2] = topCenter.z;

    topCenterPoint.normal[0] = topNormal.x;
    topCenterPoint.normal[1] = topNormal.y;
    topCenterPoint.normal[2] = topNormal.z;

    topCenterPoint.value = topCenterValue;
    _verticies.push_back(topCenterPoint);
}

void RenderableTube::updateBufferData() {
    glBindBuffer(GL_ARRAY_BUFFER, _vboId);
    glBufferData(
        GL_ARRAY_BUFFER,
        _verticies.size() * sizeof(PolygonVertex),
        _verticies.data(),
        GL_STREAM_DRAW
    );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboId);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        _indicies.size() * sizeof(unsigned int),
        _indicies.data(),
        GL_STREAM_DRAW
    );
}

void RenderableTube::render(const RenderData& data, RendererTasks&) {
    _shader->activate();

    // Model transform and view transform needs to be in double precision
    const glm::dmat4 modelViewTransform = calcModelViewTransform(data);
    glm::dmat4 normalTransform = glm::transpose(glm::inverse(modelViewTransform));

    // Uniforms
    _shader->setUniform(_uniformCache.modelViewTransform, glm::mat4(modelViewTransform));
    _shader->setUniform(
        _uniformCache.projectionTransform,
        data.camera.projectionMatrix()
    );
    _shader->setUniform(_uniformCache.normalTransform, glm::mat3(normalTransform));

    _shader->setUniform(_uniformCache.color, _color.value());
    _shader->setUniform(_uniformCache.opacity, opacity());

    // Settings
    if (!_enableFaceCulling) {
        glDisable(GL_CULL_FACE);
    }

    if (_drawWireframe) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

#ifndef __APPLE__
        glLineWidth(_wireLineWidth);
#else
        glLineWidth(1.f);
#endif
    }

    _shader->setUniform(_uniformCache.hasTransferFunction, _hasTransferFunction);
    if (_hasTransferFunction) {
        ghoul::opengl::TextureUnit transferFunctionUnit;
        transferFunctionUnit.activate();
        _transferFunction->texture().bind();
        _shader->setUniform(_uniformCache.transferFunction, transferFunctionUnit);
    }

    int nLightSources = 0;
    _lightIntensitiesBuffer.resize(_lightSources.size());
    _lightDirectionsViewSpaceBuffer.resize(_lightSources.size());
    for (const std::unique_ptr<LightSource>& lightSource : _lightSources) {
        if (!lightSource->isEnabled()) {
            continue;
        }
        _lightIntensitiesBuffer[nLightSources] = lightSource->intensity();
        _lightDirectionsViewSpaceBuffer[nLightSources] =
            lightSource->directionViewSpace(data);

        ++nLightSources;
    }

    if (_uniformCache.performShading != -1) {
        _shader->setUniform(_uniformCache.performShading, _shading.enabled);
    }

    if (_shading.enabled) {
        _shader->setUniform(_uniformCache.nLightSources, nLightSources);
        _shader->setUniform(_uniformCache.lightIntensities, _lightIntensitiesBuffer);
        _shader->setUniform(
            _uniformCache.lightDirectionsViewSpace,
            _lightDirectionsViewSpaceBuffer
        );

        _shader->setUniform(_uniformCache.ambientIntensity, _shading.ambientIntensity);
        _shader->setUniform(_uniformCache.diffuseIntensity, _shading.diffuseIntensity);
        _shader->setUniform(_uniformCache.specularIntensity, _shading.specularIntensity);
    }

    // Render
    glBindVertexArray(_vaoId);

    glDrawElements(
        GL_TRIANGLES,
        static_cast<GLsizei>(_indicies.size()),
        GL_UNSIGNED_INT,
        nullptr
    );

    // Reset
    if (!_enableFaceCulling) {
        glEnable(GL_CULL_FACE);
    }

    if (_drawWireframe) {
        glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
        global::renderEngine->openglStateCache().resetLineState();
    }

    glBindVertexArray(0);
    global::renderEngine->openglStateCache().resetLineState();

    _shader->deactivate();
}

void RenderableTube::update(const UpdateData& data) {
    if (_hasTransferFunction) {
        _transferFunction->update();
    }

    if (_shader->isDirty()) {
        _shader->rebuildFromFile();
        ghoul::opengl::updateUniformLocations(*_shader, _uniformCache, UniformNames);
    }
    if (_tubeIsDirty) {
        updateTubeData();
        updateBufferData();
        //setBoundingSphere(_length * glm::compMax(data.modelTransform.scale));
        _tubeIsDirty = false;
    }
}

} // namespace openspace
