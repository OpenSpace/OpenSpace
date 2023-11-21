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
#include <glm/gtx/projection.hpp>
#include <optional>

using json = nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "RenderableTube";
    constexpr int8_t CurrentMajorVersion = 0;
    constexpr int8_t CurrentMinorVersion = 1;
    constexpr std::array<const char*, 9> UniformNames = {
        "modelViewTransform", "projectionTransform", "normalTransform", "color",
        "opacity", "performShading", "nLightSources", "lightDirectionsViewSpace",
        "lightIntensities"
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

    struct [[codegen::Dictionary(RenderableTube)]] Parameters {
        // The input file with data for the tube
        std::string file;

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
    , diffuseIntensity(DiffuseIntensityInfo, 0.7f, 0.f, 1.f)
    , specularIntensity(SpecularIntensityInfo, 0.f, 0.f, 1.f)
{
    addProperty(enabled);
    addProperty(ambientIntensity);
    addProperty(diffuseIntensity);
    addProperty(specularIntensity);
}

RenderableTube::RenderableTube(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _color(ColorInfo, glm::vec3(1.f), glm::vec3(0.f), glm::vec3(1.f))
    , _enableFaceCulling(EnableFaceCullingInfo, true)
    , _lightSourcePropertyOwner({ "LightSources", "Light Sources" })
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _dataFile = p.file;

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
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), nullptr);

    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 6 * sizeof(float), (void*)(sizeof(float) * 3));

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

    // convert to a json object
    json jsonData = json::parse(data);

    // Ceck version
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

        // Coordinates
        auto points = it->find("points");
        if (points == it->end() || points->size() < 1) {
            LERROR("Could not find points for polygon in data");
            return;
        }
        for (auto pt = points->begin(); pt < points->end(); ++pt) {
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

            glm::dvec3 point(x, y, z);
            timePolygon.points.push_back(point);
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

    _vertexArray.clear();
    _indexArray.clear();

    // Verticies
    // Calculate the center points for the first and last polygon
    glm::dvec3 firstCenter = glm::dvec3(0.0);
    for (const glm::dvec3& coord : _data.front().points) {
        firstCenter += coord;
    }
    firstCenter /= nPoints;

    glm::dvec3 lastCenter = glm::dvec3(0.0);
    for (const glm::dvec3& coord : _data.back().points) {
        lastCenter += coord;
    }
    lastCenter /= nPoints;

    // Calciulate the normals of the first and last poylgon
    glm::dvec3 firstNormal = firstCenter - lastCenter;
    glm::dvec3 lastNormal = lastCenter - firstCenter;

    // Add the first polygon's center point
    _vertexArray.push_back(firstCenter.x);
    _vertexArray.push_back(firstCenter.y);
    _vertexArray.push_back(firstCenter.z);

    _vertexArray.push_back(firstNormal.x);
    _vertexArray.push_back(firstNormal.y);
    _vertexArray.push_back(firstNormal.z);

    // Add the first polygon's sides with proper normals
    // This will ensure a hard shadow on the tube edge
    for (const glm::dvec3& coord : _data.front().points) {
        _vertexArray.push_back(coord.x);
        _vertexArray.push_back(coord.y);
        _vertexArray.push_back(coord.z);

        _vertexArray.push_back(firstNormal.x);
        _vertexArray.push_back(firstNormal.y);
        _vertexArray.push_back(firstNormal.z);
    }

    // Add all the polygons that will create the sides of the tube
    for (const TimePolygon& poly : _data) {
        for (const glm::dvec3& coord : poly.points) {
            _vertexArray.push_back(coord.x);
            _vertexArray.push_back(coord.y);
            _vertexArray.push_back(coord.z);

            // Calculate normal
            glm::dvec3 normal = coord - glm::proj(coord, firstNormal) - firstNormal;
            _vertexArray.push_back(normal.x);
            _vertexArray.push_back(normal.y);
            _vertexArray.push_back(normal.z);
        }
    }

    // Add the last polygon's center point
    _vertexArray.push_back(lastCenter.x);
    _vertexArray.push_back(lastCenter.y);
    _vertexArray.push_back(lastCenter.z);

    _vertexArray.push_back(lastNormal.x);
    _vertexArray.push_back(lastNormal.y);
    _vertexArray.push_back(lastNormal.z);

    // Add the last polygon's sides with proper normals
    // This will ensure a hard shadow on the tube edge
    for (const glm::dvec3& coord : _data.back().points) {
        _vertexArray.push_back(coord.x);
        _vertexArray.push_back(coord.y);
        _vertexArray.push_back(coord.z);

        _vertexArray.push_back(lastNormal.x);
        _vertexArray.push_back(lastNormal.y);
        _vertexArray.push_back(lastNormal.z);
    }

    // Indicies
    unsigned int firstCenterIndex = 0;
    unsigned int firstSideIndex = 4;
    unsigned int lastCenterIndex = _vertexArray.size() / 6 - 1;

    // Indices for side triangles
    for (unsigned int polyIndex = 0; polyIndex < nPolygons - 1; ++polyIndex) {
        for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
            unsigned int vIndex = firstSideIndex + pointIndex + polyIndex * nPolygons;
            bool isLast = pointIndex == nPoints - 1;

            unsigned int v0 = vIndex;
            unsigned int v1 = v0 + nPoints;
            unsigned int v2 = isLast ? v0 + 1 : v0 + nPoints + 1;
            unsigned int v3 = isLast ? v0 + 1 - nPoints : v0 + 1;

            // 2 triangles per sector
            _indexArray.push_back(v0);
            _indexArray.push_back(v1);
            _indexArray.push_back(v2);

            _indexArray.push_back(v0);
            _indexArray.push_back(v2);
            _indexArray.push_back(v3);
        }
    }

    // Indices for first polygon that will be the bottom
    for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
        unsigned int vIndex = pointIndex + 1;
        bool isLast = pointIndex == nPoints - 1;

        unsigned int v0 = firstCenterIndex;
        unsigned int v1 = vIndex;
        unsigned int v2 = isLast ? v0 + 1 : vIndex + 1;

        _indexArray.push_back(v0);
        _indexArray.push_back(v1);
        _indexArray.push_back(v2);
    }

    // Indices for last polygon that will be the top
    for (unsigned int pointIndex = 0; pointIndex < nPoints; ++pointIndex) {
        unsigned int vIndex = lastCenterIndex - pointIndex - 1;
        bool isLast = pointIndex == nPoints - 1;

        unsigned int v0 = lastCenterIndex;
        unsigned int v1 = vIndex;
        unsigned int v2 = isLast ? v0 - 1 : vIndex - 1;

        _indexArray.push_back(v0);
        _indexArray.push_back(v1);
        _indexArray.push_back(v2);
    }
}

void RenderableTube::updateBufferData() {
    glBindBuffer(GL_ARRAY_BUFFER, _vboId);
    glBufferData(
        GL_ARRAY_BUFFER,
        _vertexArray.size() * sizeof(float),
        _vertexArray.data(),
        GL_STREAM_DRAW
    );

    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, _iboId);
    glBufferData(
        GL_ELEMENT_ARRAY_BUFFER,
        _indexArray.size() * sizeof(unsigned int),
        _indexArray.data(),
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
    }

    // Render
    glBindVertexArray(_vaoId);

    glDrawElements(
        GL_TRIANGLES,
        static_cast<GLsizei>(_indexArray.size()),
        GL_UNSIGNED_INT,
        nullptr
    );

    // Reset
    if (!_enableFaceCulling) {
        glEnable(GL_CULL_FACE);
    }
    glBindVertexArray(0);
    global::renderEngine->openglStateCache().resetLineState();

    _shader->deactivate();
}

void RenderableTube::update(const UpdateData& data) {
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
