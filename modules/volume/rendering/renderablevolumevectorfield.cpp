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

#include <modules/volume/rendering/renderablevolumevectorfield.h>

#include <modules/volume/rawvolumereader.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/logging/logmanager.h>


namespace {
    constexpr std::string_view _loggerCat = "RenderableVectorField";

    constexpr openspace::properties::Property::PropertyInfo StrideInfo = {
        "Stride",
        "Stride",
        "The stride to use when downsampling the volume data"
    };

    constexpr openspace::properties::Property::PropertyInfo VectorFieldScaleInfo = {
        "VectorFieldScale",
        "Vector Field Scale",
        "Scales the vector field lines"
    };

    constexpr openspace::properties::Property::PropertyInfo VolumeDataInfo = {
        "VolumeFile",
        "Volume file",
        "The path to the file containing the volume data"
    };

    constexpr openspace::properties::Property::PropertyInfo MinDomainInfo = {
        "MinDomain",
        "Min Domain",
        "The min domain values that the volume should be mapped to"
    };

    constexpr openspace::properties::Property::PropertyInfo MaxDomainInfo = {
        "MaxDomain",
        "Max Domain",
        "The max domain values that the volume should be mapped to"
    };

    constexpr openspace::properties::Property::PropertyInfo DimensionsInfo = {
        "Dimensions",
        "Dimensions",
        "The dimensions of the volume data"
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line width",
        "Specifies the line width of the trail lines, if the selected rendering method "
        "includes lines. If the rendering mode is Points, this value is ignored.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableVectorField)]] Parameters {
        // [[codegen::verbatim(VolumeDataInfo.description)]]
        std::filesystem::path volumeFile;
        // [[codegen::verbatim(StrideInfo.description)]]
        int stride;
        // [[codegen::verbatim(MinDomainInfo.description)]]
        glm::dvec3 minDomain;
        // [[codegen::verbatim(MaxDomainInfo.description)]]
        glm::dvec3 maxDomain;
        // [[codegen::verbatim(DimensionsInfo.description)]]
        glm::dvec3 dimensions;
        // [[codegen::verbatim(VectorFieldScaleInfo.description)]]
        std::optional<double> vectorFieldScale;

    };

#include "renderablevolumevectorfield_codegen.cpp"
} // namespace

namespace openspace::volume {

documentation::Documentation RenderableVectorField::Documentation() {
    return codegen::doc<Parameters>("volume_renderable_vectorfield");

}

RenderableVectorField::RenderableVectorField(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _sourceFile(VolumeDataInfo)
    , _minDomain(MinDomainInfo)
    , _maxDomain(MaxDomainInfo)
    , _dimensions(DimensionsInfo)
    , _stride(StrideInfo, 0, 0, 16)
    , _vectorFieldScale(VectorFieldScaleInfo, 1.f, 1.f, 100.f)
    , _lineWidth(LineWidthInfo, 1.0, 0.5f, 10.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sourceFile = p.volumeFile.string();
    _minDomain = p.minDomain;
    _maxDomain = p.maxDomain;
    _dimensions = p.dimensions;
    _stride = p.stride;
    _vectorFieldScale = static_cast<float>(p.vectorFieldScale.value_or(1.f));

    _stride.onChange([this]() { _vectorFieldIsDirty = true; });
    _vectorFieldScale.onChange([this]() { _vectorFieldIsDirty = true; });

    addProperty(_stride);
    addProperty(_vectorFieldScale);
    addProperty(_lineWidth);
    addProperty(_minDomain);
    addProperty(_maxDomain);
    addProperty(_dimensions);
}

void RenderableVectorField::initializeGL()
{
    std::filesystem::path dataFile = absPath(_sourceFile);
    if (!std::filesystem::is_regular_file(dataFile)) {
        throw ghoul::RuntimeError(std::format("Could not load data file '{}'", dataFile));
    }

    RawVolumeReader<VelocityData> reader(dataFile, _dimensions);
    _volumeData = reader.read();

    computeFieldLines();

    _program = global::renderEngine->buildRenderProgram(
        "vectorfield",
        absPath("${MODULE_VOLUME}/shaders/vectorfield_vs.glsl"),
        absPath("${MODULE_VOLUME}/shaders/vectorfield_fs.glsl")
    );

    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);

    glBindVertexArray(_vao);
    glBindBuffer(GL_ARRAY_BUFFER, _vbo);

    static_assert(sizeof(Vertex) == 6 * sizeof(float),
        "Vertex layout is not tightly packed!"
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(Vertex),
        (void*)offsetof(Vertex, position)
    );
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(Vertex),
        (void*)offsetof(Vertex, direction)
    );

    glBufferData(
        GL_ARRAY_BUFFER,
        _vertices.size() * sizeof(Vertex),
        _vertices.data(),
        GL_STATIC_DRAW
    );

    glBindVertexArray(0);

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache);

}

void RenderableVectorField::deinitializeGL()
{
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;

    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

bool RenderableVectorField::isReady() const
{
    return _program != nullptr;
}

void RenderableVectorField::render(const RenderData& data, RendererTasks& renderTask)
{
    if (_vertices.empty()) {
        return;
    }

    _program->activate();

    _program->setUniform(
        _uniformCache.modelViewProjection,
        glm::mat4(calcModelViewProjectionTransform(data))
    );

    glBindVertexArray(_vao);
    glLineWidth(_lineWidth.value());
    glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_vertices.size()));
    glBindVertexArray(0);
    _program->deactivate();

}

void RenderableVectorField::update(const UpdateData& data)
{
    if (_vectorFieldIsDirty) [[unlikely]] {
        computeFieldLines();

        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glBufferData(
            GL_ARRAY_BUFFER,
            _vertices.size() * sizeof(Vertex),
            _vertices.data(),
            GL_STATIC_DRAW
        );

        _vectorFieldIsDirty = false;

        if (_program->isDirty()) [[unlikely]] {
            _program->rebuildFromFile();
            ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
        }
    }
}

void RenderableVectorField::computeFieldLines() {
    const glm::uvec3 dims = _dimensions.value();
    int stride = _stride.value();

    _vertices.clear();
    _vertices.reserve(2 * dims.x * dims.y * dims.z / (stride * stride * stride));

    for (unsigned int z = 0; z < dims.z; z += stride) {
        for (unsigned int y = 0; y < dims.y; y += stride) {
            for (unsigned int x = 0; x < dims.x; x += stride) {
                glm::vec3 avgVelocity = glm::vec3(0.f);
                int count = 0;

                for (int dz = 0; dz < stride; dz++) {
                    for (int dy = 0; dy < stride; dy++) {
                        for (int dx = 0; dx < stride; dx++) {
                            unsigned int ix = x + dx;
                            unsigned int iy = y + dy;
                            unsigned int iz = z + dz;

                            if (ix >= dims.x || iy >= dims.y || iz >= dims.z) {
                                continue;
                            }

                            const VelocityData& v = _volumeData->get(glm::uvec3(ix, iy, iz));
                            avgVelocity += glm::vec3(v.vx, v.vy, v.vz);
                            count++;

                        }
                    }
                }

                avgVelocity /= static_cast<float>(count);

                glm::dvec3 worldSize = _maxDomain.value() - _minDomain.value();
                glm::dvec3 voxelSize =
                    worldSize / static_cast<glm::dvec3>(_dimensions.value());

                glm::vec3 blockCenterVoxel(
                    x + 0.5f * stride,
                    y + 0.5f * stride,
                    z + 0.5f * stride
                );
                glm::vec3 normalized = blockCenterVoxel / static_cast<glm::vec3>(dims);

                glm::dvec3 minD = _minDomain.value();
                glm::dvec3 maxD = _maxDomain.value();

                glm::dvec3 worldPos = minD + glm::dvec3(normalized) * (maxD - minD);

                glm::dvec3 endPos = worldPos + glm::dvec3(avgVelocity * _vectorFieldScale.value());
                glm::vec3 direction = glm::normalize(avgVelocity);

                if (count > 0 && glm::length(avgVelocity) > 0.f) {
                    _vertices.push_back({ worldPos, direction });
                    _vertices.push_back({ endPos, direction });
                }
            }
        }
    }

    if (_vertices.empty()) {
        throw ghoul::RuntimeError("Couldn't compute line segments");
    }

    _vectorFieldIsDirty = false;

}

} // namespace openspace::volume
