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
#include <execution>
#include <numeric>

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
        "includes lines. If the rendering mode is Points, this value is ignored."
    };

    constexpr openspace::properties::Property::PropertyInfo FilterOutOfRangeInfo = {
        "FilterOutOfRange",
        "Filter out of range",
        "Determines whether other data values outside the value range should be visible "
        "or filtered away."
    };

    constexpr openspace::properties::Property::PropertyInfo DataRangeInfo = {
        "DataRange",
        "Data Range",
        "Specifies the data range to render the vector field in, magnitudes outside this "
        "range will be filtered away."
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
        // [[codegen::verbatim(DataRangeInfo.description)]]
        std::optional<glm::dvec2> dataRange;
        // [[codegen::verbatim(FilterOutOfRangeInfo.description)]]
        std::optional<bool> filterOutOfRange;

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
    , _dataRange(DataRangeInfo,glm::vec2(0.f, 1.f), glm::vec2(0.f), glm::vec2(1000.f), glm::vec2(1.f))
    , _filterOutOfRange(FilterOutOfRangeInfo, false)
    , _stride(StrideInfo, 1, 1, 16)
    , _vectorFieldScale(VectorFieldScaleInfo, 1.f, 1.f, 100.f)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 10.f)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sourceFile = p.volumeFile.string();
    _minDomain = p.minDomain;
    _maxDomain = p.maxDomain;
    _dimensions = p.dimensions;
    _stride = p.stride;
    _vectorFieldScale = static_cast<float>(p.vectorFieldScale.value_or(1.f));
    _dataRange = p.dataRange.value_or(glm::vec2(0, 1));
    _filterOutOfRange = p.filterOutOfRange.value_or(false);

    _stride.onChange([this]() { _vectorFieldIsDirty = true; });

    addProperty(_stride);
    addProperty(_vectorFieldScale);
    addProperty(_lineWidth);
    addProperty(_filterOutOfRange);
    addProperty(_dataRange);
}

void RenderableVectorField::initializeGL()
{
    std::filesystem::path dataFile = absPath(_sourceFile);
    if (!std::filesystem::is_regular_file(dataFile)) {
        throw ghoul::RuntimeError(std::format("Could not load data file '{}'", dataFile));
    }

    RawVolumeReader<VelocityData> reader(dataFile, _dimensions);
    _volumeData = reader.read();

    computeFieldLinesParallel();

    _program = global::renderEngine->buildRenderProgram(
        "vectorfield",
        absPath("${MODULE_VOLUME}/shaders/vectorfield_vs.glsl"),
        absPath("${MODULE_VOLUME}/shaders/vectorfield_fs.glsl")
    );

    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vbo);
    glGenBuffers(1, &_arrowVbo);

    glBindVertexArray(_vao);

    // Arrow geometry
    glBindBuffer(GL_ARRAY_BUFFER, _arrowVbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        arrowVertices.size() * sizeof(glm::vec3),
        arrowVertices.data(),
        GL_STATIC_DRAW
    );

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(
        0,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(glm::vec3),
        nullptr
    );

    static_assert(sizeof(ArrowInstance) == 7 * sizeof(float),
        "Vertex layout is not tightly packed!"
    );

    glBindBuffer(GL_ARRAY_BUFFER, _vbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _instances.size() * sizeof(ArrowInstance),
        _instances.data(),
        GL_DYNAMIC_DRAW
    );

    // Position
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(
        1,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(ArrowInstance),
        (void*)offsetof(ArrowInstance, position)
    );
    glVertexAttribDivisor(1, 1);

    // Direction
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(
        2,
        3,
        GL_FLOAT,
        GL_FALSE,
        sizeof(ArrowInstance),
        (void*)offsetof(ArrowInstance, direction)
    );
    glVertexAttribDivisor(2, 1);

    // Magnitude
    glEnableVertexAttribArray(3);
    glVertexAttribPointer(
        3,
        1,
        GL_FLOAT,
        GL_FALSE,
        sizeof(ArrowInstance),
        (void*)offsetof(ArrowInstance, magnitude)
    );
    glVertexAttribDivisor(3, 1);

    glBindVertexArray(0);

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache);

}

void RenderableVectorField::deinitializeGL()
{
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vbo);
    _vbo = 0;
    glDeleteBuffers(1, &_arrowVbo);
    _arrowVbo = 0;

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
    if (_instances.empty()) {
        return;
    }

    _program->activate();

    _program->setUniform(
        _uniformCache.modelViewProjection,
        glm::mat4(calcModelViewProjectionTransform(data))
    );

    _program->setUniform(
        _uniformCache.arrowScale,
        _vectorFieldScale.value()
    );

    _program->setUniform(
        _uniformCache.filterOutOfRange,
        _filterOutOfRange
    );

    _program->setUniform(
        _uniformCache.dataRangeFilter,
        _dataRange
    );

    glBindVertexArray(_vao);
    glLineWidth(_lineWidth.value());
    glDrawArraysInstanced(
        GL_LINES,
        0,
        static_cast<GLsizei>(arrowVertices.size()),
        static_cast<GLsizei>(_instances.size())
    );
    //glDrawArrays(GL_LINES, 0, static_cast<GLsizei>(_instances.size()));
    glBindVertexArray(0);
    _program->deactivate();

}

void RenderableVectorField::update(const UpdateData& data)
{
    if (_vectorFieldIsDirty) [[unlikely]] {
        computeFieldLinesParallel();

        glBindBuffer(GL_ARRAY_BUFFER, _vbo);
        glBufferData(
            GL_ARRAY_BUFFER,
            _instances.size() * sizeof(ArrowInstance),
            _instances.data(),
            GL_DYNAMIC_DRAW
        );

        _vectorFieldIsDirty = false;

        if (_program->isDirty()) [[unlikely]] {
            _program->rebuildFromFile();
            ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
        }
    }
}

void RenderableVectorField::computeFieldLinesParallel() {
    const glm::uvec3 dims = _dimensions.value();
    int stride = _stride.value();

    // Divide volume into blocks of stride * stride * stride voxels
    const unsigned int numBlocksX = (dims.x + stride - 1) / stride;
    const unsigned int numBlocksY = (dims.y + stride - 1) / stride;
    const unsigned int numBlocksZ = (dims.z + stride - 1) / stride;
    const size_t totalBlocks = static_cast<size_t>(numBlocksX) * numBlocksY * numBlocksZ;

    const glm::uvec3 blockDims(numBlocksX, numBlocksY, numBlocksZ);

    _instances.clear();
    _instances.resize(totalBlocks);

    std::vector<size_t> blockIndices(totalBlocks);
    std::iota(blockIndices.begin(), blockIndices.end(), 0);

    std::for_each(std::execution::par, blockIndices.begin(), blockIndices.end(),
        [&](size_t blockIdx) {
            // Convert linear block index to 3D block coordinates
            glm::uvec3 blockCoords = indexToCoords(blockIdx, blockDims);

            // Convert block coordinates to starting voxel position
            const unsigned int x = blockCoords.x * stride;
            const unsigned int y = blockCoords.y * stride;
            const unsigned int z = blockCoords.z * stride;

            // Compute average velocity accross all voxels in this block
            glm::vec3 avgVelocity = glm::vec3(0.f);
            int count = 0;

            for (int dz = 0; dz < stride; dz++) {
                for (int dy = 0; dy < stride; dy++) {
                    for (int dx = 0; dx < stride; dx++) {
                        unsigned int ix = x + dx;
                        unsigned int iy = y + dy;
                        unsigned int iz = z + dz;

                        // Skip indices outside data volume
                        if (ix >= dims.x || iy >= dims.y || iz >= dims.z) {
                            continue;
                        }

                        const VelocityData& v = _volumeData->get(glm::uvec3(ix, iy, iz));
                        avgVelocity += glm::vec3(v.vx, v.vy, v.vz);
                        count++;
                    }
                }
            }

            if (count == 0) {
                return;
            }

            avgVelocity /= static_cast<float>(count);

            // Calculate block center in voxel space
            glm::vec3 blockCenterVoxel(
                x + 0.5f * stride,
                y + 0.5f * stride,
                z + 0.5f * stride
            );

            // Transform from voxel space [0, dims] to world space [minDomain, maxDomain]
            glm::vec3 normalized = blockCenterVoxel / static_cast<glm::vec3>(dims);
            glm::dvec3 minD = _minDomain.value();
            glm::dvec3 maxD = _maxDomain.value();
            glm::dvec3 startPos = minD + glm::dvec3(normalized) * (maxD - minD);

            float magnitude = glm::length(avgVelocity);
            glm::vec3 direction = (magnitude > 0.f) ? glm::normalize(avgVelocity) : glm::vec3(0.f);

            _instances[blockIdx] = ArrowInstance(startPos, direction, magnitude);
        }
    );

    if (_instances.empty()) {
        throw ghoul::RuntimeError("Couldn't compute line segments");
    }
    _vectorFieldIsDirty = false;
}

} // namespace openspace::volume
