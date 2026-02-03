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
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/logging/logmanager.h>
#include <algorithm>
#include <execution>
#include <numeric>

namespace {
    constexpr std::string_view _loggerCat = "RenderableVectorField";

    constexpr openspace::properties::Property::PropertyInfo StrideInfo = {
        "Stride",
        "Stride",
        "Controls how densely vectors are sampled from the volume, a stride of 1 renders "
        "every vector."
    };

    constexpr openspace::properties::Property::PropertyInfo VectorFieldScaleInfo = {
        "VectorFieldScale",
        "Vector field scale",
        "Scales the vector field lines using an exponential scale."
    };

    constexpr openspace::properties::Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line width",
        "Specifies the line width of the vector lines."
    };

    constexpr openspace::properties::Property::PropertyInfo DataRangeInfo = {
        "DataRange",
        "Data range",
        "Specifies the data range to render the vector field in, magnitudes outside this "
        "range will be filtered away."
    };

    constexpr openspace::properties::Property::PropertyInfo FilterOutOfRangeInfo = {
        "FilterOutOfRange",
        "Filter out of range",
        "Determines whether other data values outside the value range should be visible "
        "or filtered away."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorByMagnitudeInfo = {
        "ColorByMagnitude",
        "Color by magnitude",
        "If enabled, color the vector field based on the min and max magnitudes defined"
        "in the volume."
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "Color texture",
        "The path to the texture used to color the vector field."
    };

    constexpr openspace::properties::Property::PropertyInfo FilterByLuaInfo = {
        "FilterByLua",
        "Filter by Lua script",
        "If enabled, the vector field is filtered by the provided custom Lua script."
    };

    constexpr openspace::properties::Property::PropertyInfo FilterScriptInfo = {
        "FilterScript",
        "Filter script",
        "This value is the path to the Lua script that will be executed to compute the "
        "filtering. The script needs to define a function 'filter' that takes the "
        "current voxel position (x, y, z) given in galactic coordinates, and the "
        "velocity vector (vx, vy, vz). The function should return true/false if "
        "the voxel should be visualized or discarded, respectively."
    };

    constexpr openspace::properties::Property::PropertyInfo MagnitudeDataRangeInfo = {
        "MagnitudeDataRange",
        "Magnitude data range",
        "The computed magnitude data range used to normalize the magnitude value for "
        "color lookup."
    };

    struct [[codegen::Dictionary(RenderableVectorField)]] Parameters {
        // The path to the file containing the volume data.
        std::filesystem::path volumeFile;

        // [[codegen::verbatim(StrideInfo.description)]]
        int stride;

        // The min domain values that the volume should be mapped to.
        glm::dvec3 minDomain;

        // The max domain values that the volume should be mapped to.
        glm::dvec3 maxDomain;

        // The dimensions of the volume data.
        glm::dvec3 dimensions;

        // [[codegen::verbatim(VectorFieldScaleInfo.description)]]
        std::optional<double> vectorFieldScale;

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<double> lineWidth;

        // [[codegen::verbatim(DataRangeInfo.description)]]
        std::optional<glm::dvec2> dataRange;

        // [[codegen::verbatim(FilterOutOfRangeInfo.description)]]
        std::optional<bool> filterOutOfRange;

        // [[codgen::verbatim(ColorByMagnitudeInfo.description)]]
        std::optional<bool> colorByMagnitude;

        // [[codgen::verbatim(ColorTextureInfo.description)]]
        std::optional<std::filesystem::path> colorMapFile;

        // [[codegen::verbatim(FilterByLuaInfo.description)]]
        std::optional<bool> filterByLua;

        // [[codegen::verbatim(FilterScriptInfo.description)]]
        std::optional<std::filesystem::path> script;
    };

#include "renderablevolumevectorfield_codegen.cpp"
} // namespace

namespace openspace::volume {

documentation::Documentation RenderableVectorField::Documentation() {
    return codegen::doc<Parameters>("volume_renderable_vectorfield");
}

RenderableVectorField::RenderableVectorField(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _dataRange(
        DataRangeInfo,
        glm::vec2(0.f, 1.f),
        glm::vec2(0.f),
        glm::vec2(1500.f),
        glm::vec2(1.f)
    )
    , _filterOutOfRange(FilterOutOfRangeInfo, false)
    , _magnitudeDomain(
        MagnitudeDataRangeInfo,
        glm::vec2(std::numeric_limits<float>::max(),
        std::numeric_limits<float>::lowest()), glm::vec2(0.f), glm::vec2(1500.f),
        glm::vec2(1.f)
    )
    , _stride(StrideInfo, 1, 1, 16)
    , _vectorFieldScale(VectorFieldScaleInfo, 1.f, 1.f, 100.f)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 10.f)
    , _colorByMagnitude(ColorByMagnitudeInfo, false)
    , _colorTexturePath(ColorTextureInfo)
    , _filterByLua(FilterByLuaInfo, false)
    , _luaScriptFile(FilterScriptInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _sourceFile = p.volumeFile.string();
    _minDomain = p.minDomain;
    _maxDomain = p.maxDomain;
    _dimensions = p.dimensions;

    _vectorFieldScale = static_cast<float>(p.vectorFieldScale.value_or(1.f));
    _lineWidth = static_cast<float>(p.lineWidth.value_or(1.f));
    _dataRange = p.dataRange.value_or(glm::vec2(0, 1));
    _filterOutOfRange = p.filterOutOfRange.value_or(false);
    _colorByMagnitude = p.colorByMagnitude.value_or(false);

    if (p.colorMapFile.has_value()) {
        _colorTexturePath = p.colorMapFile.value().string();
    }

    _colorTexturePath.onChange([this]() {
        if (std::filesystem::exists(_colorTexturePath.value())) {
            _textureIsDirty = true;
        }
        else {
            LWARNING(std::format("File not found: '{}'", _colorTexturePath.value()));
        }
    });

    _filterByLua = p.filterByLua.value_or(false) && p.script.has_value();
    _filterByLua.onChange([this]() {_vectorFieldIsDirty = true; });

    if (p.script.has_value()) {
        _luaScriptFile = p.script.value().string();
        // Subscribe to changes in the Lua script file, @TODO can this be combined with
        // the duplicate code below?
        _luaScriptFileHandle =
            std::make_unique<ghoul::filesystem::File>(_luaScriptFile.value()
        );
        _luaScriptFileHandle->setCallback([this]() {
            _vectorFieldIsDirty = true;
        });
    }

    _luaScriptFile.onChange([this]() {
        _vectorFieldIsDirty = true;
        _luaScriptFileHandle =
            std::make_unique<ghoul::filesystem::File>(_luaScriptFile.value()
        );
        _luaScriptFileHandle->setCallback([this]() {
            _vectorFieldIsDirty = true;
        });
    });

    _stride = p.stride;
    _stride.onChange([this]() { _vectorFieldIsDirty = true; });

    addProperty(_luaScriptFile);
    addProperty(_stride);
    addProperty(_vectorFieldScale);
    addProperty(_lineWidth);
    addProperty(_colorTexturePath);
    addProperty(_colorByMagnitude);
    addProperty(_magnitudeDomain);
    addProperty(_filterByLua);
    addProperty(_filterOutOfRange);
    addProperty(_dataRange);

    global::scriptEngine->initializeLuaState(_state);
}

void RenderableVectorField::initialize() {
    std::filesystem::path dataFile = absPath(_sourceFile);
    if (!std::filesystem::is_regular_file(dataFile)) {
        throw ghoul::RuntimeError(std::format("Could not load data file '{}'", dataFile));
    }

    RawVolumeReader<VelocityData> reader(dataFile, _dimensions);
    _volumeData = reader.read();

    _volumeData->forEachVoxel(
        [this](const glm::uvec3&, const VelocityData& data) {
            float magnitude = glm::length(glm::vec3(data.vx, data.vy, data.vz));
            const glm::vec2& mag = _magnitudeDomain.value();
            _magnitudeDomain = glm::vec2(
                std::min(mag.x, magnitude),
                std::max(mag.y, magnitude)
            );
        }
    );

    computeFieldLinesParallel();
    _vectorFieldIsDirty = false;
}

void RenderableVectorField::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "vectorfield",
        absPath("${MODULE_VOLUME}/shaders/vectorfield_vs.glsl"),
        absPath("${MODULE_VOLUME}/shaders/vectorfield_fs.glsl")
    );

    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_vectorFieldVbo);
    glGenBuffers(1, &_arrowVbo);

    glBindVertexArray(_vao);

    // Arrow geometry
    glBindBuffer(GL_ARRAY_BUFFER, _arrowVbo);
    glBufferData(
        GL_ARRAY_BUFFER,
        _arrowVertices.size() * sizeof(glm::vec3),
        _arrowVertices.data(),
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

    glBindBuffer(GL_ARRAY_BUFFER, _vectorFieldVbo);
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

void RenderableVectorField::deinitializeGL() {
    glDeleteVertexArrays(1, &_vao);
    _vao = 0;
    glDeleteBuffers(1, &_vectorFieldVbo);
    _vectorFieldVbo = 0;
    glDeleteBuffers(1, &_arrowVbo);
    _arrowVbo = 0;

    _colorTexture = nullptr;

    if (_program) {
        global::renderEngine->removeRenderProgram(_program.get());
        _program = nullptr;
    }
}

bool RenderableVectorField::isReady() const {
    return _program != nullptr;
}

void RenderableVectorField::render(const RenderData& data, RendererTasks&) {
    if (_instances.empty()) {
        return;
    }

    _program->activate();

    _program->setUniform(
        _uniformCache.modelViewProjection,
        glm::mat4(calcModelViewProjectionTransform(data))
    );

    _program->setUniform(_uniformCache.arrowScale, _vectorFieldScale);
    _program->setUniform(_uniformCache.filterOutOfRange, _filterOutOfRange);
    _program->setUniform(_uniformCache.dataRangeFilter, _dataRange);
    _program->setUniform(_uniformCache.colorByMag, _colorByMagnitude);
    _program->setUniform(_uniformCache.magDomain, _magnitudeDomain);

    if (_colorTexture) {
        ghoul::opengl::TextureUnit colorUnit;
        colorUnit.activate();
        _colorTexture->bind();
        _program->setUniform(_uniformCache.colorTexture, colorUnit);
    }

    glBindVertexArray(_vao);
    glLineWidth(_lineWidth.value());
    glDrawArraysInstanced(
        GL_LINES,
        0,
        static_cast<GLsizei>(_arrowVertices.size()),
        static_cast<GLsizei>(_instances.size())
    );
    glBindVertexArray(0);
    _program->deactivate();
}

void RenderableVectorField::update(const UpdateData&) {
    if (_vectorFieldIsDirty) [[unlikely]] {
        computeFieldLinesParallel();

        glBindBuffer(GL_ARRAY_BUFFER, _vectorFieldVbo);
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

    if (_textureIsDirty) [[unlikely]] {
        _colorTexture = nullptr;

        if (!_colorTexturePath.value().empty()) {
            _colorTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_colorTexturePath),
                1
            );

            if (_colorTexture) {
                LDEBUG(std::format("Loaded texture '{}'", _colorTexturePath.value()));
                _colorTexture->setFilter(
                    ghoul::opengl::Texture::FilterMode::Nearest
                );
                _colorTexture->setWrapping(
                    ghoul::opengl::Texture::WrappingMode::ClampToEdge
                );
                _colorTexture->uploadTexture();
            }
        }
        _textureIsDirty = false;
    }
}

void RenderableVectorField::computeFieldLinesParallel() {
    LDEBUG("Computing vector field");

    // Divide volume into blocks of stride * stride * stride voxels
    const unsigned int numBlocksX = (_dimensions.x + _stride - 1) / _stride;
    const unsigned int numBlocksY = (_dimensions.y + _stride - 1) / _stride;
    const unsigned int numBlocksZ = (_dimensions.z + _stride - 1) / _stride;
    const size_t totalBlocks = static_cast<size_t>(numBlocksX) * numBlocksY * numBlocksZ;

    const glm::uvec3 blockDims(numBlocksX, numBlocksY, numBlocksZ);

    _instances.clear();
    _instances.resize(totalBlocks);

    auto computeArrowInstance = [&blockDims, this](size_t blockIdx) {
        // Convert linear block index to 3D block coordinates
        glm::uvec3 blockCoords = indexToCoords(blockIdx, blockDims);

        // Convert block coordinates to starting voxel position
        const unsigned int x = blockCoords.x * _stride;
        const unsigned int y = blockCoords.y * _stride;
        const unsigned int z = blockCoords.z * _stride;

        // Compute average velocity accross all voxels in this block
        glm::vec3 avgVelocity = glm::vec3(0.f);
        int count = 0;

        for (int dz = 0; dz < _stride; dz++) {
            for (int dy = 0; dy < _stride; dy++) {
                for (int dx = 0; dx < _stride; dx++) {
                    unsigned int ix = x + dx;
                    unsigned int iy = y + dy;
                    unsigned int iz = z + dz;

                    // Skip indices outside data volume
                    if (ix >= _dimensions.x || iy >= _dimensions.y || iz >= _dimensions.z) {
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
            x + 0.5f * _stride,
            y + 0.5f * _stride,
            z + 0.5f * _stride
        );

        // Transform from voxel space [0, dims] to world space [minDomain, maxDomain]
        glm::vec3 normalized = blockCenterVoxel / static_cast<glm::vec3>(_dimensions);
        glm::dvec3 minD = _minDomain;
        glm::dvec3 maxD = _maxDomain;
        glm::dvec3 startPos = minD + glm::dvec3(normalized) * (maxD - minD);

        float magnitude = glm::length(avgVelocity);
        glm::vec3 direction = (magnitude > 0.f) ?
            glm::normalize(avgVelocity) :
            glm::vec3(0.f);

        _instances[blockIdx] = ArrowInstance(startPos, direction, magnitude);
    };

    std::vector<size_t> blockIndices(totalBlocks);
    std::iota(blockIndices.begin(), blockIndices.end(), 0);

    std::for_each(
        std::execution::par,
        blockIndices.begin(),
        blockIndices.end(),
        computeArrowInstance
    );

    // The bounding sphere radius is the distance to the furthest corner of the domain
    double boundingSphereRadius = glm::length(glm::max(
        glm::abs(_minDomain),
        glm::abs(_maxDomain))
    );

    setBoundingSphere(boundingSphereRadius);

    if (_instances.empty()) {
        throw ghoul::RuntimeError("Couldn't compute vector field segments");
    }

    if (_filterByLua) {
        std::string path = _luaScriptFile.value();
        if (path.empty()) {
            LERROR(std::format(
                "Trying to filter data using an empty script file '{}'", path
            ));
            return;
        }

        // Load the Lua script
        ghoul::lua::runScriptFile(_state, path);
        // Get the filter function
        lua_getglobal(_state, "filter");
        const bool isFunction = lua_isfunction(_state, -1);
        if (!isFunction) {
            LERROR(std::format("Script '{}' does not have a function 'filter'", path));
            return;
        }

        _instances.erase(
            std::remove_if(
                _instances.begin(),
                _instances.end(),
                [this](const ArrowInstance& i) {
                    // Get the filter function
                    lua_getglobal(_state, "filter");
                    // First argument (x,y,z) is the averaged position of the arrow
                    ghoul::lua::push(_state, i.position);
                    // Second argument (vx, vy, vz) is the averaged direction vector
                    ghoul::lua::push(_state, i.direction);

                    const int success = lua_pcall(_state, 2, 1, 0);

                    if (success != 0) {
                        LERROR(std::format(
                            "Error executing 'filter': {}", lua_tostring(_state, -1)
                        ));
                    }

                    // The Lua function returns true for the values that should be kept
                    return !ghoul::lua::value<bool>(_state);
                }
            ),
            _instances.end()
        );
    }
}

} // namespace openspace::volume
