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

#include <modules/volume/rendering/renderablevectorfield.h>

#include <modules/volume/rawvolumereader.h>
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/csvreader.h>
#include <ghoul/opengl/texture.h>
#include <ghoul/opengl/textureunit.h>
#include <algorithm>
#include <array>
#include <execution>
#include <numeric>

namespace {
    using namespace openspace;

    constexpr std::string_view _loggerCat = "RenderableVectorField";

    enum ColorMode {
        Fixed = 0,
        Magnitude,
        Direction
    };

    constexpr Property::PropertyInfo StrideInfo = {
        "Stride",
        "Stride",
        "Controls how densely vectors are sampled from the volume. A stride of 1 renders "
        "every vector.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FixedColorInfo = {
        "FixedColor",
        "Fixed color",
        "The color of the vectors, when no color map is used.",
        Property::Visibility::NoviceUser
    };

    constexpr Property::PropertyInfo ColorModeOptionInfo = {
        "ColorModeOption",
        "Color mode option",
        "Controls how the vectors are colored. \"Fixed\" color all vectors with the same "
        "static color. \"Magnitude\" color the vector field based on the min and max "
        "magnitudes defined in the volume. \"Direction\" colors the vector based on "
        "their direction, as follows:"
        "+X -> Red, -X -> Cyan"
        "+Y -> Green, -Y -> Magenta"
        "+Z -> Blue, -Z -> Yellow",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo VectorFieldScaleInfo = {
        "VectorFieldScale",
        "Vector field scale",
        "Scales the vector field lines using an exponential scale.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo LineWidthInfo = {
        "LineWidth",
        "Line width",
        "The width of the vector lines.",
        Property::Visibility::NoviceUser
    };

    constexpr Property::PropertyInfo ColorTextureInfo = {
        "ColorMap",
        "Color texture",
        "The path to the texture used to color the vector field.",
        Property::Visibility::User
    };

    constexpr Property::PropertyInfo ColorMappingDataRangeInfo = {
        "ColorMappingDataRange",
        "Color mapping data range",
        "The magnitude data range used to normalize the magnitude value for color "
        "lookup. Computed from the volume data if unspecified.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FilterByLuaInfo = {
        "FilterByLua",
        "Filter by Lua script",
        "If enabled, the vector field is filtered by the provided custom Lua script.",
        Property::Visibility::AdvancedUser
    };

    constexpr Property::PropertyInfo FilterScriptInfo = {
        "FilterScript",
        "Filter script",
        "This value is the path to the Lua script that will be executed to compute the "
        "filtering. The script needs to define a function 'filter' that takes the "
        "current voxel position (x, y, z) given in galactic coordinates, and the "
        "velocity vector (vx, vy, vz). The function should return true/false if "
        "the voxel should be visualized or discarded, respectively.",
        Property::Visibility::AdvancedUser
    };

    // A RenderableVectorField can be used to render vectors from a given 3D volumetric
    // dataset or a sparse point-like dataset, optionally including color mapping and
    // custom filtering via Lua script.
    //
    // If `mode` is set to `Volume` the vector field is defined on a regular 3D grid
    // specified by `Dimensions`, and occupies the spatial domain defined by `MinDomain`
    // and `MaxDomain`.
    //
    // If `mode` is set to `Sparse` the vector field is defined by the position given by
    // the carteesian coordinates and direction (e.g., velocity).
    //
    // The `Stride` parameter controls how densely vectors are sampled from the volume
    // (a stride of 1 renders every vector).
    //
    // There are several coloring methods to choose from. By default the vectors are
    // colored with a fixed color. Other options includes using a color map to coloring
    // the vectors by the direction magnitude. And lastly color by direction, similar to a
    // 3D model normal map as follows:
    // +X -> Red, -X -> Cyan
    // +Y -> Green, -Y -> Magenta
    // +Z -> Blue, -Z -> Yellow
    struct [[codegen::Dictionary(RenderableVectorField)]] Parameters {
        enum class Mode {
            Volume,
            Sparse
        };

        // The mode determines the type of vectorfield data that is loaded. In `volume`
        // mode, the vectorfield is computed from a 3D volume data file, the volume is
        // mapped to the domain [min, max]. In `sparse` mode the vectorfield is computed
        // given the specified position and velocity components from a CSV file.
        Mode mode;

        struct Volume {
            // The path to the file containing the volume data.
            std::filesystem::path volumeFile;

            // The min domain values that the volume should be mapped to.
            glm::vec3 minDomain;

            // The max domain values that the volume should be mapped to.
            glm::vec3 maxDomain;

            // The dimensions of the volume data.
            glm::ivec3 dimensions;
        };

        std::optional<Volume> volume;

        struct Sparse {
            // The path to the CSV file containing the vector field data.
            std::filesystem::path filePath;

            // Specifies the column name for the x coordinate.
            std::optional<std::string> x;

            // Specifies the column name for the y coordinate.
            std::optional<std::string> y;

            // Specifies the column name for the z coordinate.
            std::optional<std::string> z;

            // Specifies the column name for the vx velocity component.
            std::optional<std::string> vx;

            // Specifies the column name for the vy velocity component.
            std::optional<std::string> vy;

            // Specifies the column name for the vz velocity component.
            std::optional<std::string> vz;
        };

        std::optional<Sparse> sparse;

        // [[codegen::verbatim(StrideInfo.description)]]
        std::optional<int> stride;

        struct ColorSettings {
            enum class [[codegen::map(ColorMode)]] ColorMode {
                Fixed [[codegen::key("Fixed")]],
                Magnitude [[codegen::key("Magnitude")]],
                Direction [[codegen::key("Direction")]]
            };

            // [[codegen::verbatim(ColorModeOptionInfo.description)]]
            ColorMode colorMode;

            // [[codegen::verbatim(FixedColorInfo.description)]]
            std::optional<glm::vec4> fixedColor [[codegen::color()]];

            // [[codgen::verbatim(ColorTextureInfo.description)]]
            std::optional<std::filesystem::path> colorMapFile;

            // [[codegen::verbatim(ColorMappingDataRangeInfo.description)]]
            std::optional<glm::vec2> colorMappingRange;
        };

        // Settings related to the coloring of the vectors, such as a fixed color,
        // color map, etc.
        std::optional<ColorSettings> coloring;

        // [[codegen::verbatim(VectorFieldScaleInfo.description)]]
        std::optional<float> vectorFieldScale [[codegen::greater(1.f)]];

        // [[codegen::verbatim(LineWidthInfo.description)]]
        std::optional<float> lineWidth [[codegen::greater(0.f)]];

        // [[codegen::verbatim(FilterByLuaInfo.description)]]
        std::optional<bool> filterByLua;

        // [[codegen::verbatim(FilterScriptInfo.description)]]
        std::optional<std::filesystem::path> script;
    };
} // namespace
#include "renderablevectorfield_codegen.cpp"

namespace openspace {

Documentation RenderableVectorField::Documentation() {
    return codegen::doc<Parameters>("volume_renderable_vectorfield");
}

RenderableVectorField::ColorSettings::ColorSettings(const ghoul::Dictionary& dictionary)
    : PropertyOwner({ "Coloring", "Coloring" })
    , colorModeOption(ColorModeOptionInfo)
    , colorTexturePath(ColorTextureInfo)
    , colorMagnitudeDomain(
        ColorMappingDataRangeInfo,
        glm::vec2(
            std::numeric_limits<float>::max(),
            std::numeric_limits<float>::lowest()
        ),
        glm::vec2(0.f),
        glm::vec2(1500.f),
        glm::vec2(1.f)
    )
    , fixedColor(FixedColorInfo, glm::vec4(1.f), glm::vec4(0.f), glm::vec4(1.f))
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    colorModeOption.addOption(ColorMode::Fixed, "Fixed");
    colorModeOption.addOption(ColorMode::Magnitude, "Magnitude");
    colorModeOption.addOption(ColorMode::Direction, "Direction");

    colorModeOption = ColorMode::Fixed;
    addProperty(colorModeOption);
    fixedColor.setViewOption(Property::ViewOptions::Color);
    addProperty(fixedColor);
    addProperty(colorTexturePath);
    addProperty(colorMagnitudeDomain);

    const bool hasColoring = p.coloring.has_value();
    if (hasColoring) {
        const Parameters::ColorSettings settings = *p.coloring;

        colorModeOption = codegen::map<ColorMode>(settings.colorMode);
        fixedColor = settings.fixedColor.value_or(fixedColor);
        if (settings.colorMapFile.has_value()) {
            colorTexturePath = settings.colorMapFile->string();
        }
        colorMagnitudeDomain = settings.colorMappingRange.value_or(
            colorMagnitudeDomain
        );
        shouldComputeMagnitudeRange = !p.coloring->colorMappingRange.has_value();
    }
}

RenderableVectorField::RenderableVectorField(const ghoul::Dictionary& dictionary)
    : Renderable(dictionary)
    , _colorSettings(dictionary)
    , _stride(StrideInfo, 1, 1, 16)
    , _vectorFieldScale(VectorFieldScaleInfo, 1.f, 1.f, 100.f)
    , _lineWidth(LineWidthInfo, 1.f, 1.f, 10.f)
    , _filterByLua(FilterByLuaInfo, false)
    , _luaScriptFile(FilterScriptInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    if (p.mode == Parameters::Mode::Volume && !p.volume.has_value()) {
        throw ghoul::RuntimeError(
            "When selecting the `Volume` mode, a `Volume` table must be specified."
        );
    }
    if (p.mode == Parameters::Mode::Sparse && !p.sparse.has_value()) {
        throw ghoul::RuntimeError(
            "When selecting the `Sparse` mode, a `Sparse` table must be specified."
        );
    }

    addProperty(Fadeable::_opacity);

    if (p.volume.has_value()) {
        _mode = Mode::Volume;

        _sourceFile = p.volume->volumeFile;
        _volume.minDomain = p.volume->minDomain;
        _volume.maxDomain = p.volume->maxDomain;
        _volume.dimensions = p.volume->dimensions;
    }

    if (p.sparse.has_value()) {
        _mode = Mode::Sparse;

        _sourceFile = p.sparse->filePath;
        _sparse.xColumnName = p.sparse->x.value_or("x");
        _sparse.yColumnName = p.sparse->y.value_or("y");
        _sparse.zColumnName = p.sparse->z.value_or("z");
        _sparse.vxColumnName = p.sparse->vx.value_or("vx");
        _sparse.vyColumnName = p.sparse->vy.value_or("vy");
        _sparse.vzColumnName = p.sparse->vz.value_or("vz");
    }

    addPropertySubOwner(_colorSettings);

    _colorSettings.colorTexturePath.onChange([this]() {
        if (std::filesystem::exists(_colorSettings.colorTexturePath.value())) {
            _textureIsDirty = true;
        }
        else {
            LWARNING(std::format("File not found: '{}'",
                _colorSettings.colorTexturePath.value()
            ));
        }
    });

    _stride = p.stride.value_or(_stride);
    _stride.onChange([this]() {
        if (_stride < 1) {
            _stride = 1;
        }
        _vectorFieldIsDirty = true;
    });
    addProperty(_stride);

    _vectorFieldScale = p.vectorFieldScale.value_or(_vectorFieldScale);
    addProperty(_vectorFieldScale);

    _lineWidth = p.lineWidth.value_or(_lineWidth);
    addProperty(_lineWidth);

    _filterByLua = p.filterByLua.value_or(_filterByLua) && p.script.has_value();
    _filterByLua.onChange([this]() {
        if (std::filesystem::exists(_luaScriptFile.value())) {
            _vectorFieldIsDirty = true;
        }
        else {
            LWARNING(std::format(
                "No filter function file found: '{}'", _luaScriptFile.value())
            );
        }
    });
    addProperty(_filterByLua);

    // Subscribe to changes in the Lua script file
    _luaScriptFile.onChange([this]() {
        _vectorFieldIsDirty = true;
        _luaScriptFileHandle = std::make_unique<ghoul::filesystem::File>(
            _luaScriptFile.value()
        );
        _luaScriptFileHandle->setCallback([this]() {
            _vectorFieldIsDirty = true;
        });
    });

    if (p.script.has_value()) {
        _luaScriptFile = p.script->string();
    }
    addProperty(_luaScriptFile);

    global::scriptEngine->initializeLuaState(_state);
}

void RenderableVectorField::initialize() {
    std::filesystem::path dataFile = absPath(_sourceFile);
    if (!std::filesystem::is_regular_file(dataFile)) {
        throw ghoul::RuntimeError(std::format("Could not load data file '{}'", dataFile));
    }

    switch (_mode) {
        case Mode::Volume:
            loadVolumeData(dataFile);
            computeVolumeFieldLines();
            break;
        case Mode::Sparse:
            loadCSVData(dataFile);
            computeSparseFieldLines();
            break;
        default:
            throw ghoul::MissingCaseException();
    }

    applyLuaFilter();
    _vectorFieldIsDirty = false;
}

void RenderableVectorField::initializeGL() {
    _program = global::renderEngine->buildRenderProgram(
        "Vectorfield",
        absPath("${MODULE_VOLUME}/shaders/vectorfield_vs.glsl"),
        absPath("${MODULE_VOLUME}/shaders/vectorfield_fs.glsl"),
        absPath("${MODULE_VOLUME}/shaders/vectorfield_gs.glsl")
    );

    glCreateVertexArrays(1, &_vao);
    glCreateBuffers(1, &_vectorFieldVbo);
    glVertexArrayVertexBuffer(_vao, 0, _vectorFieldVbo, 0, sizeof(ArrowInstance));

    glNamedBufferData(
        _vectorFieldVbo,
        _instances.size() * sizeof(ArrowInstance),
        _instances.data(),
        GL_DYNAMIC_DRAW
    );

    // Position
    GLuint posAttribLocation = 0;
    glEnableVertexArrayAttrib(_vao, posAttribLocation);
    glVertexArrayAttribFormat(_vao, posAttribLocation, 3, GL_FLOAT, GL_FALSE, 0);
    glVertexArrayAttribBinding(_vao, posAttribLocation, 0);

    // Direction
    GLuint dirAttribLocation = 1;
    glEnableVertexArrayAttrib(_vao, dirAttribLocation);
    glVertexArrayAttribFormat(
        _vao,
        dirAttribLocation,
        3,
        GL_FLOAT,
        GL_FALSE,
        offsetof(ArrowInstance, direction)
    );
    glVertexArrayAttribBinding(_vao, dirAttribLocation, 0);

    // Magnitude
    GLuint magAttribLocation = 2;
    glEnableVertexArrayAttrib(_vao, magAttribLocation);
    glVertexArrayAttribFormat(
        _vao,
        magAttribLocation,
        1,
        GL_FLOAT,
        GL_FALSE,
        offsetof(ArrowInstance, magnitude)
    );
    glVertexArrayAttribBinding(_vao, magAttribLocation, 0);

    ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
}

void RenderableVectorField::deinitializeGL() {
    glDeleteVertexArrays(1, &_vao);
    glDeleteBuffers(1, &_vectorFieldVbo);

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

    _program->setUniform(_uniformCache.opacity, opacity());
    _program->setUniform(_uniformCache.arrowScale, _vectorFieldScale);
    _program->setUniform(_uniformCache.colorMode, _colorSettings.colorModeOption);
    _program->setUniform(_uniformCache.fixedColor, _colorSettings.fixedColor);
    _program->setUniform(_uniformCache.magDomain, _colorSettings.colorMagnitudeDomain);

    ghoul::opengl::TextureUnit colorUnit;
    if (_colorTexture) {
        colorUnit.bind(*_colorTexture);
        _program->setUniform(_uniformCache.colorTexture, colorUnit);
    }

    glBindVertexArray(_vao);
    glLineWidth(_lineWidth);
    glDrawArrays(GL_POINTS, 0, static_cast<GLsizei>(_instances.size()));
    glBindVertexArray(0);
    _program->deactivate();
}

void RenderableVectorField::update(const UpdateData&) {
    if (_vectorFieldIsDirty) [[unlikely]] {
        switch (_mode) {
            case Mode::Volume:
                computeVolumeFieldLines();
                break;
            case Mode::Sparse:
                computeSparseFieldLines();
                break;
            default:
                throw ghoul::MissingCaseException();
        }
        applyLuaFilter();

        glNamedBufferData(
            _vectorFieldVbo,
            _instances.size() * sizeof(ArrowInstance),
            _instances.data(),
            GL_DYNAMIC_DRAW
        );

        if (_program->isDirty()) [[unlikely]] {
            _program->rebuildFromFile();
            ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
        }

        _vectorFieldIsDirty = false;
    }

    if (_textureIsDirty) [[unlikely]] {
        _colorTexture = nullptr;

        if (!_colorSettings.colorTexturePath.value().empty()) {
            _colorTexture = ghoul::io::TextureReader::ref().loadTexture(
                absPath(_colorSettings.colorTexturePath),
                1,
                ghoul::opengl::Texture::SamplerInit{
                    .filter = ghoul::opengl::Texture::FilterMode::Nearest,
                    .wrapping = ghoul::opengl::Texture::WrappingMode::ClampToEdge
                }
            );

            LDEBUG(std::format(
                "Loaded texture '{}'",
                _colorSettings.colorTexturePath.value()
            ));
        }

        _textureIsDirty = false;
    }
}

void RenderableVectorField::applyLuaFilter() {
    if (!_filterByLua) {
        return;
    }
    std::filesystem::path path = _luaScriptFile.value();
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
            [&state = _state](const ArrowInstance& i) {
                // Get the filter function
                lua_getglobal(state, "filter");
                // First argument (x,y,z) is the averaged position of the arrow
                ghoul::lua::push(state, i.position);
                // Second argument (vx, vy, vz) is the averaged direction vector
                ghoul::lua::push(state, i.direction);

                const int success = lua_pcall(state, 2, 1, 0);

                if (success != 0) {
                    LERROR(std::format(
                        "Error executing 'filter': {}", lua_tostring(state, -1)
                    ));
                }

                const bool shouldBeKept = ghoul::lua::value<bool>(state);
                return !shouldBeKept;
            }
        ),
        _instances.end()
    );
}

void RenderableVectorField::computeVolumeFieldLines() {
    LDEBUG("Computing vector field");

    // Divide volume into blocks of stride * stride * stride voxels
    const uint64_t numBlocksX = (_volume.dimensions.x + _stride - 1) / _stride;
    const uint64_t numBlocksY = (_volume.dimensions.y + _stride - 1) / _stride;
    const uint64_t numBlocksZ = (_volume.dimensions.z + _stride - 1) / _stride;
    const uint64_t totalBlocks = numBlocksX * numBlocksY * numBlocksZ;

    const glm::uvec3 blockDims = glm::uvec3(numBlocksX, numBlocksY, numBlocksZ);

    _instances.clear();
    _instances.resize(totalBlocks);

    auto computeArrowInstance = [this, &blockDims](size_t blockIdx) {
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
                    if (ix >= _volume.dimensions.x ||
                        iy >= _volume.dimensions.y ||
                        iz >= _volume.dimensions.z)
                    {
                        continue;
                    }
                    const glm::uvec3 indexCoordinates = glm::uvec3(ix, iy, iz);
                    const VelocityData& v = _volume.volumeData->get(indexCoordinates);
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
        glm::vec3 blockCenterVoxel = glm::vec3(
            x + 0.5f * _stride,
            y + 0.5f * _stride,
            z + 0.5f * _stride
        );

        // Transform from voxel space [0, dims] to world space [minDomain, maxDomain]
        glm::vec3 normalized = blockCenterVoxel /
            static_cast<glm::vec3>(_volume.dimensions);
        glm::vec3 minD = _volume.minDomain;
        glm::vec3 maxD = _volume.maxDomain;
        glm::vec3 startPos = minD + glm::vec3(normalized) * (maxD - minD);

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
    float boundingSphereRadius = glm::length(
        glm::max(glm::abs(_volume.minDomain), glm::abs(_volume.maxDomain))
    );

    setBoundingSphere(boundingSphereRadius);

    if (_instances.empty()) {
        throw ghoul::RuntimeError("Couldn't compute vector field segments");
    }
}

void RenderableVectorField::computeSparseFieldLines() {
    if (_sparse.data.empty()) {
        return;
    }

    const size_t totalInstances = (_sparse.data.size() + _stride - 1) / _stride;
    _instances.clear();
    _instances.resize(totalInstances);


    std::vector<size_t> indices(totalInstances);
    std::iota(indices.begin(), indices.end(), 0);


    auto computeArrowInstance = [this](size_t instanceIdx) {
        const size_t dataIdx = instanceIdx * _stride;
        const glm::vec3& position = _sparse.data[dataIdx].position;
        const glm::vec3& velocity = _sparse.data[dataIdx].velocity;

        float magnitude = glm::length(velocity);
        glm::vec3 direction = (magnitude > 0.f) ?
            glm::normalize(velocity) :
            glm::vec3(0.f);

        _instances[instanceIdx] = (ArrowInstance(position, direction, magnitude));
    };

    std::for_each(
        std::execution::par,
        indices.begin(),
        indices.end(),
        computeArrowInstance
    );

    // Compute the min and max domain boundaries
    glm::vec3 minDomain = _sparse.data[0].position;
    glm::vec3 maxDomain = _sparse.data[0].position;
    for (const auto& [position, velocity] : _sparse.data) {
        minDomain = glm::min(minDomain, position);
        maxDomain = glm::max(maxDomain, position);
    }

    // The bounding sphere radius is the distance to the furthest corner of the domain
    float boundingSphereRadius = glm::length(
        glm::max(glm::abs(minDomain), glm::abs(maxDomain))
    );
    setBoundingSphere(boundingSphereRadius);

    if (_instances.empty()) {
        throw ghoul::RuntimeError("Couldn't compute vector field segments");
    }
}

void RenderableVectorField::loadVolumeData(const std::filesystem::path& path) {
    RawVolumeReader<VelocityData> reader(path, _volume.dimensions);
    _volume.volumeData = reader.read();

    if (_colorSettings.shouldComputeMagnitudeRange) {
        _volume.volumeData->forEachVoxel(
            [this](const glm::uvec3&, const VelocityData& data) {
                float magnitude = glm::length(glm::vec3(data.vx, data.vy, data.vz));
                const glm::vec2& mag = _colorSettings.colorMagnitudeDomain;
                _colorSettings.colorMagnitudeDomain = glm::vec2(
                    std::min(mag.x, magnitude),
                    std::max(mag.y, magnitude)
                );
            }
        );
    }
}

void RenderableVectorField::loadCSVData(const std::filesystem::path& path) {
    std::vector<std::vector<std::string>> rows = ghoul::loadCSVFile(path, true);
    if (rows.size() < 2) {
        throw ghoul::RuntimeError(std::format(
            "Error loading data file '{}'. No data items read", path
        ));
    }

    const std::vector<std::string>& columns = rows.front();

    std::optional<size_t> xColumn;
    std::optional<size_t> yColumn;
    std::optional<size_t> zColumn;

    std::optional<size_t> vxColumn;
    std::optional<size_t> vyColumn;
    std::optional<size_t> vzColumn;

    // Find the data indices for position and direction
    for (size_t i = 0; i < columns.size(); i++) {
        const std::string& column = columns[i];
        if (column == _sparse.xColumnName) {
            xColumn = i;
        }
        else if (column == _sparse.yColumnName) {
            yColumn = i;
        }
        else if (column == _sparse.zColumnName) {
            zColumn = i;
        }
        else if (column == _sparse.vxColumnName) {
            vxColumn = i;
        }
        else if (column == _sparse.vyColumnName) {
            vyColumn = i;
        }
        else if (column == _sparse.vzColumnName) {
            vzColumn = i;
        }
    }

    if (!xColumn.has_value() || !yColumn.has_value() || !zColumn.has_value() ||
        !vxColumn.has_value() || !vyColumn.has_value() || !vzColumn.has_value())
    {
        throw ghoul::RuntimeError(std::format(
            "Error loading data file '{}'. Missing position or direction column", path
        ));
    }

    auto readFloatData = [](const std::string& str) -> float {
        float result = 0.f;
#ifdef WIN32
        auto [p, ec] = std::from_chars(str.data(), str.data() + str.size(), result);
        if (ec == std::errc() && std::isfinite(result)) {
            return result;
        }
        return std::numeric_limits<float>::quiet_NaN();
#else // ^^^^ WIN32 // !WIN32 vvvv
        // clang is missing float support for std::from_chars
        try {
            result = std::stof(str, nullptr);
            if (std::isfinite(result)) {
                return result;
            }
        }
        catch (const std::invalid_argument&) {}
        return NAN;
#endif // WIN32
    };

    // Skip first row (column names)
    _sparse.data.reserve(rows.size() - 1);

    for (size_t rowIdx = 1; rowIdx < rows.size(); rowIdx++) {
        const std::vector<std::string>& row = rows[rowIdx];

        glm::vec3 position;
        glm::vec3 velocity;
        for (size_t i = 0; i < row.size(); i++) {
            const float value = readFloatData(row[i]);

            if (*xColumn == i) {
                position.x = value;
            }
            else if (*yColumn == i) {
                position.y = value;
            }
            else if (*zColumn == i) {
                position.z = value;
            }
            else if (*vxColumn == i) {
                velocity.x = value;
            }
            else if (*vyColumn == i) {
                velocity.y = value;
            }
            else if (*vzColumn == i) {
                velocity.z = value;
            }
        }
        _sparse.data.push_back({ position, velocity });

        if (_colorSettings.shouldComputeMagnitudeRange) {
            float magnitude = glm::length(velocity);
            const glm::vec2& mag = _colorSettings.colorMagnitudeDomain;
            _colorSettings.colorMagnitudeDomain = glm::vec2(
                std::min(mag.x, magnitude),
                std::max(mag.y, magnitude)
            );
        }
    }
}

} // namespace openspace
