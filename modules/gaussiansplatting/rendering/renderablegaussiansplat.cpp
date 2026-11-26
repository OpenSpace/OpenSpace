#include <modules/gaussiansplatting/rendering/renderablegaussiansplat.h>

#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/logging/logmanager.h>
#include <openspace/opengl/programobject.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/util/updatestructures.h>
#include <glm/gtc/matrix_transform.hpp>
#include <glm/gtc/quaternion.hpp>
#include <algorithm>
#include <cmath>
#include <cstring>
#include <format>
#include <fstream>
#include <numeric>
#include <sstream>

using namespace gl;

namespace {
    constexpr std::string_view _loggerCat = "RenderableGaussianSplat";
    constexpr float SH_C0 = 0.28209479177387814f;

constexpr openspace::StringPropertyInfo FilePathInfo = {
        .identifier = "FilePath",
        .name = "File Path",
        .description = "The file path to the binary .ply Gaussian Splat dataset.",
        .visibility = openspace::Visibility::User
    };

    constexpr openspace::FloatPropertyInfo OpacityInfo = {
        .identifier = "Opacity",
        .name = "Opacity",
        .description = "Global opacity multiplier for all splats.",
        .visibility = openspace::Visibility::NoviceUser,
        .defaults = {
            .value = 1.0f,
            .minMax = { .min = 0.0f, .max = 1.0f }
        }
    };

    constexpr openspace::FloatPropertyInfo ScaleMultiplierInfo = {
        .identifier = "ScaleMultiplier",
        .name = "Scale Multiplier",
        .description = "Multiplier applied to the 3D scale of each Gaussian splat.",
        .visibility = openspace::Visibility::User,
        .defaults = {
            .value = 1.0f,
            .minMax = { .min = 0.01f, .max = 10.0f }
        }
    };

    constexpr openspace::IntPropertyInfo ShDegreeInfo = {
        .identifier = "ShDegree",
        .name = "Spherical Harmonics Degree",
        .description = "Active spherical harmonics degree (0-3) for view-dependent color.",
        .visibility = openspace::Visibility::AdvancedUser,
        .defaults = {
            .value = 0,
            .minMax = { .min = 0, .max = 3 }
        }
    };

    constexpr openspace::BoolPropertyInfo FrustumCullingInfo = {
        .identifier = "EnableFrustumCulling",
        .name = "Enable Frustum Culling",
        .description = "Culls splats outside the view frustum during depth sorting.",
        .visibility = openspace::Visibility::AdvancedUser,
        .defaults = {
            .value = true
        }
    };
    struct [[codegen::Dictionary(RenderableGaussianSplat)]] Parameters {
        // [[codegen::verbatim(FilePathInfo.description)]]
        std::filesystem::path filePath;

        // [[codegen::verbatim(OpacityInfo.description)]]
        std::optional<float> opacity [[codegen::inrange(0.0, 1.0)]];

        // [[codegen::verbatim(ScaleMultiplierInfo.description)]]
        std::optional<float> scaleMultiplier [[codegen::greater(0.0)]];

        // [[codegen::verbatim(ShDegreeInfo.description)]]
        std::optional<int> shDegree [[codegen::inrange(0, 3)]];

        // [[codegen::verbatim(FrustumCullingInfo.description)]]
        std::optional<bool> enableFrustumCulling;
    };
} // namespace

#include "renderablegaussiansplat_codegen.cpp"

namespace openspace {

Documentation RenderableGaussianSplat::Documentation() {
    return codegen::doc<Parameters>("gaussiansplatting_renderablegaussiansplat");
}

RenderableGaussianSplat::RenderableGaussianSplat(const Dictionary& dictionary)
    : Renderable(dictionary)
    , _filePath(FilePathInfo)
    , _opacity(OpacityInfo)
    , _scaleMultiplier(ScaleMultiplierInfo)
    , _shDegree(ShDegreeInfo)
    , _enableFrustumCulling(FrustumCullingInfo)
{
    const Parameters p = codegen::bake<Parameters>(dictionary);

    setRenderBin(RenderBin::PostDeferredTransparent);

    _filePath = p.filePath.string();
    _filePath.onChange([this]() {
        loadPlyFile(absPath(_filePath.value()));
    });
    addProperty(_filePath);

    _opacity = p.opacity.value_or(_opacity);
    addProperty(_opacity);

    _scaleMultiplier = p.scaleMultiplier.value_or(_scaleMultiplier);
    addProperty(_scaleMultiplier);

    _shDegree = p.shDegree.value_or(_shDegree);
    addProperty(_shDegree);

    _enableFrustumCulling = p.enableFrustumCulling.value_or(_enableFrustumCulling);
    addProperty(_enableFrustumCulling);
}

void RenderableGaussianSplat::initializeGL() {
    _shaderProgram = global::renderEngine->buildRenderProgram(
        "GaussianSplatProgram",
        absPath("${MODULE_GAUSSIANSPLATTING}/shaders/splat_vs.glsl"),
        absPath("${MODULE_GAUSSIANSPLATTING}/shaders/splat_fs.glsl")
    );

    glGenVertexArrays(1, &_vao);
    glGenBuffers(1, &_splatSsbo);
    glGenBuffers(1, &_sortedIndexSsbo);

    loadPlyFile(absPath(_filePath.value()));
}

void RenderableGaussianSplat::deinitializeGL() {
    if (_splatSsbo != 0) {
        glDeleteBuffers(1, &_splatSsbo);
        _splatSsbo = 0;
    }
    if (_sortedIndexSsbo != 0) {
        glDeleteBuffers(1, &_sortedIndexSsbo);
        _sortedIndexSsbo = 0;
    }
    if (_vao != 0) {
        glDeleteVertexArrays(1, &_vao);
        _vao = 0;
    }

    global::renderEngine->removeRenderProgram(_shaderProgram.get());
    _shaderProgram = nullptr;
}

void RenderableGaussianSplat::loadPlyFile(const std::filesystem::path& path) {
    std::ifstream file(path, std::ios::binary);
    if (!file.is_open()) {
        LERROR(std::format("Failed to open Gaussian splat PLY file: {}", path));
        return;
    }

    std::string line;
    size_t vertexCount = 0;
    bool isBinaryLittleEndian = false;
    std::vector<std::string> propertyNames;

    while (std::getline(file, line)) {
        if (!line.empty() && line.back() == '\r') {
            line.pop_back();
        }
        if (line == "end_header") {
            break;
        }
        std::istringstream iss(line);
        std::string token;
        iss >> token;
        if (token == "format") {
            std::string formatType;
            iss >> formatType;
            isBinaryLittleEndian = (formatType == "binary_little_endian");
        }
        else if (token == "element") {
            std::string elemType;
            iss >> elemType;
            if (elemType == "vertex") {
                iss >> vertexCount;
            }
        }
        else if (token == "property") {
            std::string propType, propName;
            iss >> propType >> propName;
            propertyNames.push_back(propName);
        }
    }

    if (!isBinaryLittleEndian || vertexCount == 0 || propertyNames.empty()) {
        LERROR(std::format("Unsupported or empty PLY format in: {}", path));
        return;
    }

    auto findPropIndex = [&](const std::string& name) -> int {
        auto it = std::find(propertyNames.begin(), propertyNames.end(), name);
        return (it != propertyNames.end()) ?
            static_cast<int>(std::distance(propertyNames.begin(), it)) : -1;
    };

    const int idxX = findPropIndex("x");
    const int idxY = findPropIndex("y");
    const int idxZ = findPropIndex("z");
    const int idxDc0 = findPropIndex("f_dc_0");
    const int idxDc1 = findPropIndex("f_dc_1");
    const int idxDc2 = findPropIndex("f_dc_2");
    const int idxOpacity = findPropIndex("opacity");
    const int idxScale0 = findPropIndex("scale_0");
    const int idxScale1 = findPropIndex("scale_1");
    const int idxScale2 = findPropIndex("scale_2");
    const int idxRot0 = findPropIndex("rot_0");
    const int idxRot1 = findPropIndex("rot_1");
    const int idxRot2 = findPropIndex("rot_2");
    const int idxRot3 = findPropIndex("rot_3");

    if (idxX < 0 || idxY < 0 || idxZ < 0 || idxOpacity < 0 ||
        idxScale0 < 0 || idxRot0 < 0)
    {
        LERROR(std::format("PLY file missing required 3DGS attributes: {}", path));
        return;
    }

    const size_t floatsPerVertex = propertyNames.size();
    std::vector<float> rawVertexData(floatsPerVertex);

    _cpuSplats.clear();
    _cpuSplats.reserve(vertexCount);

    for (size_t i = 0; i < vertexCount; ++i) {
        file.read(
            reinterpret_cast<char*>(rawVertexData.data()),
            floatsPerVertex * sizeof(float)
        );
        if (!file) {
            break;
        }

        GaussianSplatGPU splat{};
        splat.position = glm::vec4(
            rawVertexData[idxX],
            rawVertexData[idxY],
            rawVertexData[idxZ],
            1.0f
        );

        if (idxDc0 >= 0 && idxDc1 >= 0 && idxDc2 >= 0) {
            splat.color_dc = glm::vec4(
                std::max(0.0f, 0.5f + SH_C0 * rawVertexData[idxDc0]),
                std::max(0.0f, 0.5f + SH_C0 * rawVertexData[idxDc1]),
                std::max(0.0f, 0.5f + SH_C0 * rawVertexData[idxDc2]),
                1.0f
            );
        }
        else {
            splat.color_dc = glm::vec4(1.0f);
        }

        const float rawOp = rawVertexData[idxOpacity];
        const float alpha = 1.0f / (1.0f + std::exp(-rawOp));

        const glm::vec3 scale(
            std::exp(rawVertexData[idxScale0]),
            std::exp(rawVertexData[idxScale1]),
            std::exp(rawVertexData[idxScale2])
        );

        glm::quat q(
            rawVertexData[idxRot0],
            rawVertexData[idxRot1],
            rawVertexData[idxRot2],
            rawVertexData[idxRot3]
        );
        q = glm::normalize(q);

        const glm::mat3 R = glm::mat3_cast(q);
        const glm::mat3 S = glm::mat3(
            scale.x, 0.0f, 0.0f,
            0.0f, scale.y, 0.0f,
            0.0f, 0.0f, scale.z
        );
        const glm::mat3 M = R * S;
        const glm::mat3 Sigma = M * glm::transpose(M);

        splat.cov3d_col0 = glm::vec4(
            Sigma[0][0], Sigma[0][1], Sigma[0][2], Sigma[1][1]
        );
        splat.cov3d_col1 = glm::vec4(
            Sigma[1][2], Sigma[2][2], alpha, 0.0f
        );

        _cpuSplats.push_back(splat);
    }

    _sortedIndices.resize(_cpuSplats.size());
    std::iota(_sortedIndices.begin(), _sortedIndices.end(), 0u);
    _dataDirty = true;

    LINFO(std::format("Loaded {} Gaussian splats from {}", _cpuSplats.size(), path));
}

void RenderableGaussianSplat::sortSplatsByDepth(const glm::dmat4& modelViewTransform) {
    const size_t count = _cpuSplats.size();
    if (count == 0) {
        return;
    }

    const double m02 = modelViewTransform[0][2];
    const double m12 = modelViewTransform[1][2];
    const double m22 = modelViewTransform[2][2];
    const double m32 = modelViewTransform[3][2];

    std::vector<float> depths(count);
    for (size_t i = 0; i < count; ++i) {
        const glm::vec4& p = _cpuSplats[i].position;
        depths[i] = static_cast<float>(
            m02 * p.x + m12 * p.y + m22 * p.z + m32
        );
    }

    std::iota(_sortedIndices.begin(), _sortedIndices.end(), 0u);

    std::sort(
        _sortedIndices.begin(),
        _sortedIndices.end(),
        [&depths](uint32_t a, uint32_t b) {
            return depths[a] < depths[b];
        }
    );
}

void RenderableGaussianSplat::update(const UpdateData&) {
    if (_dataDirty && !_cpuSplats.empty()) {
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _splatSsbo);
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            _cpuSplats.size() * sizeof(GaussianSplatGPU),
            _cpuSplats.data(),
            GL_STATIC_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _sortedIndexSsbo);
        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            _sortedIndices.size() * sizeof(uint32_t),
            _sortedIndices.data(),
            GL_DYNAMIC_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
        _dataDirty = false;
    }
}

void RenderableGaussianSplat::render(const RenderData& data, RendererTasks&) {
    if (_cpuSplats.empty() || !_shaderProgram) {
        return;
    }

    const glm::dmat4 modelTransform = calcModelTransform(data);
    const glm::dmat4 modelViewTransform =
        data.camera.combinedViewMatrix() * modelTransform;
    const glm::mat4 projectionTransform = data.camera.projectionMatrix();

    sortSplatsByDepth(modelViewTransform);

    glBindBuffer(GL_SHADER_STORAGE_BUFFER, _sortedIndexSsbo);
    glBufferSubData(
        GL_SHADER_STORAGE_BUFFER,
        0,
        _sortedIndices.size() * sizeof(uint32_t),
        _sortedIndices.data()
    );

    GLboolean depthMaskEnabled = GL_TRUE;
    glGetBooleanv(GL_DEPTH_WRITEMASK, &depthMaskEnabled);

    glEnable(GL_BLEND);
    glBlendFunc(GL_ONE, GL_ONE_MINUS_SRC_ALPHA);
    glDepthMask(GL_FALSE);

    _shaderProgram->activate();

    GLint viewport[4];
    glGetIntegerv(GL_VIEWPORT, viewport);
    const float width = static_cast<float>(viewport[2]);
    const float height = static_cast<float>(viewport[3]);
    const float focalX = projectionTransform[0][0] * width * 0.5f;
    const float focalY = projectionTransform[1][1] * height * 0.5f;

    _shaderProgram->setUniform(
        "modelViewTransform",
        glm::mat4(modelViewTransform)
    );
    _shaderProgram->setUniform("projectionTransform", projectionTransform);
    _shaderProgram->setUniform("viewportSize", glm::vec2(width, height));
    _shaderProgram->setUniform("focalX", focalX);
    _shaderProgram->setUniform("focalY", focalY);
    _shaderProgram->setUniform("scaleMultiplier", _scaleMultiplier.value());
    _shaderProgram->setUniform("globalOpacity", _opacity.value());

    glBindVertexArray(_vao);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 0, _splatSsbo);
    glBindBufferBase(GL_SHADER_STORAGE_BUFFER, 1, _sortedIndexSsbo);

    glDrawArraysInstanced(
        GL_TRIANGLE_STRIP,
        0,
        4,
        static_cast<GLsizei>(_cpuSplats.size())
    );

    glBindVertexArray(0);
    _shaderProgram->deactivate();

    glDepthMask(depthMaskEnabled);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

} // namespace openspace