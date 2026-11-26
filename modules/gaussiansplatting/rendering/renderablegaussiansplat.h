#ifndef __OPENSPACE_MODULE_GAUSSIANSPLATTING___RENDERABLEGAUSSIANSPLAT___H__
#define __OPENSPACE_MODULE_GAUSSIANSPLATTING___RENDERABLEGAUSSIANSPLAT___H__

#include <openspace/rendering/renderable.h>

#include <glbinding/gl/gl.h>
#include <openspace/opengl/programobject.h>
#include <openspace/properties/misc/stringproperty.h>
#include <openspace/properties/scalar/boolproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/scalar/intproperty.h>
#include <glm/glm.hpp>
#include <filesystem>
#include <memory>
#include <vector>

namespace openspace {

struct RenderData;
struct UpdateData;
struct Documentation;

class RenderableGaussianSplat : public Renderable {
public:
    explicit RenderableGaussianSplat(const Dictionary& dictionary);
    ~RenderableGaussianSplat() override = default;

    void initializeGL() override;
    void deinitializeGL() override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static openspace::Documentation Documentation();

private:
    struct alignas(16) GaussianSplatGPU {
        glm::vec4 position;     // xyz = local position, w = unused/padding
        glm::vec4 cov3d_col0;   // Upper-triangular 3D covariance: (xx, xy, xz, yy)
        glm::vec4 cov3d_col1;   // Remaining 3D covariance + opacity: (yz, zz, opacity, pad)
        glm::vec4 color_dc;     // Base RGB (SH band 0) + padding
    };

    void loadPlyFile(const std::filesystem::path& path);
    void sortSplatsByDepth(const glm::dmat4& modelViewTransform);

    StringProperty _filePath;
    FloatProperty _opacity;
    FloatProperty _scaleMultiplier;
    IntProperty _shDegree;
    BoolProperty _enableFrustumCulling;

    std::unique_ptr<opengl::ProgramObject> _shaderProgram;

    gl::GLuint _vao = 0;
    gl::GLuint _splatSsbo = 0;
    gl::GLuint _sortedIndexSsbo = 0;

    std::vector<GaussianSplatGPU> _cpuSplats;
    std::vector<uint32_t> _sortedIndices;
    bool _dataDirty = false;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_GAUSSIANSPLATTING___RENDERABLEGAUSSIANSPLATT___H__