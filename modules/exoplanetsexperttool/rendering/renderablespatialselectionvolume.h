#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLESPATIALSELECTIONVOLUME___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLESPATIALSELECTIONVOLUME___H__

#include <openspace/rendering/renderable.h>

#include <openspace/properties/list/doublelistproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <ghoul/opengl/ghoul_gl.h>
#include <ghoul/opengl/uniformcache.h>
#include <memory>

namespace ghoul::opengl { class ProgramObject; }

namespace openspace::exoplanets {

class RenderableSpatialSelectionVolume : public Renderable {
public:
    explicit RenderableSpatialSelectionVolume(const ghoul::Dictionary& dictionary);

    void initializeGL() override;
    void deinitializeGL() override;
    void update(const UpdateData& data) override;
    void render(const RenderData& data, RendererTasks& tasks) override;

    static openspace::Documentation Documentation();

private:
    void updateMesh();

    DoubleListProperty _shapeParameters;
    Vec3Property _color;
    FloatProperty _outlineOpacity;
    std::unique_ptr<ghoul::opengl::ProgramObject> _program;
    UniformCache(modelViewTransform, projectionTransform, parsec, color) _uniformCache;
    GLuint _vao = 0;
    GLuint _vbo = 0;
    GLsizei _nTriangleVertices = 0;
    GLsizei _nLineVertices = 0;
    bool _meshIsDirty = true;
};

} // namespace openspace::exoplanets

#endif