#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLESPATIALSELECTIONVOLUME___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLESPATIALSELECTIONVOLUME___H__

#include <openspace/rendering/renderable.h>

#include <openspace/opengl/uniformcache.h>
#include <openspace/properties/list/doublelistproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <memory>

namespace openspace::opengl { class ProgramObject; }

namespace openspace::exoplanets {

class RenderableSpatialSelectionVolume : public Renderable {
public:
    explicit RenderableSpatialSelectionVolume(const Dictionary& dictionary);

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
    std::unique_ptr<opengl::ProgramObject> _program;
    UniformCache(modelViewTransform, projectionTransform, parsec, color) _uniformCache;
    GLuint _vao = 0;
    GLuint _vbo = 0;
    GLsizei _nTriangleVertices = 0;
    GLsizei _nLineVertices = 0;
    bool _meshIsDirty = true;
};

} // namespace openspace::exoplanets

#endif
