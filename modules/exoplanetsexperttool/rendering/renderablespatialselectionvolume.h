#ifndef __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLESPATIALSELECTIONVOLUME___H__
#define __OPENSPACE_MODULE_EXOPLANETSEXPERTTOOL___RENDERABLESPATIALSELECTIONVOLUME___H__

#include <openspace/rendering/renderable.h>

#include <modules/exoplanetsexperttool/rendering/selectionvolumemesh.h>
#include <openspace/opengl/uniformcache.h>
#include <openspace/properties/list/doublelistproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/vector/vec3property.h>
#include <openspace/util/syncable.h>
#include <memory>
#include <mutex>

namespace openspace::opengl { class ProgramObject; }

namespace openspace::exoplanets {

class RenderableSpatialSelectionVolume : public Renderable, public Syncable {
public:
    explicit RenderableSpatialSelectionVolume(const Dictionary& dictionary);
    ~RenderableSpatialSelectionVolume() override;

    void initializeGL() override;
    void deinitializeGL() override;
    void update(const UpdateData& data) override;
    void render(const RenderData& data, RendererTasks& tasks) override;

    static openspace::Documentation Documentation();

    /**
     * Sets the mesh that is shown while the shape parameters are [3, id].
     */
    void setCustomMesh(SelectionVolumeMesh mesh);

private:
    void encode(SyncBuffer* syncBuffer) override;
    void decode(SyncBuffer* syncBuffer) override;
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
    std::mutex _meshMutex;
    SelectionVolumeMesh _customMesh;
    bool _customMeshNeedsSync = false;
    bool _customMeshIsDirty = false;
};

} // namespace openspace::exoplanets

#endif
