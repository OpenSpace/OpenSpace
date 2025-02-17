#ifndef __OPENSPACE_MODULE_BLACKHOLE___RENDERABLECANVAS___H__
#define __OPENSPACE_MODULE_BLACKHOLE___RENDERABLECANVAS___H__

#include <openspace/rendering/renderable.h>
#include <ghoul/opengl/uniformcache.h>

namespace openspace {

class RenderableBlackHole : public Renderable {
public: 
    explicit RenderableBlackHole(const ghoul::Dictionary& dictionary);
    ~RenderableBlackHole() override = default;

    void initialize() override;
    void initializeGL() override;
    void deinitializeGL() override;

    bool isReady() const override;

    void render(const RenderData& data, RendererTasks& rendererTask) override;
    void update(const UpdateData& data) override;

    static documentation::Documentation Documentation();

private:
  ghoul::opengl::ProgramObject* _program = nullptr;

  GLuint _framebuffer = 0;
  GLuint _quadVao = 0;
  GLuint _quadVbo = 0;

};

} // openspace namespace
#endif //__OPENSPACE_MODULE_BLACKHOLE___RENDERABLECANVAS___H__
