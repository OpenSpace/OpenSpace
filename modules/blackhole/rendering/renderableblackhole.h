#ifndef __OPENSPACE_MODULE_BLACKHOLE___RENDERABLECANVAS___H__
#define __OPENSPACE_MODULE_BLACKHOLE___RENDERABLECANVAS___H__

#include <openspace/rendering/renderable.h>
#include <modules/blackhole/rendering/viewport.h>
#include <ghoul/opengl/uniformcache.h>
#include <ghoul/opengl/textureunit.h>
#include <ghoul/opengl/bufferbinding.h>

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
        void SendSchwarzchildTableToShader();
        void update(const UpdateData& data) override;


        static documentation::Documentation Documentation();

    private:
        void bindSSBOData(ghoul::opengl::ProgramObject* program,
            const std::string& ssboName,
            std::unique_ptr<ghoul::opengl::BufferBinding<ghoul::opengl::bufferbinding::Buffer::ShaderStorage>>& ssboBinding,
            GLuint& ssboID);    void bindFramebuffer();
        bool bindTexture(GLint chacheRegistry, ghoul::opengl::TextureUnit& textureUnit, std::unique_ptr<ghoul::opengl::Texture>& texture);
        void drawQuad();
        void setupShaders();
        void setupQuad();
        void loadEnvironmentTexture();

        ghoul::opengl::ProgramObject* _program = nullptr;

        size_t _rayCount = 1000;
        size_t _stepsCount = 50000;
        float _stepLength = 0.001f;

        float _rsBlackHole = 1.0f;
        float _rEnvmap = 60.0f;
        float _rCamera = 20.0f;

        ViewPort _viewport;

        std::vector<float> _schwarzschildWarpTable;
        std::unique_ptr<ghoul::opengl::Texture> warpTableTex;
        std::unique_ptr<ghoul::opengl::BufferBinding<
            ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboDataBinding;

        GLuint _framebuffer = 0;
        GLuint _quadVao = 0;
        GLuint _quadVbo = 0;
        GLuint _ssboData = 0;

        UniformCache(environmentTexture, viewGrid, worldRotationMatrix) _uniformCache;

        std::unique_ptr<ghoul::opengl::Texture> _warpTableTex;
        std::unique_ptr<ghoul::opengl::Texture> _environmentTexture;
    };

} // openspace namespace
#endif //__OPENSPACE_MODULE_BLACKHOLE___RENDERABLECANVAS___H__
