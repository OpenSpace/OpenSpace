#include <modules/blackhole/rendering/renderableblackhole.h>

#include <modules/blackhole/blackholemodule.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/rendering/renderengine.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/openglstatecache.h>

#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <filesystem>

namespace {
    constexpr std::string_view _loggerCat = "BlackHoleModule";
    constexpr std::string_view ProgramName = "BlackHoleProgram";
}
namespace openspace {
    RenderableBlackHole::RenderableBlackHole(const ghoul::Dictionary& dictionary) : Renderable(dictionary, { .automaticallyUpdateRenderBin = false }) {}

    void RenderableBlackHole::initialize() {
    }

    void RenderableBlackHole::initializeGL() {
        ZoneScoped;
        std::string _vertexShaderPath = "${MODULE_BLACKHOLE}/rendering/gradiant_vs.glsl";
        std::string _fragmentShaderPath = "${MODULE_BLACKHOLE}/rendering/gradiant_fs.glsl";

        LDEBUG(absPath(_vertexShaderPath).string());

        // Initialize shaders
        std::string program = std::string(ProgramName);
        program += "|vs=" + _vertexShaderPath;
        program += "|fs=" + _fragmentShaderPath;
        _program = BlackHoleModule::ProgramObjectManager.request(
            program,
            [this, _fragmentShaderPath, _vertexShaderPath, program]() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                const std::filesystem::path vs = absPath(_vertexShaderPath);
                const std::filesystem::path fs = absPath(_fragmentShaderPath);

                return global::renderEngine->buildRenderProgram(program, vs, fs);
            }
        );

        // Screen quad VAO
        constexpr std::array<GLfloat, 24> QuadVtx = {
            // x     y     s     t
            -1.f, -1.f,  0.f,  0.f,
             1.f,  1.f,  1.f,  1.f,
            -1.f,  1.f,  0.f,  1.f,
            -1.f, -1.f,  0.f,  0.f,
             1.f, -1.f,  1.f,  0.f,
             1.f,  1.f,  1.f,  1.f
        };

        glGenVertexArrays(1, &_quadVao);
        glBindVertexArray(_quadVao);

        glGenBuffers(1, &_quadVbo);
        glBindBuffer(GL_ARRAY_BUFFER, _quadVbo);

        // Send Quad to Buffer
        glBufferData(GL_ARRAY_BUFFER, sizeof(QuadVtx), QuadVtx.data(), GL_STATIC_DRAW);

        //Bind aPos
        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), nullptr);

        //Bind aTexCoord
        glEnableVertexAttribArray(1);
        glVertexAttribPointer(
            1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), reinterpret_cast<void*>(2 * sizeof(GLfloat))
        );
    }

    void RenderableBlackHole::deinitializeGL() {
        glDeleteFramebuffers(1, &_framebuffer);

        glDeleteBuffers(1, &_quadVbo);
        glDeleteVertexArrays(1, &_quadVao);
    }

    bool RenderableBlackHole::isReady() const {
        return _program;
    }

    void RenderableBlackHole::render(const RenderData&, RendererTasks&) {
        //Bind Buffer
        const GLint defaultFBO = ghoul::opengl::FramebufferObject::getActiveObject();
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);

        // Draw
        _program->activate();
        glBindVertexArray(_quadVao);
        glDrawArrays(GL_TRIANGLES, 0, 6);
        _program->deactivate();
    }

    void RenderableBlackHole::update(const UpdateData& data)
    {
    }

    documentation::Documentation RenderableBlackHole::Documentation()
    {
        return documentation::Documentation();
    }

} //openspace namespace
