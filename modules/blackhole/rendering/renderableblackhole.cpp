#include <modules/blackhole/rendering/renderableblackhole.h>
#include <modules/blackhole/blackholemodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/navigation/navigationhandler.h>

#include <openspace/rendering/renderengine.h>
#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <filesystem>
#include <vector>

#include <modules/blackhole/cuda/blackhole_cuda.h>

namespace {
    constexpr std::string_view _loggerCat = "BlackHoleModule";
    constexpr std::string_view ProgramName = "BlackHoleProgram";

    constexpr std::array<GLfloat, 24> QuadVtx = {
        -1.f, -1.f,  0.f,  0.f,
         1.f,  1.f,  1.f,  1.f,
        -1.f,  1.f,  0.f,  1.f,
        -1.f, -1.f,  0.f,  0.f,
         1.f, -1.f,  1.f,  0.f,
         1.f,  1.f,  1.f,  1.f
    };
}

namespace openspace {
    RenderableBlackHole::RenderableBlackHole(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary, { .automaticallyUpdateRenderBin = false }) {

    }

    void RenderableBlackHole::initialize() {
        _viewport = ViewPort(global::navigationHandler->camera());
        global::navigationHandler->camera()->setRotation(glm::dquat(0,0,0,0));
        _schwarzschildWarpTable = std::vector<float>(_rayCount * 2, 0.f);
        schwarzchild(_rsBlackHole, _rEnvMap, _rayCount, _stepsCount, 1.0f / _rCamera, _stepLength, _schwarzschildWarpTable.data());
    }

    void RenderableBlackHole::initializeGL() {
        const glm::vec2 screenSize = glm::vec2(global::renderEngine->renderingResolution());
        ZoneScoped;
        setupQuad();
        setupShaders();
        loadEnvironmentTexture();
        _viewport.uploadViewGrid(screenSize);
    }

    void RenderableBlackHole::deinitializeGL() {
        glDeleteFramebuffers(1, &_framebuffer);
        glDeleteBuffers(1, &_quadVbo);
        glDeleteVertexArrays(1, &_quadVao);
        delete(_program);
    }

    bool RenderableBlackHole::isReady() const {
        return _program != nullptr;
    }

    void RenderableBlackHole::update(const UpdateData&) {
        bindSSBOData(_program, "ssbo_warp_table", _ssboDataBinding, _ssboData);
    }

    void RenderableBlackHole::render(const RenderData&, RendererTasks&) {
        _program->activate();
        bindFramebuffer();

        ghoul::opengl::TextureUnit enviromentUnit;
        if (!bindTexture(_uniformCache.environmentTexture, enviromentUnit, _environmentTexture)) {
            LWARNING("UniformCache is missing 'environmentTexture'");
        }

        ghoul::opengl::TextureUnit viewGridUnit;
        if (!bindTexture(_uniformCache.viewGrid, viewGridUnit, _viewport.viewGrid)) {
            LWARNING("UniformCache is missing 'viewGrid'");
        }

        SendSchwarzchildTableToShader();

        _program->setUniform(
            _uniformCache.cameraRotationMatrix,
            glm::mat4(global::navigationHandler->camera()->viewRotationMatrix())
        );
     
        drawQuad();

        _program->deactivate();
        glBindTexture(GL_TEXTURE_2D, 0);
    }

    void RenderableBlackHole::SendSchwarzchildTableToShader()
    {
        // Update SSBO Index array with accumulated stars in all chunks.
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboData);

        const size_t indexBufferSize = _schwarzschildWarpTable.size() * sizeof(float);

        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            indexBufferSize,
            _schwarzschildWarpTable.data(),
            GL_STREAM_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
    }

    void RenderableBlackHole::setupShaders() {
        const std::string vertexShaderPath = "${MODULE_BLACKHOLE}/shaders/blackhole_vs.glsl";
        const std::string fragmentShaderPath = "${MODULE_BLACKHOLE}/shaders/blackhole_fs.glsl";

        // Initialize shaders
        std::string program = std::string(ProgramName);
        program += "|vs=" + vertexShaderPath;
        program += "|fs=" + fragmentShaderPath;
        _program = BlackHoleModule::ProgramObjectManager.request(
            program,
            [fragmentShaderPath, vertexShaderPath, program]() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                const std::filesystem::path vs = absPath(vertexShaderPath);
                const std::filesystem::path fs = absPath(fragmentShaderPath);

                return global::renderEngine->buildRenderProgram(program, vs, fs);
            }
        );

        ghoul::opengl::updateUniformLocations(*_program, _uniformCache);
    }

    void RenderableBlackHole::setupQuad() {
        glGenVertexArrays(1, &_quadVao);
        glBindVertexArray(_quadVao);

        glGenBuffers(1, &_quadVbo);
        glBindBuffer(GL_ARRAY_BUFFER, _quadVbo);
        glBufferData(GL_ARRAY_BUFFER, sizeof(QuadVtx), QuadVtx.data(), GL_STATIC_DRAW);

        glEnableVertexAttribArray(0);
        glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), nullptr);

        glEnableVertexAttribArray(1);
        glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(GLfloat), reinterpret_cast<void*>(2 * sizeof(GLfloat)));
    }

    void RenderableBlackHole::loadEnvironmentTexture() {
        //const std::string texturePath = "${MODULE_BLACKHOLE}/rendering/uv.png";
        //const std::string texturePath = "C:/Users/wilbj602/Documents/GitHub/OpenSpace/sync/http/milkyway_textures/2/DarkUniverse_mellinger_8k.jpg";
        const std::string texturePath = "C:/Users/wilbj602/Downloads/img.jpg";

        _environmentTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(texturePath), 2);

        if (_environmentTexture) {
            _environmentTexture->uploadTexture();
        }
        else {
            LWARNING(std::format("Failed to load environment texture from path '{}'", absPath(texturePath).string()));
        }
    }

    void RenderableBlackHole::bindFramebuffer() {
        const GLint defaultFBO = ghoul::opengl::FramebufferObject::getActiveObject();
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFBO);
    }

    bool RenderableBlackHole::bindTexture(GLint chacheRegistry, ghoul::opengl::TextureUnit& textureUnit, std::unique_ptr<ghoul::opengl::Texture>& texture) {
        if (!texture) return false;

        textureUnit.activate();
        texture->bind();
        _program->setUniform(chacheRegistry, textureUnit);
        return true;
    }

    void RenderableBlackHole::drawQuad() {
        glBindVertexArray(_quadVao);
        glDrawArrays(GL_TRIANGLES, 0, 6);
    }

    void RenderableBlackHole::bindSSBOData(
        ghoul::opengl::ProgramObject* program,
        const std::string& ssboName,
        std::unique_ptr<ghoul::opengl::BufferBinding<ghoul::opengl::bufferbinding::Buffer::ShaderStorage>>& ssboBinding,
        GLuint& ssboID
    )
    {
        if (ssboID == 0) {
            glGenBuffers(1, &ssboID);
            LDEBUG(std::format(
                "Generating Data Shader Storage Buffer Object id '{}'", ssboID
            ));
        }

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, ssboID);

        ssboBinding = std::make_unique<ghoul::opengl::BufferBinding<
            ghoul::opengl::bufferbinding::Buffer::ShaderStorage>
        >();
        glBindBufferBase(
            GL_SHADER_STORAGE_BUFFER,
            ssboBinding->bindingNumber(),
            ssboID
        );
        program->setSsboBinding(ssboName, ssboBinding->bindingNumber());

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
    }

    documentation::Documentation RenderableBlackHole::Documentation() {
        return documentation::Documentation();
    }
} // namespace openspace
