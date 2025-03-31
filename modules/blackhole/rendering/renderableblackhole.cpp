#include <modules/blackhole/rendering/renderableblackhole.h>
#include <modules/blackhole/blackholemodule.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/rendering/framebufferrenderer.h>
#include <openspace/util/distanceconstants.h>

#include <openspace/navigation/navigationhandler.h>
#include <openspace/navigation/orbitalnavigator.h>
#include <openspace/rendering/renderengine.h>

#include <ghoul/opengl/framebufferobject.h>
#include <ghoul/opengl/openglstatecache.h>
#include <ghoul/io/texture/texturereader.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/filesystem/filesystem.h>
#include <openspace/util/updatestructures.h>

#include <filesystem>
#include <vector>

#include <modules/blackhole/cuda/blackhole_cuda.h>

#ifndef G
#define G 6.67430e-11f
#endif

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

    constexpr openspace::properties::Property::PropertyInfo SolarMassInfo = {
        "SolarMass",
        "Solar Mass",
        "The mass of the blackhole in solar mass units",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTextureInfo = {
    "ColorMap",
    "Color Texture",
    "The path to the texture that is used to convert from the magnitude of the star "
    "to its color. The texture is used as a one dimensional lookup function.",
    openspace::properties::Property::Visibility::AdvancedUser
    };

    struct [[codegen::Dictionary(RenderableModel)]] Parameters {
        std::optional<float> SolarMass;
        std::string colorMap;
    };
    
#include "renderableblackhole_codegen.cpp"
}

namespace openspace {

    RenderableBlackHole::RenderableBlackHole(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary), _solarMass(SolarMassInfo, 4.297e6f), _colorBVMapTexturePath(ColorTextureInfo) {
        setRenderBin(Renderable::RenderBin::Background);

        const Parameters p = codegen::bake<Parameters>(dictionary);

        _solarMass = p.SolarMass.value_or(_solarMass);

        _rs = 2.0f * G * _solarMass;

        _colorBVMapTexturePath = absPath(p.colorMap).string();
    }

    RenderableBlackHole::~RenderableBlackHole() {}

    void RenderableBlackHole::initialize() {
        _schwarzschildWarpTable = std::vector<float>(_rayCount * 2, std::numeric_limits<double>::quiet_NaN());
    }

    void RenderableBlackHole::initializeGL() {
        const glm::vec2 screenSize = glm::vec2(global::renderEngine->renderingResolution());
        ZoneScoped;
        setupQuad();
        setupShaders();
        loadEnvironmentTexture();
        _viewport.updateViewGrid(screenSize);
    }

    void RenderableBlackHole::deinitializeGL() {
        _warpTableTex = nullptr;
        _environmentTexture = nullptr;
        _viewport.viewGrid = nullptr;
        glDeleteBuffers(1, &_quadVbo);
        glDeleteVertexArrays(1, &_quadVao);

        std::string program = std::string(ProgramName);
        BlackHoleModule::ProgramObjectManager.release(
            program,
            [](ghoul::opengl::ProgramObject* p) {
                global::renderEngine->removeRenderProgram(p);
            }
        );
        _program = nullptr;
    }

    bool RenderableBlackHole::isReady() const {
        return _program != nullptr;
    }

    void RenderableBlackHole::update(const UpdateData& data) {
        if (data.modelTransform.translation != _chachedTranslation) {
            _chachedTranslation = data.modelTransform.translation;
            _starKDTree.build("${BASE}/sync/http/stars_du/6/stars.speck", _chachedTranslation, { {0, 25 }, {25, 50}, { 50, 100 } });
        }

        _viewport.updateViewGrid(global::renderEngine->renderingResolution());

        glm::dvec3 cameraPosition = global::navigationHandler->camera()->positionVec3();
        float distanceToAnchor = static_cast<float>(glm::distance(cameraPosition, _chachedTranslation) / distanceconstants::LightYear);
        if (abs(_rCamera - distanceToAnchor) > _rs * 0.01) {
            _rCamera = distanceToAnchor;
            _rEnvmap = 2 * _rCamera;

            schwarzchild(_rs, { _rCamera * 1.5f, _rCamera * 2.0f, _rCamera * 3.0f}, _rayCount, _stepsCount, _rCamera, _stepLength, _schwarzschildWarpTable);
        }
        bindSSBOData(_program, "ssbo_warp_table", _ssboSchwarzschildDataBinding, _ssboSchwarzschildWarpTable);
        bindSSBOData(_program, "ssbo_star_map_start_indices", _ssboStarIndicesDataBinding, _ssboStarKDTreeIndices);
        bindSSBOData(_program, "ssbo_star_map", _ssboStarDataBinding, _ssboStarKDTree);
    }

    void RenderableBlackHole::render(const RenderData& renderData, RendererTasks&) {
        _program->activate();
        bindFramebuffer();

        glDisable(GL_DEPTH_TEST);

        ghoul::opengl::TextureUnit enviromentUnit;
        if (!bindTexture(_uniformCache.environmentTexture, enviromentUnit, _environmentTexture)) {
            LWARNING("UniformCache is missing 'environmentTexture'");
        }

        ghoul::opengl::TextureUnit viewGridUnit;
        if (!bindTexture(_uniformCache.viewGrid, viewGridUnit, _viewport.viewGrid)) {
            LWARNING("UniformCache is missing 'viewGrid'");
        }

        ghoul::opengl::TextureUnit colorBVMapUnit;
        if (!bindTexture(_uniformCache.colorBVMap, colorBVMapUnit, _colorBVMapTexture)) {
            LWARNING("UniformCache is missing 'colorBVMap'");
        }

        SendSchwarzchildTableToShader();
        SendStarKDTreeToShader();

        interaction::OrbitalNavigator::CameraRotationDecomposition camRot = global::navigationHandler->orbitalNavigator().decomposeCameraRotationSurface(
            CameraPose{renderData.camera.positionVec3(), renderData.camera.rotationQuaternion()},
            *parent()
        );

        // Calculate the camera planes rotation to make sure fisheye works correcly (dcm in sgct projection.cpp)
        glm::mat4 invViewPlaneTranslationMatrix = glm::translate(
            glm::mat4(1.f), glm::vec3(static_cast<float>(renderData.camera.eyePositionVec3().x))
        );
        glm::mat4 viewMatrix = renderData.camera.viewMatrix();
        glm::mat4 const CameraPlaneRotation = glm::inverse(viewMatrix * invViewPlaneTranslationMatrix);
        
        _program->setUniform(
            _uniformCache.cameraRotationMatrix,
             glm::mat4(glm::mat4_cast(camRot.localRotation)) * CameraPlaneRotation
        );

        _program->setUniform(
            _uniformCache.worldRotationMatrix,
            glm::mat4(glm::mat4_cast(camRot.globalRotation))
            );
     
        drawQuad();

        glEnable(GL_DEPTH_TEST);

        _program->deactivate();
        glBindTexture(GL_TEXTURE_2D, 0);
    }

    void RenderableBlackHole::SendSchwarzchildTableToShader()
    {
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboSchwarzschildWarpTable);

        const size_t indexBufferSize = _schwarzschildWarpTable.size() * sizeof(float);

        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            indexBufferSize,
            _schwarzschildWarpTable.data(),
            GL_STREAM_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
    }

    void RenderableBlackHole::SendStarKDTreeToShader()
    {
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboStarKDTree);

        size_t indexBufferSize = _starKDTree.mapsSize() * sizeof(float);

        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            indexBufferSize,
            _starKDTree.mapsData(),
            GL_STREAM_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboStarKDTreeIndices);

        indexBufferSize = _starKDTree.indicesSize() * sizeof(int);

        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            indexBufferSize,
            _starKDTree.indicesData(),
            GL_STREAM_DRAW
        );

        glBindBuffer(GL_SHADER_STORAGE_BUFFER, 0);
    }

    void RenderableBlackHole::setupShaders() {
        const std::string vertexShaderPath = "${MODULE_BLACKHOLE}/shaders/blackhole_vs.glsl";
        const std::string fragmentShaderPath = "${MODULE_BLACKHOLE}/shaders/blackhole_fs.glsl";

        // Initialize shaders
        std::string program = std::string(ProgramName);
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
        const std::string texturePath = "${BASE}/sync/http/milkyway_textures/2/DarkUniverse_mellinger_8k.jpg";
        //const std::string texturePath = "${MODULE_BLACKHOLE}/rendering/skybox.jpg";

        _environmentTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(texturePath), 2);

        if (_environmentTexture) {
            _environmentTexture->uploadTexture();
        }
        else {
            LWARNING(std::format("Failed to load environment texture from path '{}'", absPath(texturePath).string()));
        }

        _colorBVMapTexture = ghoul::io::TextureReader::ref().loadTexture(absPath(_colorBVMapTexturePath), 1);


        if (_colorBVMapTexture) {
            _colorBVMapTexture->uploadTexture();
        }
        else {
            LWARNING(std::format("Failed to load environment texture from path '{}'", absPath(_colorBVMapTexturePath).string()));
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
