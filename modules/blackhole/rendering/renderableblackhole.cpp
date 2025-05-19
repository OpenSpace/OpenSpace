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
#include <modules/base/basemodule.h>

#include <filesystem>
#include <vector>
#include <chrono>

#include <modules/blackhole/cuda/kerr.h>
#include <modules/blackhole/cuda/schwarzschild.h>

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

    constexpr openspace::properties::Property::PropertyInfo KerrRotationInfo = {
        "KerrRotation",
        "Kerr Rotation (a)",
        "temp description",
        openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo StarMapRangesInfo = {
        "StarMapRanges",
        "Star Map Ranges",
        "temp description",
         openspace::properties::Property::Visibility::User
    };

    constexpr openspace::properties::Property::PropertyInfo ColorTextureInfo = {
    "ColorMap",
    "Color Texture",
    "The path to the texture that is used to convert from the magnitude of the star "
    "to its color. The texture is used as a one dimensional lookup function.",
    openspace::properties::Property::Visibility::AdvancedUser
    };

    constexpr openspace::properties::Property::PropertyInfo BlackHoleTypeInfo = {
        "BlackHoleType",
        "Black Hole Type",
        "Selects what type of black hole to render.",
        openspace::properties::Property::Visibility::User
    };

    struct [[codegen::Dictionary(RenderableModel)]] Parameters {
        std::optional<float> solarMass;
        std::optional<float> kerrRotation;
        std::string colorMap;
        std::optional<std::vector<double>> starMapRanges;
        std::optional<int> blackHoleType;
    };

    auto lastTime = std::chrono::high_resolution_clock::now();

    bool _shaderIsDirty = false;

#include "renderableblackhole_codegen.cpp"

}

namespace openspace {

    RenderableBlackHole::RenderableBlackHole(const ghoul::Dictionary& dictionary)
        : Renderable(dictionary),
        _solarMass(SolarMassInfo, 4.297e6f),
        _kerrRotation(KerrRotationInfo, 0.5f, 0.0f, 0.999999f),
        _colorBVMapTexturePath(ColorTextureInfo),
        _starMapRanges(StarMapRangesInfo, { 0.00035, 12.5002625, 12.5002625, 25.000175000000002, 25.000175000000002, 37.5000875, 37.5000875, 50.0 }),
        _blackholeType(BlackHoleTypeInfo)
    {
        const Parameters p = codegen::bake<Parameters>(dictionary);


        auto updateStarMapRanges = [this]() {
            std::vector<double> mapRangeValus = _starMapRanges.value();
            std::sort(mapRangeValus.begin(), mapRangeValus.end());
            _starMapRanges.setValue(mapRangeValus);
            _layerLayout.ranges.resize(_starMapRanges.value().size() / 2);
            for (int i = 0; i < _layerLayout.ranges.size(); i++) {
                _layerLayout.ranges[i] = { mapRangeValus[i * 2], mapRangeValus[i * 2 + 1] };
            }
            _layerLayout.isDirty = true;
            };
        _starMapRanges.onChange(updateStarMapRanges);
        _starMapRanges.setValue(p.starMapRanges.value_or(_starMapRanges.value()));
        updateStarMapRanges();


        constexpr float G = 6.67430e-11;
        constexpr float c = 2.99792458e8;
        constexpr float M = 1.9885e30;
        auto calcRs = [this]() {
            _rs = 2.0f * G * _solarMass * M / (c * c);
            setInteractionSphere(_rs * 1.2);
            setBoundingSphere(_rs * 20);
            _layerLayout.calcPositions(distanceconstants::Parsec / _rs);
            };
        _solarMass.onChange(calcRs);
        _solarMass.setValue(p.solarMass.value_or(_solarMass));
        calcRs();

       
        _kerrRotation.setValue(p.kerrRotation.value_or(_kerrRotation));


        _blackholeType.addOptions({
            {static_cast<int>(BlackHoleType::schwarzschild), "Schwarzschild"},
            {static_cast<int>(BlackHoleType::kerr), "Kerr"}
            });

        auto changeBlackHoleType = [this]() {
                _shaderIsDirty = true;
                _starKDTree.isDirty = true;
            };

        _blackholeType.onChange(changeBlackHoleType);
        _blackholeType.setValue(p.blackHoleType.value_or(static_cast<int>(BlackHoleType::kerr)));


        addProperty(_blackholeType);
        addProperty(_solarMass);
        addProperty(_kerrRotation);
        addProperty(_starMapRanges);


        _colorBVMapTexturePath = absPath(p.colorMap).string();
    }

    RenderableBlackHole::~RenderableBlackHole() {}

    void RenderableBlackHole::initialize() {
        _blackHoleWarpTable.reserve(_rayCountHighRes * _rayCountHighRes * 4);
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
        for (const auto& [_, config] : RenderableBlackHole::BlackHoleShaderConfigs) {
            BaseModule::ProgramObjectManager.release(config.programName);
        }
        _program = nullptr;
        glDeleteBuffers(1, &_quadVbo);
        glDeleteVertexArrays(1, &_quadVao);
    }

    bool RenderableBlackHole::isReady() const {
        return _program != nullptr;
    }
    bool highres = false;
    void RenderableBlackHole::update(const UpdateData& data) {
        if (data.modelTransform.translation != _chachedTranslation || _layerLayout.isDirty) {
            _chachedTranslation = data.modelTransform.translation;
            _layerLayout.calcPositions(distanceconstants::Parsec / _rs);
            _starKDTree.build("${BASE}/sync/http/stars_du/6/stars.speck", _chachedTranslation, _layerLayout.ranges);
            _layerLayout.isDirty = false;
        }

        if (_shaderIsDirty) {
            setupShaders();
        }

        _viewport.updateViewGrid(global::renderEngine->renderingResolution());

        if (_blackholeType.value() == static_cast<int>(BlackHoleType::kerr)) {
            // world-space camera
            glm::dvec3 camW = global::navigationHandler->camera()->positionVec3();

            // 1) Translate into model-centered space
            glm::dvec3 v = camW - data.modelTransform.translation;

            // 2) Remove the rotation: for an orthonormal matrix, inverse == transpose
            glm::dvec3 v_rot = glm::transpose(data.modelTransform.rotation) * v;

            // 3) Remove scaling (component-wise)
            glm::dvec3 cameraPosition = v_rot / data.modelTransform.scale;

            if (glm::distance(cameraPosition, _chacedCameraPos) > 0.01f * _rs || _starKDTree.isDirty) {
                traceKerr(cameraPosition, _rs, _kerrRotation, _layerLayout.positions, _rayCount, _stepsCount, _blackHoleWarpTable);
                _chacedCameraPos = cameraPosition;
                highres = false;
                lastTime = std::chrono::high_resolution_clock::now();
            }
            else if (!highres) {
                auto currentTime = std::chrono::high_resolution_clock::now();
                std::chrono::duration<float> deltaTime = currentTime - lastTime;
                float deltaTimeSeconds = deltaTime.count();
                if (deltaTimeSeconds > 1.0f) {
                    traceKerr(cameraPosition, _rs, _kerrRotation, _layerLayout.positions, _rayCountHighRes, _stepsCount, _blackHoleWarpTable);
                    highres = true;
                }
            }
        }

        if (_blackholeType.value() == static_cast<int>(BlackHoleType::schwarzschild)) {
            glm::dvec3 cameraPosition = global::navigationHandler->camera()->positionVec3();
            float distanceToAnchor = static_cast<float>(glm::distance(cameraPosition, _chachedTranslation));
            if (abs(_rCamera * _rs - distanceToAnchor) > _rs * 0.1 || _starKDTree.isDirty) {
                _rCamera = distanceToAnchor / _rs;
                schwarzschild(_layerLayout.positions, _rayCount, _stepsCount, _rCamera, _stepLength, _blackHoleWarpTable);
            }
        }
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

        if (_blackholeType.value() == static_cast<int>(BlackHoleType::kerr)) {
            ghoul::opengl::TextureUnit accretionDiskUnit;
            if (!bindTexture(_uniformCache.accretionDisk, accretionDiskUnit, _accretionDiskTexture)) {
                LWARNING("UniformCache is missing 'accretionDisk'");
            }
        }

        SendSchwarzschildTableToShader();

        if (_starKDTree.isDirty) {
            SendStarKDTreeToShader();
            _starKDTree.isDirty = false;
        }

        interaction::OrbitalNavigator::CameraRotationDecomposition camRot = global::navigationHandler->orbitalNavigator().decomposeCameraRotationSurface(
            CameraPose{ renderData.camera.positionVec3(), renderData.camera.rotationQuaternion() },
            *parent()
        );

        // Calculate the camera planes rotation to make sure fisheye works correcly (dcm in sgct projection.cpp)
        glm::mat4 invViewPlaneTranslationMatrix = glm::translate(
            glm::mat4(1.f), glm::vec3(static_cast<float>(renderData.camera.eyePositionVec3().x))
        );
        glm::mat4 viewMatrix = renderData.camera.viewMatrix();
        glm::mat4 const CameraPlaneRotation = glm::inverse(viewMatrix * invViewPlaneTranslationMatrix);

        if (_blackholeType.value() == static_cast<int>(BlackHoleType::kerr)) {
            _program->setUniform(
                _uniformCache.cameraRotationMatrix,
                glm::mat4(glm::mat4_cast(camRot.globalRotation * camRot.localRotation)) * CameraPlaneRotation
            );
        }

        if (_blackholeType.value() == static_cast<int>(BlackHoleType::schwarzschild)) {
            _program->setUniform(
                _uniformCache.cameraRotationMatrix,
                glm::mat4(glm::mat4_cast(camRot.localRotation)) * CameraPlaneRotation
            );

            _program->setUniform(
                _uniformCache.worldRotationMatrix,
                glm::mat4(glm::mat4_cast(camRot.globalRotation))
            );

            if (_uniformCache.r_0 != -1) {
                _program->setUniform(
                    _uniformCache.r_0,
                    _rCamera
                );
            }
        }

        drawQuad();

        glEnable(GL_DEPTH_TEST);
        _program->deactivate();

        glBindTexture(GL_TEXTURE_2D, 0);
    }

    void RenderableBlackHole::SendSchwarzschildTableToShader()
    {
        glBindBuffer(GL_SHADER_STORAGE_BUFFER, _ssboBlackHoleWarpTable);

        const size_t indexBufferSize = _blackHoleWarpTable.size() * sizeof(float);

        glBufferData(
            GL_SHADER_STORAGE_BUFFER,
            indexBufferSize,
            _blackHoleWarpTable.data(),
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
        constexpr std::string_view ShaderBaseDir = "${MODULE_BLACKHOLE}/shaders/";

        BlackHoleType bhType = static_cast<BlackHoleType>(_blackholeType.value());

        auto it = BlackHoleShaderConfigs.find(bhType);
        if (it == BlackHoleShaderConfigs.end()) {
            throw ghoul::RuntimeError("Unknown BlackHoleType", "RenderableBlackHole");
        }

        const BlackHoleShaderConfig& config = it->second;

        const std::string vertexShader = std::string(ShaderBaseDir) + config.shaderName + "_vs.glsl";
        const std::string fragmentShader = std::string(ShaderBaseDir) + config.shaderName + "_fs.glsl";

        _program = BaseModule::ProgramObjectManager.request(
            config.programName,
            [=]() -> std::unique_ptr<ghoul::opengl::ProgramObject> {
                return global::renderEngine->buildRenderProgram(
                    config.programName,
                    absPath(vertexShader),
                    absPath(fragmentShader)
                );
            }
        );

        if (!_program) {
            throw ghoul::RuntimeError("Shader program creation failed", "setupShaders");
        }

        ghoul::opengl::updateUniformLocations(*_program, _uniformCache);

        bindSSBOData(_program, "ssbo_warp_table", _ssboBlackHoleDataBinding, _ssboBlackHoleWarpTable);
        bindSSBOData(_program, "ssbo_star_map_start_indices", _ssboStarIndicesDataBinding, _ssboStarKDTreeIndices);
        bindSSBOData(_program, "ssbo_star_map", _ssboStarDataBinding, _ssboStarKDTree);

        _shaderIsDirty = false;
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

        _accretionDiskTexture = ghoul::io::TextureReader::ref().loadTexture(absPath("${MODULE_BLACKHOLE}/rendering/accretion_disk.png"), 1);

        if (_accretionDiskTexture) {
            _accretionDiskTexture->uploadTexture();
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
