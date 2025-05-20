#ifndef __OPENSPACE_MODULE_BLACKHOLE___RENDERABLECANVAS___H__
#define __OPENSPACE_MODULE_BLACKHOLE___RENDERABLECANVAS___H__

#include <ghoul/opengl/bufferbinding.h>
#include <ghoul/opengl/uniformcache.h>
#include <ghoul/opengl/textureunit.h>

#include <openspace/rendering/renderable.h>

#include <openspace/properties/list/doublelistproperty.h>
#include <openspace/properties/scalar/floatproperty.h>
#include <openspace/properties/misc/optionproperty.h>

#include <modules/blackhole/rendering/viewport.h>
#include <modules/blackhole/rendering/kdtree.h>




namespace openspace {

    class RenderableBlackHole : public Renderable {
    public:
        explicit RenderableBlackHole(const ghoul::Dictionary& dictionary);
        ~RenderableBlackHole() override;

        void initialize() override;
        void initializeGL() override;
        void deinitializeGL() override;


        bool isReady() const override;

        void render(const RenderData& data, RendererTasks& rendererTask) override;
        void update(const UpdateData& data) override;


        static documentation::Documentation Documentation();

    private:
        void SendLookupTableToShader();
        void SendStarKDTreeToShader();
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
        std::unique_ptr<ghoul::opengl::ProgramObject> _cullProgram = nullptr;
        glm::dvec3 _chachedTranslation{};
        glm::dvec3 _chacedCameraPos{};
        static constexpr size_t _rayCount{ 300 };
        static constexpr size_t _rayCountHighRes{ 750 };
        static constexpr size_t _stepsCount = 100000;
        static constexpr float _stepLength = 0.0001f;
        static constexpr size_t _mapCount = 5;

        properties::FloatProperty _solarMass;
        properties::FloatProperty _kerrRotation;
        properties::StringProperty _colorBVMapTexturePath;
        properties::DoubleListProperty _starMapRanges;
        properties::OptionProperty _blackholeType;

        enum class BlackHoleType{
            schwarzschild,
            kerr
        };

        struct LayerLayout {
            std::vector<std::pair<float, float>> ranges;
            std::vector<float> positions;
            bool isDirty = false;
            void calcPositions(float scale) {
                positions.resize(ranges.size());
                for (int i = 0; i < positions.size(); i++) {
                    positions[i] = (ranges[i].first + ranges[i].second) / 2.0f * scale;
                }
            }
        };

        LayerLayout _layerLayout{};

        struct BlackHoleShaderConfig {
            std::string shaderName;
            std::string programName;
        };

        inline static const std::unordered_map<BlackHoleType, BlackHoleShaderConfig> BlackHoleShaderConfigs {
            { BlackHoleType::schwarzschild, { "schwarzschild", "BlackHoleProgram" } },
            { BlackHoleType::kerr,         { "kerr", "KerrBlackHoleProgram" } }
        };

        float _rs;
        float _rEnvmap;
        float _rCamera;

        ViewPort _viewport{};

        std::vector<float> _blackHoleWarpTable;
        std::vector<float> flatDataStar;
        StarMaps _starKDTree{};

        std::unique_ptr<ghoul::opengl::BufferBinding<
            ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboBlackHoleDataBinding;
        std::unique_ptr<ghoul::opengl::BufferBinding<
            ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboStarDataBinding;
        std::unique_ptr<ghoul::opengl::BufferBinding<
            ghoul::opengl::bufferbinding::Buffer::ShaderStorage>> _ssboStarIndicesDataBinding;

        GLuint _quadVao = 0;
        GLuint _quadVbo = 0;
        GLuint _ssboBlackHoleWarpTable = 0;
        GLuint _ssboStarKDTree = 0;
        GLuint _ssboStarKDTreeIndices = 0;

        UniformCache(environmentTexture, viewGrid, colorBVMap) _uniformCache;
        UniformCache(worldRotationMatrix, cameraRotationMatrix, r_0) _uniformSchwarzschildCache;
        UniformCache(cameraRotationMatrix, accretionDisk) _uniformKerrCache;

        std::unique_ptr<ghoul::opengl::Texture> _environmentTexture;
        std::unique_ptr<ghoul::opengl::Texture> _colorBVMapTexture;
        std::unique_ptr<ghoul::opengl::Texture> _accretionDiskTexture;
    };

} // openspace namespace
#endif //__OPENSPACE_MODULE_BLACKHOLE___RENDERABLECANVAS___H__
