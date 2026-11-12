/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
 *                                                                                       *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this  *
 * software and associated documentation files (the "Software"), to deal in the Software *
 * without restriction, including without limitation the rights to use, copy, modify,    *
 * merge, publish, distribute, sublicense, and/or sell copies of the Software, and to    *
 * permit persons to whom the Software is furnished to do so, subject to the following   *
 * conditions:                                                                           *
 *                                                                                       *
 * The above copyright notice and this permission notice shall be included in all copies *
 * or substantial portions of the Software.                                              *
 *                                                                                       *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,   *
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A         *
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT    *
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF  *
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  *
 * OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                                         *
 ****************************************************************************************/

#include <catch2/catch_test_macros.hpp>

#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/filesystem/filesystem.h>
#include <openspace/misc/defer.h>
#include <openspace/misc/dictionary.h>
#include <openspace/misc/exception.h>
#include <openspace/misc/stringhelper.h>
#include <openspace/opengl/shaderpreprocessor.h>
#include <openspace/systemcapabilities/openglcapabilitiescomponent.h>
#include <openspace/systemcapabilities/systemcapabilities.h>
#include <openspace/util/openspacemodule.h>
#include <glslang/Public/ResourceLimits.h>
#include <glslang/Public/ShaderLang.h>
#include <algorithm>
#include <array>
#include <filesystem>
#include <format>
#include <fstream>
#include <iterator>
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace {
    using namespace openspace;

    // OpenSpace always requests an OpenGL 4.6 core profile context through SGCT
    constexpr int GlslVersion = 460;

    // Wraps a file that only provides `Fragment getFragment()` into the main fragment
    // shader of the renderer, the same way `RenderEngine::buildRenderProgram` does
    constexpr std::string_view RenderFsPath = "${SHADERS}/render_fs.glsl";
    constexpr std::string_view RenderFragmentShaderPath =
        "${SHADERS}/framebuffer/renderframebuffer_fs.glsl";

    struct ShaderFile {
        std::filesystem::path path;
        EShLanguage stage = EShLangVertex;
        bool providesFragmentFunction = false;
    };

    std::optional<EShLanguage> shaderStage(const std::filesystem::path& path) {
        const std::string stem = path.stem().string();
        if (stem.ends_with("_vs")) {
            return EShLangVertex;
        }
        else if (stem.ends_with("_gs")) {
            return EShLangGeometry;
        }
        else if (stem.ends_with("_fs")) {
            return EShLangFragment;
        }
        else {
            return std::nullopt;
        }
    }

    std::string readFile(const std::filesystem::path& path) {
        std::ifstream file = std::ifstream(path);
        return std::string(
            std::istreambuf_iterator<char>(file),
            std::istreambuf_iterator<char>()
        );
    }

    /**
     * Returns all shader stage files of the core and of all enabled modules. Files that
     * neither start a shader stage (`#version 460 core`) nor provide a
     * `Fragment getFragment()` function are only ever included and are skipped.
     */
    std::vector<ShaderFile> collectShaderFiles() {
        std::vector<std::filesystem::path> folders = { absPath("${SHADERS}") };
        for (const OpenSpaceModule* m : global::moduleEngine->modules()) {
            const std::string token = std::format(
                "${{MODULE_{}}}", toUpperCase(m->identifier())
            );
            if (!FileSys.hasRegisteredToken(token)) {
                continue;
            }

            const std::filesystem::path folder = absPath(token) / "shaders";
            if (std::filesystem::is_directory(folder)) {
                folders.push_back(folder);
            }
        }

        // The render and framebuffer shaders wrap other shaders and are tested through
        // those, or need raycaster-specific data that is only available at runtime
        const std::filesystem::path renderFs = absPath(RenderFsPath);
        const std::filesystem::path framebufferFolder = absPath("${SHADERS}/framebuffer");

        std::vector<ShaderFile> result;
        namespace fs = std::filesystem;
        for (const fs::path& dir : folders) {
            for (const fs::directory_entry& e : fs::recursive_directory_iterator(dir)) {
                const fs::path& path = e.path();
                if (!e.is_regular_file() || path.extension() != ".glsl" ||
                    path == renderFs || path.parent_path() == framebufferFolder)
                {
                    continue;
                }

                const std::optional<EShLanguage> stage = shaderStage(path);
                if (!stage.has_value()) {
                    continue;
                }

                const std::string content = readFile(path);
                const bool isStage = content.contains("#version 460 core");
                const bool providesFragmentFunction =
                    content.contains("Fragment getFragment()");
                if (isStage || providesFragmentFunction) {
                    result.push_back({
                        .path = path,
                        .stage = *stage,
                        .providesFragmentFunction = providesFragmentFunction
                    });
                }
            }
        }

        std::sort(
            result.begin(),
            result.end(),
            [](const ShaderFile& lhs, const ShaderFile& rhs) {
                return lhs.path < rhs.path;
            }
        );
        return result;
    }

    /**
     * Creates a dictionary that contains representative values for all keys that the
     * shaders substitute at runtime. Keys that a shader does not use are ignored by the
     * preprocessor, so a single dictionary can be used for all shaders.
     */
    Dictionary representativeDictionary() {
        Dictionary dict;

        // Data that the `RenderEngine` adds to every render program
        Dictionary rendererData;
        rendererData.setValue(
            "fragmentRendererPath",
            std::string(RenderFragmentShaderPath)
        );
        rendererData.setValue("hdrExposure", std::string("3.7"));
        rendererData.setValue("disableHDR", std::string("0"));
        dict.setValue("rendererData", std::move(rendererData));

        // CefWebGui: GuiRenderHandler
        dict.setValue("useAcceleratedRendering", true);

        // Molecule: PostProcessing
        dict.setValue("UseMotionBlur", 1);
        dict.setValue("TileSize", 8);

        // Globebrowsing: RenderableGlobe::recompileShaders and RingsComponent, with one
        // active layer in each layer group
        constexpr std::array<std::string_view, 5> LayerGroups = {
            "HeightLayers", "ColorLayers", "Overlays", "NightLayers", "WaterMasks"
        };
        Dictionary layerGroups;
        for (size_t i = 0; i < LayerGroups.size(); i++) {
            const std::string_view group = LayerGroups[i];
            layerGroups.setValue(std::to_string(i), std::string(group));
            dict.setValue(std::format("lastLayerIndex{}", group), 0);
            dict.setValue(std::format("use{}", group), true);
            dict.setValue(std::format("blend{}", group), true);
            dict.setValue(std::format("{}0LayerType", group), 0);
            dict.setValue(std::format("{}0BlendMode", group), 0);
            dict.setValue(std::format("{}0LayerAdjustmentType", group), 0);
        }
        dict.setValue("layerGroups", std::move(layerGroups));
        dict.setValue("useAccurateNormals", std::string("1"));
        dict.setValue("performShading", std::string("1"));
        dict.setValue("useEclipseShadows", std::string("1"));
        dict.setValue("useEclipseHardShadows", std::string("0"));
        dict.setValue("enableShadowMapping", std::string("1"));
        dict.setValue("useRingShadows", std::string("1"));
        dict.setValue("showChunkEdges", std::string("0"));
        dict.setValue("showHeightResolution", std::string("0"));
        dict.setValue("showHeightIntensities", std::string("0"));
        dict.setValue("defaultHeight", std::string("0.000000"));
        dict.setValue("nShadowSamples", 4);
        dict.setValue("nEclipseShadows", 0);
        dict.setValue("useDepthmapShadows", 1);
        dict.setValue("nDepthMaps", 1);

        return dict;
    }

    enum class Rules {
        // Plain GLSL, the way an OpenGL driver compiles the shaders
        OpenGL,
        // The stricter rules that apply when generating SPIR-V for OpenGL
        SpirV
    };

    struct ValidationResult {
        bool isValid = false;
        std::string log;
    };

    ValidationResult validate(const ShaderFile& shader, const Dictionary& dictionary,
                              Rules rules)
    {
        Dictionary dict = dictionary;
        std::filesystem::path root = shader.path;
        if (shader.providesFragmentFunction) {
            dict.setValue("fragmentPath", shader.path);
            root = absPath(RenderFsPath);
        }

        opengl::ShaderPreprocessor preprocessor = opengl::ShaderPreprocessor(
            root,
            std::move(dict)
        );
        std::string source;
        try {
            source = preprocessor.process();
        }
        catch (const RuntimeError& e) {
            return {
                .isValid = false,
                .log = std::format("Preprocessor: {}", e.message)
            };
        }

        glslang::TShader glslShader = glslang::TShader(shader.stage);
        const char* sourcePtr = source.c_str();
        glslShader.setStrings(&sourcePtr, 1);
        EShMessages messages = EShMsgDefault;
        switch (rules) {
            case Rules::OpenGL:
                // No client or target is set, so the shader is validated as plain GLSL
                // the way an OpenGL driver would compile it
                glslShader.setEnvInput(
                    glslang::EShSourceGlsl,
                    shader.stage,
                    glslang::EShClientNone,
                    0
                );
                break;
            case Rules::SpirV:
                glslShader.setEnvInput(
                    glslang::EShSourceGlsl,
                    shader.stage,
                    glslang::EShClientOpenGL,
                    100
                );
                glslShader.setEnvClient(
                    glslang::EShClientOpenGL,
                    glslang::EShTargetOpenGL_450
                );
                glslShader.setEnvTarget(glslang::EShTargetSpv, glslang::EShTargetSpv_1_0);
                // The shaders rely on the driver to assign locations and bindings, which
                // SPIR-V would otherwise require to be explicit for every variable
                glslShader.setAutoMapLocations(true);
                glslShader.setAutoMapBindings(true);
                messages = EShMsgSpvRules;
                break;
            default:
                throw MissingCaseException();
        }
        const bool isValid = glslShader.parse(
            GetDefaultResources(),
            GlslVersion,
            false,
            messages
        );

        // The `#line` directives inserted by the preprocessor use the index of the file
        // as the source string number, which is the first number in each glslang message
        return {
            .isValid = isValid,
            .log = std::format(
                "{}\nFiles:\n{}",
                glslShader.getInfoLog(), preprocessor.includedFiles()
            )
        };
    }
    void validateAllShaders(Rules rules) {
        // The preprocessor asks for the GPU vendor. The component is never detected, so
        // the vendor is `Other` and the preprocessor emits vendor-neutral `#line`
        // directives. Both test cases can run in the same process, so the component is
        // only added once
        using namespace systemcapabilities;
        try {
            SystemCapabilities::ref().component<OpenGLCapabilitiesComponent>();
        }
        catch (const SystemCapabilities::CapabilitiesComponentNotFoundError&) {
            SystemCapabilities::ref().addComponent(
                std::make_unique<OpenGLCapabilitiesComponent>()
            );
        }

        glslang::InitializeProcess();
        defer { glslang::FinalizeProcess(); };

        const std::vector<ShaderFile> shaders = collectShaderFiles();
        REQUIRE_FALSE(shaders.empty());

        const Dictionary dictionary = representativeDictionary();
        for (const ShaderFile& shader : shaders) {
            const ValidationResult result = validate(shader, dictionary, rules);
            INFO(std::format("{}\n{}", shader.path, result.log));
            CHECK(result.isValid);
        }
    }
} // namespace

TEST_CASE(
    "ShaderValidation: Compile all shaders with glslang using OpenGL rules",
    "[.][shadervalidationopengl]"
)
{
    validateAllShaders(Rules::OpenGL);
}

TEST_CASE(
    "ShaderValidation: Compile all shaders with glslang using SPIR-V rules",
    "[.][shadervalidationspirv]"
)
{
    validateAllShaders(Rules::SpirV);
}
