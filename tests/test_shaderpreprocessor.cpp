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

#include <openspace/filesystem/filesystem.h>
#include <openspace/misc/dictionary.h>
#include <openspace/opengl/shaderpreprocessor.h>
#include <filesystem>
#include <format>
#include <fstream>
#include <string>
#include <string_view>

using namespace openspace;

namespace {
    // A small shader with the same binding template as shaders/depth.glsl
    constexpr std::string_view BindingShader =
        "layout(std140, binding = #{depthDataBinding}) uniform Block { float x; };\n";

    // The preprocessor reads from a file, so this writes the shader to a temporary file
    std::filesystem::path writeTemporaryShader(std::string_view name,
                                               std::string_view content)
    {
        const std::filesystem::path path = absPath(
            std::format("${{TEMPORARY}}/{}", name)
        );
        std::ofstream file = std::ofstream(path);
        file << content;
        return path;
    }
} // namespace

TEST_CASE("ShaderPreprocessor: Depth data binding", "[shaderpreprocessor]") {
    // Test that the preprocessor can fill in the default depth data binding when no
    // value is given. Shaders need this to read depthNear from the right buffer
    const std::filesystem::path path = writeTemporaryShader(
        "test_depthbinding.glsl",
        BindingShader
    );

    std::string result;
    {
        opengl::ShaderPreprocessor preprocessor = opengl::ShaderPreprocessor(path);
        result = preprocessor.process();
    }
    std::filesystem::remove(path);

    CHECK(result.contains(std::format("binding = {})", opengl::DepthDataBinding)));
    CHECK(!result.contains("#{"));
}

TEST_CASE("ShaderPreprocessor: Depth data binding can be overridden",
          "[shaderpreprocessor]")
{
    // Test that a caller can override the default depth data binding by
    // setting depthDataBinding in the dictionary
    const std::filesystem::path path = writeTemporaryShader(
        "test_depthbindingoverride.glsl",
        BindingShader
    );

    Dictionary dictionary;
    dictionary.setValue("depthDataBinding", opengl::DepthDataBinding + 1);
    std::string result;
    {
        opengl::ShaderPreprocessor preprocessor = opengl::ShaderPreprocessor(
            path,
            dictionary
        );
        result = preprocessor.process();
    }
    std::filesystem::remove(path);

    CHECK(result.contains(std::format("binding = {})", opengl::DepthDataBinding + 1)));
}

TEST_CASE("ShaderPreprocessor: Depth shader", "[shaderpreprocessor]") {
    // Test that the real shaders/depth.glsl can be preprocessed, and that it still has
    // the depth data binding and the depthNear variable that other shaders use
    opengl::ShaderPreprocessor preprocessor = opengl::ShaderPreprocessor(
        absPath("${SHADERS}/depth.glsl")
    );
    const std::string result = preprocessor.process();

    CHECK(result.contains(std::format("binding = {})", opengl::DepthDataBinding)));
    CHECK(result.contains("float depthNear;"));
}
