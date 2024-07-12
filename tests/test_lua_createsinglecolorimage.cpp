/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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
#include <catch2/matchers/catch_matchers_exception.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>

#include <openspace/engine/openspaceengine.h>
#include <ghoul/filesystem/file.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/lua/ghoul_lua.h>
#include <ghoul/lua/luastate.h>
#include <ghoul/lua/lua_helper.h>
#include <filesystem>

TEST_CASE("CreateSingleColorImage: Create image and check return value",
          "[createsinglecolorimage]")
{
    const std::filesystem::path path = createSingleColorImage(
        "colorFile",
        glm::dvec3(1.0, 0.0, 0.0)
    );

    CHECK_THAT(path.string(), Catch::Matchers::ContainsSubstring("colorFile.ppm"));
}

TEST_CASE("CreateSingleColorImage: Faulty color value (invalid values)",
          "[createsinglecolorimage]")
{
    CHECK_THROWS_WITH(
        createSingleColorImage("notCreatedColorFile", glm::dvec3(255.0, 0.0, 0.0)),
        Catch::Matchers::Equals(
            "Invalid color. Expected three double values {r, g, b} in range 0 to 1"
        )
    );
}

TEST_CASE("CreateSingleColorImage: Check if file was created",
          "[createsinglecolorimage]")
{
    const std::filesystem::path path = createSingleColorImage(
        "colorFile2",
        glm::dvec3(0.0, 1.0, 0.0)
    );
    CHECK(std::filesystem::is_regular_file(path));
}

TEST_CASE("CreateSingleColorImage: Load created image", "[createsinglecolorimage]") {
    const std::filesystem::path path = createSingleColorImage(
        "colorFile",
        glm::dvec3(1.0, 0.0, 0.0)
    );

    // Read the PPM file and check the image dimensions
    // (maybe too hard coded, but cannot load a texture here...)
    std::ifstream ppmFile = std::ifstream(path, std::ifstream::binary);
    REQUIRE(ppmFile.is_open());

    std::string version;
    unsigned int width = 0;
    unsigned int height = 0;

    ppmFile >> version >> width >> height;

    CHECK(width == 1);
    CHECK(height == 1);
}
