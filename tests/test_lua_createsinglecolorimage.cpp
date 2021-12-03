/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include "catch2/catch.hpp"

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
    ghoul::lua::LuaState L;
    ghoul::lua::push(L, "colorFile");
    ghoul::lua::push(L, std::vector{ 1.0, 0.0, 0.0 });

    int res = openspace::luascriptfunctions::createSingleColorImage(L);

    // One return value
    CHECK(res == 1);
    CHECK(lua_gettop(L) == 1);
    CHECK(lua_isstring(L, 1));

    std::string path = ghoul::lua::value<std::string>(L, 1);
    CHECK_THAT(
        path,
        Catch::Matchers::Contains("OpenSpace\\cache\\colorFile.ppm")
    );
}

TEST_CASE("CreateSingleColorImage: Faulty 1st input type", "[createsinglecolorimage]") {
    ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector{ 1.0, 0.0, 0.0 });
    ghoul::lua::push(L, std::vector{ 1.0, 0.0, 0.0 });

    CHECK_THROWS_WITH(
        openspace::luascriptfunctions::createSingleColorImage(L),
        Catch::Matchers::Contains("parameter 1 was not the expected type")
    );
}

TEST_CASE("CreateSingleColorImage: Faulty 2nd input type", "[createsinglecolorimage]") {
    ghoul::lua::LuaState L;
    ghoul::lua::push(L, "notCreatedColorFile");
    ghoul::lua::push(L, "not a vector");

    CHECK_THROWS_WITH(
        openspace::luascriptfunctions::createSingleColorImage(L),
        Catch::Matchers::Contains("parameter 2 was not the expected type")
    );
}

TEST_CASE("CreateSingleColorImage: Invalid number of inputs", "[createsinglecolorimage]")
{
    ghoul::lua::LuaState L;
    ghoul::lua::push(L, std::vector{ 1.0, 0.0, 0.0 });

    CHECK_THROWS_WITH(
        openspace::luascriptfunctions::createSingleColorImage(L),
        Catch::Matchers::Contains("Expected 2 arguments, got 1")
    );
}

TEST_CASE("CreateSingleColorImage: Faulty color value (vec4)",
          "[createsinglecolorimage]")
{
    ghoul::lua::LuaState L;
    ghoul::lua::push(L, "notCreatedColorFile");
    ghoul::lua::push(L, std::vector{ 1.0, 0.0, 0.0, 0.0 });

    CHECK_THROWS_WITH(
        openspace::luascriptfunctions::createSingleColorImage(L),
        Catch::Matchers::Contains(
            "Invalid color. Expected three double values {r, g, b} in range 0 to 1"
        )
    );
}

TEST_CASE("CreateSingleColorImage: Faulty color value (invalid values)",
          "[createsinglecolorimage]")
{
    ghoul::lua::LuaState L;
    ghoul::lua::push(L, "notCreatedColorFile");
    ghoul::lua::push(L, std::vector{ 255.0, 0.0, 0.0 }); // not a valid color

    CHECK_THROWS_WITH(
        openspace::luascriptfunctions::createSingleColorImage(L),
        Catch::Matchers::Contains(
            "Invalid color. Expected three double values {r, g, b} in range 0 to 1"
        )
    );
}

TEST_CASE("CreateSingleColorImage: Check if file was created",
          "[createsinglecolorimage]")
{
    ghoul::lua::LuaState L;
    ghoul::lua::push(L, "colorFile2");
    ghoul::lua::push(L, std::vector{ 0.0, 1.0, 0.0 });

    int res = openspace::luascriptfunctions::createSingleColorImage(L);

    CHECK(res == 1);
    std::string path = ghoul::lua::value<std::string>(L, 1);
    CHECK(std::filesystem::is_regular_file(path));
}

TEST_CASE("CreateSingleColorImage: Load created image", "[createsinglecolorimage]") {
    ghoul::lua::LuaState L;
    ghoul::lua::push(L, "colorFile");
    ghoul::lua::push(L, std::vector{ 1.0, 0.0, 0.0 });

    // Loads the same file that was created in a previous test case
    int res = openspace::luascriptfunctions::createSingleColorImage(L);
    CHECK(res == 1);
    CHECK(lua_gettop(L) == 1);

    std::string path = ghoul::lua::value<std::string>(L, 1);

    // Read the PPM file and check the image dimensions
    // (maybe too hard coded, but cannot load a texture here...)
    std::ifstream ppmFile(path, std::ifstream::binary);
    REQUIRE(ppmFile.is_open());

    std::string version;
    unsigned int width;
    unsigned int height;

    ppmFile >> version >> width >> height;

    REQUIRE(width == 1);
    REQUIRE(height == 1);
}

