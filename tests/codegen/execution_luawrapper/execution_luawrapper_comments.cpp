/*****************************************************************************************
 *                                                                                       *
 * OpenSpace Codegen                                                                     *
 *                                                                                       *
 * Copyright (c) 2021-2026                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>
#include <openspace/lua/lua_helper.h>
#include <openspace/misc/dictionary.h>
#include <openspace/scripting/lualibrary.h>
#include <optional>

namespace {
    /*
     * Some example documentation
     * that covers a few lines.
     * And another one for good measure
     */
    [[codegen::luawrap]] double foo(int arg) {
        return arg * 2.0;
    }

    // Some example documentation
    // that covers a few lines.
    // And another one for good measure
    [[codegen::luawrap]] void foo2() {
    }

    //
    [[codegen::luawrap]] void foo3() {
    }

    // 
    [[codegen::luawrap]] void foo4() {
    }

    //
    //
    [[codegen::luawrap]] void foo5() {
    }
} // namespace
#include "execution_luawrapper_comments_codegen.cpp"


TEST_CASE("Execution/LuaWrapper/Comments:  Direct Comment/1", "[Execution][LuaWrapper]") {
    CHECK(codegen::lua::Foo.name == "foo");
    CHECK(codegen::lua::Foo.function);
    CHECK(
        codegen::lua::Foo.helpText ==
        "Some example documentation that covers a few lines. "
        "And another one for good measure"
    );
}

TEST_CASE("Execution/LuaWrapper/Comments:  Direct Comment/2", "[Execution][LuaWrapper]") {
    CHECK(codegen::lua::Foo2.name == "foo2");
    CHECK(codegen::lua::Foo2.function);
    CHECK(
        codegen::lua::Foo2.helpText ==
        "Some example documentation that covers a few lines. "
        "And another one for good measure"
    );
}

TEST_CASE("Execution/LuaWrapper/Comments:  Empty Comment/1", "[Execution][LuaWrapper]") {
    CHECK(codegen::lua::Foo3.name == "foo3");
    CHECK(codegen::lua::Foo3.function);
    CHECK(codegen::lua::Foo3.helpText.empty());
}

TEST_CASE("Execution/LuaWrapper/Comments:  Empty Comment/2", "[Execution][LuaWrapper]") {
    CHECK(codegen::lua::Foo4.name == "foo4");
    CHECK(codegen::lua::Foo4.function);
    CHECK(codegen::lua::Foo4.helpText.empty());
}

TEST_CASE("Execution/LuaWrapper/Comments:  Empty Comment/3", "[Execution][LuaWrapper]") {
    CHECK(codegen::lua::Foo5.name == "foo5");
    CHECK(codegen::lua::Foo5.function);
    CHECK(codegen::lua::Foo5.helpText.empty());
}
