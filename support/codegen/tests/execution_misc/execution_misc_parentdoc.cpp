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
#include <openspace/misc/dictionary.h>

namespace {
    struct [[codegen::Dictionary(ABC)]] Parameters {
        float test;
    };
} // namespace
#include "execution_misc_parentdoc_codegen.cpp"


TEST_CASE("Execution/Misc:  Parenting", "[Execution][Misc]") {
    using namespace openspace;

    Documentation parent;
    parent.entries.emplace_back("key", std::make_shared<IntVerifier>());
    parent.entries.emplace_back("key2", std::make_shared<Vector2Verifier<int>>());

    const Documentation d = codegen::doc<Parameters>("abc", parent);

    REQUIRE(d.entries.size() == 3);
    CHECK(d.entries[0].key == "key");
    CHECK(d.entries[0].verifier->type() == "Integer");
    CHECK(d.entries[1].key == "key2");
    CHECK(d.entries[1].verifier->type() == "Vector2<int>");
    CHECK(d.entries[2].key == "Test");
    CHECK(d.entries[2].verifier->type() == "Double");
}
