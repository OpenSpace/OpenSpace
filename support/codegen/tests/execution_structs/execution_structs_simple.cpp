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

using namespace openspace;

namespace {
    struct [[codegen::Dictionary(Simple)]] Parameters {
        // value documentation
        float value;
    };
} // namespace
#include "execution_structs_simple_codegen.cpp"


TEST_CASE("Execution/Structs/Simple:  Bake", "[Execution][Structs]") {
    Dictionary d;
    d.setValue("Value", 5.0);

    const Parameters p = codegen::bake<Parameters>(d);
    CHECK(p.value == 5.f);
}

TEST_CASE("Execution/Structs/Simple:  Documentation", "[Execution][Structs]") {
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 1);
    const DocumentationEntry& e = doc.entries[0];
    CHECK(e.key == "Value");
    CHECK(!e.optional);
    CHECK(!e.isPrivate);
    CHECK(e.documentation == "value documentation");
    CHECK(e.verifier->type() == "Double");
    CHECK(dynamic_cast<DoubleVerifier*>(e.verifier.get()));
}
