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
#include <optional>
#include <variant>
#include <vector>

using namespace openspace;

namespace {
    struct [[codegen::Dictionary(Variant)]] Parameters {
        // optional variant vector documentation
        std::optional<std::variant<std::string, std::vector<std::string>>> ovv;
    };
} // namespace
#include "execution_structs_optional_variant_vector_codegen.cpp"


TEST_CASE("Execution/Structs/Optional/Variant/Vector:  Bake", "[Execution][Structs]") {
    using namespace std::string_literals;

    {
        const Dictionary d;
        const Parameters p = codegen::bake<Parameters>(d);
        CHECK(!p.ovv.has_value());
    }

    {
        Dictionary d;
        d.setValue("Ovv", "abc"s);
        const Parameters p = codegen::bake<Parameters>(d);
        REQUIRE(p.ovv.has_value());
        REQUIRE(std::holds_alternative<std::string>(*p.ovv));
        CHECK(std::get<std::string>(*p.ovv) == "abc");
    }
    {
        Dictionary d;
        Dictionary e;
        e.setValue("1", "def"s);
        e.setValue("2", "ghi"s);
        d.setValue("Ovv", e);
        const Parameters p = codegen::bake<Parameters>(d);
        REQUIRE(p.ovv.has_value());
        REQUIRE(std::holds_alternative<std::vector<std::string>>(*p.ovv));
        CHECK(
            std::get<std::vector<std::string>>(*p.ovv) ==
            std::vector<std::string>{ "def", "ghi" }
        );
    }
}

TEST_CASE(
    "Execution/Structs/Optional/Variant/Vector:  Documentation",
    "[Execution][Structs]"
)
{
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 1);
    const DocumentationEntry& e = doc.entries[0];
    CHECK(e.key == "Ovv");
    CHECK(e.optional);
    CHECK(!e.isPrivate);
    CHECK(e.documentation == "optional variant vector documentation");
    CHECK(e.verifier->type() == "String, or List of strings");
    OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
    REQUIRE(v);
    REQUIRE(v->values.size() == 2);
    CHECK(v->values[0]->type() == "String");
    StringVerifier* w = dynamic_cast<StringVerifier*>(v->values[0].get());
    REQUIRE(w);
    CHECK(w->mustBeNotEmpty() == false);
    CHECK(v->values[1]->type() == "List of strings");
    StringListVerifier* u = dynamic_cast<StringListVerifier*>(v->values[1].get());
    REQUIRE(u);
    REQUIRE(u->documentations.size() == 1);
    CHECK(u->documentations[0].key == "*");
    CHECK(u->documentations[0].verifier->type() == "String");
    StringVerifier* x =
        dynamic_cast<StringVerifier*>(u->documentations[0].verifier.get());
    REQUIRE(x);
    CHECK(x->mustBeNotEmpty() == false);
}
