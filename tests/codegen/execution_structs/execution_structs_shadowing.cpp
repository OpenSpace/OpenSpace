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
#include <vector>

using namespace openspace;

namespace {
    struct [[codegen::Dictionary(D)]] P {
        struct A {
            enum class T {
                Type1,
                Type2
            };
            T t;
        };
        A a;

        struct B {
            enum class T {
                Type3,
                Type4
            };
            T t;
        };
        B b;

        enum class T {
            Type5,
            Type6
        };
        T t;

        struct C {
            struct A {
                int ca;
            };
            A a;

            enum class T {
                Type7,
                Type8
            };
            T t;
        };
        C c;
    };
} // namespace
#include "execution_structs_shadowing_codegen.cpp"


TEST_CASE("Execution/Structs/Shadowing:  Bake", "[Execution][Structs]") {
    using namespace std::string_literals;

    Dictionary d;
    {
        Dictionary e;
        e.setValue("T", "Type1"s);
        d.setValue("A", e);
    }
    {
        Dictionary e;
        e.setValue("T", "Type4"s);
        d.setValue("B", e);
    }
    d.setValue("T", "Type5"s);
    {
        Dictionary e;
        {
            Dictionary f;
            f.setValue("Ca", 2.0);
            e.setValue("A", f);
        }
        e.setValue("T", "Type8"s);
        d.setValue("C", e);
    }


    const P p = codegen::bake<P>(d);
    CHECK(p.a.t == P::A::T::Type1);
    CHECK(p.b.t == P::B::T::Type4);
    CHECK(p.t == P::T::Type5);
    CHECK(p.c.a.ca == 2);
    CHECK(p.c.t == P::C::T::Type8);
}

TEST_CASE("Execution/Structs/Shadowing:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    Documentation doc = codegen::doc<P>("");

    REQUIRE(doc.entries.size() == 4);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "A");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation.empty());
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "T");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "String");
        StringInListVerifier* sil =
            dynamic_cast<StringInListVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(sil);
        CHECK(sil->values == std::vector<std::string>{ "Type1", "Type2" });
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "B");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation.empty());
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "T");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "String");
        StringInListVerifier* sil =
            dynamic_cast<StringInListVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(sil);
        CHECK(sil->values == std::vector<std::string>{ "Type3", "Type4" });
    }
    {
        const DocumentationEntry& e = doc.entries[2];
        CHECK(e.key == "T");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation.empty());
        CHECK(e.verifier->type() == "String");
        StringInListVerifier* sil =
            dynamic_cast<StringInListVerifier*>(e.verifier.get());
        REQUIRE(sil);
        CHECK(sil->values == std::vector<std::string>{ "Type5", "Type6" });
    }
    {
        const DocumentationEntry& e = doc.entries[3];
        CHECK(e.key == "C");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation.empty());
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        CHECK(v->documentations[0].key == "A");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation.empty());
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w =
            dynamic_cast<TableVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        REQUIRE(w->documentations.size() == 1);
        CHECK(w->documentations[0].key == "Ca");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].documentation.empty());
        CHECK(w->documentations[0].verifier->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(w->documentations[0].verifier.get()));

        CHECK(v->documentations[1].key == "T");
        CHECK(!v->documentations[1].optional);
        CHECK(v->documentations[1].documentation.empty());
        CHECK(v->documentations[1].verifier->type() == "String");
        StringInListVerifier* sil =
            dynamic_cast<StringInListVerifier*>(v->documentations[1].verifier.get());
        REQUIRE(sil);
        CHECK(sil->values == std::vector<std::string>{ "Type7", "Type8" });
    }
}
