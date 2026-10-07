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
    struct [[codegen::Dictionary(Substructs)]] Parameters {
        // struct A documentation
        struct A {
            // A::value documentation
            int value;
        };
        // variable structAValue documentation
        A structAValue;

        // struct B documentation
        struct B {
            // B::valval documentation
            float valval;
        };
        // variable structBValue documentation
        std::optional<B> structBValue;

        // struct C documentation
        struct C {
            // C::eulav documentation
            std::string eulav;

            // C::lavlav documentation
            bool lavlav;
        };
        // variable structCValue documentation
        std::vector<C> structCValue;

        // struct D documentation
        struct D {
            // D::a documentation
            std::string a;
            // D::b docuimentation
            int b;
        };

        // variable structDValue documentation
        D structDValue;

        // variable structDVectorValue documentation
        std::vector<D> structDVectorValue;

        // variable structDOptionalValue documentation
        std::optional<D> structDOptionalValue;
    };
} // namespace
#include "execution_structs_substructs_codegen.cpp"


TEST_CASE("Execution/Structs/Substruct:  Bake", "[Execution][Structs]") {
    using namespace std::string_literals;

    Dictionary d;
    {
        Dictionary e;
        e.setValue("Value", 5.0);
        d.setValue("StructAValue", e);
    }
    {
        Dictionary e;
        e.setValue("Valval", 2.1);
        d.setValue("StructBValue", e);
    }
    {
        Dictionary e;
        {
            Dictionary f;
            f.setValue("Eulav", "text"s);
            f.setValue("Lavlav", true);
            e.setValue("1", f);
        }
        {
            Dictionary f;
            f.setValue("Eulav", "text2"s);
            f.setValue("Lavlav", false);
            e.setValue("2", f);
        }
        d.setValue("StructCValue", e);
    }
    {
        Dictionary e;
        e.setValue("A", "abc"s);
        e.setValue("B", 2.0);

        Dictionary f;
        f.setValue("A", "def"s);
        f.setValue("B", 3.0);

        d.setValue("StructDValue", e);

        {
            Dictionary g;
            g.setValue("1", e);
            g.setValue("2", f);
            d.setValue("StructDVectorValue", g);
        }

        d.setValue("StructDOptionalValue", e);
    }

    const Parameters p = codegen::bake<Parameters>(d);
    CHECK(p.structAValue.value == 5);
    REQUIRE(p.structBValue.has_value());
    CHECK(p.structBValue->valval == 2.1f);
    REQUIRE(p.structCValue.size() == 2);
    CHECK(p.structCValue[0].eulav == "text");
    CHECK(p.structCValue[0].lavlav == true);
    CHECK(p.structCValue[1].eulav == "text2");
    CHECK(p.structCValue[1].lavlav == false);
    CHECK(p.structDValue.a == "abc");
    CHECK(p.structDValue.b == 2);
    REQUIRE(p.structDVectorValue.size() == 2);
    CHECK(p.structDVectorValue[0].a == "abc");
    CHECK(p.structDVectorValue[0].b == 2);
    CHECK(p.structDVectorValue[1].a == "def");
    CHECK(p.structDVectorValue[1].b == 3);
    REQUIRE(p.structDOptionalValue.has_value());
    CHECK(p.structDOptionalValue->a == "abc");
    CHECK(p.structDOptionalValue->b == 2);
}

TEST_CASE("Execution/Structs/Substruct:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 6);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "StructAValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variable structAValue documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "Value");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation == "A::value documentation");
        CHECK(v->documentations[0].verifier->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "StructBValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variable structBValue documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "Valval");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation == "B::valval documentation");
        CHECK(v->documentations[0].verifier->type() == "Double");
        CHECK(dynamic_cast<DoubleVerifier*>(v->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[2];
        CHECK(e.key == "StructCValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variable structCValue documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].optional);
        CHECK(v->documentations[0].documentation == "struct C documentation");
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(
            v->documentations[0].verifier.get()
        );
        REQUIRE(w);
        REQUIRE(w->documentations.size() == 2);
        CHECK(w->documentations[0].key == "Eulav");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].documentation == "C::eulav documentation");
        CHECK(w->documentations[0].verifier->type() == "String");
        StringVerifier* u =
            dynamic_cast<StringVerifier*>(w->documentations[0].verifier.get());
        REQUIRE(u);
        CHECK(u->mustBeNotEmpty() == false);

        CHECK(w->documentations[1].key == "Lavlav");
        CHECK(!w->documentations[1].optional);
        CHECK(w->documentations[1].documentation == "C::lavlav documentation");
        CHECK(w->documentations[1].verifier->type() == "Boolean");
        CHECK(dynamic_cast<BoolVerifier*>(w->documentations[1].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[3];
        CHECK(e.key == "StructDValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variable structDValue documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        CHECK(v->documentations[0].key == "A");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation == "D::a documentation");
        CHECK(v->documentations[0].verifier->type() == "String");
        StringVerifier* w =
            dynamic_cast<StringVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);

        CHECK(v->documentations[1].key == "B");
        CHECK(!v->documentations[1].optional);
        CHECK(v->documentations[1].documentation == "D::b docuimentation");
        CHECK(v->documentations[1].verifier->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(v->documentations[1].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[4];
        CHECK(e.key == "StructDVectorValue");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variable structDVectorValue documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 1);
        CHECK(v->documentations[0].key == "*");
        CHECK(v->documentations[0].optional);
        CHECK(v->documentations[0].documentation == "struct D documentation");
        CHECK(v->documentations[0].verifier->type() == "Table");
        TableVerifier* w = dynamic_cast<TableVerifier*>(
            v->documentations[0].verifier.get()
        );
        REQUIRE(w);

        REQUIRE(w->documentations.size() == 2);
        CHECK(w->documentations[0].key == "A");
        CHECK(!w->documentations[0].optional);
        CHECK(w->documentations[0].documentation == "D::a documentation");
        CHECK(w->documentations[0].verifier->type() == "String");
        StringVerifier* u =
            dynamic_cast<StringVerifier*>(w->documentations[0].verifier.get());
        REQUIRE(u);
        CHECK(u->mustBeNotEmpty() == false);

        CHECK(w->documentations[1].key == "B");
        CHECK(!w->documentations[1].optional);
        CHECK(w->documentations[1].documentation == "D::b docuimentation");
        CHECK(w->documentations[1].verifier->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(w->documentations[1].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[5];
        CHECK(e.key == "StructDOptionalValue");
        CHECK(e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variable structDOptionalValue documentation");
        CHECK(e.verifier->type() == "Table");
        TableVerifier* v = dynamic_cast<TableVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->documentations.size() == 2);
        CHECK(v->documentations[0].key == "A");
        CHECK(!v->documentations[0].optional);
        CHECK(v->documentations[0].documentation == "D::a documentation");
        CHECK(v->documentations[0].verifier->type() == "String");
        StringVerifier* w =
            dynamic_cast<StringVerifier*>(v->documentations[0].verifier.get());
        REQUIRE(w);
        CHECK(w->mustBeNotEmpty() == false);

        CHECK(v->documentations[1].key == "B");
        CHECK(!v->documentations[1].optional);
        CHECK(v->documentations[1].documentation == "D::b docuimentation");
        CHECK(v->documentations[1].verifier->type() == "Integer");
        CHECK(dynamic_cast<IntVerifier*>(v->documentations[1].verifier.get()));
    }
}
