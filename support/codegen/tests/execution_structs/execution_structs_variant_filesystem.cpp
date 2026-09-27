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
#include <filesystem>
#include <fstream>
#include <variant>
#include <vector>

namespace {
    struct [[codegen::Dictionary(VariantFilesystem)]] Parameters {
        // variant path vector path documentation
        std::variant<
            std::filesystem::path, std::vector<std::filesystem::path>
        > variantPath;

        // variant vector path path documentation
        std::variant<
            std::vector<std::filesystem::path>, std::filesystem::path
        > variantPath2;
    };
} // namespace
#include "execution_structs_variant_filesystem_codegen.cpp"


TEST_CASE("Execution/Structs/Variant-Filesystem:  Bake", "[Execution][Structs]") {
    using namespace std::string_literals;

    const std::filesystem::path path = std::filesystem::temp_directory_path();
    std::filesystem::path tmpFile = (path / "codegen_execution_variants.txt");
    {
        std::ofstream f(tmpFile);
        f << "unit test";
    }

    std::filesystem::path tmpFile2 = (path / "codegen_execution_variants_2.txt");
    {
        std::ofstream f(tmpFile2);
        f << "unit test";
    }

    {
        ghoul::Dictionary d1;
        d1.setValue("VariantPath", tmpFile);
        {
            ghoul::Dictionary e;
            e.setValue("1", tmpFile);
            e.setValue("2", tmpFile2);
            d1.setValue("VariantPath2", e);
        }

        const Parameters p1 = codegen::bake<Parameters>(d1);
        REQUIRE(std::holds_alternative<std::filesystem::path>(p1.variantPath));
        CHECK(std::get<std::filesystem::path>(p1.variantPath) == tmpFile);
        REQUIRE(
            std::holds_alternative<std::vector<std::filesystem::path>>(p1.variantPath2)
        );
        REQUIRE(
            std::get<std::vector<std::filesystem::path>>(p1.variantPath2).size() == 2
        );
        CHECK(
            std::get<std::vector<std::filesystem::path>>(p1.variantPath2)[0] == tmpFile
        );
        CHECK(
            std::get<std::vector<std::filesystem::path>>(p1.variantPath2)[1] == tmpFile2
        );
    }

    {
        ghoul::Dictionary d2;
        {
            ghoul::Dictionary e;
            e.setValue("1", tmpFile);
            e.setValue("2", tmpFile2);
            d2.setValue("VariantPath", e);
        }
        d2.setValue("VariantPath2", tmpFile);

        const Parameters p2 = codegen::bake<Parameters>(d2);
        REQUIRE(
            std::holds_alternative<std::vector<std::filesystem::path>>(p2.variantPath)
        );
        REQUIRE(
            std::get<std::vector<std::filesystem::path>>(p2.variantPath).size() == 2
        );
        CHECK(
            std::get<std::vector<std::filesystem::path>>(p2.variantPath)[0] == tmpFile
        );
        CHECK(
            std::get<std::vector<std::filesystem::path>>(p2.variantPath)[1] == tmpFile2
        );
        REQUIRE(std::holds_alternative<std::filesystem::path>(p2.variantPath2));
        CHECK(std::get<std::filesystem::path>(p2.variantPath2) == tmpFile);
    }
}

TEST_CASE("Execution/Structs/Variant-Filesystem:  Documentation", "[Execution][Structs]") {
    using namespace openspace;
    Documentation doc = codegen::doc<Parameters>("");

    REQUIRE(doc.entries.size() == 2);
    {
        const DocumentationEntry& e = doc.entries[0];
        CHECK(e.key == "VariantPath");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variant path vector path documentation");
        CHECK(e.verifier->type() == "File, or Table");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(v->values[0].get());
        REQUIRE(w);
        CHECK(v->values[1]->type() == "Table");
        TableVerifier* w2 = dynamic_cast<TableVerifier*>(v->values[1].get());
        REQUIRE(w2->documentations.size() == 1);
        CHECK(w2->documentations[0].key == "*");
        CHECK(w2->documentations[0].verifier->type() == "File");
        CHECK(dynamic_cast<FileVerifier*>(w2->documentations[0].verifier.get()));
    }
    {
        const DocumentationEntry& e = doc.entries[1];
        CHECK(e.key == "VariantPath2");
        CHECK(!e.optional);
        CHECK(!e.isPrivate);
        CHECK(e.documentation == "variant vector path path documentation");
        CHECK(e.verifier->type() == "Table, or File");
        OrVerifier* v = dynamic_cast<OrVerifier*>(e.verifier.get());
        REQUIRE(v);
        REQUIRE(v->values.size() == 2);
        CHECK(v->values[0]->type() == "Table");
        TableVerifier* w2 = dynamic_cast<TableVerifier*>(v->values[0].get());
        REQUIRE(w2->documentations.size() == 1);
        CHECK(w2->documentations[0].key == "*");
        CHECK(w2->documentations[0].verifier->type() == "File");
        CHECK(dynamic_cast<FileVerifier*>(w2->documentations[0].verifier.get()));
        CHECK(v->values[1]->type() == "File");
        FileVerifier* w = dynamic_cast<FileVerifier*>(v->values[1].get());
        REQUIRE(w);
    }
}
