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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/documentation/verifier.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/dictionary.h>
#include <string>

TEST_CASE("Documentation: Constructor", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc;

    // Basic Verifiers
    doc.entries.emplace_back(
        "BoolVerifier",
        new BoolVerifier,
        Optional::No
    );
    doc.entries.emplace_back(
        "DoubleVerifier",
        new DoubleVerifier,
        Optional::No
    );
    doc.entries.emplace_back(
        "IntVerifier",
        new IntVerifier,
        Optional::No
    );
    doc.entries.emplace_back(
        "StringVerifier",
        new StringVerifier,
        Optional::No
    );
    doc.entries.emplace_back(
        "IdentifierVerifier",
        new IdentifierVerifier,
        Optional::No
    );
    doc.entries.emplace_back(
        "FileVerifier",
        new FileVerifier,
        Optional::No
    );
    doc.entries.emplace_back(
        "DirectoryVerifier",
        new DirectoryVerifier,
        Optional::No
    );
    doc.entries.emplace_back(
        "DateTimeVerifier",
        new DateTimeVerifier,
        Optional::No
    );
    doc.entries.emplace_back(
        "TableVerifier",
        new TableVerifier,
        Optional::No
    );

    // Operator Verifiers
    doc.entries.emplace_back(
        "LessDouble",
        new DoubleLessVerifier(0.0),
        Optional::No
    );
    doc.entries.emplace_back(
        "LessInt",
        new IntLessVerifier(0),
        Optional::No
    );
    doc.entries.emplace_back(
        "LessEqualDouble",
        new DoubleLessEqualVerifier(0.0),
        Optional::No
    );
    doc.entries.emplace_back(
        "LessEqualInt",
        new IntLessEqualVerifier(0),
        Optional::No
    );

    doc.entries.emplace_back(
        "GreaterDouble",
        new DoubleGreaterVerifier(0.0),
        Optional::No
    );
    doc.entries.emplace_back(
        "GreaterInt",
        new IntGreaterVerifier(0),
        Optional::No
    );

    doc.entries.emplace_back(
        "GreaterEqualDouble",
        new DoubleGreaterEqualVerifier(0.0),
        Optional::No
    );
    doc.entries.emplace_back(
        "GreaterEqualInt",
        new IntGreaterEqualVerifier(0),
        Optional::No
    );

    doc.entries.emplace_back(
        "EqualBool",
        new BoolEqualVerifier(false),
        Optional::No
    );
    doc.entries.emplace_back(
        "EqualDouble",
        new DoubleEqualVerifier(0.0),
        Optional::No
     );
    doc.entries.emplace_back(
        "EqualInt",
        new IntEqualVerifier(0),
        Optional::No
    );
    doc.entries.emplace_back(
        "EqualString",
        new StringEqualVerifier(""),
        Optional::No
    );

    doc.entries.emplace_back(
        "UnequalBool",
        new BoolUnequalVerifier(false),
        Optional::No
    );
    doc.entries.emplace_back(
        "UnequalDouble",
        new DoubleUnequalVerifier(0.0),
        Optional::No
    );
    doc.entries.emplace_back(
        "UnequalInt",
        new IntUnequalVerifier(0),
        Optional::No
    );
    doc.entries.emplace_back(
        "UnequalString",
        new StringUnequalVerifier(""),
        Optional::No
    );

    // List Verifiers
    doc.entries.emplace_back(
        "InListBool",
        new BoolInListVerifier({ true, false }),
        Optional::No
    );
    doc.entries.emplace_back(
        "InListDouble",
        new DoubleInListVerifier({ 0.0, 1.0}),
        Optional::No
    );
    doc.entries.emplace_back(
        "InListInt",
        new IntInListVerifier({ 0, 1 }),
        Optional::No
    );
    doc.entries.emplace_back(
        "InListString",
        new StringInListVerifier({ "", "a" }),
        Optional::No
    );

    doc.entries.emplace_back(
        "NotInListBool",
        new BoolNotInListVerifier({ true, false }),
        Optional::No
    );
    doc.entries.emplace_back(
        "NotInListDouble",
        new DoubleNotInListVerifier({ 0.0, 1.0 }),
        Optional::No
    );
    doc.entries.emplace_back(
        "NotInListInt",
        new IntNotInListVerifier({ 0, 1 }),
        Optional::No
    );
    doc.entries.emplace_back(
        "NotInListString",
        new StringNotInListVerifier({ "", "a" }),
        Optional::No
    );

    doc.entries.emplace_back(
        "StringListVerifier",
        new StringListVerifier,
        Optional::No
    );
    doc.entries.emplace_back(
        "IntListVerifier",
        new IntListVerifier,
        Optional::No
    );

    // Range Verifiers
    doc.entries.emplace_back(
        "InListDouble",
        new DoubleInRangeVerifier({ 0.0, 1.0 }),
        Optional::No
    );
    doc.entries.emplace_back(
        "InListInt",
        new IntInRangeVerifier({ 0, 1 }),
        Optional::No
    );

    doc.entries.emplace_back(
        "NotInListDouble",
        new DoubleNotInRangeVerifier({ 0.0, 1.0 }),
        Optional::No
    );
    doc.entries.emplace_back(
        "NotInListInt",
        new IntNotInRangeVerifier({ 0, 1 }),
        Optional::No
    );

    // Misc Verifiers
    doc.entries.emplace_back(
        "AnnotationBool",
        new BoolAnnotationVerifier("Bool"),
        Optional::No
    );
    doc.entries.emplace_back(
        "AnnotationDouble",
        new DoubleAnnotationVerifier("Double"),
        Optional::No
    );
    doc.entries.emplace_back(
        "AnnotationInt",
        new IntAnnotationVerifier("Int"),
        Optional::No
    );
    doc.entries.emplace_back(
        "AnnotationString",
        new StringAnnotationVerifier("String"),
        Optional::No
    );
    doc.entries.emplace_back(
        "AnnotationTable",
        new TableAnnotationVerifier("Table"),
        Optional::No
    );
}

TEST_CASE("Documentation: Initializer Constructor", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            // Basic Verifiers
            { "BoolVerifier", new BoolVerifier, Optional::No },
            { "DoubleVerifier", new DoubleVerifier, Optional::No },
            { "IntVerifier", new IntVerifier, Optional::No },
            { "StringVerifier", new StringVerifier, Optional::No },
            { "IdentifierVerifier", new IdentifierVerifier, Optional::No },
            { "FileVerifier", new FileVerifier, Optional::No },
            { "DirectoryVerifier", new DirectoryVerifier, Optional::No },
            { "DateTimeVerifier", new DateTimeVerifier, Optional::No },
            { "TableVerifier", new TableVerifier, Optional::No },

            // Operator Verifiers
            { "LessDouble", new DoubleLessVerifier(0.0), Optional::No },
            { "LessInt", new IntLessVerifier(0), Optional::No },

            { "LessEqualDouble", new DoubleLessEqualVerifier(0.0), Optional::No },
            { "LessEqualInt", new IntLessEqualVerifier(0), Optional::No },

            { "GreaterDouble", new DoubleGreaterVerifier(0.0), Optional::No },
            { "GreaterInt", new IntGreaterVerifier(0), Optional::No },

            { "GreaterEqualDouble", new DoubleGreaterEqualVerifier(0.0), Optional::No },
            { "GreaterEqualInt", new IntGreaterEqualVerifier(0), Optional::No },

            { "EqualBool", new BoolEqualVerifier(false), Optional::No },
            { "EqualDouble", new DoubleEqualVerifier(0.0), Optional::No },
            { "EqualInt", new IntEqualVerifier(0), Optional::No },
            { "EqualString", new StringEqualVerifier(""), Optional::No },

            { "UnequalBool", new BoolUnequalVerifier(false), Optional::No },
            { "UnequalDouble", new DoubleUnequalVerifier(0.0), Optional::No },
            { "UnequalInt", new IntUnequalVerifier(0), Optional::No },
            { "UnequalString", new StringUnequalVerifier(""), Optional::No },

            // List Verifiers
            { "InListBool", new BoolInListVerifier({ true, false }), Optional::No },
            { "InListDouble", new DoubleInListVerifier({ 0.0, 1.0 }), Optional::No },
            { "InListInt", new IntInListVerifier({ 0, 1 }), Optional::No },
            { "InListString", new StringInListVerifier({ "", "a" }), Optional::No },

            { "NotInListBool", new BoolNotInListVerifier({ true, false }), Optional::No },
            { "NotInListDouble", new DoubleNotInListVerifier({ 0.0, 1.0 }), Optional::No },
            { "NotInListInt", new IntNotInListVerifier({ 0, 1 }), Optional::No },
            { "NotInListString", new StringNotInListVerifier({ "", "a" }), Optional::No },

            // Range Verifiers
            { "InRangeDouble", new DoubleInRangeVerifier(0.0, 1.0), Optional::No },
            { "InRangeInt", new IntInRangeVerifier(0, 1), Optional::No },

            { "InRangeDouble", new DoubleNotInRangeVerifier(0.0, 1.0), Optional::No },
            { "InRangeInt", new IntNotInRangeVerifier(0, 1), Optional::No },

            // Misc Verifiers
            { "AnnotationBool", new BoolAnnotationVerifier("Bool"), Optional::No },
            { "AnnotationDouble", new DoubleAnnotationVerifier("Double"), Optional::No },
            { "AnnotationInt", new IntAnnotationVerifier("Int"), Optional::No },
            { "AnnotationString", new StringAnnotationVerifier("String"), Optional::No },
            { "AnnotationTable", new TableAnnotationVerifier("Table"), Optional::No }
        }
    };
}

TEST_CASE("Documentation: BoolVerifier", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Bool", new BoolVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeType;
    negativeType.setValue("Bool", 0);
    TestResult negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("Bool2", 0);
    negativeRes = testSpecification(doc, negativeExist);

    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: DoubleVerifier", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Double", new DoubleVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeType;
    negativeType.setValue("Double", 0);
    TestResult negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("Double2", 0.0);
    negativeRes = testSpecification(doc, negativeExist);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: IntVerifier", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Int", new IntVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 0.0);
    positiveRes = testSpecification(doc, positive2);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeType;
    negativeType.setValue("Int", 0.1);
    TestResult negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("Int2", 0);
    negativeRes = testSpecification(doc, negativeExist);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: StringVerifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "String", new StringVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("String", ""s);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeType;
    negativeType.setValue("String", 0);
    TestResult negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "String");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("String2", ""s);
    negativeRes = testSpecification(doc, negativeExist);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "String");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: IdentifierVerifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "Identifier", new IdentifierVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Identifier", "abcdef"s);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeSpace;
    negativeSpace.setValue("Identifier", "abc def"s);
    TestResult negativeRes = testSpecification(doc, negativeSpace);
    CHECK(!negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Identifier");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negativeTab;
    negativeTab.setValue("Identifier", "abc\tdef"s);
    negativeRes = testSpecification(doc, negativeTab);
    CHECK(!negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Identifier");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negativeNewline;
    negativeNewline.setValue("Identifier", "abc\ndef"s);
    negativeRes = testSpecification(doc, negativeNewline);
    CHECK(!negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Identifier");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negativeCarriageReturn;
    negativeCarriageReturn.setValue("Identifier", "abc\rdef"s);
    negativeRes = testSpecification(doc, negativeCarriageReturn);
    CHECK(!negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Identifier");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negativeDot;
    negativeDot.setValue("Identifier", "abc.def"s);
    negativeRes = testSpecification(doc, negativeDot);
    CHECK(!negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Identifier");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negativeType;
    negativeType.setValue("Identifier", 0);
    negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Identifier");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: FileVerifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "File", new FileVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("File", absPath("${TESTDIR}/verifier/dummyfile.txt").string());
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative404;
    negative404.setValue("File", absPath("${TESTDIR}/verifier/404.txt").string());
    TestResult negativeRes = testSpecification(doc, negative404);
    CHECK(!negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "File");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negativeType;
    negativeType.setValue("File", 0);
    negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "File");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("File2", ""s);
    negativeRes = testSpecification(doc, negativeExist);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "File");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: DirectoryVerifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "Dir", new DirectoryVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Dir", absPath("${TESTDIR}/verifier").string());
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative404;
    negative404.setValue("Dir", absPath("${TESTDIR}/verifier404").string());
    TestResult negativeRes = testSpecification(doc, negative404);
    CHECK(!negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Dir");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negativeType;
    negativeType.setValue("Dir", 0);
    negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Dir");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("Dir2", ""s);
    negativeRes = testSpecification(doc, negativeExist);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Dir");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: DateTimeVerifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "DateTime", new DateTimeVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("DateTime", "1969 07 20 20:17:00"s);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative404;
    negative404.setValue("DateTime", "abc"s);
    TestResult negativeRes = testSpecification(doc, negative404);
    CHECK(!negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "DateTime");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negativeType;
    negativeType.setValue("DateTime", 0);
    negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "DateTime");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("DateTime2", ""s);
    negativeRes = testSpecification(doc, negativeExist);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "DateTime");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: TableVerifierType", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Table", new TableVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Table", ghoul::Dictionary());
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeType;
    negativeType.setValue("Table", 0);
    TestResult negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Table");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("Table2", ghoul::Dictionary());
    negativeRes = testSpecification(doc, negativeExist);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Table");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: StringListVerifierType", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "StringList", new StringListVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", "a"s);
        inner.setValue("2", "b"s);
        inner.setValue("3", "c"s);
        positive.setValue("StringList", inner);
    }
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeType;
    negativeType.setValue("StringList", 0);
    TestResult negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "StringList");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);


    ghoul::Dictionary negative2;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", "a"s);
        inner.setValue("2", "b"s);
        inner.setValue("3", 2.0);
        negative2.setValue("StringList", inner);
    }
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "StringList.3");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("StringList2", ghoul::Dictionary());
    negativeRes = testSpecification(doc, negativeExist);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "StringList");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: IntListVerifierType", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "IntList", new IntListVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", 1);
        inner.setValue("2", 2);
        inner.setValue("3", 3);
        positive.setValue("IntList", inner);
    }
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeType;
    negativeType.setValue("IntList", 0);
    TestResult negativeRes = testSpecification(doc, negativeType);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "IntList");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", "a"s);
        inner.setValue("2", 1);
        inner.setValue("3", 2);
        negative2.setValue("IntList", inner);
    }
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "IntList.1");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("IntList2", ghoul::Dictionary());
    negativeRes = testSpecification(doc, negativeExist);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "IntList");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: MixedVerifiers", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "Bool", new BoolVerifier, Optional::No },
            { "Double", new DoubleVerifier, Optional::No },
            { "Int", new IntVerifier, Optional::No },
            { "String", new StringVerifier, Optional::No },
            { "Table", new TableVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);
    positive.setValue("Double", 0.0);
    positive.setValue("Int", 0);
    positive.setValue("String", ""s);
    positive.setValue("Table", ghoul::Dictionary());
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeType1;
    negativeType1.setValue("Bool", true);
    negativeType1.setValue("Double", 1);
    negativeType1.setValue("Int", 0);
    negativeType1.setValue("String", ""s);
    negativeType1.setValue("Table", ghoul::Dictionary());
    TestResult negativeRes = testSpecification(doc, negativeType1);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeType2;
    negativeType2.setValue("Bool", true);
    negativeType2.setValue("Double", 0.0);
    negativeType2.setValue("Int", ""s);
    negativeType2.setValue("String", 1);
    negativeType2.setValue("Table", ghoul::Dictionary());
    negativeRes = testSpecification(doc, negativeType2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "String");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: NestedTables", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "Outer_Int", new IntVerifier, Optional::No },
            { "Outer_Table", new TableVerifier({
                { "Inner_Double", new DoubleVerifier, Optional::No },
                { "Inner_String", new StringVerifier, Optional::No }
            }), Optional::No },
            { "Outer_Double", new DoubleVerifier, Optional::No },
            { "Outer_Table2" , new TableVerifier({
                { "Inner_Double2", new DoubleVerifier, Optional::No },
                { "Inner_String2", new StringVerifier, Optional::No },
                { "Inner_Table" , new TableVerifier({
                    { "Inner_Inner_Int", new IntVerifier, Optional::No }
                }), Optional::No }
            }), Optional::No}
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Outer_Int", 1);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double", 0.0);
        inner.setValue("Inner_String", ""s);
        positive.setValue("Outer_Table", inner);
    }
    positive.setValue("Outer_Double", 0.0);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double2", 0.0);
        inner.setValue("Inner_String2", ""s);
        {
            ghoul::Dictionary innerInner;
            innerInner.setValue("Inner_Inner_Int", 0);
            inner.setValue("Inner_Table", innerInner);
        }
        positive.setValue("Outer_Table2", inner);
    }
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negativeSimple;
    negativeSimple.setValue("Outer_Int", 1);
    negativeSimple.setValue("Outer_Table", 0);
    negativeSimple.setValue("Outer_Double", 0.0);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double2", 0.0);
        inner.setValue("Inner_String2", ""s);
        {
            ghoul::Dictionary innerInner;
            innerInner.setValue("Inner_Inner_Int", 0);
            inner.setValue("Inner_Table", innerInner);
        }
        negativeSimple.setValue("Outer_Table2", inner);
    }
    TestResult negativeRes = testSpecification(doc, negativeSimple);
    CHECK_FALSE(negativeRes.success);
    CHECK(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Outer_Table");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeInner;
    negativeInner.setValue("Outer_Int", 1);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double", ""s);
        inner.setValue("Inner_String", ""s);
        negativeInner.setValue("Outer_Table", inner);
    }
    negativeInner.setValue("Outer_Double", 0.0);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double2", 0.0);
        inner.setValue("Inner_String2", ""s);
        {
            ghoul::Dictionary innerInner;
            innerInner.setValue("Inner_Inner_Int", 0);
            inner.setValue("Inner_Table", innerInner);
        }
        negativeInner.setValue("Outer_Table2", inner);
    }
    negativeRes = testSpecification(doc, negativeInner);
    CHECK_FALSE(negativeRes.success);
    CHECK(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeInner2;
    negativeInner2.setValue("Outer_Int", 1);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double", ""s);
        inner.setValue("Inner_String", 0.0);
        negativeInner2.setValue("Outer_Table", inner);
    }
    negativeInner2.setValue("Outer_Double", 0.0);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double2", 0.0);
        inner.setValue("Inner_String2", ""s);
        {
            ghoul::Dictionary innerInner;
            innerInner.setValue("Inner_Inner_Int", 0);
            inner.setValue("Inner_Table", innerInner);
        }
        negativeInner2.setValue("Outer_Table2", inner);
    }
    negativeRes = testSpecification(doc, negativeInner2);
    CHECK_FALSE(negativeRes.success);
    CHECK(negativeRes.offenses.size() == 2);
    CHECK(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "Outer_Table.Inner_String");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeInnerSeparate;
    negativeInnerSeparate.setValue("Outer_Int", 1);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double", ""s);
        inner.setValue("Inner_String", ""s);
        negativeInnerSeparate.setValue("Outer_Table", inner);
    }
    negativeInnerSeparate.setValue("Outer_Double", 0.0);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double2", ""s);
        inner.setValue("Inner_String2", ""s);
        {
            ghoul::Dictionary innerInner;
            innerInner.setValue("Inner_Inner_Int", 0);
            inner.setValue("Inner_Table", innerInner);
        }
        negativeInnerSeparate.setValue("Outer_Table2", inner);
    }
    negativeRes = testSpecification(doc, negativeInnerSeparate);
    CHECK_FALSE(negativeRes.success);
    CHECK(negativeRes.offenses.size() == 2);
    CHECK(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "Outer_Table2.Inner_Double2");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeInnerFull;
    negativeInnerFull.setValue("Outer_Int", 1);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double", ""s);
        inner.setValue("Inner_String", ""s);
        negativeInnerFull.setValue("Outer_Table", inner);
    }
    negativeInnerFull.setValue("Outer_Double", 0.0);
    {
        ghoul::Dictionary inner;
        inner.setValue("Inner_Double2", ""s);
        inner.setValue("Inner_String2", ""s);
        {
            ghoul::Dictionary innerInner;
            innerInner.setValue("Inner_Inner_Int", ""s);
            inner.setValue("Inner_Table", innerInner);
        }
        negativeInnerFull.setValue("Outer_Table2", inner);
    }
    negativeRes = testSpecification(doc, negativeInnerFull);
    CHECK_FALSE(negativeRes.success);
    CHECK(negativeRes.offenses.size() == 3);
    CHECK(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "Outer_Table2.Inner_Double2");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[2].offender == "Outer_Table2.Inner_Table.Inner_Inner_Int");
    CHECK(negativeRes.offenses[2].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Optional", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Bool_Force", new BoolVerifier, Optional::No },
            { "Bool_Optional", new BoolVerifier, Optional::Yes }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Bool_Force", true);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Bool_Force", true);
    positive2.setValue("Bool_Optional", true);
    positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    const ghoul::Dictionary negative;
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool_Force");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);

    ghoul::Dictionary negative2;
    negative2.setValue("Bool_Optional", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool_Force");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);

    ghoul::Dictionary negative3;
    negative3.setValue("Bool_Force", true);
    negative3.setValue("Bool_Optional", 1);
    negativeRes = testSpecification(doc, negative3);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool_Optional");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Required In Optional", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            {
                "a",
                new TableVerifier({
                    {
                        "b",
                        new IntVerifier,
                        Optional::No
                    },
                    {
                        "c",
                        new IntVerifier,
                        Optional::Yes
                    }
                }),
                Optional::Yes
            }
        }
    };

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("b", 1);
        positive.setValue("a", inner);
    }
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    {
        ghoul::Dictionary inner;
        inner.setValue("b", 1);
        inner.setValue("c", 2);
        positive2.setValue("a", inner);
    }
    positiveRes = testSpecification(doc, positive2);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    const ghoul::Dictionary positive3;
    positiveRes = testSpecification(doc, positive3);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("c", 2);
        negative.setValue("a", inner);
    }
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a.b");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: Exhaustive", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Int", new IntVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 1);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("False_Int", 1);
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);

    ghoul::Dictionary negative2;
    negative2.setValue("Double", 2.0);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: Nested Exhaustive", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            {
                "Table",
                new TableVerifier({ { "a", new IntVerifier, Optional::No } }),
                Optional::No
            }
        }
    };

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        positive.setValue("Table", inner);
    }
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("b", 2.0);
        negative.setValue("Table", inner);
    }
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Table.a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: Empty Entries Non Exhaustive", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc;

    const ghoul::Dictionary positive {};
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("a", 1);
    positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());
}

TEST_CASE("Documentation: Empty Nested Exhaustive", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Table", new TableVerifier(), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Table", ghoul::Dictionary());
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        negative.setValue("Table", inner);
    }
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK(negativeRes.success);
    CHECK(negativeRes.offenses.empty());
}

TEST_CASE("Documentation: Less Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Int", new IntLessVerifier(5), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 0.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 10.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Less Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Double", new DoubleLessVerifier(5.0), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 10.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: LessEqual Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int", new IntLessEqualVerifier(5), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 0.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual;
    positiveEqual.setValue("Int", 5.0);
    positiveRes = testSpecification(doc, positiveEqual);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 10.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: LessEqual Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Double", new DoubleLessEqualVerifier(5.0), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual;
    positiveEqual.setValue("Double", 5.0);
    positiveRes = testSpecification(doc, positiveEqual);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 10.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Greater Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int", new IntGreaterVerifier(5), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 10.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 0.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Greater Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Double", new DoubleGreaterVerifier(5.0), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 10.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 0.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: GreaterEqual Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int", new IntGreaterEqualVerifier(5), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 10.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual;
    positiveEqual.setValue("Int", 5.0);
    positiveRes = testSpecification(doc, positiveEqual);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 0.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: GreaterEqual Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Double", new DoubleGreaterEqualVerifier(5.0), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 10.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual;
    positiveEqual.setValue("Double", 5.0);
    positiveRes = testSpecification(doc, positiveEqual);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 0.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Equal Bool", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Bool", new BoolEqualVerifier(true), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", false);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Equal Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int", new IntEqualVerifier(1), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 1.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 0.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Equal Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Double", new DoubleEqualVerifier(1.0), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 1.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 0.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Equal String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = { { "String", new StringEqualVerifier("string"s), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("String", "string"s);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", "no_string"s);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "String");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Unequal Bool", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Bool", new BoolUnequalVerifier(true), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Bool", false);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", true);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Unequal Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int", new IntUnequalVerifier(1), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 0.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 1.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Unequal Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Double", new DoubleUnequalVerifier(1.0), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 1.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Unequal String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = { { "String", new StringUnequalVerifier("string"s), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("String", "no_string"s);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", "string"s);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "String");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: List Bool", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Bool" , new BoolInListVerifier({ true }), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", false);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: List Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int" , new IntInListVerifier({ 0, 1, 2 }), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 2.0);
    positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 5.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: List Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Double" , new DoubleInListVerifier({ 0.0, 1.0, 2.0 }), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Double", 2.0);
    positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 5.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: List String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "String" , new StringInListVerifier({ "0"s, "1"s, "2"s }), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("String", "1"s);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("String", "2"s);
    positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", "5"s);
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "String");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotList Bool", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Bool" , new BoolNotInListVerifier({ true }), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Bool", false);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", true);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotList Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int" , new IntNotInListVerifier({ 0, 1, 2 }), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", -1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 3.0);
    positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 2.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotList Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Double" , new DoubleNotInListVerifier({ 0.0, 1.0, 2.0 }), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", -1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Double", 3.0);
    positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 1.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotList String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "String" , new StringNotInListVerifier({ "0"s, "1"s, "2"s }), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("String", "string"s);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("String", "foo_string"s);
    positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", "1"s);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "String");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Annotation Bool", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Bool", new BoolAnnotationVerifier("Bool"), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", 0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Bool");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Annotation Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int", new IntAnnotationVerifier("Int"), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 1.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 1.1);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Annotation Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Double", new DoubleAnnotationVerifier("Double"), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", true);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Annotation String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = { { "String", new StringAnnotationVerifier("String"), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("String", ""s);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", 1);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "String");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Annotation Table", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Table", new TableAnnotationVerifier("Table"), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Table", ghoul::Dictionary());
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Table", 1);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Table");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: InRange Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int", new InRangeVerifier<IntVerifier>(0, 5), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 2.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 0.0);
    positiveRes = testSpecification(doc, positive2);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive3;
    positive3.setValue("Int", 5.0);
    positiveRes = testSpecification(doc, positive3);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 10.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: InRange Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Double", new InRangeVerifier<DoubleVerifier>(0.0, 5.0), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", 2.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Double", 0.0);
    positiveRes = testSpecification(doc, positive2);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive3;
    positive3.setValue("Double", 5.0);
    positiveRes = testSpecification(doc, positive3);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive4;
    positive4.setValue("Double", 1.5);
    positiveRes = testSpecification(doc, positive4);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 10.0);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotInRange Int", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { "Int", new NotInRangeVerifier<IntVerifier>(0, 5), Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", -1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 6.0);
    positiveRes = testSpecification(doc, positive2);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 2.0);
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2;
    negative2.setValue("Int", 0.0);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative3;
    negative3.setValue("Int", 5.0);
    negativeRes = testSpecification(doc, negative3);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Int");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotInRange Double", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "Double", new NotInRangeVerifier<DoubleVerifier>(0.0, 5.0), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("Double", -1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Double", 6.0);
    positiveRes = testSpecification(doc, positive2);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 0.0);
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2;
    negative2.setValue("Double", 5.0);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative3;
    negative3.setValue("Double", 2.5);
    negativeRes = testSpecification(doc, negative3);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Double");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Wildcard", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = { { DocumentationEntry::Wildcard, new IntVerifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", 1);
    positive.setValue("b", 2);
    positive.setValue("c", 3);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("a", false);
    negative.setValue("b", 2);
    negative.setValue("c", 3);
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", false);
    negative2.setValue("b", false);
    negative2.setValue("c", 3);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "b");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative3;
    negative3.setValue("a", false);
    negative3.setValue("b", false);
    negative3.setValue("c", false);
    negativeRes = testSpecification(doc, negative3);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 3);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "b");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[2].offender == "c");
    CHECK(negativeRes.offenses[2].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Wildcard Mixed", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { DocumentationEntry::Wildcard, new IntVerifier, Optional::No },
            { "b", new IntGreaterVerifier(5), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", 1.0);
    positive.setValue("b", 8.0);
    positive.setValue("c", 3.0);
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("a", false);
    negative.setValue("b", 2.0);
    negative.setValue("c", 3.0);
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "b");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2;
    negative2.setValue("a", false);
    negative2.setValue("b", false);
    negative2.setValue("c", 3.0);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "b");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative3;
    negative3.setValue("a", false);
    negative3.setValue("b", 1.0);
    negative3.setValue("c", false);
    negativeRes = testSpecification(doc, negative3);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 3);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "b");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::Verification);
    CHECK(negativeRes.offenses[2].offender == "c");
    CHECK(negativeRes.offenses[2].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative4;
    negative4.setValue("a", false);
    negative4.setValue("b", 10.0);
    negative4.setValue("c", false);
    negativeRes = testSpecification(doc, negative4);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    CHECK(negativeRes.offenses[1].offender == "c");
    CHECK(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Referencing", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation referenced = {
        "Referenced Name",
        "referenced_id",
        "",
        {
            { "a", new IntVerifier, Optional::No },
            { "b", new DoubleVerifier, Optional::No }
        },
    };
    DocEng.addDocumentation(referenced);

    const Documentation doc = {
        .entries = {
            { "Table", new ReferencingVerifier("referenced_id"), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        inner.setValue("b", 2.0);
        positive.setValue("Table", inner);
    };
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Table", 1);
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Table");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        inner.setValue("b", true);
        negative2.setValue("Table", inner);
    };
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Table.b");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);


    const Documentation wrongDoc = {
        .entries = {
            { "Table", new ReferencingVerifier("WRONG"), Optional::No }
        }
    };
    ghoul::Dictionary wrongNegative;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        inner.setValue("b", 2.0);
        wrongNegative.setValue("Table", inner);
    }
    negativeRes = testSpecification(wrongDoc, wrongNegative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "Table");
    CHECK(
        negativeRes.offenses[0].reason == TestResult::Offense::Reason::UnknownIdentifier
    );
}

TEST_CASE("Documentation: OrOperator", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new OrVerifier({ new StringVerifier, new IntVerifier }), Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", ""s);
    TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("a", 1);
    positiveRes = testSpecification(doc, positive2);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("a", false);
    const TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: IntVector2Verifier", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "a", new IntVector2Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::ivec2(2));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleVector2Verifier", "[documentation]") {
    using namespace openspace::documentation;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleVector2Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dvec2(2.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: IntVector3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new IntVector3Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::ivec3(2));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleVector3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleVector3Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dvec3(2.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: IntVector4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new IntVector4Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::ivec4(2));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1);
        inner.setValue("3", "s"s);
        inner.setValue("4", 1);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleVector4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleVector4Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dvec4(2.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        inner.setValue("4", 1);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix2x2Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleMatrix2x2Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat2x2(1.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix2x3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleMatrix2x3Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat2x3(1.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    }
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix2x4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleMatrix2x4Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat2x4(1.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix3x2Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleMatrix3x2Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat3x2(1.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix3x3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleMatrix3x3Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat3x3(1.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix3x4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleMatrix3x4Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat3x4(1.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix4x2Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleMatrix4x2Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat4x2(1.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix4x3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleMatrix4x3Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat4x3(1.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix4x4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    const Documentation doc = {
        .entries = {
            { "a", new DoubleMatrix4x4Verifier, Optional::No }
        }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat4x4(1.0));
    const TestResult positiveRes = testSpecification(doc, positive);
    CHECK(positiveRes.success);
    CHECK(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    CHECK_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    CHECK(negativeRes.offenses[0].offender == "a");
    CHECK(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Verifier Type Post Conditions", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    CHECK(!BoolVerifier().type().empty());
    CHECK(!DoubleVerifier().type().empty());
    CHECK(!IntVerifier().type().empty());
    CHECK(!StringVerifier().type().empty());
    CHECK(!TableVerifier().type().empty());

    CHECK(!IntVector2Verifier().type().empty());
    CHECK(!DoubleVector2Verifier().type().empty());
    CHECK(!IntVector3Verifier().type().empty());
    CHECK(!DoubleVector3Verifier().type().empty());
    CHECK(!IntVector4Verifier().type().empty());
    CHECK(!DoubleVector4Verifier().type().empty());

    CHECK(!IntLessVerifier(0).type().empty());
    CHECK(!DoubleLessVerifier(0.0).type().empty());
    CHECK(!IntLessEqualVerifier(0).type().empty());
    CHECK(!DoubleLessEqualVerifier(0.0).type().empty());
    CHECK(!IntGreaterVerifier(0).type().empty());
    CHECK(!DoubleGreaterVerifier(0.0).type().empty());
    CHECK(!IntGreaterEqualVerifier(0).type().empty());
    CHECK(!DoubleGreaterEqualVerifier(0.0).type().empty());

    CHECK(!BoolEqualVerifier(true).type().empty());
    CHECK(!IntEqualVerifier(0).type().empty());
    CHECK(!DoubleEqualVerifier(0.0).type().empty());
    CHECK(!StringEqualVerifier(""s).type().empty());
    CHECK(!BoolUnequalVerifier(true).type().empty());
    CHECK(!IntUnequalVerifier(0).type().empty());
    CHECK(!DoubleUnequalVerifier(0.0).type().empty());
    CHECK(!StringUnequalVerifier(""s).type().empty());

    CHECK(!BoolInListVerifier({ true }).type().empty());
    CHECK(!IntInListVerifier({ 0 }).type().empty());
    CHECK(!DoubleInListVerifier({ 0.0 }).type().empty());
    CHECK(!StringInListVerifier({ ""s }).type().empty());
    CHECK(!BoolNotInListVerifier({ true }).type().empty());
    CHECK(!IntNotInListVerifier({ 0 }).type().empty());
    CHECK(!DoubleNotInListVerifier({ 0.0 }).type().empty());
    CHECK(!StringNotInListVerifier({ ""s }).type().empty());

    CHECK(!IntInRangeVerifier({ 0, 1 }).type().empty());
    CHECK(!DoubleInRangeVerifier({ 0.0, 1.0 }).type().empty());
    CHECK(!IntNotInRangeVerifier({ 0, 1 }).type().empty());
    CHECK(!DoubleNotInRangeVerifier({ 0.0, 1.0 }).type().empty());

    CHECK(!BoolAnnotationVerifier("A"s).type().empty());
    CHECK(!IntAnnotationVerifier("A"s).type().empty());
    CHECK(!DoubleAnnotationVerifier("A"s).type().empty());
    CHECK(!StringAnnotationVerifier("A"s).type().empty());
    CHECK(!TableAnnotationVerifier("A"s).type().empty());
    CHECK(!AnnotationVerifier<IntVector2Verifier>("A"s).type().empty());
    CHECK(!AnnotationVerifier<DoubleVector2Verifier>("A"s).type().empty());
    CHECK(!AnnotationVerifier<IntVector3Verifier>("A"s).type().empty());
    CHECK(!AnnotationVerifier<DoubleVector3Verifier>("A"s).type().empty());
    CHECK(!AnnotationVerifier<IntVector4Verifier>("A"s).type().empty());
    CHECK(!AnnotationVerifier<DoubleVector4Verifier>("A"s).type().empty());

    CHECK(!ReferencingVerifier("identifier"s).type().empty());
}

TEST_CASE("Documentation: Verifier Documentation Post Conditions", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    CHECK(!BoolVerifier().documentation().empty());
    CHECK(!DoubleVerifier().documentation().empty());
    CHECK(!IntVerifier().documentation().empty());
    CHECK(!StringVerifier().documentation().empty());
    CHECK(!TableVerifier().documentation().empty());

    CHECK(!IntVector2Verifier().documentation().empty());
    CHECK(!DoubleVector2Verifier().documentation().empty());
    CHECK(!IntVector3Verifier().documentation().empty());
    CHECK(!DoubleVector3Verifier().documentation().empty());
    CHECK(!IntVector4Verifier().documentation().empty());
    CHECK(!DoubleVector4Verifier().documentation().empty());

    CHECK(!IntLessVerifier(0).documentation().empty());
    CHECK(!DoubleLessVerifier(0.0).documentation().empty());
    CHECK(!IntLessEqualVerifier(0).documentation().empty());
    CHECK(!DoubleLessEqualVerifier(0.0).documentation().empty());
    CHECK(!IntGreaterVerifier(0).documentation().empty());
    CHECK(!DoubleGreaterVerifier(0.0).documentation().empty());
    CHECK(!IntGreaterEqualVerifier(0).documentation().empty());
    CHECK(!DoubleGreaterEqualVerifier(0.0).documentation().empty());

    CHECK(!BoolEqualVerifier(true).documentation().empty());
    CHECK(!IntEqualVerifier(0).documentation().empty());
    CHECK(!DoubleEqualVerifier(0.0).documentation().empty());
    CHECK(!StringEqualVerifier(""s).documentation().empty());
    CHECK(!BoolUnequalVerifier(true).documentation().empty());
    CHECK(!IntUnequalVerifier(0).documentation().empty());
    CHECK(!DoubleUnequalVerifier(0.0).documentation().empty());
    CHECK(!StringUnequalVerifier(""s).documentation().empty());

    CHECK(!BoolInListVerifier({ true }).documentation().empty());
    CHECK(!IntInListVerifier({ 0 }).documentation().empty());
    CHECK(!DoubleInListVerifier({ 0.0 }).documentation().empty());
    CHECK(!StringInListVerifier({ ""s }).documentation().empty());
    CHECK(!BoolNotInListVerifier({ true }).documentation().empty());
    CHECK(!IntNotInListVerifier({ 0 }).documentation().empty());
    CHECK(!DoubleNotInListVerifier({ 0.0 }).documentation().empty());
    CHECK(!StringNotInListVerifier({ ""s }).documentation().empty());

    CHECK(!IntInRangeVerifier({ 0, 1 }).documentation().empty());
    CHECK(!DoubleInRangeVerifier({ 0.0, 1.0 }).documentation().empty());
    CHECK(!IntNotInRangeVerifier({ 0, 1 }).documentation().empty());
    CHECK(!DoubleNotInRangeVerifier({ 0.0, 1.0 }).documentation().empty());

    CHECK(!BoolAnnotationVerifier("A"s).documentation().empty());
    CHECK(!IntAnnotationVerifier("A"s).documentation().empty());
    CHECK(!DoubleAnnotationVerifier("A"s).documentation().empty());
    CHECK(!StringAnnotationVerifier("A"s).documentation().empty());
    CHECK(!TableAnnotationVerifier("A"s).documentation().empty());
    CHECK(!AnnotationVerifier<IntVector2Verifier>("A"s).documentation().empty());
    CHECK(!AnnotationVerifier<DoubleVector2Verifier>("A"s).documentation().empty());
    CHECK(!AnnotationVerifier<IntVector3Verifier>("A"s).documentation().empty());
    CHECK(!AnnotationVerifier<DoubleVector3Verifier>("A"s).documentation().empty());
    CHECK(!AnnotationVerifier<IntVector4Verifier>("A"s).documentation().empty());
    CHECK(!AnnotationVerifier<DoubleVector4Verifier>("A"s).documentation().empty());

    CHECK(!ReferencingVerifier("identifier"s).documentation().empty());
}
