/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2020                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/documentation/verifier.h>
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
    
    Documentation doc {
        {
            // Basic Verifiers
            {"BoolVerifier", new BoolVerifier, Optional::No },
            {"DoubleVerifier", new DoubleVerifier, Optional::No },
            {"IntVerifier", new IntVerifier, Optional::No },
            {"StringVerifier", new StringVerifier, Optional::No },
            {"TableVerifier", new TableVerifier, Optional::No },

            // Operator Verifiers
            { "LessDouble", new DoubleLessVerifier(0.0), Optional::No },
            { "LessInt", new IntLessVerifier(0), Optional::No },

            {"LessEqualDouble", new DoubleLessEqualVerifier(0.0), Optional::No },
            {"LessEqualInt", new IntLessEqualVerifier(0), Optional::No },

            {"GreaterDouble", new DoubleGreaterVerifier(0.0), Optional::No },
            {"GreaterInt", new IntGreaterVerifier(0), Optional::No },

            {"GreaterEqualDouble", new DoubleGreaterEqualVerifier(0.0), Optional::No },
            {"GreaterEqualInt", new IntGreaterEqualVerifier(0), Optional::No },

            {"EqualBool", new BoolEqualVerifier(false), Optional::No },
            {"EqualDouble", new DoubleEqualVerifier(0.0), Optional::No },
            {"EqualInt", new IntEqualVerifier(0), Optional::No },
            {"EqualString", new StringEqualVerifier(""), Optional::No },

            {"UnequalBool", new BoolUnequalVerifier(false), Optional::No },
            {"UnequalDouble", new DoubleUnequalVerifier(0.0), Optional::No },
            {"UnequalInt", new IntUnequalVerifier(0), Optional::No },
            {"UnequalString", new StringUnequalVerifier(""), Optional::No },

            // List Verifiers
            {"InListBool", new BoolInListVerifier({ true, false }), Optional::No },
            {"InListDouble", new DoubleInListVerifier({ 0.0, 1.0 }), Optional::No },
            {"InListInt", new IntInListVerifier({ 0, 1 }), Optional::No },
            {"InListString", new StringInListVerifier({ "", "a" }), Optional::No },

            {"NotInListBool", new BoolNotInListVerifier({ true, false }), Optional::No },
            {"NotInListDouble", new DoubleNotInListVerifier({ 0.0, 1.0 }), Optional::No },
            {"NotInListInt", new IntNotInListVerifier({ 0, 1 }), Optional::No },
            {"NotInListString", new StringNotInListVerifier({ "", "a" }), Optional::No },

            // Range Verifiers
            {"InRangeDouble", new DoubleInRangeVerifier(0.0, 1.0), Optional::No },
            {"InRangeInt", new IntInRangeVerifier(0, 1), Optional::No },

            {"InRangeDouble", new DoubleNotInRangeVerifier(0.0, 1.0), Optional::No },
            {"InRangeInt", new IntNotInRangeVerifier(0, 1), Optional::No },

            // Misc Verifiers
            {"AnnotationBool", new BoolAnnotationVerifier("Bool"), Optional::No },
            {"AnnotationDouble", new DoubleAnnotationVerifier("Double"), Optional::No },
            {"AnnotationInt", new IntAnnotationVerifier("Int"), Optional::No },
            {"AnnotationString", new StringAnnotationVerifier("String"), Optional::No },
            {"AnnotationTable", new TableAnnotationVerifier("Table"), Optional::No }
        }
    };
}

TEST_CASE("Documentation: BoolVerifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Bool", new BoolVerifier, Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Bool", true }
    };

    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Bool", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist {
        { "Bool2", 0 }
    };
    negativeRes = testSpecification(doc, negativeExist);

    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: DoubleVerifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new DoubleVerifier, Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };

    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 0 }
    };

    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist{
        { "Double2" , 0.0 }
    };
    negativeRes = testSpecification(doc, negativeExist);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: IntVerifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntVerifier, Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Int", 0.0 }
    };
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 0.1 }
    };

    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist {
        { "Int2", 0 }
    };
    negativeRes = testSpecification(doc, negativeExist);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: StringVerifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        {{ "String", new StringVerifier, Optional::No }}
    };

    ghoul::Dictionary positive {
        { "String", ""s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "String", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "String");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist {
        { "String2", ""s }
    };
    negativeRes = testSpecification(doc, negativeExist);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "String");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: TableVerifierType", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Table", new TableVerifier, Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Table", ghoul::Dictionary{} }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Table", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist {
        { "Table2", ghoul::Dictionary{} }
    };
    negativeRes = testSpecification(doc, negativeExist);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: StringListVerifierType", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "StringList", new StringListVerifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        {
            "StringList",
            ghoul::Dictionary {
                { "1", "a"s },
                { "2", "b"s },
                { "3", "c"s }
            }
        }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "StringList", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "StringList");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        {
            "StringList",
            ghoul::Dictionary {
                { "1", "a"s },
                { "2", "b"s },
                { "3", 2.0 }
            }
        }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "StringList.3");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist {
        { "StringList2", ghoul::Dictionary{} }
    };
    negativeRes = testSpecification(doc, negativeExist);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "StringList");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: IntListVerifierType", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "IntList", new IntListVerifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        {
            "IntList",
            ghoul::Dictionary{
                { "1", 1 },
                { "2", 2 },
                { "3", 3 }
    }
        }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative{
        { "IntList", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "IntList");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        {
            "IntList",
            ghoul::Dictionary{
                { "1", "a"s },
                { "2", 1 },
                { "3", 2 }
    }
        }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "IntList.1");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist {
        { "IntList2", ghoul::Dictionary{} }
    };
    negativeRes = testSpecification(doc, negativeExist);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "IntList");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: MixedVerifiers", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        {
            { "Bool", new BoolVerifier, Optional::No },
            { "Double", new DoubleVerifier, Optional::No },
            { "Int", new IntVerifier, Optional::No },
            { "String", new StringVerifier, Optional::No },
            { "Table", new TableVerifier, Optional::No }
        }
    };

    ghoul::Dictionary positive {
        { "Bool", true },
        { "Double", 0.0 },
        { "Int", 0 },
        { "String", ""s },
        { "Table", ghoul::Dictionary{} }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative1 {
        { "Bool", true },
        { "Double", 1 },
        { "Int", 0 },
        { "String", ""s },
        { "Table", ghoul::Dictionary{} }
    };
    TestResult negativeRes = testSpecification(doc, negative1);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "Bool", true },
        { "Double", 0.0 },
        { "Int", ""s },
        { "String", 1 },
        { "Table", ghoul::Dictionary{} }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "String");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: NestedTables", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        {
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

    ghoul::Dictionary positive {
        { "Outer_Int", 1 },
        { "Outer_Table", ghoul::Dictionary {
            { "Inner_Double", 0.0 },
            { "Inner_String", ""s }
        }},
        { "Outer_Double", 0.0 },
        { "Outer_Table2", ghoul::Dictionary {
            { "Inner_Double2", 0.0 },
            { "Inner_String2", ""s },
            { "Inner_Table", ghoul::Dictionary {
                { "Inner_Inner_Int", 0 }
            }}
        }}
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negativeSimple {
        { "Outer_Int", 1 },
        { "Outer_Table", 0},
        { "Outer_Double", 0.0 },
        { "Outer_Table2", ghoul::Dictionary {
            { "Inner_Double2", 0.0 },
            { "Inner_String2", ""s },
            { "Inner_Table", ghoul::Dictionary {
                { "Inner_Inner_Int", 0 }
            }}
        }}
    };
    TestResult negativeRes = testSpecification(doc, negativeSimple);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Outer_Table");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeInner {
        { "Outer_Int", 1 },
        { "Outer_Table", ghoul::Dictionary {
            { "Inner_Double", ""s },
            { "Inner_String", ""s }
        }},
        { "Outer_Double", 0.0 },
        { "Outer_Table2", ghoul::Dictionary {
            { "Inner_Double2", 0.0 },
            { "Inner_String2", ""s },
            { "Inner_Table", ghoul::Dictionary {
                { "Inner_Inner_Int", 0 }
            }}
        }}
    };
    negativeRes = testSpecification(doc, negativeInner);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeInner2 {
        { "Outer_Int", 1 },
        { "Outer_Table", ghoul::Dictionary {
            { "Inner_Double", ""s },
            { "Inner_String", 0.0 }
        }},
        { "Outer_Double", 0.0 },
        { "Outer_Table2", ghoul::Dictionary {
            { "Inner_Double2", 0.0 },
            { "Inner_String2", ""s },
            { "Inner_Table", ghoul::Dictionary {
                { "Inner_Inner_Int", 0 }
            }}
        }}
    };
    negativeRes = testSpecification(doc, negativeInner2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "Outer_Table.Inner_String");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeInnerSeparate {
        { "Outer_Int", 1 },
        { "Outer_Table", ghoul::Dictionary {
            { "Inner_Double", ""s },
            { "Inner_String", ""s }
        } },
        { "Outer_Double", 0.0 },
        { "Outer_Table2", ghoul::Dictionary {
            { "Inner_Double2", ""s },
            { "Inner_String2", ""s },
            { "Inner_Table", ghoul::Dictionary {
                { "Inner_Inner_Int", 0 }
            }}
        }}
    };
    negativeRes = testSpecification(doc, negativeInnerSeparate);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "Outer_Table2.Inner_Double2");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeInnerFull {
        { "Outer_Int", 1 },
        { "Outer_Table", ghoul::Dictionary {
            { "Inner_Double", ""s },
            { "Inner_String", ""s }
        } },
        { "Outer_Double", 0.0 },
        { "Outer_Table2", ghoul::Dictionary {
            { "Inner_Double2", ""s },
            { "Inner_String2", ""s },
            { "Inner_Table", ghoul::Dictionary {
                { "Inner_Inner_Int", ""s }
            }}
        }}
    };
    negativeRes = testSpecification(doc, negativeInnerFull);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 3);
    REQUIRE(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "Outer_Table2.Inner_Double2");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(
        negativeRes.offenses[2].offender == "Outer_Table2.Inner_Table.Inner_Inner_Int"
    );
    REQUIRE(negativeRes.offenses[2].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Optional", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {
            { "Bool_Force", new BoolVerifier, Optional::No },
            { "Bool_Optional", new BoolVerifier, Optional::Yes }
        }
    };

    ghoul::Dictionary positive {
        { "Bool_Force", true }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Bool_Force", true },
        { "Bool_Optional", true }
    };
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());
    
    ghoul::Dictionary negative {
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool_Force");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);

    ghoul::Dictionary negative2 {
        { "Bool_Optional", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool_Force");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);

    ghoul::Dictionary negative3 {
        { "Bool_Force", true },
        { "Bool_Optional", 1 }
    };
    negativeRes = testSpecification(doc, negative3);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool_Optional");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Required In Optional", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{
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
        }}
    };

    ghoul::Dictionary positive {
        {
            "a", ghoul::Dictionary{
                { "b", 1 }
            }
        }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        {
            "a", ghoul::Dictionary{
                { "b", 1 },
                { "c", 2 }
        }
        }
    };
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive3 {};
    positiveRes = testSpecification(doc, positive3);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "c", 2 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a.b");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: Exhaustive", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntVerifier, Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int" , 1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "False_Int", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);

    ghoul::Dictionary negative2 {
        { "Double", 2.0 }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: Nested Exhaustive", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Table", new TableVerifier(
            { { "a", new IntVerifier, Optional::No } }
        ), Optional::No
        }}
    };

    ghoul::Dictionary positive {
        { "Table", ghoul::Dictionary{{ "a", 1 }}}
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Table", ghoul::Dictionary{{ "b", 2.0 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table.a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);
}

TEST_CASE("Documentation: Empty Entries Non Exhaustive", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc;

    ghoul::Dictionary positive {};
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "a", 1 }
    };
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());
}

TEST_CASE("Documentation: Empty Nested Exhaustive", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{
            "Table",
            new TableVerifier(),
            Optional::No,
        }}
    };

    ghoul::Dictionary positive {
        { "Table", ghoul::Dictionary() }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Table", ghoul::Dictionary{ { "a", 1 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE(negativeRes.success);
    REQUIRE(negativeRes.offenses.empty());
}

TEST_CASE("Documentation: Less Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntLessVerifier(5), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 10 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Less Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new DoubleLessVerifier(5.0), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 10.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: LessEqual Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntLessEqualVerifier(5), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual {
        { "Int", 5 }
    };
    positiveRes = testSpecification(doc, positiveEqual);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 10 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: LessEqual Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new DoubleLessEqualVerifier(5.0), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual {
        { "Double", 5.0 }
    };
    positiveRes = testSpecification(doc, positiveEqual);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 10.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Greater Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntGreaterVerifier(5), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 10 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Greater Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new DoubleGreaterVerifier(5.0), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 10.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 0.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: GreaterEqual Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntGreaterEqualVerifier(5), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 10 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual {
        { "Int", 5 }
    };
    positiveRes = testSpecification(doc, positiveEqual);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: GreaterEqual Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new DoubleGreaterEqualVerifier(5.0), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 10.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual {
        { "Double", 5.0 }
    };
    positiveRes = testSpecification(doc, positiveEqual);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 0.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Equal Bool", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Bool", new BoolEqualVerifier(true), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Bool", true}
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Bool", false }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Equal Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntEqualVerifier(1), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 1}
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Equal Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new DoubleEqualVerifier(1.0), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 1.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 0.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Equal String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        {{ "String", new StringEqualVerifier("string"s), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "String", "string"s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "String", "no_string"s }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "String");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Unequal Bool", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Bool", new BoolUnequalVerifier(true), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Bool", false }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Bool", true }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Unequal Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntUnequalVerifier(1), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Unequal Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new DoubleUnequalVerifier(1.0), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 1.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Unequal String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        {{ "String", new StringUnequalVerifier("string"s), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "String", "no_string"s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "String", "string"s }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "String");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: List Bool", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Bool" , new BoolInListVerifier({ true }), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Bool", true }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Bool", false }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: List Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int" , new IntInListVerifier({ 0, 1, 2 }), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Int", 2 }
    };
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 5 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: List Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double" , new DoubleInListVerifier({ 0.0, 1.0, 2.0 }), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 1.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Double", 2.0 }
    };
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 5.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: List String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        {{ "String" , new StringInListVerifier({ "0"s, "1"s, "2"s }), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "String", "1"s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "String", "2"s }
    };
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "String", "5"s }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "String");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotList Bool", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Bool" , new BoolNotInListVerifier({ true }), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Bool", false }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Bool", true }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotList Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int" , new IntNotInListVerifier({ 0, 1, 2 }), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", -1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Int", 3 }
    };
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 2 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotList Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double" , new DoubleNotInListVerifier({ 0.0, 1.0, 2.0 }), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", -1.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Double", 3.0 }
    };
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 1.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotList String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        {{ "String" , new StringNotInListVerifier({ "0"s, "1"s, "2"s }), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "String", "string"s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "String", "foo_string"s }
    };
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "String", "1"s }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "String");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Annotation Bool", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Bool", new BoolAnnotationVerifier("Bool"), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Bool", true }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Bool", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Annotation Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntAnnotationVerifier("Int"), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 1.1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Annotation Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new DoubleAnnotationVerifier("Double"), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", true }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Annotation String", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        {{ "String", new StringAnnotationVerifier("String"), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "String", ""s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "String", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "String");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Annotation Table", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Table", new TableAnnotationVerifier("Table"), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Table", ghoul::Dictionary{} }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Table", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: InRange Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new InRangeVerifier<IntVerifier>(0, 5), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", 2 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Int", 0 }
    };
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive3 {
        { "Int", 5 }
    };
    positiveRes = testSpecification(doc, positive3);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 10 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: InRange Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new InRangeVerifier<DoubleVerifier>(0.0, 5.0), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", 2.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Double", 0.0 }
    };
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive3 {
        { "Double", 5.0 }
    };
    positiveRes = testSpecification(doc, positive3);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive4 {
        { "Double", 1.5 }
    };
    positiveRes = testSpecification(doc, positive4);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 10.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotInRange Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new NotInRangeVerifier<IntVerifier>(0, 5), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Int", -1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Int", 6 }
    };
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Int", 2 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2 {
        { "Int", 0 }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative3 {
        { "Int", 5 }
    };
    negativeRes = testSpecification(doc, negative3);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: NotInRange Double", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Double", new NotInRangeVerifier<DoubleVerifier>(0.0, 5.0), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "Double", -1.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "Double", 6.0 }
    };
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Double", 0.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2 {
        { "Double", 5.0 }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative3 {
        { "Double", 2.5 }
    };
    negativeRes = testSpecification(doc, negative3);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: Wildcard", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ DocumentationEntry::Wildcard, new IntVerifier, Optional::No }}
    };

    ghoul::Dictionary positive {
        { "a", 1 },
        { "b", 2 },
        { "c", 3 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", false },
        { "b", 2 },
        { "c", 3 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", false },
        { "b", false },
        { "c", 3 }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "b");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative3 {
        { "a", false },
        { "b", false },
        { "c", false }
    };
    negativeRes = testSpecification(doc, negative3);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 3);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "b");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[2].offender == "c");
    REQUIRE(negativeRes.offenses[2].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Wildcard Mixed", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {
            { DocumentationEntry::Wildcard, new IntVerifier , Optional::No},
            { "b", new IntGreaterVerifier(5), Optional::No }
        }
    };

    ghoul::Dictionary positive {
        { "a", 1 },
        { "b", 8 },
        { "c", 3 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", false },
        { "b", 2 },
        { "c", 3 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "b");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2 {
        { "a", false },
        { "b", false },
        { "c", 3 }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "b");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative3 {
        { "a", false },
        { "b", 1 },
        { "c", false }
    };
    negativeRes = testSpecification(doc, negative3);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 3);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "b");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::Verification);
    REQUIRE(negativeRes.offenses[2].offender == "c");
    REQUIRE(negativeRes.offenses[2].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative4 {
        { "a", false },
        { "b", 10 },
        { "c", false }
    };
    negativeRes = testSpecification(doc, negative4);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "c");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: Referencing", "[documentation]") {
    using namespace openspace::documentation;

    Documentation referenced {
        "Referenced Name",
        "referenced_id",
        {
            { "a", new IntVerifier, Optional::No },
            { "b", new DoubleVerifier, Optional::No }
        },
    };
    DocEng.addDocumentation(referenced);

    Documentation doc {{
        { "Table", new ReferencingVerifier("referenced_id"), Optional::No }
    }};

    ghoul::Dictionary positive {
        { "Table", ghoul::Dictionary{ { "a", 1 }, { "b", 2.0 } }}
    };

    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "Table", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "Table", ghoul::Dictionary{ { "a", 1 }, { "b", true }}}
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table.b");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);


    Documentation wrongDoc {{
        { "Table", new ReferencingVerifier("WRONG"), Optional::No }
    } };
    ghoul::Dictionary wrongNegative {
        { "Table", ghoul::Dictionary{ { "a", 1 },{ "b", 2.0 } } }
    };
    negativeRes = testSpecification(wrongDoc, wrongNegative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table");
    REQUIRE(
        negativeRes.offenses[0].reason == TestResult::Offense::Reason::UnknownIdentifier
    );
}

TEST_CASE("Documentation: AndOperator", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {
            {
                "a",
                new AndVerifier({
                    new IntGreaterEqualVerifier(2), new IntLessEqualVerifier(5)
                }),
                Optional::No
            }
        }
    };

    ghoul::Dictionary positive {
        { "a", 4 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2 {
        { "a", 8 }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: OrOperator", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        {{ "a", new OrVerifier({ new StringVerifier, new IntVerifier }), Optional::No }}
    };

    ghoul::Dictionary positive {
        { "a", ""s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2 {
        { "a", 1 }
    };
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", false }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: BoolVector2Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "a", new BoolVector2Verifier, Optional::No }}
    };

    ghoul::Dictionary positive {
        { "a", glm::bvec2(true) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true }, { "2", 1.0 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: IntVector2Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new IntVector2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::ivec2(2) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleVector2Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleVector2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dvec2(2.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true }, { "2", 1.0 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: BoolVector3Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new BoolVector3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::bvec3(true) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 }, { "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: IntVector3Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new IntVector3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::ivec3(2) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1 }, { "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleVector3Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleVector3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dvec3(2.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 }, { "3", "s"} } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: BoolVector4Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new BoolVector4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::bvec4(true) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 }, { "3", "s" }, { "4", 1 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: IntVector4Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new IntVector4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::ivec4(2) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1 },{ "3", "s" }, { "4", 1 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2{
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleVector4Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleVector4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dvec4(2.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" }, { "4", 1 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix2x2Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix2x2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat2x2(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary { { "1", true },{ "2", 1.0 },{ "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix2x3Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix2x3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat2x3(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix2x4Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix2x4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat2x4(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix3x2Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix3x2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat3x2(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix3x3Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix3x3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat3x3(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix3x4Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix3x4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat3x4(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix4x2Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix4x2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat4x2(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix4x3Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix4x3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat4x3(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix4x4Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix4x4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat4x4(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DeprecatedVerifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc { {
        { "bool", new BoolDeprecatedVerifier, Optional::No },
        { "int" , new IntDeprecatedVerifier, Optional::No },
        { "double", new DoubleDeprecatedVerifier, Optional::No },
        { "string" , new StringDeprecatedVerifier, Optional::No },
        { "boolvec2", new DeprecatedVerifier<BoolVector2Verifier>, Optional::No },
        { "intvec2", new DeprecatedVerifier<IntVector2Verifier>, Optional::No },
        { "doublevec2", new DeprecatedVerifier<DoubleVector2Verifier>, Optional::No },
        { "boolvec3", new DeprecatedVerifier<BoolVector3Verifier>, Optional::No },
        { "intvec3", new DeprecatedVerifier<IntVector3Verifier>, Optional::No },
        { "doublevec3", new DeprecatedVerifier<DoubleVector3Verifier>, Optional::No },
        { "boolvec4", new DeprecatedVerifier<BoolVector4Verifier>, Optional::No },
        { "intvec4", new DeprecatedVerifier<IntVector4Verifier>, Optional::No },
        { "doublevec4", new DeprecatedVerifier<DoubleVector4Verifier>, Optional::No }
    }};

    ghoul::Dictionary positive {
        { "bool", true },
        { "int", 1 },
        { "double", 2.0 },
        { "string" , ""s },
        { "boolvec2", glm::bvec2(false) },
        { "intvec2", glm::ivec2(0) },
        { "doublevec2", glm::dvec2(0.0) },
        { "boolvec3", glm::bvec3(false) },
        { "intvec3", glm::ivec3(0) },
        { "doublevec3", glm::dvec3(0.0) },
        { "boolvec4", glm::bvec4(false) },
        { "intvec4", glm::ivec4(0) },
        { "doublevec4", glm::dvec4(0.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());
    REQUIRE(positiveRes.warnings.size() == 13);
    REQUIRE(positiveRes.warnings[0].offender == "bool");
    REQUIRE(positiveRes.warnings[0].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[1].offender == "boolvec2");
    REQUIRE(positiveRes.warnings[1].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[2].offender == "boolvec3");
    REQUIRE(positiveRes.warnings[2].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[3].offender == "boolvec4");
    REQUIRE(positiveRes.warnings[3].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[4].offender == "double");
    REQUIRE(positiveRes.warnings[4].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[5].offender == "doublevec2");
    REQUIRE(positiveRes.warnings[5].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[6].offender == "doublevec3");
    REQUIRE(positiveRes.warnings[6].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[7].offender == "doublevec4");
    REQUIRE(positiveRes.warnings[7].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[8].offender == "int");
    REQUIRE(positiveRes.warnings[8].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[9].offender == "intvec2");
    REQUIRE(positiveRes.warnings[9].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[10].offender == "intvec3");
    REQUIRE(positiveRes.warnings[10].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[11].offender == "intvec4");
    REQUIRE(positiveRes.warnings[11].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[12].offender == "string");
    REQUIRE(positiveRes.warnings[12].reason == TestResult::Warning::Reason::Deprecated);
}

TEST_CASE("Documentation: Verifier Type Post Conditions", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    REQUIRE(BoolVerifier().type() != "");
    REQUIRE(DoubleVerifier().type() != "");
    REQUIRE(IntVerifier().type() != "");
    REQUIRE(StringVerifier().type() != "");
    REQUIRE(TableVerifier().type() != "");

    REQUIRE(BoolVector2Verifier().type() != "");
    REQUIRE(IntVector2Verifier().type() != "");
    REQUIRE(DoubleVector2Verifier().type() != "");
    REQUIRE(BoolVector3Verifier().type() != "");
    REQUIRE(IntVector3Verifier().type() != "");
    REQUIRE(DoubleVector3Verifier().type() != "");
    REQUIRE(BoolVector4Verifier().type() != "");
    REQUIRE(IntVector4Verifier().type() != "");
    REQUIRE(DoubleVector4Verifier().type() != "");

    REQUIRE(IntLessVerifier(0).type() != "");
    REQUIRE(DoubleLessVerifier(0.0).type() != "");
    REQUIRE(IntLessEqualVerifier(0).type() != "");
    REQUIRE(DoubleLessEqualVerifier(0.0).type() != "");
    REQUIRE(IntGreaterVerifier(0).type() != "");
    REQUIRE(DoubleGreaterVerifier(0.0).type() != "");
    REQUIRE(IntGreaterEqualVerifier(0).type() != "");
    REQUIRE(DoubleGreaterEqualVerifier(0.0).type() != "");

    REQUIRE(BoolEqualVerifier(true).type() != "");
    REQUIRE(IntEqualVerifier(0).type() != "");
    REQUIRE(DoubleEqualVerifier(0.0).type() != "");
    REQUIRE(StringEqualVerifier(""s).type() != "");
    REQUIRE(BoolUnequalVerifier(true).type() != "");
    REQUIRE(IntUnequalVerifier(0).type() != "");
    REQUIRE(DoubleUnequalVerifier(0.0).type() != "");
    REQUIRE(StringUnequalVerifier(""s).type() != "");

    REQUIRE(BoolInListVerifier({ true }).type() != "");
    REQUIRE(IntInListVerifier({ 0 }).type() != "");
    REQUIRE(DoubleInListVerifier({ 0.0 }).type() != "");
    REQUIRE(StringInListVerifier({ ""s }).type() != "");
    REQUIRE(BoolNotInListVerifier({ true }).type() != "");
    REQUIRE(IntNotInListVerifier({ 0 }).type() != "");
    REQUIRE(DoubleNotInListVerifier({ 0.0 }).type() != "");
    REQUIRE(StringNotInListVerifier({ ""s }).type() != "");

    REQUIRE(IntInRangeVerifier({ 0, 1 }).type() != "");
    REQUIRE(DoubleInRangeVerifier({ 0.0, 1.0 }).type() != "");
    REQUIRE(IntNotInRangeVerifier({ 0, 1 }).type() != "");
    REQUIRE(DoubleNotInRangeVerifier({ 0.0, 1.0 }).type() != "");

    REQUIRE(BoolAnnotationVerifier("A"s).type() != "");
    REQUIRE(IntAnnotationVerifier("A"s).type() != "");
    REQUIRE(DoubleAnnotationVerifier("A"s).type() != "");
    REQUIRE(StringAnnotationVerifier("A"s).type() != "");
    REQUIRE(TableAnnotationVerifier("A"s).type() != "");
    REQUIRE(AnnotationVerifier<BoolVector2Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<IntVector2Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<DoubleVector2Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<BoolVector3Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<IntVector3Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<DoubleVector3Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<BoolVector4Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<IntVector4Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<DoubleVector4Verifier>("A"s).type() != "");

    REQUIRE(BoolDeprecatedVerifier().type() != "");
    REQUIRE(IntDeprecatedVerifier().type() != "");
    REQUIRE(DoubleDeprecatedVerifier().type() != "");
    REQUIRE(StringDeprecatedVerifier().type() != "");
    REQUIRE(TableDeprecatedVerifier().type() != "");
    REQUIRE(DeprecatedVerifier<BoolVector2Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<IntVector2Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector2Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<BoolVector3Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<IntVector3Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector3Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<BoolVector4Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<IntVector4Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector4Verifier>().type() != "");

    REQUIRE(ReferencingVerifier("identifier"s).type() != "");
}

TEST_CASE("Documentation: Verifier Documentation Post Conditions", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    REQUIRE(BoolVerifier().documentation() != "");
    REQUIRE(DoubleVerifier().documentation() != "");
    REQUIRE(IntVerifier().documentation() != "");
    REQUIRE(StringVerifier().documentation() != "");
    REQUIRE(TableVerifier().documentation() != "");

    REQUIRE(BoolVector2Verifier().documentation() != "");
    REQUIRE(IntVector2Verifier().documentation() != "");
    REQUIRE(DoubleVector2Verifier().documentation() != "");
    REQUIRE(BoolVector3Verifier().documentation() != "");
    REQUIRE(IntVector3Verifier().documentation() != "");
    REQUIRE(DoubleVector3Verifier().documentation() != "");
    REQUIRE(BoolVector4Verifier().documentation() != "");
    REQUIRE(IntVector4Verifier().documentation() != "");
    REQUIRE(DoubleVector4Verifier().documentation() != "");

    REQUIRE(IntLessVerifier(0).documentation() != "");
    REQUIRE(DoubleLessVerifier(0.0).documentation() != "");
    REQUIRE(IntLessEqualVerifier(0).documentation() != "");
    REQUIRE(DoubleLessEqualVerifier(0.0).documentation() != "");
    REQUIRE(IntGreaterVerifier(0).documentation() != "");
    REQUIRE(DoubleGreaterVerifier(0.0).documentation() != "");
    REQUIRE(IntGreaterEqualVerifier(0).documentation() != "");
    REQUIRE(DoubleGreaterEqualVerifier(0.0).documentation() != "");

    REQUIRE(BoolEqualVerifier(true).documentation() != "");
    REQUIRE(IntEqualVerifier(0).documentation() != "");
    REQUIRE(DoubleEqualVerifier(0.0).documentation() != "");
    REQUIRE(StringEqualVerifier(""s).documentation() != "");
    REQUIRE(BoolUnequalVerifier(true).documentation() != "");
    REQUIRE(IntUnequalVerifier(0).documentation() != "");
    REQUIRE(DoubleUnequalVerifier(0.0).documentation() != "");
    REQUIRE(StringUnequalVerifier(""s).documentation() != "");

    REQUIRE(BoolInListVerifier({ true }).documentation() != "");
    REQUIRE(IntInListVerifier({ 0 }).documentation() != "");
    REQUIRE(DoubleInListVerifier({ 0.0 }).documentation() != "");
    REQUIRE(StringInListVerifier({ ""s }).documentation() != "");
    REQUIRE(BoolNotInListVerifier({ true }).documentation() != "");
    REQUIRE(IntNotInListVerifier({ 0 }).documentation() != "");
    REQUIRE(DoubleNotInListVerifier({ 0.0 }).documentation() != "");
    REQUIRE(StringNotInListVerifier({ ""s }).documentation() != "");

    REQUIRE(IntInRangeVerifier({ 0, 1 }).documentation() != "");
    REQUIRE(DoubleInRangeVerifier({ 0.0, 1.0 }).documentation() != "");
    REQUIRE(IntNotInRangeVerifier({ 0, 1 }).documentation() != "");
    REQUIRE(DoubleNotInRangeVerifier({ 0.0, 1.0 }).documentation() != "");

    REQUIRE(BoolAnnotationVerifier("A"s).documentation() != "");
    REQUIRE(IntAnnotationVerifier("A"s).documentation() != "");
    REQUIRE(DoubleAnnotationVerifier("A"s).documentation() != "");
    REQUIRE(StringAnnotationVerifier("A"s).documentation() != "");
    REQUIRE(TableAnnotationVerifier("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<BoolVector2Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<IntVector2Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<DoubleVector2Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<BoolVector3Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<IntVector3Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<DoubleVector3Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<BoolVector4Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<IntVector4Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<DoubleVector4Verifier>("A"s).documentation() != "");

    REQUIRE(BoolDeprecatedVerifier().documentation() != "");
    REQUIRE(IntDeprecatedVerifier().documentation() != "");
    REQUIRE(DoubleDeprecatedVerifier().documentation() != "");
    REQUIRE(StringDeprecatedVerifier().documentation() != "");
    REQUIRE(TableDeprecatedVerifier().documentation() != "");
    REQUIRE(DeprecatedVerifier<BoolVector2Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<IntVector2Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector2Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<BoolVector3Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<IntVector3Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector3Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<BoolVector4Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<IntVector4Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector4Verifier>().documentation() != "");

    REQUIRE(ReferencingVerifier("identifier"s).documentation() != "");
}
