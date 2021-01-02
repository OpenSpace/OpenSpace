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

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);

    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", 0);

    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("Bool2", 0);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);

    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 0);

    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("Double2", 0.0);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 0.0);
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 0.1);

    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("Int2", 0);
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

    ghoul::Dictionary positive;
    positive.setValue("String", ""s);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", 0);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "String");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("String2", ""s);
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

    ghoul::Dictionary positive;
    positive.setValue("Table", ghoul::Dictionary());
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Table", 0);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("Table2", ghoul::Dictionary());
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

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", "a"s);
        inner.setValue("2", "b"s);
        inner.setValue("3", "c"s);
        positive.setValue("StringList", inner);
    }
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("StringList", 0);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "StringList");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);


    ghoul::Dictionary negative2;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", "a"s);
        inner.setValue("2", "b"s);
        inner.setValue("3", 2.0);
        negative2.setValue("StringList", inner);
    }
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "StringList.3");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("StringList2", ghoul::Dictionary());
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

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", 1);
        inner.setValue("2", 2);
        inner.setValue("3", 3);
        positive.setValue("IntList", inner);
    }
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("IntList", 0);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "IntList");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", "a"s);
        inner.setValue("2", 1);
        inner.setValue("3", 2);
        negative2.setValue("IntList", inner);
    }
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "IntList.1");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negativeExist;
    negativeExist.setValue("IntList2", ghoul::Dictionary());
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

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);
    positive.setValue("Double", 0.0);
    positive.setValue("Int", 0);
    positive.setValue("String", ""s);
    positive.setValue("Table", ghoul::Dictionary());
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative1;
    negative1.setValue("Bool", true);
    negative1.setValue("Double", 1);
    negative1.setValue("Int", 0);
    negative1.setValue("String", ""s);
    negative1.setValue("Table", ghoul::Dictionary());
    TestResult negativeRes = testSpecification(doc, negative1);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("Bool", true);
    negative2.setValue("Double", 0.0);
    negative2.setValue("Int", ""s);
    negative2.setValue("String", 1);
    negative2.setValue("Table", ghoul::Dictionary());
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
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

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
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Outer_Table");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

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
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

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
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "Outer_Table.Inner_String");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

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
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "Outer_Table.Inner_Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "Outer_Table2.Inner_Double2");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

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

    ghoul::Dictionary positive;
    positive.setValue("Bool_Force", true);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Bool_Force", true);
    positive2.setValue("Bool_Optional", true);
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());
    
    ghoul::Dictionary negative;
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool_Force");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);

    ghoul::Dictionary negative2;
    negative2.setValue("Bool_Optional", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Bool_Force");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);

    ghoul::Dictionary negative3;
    negative3.setValue("Bool_Force", true);
    negative3.setValue("Bool_Optional", 1);
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

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("b", 1);
        positive.setValue("a", inner);
    }
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    {
        ghoul::Dictionary inner;
        inner.setValue("b", 1);
        inner.setValue("c", 2);
        positive2.setValue("a", inner);
    }
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive3;
    positiveRes = testSpecification(doc, positive3);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("c", 2);
        negative.setValue("a", inner);
    }
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 1);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("False_Int", 1);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::MissingKey);

    ghoul::Dictionary negative2;
    negative2.setValue("Double", 2.0);
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

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        positive.setValue("Table", inner);
    }
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("b", 2.0);
        negative.setValue("Table", inner);
    }
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

    ghoul::Dictionary positive2;
    positive2.setValue("a", 1);
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

    ghoul::Dictionary positive;
    positive.setValue("Table", ghoul::Dictionary());
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        negative.setValue("Table", inner);
    }
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE(negativeRes.success);
    REQUIRE(negativeRes.offenses.empty());
}

TEST_CASE("Documentation: Less Int", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntLessVerifier(5), Optional::No }}
    };

    ghoul::Dictionary positive;
    positive.setValue("Int", 0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 10);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 10.0);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual;
    positiveEqual.setValue("Int", 5);
    positiveRes = testSpecification(doc, positiveEqual);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 10);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual;
    positiveEqual.setValue("Double", 5.0);
    positiveRes = testSpecification(doc, positiveEqual);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 10.0);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 10);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 0);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 10.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 0.0);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 10);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual;
    positiveEqual.setValue("Int", 5);
    positiveRes = testSpecification(doc, positiveEqual);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 0);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 10.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positiveEqual;
    positiveEqual.setValue("Double", 5.0);
    positiveRes = testSpecification(doc, positiveEqual);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 0.0);
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

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", false);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 1);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 0);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 0.0);
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

    ghoul::Dictionary positive;
    positive.setValue("String", "string"s);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", "no_string"s);
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

    ghoul::Dictionary positive;
    positive.setValue("Bool", false);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", true);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 1);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 1.0);
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

    ghoul::Dictionary positive;
    positive.setValue("String", "no_string"s);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", "string"s);
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

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", false);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 1);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 2);
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 5);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Double", 2.0);
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 5.0);
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

    ghoul::Dictionary positive;
    positive.setValue("String", "1"s);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("String", "2"s);
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", "5"s);
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

    ghoul::Dictionary positive;
    positive.setValue("Bool", false);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", true);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", -1);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 3);
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 2);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", -1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Double", 3.0);
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 1.0);
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

    ghoul::Dictionary positive;
    positive.setValue("String", "string"s);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("String", "foo_string"s);
    positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", "1"s);
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

    ghoul::Dictionary positive;
    positive.setValue("Bool", true);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Bool", 0);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 1);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 1.1);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 0.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", true);
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

    ghoul::Dictionary positive;
    positive.setValue("String", ""s);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("String", 1);
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

    ghoul::Dictionary positive;
    positive.setValue("Table", ghoul::Dictionary());
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Table", 1);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", 2);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 0);
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive3;
    positive3.setValue("Int", 5);
    positiveRes = testSpecification(doc, positive3);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 10);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", 2.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Double", 0.0);
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive3;
    positive3.setValue("Double", 5.0);
    positiveRes = testSpecification(doc, positive3);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive4;
    positive4.setValue("Double", 1.5);
    positiveRes = testSpecification(doc, positive4);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 10.0);
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

    ghoul::Dictionary positive;
    positive.setValue("Int", -1);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Int", 6);
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Int", 2);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2;
    negative2.setValue("Int", 0);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Int");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative3;
    negative3.setValue("Int", 5);
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

    ghoul::Dictionary positive;
    positive.setValue("Double", -1.0);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("Double", 6.0);
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Double", 0.0);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2;
    negative2.setValue("Double", 5.0);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Double");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative3;
    negative3.setValue("Double", 2.5);
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

    ghoul::Dictionary positive;
    positive.setValue("a", 1);
    positive.setValue("b", 2);
    positive.setValue("c", 3);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("a", false);
    negative.setValue("b", 2);
    negative.setValue("c", 3);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", false);
    negative2.setValue("b", false);
    negative2.setValue("c", 3);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "b");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative3;
    negative3.setValue("a", false);
    negative3.setValue("b", false);
    negative3.setValue("c", false);
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

    ghoul::Dictionary positive;
    positive.setValue("a", 1);
    positive.setValue("b", 8);
    positive.setValue("c", 3);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("a", false);
    negative.setValue("b", 2);
    negative.setValue("c", 3);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "b");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2;
    negative2.setValue("a", false);
    negative2.setValue("b", false);
    negative2.setValue("c", 3);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 2);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "b");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative3;
    negative3.setValue("a", false);
    negative3.setValue("b", 1);
    negative3.setValue("c", false);
    negativeRes = testSpecification(doc, negative3);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 3);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
    REQUIRE(negativeRes.offenses[1].offender == "b");
    REQUIRE(negativeRes.offenses[1].reason == TestResult::Offense::Reason::Verification);
    REQUIRE(negativeRes.offenses[2].offender == "c");
    REQUIRE(negativeRes.offenses[2].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative4;
    negative4.setValue("a", false);
    negative4.setValue("b", 10);
    negative4.setValue("c", false);
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

    ghoul::Dictionary positive;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        inner.setValue("b", 2.0);
        positive.setValue("Table", inner);
    };
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("Table", 1);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        inner.setValue("b", true);
        negative2.setValue("Table", inner);
    };
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "Table.b");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);


    Documentation wrongDoc {{
        { "Table", new ReferencingVerifier("WRONG"), Optional::No }
    } };
    ghoul::Dictionary wrongNegative;
    {
        ghoul::Dictionary inner;
        inner.setValue("a", 1);
        inner.setValue("b", 2.0);
        wrongNegative.setValue("Table", inner);
    }
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

    ghoul::Dictionary positive;
    positive.setValue("a", 4);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("a", 0);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);

    ghoul::Dictionary negative2;
    negative2.setValue("a", 8);
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

    ghoul::Dictionary positive;
    positive.setValue("a", ""s);
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary positive2;
    positive2.setValue("a", 1);
    positiveRes = testSpecification(doc, positive2);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    negative.setValue("a", false);
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::Verification);
}

TEST_CASE("Documentation: IntVector2Verifier", "[documentation]") {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new IntVector2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::ivec2(2));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
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

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dvec2(2.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: IntVector3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new IntVector3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::ivec3(2));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleVector3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleVector3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dvec3(2.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: IntVector4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new IntVector4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::ivec4(2));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

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
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleVector4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleVector4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dvec4(2.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

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
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix2x2Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleMatrix2x2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat2x2(1.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix2x3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleMatrix2x3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat2x3(1.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    }
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix2x4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleMatrix2x4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat2x4(1.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix3x2Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleMatrix3x2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat3x2(1.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix3x3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleMatrix3x3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat3x3(1.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix3x4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleMatrix3x4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat3x4(1.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix4x2Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleMatrix4x2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat4x2(1.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix4x3Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleMatrix4x3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat4x3(1.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
    negativeRes = testSpecification(doc, negative2);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);
}

TEST_CASE("Documentation: DoubleMatrix4x4Verifier", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        { { "a", new DoubleMatrix4x4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive;
    positive.setValue("a", glm::dmat4x4(1.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());

    ghoul::Dictionary negative;
    {
        ghoul::Dictionary inner;
        inner.setValue("1", true);
        inner.setValue("2", 1.0);
        inner.setValue("3", "s"s);
        negative.setValue("a", inner);
    };
    TestResult negativeRes = testSpecification(doc, negative);
    REQUIRE_FALSE(negativeRes.success);
    REQUIRE(negativeRes.offenses.size() == 1);
    REQUIRE(negativeRes.offenses[0].offender == "a");
    REQUIRE(negativeRes.offenses[0].reason == TestResult::Offense::Reason::WrongType);

    ghoul::Dictionary negative2;
    negative2.setValue("a", true);
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
        { "intvec2", new DeprecatedVerifier<IntVector2Verifier>, Optional::No },
        { "doublevec2", new DeprecatedVerifier<DoubleVector2Verifier>, Optional::No },
        { "intvec3", new DeprecatedVerifier<IntVector3Verifier>, Optional::No },
        { "doublevec3", new DeprecatedVerifier<DoubleVector3Verifier>, Optional::No },
        { "intvec4", new DeprecatedVerifier<IntVector4Verifier>, Optional::No },
        { "doublevec4", new DeprecatedVerifier<DoubleVector4Verifier>, Optional::No }
    }};

    ghoul::Dictionary positive;
    positive.setValue("bool", true);
    positive.setValue("int", 1);
    positive.setValue("double", 2.0);
    positive.setValue("string", ""s);
    positive.setValue("intvec2", glm::ivec2(0));
    positive.setValue("doublevec2", glm::dvec2(0.0));
    positive.setValue("intvec3", glm::ivec3(0));
    positive.setValue("doublevec3", glm::dvec3(0.0));
    positive.setValue("intvec4", glm::ivec4(0));
    positive.setValue("doublevec4", glm::dvec4(0.0));
    TestResult positiveRes = testSpecification(doc, positive);
    REQUIRE(positiveRes.success);
    REQUIRE(positiveRes.offenses.empty());
    REQUIRE(positiveRes.warnings.size() == 13);
    REQUIRE(positiveRes.warnings[0].offender == "bool");
    REQUIRE(positiveRes.warnings[0].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[1].offender == "double");
    REQUIRE(positiveRes.warnings[1].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[2].offender == "doublevec2");
    REQUIRE(positiveRes.warnings[2].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[3].offender == "doublevec3");
    REQUIRE(positiveRes.warnings[3].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[4].offender == "doublevec4");
    REQUIRE(positiveRes.warnings[4].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[5].offender == "int");
    REQUIRE(positiveRes.warnings[5].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[6].offender == "intvec2");
    REQUIRE(positiveRes.warnings[6].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[7].offender == "intvec3");
    REQUIRE(positiveRes.warnings[7].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[8].offender == "intvec4");
    REQUIRE(positiveRes.warnings[8].reason == TestResult::Warning::Reason::Deprecated);
    REQUIRE(positiveRes.warnings[9].offender == "string");
    REQUIRE(positiveRes.warnings[9].reason == TestResult::Warning::Reason::Deprecated);
}

TEST_CASE("Documentation: Verifier Type Post Conditions", "[documentation]") {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    REQUIRE(BoolVerifier().type() != "");
    REQUIRE(DoubleVerifier().type() != "");
    REQUIRE(IntVerifier().type() != "");
    REQUIRE(StringVerifier().type() != "");
    REQUIRE(TableVerifier().type() != "");

    REQUIRE(IntVector2Verifier().type() != "");
    REQUIRE(DoubleVector2Verifier().type() != "");
    REQUIRE(IntVector3Verifier().type() != "");
    REQUIRE(DoubleVector3Verifier().type() != "");
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
    REQUIRE(AnnotationVerifier<IntVector2Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<DoubleVector2Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<IntVector3Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<DoubleVector3Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<IntVector4Verifier>("A"s).type() != "");
    REQUIRE(AnnotationVerifier<DoubleVector4Verifier>("A"s).type() != "");

    REQUIRE(BoolDeprecatedVerifier().type() != "");
    REQUIRE(IntDeprecatedVerifier().type() != "");
    REQUIRE(DoubleDeprecatedVerifier().type() != "");
    REQUIRE(StringDeprecatedVerifier().type() != "");
    REQUIRE(TableDeprecatedVerifier().type() != "");
    REQUIRE(DeprecatedVerifier<IntVector2Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector2Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<IntVector3Verifier>().type() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector3Verifier>().type() != "");
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

    REQUIRE(IntVector2Verifier().documentation() != "");
    REQUIRE(DoubleVector2Verifier().documentation() != "");
    REQUIRE(IntVector3Verifier().documentation() != "");
    REQUIRE(DoubleVector3Verifier().documentation() != "");
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
    REQUIRE(AnnotationVerifier<IntVector2Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<DoubleVector2Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<IntVector3Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<DoubleVector3Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<IntVector4Verifier>("A"s).documentation() != "");
    REQUIRE(AnnotationVerifier<DoubleVector4Verifier>("A"s).documentation() != "");

    REQUIRE(BoolDeprecatedVerifier().documentation() != "");
    REQUIRE(IntDeprecatedVerifier().documentation() != "");
    REQUIRE(DoubleDeprecatedVerifier().documentation() != "");
    REQUIRE(StringDeprecatedVerifier().documentation() != "");
    REQUIRE(TableDeprecatedVerifier().documentation() != "");
    REQUIRE(DeprecatedVerifier<IntVector2Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector2Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<IntVector3Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector3Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<IntVector4Verifier>().documentation() != "");
    REQUIRE(DeprecatedVerifier<DoubleVector4Verifier>().documentation() != "");

    REQUIRE(ReferencingVerifier("identifier"s).documentation() != "");
}
