/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2016                                                               *
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

#include "gtest/gtest.h"

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/misc/dictionary.h>

#include <string>

class DocumentationTest : public testing::Test {};

TEST_F(DocumentationTest, Constructor) {
    using namespace openspace::documentation;

    Documentation doc;

    // Basic Verifiers
    doc.entries.emplace_back("BoolVerifier", new BoolVerifier);
    doc.entries.emplace_back("DoubleVerifier", new DoubleVerifier);
    doc.entries.emplace_back("IntVerifier", new IntVerifier);
    doc.entries.emplace_back("StringVerifier", new StringVerifier);
    doc.entries.emplace_back("TableVerifier", new TableVerifier);

    // Operator Verifiers
    doc.entries.emplace_back("LessDouble", new DoubleLessVerifier(0.0));
    doc.entries.emplace_back("LessInt", new IntLessVerifier(0));

    doc.entries.emplace_back("LessEqualDouble", new DoubleLessEqualVerifier(0.0));
    doc.entries.emplace_back("LessEqualInt", new IntLessEqualVerifier(0));

    doc.entries.emplace_back("GreaterDouble", new DoubleGreaterVerifier(0.0));
    doc.entries.emplace_back("GreaterInt", new IntGreaterVerifier(0));

    doc.entries.emplace_back("GreaterEqualDouble", new DoubleGreaterEqualVerifier(0.0));
    doc.entries.emplace_back("GreaterEqualInt", new IntGreaterEqualVerifier(0));

    doc.entries.emplace_back("EqualBool", new BoolEqualVerifier(false));
    doc.entries.emplace_back("EqualDouble", new DoubleEqualVerifier(0.0));
    doc.entries.emplace_back("EqualInt", new IntEqualVerifier(0));
    doc.entries.emplace_back("EqualString", new StringEqualVerifier(""));

    doc.entries.emplace_back("UnequalBool", new BoolUnequalVerifier(false));
    doc.entries.emplace_back("UnequalDouble", new DoubleUnequalVerifier(0.0));
    doc.entries.emplace_back("UnequalInt", new IntUnequalVerifier(0));
    doc.entries.emplace_back("UnequalString", new StringUnequalVerifier(""));

    // List Verifiers
    doc.entries.emplace_back("InListBool", new BoolInListVerifier({ true, false }));
    doc.entries.emplace_back("InListDouble", new DoubleInListVerifier({ 0.0, 1.0}));
    doc.entries.emplace_back("InListInt", new IntInListVerifier({ 0, 1 }));
    doc.entries.emplace_back("InListString", new StringInListVerifier({ "", "a" }));

    doc.entries.emplace_back("NotInListBool", new BoolNotInListVerifier({ true, false }));
    doc.entries.emplace_back("NotInListDouble", new DoubleNotInListVerifier({ 0.0, 1.0 }));
    doc.entries.emplace_back("NotInListInt", new IntNotInListVerifier({ 0, 1 }));
    doc.entries.emplace_back("NotInListString", new StringNotInListVerifier({ "", "a" }));

    // Range Verifiers
    doc.entries.emplace_back("InListDouble", new DoubleInRangeVerifier({ 0.0, 1.0 }));
    doc.entries.emplace_back("InListInt", new IntInRangeVerifier({ 0, 1 }));

    doc.entries.emplace_back("NotInListDouble", new DoubleNotInRangeVerifier({ 0.0, 1.0 }));
    doc.entries.emplace_back("NotInListInt", new IntNotInRangeVerifier({ 0, 1 }));

    // Misc Verifiers
    doc.entries.emplace_back("AnnotationBool", new BoolAnnotationVerifier("Bool"));
    doc.entries.emplace_back("AnnotationDouble", new DoubleAnnotationVerifier("Double"));
    doc.entries.emplace_back("AnnotationInt", new IntAnnotationVerifier("Int"));
    doc.entries.emplace_back("AnnotationString", new StringAnnotationVerifier("String"));
    doc.entries.emplace_back("AnnotationTable", new TableAnnotationVerifier("Table"));
}

TEST_F(DocumentationTest, InitializerConstructor) {
    using namespace openspace::documentation;
    
    Documentation doc {
        "Test",
        {
            // Basic Verifiers
            {"BoolVerifier", new BoolVerifier },
            {"DoubleVerifier", new DoubleVerifier},
            {"IntVerifier", new IntVerifier},
            {"StringVerifier", new StringVerifier},
            {"TableVerifier", new TableVerifier},

            // Operator Verifiers
            { "LessDouble", new DoubleLessVerifier(0.0)},
            { "LessInt", new IntLessVerifier(0)},

            {"LessEqualDouble", new DoubleLessEqualVerifier(0.0)},
            {"LessEqualInt", new IntLessEqualVerifier(0)},

            {"GreaterDouble", new DoubleGreaterVerifier(0.0)},
            {"GreaterInt", new IntGreaterVerifier(0)},

            {"GreaterEqualDouble", new DoubleGreaterEqualVerifier(0.0)},
            {"GreaterEqualInt", new IntGreaterEqualVerifier(0)},

            {"EqualBool", new BoolEqualVerifier(false)},
            {"EqualDouble", new DoubleEqualVerifier(0.0)},
            {"EqualInt", new IntEqualVerifier(0)},
            {"EqualString", new StringEqualVerifier("")},

            {"UnequalBool", new BoolUnequalVerifier(false)},
            {"UnequalDouble", new DoubleUnequalVerifier(0.0)},
            {"UnequalInt", new IntUnequalVerifier(0)},
            {"UnequalString", new StringUnequalVerifier("")},

            // List Verifiers
            {"InListBool", new BoolInListVerifier({ true, false })},
            {"InListDouble", new DoubleInListVerifier({ 0.0, 1.0 })},
            {"InListInt", new IntInListVerifier({ 0, 1 })},
            {"InListString", new StringInListVerifier({ "", "a" })},

            {"NotInListBool", new BoolNotInListVerifier({ true, false })},
            {"NotInListDouble", new DoubleNotInListVerifier({ 0.0, 1.0 })},
            {"NotInListInt", new IntNotInListVerifier({ 0, 1 })},
            {"NotInListString", new StringNotInListVerifier({ "", "a" })},

            // Range Verifiers
            {"InRangeDouble", new DoubleInRangeVerifier(0.0, 1.0)},
            {"InRangeInt", new IntInRangeVerifier(0, 1)},

            {"InRangeDouble", new DoubleNotInRangeVerifier(0.0, 1.0)},
            {"InRangeInt", new IntNotInRangeVerifier(0, 1)},

            // Misc Verifiers
            {"AnnotationBool", new BoolAnnotationVerifier("Bool")},
            {"AnnotationDouble", new DoubleAnnotationVerifier("Double")},
            {"AnnotationInt", new IntAnnotationVerifier("Int")},
            {"AnnotationString", new StringAnnotationVerifier("String")},
            {"AnnotationTable", new TableAnnotationVerifier("Table")}
        }
    };
}

TEST_F(DocumentationTest, BoolVerifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "",
        {{ "Bool", new BoolVerifier }}
    };

    ghoul::Dictionary positive {
        { "Bool", true }
    };

    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Bool", 0}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    
    ghoul::Dictionary negativeExist {
        { "Bool2", 0}
    };
    negativeRes = testSpecification(doc, negativeExist);

    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, DoubleVerifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "",
        {{ "Double", new DoubleVerifier }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };

    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 0 }
    };

    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negativeExist{
        { "Double2" , 0.0 }
    };
    negativeRes = testSpecification(doc, negativeExist);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, IntVerifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "",
        {{ "Int", new IntVerifier }}
    };

    ghoul::Dictionary positive {
        { "Int", 0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Int", 0.0 }
    };
    positiveRes = testSpecification(doc, positive2);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 0.1 }
    };

    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negativeExist {
        { "Int2", 0 }
    };
    negativeRes = testSpecification(doc, negativeExist);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, StringVerifier) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        "Test",
        {{ "String", new StringVerifier }}
    };

    ghoul::Dictionary positive {
        { "String", ""s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "String", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("String", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negativeExist {
        { "String2", ""s }
    };
    negativeRes = testSpecification(doc, negativeExist);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("String", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, TableVerifierType) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Table", new TableVerifier }}
    };

    ghoul::Dictionary positive {
        { "Table", ghoul::Dictionary{} }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Table", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Table", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negativeExist {
        { "Table2", ghoul::Dictionary{} }
    };
    negativeRes = testSpecification(doc, negativeExist);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Table", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, MixedVerifiers) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        "Test",
        {
            { "Bool", new BoolVerifier },
            { "Double", new DoubleVerifier },
            { "Int", new IntVerifier },
            { "String", new StringVerifier },
            { "Table", new TableVerifier }
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
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative1 {
        { "Bool", true },
        { "Double", 1 },
        { "Int", 0 },
        { "String", ""s },
        { "Table", ghoul::Dictionary{} }
    };
    TestResult negativeRes = testSpecification(doc, negative1);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "Bool", true },
        { "Double", 0.0 },
        { "Int", ""s },
        { "String", 1 },
        { "Table", ghoul::Dictionary{} }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("String", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[1].reason);
}

TEST_F(DocumentationTest, NestedTables) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        "Test",
        {
            { "Outer_Int", new IntVerifier },
            { "Outer_Table", new TableVerifier({
                { "Inner_Double", new DoubleVerifier },
                { "Inner_String", new StringVerifier }
            })},
            { "Outer_Double", new DoubleVerifier },
            { "Outer_Table2" , new TableVerifier({
                { "Inner_Double2", new DoubleVerifier },
                { "Inner_String2", new StringVerifier },
                { "Inner_Table" , new TableVerifier({
                    { "Inner_Inner_Int", new IntVerifier }
                })}
            })}
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
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

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
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Outer_Table", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

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
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Outer_Table.Inner_Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

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
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("Outer_Table.Inner_Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("Outer_Table.Inner_String", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[1].reason);

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
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("Outer_Table.Inner_Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("Outer_Table2.Inner_Double2", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[1].reason);

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
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(3, negativeRes.offenses.size());
    EXPECT_EQ("Outer_Table.Inner_Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("Outer_Table2.Inner_Double2", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[1].reason);
    EXPECT_EQ("Outer_Table2.Inner_Table.Inner_Inner_Int", negativeRes.offenses[2].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[2].reason);
}

TEST_F(DocumentationTest, Optional) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {
            { "Bool_Force", new BoolVerifier, "", Optional::No },
            { "Bool_Optional", new BoolVerifier, "", Optional::Yes }
        }
    };

    ghoul::Dictionary positive {
        { "Bool_Force", true },
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Bool_Force", true },
        { "Bool_Optional", true }
    };
    positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());
    
    ghoul::Dictionary negative {
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool_Force", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "Bool_Optional", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool_Force", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative3 {
        { "Bool_Force", true },
        { "Bool_Optional", 1 }
    };
    negativeRes = testSpecification(doc, negative3);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool_Optional", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, RequiredInOptional) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{
            "a",
            new TableVerifier({
                {
                    "b",
                    new IntVerifier
                },
                {
                    "c",
                    new IntVerifier,
                    "",
                    Optional::Yes
                }
            }),
            "",
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
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        {
            "a", ghoul::Dictionary{
                { "b", 1 },
                { "c", 2 }
        }
        }
    };
    positiveRes = testSpecification(doc, positive2);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive3 {};
    positiveRes = testSpecification(doc, positive3);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "c", 2 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a.b", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, Exhaustive) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new IntVerifier }},
        Exhaustive::Yes
    };

    ghoul::Dictionary positive {
        { "Int" , 1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "False_Int", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("False_Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::ExtraKey, negativeRes.offenses[0].reason);
    EXPECT_EQ("Int", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[1].reason);

    ghoul::Dictionary negative2 {
        { "Double", 2.0 }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::ExtraKey, negativeRes.offenses[0].reason);
    EXPECT_EQ("Int", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[1].reason);
}

TEST_F(DocumentationTest, NestedExhaustive) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Table", new TableVerifier(
            { { "a", new IntVerifier } },
            Exhaustive::Yes
        )
        }}
    };

    ghoul::Dictionary positive {
        { "Table", ghoul::Dictionary{{ "a", 1 }}}
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Table", ghoul::Dictionary{{ "b", 2.0 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("Table.a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
    EXPECT_EQ("Table.b", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::ExtraKey, negativeRes.offenses[1].reason);
}

TEST_F(DocumentationTest, EmptyEntriesNonExhaustive) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {}
    };

    ghoul::Dictionary positive {};
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "a", 1 }
    };
    positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());
}

TEST_F(DocumentationTest, EmptyEntriesExhaustive) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {},
        Exhaustive::Yes
    };

    ghoul::Dictionary positive {};
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::ExtraKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, EmptyNestedExhaustive) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{
            "Table",
            new TableVerifier(
            {
            },
            Exhaustive::Yes
            )
        }}
    };

    ghoul::Dictionary positive {
        { "Table", ghoul::Dictionary() }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Table", ghoul::Dictionary{ { "a", 1 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Table.a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::ExtraKey, negativeRes.offenses[0].reason);
}



TEST_F(DocumentationTest, LessInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new IntLessVerifier(5) }}
    };

    ghoul::Dictionary positive {
        { "Int", 0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 10 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, LessDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double", new DoubleLessVerifier(5.0) }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 10.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, LessEqualInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new IntLessEqualVerifier(5) }}
    };

    ghoul::Dictionary positive {
        { "Int", 0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positiveEqual {
        { "Int", 5 }
    };
    positiveRes = testSpecification(doc, positiveEqual);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 10 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, LessEqualDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double", new DoubleLessEqualVerifier(5.0) }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positiveEqual {
        { "Double", 5.0 }
    };
    positiveRes = testSpecification(doc, positiveEqual);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 10.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, GreaterInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new IntGreaterVerifier(5) }}
    };

    ghoul::Dictionary positive {
        { "Int", 10 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, GreaterDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double", new DoubleGreaterVerifier(5.0) }}
    };

    ghoul::Dictionary positive {
        { "Double", 10.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 0.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, GreaterEqualInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new IntGreaterEqualVerifier(5) }}
    };

    ghoul::Dictionary positive {
        { "Int", 10 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positiveEqual {
        { "Int", 5 }
    };
    positiveRes = testSpecification(doc, positiveEqual);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, GreaterEqualDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double", new DoubleGreaterEqualVerifier(5.0) }}
    };

    ghoul::Dictionary positive {
        { "Double", 10.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positiveEqual {
        { "Double", 5.0 }
    };
    positiveRes = testSpecification(doc, positiveEqual);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 0.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, EqualBool) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Bool", new BoolEqualVerifier(true) }}
    };

    ghoul::Dictionary positive {
        { "Bool", true}
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Bool", false }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, EqualInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new IntEqualVerifier(1) }}
    };

    ghoul::Dictionary positive {
        { "Int", 1}
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, EqualDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double", new DoubleEqualVerifier(1.0) }}
    };

    ghoul::Dictionary positive {
        { "Double", 1.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 0.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, EqualString) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        "Test",
        {{ "String", new StringEqualVerifier("string"s) }}
    };

    ghoul::Dictionary positive {
        { "String", "string"s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "String", "no_string"s }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("String", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, UnequalBool) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Bool", new BoolUnequalVerifier(true) }}
    };

    ghoul::Dictionary positive {
        { "Bool", false }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Bool", true }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, UnequalInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new IntUnequalVerifier(1) }}
    };

    ghoul::Dictionary positive {
        { "Int", 0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, UnequalDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double", new DoubleUnequalVerifier(1.0) }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 1.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, UnequalString) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        "Test",
        {{ "String", new StringUnequalVerifier("string"s) }}
    };

    ghoul::Dictionary positive {
        { "String", "no_string"s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "String", "string"s }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("String", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, ListBool) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Bool" , new BoolInListVerifier({ true }) }}
    };

    ghoul::Dictionary positive {
        { "Bool", true }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Bool", false }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, ListInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int" , new IntInListVerifier({ 0, 1, 2 }) }}
    };

    ghoul::Dictionary positive {
        { "Int", 1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Int", 2 }
    };
    positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 5 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, ListDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double" , new DoubleInListVerifier({ 0.0, 1.0, 2.0 }) }}
    };

    ghoul::Dictionary positive {
        { "Double", 1.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Double", 2.0 }
    };
    positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 5.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, ListString) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        "Test",
        {{ "String" , new StringInListVerifier({ "0"s, "1"s, "2"s }) }}
    };

    ghoul::Dictionary positive {
        { "String", "1"s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "String", "2"s }
    };
    positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "String", "5"s }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("String", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, NotListBool) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Bool" , new BoolNotInListVerifier({ true }) }}
    };

    ghoul::Dictionary positive {
        { "Bool", false }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Bool", true }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, NotListInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int" , new IntNotInListVerifier({ 0, 1, 2 }) }}
    };

    ghoul::Dictionary positive {
        { "Int", -1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Int", 3 }
    };
    positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 2 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, NotListDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double" , new DoubleNotInListVerifier({ 0.0, 1.0, 2.0 }) }}
    };

    ghoul::Dictionary positive {
        { "Double", -1.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Double", 3.0 }
    };
    positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 1.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, NotListString) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        "Test",
        {{ "String" , new StringNotInListVerifier({ "0"s, "1"s, "2"s }) }}
    };

    ghoul::Dictionary positive {
        { "String", "string"s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "String", "foo_string"s }
    };
    positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "String", "1"s }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("String", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, AnnotationBool) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Bool", new BoolAnnotationVerifier("Bool") }}
    };

    ghoul::Dictionary positive {
        { "Bool", true }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Bool", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Bool", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, AnnotationInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new IntAnnotationVerifier("Int") }}
    };

    ghoul::Dictionary positive {
        { "Int", 1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 1.1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, AnnotationDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double", new DoubleAnnotationVerifier("Double") }}
    };

    ghoul::Dictionary positive {
        { "Double", 0.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", true }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, AnnotationString) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        "Test",
        {{ "String", new StringAnnotationVerifier("String") }}
    };

    ghoul::Dictionary positive {
        { "String", ""s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "String", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("String", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, AnnotationTable) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Table", new TableAnnotationVerifier("Table") }}
    };

    ghoul::Dictionary positive {
        { "Table", ghoul::Dictionary{} }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Table", 1 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Table", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, InRangeInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new InRangeVerifier<IntVerifier>(0, 5) }}
    };

    ghoul::Dictionary positive {
        { "Int", 2 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Int", 0 }
    };
    positiveRes = testSpecification(doc, positive2);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive3 {
        { "Int", 5 }
    };
    positiveRes = testSpecification(doc, positive3);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 10 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, InRangeDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double", new InRangeVerifier<DoubleVerifier>(0.0, 5.0) }}
    };

    ghoul::Dictionary positive {
        { "Double", 2.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Double", 0.0 }
    };
    positiveRes = testSpecification(doc, positive2);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive3 {
        { "Double", 5.0 }
    };
    positiveRes = testSpecification(doc, positive3);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive4 {
        { "Double", 1.5 }
    };
    positiveRes = testSpecification(doc, positive4);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 10.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, NotInRangeInt) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Int", new NotInRangeVerifier<IntVerifier>(0, 5) }}
    };

    ghoul::Dictionary positive {
        { "Int", -1 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Int", 6 }
    };
    positiveRes = testSpecification(doc, positive2);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Int", 2 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "Int", 0 }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative3 {
        { "Int", 5 }
    };
    negativeRes = testSpecification(doc, negative3);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, NotInRangeDouble) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "Double", new NotInRangeVerifier<DoubleVerifier>(0.0, 5.0) }}
    };

    ghoul::Dictionary positive {
        { "Double", -1.0 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "Double", 6.0 }
    };
    positiveRes = testSpecification(doc, positive2);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Double", 0.0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "Double", 5.0 }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative3 {
        { "Double", 2.5 }
    };
    negativeRes = testSpecification(doc, negative3);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Double", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, Wildcard) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ DocumentationEntry::Wildcard, new IntVerifier }}
    };

    ghoul::Dictionary positive {
        { "a", 1 },
        { "b", 2 },
        { "c", 3 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", false },
        { "b", 2 },
        { "c", 3 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", false },
        { "b", false },
        { "c", 3 }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("b", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[1].reason);

    ghoul::Dictionary negative3 {
        { "a", false },
        { "b", false },
        { "c", false }
    };
    negativeRes = testSpecification(doc, negative3);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(3, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("b", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[1].reason);
    EXPECT_EQ("c", negativeRes.offenses[2].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[2].reason);
}

TEST_F(DocumentationTest, WildcardMixed) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {
            { DocumentationEntry::Wildcard, new IntVerifier },
            { "b", new IntGreaterVerifier(5) }
        }
    };

    ghoul::Dictionary positive {
        { "a", 1 },
        { "b", 8 },
        { "c", 3 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", false },
        { "b", 2 },
        { "c", 3 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("b", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[1].reason);

    ghoul::Dictionary negative2 {
        { "a", false },
        { "b", false },
        { "c", 3 }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("b", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[1].reason);

    ghoul::Dictionary negative3 {
        { "a", false },
        { "b", 1 },
        { "c", false }
    };
    negativeRes = testSpecification(doc, negative3);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(3, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("b", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[1].reason);
    EXPECT_EQ("c", negativeRes.offenses[2].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[2].reason);

    ghoul::Dictionary negative4 {
        { "a", false },
        { "b", 10 },
        { "c", false }
    };
    negativeRes = testSpecification(doc, negative4);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(2, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
    EXPECT_EQ("c", negativeRes.offenses[1].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[1].reason);
}

TEST_F(DocumentationTest, AndOperator) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {
            { "a", new AndVerifier(
                new IntGreaterEqualVerifier(2), new IntLessEqualVerifier(5)
                )
            }
        }
    };

    ghoul::Dictionary positive {
        { "a", 4 }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", 8 }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, OrOperator) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    Documentation doc {
        "Test",
        {{ "a", new OrVerifier(new StringVerifier, new IntVerifier)}}
    };

    ghoul::Dictionary positive {
        { "a", ""s }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary positive2 {
        { "a", 1 }
    };
    positiveRes = testSpecification(doc, positive2);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", false }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::Verification, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, BoolVector2Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        {{ "a", new BoolVector2Verifier }}
    };

    ghoul::Dictionary positive {
        { "a", glm::bvec2(true) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true }, { "2", 1.0 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, IntVector2Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        { { "a", new IntVector2Verifier } }
    };

    ghoul::Dictionary positive {
        { "a", glm::ivec2(2) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, DoubleVector2Verifier) {
    using namespace openspace::documentation;

    Documentation doc{
        "Test",
        { { "a", new DoubleVector2Verifier } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dvec2(2.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true }, { "2", 1.0 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, BoolVector3Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        { { "a", new BoolVector3Verifier } }
    };

    ghoul::Dictionary positive {
        { "a", glm::bvec3(true) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 }, { "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, IntVector3Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        { { "a", new IntVector3Verifier } }
    };

    ghoul::Dictionary positive {
        { "a", glm::ivec3(2) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1 }, { "3", "s" } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, DoubleVector3Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        { { "a", new DoubleVector3Verifier } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dvec3(2.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 }, { "3", "s"} } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, BoolVector4Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        { { "a", new BoolVector4Verifier } }
    };

    ghoul::Dictionary positive {
        { "a", glm::bvec4(true) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 }, { "3", "s" }, { "4", 1 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, IntVector4Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        { { "a", new IntVector4Verifier } }
    };

    ghoul::Dictionary positive {
        { "a", glm::ivec4(2) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1 },{ "3", "s" }, { "4", 1 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2{
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, DoubleVector4Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        "Test",
        { { "a", new DoubleVector4Verifier } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dvec4(2.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" }, { "4", 1 } } }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "a", true }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, VerifierTypePostConditions) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    EXPECT_NE("", BoolVerifier().type());
    EXPECT_NE("", DoubleVerifier().type());
    EXPECT_NE("", IntVerifier().type());
    EXPECT_NE("", StringVerifier().type());
    EXPECT_NE("", TableVerifier().type());

    EXPECT_NE("", BoolVector2Verifier().type());
    EXPECT_NE("", IntVector2Verifier().type());
    EXPECT_NE("", DoubleVector2Verifier().type());
    EXPECT_NE("", BoolVector3Verifier().type());
    EXPECT_NE("", IntVector3Verifier().type());
    EXPECT_NE("", DoubleVector3Verifier().type());
    EXPECT_NE("", BoolVector4Verifier().type());
    EXPECT_NE("", IntVector4Verifier().type());
    EXPECT_NE("", DoubleVector4Verifier().type());

    EXPECT_NE("", IntLessVerifier(0).type());
    EXPECT_NE("", DoubleLessVerifier(0.0).type());
    EXPECT_NE("", IntLessEqualVerifier(0).type());
    EXPECT_NE("", DoubleLessEqualVerifier(0.0).type());
    EXPECT_NE("", IntGreaterVerifier(0).type());
    EXPECT_NE("", DoubleGreaterVerifier(0.0).type());
    EXPECT_NE("", IntGreaterEqualVerifier(0).type());
    EXPECT_NE("", DoubleGreaterEqualVerifier(0.0).type());

    EXPECT_NE("", BoolEqualVerifier(true).type());
    EXPECT_NE("", IntEqualVerifier(0).type());
    EXPECT_NE("", DoubleEqualVerifier(0.0).type());
    EXPECT_NE("", StringEqualVerifier(""s).type());
    EXPECT_NE("", BoolUnequalVerifier(true).type());
    EXPECT_NE("", IntUnequalVerifier(0).type());
    EXPECT_NE("", DoubleUnequalVerifier(0.0).type());
    EXPECT_NE("", StringUnequalVerifier(""s).type());

    EXPECT_NE("", BoolInListVerifier({ true }).type());
    EXPECT_NE("", IntInListVerifier({ 0 }).type());
    EXPECT_NE("", DoubleInListVerifier({ 0.0 }).type());
    EXPECT_NE("", StringInListVerifier({ ""s }).type());
    EXPECT_NE("", BoolNotInListVerifier({ true }).type());
    EXPECT_NE("", IntNotInListVerifier({ 0 }).type());
    EXPECT_NE("", DoubleNotInListVerifier({ 0.0 }).type());
    EXPECT_NE("", StringNotInListVerifier({ ""s }).type());

    EXPECT_NE("", IntInRangeVerifier({ 0, 1 }).type());
    EXPECT_NE("", DoubleInRangeVerifier({ 0.0, 1.0 }).type());
    EXPECT_NE("", IntNotInRangeVerifier({ 0, 1 }).type());
    EXPECT_NE("", DoubleNotInRangeVerifier({ 0.0, 1.0 }).type());

    EXPECT_NE("", BoolAnnotationVerifier("Annotation"s).type());
    EXPECT_NE("", IntAnnotationVerifier("Annotation"s).type());
    EXPECT_NE("", DoubleAnnotationVerifier("Annotation"s).type());
    EXPECT_NE("", StringAnnotationVerifier("Annotation"s).type());
    EXPECT_NE("", TableAnnotationVerifier("Annotation"s).type());
}

TEST_F(DocumentationTest, VerifierDocumentationPostConditions) {
    using namespace openspace::documentation;
    using namespace std::string_literals;

    EXPECT_NE("", BoolVerifier().documentation());
    EXPECT_NE("", DoubleVerifier().documentation());
    EXPECT_NE("", IntVerifier().documentation());
    EXPECT_NE("", StringVerifier().documentation());
    EXPECT_NE("", TableVerifier().documentation());

    EXPECT_NE("", BoolVector2Verifier().documentation());
    EXPECT_NE("", IntVector2Verifier().documentation());
    EXPECT_NE("", DoubleVector2Verifier().documentation());
    EXPECT_NE("", BoolVector3Verifier().documentation());
    EXPECT_NE("", IntVector3Verifier().documentation());
    EXPECT_NE("", DoubleVector3Verifier().documentation());
    EXPECT_NE("", BoolVector4Verifier().documentation());
    EXPECT_NE("", IntVector4Verifier().documentation());
    EXPECT_NE("", DoubleVector4Verifier().documentation());

    EXPECT_NE("", IntLessVerifier(0).documentation());
    EXPECT_NE("", DoubleLessVerifier(0.0).documentation());
    EXPECT_NE("", IntLessEqualVerifier(0).documentation());
    EXPECT_NE("", DoubleLessEqualVerifier(0.0).documentation());
    EXPECT_NE("", IntGreaterVerifier(0).documentation());
    EXPECT_NE("", DoubleGreaterVerifier(0.0).documentation());
    EXPECT_NE("", IntGreaterEqualVerifier(0).documentation());
    EXPECT_NE("", DoubleGreaterEqualVerifier(0.0).documentation());

    EXPECT_NE("", BoolEqualVerifier(true).documentation());
    EXPECT_NE("", IntEqualVerifier(0).documentation());
    EXPECT_NE("", DoubleEqualVerifier(0.0).documentation());
    EXPECT_NE("", StringEqualVerifier(""s).documentation());
    EXPECT_NE("", BoolUnequalVerifier(true).documentation());
    EXPECT_NE("", IntUnequalVerifier(0).documentation());
    EXPECT_NE("", DoubleUnequalVerifier(0.0).documentation());
    EXPECT_NE("", StringUnequalVerifier(""s).documentation());

    EXPECT_NE("", BoolInListVerifier({ true }).documentation());
    EXPECT_NE("", IntInListVerifier({ 0 }).documentation());
    EXPECT_NE("", DoubleInListVerifier({ 0.0 }).documentation());
    EXPECT_NE("", StringInListVerifier({ ""s }).documentation());
    EXPECT_NE("", BoolNotInListVerifier({ true }).documentation());
    EXPECT_NE("", IntNotInListVerifier({ 0 }).documentation());
    EXPECT_NE("", DoubleNotInListVerifier({ 0.0 }).documentation());
    EXPECT_NE("", StringNotInListVerifier({ ""s }).documentation());

    EXPECT_NE("", IntInRangeVerifier({ 0, 1 }).documentation());
    EXPECT_NE("", DoubleInRangeVerifier({ 0.0, 1.0 }).documentation());
    EXPECT_NE("", IntNotInRangeVerifier({ 0, 1 }).documentation());
    EXPECT_NE("", DoubleNotInRangeVerifier({ 0.0, 1.0 }).documentation());

    EXPECT_NE("", BoolAnnotationVerifier("Annotation"s).documentation());
    EXPECT_NE("", IntAnnotationVerifier("Annotation"s).documentation());
    EXPECT_NE("", DoubleAnnotationVerifier("Annotation"s).documentation());
    EXPECT_NE("", StringAnnotationVerifier("Annotation"s).documentation());
    EXPECT_NE("", TableAnnotationVerifier("Annotation"s).documentation());
}
