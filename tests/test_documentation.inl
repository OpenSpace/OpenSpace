/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#include <openspace/documentation/documentation.h>
#include <openspace/documentation/documentationengine.h>
#include <openspace/documentation/verifier.h>

#include <ghoul/misc/dictionary.h>

#include <string>

class DocumentationTest : public testing::Test {};

TEST_F(DocumentationTest, Constructor) {
    using namespace openspace::documentation;

    Documentation doc;

    // Basic Verifiers
    doc.entries.emplace_back("BoolVerifier", new BoolVerifier, Optional::No);
    doc.entries.emplace_back("DoubleVerifier", new DoubleVerifier, Optional::No);
    doc.entries.emplace_back("IntVerifier", new IntVerifier, Optional::No);
    doc.entries.emplace_back("StringVerifier", new StringVerifier, Optional::No);
    doc.entries.emplace_back("TableVerifier", new TableVerifier, Optional::No);

    // Operator Verifiers
    doc.entries.emplace_back("LessDouble", new DoubleLessVerifier(0.0), Optional::No);
    doc.entries.emplace_back("LessInt", new IntLessVerifier(0), Optional::No);

    doc.entries.emplace_back("LessEqualDouble", new DoubleLessEqualVerifier(0.0), Optional::No);
    doc.entries.emplace_back("LessEqualInt", new IntLessEqualVerifier(0), Optional::No);

    doc.entries.emplace_back("GreaterDouble", new DoubleGreaterVerifier(0.0), Optional::No);
    doc.entries.emplace_back("GreaterInt", new IntGreaterVerifier(0), Optional::No);

    doc.entries.emplace_back("GreaterEqualDouble", new DoubleGreaterEqualVerifier(0.0), Optional::No);
    doc.entries.emplace_back("GreaterEqualInt", new IntGreaterEqualVerifier(0), Optional::No);

    doc.entries.emplace_back("EqualBool", new BoolEqualVerifier(false), Optional::No);
    doc.entries.emplace_back("EqualDouble", new DoubleEqualVerifier(0.0), Optional::No);
    doc.entries.emplace_back("EqualInt", new IntEqualVerifier(0), Optional::No);
    doc.entries.emplace_back("EqualString", new StringEqualVerifier(""), Optional::No);

    doc.entries.emplace_back("UnequalBool", new BoolUnequalVerifier(false), Optional::No);
    doc.entries.emplace_back("UnequalDouble", new DoubleUnequalVerifier(0.0), Optional::No);
    doc.entries.emplace_back("UnequalInt", new IntUnequalVerifier(0), Optional::No);
    doc.entries.emplace_back("UnequalString", new StringUnequalVerifier(""), Optional::No);

    // List Verifiers
    doc.entries.emplace_back("InListBool", new BoolInListVerifier({ true, false }), Optional::No);
    doc.entries.emplace_back("InListDouble", new DoubleInListVerifier({ 0.0, 1.0}), Optional::No);
    doc.entries.emplace_back("InListInt", new IntInListVerifier({ 0, 1 }), Optional::No);
    doc.entries.emplace_back("InListString", new StringInListVerifier({ "", "a" }), Optional::No);

    doc.entries.emplace_back("NotInListBool", new BoolNotInListVerifier({ true, false }), Optional::No);
    doc.entries.emplace_back("NotInListDouble", new DoubleNotInListVerifier({ 0.0, 1.0 }), Optional::No);
    doc.entries.emplace_back("NotInListInt", new IntNotInListVerifier({ 0, 1 }), Optional::No);
    doc.entries.emplace_back("NotInListString", new StringNotInListVerifier({ "", "a" }), Optional::No);

    doc.entries.emplace_back("StringListVerifier", new StringListVerifier, Optional::No);
    doc.entries.emplace_back("IntListVerifier", new IntListVerifier, Optional::No);

    // Range Verifiers
    doc.entries.emplace_back("InListDouble", new DoubleInRangeVerifier({ 0.0, 1.0 }), Optional::No);
    doc.entries.emplace_back("InListInt", new IntInRangeVerifier({ 0, 1 }), Optional::No);

    doc.entries.emplace_back("NotInListDouble", new DoubleNotInRangeVerifier({ 0.0, 1.0 }), Optional::No);
    doc.entries.emplace_back("NotInListInt", new IntNotInRangeVerifier({ 0, 1 }), Optional::No);

    // Misc Verifiers
    doc.entries.emplace_back("AnnotationBool", new BoolAnnotationVerifier("Bool"), Optional::No);
    doc.entries.emplace_back("AnnotationDouble", new DoubleAnnotationVerifier("Double"), Optional::No);
    doc.entries.emplace_back("AnnotationInt", new IntAnnotationVerifier("Int"), Optional::No);
    doc.entries.emplace_back("AnnotationString", new StringAnnotationVerifier("String"), Optional::No);
    doc.entries.emplace_back("AnnotationTable", new TableAnnotationVerifier("Table"), Optional::No);
}

TEST_F(DocumentationTest, InitializerConstructor) {
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

TEST_F(DocumentationTest, BoolVerifier) {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Bool", new BoolVerifier, Optional::No }}
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
        {{ "Double", new DoubleVerifier, Optional::No }}
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
        {{ "Int", new IntVerifier, Optional::No }}
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
        {{ "String", new StringVerifier, Optional::No }}
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
        {{ "Table", new TableVerifier, Optional::No }}
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

TEST_F(DocumentationTest, StringListVerifierType) {
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
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "StringList", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("StringList", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

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
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("StringList.3", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negativeExist {
        { "StringList2", ghoul::Dictionary{} }
    };
    negativeRes = testSpecification(doc, negativeExist);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("StringList", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, IntListVerifierType) {
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
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative{
        { "IntList", 0 }
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("IntList", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

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
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("IntList.1", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);

    ghoul::Dictionary negativeExist {
        { "IntList2", ghoul::Dictionary{} }
    };
    negativeRes = testSpecification(doc, negativeExist);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("IntList", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, MixedVerifiers) {
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
        {
            { "Bool_Force", new BoolVerifier, Optional::No },
            { "Bool_Optional", new BoolVerifier, Optional::Yes }
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

// Exhaustive documentations went away, but we are keeping this test just for funsies
TEST_F(DocumentationTest, Exhaustive) {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntVerifier, Optional::No }}
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
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);

    ghoul::Dictionary negative2 {
        { "Double", 2.0 }
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Int", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, NestedExhaustive) {
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
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Table", ghoul::Dictionary{{ "b", 2.0 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Table.a", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::MissingKey, negativeRes.offenses[0].reason);
}

TEST_F(DocumentationTest, EmptyEntriesNonExhaustive) {
    using namespace openspace::documentation;

    Documentation doc;

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

TEST_F(DocumentationTest, EmptyNestedExhaustive) {
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
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "Table", ghoul::Dictionary{ { "a", 1 }}}
    };
    TestResult negativeRes = testSpecification(doc, negative);
    EXPECT_TRUE(negativeRes.success);
    ASSERT_EQ(0, negativeRes.offenses.size());
}

TEST_F(DocumentationTest, LessInt) {
    using namespace openspace::documentation;

    Documentation doc {
        {{ "Int", new IntLessVerifier(5), Optional::No }}
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
        {{ "Double", new DoubleLessVerifier(5.0), Optional::No }}
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
        {{ "Int", new IntLessEqualVerifier(5), Optional::No }}
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
        {{ "Double", new DoubleLessEqualVerifier(5.0), Optional::No }}
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
        {{ "Int", new IntGreaterVerifier(5), Optional::No }}
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
        {{ "Double", new DoubleGreaterVerifier(5.0), Optional::No }}
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
        {{ "Int", new IntGreaterEqualVerifier(5), Optional::No }}
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
        {{ "Double", new DoubleGreaterEqualVerifier(5.0), Optional::No }}
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
        {{ "Bool", new BoolEqualVerifier(true), Optional::No }}
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
        {{ "Int", new IntEqualVerifier(1), Optional::No }}
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
        {{ "Double", new DoubleEqualVerifier(1.0), Optional::No }}
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
        {{ "String", new StringEqualVerifier("string"s), Optional::No }}
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
        {{ "Bool", new BoolUnequalVerifier(true), Optional::No }}
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
        {{ "Int", new IntUnequalVerifier(1), Optional::No }}
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
        {{ "Double", new DoubleUnequalVerifier(1.0), Optional::No }}
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
        {{ "String", new StringUnequalVerifier("string"s), Optional::No }}
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
        {{ "Bool" , new BoolInListVerifier({ true }), Optional::No }}
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
        {{ "Int" , new IntInListVerifier({ 0, 1, 2 }), Optional::No }}
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
        {{ "Double" , new DoubleInListVerifier({ 0.0, 1.0, 2.0 }), Optional::No }}
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
        {{ "String" , new StringInListVerifier({ "0"s, "1"s, "2"s }), Optional::No }}
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
        {{ "Bool" , new BoolNotInListVerifier({ true }), Optional::No }}
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
        {{ "Int" , new IntNotInListVerifier({ 0, 1, 2 }), Optional::No }}
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
        {{ "Double" , new DoubleNotInListVerifier({ 0.0, 1.0, 2.0 }), Optional::No }}
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
        {{ "String" , new StringNotInListVerifier({ "0"s, "1"s, "2"s }), Optional::No }}
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
        {{ "Bool", new BoolAnnotationVerifier("Bool"), Optional::No }}
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
        {{ "Int", new IntAnnotationVerifier("Int"), Optional::No }}
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
        {{ "Double", new DoubleAnnotationVerifier("Double"), Optional::No }}
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
        {{ "String", new StringAnnotationVerifier("String"), Optional::No }}
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
        {{ "Table", new TableAnnotationVerifier("Table"), Optional::No }}
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
        {{ "Int", new InRangeVerifier<IntVerifier>(0, 5), Optional::No }}
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
        {{ "Double", new InRangeVerifier<DoubleVerifier>(0.0, 5.0), Optional::No }}
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
        {{ "Int", new NotInRangeVerifier<IntVerifier>(0, 5), Optional::No }}
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
        {{ "Double", new NotInRangeVerifier<DoubleVerifier>(0.0, 5.0), Optional::No }}
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
        {{ DocumentationEntry::Wildcard, new IntVerifier, Optional::No }}
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

TEST_F(DocumentationTest, Referencing) {
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

    ghoul::Dictionary negative2 {
        { "Table", ghoul::Dictionary{ { "a", 1 }, { "b", true }}}
    };
    negativeRes = testSpecification(doc, negative2);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Table.b", negativeRes.offenses[0].offender);
    EXPECT_EQ(TestResult::Offense::Reason::WrongType, negativeRes.offenses[0].reason);


    Documentation wrongDoc {{
        { "Table", new ReferencingVerifier("WRONG"), Optional::No }
    } };
    ghoul::Dictionary wrongNegative {
        { "Table", ghoul::Dictionary{ { "a", 1 },{ "b", 2.0 } } }
    };
    negativeRes = testSpecification(wrongDoc, wrongNegative);
    EXPECT_FALSE(negativeRes.success);
    ASSERT_EQ(1, negativeRes.offenses.size());
    EXPECT_EQ("Table", negativeRes.offenses[0].offender);
    EXPECT_EQ(
        TestResult::Offense::Reason::UnknownIdentifier,
        negativeRes.offenses[0].reason
    );
}


TEST_F(DocumentationTest, AndOperator) {
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
        {{ "a", new OrVerifier({ new StringVerifier, new IntVerifier }), Optional::No }}
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
        {{ "a", new BoolVector2Verifier, Optional::No }}
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
        { { "a", new IntVector2Verifier, Optional::No } }
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

    Documentation doc {
        { { "a", new DoubleVector2Verifier, Optional::No } }
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
        { { "a", new BoolVector3Verifier, Optional::No } }
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
        { { "a", new IntVector3Verifier, Optional::No } }
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
        { { "a", new DoubleVector3Verifier, Optional::No } }
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
        { { "a", new BoolVector4Verifier, Optional::No } }
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
        { { "a", new IntVector4Verifier, Optional::No } }
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
        { { "a", new DoubleVector4Verifier, Optional::No } }
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

TEST_F(DocumentationTest, DoubleMatrix2x2Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix2x2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat2x2(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary { { "1", true },{ "2", 1.0 },{ "3", "s" } } }
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

TEST_F(DocumentationTest, DoubleMatrix2x3Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix2x3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat2x3(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
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

TEST_F(DocumentationTest, DoubleMatrix2x4Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix2x4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat2x4(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
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

TEST_F(DocumentationTest, DoubleMatrix3x2Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix3x2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat3x2(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
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

TEST_F(DocumentationTest, DoubleMatrix3x3Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix3x3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat3x3(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
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

TEST_F(DocumentationTest, DoubleMatrix3x4Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix3x4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat3x4(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
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

TEST_F(DocumentationTest, DoubleMatrix4x2Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix4x2Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat4x2(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
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

TEST_F(DocumentationTest, DoubleMatrix4x3Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix4x3Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat4x3(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
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

TEST_F(DocumentationTest, DoubleMatrix4x4Verifier) {
    using namespace openspace::documentation;

    Documentation doc {
        { { "a", new DoubleMatrix4x4Verifier, Optional::No } }
    };

    ghoul::Dictionary positive {
        { "a", glm::dmat4x4(1.0) }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());

    ghoul::Dictionary negative {
        { "a", ghoul::Dictionary{ { "1", true },{ "2", 1.0 },{ "3", "s" } } }
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

TEST_F(DocumentationTest, DeprecatedVerifier) {
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
        { "boolvec2", glm::bvec2() },
        { "intvec2", glm::ivec2() },
        { "doublevec2", glm::dvec2() },
        { "boolvec3", glm::bvec3() },
        { "intvec3", glm::ivec3() },
        { "doublevec3", glm::dvec3() },
        { "boolvec4", glm::bvec4() },
        { "intvec4", glm::ivec4() },
        { "doublevec4", glm::dvec4() }
    };
    TestResult positiveRes = testSpecification(doc, positive);
    EXPECT_TRUE(positiveRes.success);
    EXPECT_EQ(0, positiveRes.offenses.size());
    ASSERT_EQ(13, positiveRes.warnings.size());
    EXPECT_EQ("bool", positiveRes.warnings[0].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[0].reason);
    EXPECT_EQ("boolvec2", positiveRes.warnings[1].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[1].reason);
    EXPECT_EQ("boolvec3", positiveRes.warnings[2].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[2].reason);
    EXPECT_EQ("boolvec4", positiveRes.warnings[3].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[3].reason);
    EXPECT_EQ("double", positiveRes.warnings[4].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[4].reason);
    EXPECT_EQ("doublevec2", positiveRes.warnings[5].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[5].reason);
    EXPECT_EQ("doublevec3", positiveRes.warnings[6].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[6].reason);
    EXPECT_EQ("doublevec4", positiveRes.warnings[7].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[7].reason);
    EXPECT_EQ("int", positiveRes.warnings[8].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[8].reason);
    EXPECT_EQ("intvec2", positiveRes.warnings[9].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[9].reason);
    EXPECT_EQ("intvec3", positiveRes.warnings[10].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[10].reason);
    EXPECT_EQ("intvec4", positiveRes.warnings[11].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[11].reason);
    EXPECT_EQ("string", positiveRes.warnings[12].offender);
    EXPECT_EQ(TestResult::Warning::Reason::Deprecated, positiveRes.warnings[12].reason);
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

    EXPECT_NE("", BoolAnnotationVerifier("A"s).type());
    EXPECT_NE("", IntAnnotationVerifier("A"s).type());
    EXPECT_NE("", DoubleAnnotationVerifier("A"s).type());
    EXPECT_NE("", StringAnnotationVerifier("A"s).type());
    EXPECT_NE("", TableAnnotationVerifier("A"s).type());
    EXPECT_NE("", AnnotationVerifier<BoolVector2Verifier>("A"s).type());
    EXPECT_NE("", AnnotationVerifier<IntVector2Verifier>("A"s).type());
    EXPECT_NE("", AnnotationVerifier<DoubleVector2Verifier>("A"s).type());
    EXPECT_NE("", AnnotationVerifier<BoolVector3Verifier>("A"s).type());
    EXPECT_NE("", AnnotationVerifier<IntVector3Verifier>("A"s).type());
    EXPECT_NE("", AnnotationVerifier<DoubleVector3Verifier>("A"s).type());
    EXPECT_NE("", AnnotationVerifier<BoolVector4Verifier>("A"s).type());
    EXPECT_NE("", AnnotationVerifier<IntVector4Verifier>("A"s).type());
    EXPECT_NE("", AnnotationVerifier<DoubleVector4Verifier>("A"s).type());

    EXPECT_NE("", BoolDeprecatedVerifier().type());
    EXPECT_NE("", IntDeprecatedVerifier().type());
    EXPECT_NE("", DoubleDeprecatedVerifier().type());
    EXPECT_NE("", StringDeprecatedVerifier().type());
    EXPECT_NE("", TableDeprecatedVerifier().type());
    EXPECT_NE("", DeprecatedVerifier<BoolVector2Verifier>().type());
    EXPECT_NE("", DeprecatedVerifier<IntVector2Verifier>().type());
    EXPECT_NE("", DeprecatedVerifier<DoubleVector2Verifier>().type());
    EXPECT_NE("", DeprecatedVerifier<BoolVector3Verifier>().type());
    EXPECT_NE("", DeprecatedVerifier<IntVector3Verifier>().type());
    EXPECT_NE("", DeprecatedVerifier<DoubleVector3Verifier>().type());
    EXPECT_NE("", DeprecatedVerifier<BoolVector4Verifier>().type());
    EXPECT_NE("", DeprecatedVerifier<IntVector4Verifier>().type());
    EXPECT_NE("", DeprecatedVerifier<DoubleVector4Verifier>().type());

    EXPECT_NE("", ReferencingVerifier("identifier"s).type());
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

    EXPECT_NE("", BoolAnnotationVerifier("A"s).documentation());
    EXPECT_NE("", IntAnnotationVerifier("A"s).documentation());
    EXPECT_NE("", DoubleAnnotationVerifier("A"s).documentation());
    EXPECT_NE("", StringAnnotationVerifier("A"s).documentation());
    EXPECT_NE("", TableAnnotationVerifier("A"s).documentation());
    EXPECT_NE("", AnnotationVerifier<BoolVector2Verifier>("A"s).documentation());
    EXPECT_NE("", AnnotationVerifier<IntVector2Verifier>("A"s).documentation());
    EXPECT_NE("", AnnotationVerifier<DoubleVector2Verifier>("A"s).documentation());
    EXPECT_NE("", AnnotationVerifier<BoolVector3Verifier>("A"s).documentation());
    EXPECT_NE("", AnnotationVerifier<IntVector3Verifier>("A"s).documentation());
    EXPECT_NE("", AnnotationVerifier<DoubleVector3Verifier>("A"s).documentation());
    EXPECT_NE("", AnnotationVerifier<BoolVector4Verifier>("A"s).documentation());
    EXPECT_NE("", AnnotationVerifier<IntVector4Verifier>("A"s).documentation());
    EXPECT_NE("", AnnotationVerifier<DoubleVector4Verifier>("A"s).documentation());

    EXPECT_NE("", BoolDeprecatedVerifier().documentation());
    EXPECT_NE("", IntDeprecatedVerifier().documentation());
    EXPECT_NE("", DoubleDeprecatedVerifier().documentation());
    EXPECT_NE("", StringDeprecatedVerifier().documentation());
    EXPECT_NE("", TableDeprecatedVerifier().documentation());
    EXPECT_NE("", DeprecatedVerifier<BoolVector2Verifier>().documentation());
    EXPECT_NE("", DeprecatedVerifier<IntVector2Verifier>().documentation());
    EXPECT_NE("", DeprecatedVerifier<DoubleVector2Verifier>().documentation());
    EXPECT_NE("", DeprecatedVerifier<BoolVector3Verifier>().documentation());
    EXPECT_NE("", DeprecatedVerifier<IntVector3Verifier>().documentation());
    EXPECT_NE("", DeprecatedVerifier<DoubleVector3Verifier>().documentation());
    EXPECT_NE("", DeprecatedVerifier<BoolVector4Verifier>().documentation());
    EXPECT_NE("", DeprecatedVerifier<IntVector4Verifier>().documentation());
    EXPECT_NE("", DeprecatedVerifier<DoubleVector4Verifier>().documentation());

    EXPECT_NE("", ReferencingVerifier("identifier"s).documentation());

}
