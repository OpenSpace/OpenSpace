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

#include <openspace/documentation/verifier.h>

namespace openspace {
namespace documentation {

template struct Vector2Verifier<bool>;
template struct Vector2Verifier<int>;
template struct Vector2Verifier<double>;
template struct Vector3Verifier<bool>;
template struct Vector3Verifier<int>;
template struct Vector3Verifier<double>;
template struct Vector4Verifier<bool>;
template struct Vector4Verifier<int>;
template struct Vector4Verifier<double>;

template struct LessVerifier<IntVerifier>;
template struct LessVerifier<DoubleVerifier>;
template struct LessEqualVerifier<IntVerifier>;
template struct LessEqualVerifier<DoubleVerifier>;
template struct GreaterVerifier<IntVerifier>;
template struct GreaterVerifier<DoubleVerifier>;
template struct GreaterEqualVerifier<IntVerifier>;
template struct GreaterEqualVerifier<DoubleVerifier>;
template struct EqualVerifier<BoolVerifier>;
template struct EqualVerifier<IntVerifier>;
template struct EqualVerifier<DoubleVerifier>;
template struct EqualVerifier<StringVerifier>;
template struct UnequalVerifier<BoolVerifier>;
template struct UnequalVerifier<IntVerifier>;
template struct UnequalVerifier<DoubleVerifier>;
template struct UnequalVerifier<StringVerifier>;

template struct InListVerifier<BoolVerifier>;
template struct InListVerifier<IntVerifier>;
template struct InListVerifier<DoubleVerifier>;
template struct InListVerifier<StringVerifier>;
template struct NotInListVerifier<BoolVerifier>;
template struct NotInListVerifier<IntVerifier>;
template struct NotInListVerifier<DoubleVerifier>;
template struct NotInListVerifier<StringVerifier>;

template struct InRangeVerifier<IntVerifier>;
template struct InRangeVerifier<DoubleVerifier>;
template struct NotInRangeVerifier<IntVerifier>;
template struct NotInRangeVerifier<DoubleVerifier>;

template struct AnnotationVerifier<BoolVerifier>;
template struct AnnotationVerifier<IntVerifier>;
template struct AnnotationVerifier<DoubleVerifier>;
template struct AnnotationVerifier<StringVerifier>;
template struct AnnotationVerifier<TableVerifier>;

std::string Verifier::documentation() const {
    return "";
}

std::string BoolVerifier::type() const {
    return "Boolean";
}

std::string DoubleVerifier::type() const {
    return "Double";
}

TestResult IntVerifier::operator()(const ghoul::Dictionary & dict,
                             const std::string & key) const
{
    if (dict.hasKeyAndValue<int>(key)) {
        return { true, {} };
    }
    else {
        if (dict.hasKey(key)) {
            if (dict.hasValue<double>(key)) {
                // If we have a double value, we need to check if it is integer
                double value = dict.value<double>(key);
                double intPart;
                bool isInt = modf(value, &intPart) == 0.0;
                if (isInt) {
                    return { true,{} };
                }
                else {
                    return { false, { { key, TestResult::Offense::Reason::WrongType } } };
                }
            }
            else {
                // If we don't have a double value, we cannot have an int value
                return { false, { { key, TestResult::Offense::Reason::WrongType } } };
            }
        }
        else {
            return { false, { {key, TestResult::Offense::Reason::MissingKey }}};
        }
    }
}

std::string IntVerifier::type() const {
    return "Integer";
}

std::string StringVerifier::type() const {
    return "String";
}

TableVerifier::TableVerifier(std::vector<DocumentationEntry> d, Exhaustive exhaustive)
    : doc(std::move(d))
    , exhaustive(std::move(exhaustive))
{}

TestResult TableVerifier::operator()(const ghoul::Dictionary& dict,
                                     const std::string& key) const
{
    if (dict.hasKeyAndValue<Type>(key)) {
        ghoul::Dictionary d = dict.value<ghoul::Dictionary>(key);
        TestResult res = testSpecification({ "", doc, exhaustive }, d);

        for (TestResult::Offense& s : res.offenses) {
            s.offender = key + "." + s.offender;
        }

        return res;
    }
    else {
        if (dict.hasKey(key)) {
            return { false, { { key, TestResult::Offense::Reason::WrongType } } };

        }
        else {
            return { false, { { key, TestResult::Offense::Reason::MissingKey } } };
        }
    }
}

std::string TableVerifier::type() const {
    return "Table";
}

AndVerifier::AndVerifier(Verifier* a, Verifier* b)
    : a(a)
    , b(b) 
{
    ghoul_assert(a->type() == b->type(), "Cannot use AndVerifier with different types");
}

TestResult AndVerifier::operator()(const ghoul::Dictionary& dict,
                                   const std::string& key) const 
{
    TestResult resA = a->operator()(dict, key);
    TestResult resB = b->operator()(dict, key);

    if (resA.success && resB.success) {
        return { true, {} };
    }
    else {
        return { false, { { key, TestResult::Offense::Reason::Verification } } };
    }
}

std::string AndVerifier::type() const {
    // It does not matter which type we choose as they both have to be the same
    return a->type();
}

std::string AndVerifier::documentation() const {
    return a->documentation() + " and " + b->documentation();
}

OrVerifier::OrVerifier(Verifier* a, Verifier* b)
    : a(a)
    , b(b) 
{}

TestResult OrVerifier::operator()(const ghoul::Dictionary& dict,
                                   const std::string& key) const {
    TestResult resA = a->operator()(dict, key);
    TestResult resB = b->operator()(dict, key);

    if (resA.success || resB.success) {
        return { true, {} };
    }
    else {
        return { false, { { key, TestResult::Offense::Reason::Verification } } };
    }
}

std::string OrVerifier::type() const {
    if (a->type() != b->type()) {
        return a->type() + " or " + b->type();
    }
    else {
        return a->type();
    }
}

std::string OrVerifier::documentation() const {
    return a->documentation() + " or " + b->documentation();
}


} // namespace documentation
} // namespace openspace
