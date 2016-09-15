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

TestResult Verifier::operator()(const ghoul::Dictionary& dict,
                                const std::string& key) const
{
    bool testSuccess = test(dict, key);
    if (testSuccess) {
        return { testSuccess,{} };
    }
    else {
        return { testSuccess,{ key } };
    }
}

bool Verifier::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return false;
};

std::string Verifier::documentation() const {
    return "";
}

bool BoolVerifier::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return dict.hasKeyAndValue<Type>(key);
}

std::string BoolVerifier::type() const {
    return "Boolean";
}

bool DoubleVerifier::test(const ghoul::Dictionary & dict, const std::string & key) const {
    return dict.hasKeyAndValue<Type>(key);
}

std::string DoubleVerifier::type() const {
    return "Double";
}

bool IntVerifier::test(const ghoul::Dictionary & dict, const std::string & key) const {
    if (dict.hasKeyAndValue<int>(key)) {
        return true;
    }
    else {
        if (dict.hasKeyAndValue<double>(key)) {
            // If we have a double value, we need to check if it is integer
            double value = dict.value<double>(key);
            double intPart;
            return modf(value, &intPart) == 0.0;
        }
        else {
            // If we don't have a double value, we cannot have an int value
            return false;
        }
    }
}

std::string IntVerifier::type() const {
    return "Integer";
}

bool StringVerifier::test(const ghoul::Dictionary & dict, const std::string & key) const {
    return dict.hasKeyAndValue<Type>(key);
}

std::string StringVerifier::type() const {
    return "String";
}

TableVerifier::TableVerifier(std::vector<DocumentationEntry> d)
    : doc(std::move(d))
{}

TestResult TableVerifier::operator()(const ghoul::Dictionary& dict,
                                     const std::string& key) const
{
    if (dict.hasKeyAndValue<Type>(key)) {
        ghoul::Dictionary d = dict.value<Type>(key);
        TestResult res = testSpecification(doc, d);

        for (std::string& s : res.offenders) {
            s = key + "." + s;
        }

        return res;
    }
    return { dict.hasKeyAndValue<Type>(key), { key } };
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

bool AndVerifier::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return a->test(dict, key) && b->test(dict, key);
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

bool OrVerifier::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return a->test(dict, key) || b->test(dict, key);
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
