/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <openspace/documentation/documentationengine.h>

#include <algorithm>

namespace openspace::documentation {

// The explicit template instantiations for many of the commonly used template values
// This cuts down on the compilation time by only compiling these once
template struct Vector2Verifier<bool>;
template struct Vector2Verifier<int>;
template struct Vector2Verifier<double>;
template struct Vector3Verifier<bool>;
template struct Vector3Verifier<int>;
template struct Vector3Verifier<double>;
template struct Vector4Verifier<bool>;
template struct Vector4Verifier<int>;
template struct Vector4Verifier<double>;

template struct Matrix2x2Verifier<double>;
template struct Matrix2x3Verifier<double>;
template struct Matrix2x4Verifier<double>;
template struct Matrix3x2Verifier<double>;
template struct Matrix3x3Verifier<double>;
template struct Matrix3x4Verifier<double>;
template struct Matrix4x2Verifier<double>;
template struct Matrix4x3Verifier<double>;
template struct Matrix4x4Verifier<double>;

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
template struct AnnotationVerifier<BoolVector2Verifier>;
template struct AnnotationVerifier<IntVector2Verifier>;
template struct AnnotationVerifier<DoubleVector2Verifier>;
template struct AnnotationVerifier<BoolVector3Verifier>;
template struct AnnotationVerifier<IntVector3Verifier>;
template struct AnnotationVerifier<DoubleVector3Verifier>;
template struct AnnotationVerifier<BoolVector4Verifier>;
template struct AnnotationVerifier<IntVector4Verifier>;
template struct AnnotationVerifier<DoubleVector4Verifier>;

template struct DeprecatedVerifier<BoolVerifier>;
template struct DeprecatedVerifier<IntVerifier>;
template struct DeprecatedVerifier<DoubleVerifier>;
template struct DeprecatedVerifier<StringVerifier>;
template struct DeprecatedVerifier<TableVerifier>;
template struct DeprecatedVerifier<BoolVector2Verifier>;
template struct DeprecatedVerifier<IntVector2Verifier>;
template struct DeprecatedVerifier<DoubleVector2Verifier>;
template struct DeprecatedVerifier<BoolVector3Verifier>;
template struct DeprecatedVerifier<IntVector3Verifier>;
template struct DeprecatedVerifier<DoubleVector3Verifier>;
template struct DeprecatedVerifier<BoolVector4Verifier>;
template struct DeprecatedVerifier<IntVector4Verifier>;
template struct DeprecatedVerifier<DoubleVector4Verifier>;

std::string BoolVerifier::type() const {
    return "Boolean";
}

std::string DoubleVerifier::type() const {
    return "Double";
}

TestResult IntVerifier::operator()(const ghoul::Dictionary & dict,
                                   const std::string & key) const {
    if (dict.hasKeyAndValue<int>(key)) {
        // We we have a key and the value is int, we are done
        return { true, {}, {} };
    }
    else {
        if (dict.hasKey(key)) {
            if (dict.hasValue<double>(key)) {
                // If we have a double value, we need to check if it is integer
                double value = dict.value<double>(key);
                double intPart;
                bool isInt = modf(value, &intPart) == 0.0;
                if (isInt) {
                    return { true, {}, {} };
                }
                else {
                    return {
                        false,
                        { { key, TestResult::Offense::Reason::WrongType } },
                        {}
                    };
                }
            }
            else {
                // If we don't have a double value, we cannot have an int value
                return { false, {{ key, TestResult::Offense::Reason::WrongType }}, {} };
            }
        }
        else {
            return { false, {{ key, TestResult::Offense::Reason::MissingKey }}, {} };
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
    : documentations(std::move(d))
    , exhaustive(std::move(exhaustive))
{}

TestResult TableVerifier::operator()(const ghoul::Dictionary& dict,
                                     const std::string& key) const {
    if (dict.hasKeyAndValue<Type>(key)) {
        ghoul::Dictionary d = dict.value<ghoul::Dictionary>(key);
        TestResult res = testSpecification({documentations, exhaustive}, d);

        // Add the 'key' as a prefix to make the new offender a fully qualified identifer
        for (TestResult::Offense& s : res.offenses) {
            s.offender = key + "." + s.offender;
        }

        // Add the 'key' as a prefix to make the new warning a fully qualified identifer
        for (TestResult::Warning& w : res.warnings) {
            w.offender = key + "." + w.offender;
        }

        return res;
    }
    else {
        if (dict.hasKey(key)) {
            return { false, { { key, TestResult::Offense::Reason::WrongType } }, {} };

        }
        else {
            return { false, { { key, TestResult::Offense::Reason::MissingKey } }, {} };
        }
    }
}

std::string TableVerifier::type() const {
    return "Table";
}

StringListVerifier::StringListVerifier(std::string elementDocumentation)
    : TableVerifier({{ "*", new StringVerifier, std::move(elementDocumentation) }})
{}

std::string StringListVerifier::type() const {
    return "List of strings";
}

IntListVerifier::IntListVerifier(std::string elementDocumentation)
    : TableVerifier({ { "*", new IntVerifier, std::move(elementDocumentation) } })
{}

std::string IntListVerifier::type() const {
    return "List of ints";
}

ReferencingVerifier::ReferencingVerifier(std::string id)
    : identifier(std::move(id))
{
    ghoul_assert(!identifier.empty(), "identifier must not be empty");
}

TestResult ReferencingVerifier::operator()(const ghoul::Dictionary& dictionary,
                                           const std::string& key) const
{
    TestResult res = TableVerifier::operator()(dictionary, key);
    if (res.success) {
        std::vector<Documentation> docs = DocEng.documentations();

        auto it = std::find_if(
            docs.begin(),
            docs.end(),
            [this](const Documentation& doc) { return doc.id == identifier; }
        );

        if (it == docs.end()) {
            res.offenses.push_back({
                key,
                TestResult::Offense::Reason::UnknownIdentifier
            });
            res.success = false;
            return res;
        }

        //ghoul_assert(
        //    it != docs.end(),
        //    "Did not find referencing identifier '" + identifier + "'"
        //);

        ghoul::Dictionary d = dictionary.value<ghoul::Dictionary>(key);
        TestResult r = testSpecification(*it, d);

        // Add the 'key' as a prefix to make the offender a fully qualified identifer
        for (TestResult::Offense& s : r.offenses) {
            s.offender = key + "." + s.offender;
        }

        // Add the 'key' as a prefix to make the warning a fully qualified identifer
        for (TestResult::Warning& w : r.warnings) {
            w.offender = key + "." + w.offender;
        }

        return r;
    }
    else {
        return res;
    }
}

std::string ReferencingVerifier::documentation() const {
    using namespace std::string_literals;
    return "Referencing Documentation: '"s + identifier + "'";
}

AndVerifier::AndVerifier(Verifier* lhs, Verifier* rhs)
    : lhs(lhs)
    , rhs(rhs)
{
    ghoul_assert(lhs, "lhs must not be nullptr");
    ghoul_assert(rhs, "rhs must not be nullptr");
}

TestResult AndVerifier::operator()(const ghoul::Dictionary& dict,
                                   const std::string& key) const 
{
    TestResult resLhs = lhs->operator()(dict, key);
    TestResult resRhs = rhs->operator()(dict, key);

    if (resLhs.success && resRhs.success) {
        return { true, {}, {} };
    }
    else {
        return { false, { { key, TestResult::Offense::Reason::Verification } }, {} };
    }
}

std::string AndVerifier::type() const {
    if (lhs->type() != rhs->type()) {
        return lhs->type() + " and " + rhs->type();
    }
    else {
        return lhs->type();
    }
}

std::string AndVerifier::documentation() const {
    return lhs->documentation() + " and " + rhs->documentation();
}

OrVerifier::OrVerifier(Verifier* lhs, Verifier* rhs)
    : lhs(lhs)
    , rhs(rhs)
{
    ghoul_assert(lhs, "lhs must not be nullptr");
    ghoul_assert(rhs, "rhs must not be nullptr");
}

TestResult OrVerifier::operator()(const ghoul::Dictionary& dict,
                                   const std::string& key) const {
    TestResult resA = lhs->operator()(dict, key);
    TestResult resB = rhs->operator()(dict, key);

    if (resA.success || resB.success) {
        return { true, {}, {} };
    }
    else {
        return { false, { { key, TestResult::Offense::Reason::Verification } }, {} };
    }
}

std::string OrVerifier::type() const {
    if (lhs->type() != rhs->type()) {
        return lhs->type() + " or " + rhs->type();
    }
    else {
        return lhs->type();
    }
}

std::string OrVerifier::documentation() const {
    return lhs->documentation() + " or " + rhs->documentation();
}


} // namespace openspace::documentation
