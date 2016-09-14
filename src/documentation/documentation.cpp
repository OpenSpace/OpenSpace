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

#include <openspace/documentation/documentation.h>

#include <set>

namespace {
    const std::string Wildcard = "*";
} // namespace

namespace std {
std::string to_string(std::string value) {
    return value; 
}
} // namespace std

namespace openspace {
namespace documentation {

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
        return{ testSuccess, {} };
    }
    else {
        return{ testSuccess, { key } };
    }
}

bool Verifier::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return false;
};

DocumentationEntry::DocumentationEntry(std::string key, Verifier* t,
                                       Optional optional, std::string doc)
    : key(std::move(key))
    , tester(std::move(t))
    , optional(optional)
    , documentation(std::move(doc)) {}

TestResult testSpecification(const Documentation& d, const ghoul::Dictionary& dictionary){
    TestResult result;
    result.success = true;

    for (const auto& p : d) {
        if (p.key == Wildcard) {
            for (const std::string& key : dictionary.keys()) {
                Verifier& verifier = *(p.tester);
                TestResult res = verifier(dictionary, key);
                if (!res.success) {
                    result.success = false;
                    result.offenders.insert(
                        result.offenders.end(),
                        res.offenders.begin(),
                        res.offenders.end()
                    );
                }
            }
        }
        else {
            if (p.optional && !dictionary.hasKey(p.key)) {
                // If the key is optional and it doesn't exist, we don't need to check it
                // if the key exists, it has to be correct, however
                continue;
            }
            Verifier& verifier = *(p.tester);
            TestResult res = verifier(dictionary, p.key);
            if (!res.success) {
                result.success = false;
                result.offenders.insert(
                    result.offenders.end(),
                    res.offenders.begin(),
                    res.offenders.end()
                );
            }
        }
    }

    // Make the offenders unique so that they only appear once in the list
    std::set<std::string> uniqueOffenders(
        result.offenders.begin(), result.offenders.end()
    );
    result.offenders = std::vector<std::string>(
        uniqueOffenders.begin(), uniqueOffenders.end()
    );

    return result;
}

std::string generateDocumentation(const Documentation& d) {
    using namespace std::string_literals;
    std::string result;

    for (const auto& p : d) {
        result += p.key + '\n';
        result += "Optional: "s + (p.optional ? "true" : "false") + '\n';
        result += p.tester->documentation() + '\n';
        result += '\n';
        result += p.documentation + '\n';
    }

    return result;
}

bool BoolVerifier::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return dict.hasKeyAndValue<Type>(key);
}

std::string BoolVerifier::documentation() const {
    return "Type: Boolean";
}

bool DoubleVerifier::test(const ghoul::Dictionary & dict, const std::string & key) const {
    return dict.hasKeyAndValue<Type>(key);
}

std::string DoubleVerifier::documentation() const {
    return "Type: Double";
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

std::string IntVerifier::documentation() const {
    return "Type: Integer";
}

bool StringVerifier::test(const ghoul::Dictionary & dict, const std::string & key) const {
    return dict.hasKeyAndValue<Type>(key);
}

std::string StringVerifier::documentation() const {
    return "Type: String";
}

TableVerifier::TableVerifier(Documentation d) 
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
    return{ dict.hasKeyAndValue<Type>(key), { key } };
}

std::string TableVerifier::documentation() const {
    return "Type: Table" + '\n' + generateDocumentation(doc);
}

} // namespace documentation
} // namespace openspace
