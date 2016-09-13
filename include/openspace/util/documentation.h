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

#ifndef __DOCUMENTATION_H__
#define __DOCUMENTATION_H__

#include <ghoul/misc/assert.h>
#include <ghoul/misc/dictionary.h>

#include <iterator>
#include <map>
#include <memory>
#include <string>
#include <vector>

namespace openspace {
namespace documentation {

struct AbstractVerifier {
    using Success = bool;
    using Offender = std::vector<std::string>;
    using TestResult = std::tuple<Success, Offender>;

    virtual TestResult operator()(const ghoul::Dictionary& dict, const std::string& key) const {
        bool testSuccess = test(dict, key);
        if (testSuccess) {
            return { testSuccess, {} };
        }
        else {
            return { testSuccess, {key} };
        }
    }

    virtual bool test(const ghoul::Dictionary& dict, const std::string& key) const {
        return false;
    };

    virtual std::string documentation() const = 0;
};


struct DocumentationEntry {
    DocumentationEntry(std::string key, AbstractVerifier* t, bool optional = false, std::string doc = "")
        : key(std::move(key))
        , tester(std::move(t))
        , optional(optional)
        , documentation(std::move(doc)) {}

    std::string key;
    std::unique_ptr<AbstractVerifier> tester;
    bool optional;
    std::string documentation;
};

using Documentation = std::vector<DocumentationEntry>;


std::tuple<bool, std::vector<std::string>> testSpecification(const Documentation& d, const ghoul::Dictionary& dictionary) {
    bool success = true;
    std::vector<std::string> offenders;

    for (const auto& p : d) {
        AbstractVerifier& verifier = *(p.tester);
        AbstractVerifier::TestResult res = verifier(dictionary, p.key);
        if (!std::get<0>(res)) {
            success = false;
            offenders.insert(
                offenders.end(),
                std::get<1>(res).begin(),
                std::get<1>(res).end()
            );
        }
    }
    return { success, offenders };
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

template <typename T>
struct TemplateVerifier : public AbstractVerifier {
    using Type = T;
};

struct BoolVerifier : public TemplateVerifier<bool> {
    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        return dict.hasKeyAndValue<Type>(key);
    }

    std::string documentation() const override {
        return "Type: Boolean";
    }
};

struct DoubleVerifier : public TemplateVerifier<double> {
    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        return dict.hasKeyAndValue<Type>(key);
    }

    std::string documentation() const override {
        return "Type: Double";
    }
};

struct IntVerifier : public TemplateVerifier<int> {
    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
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

    std::string documentation() const override {
        return "Type: Integer";
    }
};

struct StringVerifier : public TemplateVerifier<std::string> {
    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        return dict.hasKeyAndValue<Type>(key);
    }

    std::string documentation() const override {
        return "Type: String";
    }
};

struct TableVerifier : public TemplateVerifier<ghoul::Dictionary> {
    TableVerifier(Documentation d) : doc(std::move(d)) {}

    TestResult operator()(const ghoul::Dictionary& dict, const std::string& key) const override {
        if (dict.hasKeyAndValue<Type>(key)) {
            ghoul::Dictionary d = dict.value<Type>(key);
            AbstractVerifier::TestResult res = testSpecification(doc, d);
            
            for (std::string& s : std::get<1>(res)) {
                s = key + "." + s;
            }

            return res;
        }
        return{ dict.hasKeyAndValue<Type>(key), {} };
    }

    std::string documentation() const override {
        return "Type: Table" + '\n' + generateDocumentation(doc);
    }

    Documentation doc;
};

template <typename T>
struct LessVerifier : public T {
    LessVerifier(typename T::Type value) : value(std::move(value)) {}

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        return T::test(dict, key) && dict.value<Type>(key) < value;
    }

    std::string documentation() const override {
        return T::documentation() + '\n' + "Less than: " + std::to_string(value);
    }

    typename T::Type value;
};

template <typename T>
struct LessEqualVerifier : public T {
    LessEqualVerifier(typename T::Type value) : value(std::move(value)) {}

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        return T::test(dict, key) && dict.value<Type>(key) <= value;
    }

    std::string documentation() const override {
        return T::documentation() + '\n' + "Less or equal to: " + std::to_string(value);
    }

    typename T::Type value;
};

template <typename T>
struct GreaterVerifier : public T {
    GreaterVerifier(typename T::Type value) : value(std::move(value)) {}

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        return T::test(dict, key) && dict.value<Type>(key) > value;
    }

    std::string documentation() const override {
        return T::documentation() + '\n' + "Greater than: " + std::to_string(value);
    }

    typename T::Type value;
};

template <typename T>
struct GreaterEqualVerifier : public T {
    GreaterEqualVerifier(typename T::Type value) : value(std::move(value)) {}

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        return T::test(dict, key) && dict.value<Type>(key) >= value;
    }

    std::string documentation() const override {
        return T::documentation() + '\n' + "Greater or equal to: " + std::to_string(value);
    }

    typename T::Type value;
};

template <typename T>
struct EqualVerifier : public T {
    EqualVerifier(typename T::Type value) : value(std::move(value)) {}

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        return T::test(dict, key) && dict.value<Type>(key) == value;
    }

    std::string documentation() const override {
        return T::documentation() + '\n' + "Equal to: " + std::to_string(value);
    }

    typename T::Type value;
};

template <typename T>
struct UnequalVerifier : public T {
    UnequalVerifier(typename T::Type value) : value(std::move(value)) {}

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        return T::test(dict, key) && dict.value<Type>(key) != value;
    }

    std::string documentation() const override {
        return T::documentation() + '\n' + "Unequal to: " + std::to_string(value);
    }

    typename T::Type value;
};

template <typename T>
struct InListVerifier : public T {
    InListVerifier(std::vector<typename T::Type> values) : values(std::move(values)) {}

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        if (T::test(dict, key)) {
            typename T::Type value = dict.value<typename T::Type>(key);

            auto it = std::find(values.begin(), values.end(), value);
            return it != values.end();
        }
        else {
            return false;
        }
    }

    std::string documentation() const override {
        std::string result = T::documentation() + '\n' + "In list {";

        std::stringstream s;
        std::copy(values.begin(), values.end(), std::ostream_iterator<typename T::Type>(s, ","));

        std::string joined = s.str();
        // We need to remove a trailing ',' at the end of the string
        result += joined.substr(0, joined.size() - 1);

        result += "}";
        return result;
    }

    std::vector<typename T::Type> values;
};

template <typename T>
struct NotInListVerifier : public T {
    NotInListVerifier(std::vector<typename T::Type> values) : values(std::move(values)) {}

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override {
        if (T::test(dict, key)) {
            typename T::Type value = dict.value<typename T::Type>(key);

            auto it = std::find(values.begin(), values.end(), value);
            return it == values.end();
        }
        else {
            return false;
        }
    }

    std::string documentation() const override {
        std::string result = T::documentation() + '\n' + "Not in list {";

        std::stringstream s;
        std::copy(values.begin(), values.end(), std::ostream_iterator<typename T::Type>(s, ","));

        std::string joined = s.str();
        // We need to remove a trailing ',' at the end of the string
        result += joined.substr(0, joined.size() - 1);


        result += "}";
        return result;
    }

    std::vector<typename T::Type> values;
};






} // namespace documentation
} // namespace openspace

#endif // __DOCUMENTATION_H__
