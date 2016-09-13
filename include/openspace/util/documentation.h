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

struct TestResult {
    bool success;
    std::vector<std::string> offenders;
};

struct Verifier {
    virtual TestResult operator()(const ghoul::Dictionary& dict,
        const std::string& key) const;

    virtual bool test(const ghoul::Dictionary& dict, const std::string& key) const;

    virtual std::string documentation() const = 0;
};


struct DocumentationEntry {
    DocumentationEntry(std::string key, Verifier* t, bool optional = false,
                       std::string doc = "");

    std::string key;
    std::shared_ptr<Verifier> tester;
    bool optional;
    std::string documentation;
};

using Documentation = std::vector<DocumentationEntry>;

TestResult testSpecification(const Documentation& d, const ghoul::Dictionary& dictionary);

std::string generateDocumentation(const Documentation& d);

// General verifiers
template <typename T>
struct TemplateVerifier : public Verifier {
    using Type = T;
};

struct BoolVerifier : public TemplateVerifier<bool> {
    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;
};

struct DoubleVerifier : public TemplateVerifier<double> {
    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;
};

struct IntVerifier : public TemplateVerifier<int> {
    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;
};

struct StringVerifier : public TemplateVerifier<std::string> {
    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;
};

struct TableVerifier : public TemplateVerifier<ghoul::Dictionary> {
    TableVerifier(Documentation d);

    TestResult operator()(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    Documentation doc;
};

// Operator Verifiers

template <typename T>
struct LessVerifier : public T {
    LessVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const;

    typename T::Type value;
};

template <typename T>
struct LessEqualVerifier : public T {
    LessEqualVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

template <typename T>
struct GreaterVerifier : public T {
    GreaterVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

template <typename T>
struct GreaterEqualVerifier : public T {
    GreaterEqualVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

template <typename T>
struct EqualVerifier : public T {
    EqualVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

template <typename T>
struct UnequalVerifier : public T {
    UnequalVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

// List Verifiers

template <typename T>
struct InListVerifier : public T {
    InListVerifier(std::vector<typename T::Type> values);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    std::vector<typename T::Type> values;
};

template <typename T>
struct NotInListVerifier : public T {
    NotInListVerifier(std::vector<typename T::Type> values);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    std::vector<typename T::Type> values;
};

// Misc Verifiers

template <typename T>
struct AnnotationVerifier : public T {
    AnnotationVerifier(std::string annotation);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    std::string annotation;
};

} // namespace documentation
} // namespace openspace

#include "documentation.inl"

#endif // __DOCUMENTATION_H__
