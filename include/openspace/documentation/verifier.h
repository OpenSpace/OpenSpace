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

#ifndef __VERIFIER_H__
#define __VERIFIER_H__

#include <openspace/documentation/documentation.h>

namespace openspace {
namespace documentation {

struct Verifier {
    virtual TestResult operator()(const ghoul::Dictionary& dict,
                                  const std::string& key) const;

    virtual bool test(const ghoul::Dictionary& dict, const std::string& key) const;

    virtual std::string documentation() const = 0;
};

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
    TableVerifier(Documentation d = {});

    TestResult operator()(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    Documentation doc;
};

// Operator Verifiers

template <typename T>
struct LessVerifier : public T {
    static_assert(!std::is_base_of_v<BoolVerifier, T>, "T cannot be BoolVerifier");
    static_assert(!std::is_base_of_v<StringVerifier, T>, "T cannot be StringVerifier");
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    LessVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const;

    typename T::Type value;
};

template <typename T>
struct LessEqualVerifier : public T {
    static_assert(!std::is_base_of_v<BoolVerifier, T>, "T cannot be BoolVerifier");
    static_assert(!std::is_base_of_v<StringVerifier, T>, "T cannot be StringVerifier");
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    LessEqualVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

template <typename T>
struct GreaterVerifier : public T {
    static_assert(!std::is_base_of_v<BoolVerifier, T>, "T cannot be BoolVerifier");
    static_assert(!std::is_base_of_v<StringVerifier, T>, "T cannot be StringVerifier");
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    GreaterVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

template <typename T>
struct GreaterEqualVerifier : public T {
    static_assert(!std::is_base_of_v<BoolVerifier, T>, "T cannot be BoolVerifier");
    static_assert(!std::is_base_of_v<StringVerifier, T>, "T cannot be StringVerifier");
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    GreaterEqualVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

template <typename T>
struct EqualVerifier : public T {
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    EqualVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

template <typename T>
struct UnequalVerifier : public T {
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    UnequalVerifier(typename T::Type value);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type value;
};

// List Verifiers

template <typename T>
struct InListVerifier : public T {
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    InListVerifier(std::vector<typename T::Type> values);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    std::vector<typename T::Type> values;
};

template <typename T>
struct NotInListVerifier : public T {
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    NotInListVerifier(std::vector<typename T::Type> values);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    std::vector<typename T::Type> values;
};

// Range Verifiers
template <typename T>
struct InRangeVerifier : public T {
    static_assert(!std::is_base_of_v<BoolVerifier, T>, "T cannot be BoolVerifier");
    static_assert(!std::is_base_of_v<StringVerifier, T>, "T cannot be StringVerifier");
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    InRangeVerifier(typename T::Type lower, typename T::Type upper);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type lower;
    typename T::Type upper;
};

template <typename T>
struct NotInRangeVerifier : public T {
    static_assert(!std::is_base_of_v<BoolVerifier, T>, "T cannot be BoolVerifier");
    static_assert(!std::is_base_of_v<StringVerifier, T>, "T cannot be StringVerifier");
    static_assert(!std::is_base_of_v<TableVerifier, T>, "T cannot be TableVerifier");

    NotInRangeVerifier(typename T::Type lower, typename T::Type upper);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    typename T::Type lower;
    typename T::Type upper;
};

// Misc Verifiers

template <typename T>
struct AnnotationVerifier : public T {
    AnnotationVerifier(std::string annotation);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    std::string annotation;
};

// Boolean Verifiers

struct AndVerifier : public Verifier {
    AndVerifier(Verifier* a, Verifier* b);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    std::shared_ptr<Verifier> a;
    std::shared_ptr<Verifier> b;
};

struct OrVerifier : public Verifier {
    OrVerifier(Verifier* a, Verifier* b);

    bool test(const ghoul::Dictionary& dict, const std::string& key) const override;

    std::string documentation() const override;

    std::shared_ptr<Verifier> a;
    std::shared_ptr<Verifier> b;
};

using IntLessVerifier = LessVerifier<IntVerifier>;
using DoubleLessVerifier = LessVerifier<DoubleVerifier>;
using IntLessEqualVerifier = LessEqualVerifier<IntVerifier>;
using DoubleLessEqualVerifier = LessEqualVerifier<DoubleVerifier>;
using IntGreaterVerifier = GreaterVerifier<IntVerifier>;
using DoubleGreaterVerifier = GreaterVerifier<DoubleVerifier>;
using IntGreaterEqualVerifier = GreaterEqualVerifier<IntVerifier>;
using DoubleGreaterEqualVerifier = GreaterEqualVerifier<DoubleVerifier>;
using BoolEqualVerifier = EqualVerifier<BoolVerifier>;
using IntEqualVerifier = EqualVerifier<IntVerifier>;
using DoubleEqualVerifier = EqualVerifier<DoubleVerifier>;
using StringEqualVerifier = EqualVerifier<StringVerifier>;
using BoolUnequalVerifier = UnequalVerifier<BoolVerifier>;
using IntUnequalVerifier = UnequalVerifier<IntVerifier>;
using DoubleUnequalVerifier = UnequalVerifier<DoubleVerifier>;
using StringUnequalVerifier = UnequalVerifier<StringVerifier>;

using BoolInListVerifier = InListVerifier<BoolVerifier>;
using IntInListVerifier = InListVerifier<IntVerifier>;
using DoubleInListVerifier = InListVerifier<DoubleVerifier>;
using StringInListVerifier = InListVerifier<StringVerifier>;
using BoolNotInListVerifier = NotInListVerifier<BoolVerifier>;
using IntNotInListVerifier = NotInListVerifier<IntVerifier>;
using DoubleNotInListVerifier = NotInListVerifier<DoubleVerifier>;
using StringNotInListVerifier = NotInListVerifier<StringVerifier>;

using IntInRangeVerifier = InRangeVerifier<IntVerifier>;
using DoubleInRangeVerifier = InRangeVerifier<DoubleVerifier>;
using IntNotInRangeVerifier = NotInRangeVerifier<IntVerifier>;
using DoubleNotInRangeVerifier = NotInRangeVerifier<DoubleVerifier>;

using BoolAnnotationVerifier = AnnotationVerifier<BoolVerifier>;
using IntAnnotationVerifier = AnnotationVerifier<IntVerifier>;
using DoubleAnnotationVerifier = AnnotationVerifier<DoubleVerifier>;
using StringAnnotationVerifier = AnnotationVerifier<StringVerifier>;
using TableAnnotationVerifier = AnnotationVerifier<TableVerifier>;


extern template struct LessVerifier<IntVerifier>;
extern template struct LessVerifier<DoubleVerifier>;
extern template struct LessEqualVerifier<IntVerifier>;
extern template struct LessEqualVerifier<DoubleVerifier>;
extern template struct GreaterVerifier<IntVerifier>;
extern template struct GreaterVerifier<DoubleVerifier>;
extern template struct GreaterEqualVerifier<IntVerifier>;
extern template struct GreaterEqualVerifier<DoubleVerifier>;
extern template struct EqualVerifier<BoolVerifier>;
extern template struct EqualVerifier<IntVerifier>;
extern template struct EqualVerifier<DoubleVerifier>;
extern template struct EqualVerifier<StringVerifier>;
extern template struct UnequalVerifier<BoolVerifier>;
extern template struct UnequalVerifier<IntVerifier>;
extern template struct UnequalVerifier<DoubleVerifier>;
extern template struct UnequalVerifier<StringVerifier>;

extern template struct InListVerifier<BoolVerifier>;
extern template struct InListVerifier<IntVerifier>;
extern template struct InListVerifier<DoubleVerifier>;
extern template struct InListVerifier<StringVerifier>;
extern template struct NotInListVerifier<BoolVerifier>;
extern template struct NotInListVerifier<IntVerifier>;
extern template struct NotInListVerifier<DoubleVerifier>;
extern template struct NotInListVerifier<StringVerifier>;

extern template struct InRangeVerifier<IntVerifier>;
extern template struct InRangeVerifier<DoubleVerifier>;
extern template struct NotInRangeVerifier<IntVerifier>;
extern template struct NotInRangeVerifier<DoubleVerifier>;

extern template struct AnnotationVerifier<BoolVerifier>;
extern template struct AnnotationVerifier<IntVerifier>;
extern template struct AnnotationVerifier<DoubleVerifier>;
extern template struct AnnotationVerifier<StringVerifier>;
extern template struct AnnotationVerifier<TableVerifier>;


} // namespace documentation
} // namespace openspace

#include "verifier.inl"

#endif // __VERIFIER_H__
