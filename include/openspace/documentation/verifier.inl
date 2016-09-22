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

#include <iterator>

namespace std {
    std::string to_string(std::string value);
}

namespace openspace {
namespace documentation {

template <typename T>
TestResult TemplateVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                           const std::string& key) const
{
    if (dict.hasKeyAndValue<Type>(key)) {
        return { true, {} };
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

template <typename T>
std::string TemplateVerifier<T>::documentation() const {
    return "Type testing of '" + type() + "'";
}

template <typename T>
std::string Vector2Verifier<T>::type() const {
    using namespace std::string_literals;

    return "Vector2<"s + typeid(T).name() + ">";
}

template <typename T>
std::string Vector3Verifier<T>::type() const {
    using namespace std::string_literals;

    return "Vector3<"s + typeid(T).name() + ">";
}

template <typename T>
std::string Vector4Verifier<T>::type() const {
    using namespace std::string_literals;

    return "Vector4<"s + typeid(T).name() + ">";
}

template <typename T, typename Operator>
OperatorVerifier<T, Operator>::OperatorVerifier(typename T::Type value)
    : value(std::move(value))
{}

template <typename T, typename Operator>
TestResult OperatorVerifier<T, Operator>::operator()(const ghoul::Dictionary& dict,
                                                     const std::string& key) const 
{
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        if (Operator()(dict.value<typename T::Type>(key), value)) {
            return { true, {} };
        }
        else {
            return { false, { { key, TestResult::Offense::Reason::Verification }}};
        }
    }
    else {
        return res;
    }
}

template <typename T>
std::string LessVerifier<T>::documentation() const {
    return "Less than: " + std::to_string(value);
}

template <typename T>
std::string LessEqualVerifier<T>::documentation() const {
    return "Less or equal to: " + std::to_string(value);
}

template <typename T>
std::string GreaterVerifier<T>::documentation() const {
    return "Greater than: " + std::to_string(value);
}

template <typename T>
std::string GreaterEqualVerifier<T>::documentation() const {
    return "Greater or equal to: " + std::to_string(value);
}

template <typename T>
std::string EqualVerifier<T>::documentation() const {
    return "Equal to: " + std::to_string(value);
}

template <typename T>
std::string UnequalVerifier<T>::documentation() const {
    return "Unequal to: " + std::to_string(value);
}

template <typename T>
InListVerifier<T>::InListVerifier(std::vector<typename T::Type> values)
    : values(std::move(values))
{}

template <typename T>
TestResult InListVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                         const std::string& key) const
{
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        typename T::Type value = dict.value<typename T::Type>(key);

        auto it = std::find(values.begin(), values.end(), value);

        if (it != values.end()) {
            return { true, {} };
        }
        else {
            return { false, { { key, TestResult::Offense::Reason::Verification } } };
        }
    }
    else {
        return res;
    }
}

template <typename T>
std::string InListVerifier<T>::documentation() const {
    std::string result = "In list { ";

    std::stringstream s;
    std::copy(
        values.begin(),
        values.end(),
        std::ostream_iterator<typename T::Type>(s, ",")
    );

    std::string joined = s.str();
    // We need to remove a trailing ',' at the end of the string
    result += joined.substr(0, joined.size() - 1);

    result += " }";
    return result;
}

template <typename T>
NotInListVerifier<T>::NotInListVerifier(std::vector<typename T::Type> values)
    : values(std::move(values))
{}

template <typename T>
TestResult NotInListVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                         const std::string& key) const
{
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        typename T::Type value = dict.value<typename T::Type>(key);

        auto it = std::find(values.begin(), values.end(), value);

        if (it == values.end()) {
            return { true, {} };
        }
        else {
            return { false, { { key, TestResult::Offense::Reason::Verification } } };
        }
    }
    else {
        return res;
    }
}

template <typename T>
std::string NotInListVerifier<T>::documentation() const {
    std::string result = "Not in list { ";

    std::stringstream s;
    std::copy(
        values.begin(),
        values.end(),
        std::ostream_iterator<typename T::Type>(s, ",")
    );

    std::string joined = s.str();
    // We need to remove a trailing ',' at the end of the string
    result += joined.substr(0, joined.size() - 1);

    result += " }";
    return result;
}

template <typename T>
InRangeVerifier<T>::InRangeVerifier(typename T::Type lower, typename T::Type upper)
    : lower(std::move(lower))
    , upper(std::move(upper))
{
    ghoul_assert(lower <= upper, "lower must be smaller or equal to upper");
}

template <typename T>
TestResult InRangeVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                          const std::string& key) const
{
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        typename T::Type val = dict.value<typename T::Type>(key);

        if (val >= lower && val <= upper) {
            return { true, {} };
        }
        else {
            return { false, { { key, TestResult::Offense::Reason::Verification } } };
        }
    }
    else {
        return res;
    }
}

template <typename T>
std::string InRangeVerifier<T>::documentation() const {
    return "In range: ( " + std::to_string(lower) + "," +
        std::to_string(upper) + " )";
}

template <typename T>
NotInRangeVerifier<T>::NotInRangeVerifier(typename T::Type lower, typename T::Type upper)
    : lower(std::move(lower))
    , upper(std::move(upper))
{
    ghoul_assert(lower <= upper, "lower must be smaller or equal to upper");
}

template <typename T>
TestResult NotInRangeVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                          const std::string& key) const {
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        typename T::Type val = dict.value<typename T::Type>(key);

        if (val >= lower && val <= upper) {
            return { false, { { key, TestResult::Offense::Reason::Verification } } };
        }
        else {
            return { true, {} };
        }
    }
    else {
        return res;
    }
}

template <typename T>
std::string NotInRangeVerifier<T>::documentation() const {
    return "Not in range: ( " + std::to_string(lower) + "," +
        std::to_string(upper) + " )";
}


template <typename T>
AnnotationVerifier<T>::AnnotationVerifier(std::string annotation)
    : annotation(std::move(annotation))
{
    ghoul_assert(!this->annotation.empty(), "Annotation must not be empty");
}

template <typename T>
std::string AnnotationVerifier<T>::documentation() const {
    return annotation;
}

} // namespace documentation
} // namespace openspace
