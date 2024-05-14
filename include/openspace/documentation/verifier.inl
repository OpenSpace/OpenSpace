/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <ghoul/misc/dictionary.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <iterator>
#include <numeric>
#include <sstream>

namespace openspace::documentation {

template <>
TestResult TemplateVerifier<glm::ivec2>::operator()(const ghoul::Dictionary& dict,
    const std::string& key) const;

template <>
TestResult TemplateVerifier<glm::ivec3>::operator()(const ghoul::Dictionary& dict,
    const std::string& key) const;

template <>
TestResult TemplateVerifier<glm::ivec4>::operator()(const ghoul::Dictionary& dict,
    const std::string& key) const;

template <typename T>
TestResult TemplateVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                           const std::string& key) const
{
    TestResult res;
    if (dict.hasValue<Type>(key)) {
        res.success = true;
    }
    else {
        res.success = false;

        if (dict.hasKey(key)) {
            TestResult::Offense o;
            o.offender = key;
            o.reason = TestResult::Offense::Reason::WrongType;
            res.offenses.push_back(o);
        }
        else {
            TestResult::Offense o;
            o.offender = key;
            o.reason = TestResult::Offense::Reason::MissingKey;
            res.offenses.push_back(o);
        }
    }
    return res;
}

template <typename T>
std::string TemplateVerifier<T>::documentation() const {
    return "Value of type '" + type() + "'";
}

template <typename T>
std::string Vector2Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Vector2<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Vector2<double>";
    }
    else {
        return std::string("Vector2<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Vector3Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Vector3<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Vector3<double>";
    }
    else {
        return std::string("Vector3<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Vector4Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Vector4<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Vector4<double>";
    }
    else {
        return std::string("Vector4<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Matrix2x2Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Matrix2x2<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Matrix2x2<double>";
    }
    else {
        return std::string("Matrix2x2<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Matrix2x3Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Matrix2x3<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Matrix2x3<double>";
    }
    else {
        return std::string("Matrix2x3<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Matrix2x4Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Matrix2x4<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Matrix2x4<double>";
    }
    else {
        return std::string("Matrix2x4<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Matrix3x2Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Matrix3x2<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Matrix3x2<double>";
    }
    else {
        return std::string("Matrix3x2<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Matrix3x3Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Matrix3x3<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Matrix3x3<double>";
    }
    else {
        return std::string("Matrix3x3<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Matrix3x4Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Matrix3x4<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Matrix3x4<double>";
    }
    else {
        return std::string("Matrix3x4<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Matrix4x2Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Matrix4x2<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Matrix4x2<double>";
    }
    else {
        return std::string("Matrix4x2<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Matrix4x3Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Matrix4x3<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Matrix4x3<double>";
    }
    else {
        return std::string("Matrix4x3<") + typeid(T).name() + ">";
    }
}

template <typename T>
std::string Matrix4x4Verifier<T>::type() const {
    if constexpr (std::is_same_v<T, int>) {
        return "Matrix4x4<int>";
    }
    else if constexpr (std::is_same_v<T, double>) {
        return "Matrix4x4<double>";
    }
    else {
        return std::string("Matrix4x4<") + typeid(T).name() + ">";
    }
}

template <typename T, typename Operator>
OperatorVerifier<T, Operator>::OperatorVerifier(typename T::Type val)
    : value(std::move(val))
{}

template <typename T, typename Operator>
TestResult OperatorVerifier<T, Operator>::operator()(const ghoul::Dictionary& dict,
                                                     const std::string& key) const
{
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        typename T::Type val;
        if constexpr (std::is_same_v<typename T::Type, glm::ivec2>) {
            val = dict.value<glm::dvec2>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec3>) {
            val = dict.value<glm::dvec3>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec4>) {
            val = dict.value<glm::dvec4>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, int>) {
            const double d = dict.value<double>(key);
            double intPart;
            bool isInt = modf(d, &intPart) == 0.0;
            if (isInt) {
                val = static_cast<int>(d);
            }
            else {
                TestResult r;
                r.success = false;
                TestResult::Offense o;
                o.offender = key;
                o.reason = TestResult::Offense::Reason::WrongType;
                r.offenses.push_back(o);
                return r;
            }
        }
        else {
            val = dict.value<typename T::Type>(key);
        }

        if (Operator()(val, value)) {
            return { true, {}, {} };
        }
        else {
            TestResult r;
            r.success = false;
            TestResult::Offense o;
            o.offender = key;
            o.reason = TestResult::Offense::Reason::Verification;
            r.offenses.push_back(o);
            return r;
        }
    }
    else {
        return res;
    }
}

template <typename T>
std::string LessVerifier<T>::documentation() const {
    return "Less than: " + ghoul::to_string(value);
}

template <typename T>
std::string LessEqualVerifier<T>::documentation() const {
    return "Less or equal to: " + ghoul::to_string(value);
}

template <typename T>
std::string GreaterVerifier<T>::documentation() const {
    return "Greater than: " + ghoul::to_string(value);
}

template <typename T>
std::string GreaterEqualVerifier<T>::documentation() const {
    return "Greater or equal to: " + ghoul::to_string(value);
}

template <typename T>
std::string EqualVerifier<T>::documentation() const {
    return "Equal to: " + ghoul::to_string(value);
}

template <typename T>
std::string UnequalVerifier<T>::documentation() const {
    return "Unequal to: " + ghoul::to_string(value);
}

template <typename T>
InListVerifier<T>::InListVerifier(std::vector<typename T::Type> vals)
    : values(std::move(vals))
{}

template <typename T>
TestResult InListVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                         const std::string& key) const
{
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        typename T::Type val;
        if constexpr (std::is_same_v<typename T::Type, glm::ivec2>) {
            val = dict.value<glm::dvec2>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec3>) {
            val = dict.value<glm::dvec3>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec4>) {
            val = dict.value<glm::dvec4>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, int>) {
            const double d = dict.value<double>(key);
            double intPart;
            bool isInt = modf(d, &intPart) == 0.0;
            if (isInt) {
                val = static_cast<int>(d);
            }
            else {
                TestResult r;
                r.success = false;
                TestResult::Offense o;
                o.offender = key;
                o.reason = TestResult::Offense::Reason::WrongType;
                r.offenses.push_back(o);
                return r;
            }
        }
        else {
            val = dict.value<typename T::Type>(key);
        }

        auto it = std::find(values.begin(), values.end(), val);
        if (it != values.end()) {
            return { true, {}, {} };
        }
        else {
            TestResult r;
            r.success = false;
            TestResult::Offense o;
            o.offender = key;
            o.reason = TestResult::Offense::Reason::Verification;

            std::string list = std::accumulate(
                values.begin() + 1,
                values.end(),
                std::format("{}", values.front()),
                [](std::string lhs, typename T::Type rhs) {
                    return std::format("{}, {}", lhs, rhs);
                }
            );
            o.explanation = std::format(
                "'{}' not in list of accepted values '{}'",
                key, list
            );
            r.offenses.push_back(o);
            return r;
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
        std::ostream_iterator<typename T::Type>(s, ", ")
    );

    std::string joined = s.str();
    // We need to remove a trailing ',' and whitespace at the end of the string
    result += joined.substr(0, joined.size() - 2);

    result += " }";
    return result;
}

template <typename T>
NotInListVerifier<T>::NotInListVerifier(std::vector<typename T::Type> vals)
    : values(std::move(vals))
{}

template <typename T>
TestResult NotInListVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                         const std::string& key) const
{
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        typename T::Type val;
        if constexpr (std::is_same_v<typename T::Type, glm::ivec2>) {
            val = dict.value<glm::dvec2>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec3>) {
            val = dict.value<glm::dvec3>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec4>) {
            val = dict.value<glm::dvec4>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, int>) {
            const double d = dict.value<double>(key);
            double intPart;
            bool isInt = modf(d, &intPart) == 0.0;
            if (isInt) {
                val = static_cast<int>(d);
            }
            else {
                TestResult r;
                r.success = false;
                TestResult::Offense o;
                o.offender = key;
                o.reason = TestResult::Offense::Reason::WrongType;
                r.offenses.push_back(o);
                return r;
            }
        }
        else {
            val = dict.value<typename T::Type>(key);
        }

        auto it = std::find(values.begin(), values.end(), val);
        if (it == values.end()) {
            return { true, {}, {} };
        }
        else {
            TestResult r;
            r.success = false;
            TestResult::Offense o;
            o.offender = key;
            o.reason = TestResult::Offense::Reason::Verification;
            r.offenses.push_back(o);
            return r;
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
InRangeVerifier<T>::InRangeVerifier(typename T::Type l, typename T::Type u)
    : lower(std::move(l))
    , upper(std::move(u))
{
    if constexpr (std::is_same_v<IntVector2Verifier, T> ||
                  std::is_same_v<DoubleVector2Verifier, T>)
    {
        ghoul_assert(lower.x <= upper.x, "lower must be smaller or equal to upper for x");
        ghoul_assert(lower.y <= upper.y, "lower must be smaller or equal to upper for y");
    }
    else if constexpr (std::is_same_v<IntVector3Verifier, T> ||
                       std::is_same_v<DoubleVector3Verifier, T>)
    {
        ghoul_assert(lower.x <= upper.x, "lower must be smaller or equal to upper for x");
        ghoul_assert(lower.y <= upper.y, "lower must be smaller or equal to upper for y");
        ghoul_assert(lower.z <= upper.z, "lower must be smaller or equal to upper for z");
    }
    else if constexpr (std::is_same_v<IntVector4Verifier, T> ||
                       std::is_same_v<DoubleVector4Verifier, T>)
    {
        ghoul_assert(lower.x <= upper.x, "lower must be smaller or equal to upper for x");
        ghoul_assert(lower.y <= upper.y, "lower must be smaller or equal to upper for y");
        ghoul_assert(lower.z <= upper.z, "lower must be smaller or equal to upper for z");
        ghoul_assert(lower.w <= upper.w, "lower must be smaller or equal to upper for w");
    }
    else {
        ghoul_assert(lower <= upper, "lower must be smaller or equal to upper");
    }
}

template <typename T>
TestResult InRangeVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                          const std::string& key) const
{
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        typename T::Type val;
        if constexpr (std::is_same_v<typename T::Type, glm::ivec2>) {
            val = dict.value<glm::dvec2>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec3>) {
            val = dict.value<glm::dvec3>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec4>) {
            val = dict.value<glm::dvec4>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, int>) {
            const double d = dict.value<double>(key);
            double intPart;
            bool isInt = modf(d, &intPart) == 0.0;
            if (isInt) {
                val = static_cast<int>(d);
            }
            else {
                TestResult r;
                r.success = false;
                TestResult::Offense o;
                o.offender = key;
                o.reason = TestResult::Offense::Reason::WrongType;
                r.offenses.push_back(o);
                return r;
            }
        }
        else {
            val = dict.value<typename T::Type>(key);
        }

        if (std::greater_equal<typename T::Type>()(val, lower) &&
            std::less_equal<typename T::Type>()(val, upper))
        {
            return { true, {}, {} };
        }
        else {
            TestResult r;
            r.success = false;
            TestResult::Offense o;
            o.offender = key;
            o.reason = TestResult::Offense::Reason::Verification;
            r.offenses.push_back(o);
            return r;
        }
    }
    else {
        return res;
    }
}

template <typename T>
std::string InRangeVerifier<T>::documentation() const {
    return "In range: ( " + ghoul::to_string(lower) + "," +
           ghoul::to_string(upper) + " )";
}

template <typename T>
NotInRangeVerifier<T>::NotInRangeVerifier(typename T::Type l, typename T::Type u)
    : lower(std::move(l))
    , upper(std::move(u))
{
    if constexpr (std::is_same_v<IntVector2Verifier, T> ||
                  std::is_same_v<DoubleVector2Verifier, T>)
    {
        ghoul_assert(lower.x <= upper.x, "lower must be smaller or equal to upper for x");
        ghoul_assert(lower.y <= upper.y, "lower must be smaller or equal to upper for y");
    }
    else if constexpr (std::is_same_v<IntVector3Verifier, T> ||
                       std::is_same_v<DoubleVector3Verifier, T>)
    {
        ghoul_assert(lower.x <= upper.x, "lower must be smaller or equal to upper for x");
        ghoul_assert(lower.y <= upper.y, "lower must be smaller or equal to upper for y");
        ghoul_assert(lower.z <= upper.z, "lower must be smaller or equal to upper for z");
    }
    else if constexpr (std::is_same_v<IntVector4Verifier, T> ||
                       std::is_same_v<DoubleVector4Verifier, T>)
    {
        ghoul_assert(lower.x <= upper.x, "lower must be smaller or equal to upper for x");
        ghoul_assert(lower.y <= upper.y, "lower must be smaller or equal to upper for y");
        ghoul_assert(lower.z <= upper.z, "lower must be smaller or equal to upper for z");
        ghoul_assert(lower.w <= upper.w, "lower must be smaller or equal to upper for w");
    }
    else {
        ghoul_assert(lower <= upper, "lower must be smaller or equal to upper");
    }
}

template <typename T>
TestResult NotInRangeVerifier<T>::operator()(const ghoul::Dictionary& dict,
                                             const std::string& key) const {
    TestResult res = T::operator()(dict, key);
    if (res.success) {
        typename T::Type val;
        if constexpr (std::is_same_v<typename T::Type, glm::ivec2>) {
            val = dict.value<glm::dvec2>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec3>) {
            val = dict.value<glm::dvec3>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, glm::ivec4>) {
            val = dict.value<glm::dvec4>(key);
        }
        else if constexpr (std::is_same_v<typename T::Type, int>) {
            const double d = dict.value<double>(key);
            double intPart;
            bool isInt = modf(d, &intPart) == 0.0;
            if (isInt) {
                val = static_cast<int>(d);
            }
            else {
                TestResult r;
                r.success = false;
                TestResult::Offense o;
                o.offender = key;
                o.reason = TestResult::Offense::Reason::WrongType;
                r.offenses.push_back(o);
                return r;
            }
        }
        else {
            val = dict.value<typename T::Type>(key);
        }

        if (std::less<typename T::Type>()(val, lower) ||
            std::greater<typename T::Type>()(val, upper))
        {
            return { true, {}, {} };
        }
        else {
            TestResult r;
            r.success = false;
            TestResult::Offense o;
            o.offender = key;
            o.reason = TestResult::Offense::Reason::Verification;
            r.offenses.push_back(o);
            return r;
        }
    }
    else {
        return res;
    }
}

template <typename T>
std::string NotInRangeVerifier<T>::documentation() const {
    return "Not in range: ( " + ghoul::to_string(lower) + "," +
           ghoul::to_string(upper) + " )";
}


template <typename T>
AnnotationVerifier<T>::AnnotationVerifier(std::string a)
    : annotation(std::move(a))
{

}

template <typename T>
std::string AnnotationVerifier<T>::documentation() const {
    return annotation;
}

} // namespace openspace::documentation
