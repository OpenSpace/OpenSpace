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

namespace std {
std::string to_string(std::string value);
}

namespace openspace {
namespace documentation {

template <typename T>
LessVerifier<T>::LessVerifier(typename T::Type value) 
    : value(std::move(value))
{
}

template <typename T>
bool LessVerifier<T>::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return T::test(dict, key) && dict.value<Type>(key) < value;
}

template <typename T>
std::string LessVerifier<T>::documentation() const {
    return T::documentation() + '\n' + "Less than: " + std::to_string(value);
}


template <typename T>
LessEqualVerifier<T>::LessEqualVerifier(typename T::Type value)
    : value(std::move(value))
{}

template <typename T>
bool LessEqualVerifier<T>::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return T::test(dict, key) && dict.value<Type>(key) <= value;
}

template <typename T>
std::string LessEqualVerifier<T>::documentation() const {
    return T::documentation() + '\n' + "Less or equal to: " + std::to_string(value);
}

template <typename T>
GreaterVerifier<T>::GreaterVerifier(typename T::Type value)
    : value(std::move(value))
{}

template <typename T>
bool GreaterVerifier<T>::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return T::test(dict, key) && dict.value<Type>(key) > value;
}

template <typename T>
std::string GreaterVerifier<T>::documentation() const {
    return T::documentation() + '\n' + "Greater than: " + std::to_string(value);
}

template <typename T>
GreaterEqualVerifier<T>::GreaterEqualVerifier(typename T::Type value)
    : value(std::move(value)) 
{}

template <typename T>
bool GreaterEqualVerifier<T>::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return T::test(dict, key) && dict.value<Type>(key) >= value;
}

template <typename T>
std::string GreaterEqualVerifier<T>::documentation() const {
    return T::documentation() + '\n' + "Greater or equal to: " + std::to_string(value);
}

template <typename T>
EqualVerifier<T>::EqualVerifier(typename T::Type value) 
    : value(std::move(value)) 
{}

template <typename T>
bool EqualVerifier<T>::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return T::test(dict, key) && dict.value<Type>(key) == value;
}

template <typename T>
std::string EqualVerifier<T>::documentation() const {
    return T::documentation() + '\n' + "Equal to: " + std::to_string(value);
}

template <typename T>
UnequalVerifier<T>::UnequalVerifier(typename T::Type value)
    : value(std::move(value))
{}

template <typename T>
bool UnequalVerifier<T>::test(const ghoul::Dictionary& dict, const std::string& key) const {
    return T::test(dict, key) && dict.value<Type>(key) != value;
}

template <typename T>
std::string UnequalVerifier<T>::documentation() const {
    return T::documentation() + '\n' + "Unequal to: " + std::to_string(value);
}

template <typename T>
InListVerifier<T>::InListVerifier(std::vector<typename T::Type> values)
    : values(std::move(values))
{}

template <typename T>
bool InListVerifier<T>::test(const ghoul::Dictionary& dict, const std::string& key) const {
    if (T::test(dict, key)) {
        typename T::Type value = dict.value<typename T::Type>(key);

        auto it = std::find(values.begin(), values.end(), value);
        return it != values.end();
    }
    else {
        return false;
    }
}

template <typename T>
std::string InListVerifier<T>::documentation() const {
    std::string result = T::documentation() + '\n' + "In list {";

    std::stringstream s;
    std::copy(values.begin(), values.end(), std::ostream_iterator<typename T::Type>(s, ","));

    std::string joined = s.str();
    // We need to remove a trailing ',' at the end of the string
    result += joined.substr(0, joined.size() - 1);

    result += "}";
    return result;
}

template <typename T>
NotInListVerifier<T>::NotInListVerifier(std::vector<typename T::Type> values)
    : values(std::move(values))
{}

template <typename T>
bool NotInListVerifier<T>::test(const ghoul::Dictionary& dict, const std::string& key) const {
    if (T::test(dict, key)) {
        typename T::Type value = dict.value<typename T::Type>(key);

        auto it = std::find(values.begin(), values.end(), value);
        return it == values.end();
    }
    else {
        return false;
    }
}

template <typename T>
std::string NotInListVerifier<T>::documentation() const {
    std::string result = T::documentation() + '\n' + "Not in list {";

    std::stringstream s;
    std::copy(values.begin(), values.end(), std::ostream_iterator<typename T::Type>(s, ","));

    std::string joined = s.str();
    // We need to remove a trailing ',' at the end of the string
    result += joined.substr(0, joined.size() - 1);


    result += "}";
    return result;
}

template <typename T>
InRangeVerifier<T>::InRangeVerifier(typename T::Type lower, typename T::Type upper)
    : lower(std::move(lower))
    , upper(std::move(upper))
{
    ghoul_assert(lower <= upper, "Lower value must be smaller or equal to upper value");
}

template <typename T>
bool InRangeVerifier<T>::test(const ghoul::Dictionary& d, const std::string& key) const {
    if (T::test(d, key)) {
        typename T::Type val = d.value<typename T::Type>(key);
        return val >= lower && val <= upper;
    }
    else {
        return false;
    }
}

template <typename T>
std::string InRangeVerifier<T>::documentation() const {
    return T::documentation() + '\n' + "In range: (" + std::to_string(lower) + "," +
        std::to_string(upper) + ")";
}

template <typename T>
NotInRangeVerifier<T>::NotInRangeVerifier(typename T::Type lower, typename T::Type upper)
    : lower(std::move(lower))
    , upper(std::move(upper))
{
    ghoul_assert(lower <= upper, "Lower value must be smaller or equal to upper value");
}

template <typename T>
bool NotInRangeVerifier<T>::test(const ghoul::Dictionary& d, const std::string& k) const {
    if (T::test(d, k)) {
        typename T::Type val = d.value<typename T::Type>(k);
        return !(val >= lower && val <= upper);
    }
    else {
        return false;
    }
}

template <typename T>
std::string NotInRangeVerifier<T>::documentation() const {
    return T::documentation() + '\n' + "Not in range: (" + std::to_string(lower) + "," +
        std::to_string(upper) + ")";
}


template <typename T>
AnnotationVerifier<T>::AnnotationVerifier(std::string annotation)
    : annotation(std::move(annotation))
{}

template <typename T>
bool AnnotationVerifier<T>::test(const ghoul::Dictionary& dict,
                                 const std::string& key) const
{
    return T::test(dict, key);
}

template <typename T>
std::string AnnotationVerifier<T>::documentation() const {
    return T::documentation() + '\n' + annotation;
}

} // namespace documentation
} // namespace openspace
