/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___DICTIONARY___H__
#define __GHOUL___DICTIONARY___H__

#include <ghoul/glm.h>
#include <concepts>
#include <filesystem>
#include <functional>
#include <map>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

namespace ghoul {

template <typename T, typename... U>
concept IsAnyOf = (std::same_as<T, U> || ...);

class Dictionary;

template <typename T>
concept SupportedByDictionary = IsAnyOf<
    T,
    bool, int, double, std::string, std::filesystem::path, Dictionary, void*,
    std::vector<int>, std::vector<double>, std::vector<std::string>,
    std::vector<std::filesystem::path>, glm::ivec2, glm::ivec3, glm::ivec4, glm::dvec2,
    glm::dvec3, glm::dvec4, glm::dmat2x2, glm::dmat2x3, glm::dmat2x4, glm::dmat3x2,
    glm::dmat3x3, glm::dmat3x4, glm::dmat4x2, glm::dmat4x3, glm::dmat4x4
>;

/**
 * The Dictionary is a class that represents a mapping from a string to a fixed selection
 * of types. It has the ability to store and retrieve these items by unique string keys.
 * The only automatic conversion that is currently performed is to convert glm vector or
 * matrix types into std::vector types and reconstructing them on access such that:
 * ```
 * Dictionary d;
 * d.setValue("a", glm::dvec4(1.0, 2.0, 3.0, 4.0);
 * std::vector<double> v = d.value<std::vector<double>>("a");
 * // v.size() == 4
 * // v[0] == 1.0 && v[1] == 2.0 && v[2] == 3.0 && v[3] == 4.0
 *
 * Dictionary e;
 * e.setValue("a", std::vector<double>{5.0, 6.0, 7.0, 8.0});
 * glm::dvec4 vv = e.value<glm::dvec4>("a");
 * // vv.x == 5.0 && vv.y == 6.0 && vv.z == 7.0 && vv.w == 8.0
 * ```
 * are legal
 */
class Dictionary {
public:
    /**
     * This is a list of all types that can be stored and retrieved from the Dictionary.
     */
    using Types = std::variant<
        bool, int, double, std::string, std::filesystem::path, Dictionary, void*,
        std::vector<int>, std::vector<double>, std::vector<std::string>,
        std::vector<std::filesystem::path>, glm::ivec2, glm::ivec3, glm::ivec4,
        glm::dvec2, glm::dvec3, glm::dvec4, glm::dmat2x2, glm::dmat2x3, glm::dmat2x4,
        glm::dmat3x2, glm::dmat3x3, glm::dmat3x4, glm::dmat4x2, glm::dmat4x3, glm::dmat4x4
    >;

    bool operator==(const Dictionary& rhs) const noexcept;
    bool operator!=(const Dictionary& rhs) const noexcept;

    /**
     * Store the value \p value at the specified \p key, overwriting any existing value.
     * The type for T has to be a type that can be represented in the Types variant type.
     *
     * \param key The key under which the \p value is stored. If \p key already existed,
     *        it will be silently overwritten
     * \param value The value to store. It has to be one of the types that is present in
     *        the Types variant
     *
     * \pre \p key must not be the empty string
     */
    template <SupportedByDictionary T>
    void setValue(std::string key, T value);

    /**
     * Retrieves the value stored at the provided \p key. The template parameter has to be
     * one of the types contained in the Types variant described above. If the Dictionary
     * does not contain a value for the \p key, a KeyError will be raised. If the
     * Dictionary stores a type for the existing \p key than requested a ValueError will
     * be raised.
     *
     * \param key The key for which to retrieve the value
     * \return The value in the Dictionary stored at the \p key
     *
     * \throw KeyError If the provided \p key does not exist
     * \throw ValueError If the value stored at \p key is not of type T
     * \pre \p key must not be the empty string
     */
    template <SupportedByDictionary T>
    T value(std::string_view key) const;

    /**
     * Checks whether the Dictionary stores a value of type T at the provided \p key. This
     * function returns false if the key does not exist or if the stored value is not
     * compatible with the type T.
     *
     * \param key The key for which to check the existence and type
     * \return `true` if the Dictionary contains such a key and it is of the requested
               type T
     *
     * \pre \p key must not be the empty string
     */
    template <SupportedByDictionary T>
    bool hasValue(std::string_view key) const;

    /**
     * Checks whether the Dictionary stores any value under the provided key, regardless
     * of its type.
     *
     * \param key The key for which to check the existence and type
     * \return `true` if the Dictionary contains such a key and it is of the requested
               type T
     *
     * \pre \p key must not be the empty string
     */
    bool hasKey(std::string_view key) const;

    /**
     * Returns a list of all keys stored in the Dictionary.
     *
     * \return A list of all keys stored in the Dictionary
     */
    std::vector<std::string_view> keys() const;

    /**
     * Removes the provided \p key from the dictionary. If the key does not exist, this
     * operation does not do anything.
     *
     * \param key The key to remove
     */
    void removeValue(std::string_view key);

    /**
     * Returns whether the Dictionary is empty or contains values.
     *
     * \return Whether the Dictionary is empty or contains values
     */
    bool isEmpty() const;

    /**
     * Returns the number of values stored in the Dictionary.
     *
     * \return The number of values stored in the Dictionary
     */
    size_t size() const;

    /**
     * Checks if the \p dict Dictionary is a subset of this dictionary, meaning that all
     * values contained in \p dict also exist in this dictionary. This dictionary might
     * contain additional values though.
     *
     * \param dict The subset dictionary
     * \return `true` if the \p dict is a subset, `false` otherwise
     */
    bool isSubset(const Dictionary& dict) const;

private:
    using StorageTypes = std::variant<
        bool, int, double, std::string, Dictionary, void*, std::vector<int>,
        std::vector<double>, std::vector<std::string>
    >;
    std::map<std::string, StorageTypes, std::less<>> _storage;
};

extern template void Dictionary::setValue(std::string, Dictionary value);
extern template void Dictionary::setValue(std::string, bool value);
extern template void Dictionary::setValue(std::string, double);
extern template void Dictionary::setValue(std::string, int);
extern template void Dictionary::setValue(std::string, std::string);
extern template void Dictionary::setValue(std::string, std::filesystem::path);
extern template void Dictionary::setValue(std::string, void*);
extern template void Dictionary::setValue(std::string, std::vector<int>);
extern template void Dictionary::setValue(std::string, std::vector<double>);
extern template void Dictionary::setValue(std::string, std::vector<std::string>);
extern template void Dictionary::setValue(std::string,
    std::vector<std::filesystem::path>);
extern template void Dictionary::setValue(std::string, glm::ivec2);
extern template void Dictionary::setValue(std::string, glm::ivec3);
extern template void Dictionary::setValue(std::string, glm::ivec4);
extern template void Dictionary::setValue(std::string, glm::dvec2);
extern template void Dictionary::setValue(std::string, glm::dvec3);
extern template void Dictionary::setValue(std::string, glm::dvec4);
extern template void Dictionary::setValue(std::string, glm::dmat2x2);
extern template void Dictionary::setValue(std::string, glm::dmat2x3);
extern template void Dictionary::setValue(std::string, glm::dmat2x4);
extern template void Dictionary::setValue(std::string, glm::dmat3x2);
extern template void Dictionary::setValue(std::string, glm::dmat3x3);
extern template void Dictionary::setValue(std::string, glm::dmat3x4);
extern template void Dictionary::setValue(std::string, glm::dmat4x2);
extern template void Dictionary::setValue(std::string, glm::dmat4x3);
extern template void Dictionary::setValue(std::string, glm::dmat4x4);

extern template Dictionary Dictionary::value(std::string_view) const;
extern template bool Dictionary::value(std::string_view) const;
extern template double Dictionary::value(std::string_view) const;
extern template int Dictionary::value(std::string_view) const;
extern template std::string Dictionary::value(std::string_view) const;
extern template std::filesystem::path Dictionary::value(std::string_view) const;
extern template void* Dictionary::value(std::string_view) const;
extern template std::vector<int> Dictionary::value(std::string_view) const;
extern template std::vector<double> Dictionary::value(std::string_view) const;
extern template std::vector<std::string> Dictionary::value(std::string_view) const;
extern template std::vector<std::filesystem::path> Dictionary::value(
    std::string_view) const;
extern template glm::ivec2 Dictionary::value(std::string_view) const;
extern template glm::ivec3 Dictionary::value(std::string_view) const;
extern template glm::ivec4 Dictionary::value(std::string_view) const;
extern template glm::dvec2 Dictionary::value(std::string_view) const;
extern template glm::dvec3 Dictionary::value(std::string_view) const;
extern template glm::dvec4 Dictionary::value(std::string_view) const;
extern template glm::dmat2x2 Dictionary::value(std::string_view) const;
extern template glm::dmat2x3 Dictionary::value(std::string_view) const;
extern template glm::dmat2x4 Dictionary::value(std::string_view) const;
extern template glm::dmat3x2 Dictionary::value(std::string_view) const;
extern template glm::dmat3x3 Dictionary::value(std::string_view) const;
extern template glm::dmat3x4 Dictionary::value(std::string_view) const;
extern template glm::dmat4x2 Dictionary::value(std::string_view) const;
extern template glm::dmat4x3 Dictionary::value(std::string_view) const;
extern template glm::dmat4x4 Dictionary::value(std::string_view) const;

extern template bool Dictionary::hasValue<Dictionary>(std::string_view) const;
extern template bool Dictionary::hasValue<bool>(std::string_view) const;
extern template bool Dictionary::hasValue<double>(std::string_view) const;
extern template bool Dictionary::hasValue<int>(std::string_view) const;
extern template bool Dictionary::hasValue<std::string>(std::string_view) const;
extern template bool Dictionary::hasValue<std::filesystem::path>(std::string_view) const;
extern template bool Dictionary::hasValue<void*>(std::string_view) const;
extern template bool Dictionary::hasValue<std::vector<int>>(std::string_view) const;
extern template bool Dictionary::hasValue<std::vector<double>>(std::string_view) const;
extern template bool Dictionary::hasValue<std::vector<std::string>>(
    std::string_view) const;
extern template bool Dictionary::hasValue<std::vector<std::filesystem::path>>(
    std::string_view) const;
extern template bool Dictionary::hasValue<glm::ivec2>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::ivec3>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::ivec4>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dvec2>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dvec3>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dvec4>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dmat2x2>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dmat2x3>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dmat2x4>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dmat3x2>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dmat3x3>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dmat3x4>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dmat4x2>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dmat4x3>(std::string_view) const;
extern template bool Dictionary::hasValue<glm::dmat4x4>(std::string_view) const;

} // namespace ghoul

#endif // __GHOUL___DICTIONARY___H__
