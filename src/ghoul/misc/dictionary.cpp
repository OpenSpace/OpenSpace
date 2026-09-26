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

#include <ghoul/misc/dictionary.h>

#include <ghoul/format.h>
#include <ghoul/misc/assert.h>
#include <ghoul/misc/exception.h>
#include <cstring>
#include <utility>

namespace {
    using namespace ghoul;

    // Boolean constant used to check whether a value T is part of a parameter pack Ts
    template <typename T, typename U> struct is_one_of;
    template <typename T, typename... Ts>
    struct is_one_of<T, std::variant<Ts...>> :
        std::bool_constant<(std::is_same_v<T, Ts> || ...)>
    {};

    // List of types that are stored as-is in the Dictionary
    using DirectTypes = std::variant<bool, double, int, std::string, Dictionary,
        void*, std::vector<int>, std::vector<double>, std::vector<std::string>>;
    template <typename T> using isDirectType = is_one_of<T, DirectTypes>;

    // Vector types that are converted into std::vector for storage purposes
    using GLMTypes = std::variant<glm::ivec2, glm::ivec3, glm::ivec4, glm::dvec2,
        glm::dvec3, glm::dvec4, glm::dmat2x2, glm::dmat2x3, glm::dmat2x4, glm::dmat3x2,
        glm::dmat3x3, glm::dmat3x4, glm::dmat4x2, glm::dmat4x3, glm::dmat4x4>;
    template <typename T> using isGLMType = is_one_of<T, GLMTypes>;


    /**
     * Exception that is thrown if the Dictionary does not contain a provided key.
     */
    struct KeyError final : public RuntimeError {
        explicit KeyError(std::string msg)
            : RuntimeError(std::move(msg), "Dictionary")
        {}
    };

    /**
     * Exception thrown if there was an error with a value, either trying to access the
     * wrong type for a known key or if trying to access a vector/matrix based type and
     * the underlying std::vector did contain the wrong number of values.
     */
    struct ValueError final : public RuntimeError {
        explicit ValueError(std::string key, std::string msg)
            : RuntimeError(
                std::format("Key '{}': {}", std::move(key), std::move(msg)),
                "Dictionary"
            )
        {}
    };
} // namespace

namespace ghoul {

bool Dictionary::operator==(const Dictionary& rhs) const noexcept {
    return _storage == rhs._storage;
}

bool Dictionary::operator!=(const Dictionary& rhs) const noexcept {
    return _storage != rhs._storage;
}

template <SupportedByDictionary T>
void Dictionary::setValue(std::string key, T value) {
    ghoul_assert(!key.empty(), "Key must not be empty");
    if constexpr (isDirectType<T>::value) {
        _storage.insert_or_assign(std::move(key), std::move(value));
    }
    else if constexpr (isGLMType<T>::value) {
        typename T::value_type* p = glm::value_ptr(value);
        std::vector<typename T::value_type> vec(p, p + glm_components<T>::value);
        _storage.insert_or_assign(std::move(key), std::move(vec));
    }
    else if constexpr (std::is_same_v<T, const char*> || std::is_same_v<T, const char[]>)
    {
        setValue(std::move(key), std::string(value));
    }
    else if constexpr (std::is_same_v<T, std::filesystem::path>) {
        setValue(std::move(key), value.string());
    }
    else if constexpr (std::is_same_v<T, std::vector<std::filesystem::path>>) {
        std::vector<std::string> vs;
        vs.reserve(value.size());
        for (const std::filesystem::path& p : value) {
            vs.push_back(p.string());
        }
        setValue(std::move(key), std::move(vs));
    }
    else {
        static_assert(sizeof(T) == 0, "Unsupported type");
    }
}

template <SupportedByDictionary T>
T Dictionary::value(std::string_view key) const {
    ghoul_assert(!key.empty(), "Key must not be empty");

    auto it = _storage.find(key);
    if (it == _storage.end()) {
        throw KeyError(std::format("Could not find key '{}'", key));
    }

    if constexpr (isDirectType<T>::value) {
        if (std::holds_alternative<T>(it->second)) {
            return std::get<T>(it->second);
        }
        else {
            throw ValueError(
                std::string(key),
                std::format(
                    "Error accessing value, wanted type '{}' has '{}'",
                    typeid(T).name(), it->second.index()
                )
            );
        }
    }
    else if constexpr (isGLMType<T>::value) {
        using VT = std::vector<typename T::value_type>;
        VT vec;
        if (std::holds_alternative<VT>(it->second)) {
            vec = std::get<VT>(it->second);
        }
        else if (std::holds_alternative<Dictionary>(it->second)) {
            const Dictionary d = std::get<Dictionary>(it->second);
            vec.resize(d._storage.size());
            for (const auto& kv : d._storage) {
                // Lua is 1-based index, the rest of the world is 0-based
                int k = std::stoi(kv.first) - 1;
                if (k < 0 || k >= static_cast<int>(d._storage.size())) {
                    throw ValueError(
                        std::string(key),
                        std::format(
                            "Invalid key '{}' outside range [0,{}]", k, d._storage.size()
                        )
                    );
                }
                vec[k] = std::get<typename T::value_type>(kv.second);
            }
        }
        else {
            throw ValueError(
                std::string(key),
                std::format(
                    "Requested '{}' but did not contain '{}' or '{}'",
                    typeid(T).name(), typeid(T).name(),
                    typeid(std::vector<typename T::value_type>).name()
                )
            );
        }

        if (vec.size() != glm_components<T>::value) {
            throw ValueError(
                std::string(key),
                std::format(
                    "Contained wrong number of values. Expected {} got {}",
                    glm_components<T>::value, vec.size()
                )
            );
        }

        T res;
        std::memcpy(glm::value_ptr(res), vec.data(), sizeof(T));
        return res;
    }
    else if constexpr (std::is_same_v<T, const char*> ||
                       std::is_same_v<T, const char[]> ||
                       std::is_same_v<T, std::filesystem::path>)
    {
        return value<std::string>(key);
    }
    else if constexpr (std::is_same_v<T, std::vector<std::filesystem::path>>) {
        std::vector<std::string> values = value<std::vector<std::string>>(key);
        std::vector<std::filesystem::path> vs;
        vs.reserve(values.size());
        for (const std::string& v : values) {
            vs.push_back(v);
        }
        return vs;
    }
    else {
        static_assert(sizeof(T) == 0, "Unsupported type");
    }
}

template <SupportedByDictionary T>
bool Dictionary::hasValue(std::string_view key) const {
    ghoul_assert(!key.empty(), "Key must not be empty");

    auto it = _storage.find(key);
    if (it == _storage.end()) {
        return false;
    }
    if constexpr (isDirectType<T>::value) {
        return std::holds_alternative<T>(it->second);
    }
    else if constexpr (isGLMType<T>::value) {
        if (std::holds_alternative<Dictionary>(it->second)) {
            const Dictionary d = std::get<Dictionary>(it->second);

            if (d.size() != glm_components<T>::value) {
                return false;
            }

            // Check whether we have all keys and they are of the correct type
            for (int i = 1; i <= glm_components<T>::value; i++) {
                if (!d.hasValue<typename T::value_type>(std::to_string(i))) {
                    return false;
                }
            }

            // We should check whether the keys are sorted, too
            return true;
        }
        else {
            using VT = std::vector<typename T::value_type>;
            return std::holds_alternative<VT>(it->second) &&
                std::get<VT>(it->second).size() == glm_components<T>::value;
        }
    }
    else if constexpr (std::is_same_v<T, const char*> ||
                       std::is_same_v<T, const char[]> ||
                       std::is_same_v<T, std::filesystem::path>)
    {
        return hasValue<std::string>(key);
    }
    else if constexpr (std::is_same_v<T, std::vector<std::filesystem::path>>) {
        return hasValue<std::vector<std::string>>(key);
    }
    else {
        static_assert(sizeof(T) == 0, "Unknown type T");
    }
}

bool Dictionary::hasKey(std::string_view key) const {
    ghoul_assert(!key.empty(), "Key must not be empty");

    auto it = _storage.find(key);
    return it != _storage.end();
}

std::vector<std::string_view> Dictionary::keys() const {
    std::vector<std::string_view> keys;
    keys.reserve(_storage.size());
    for (const std::pair<const std::string, StorageTypes>& kv : _storage) {
        keys.push_back(kv.first);
    }
    return keys;
}

void Dictionary::removeValue(std::string_view key) {
    if (const auto it = _storage.find(key);  it != _storage.end()) {
        _storage.erase(it);
    }
}

bool Dictionary::isEmpty() const {
    return _storage.empty();
}

size_t Dictionary::size() const {
    return _storage.size();
}

bool Dictionary::isSubset(const Dictionary& dict) const {
    for (const std::pair<const std::string, StorageTypes>& kv : dict._storage) {
        auto it = _storage.find(kv.first);
        if (it == _storage.end() || it->second != kv.second) {
            return false;
        }
    }

    // If we get this far, we didn't fail so we have a subset
    return true;
}

template void Dictionary::setValue(std::string, Dictionary value);
template void Dictionary::setValue(std::string, bool value);
template void Dictionary::setValue(std::string, double);
template void Dictionary::setValue(std::string, int);
template void Dictionary::setValue(std::string, std::string);
template void Dictionary::setValue(std::string, std::filesystem::path);
template void Dictionary::setValue(std::string, void*);
template void Dictionary::setValue(std::string, std::vector<int>);
template void Dictionary::setValue(std::string, std::vector<double>);
template void Dictionary::setValue(std::string, std::vector<std::string>);
template void Dictionary::setValue(std::string, std::vector<std::filesystem::path>);
template void Dictionary::setValue(std::string, glm::ivec2);
template void Dictionary::setValue(std::string, glm::ivec3);
template void Dictionary::setValue(std::string, glm::ivec4);
template void Dictionary::setValue(std::string, glm::dvec2);
template void Dictionary::setValue(std::string, glm::dvec3);
template void Dictionary::setValue(std::string, glm::dvec4);
template void Dictionary::setValue(std::string, glm::dmat2x2);
template void Dictionary::setValue(std::string, glm::dmat2x3);
template void Dictionary::setValue(std::string, glm::dmat2x4);
template void Dictionary::setValue(std::string, glm::dmat3x2);
template void Dictionary::setValue(std::string, glm::dmat3x3);
template void Dictionary::setValue(std::string, glm::dmat3x4);
template void Dictionary::setValue(std::string, glm::dmat4x2);
template void Dictionary::setValue(std::string, glm::dmat4x3);
template void Dictionary::setValue(std::string, glm::dmat4x4);


template Dictionary Dictionary::value(std::string_view) const;
template bool Dictionary::value(std::string_view) const;
template double Dictionary::value(std::string_view) const;
template int Dictionary::value(std::string_view) const;
template std::string Dictionary::value(std::string_view) const;
template std::filesystem::path Dictionary::value(std::string_view) const;
template void* Dictionary::value(std::string_view) const;
template std::vector<int> Dictionary::value(std::string_view) const;
template std::vector<double> Dictionary::value(std::string_view) const;
template std::vector<std::string> Dictionary::value(std::string_view) const;
template std::vector<std::filesystem::path> Dictionary::value(std::string_view) const;
template glm::ivec2 Dictionary::value(std::string_view) const;
template glm::ivec3 Dictionary::value(std::string_view) const;
template glm::ivec4 Dictionary::value(std::string_view) const;
template glm::dvec2 Dictionary::value(std::string_view) const;
template glm::dvec3 Dictionary::value(std::string_view) const;
template glm::dvec4 Dictionary::value(std::string_view) const;
template glm::dmat2x2 Dictionary::value(std::string_view) const;
template glm::dmat2x3 Dictionary::value(std::string_view) const;
template glm::dmat2x4 Dictionary::value(std::string_view) const;
template glm::dmat3x2 Dictionary::value(std::string_view) const;
template glm::dmat3x3 Dictionary::value(std::string_view) const;
template glm::dmat3x4 Dictionary::value(std::string_view) const;
template glm::dmat4x2 Dictionary::value(std::string_view) const;
template glm::dmat4x3 Dictionary::value(std::string_view) const;
template glm::dmat4x4 Dictionary::value(std::string_view) const;


template bool Dictionary::hasValue<Dictionary>(std::string_view) const;
template bool Dictionary::hasValue<bool>(std::string_view) const;
template bool Dictionary::hasValue<double>(std::string_view) const;
template bool Dictionary::hasValue<int>(std::string_view) const;
template bool Dictionary::hasValue<std::string>(std::string_view) const;
template bool Dictionary::hasValue<std::filesystem::path>(std::string_view) const;
template bool Dictionary::hasValue<void*>(std::string_view) const;
template bool Dictionary::hasValue<std::vector<int>>(std::string_view) const;
template bool Dictionary::hasValue<std::vector<double>>(std::string_view) const;
template bool Dictionary::hasValue<std::vector<std::string>>(std::string_view) const;
template bool Dictionary::hasValue<std::vector<std::filesystem::path>>(
    std::string_view) const;
template bool Dictionary::hasValue<glm::ivec2>(std::string_view) const;
template bool Dictionary::hasValue<glm::ivec3>(std::string_view) const;
template bool Dictionary::hasValue<glm::ivec4>(std::string_view) const;
template bool Dictionary::hasValue<glm::dvec2>(std::string_view) const;
template bool Dictionary::hasValue<glm::dvec3>(std::string_view) const;
template bool Dictionary::hasValue<glm::dvec4>(std::string_view) const;
template bool Dictionary::hasValue<glm::dmat2x2>(std::string_view) const;
template bool Dictionary::hasValue<glm::dmat2x3>(std::string_view) const;
template bool Dictionary::hasValue<glm::dmat2x4>(std::string_view) const;
template bool Dictionary::hasValue<glm::dmat3x2>(std::string_view) const;
template bool Dictionary::hasValue<glm::dmat3x3>(std::string_view) const;
template bool Dictionary::hasValue<glm::dmat3x4>(std::string_view) const;
template bool Dictionary::hasValue<glm::dmat4x2>(std::string_view) const;
template bool Dictionary::hasValue<glm::dmat4x3>(std::string_view) const;
template bool Dictionary::hasValue<glm::dmat4x4>(std::string_view) const;

} // namespace ghoul
