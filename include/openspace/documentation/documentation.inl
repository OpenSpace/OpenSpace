/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/properties/propertyinfo.h>
#include <concepts>
#include <format>
#include <ranges>
#include <type_traits>

namespace openspace::internal {

/// A glm vector or matrix, which provide a static `length` function and element access
template <typename T>
concept GlmType = requires(const T& v) {
    T::length();
    v[0];
};

/// A contiguous range of characters, such as std::string or std::string_view
template <typename T>
concept CharacterRange =
    std::ranges::contiguous_range<T> && std::same_as<std::ranges::range_value_t<T>, char>;

template <typename T>
std::string formatDefaultValue(const T& value);

/**
 * Returns the comma-separated components of the glm vector or matrix \p value. Matrices
 * are flattened column by column.
 */
template <GlmType T>
std::string glmComponents(const T& value) {
    std::string res;
    for (decltype(T::length()) i = 0; i < T::length(); i++) {
        if (i > 0) {
            res += ", ";
        }

        using Element = std::remove_cvref_t<decltype(value[i])>;
        if constexpr (GlmType<Element>) {
            res += glmComponents(value[i]);
        }
        else {
            res += formatDefaultValue(value[i]);
        }
    }
    return res;
}

/**
 * Converts the \p value into a string.
 */
template <typename T>
std::string formatDefaultValue(const T& value) {
    if constexpr (std::same_as<T, bool>) {
        return value ? "true" : "false";
    }
    else if constexpr (std::floating_point<T>) {
        // Always add a decimal point to make it clear that this is a floating point value
        std::string res = std::format("{}", value);
        if (res.find_first_not_of("-0123456789") == std::string::npos) {
            res += ".0";
        }
        return res;
    }
    else if constexpr (std::integral<T>) {
        return std::format("{}", value);
    }
    else if constexpr (CharacterRange<T>) {
        return std::format(
            "\"{}\"",
            std::string_view(std::ranges::data(value), std::ranges::size(value))
        );
    }
    else if constexpr (GlmType<T>) {
        return std::format("{{ {} }}", glmComponents(value));
    }
    else if constexpr (std::ranges::range<T>) {
        std::string res;
        for (const auto& v : value) {
            if (!res.empty()) {
                res += ", ";
            }
            res += formatDefaultValue(v);
        }
        return std::format("{{ {} }}", res);
    }
    else {
        static_assert(sizeof(T) == 0, "Missing formatting for default value type");
        return "";
    }
}

} // namespace openspace::internal

namespace openspace {

template <typename Info>
std::string descriptionWithDefaultValue(const Info& info, std::string_view comment) {
    std::string res = std::string(info.description);
    if (!comment.empty()) {
        if (!res.empty()) {
            res += " ";
        }
        res += comment;
    }

    if constexpr (requires { info.defaults.value; }) {
        using Defaults = std::remove_cvref_t<decltype(info.defaults)>;
        using T = std::remove_cvref_t<decltype(info.defaults.value)>;

        // OptionProperty uses ValueDefaults<int> to store the index of the default
        // option. Since the options are only added at runtime, the index on its own would
        // not be useful to show
        if constexpr (!std::same_as<Defaults, ValueDefaults<int>>) {
            const T& value = info.defaults.value;

            bool isEmpty = false;
            if constexpr (std::ranges::sized_range<T>) {
                isEmpty = std::ranges::empty(value);
            }

            if (!isEmpty) {
                if (!res.empty()) {
                    res += " ";
                }
                res += std::format(
                    "Default value: {}", internal::formatDefaultValue(value)
                );
            }
        }
    }

    return res;
}

} // namespace openspace
