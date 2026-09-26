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

#include <ghoul/misc/dictionaryjsonformatter.h>

#include <ghoul/glm.h>
#include <ghoul/misc/dictionary.h>
#include <algorithm>
#include <charconv>
#include <cmath>
#include <iterator>
#include <numeric>
#include <string>
#include <sstream>
#include <string_view>
#include <system_error>
#include <type_traits>
#include <utility>

namespace {
    using namespace ghoul;

    std::string formatNumber(double d) {
        // std::format will represent infinite values with 'inf' and NaNs with 'nan'.
        // These are not valid in JSON, so use 'null' instead
        if (!std::isfinite(d)) {
            return "null";
        }

        return std::format("{}", d);
    }

    std::string formatString(const std::string& value) {
        std::string jsonString;
        for (const char& c : value) {
            switch (c) {
            case '"':
                jsonString += "\\\"";
                break;
            case '\\':
                jsonString += "\\\\";
                break;
            case '\b':
                jsonString += "\\b";
                break;
            case '\f':
                jsonString += "\\f";
                break;
            case '\n':
                jsonString += "\\n";
                break;
            case '\r':
                jsonString += "\\r";
                break;
            case '\t':
                jsonString += "\\t";
                break;
            default:
                jsonString += c;
            }
        }

        return std::format("\"{}\"", jsonString);
    }

    template <typename T>
    std::string formatVector(const std::vector<T>& vec) {
        static_assert(
            std::is_same_v<T, double> || std::is_same_v<T, int> ||
            std::is_same_v<T, std::string>,
            "Only double, ints, or strings are allowed in vectors"
        );
        if (vec.empty()) {
            return "[]";
        }

        std::stringstream values;
        for (size_t i = 0; i < vec.size() - 1; i++) {
            if constexpr (std::is_arithmetic_v<T>) {
                const double v = static_cast<double>(vec[i]);
                values << formatNumber(v) << ",";
            }
            else {
                values << formatString(vec[i]) << ",";
            }
        }
        values << vec.back();

        return std::format("[{}]", values.str());
    }

    /**
    * Converts a single value \p key out of the \p dictionary by manually iterating all
    * the types and trying to access them.
    *
    * \param dictionary The Dictionary from which the \p key should be extracted and
    *        converted
    * \param key The key in the Dictionary that should be converted
    * \return A JSON representation of the \p key's value
    *
    * \throw JsonFormattingError If the \p key points to a type that cannot be converted
    */
    std::string formatValue(const Dictionary& dictionary, const std::string& key) {
        if (dictionary.hasValue<Dictionary>(key)) {
            const Dictionary subDictionary = dictionary.value<Dictionary>(key);
            return formatJson(subDictionary);
        }

        if (dictionary.hasValue<double>(key)) {
            const double value = dictionary.value<double>(key);
            return formatNumber(value);
        }

        if (dictionary.hasValue<int>(key)) {
            const int value = dictionary.value<int>(key);
            return formatNumber(static_cast<double>(value));
        }

        if (dictionary.hasValue<bool>(key)) {
            const bool value = dictionary.value<bool>(key);
            return value ? "true" : "false";
        }

        if (dictionary.hasValue<std::vector<int>>(key)) {
            const std::vector<int> vec = dictionary.value<std::vector<int>>(key);
            return formatVector(vec);
        }

        if (dictionary.hasValue<std::vector<double>>(key)) {
            const std::vector<double> vec = dictionary.value<std::vector<double>>(key);
            return formatVector(vec);
        }

        if (dictionary.hasValue<std::vector<std::string>>(key)) {
            const std::vector<std::string> vec =
                dictionary.value<std::vector<std::string>>(key);
            return formatVector(vec);
        }

        if (dictionary.hasValue<std::string>(key)) {
            const std::string value = dictionary.value<std::string>(key);
            return formatString(value);
        }

        throw JsonFormattingError(std::format(
            "Key '{}' has invalid type for formatting dictionary as JSON", key
        ));
    }
} // namespace

namespace ghoul {

JsonFormattingError::JsonFormattingError(std::string msg)
    : RuntimeError(std::move(msg), "Dictionary")
{}

std::string formatJson(const Dictionary& dictionary) {
    if (dictionary.isEmpty()) {
        return "{}";
    }

    std::vector<std::string> keys;
    std::vector<int> intKeys;
    for (const std::string_view k : dictionary.keys()) {
        keys.emplace_back(k);
        int result;
        auto [p, ec] = std::from_chars(k.data(), k.data() + k.size(), result);
        if (ec == std::errc()) {
            intKeys.emplace_back(result);
        }
    }
    std::sort(intKeys.begin(), intKeys.end());

    // Check whether the dictionary contains only numerical and sequential keys
    std::vector<int> seqRef = std::vector<int>(keys.size());
    std::iota(seqRef.begin(), seqRef.end(), 1);
    if (intKeys == seqRef) {
        auto convert = [](const std::string& key, const Dictionary& d) {
            return formatValue(d, key);
        };
        const std::string json = std::accumulate(
            std::next(keys.begin()),
            keys.end(),
            convert(*keys.begin(), dictionary),
            [convert, dictionary](const std::string& a, const std::string& key) {
                return std::format("{},{}", a, convert(key, dictionary));
            }
        );
        return std::format("[{}]", json);
    }
    else {
        auto convert = [](const std::string& key, const Dictionary& d) {
            return "\"" + key + "\":" + formatValue(d, key);
        };
        const std::string json = std::accumulate(
            std::next(keys.begin()),
            keys.end(),
            convert(*keys.begin(), dictionary),
            [convert, dictionary](const std::string& a, const std::string& key) {
                return std::format("{},{}", a, convert(key, dictionary));
            }
        );
        return std::format("{{{}}}", json);
    }
}

} // namespace ghoul
