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

#include <ghoul/misc/stringhelper.h>

#include <ghoul/misc/assert.h>
#include <algorithm>
#include <cctype>

namespace ghoul {

std::string toUpperCase(const std::string& s) {
    std::string t;
    t.resize(s.size());
    std::transform(
        s.begin(),
        s.end(),
        t.begin(),
        [](unsigned char c) { return std::toupper(c); }
    );
    return t;
}

std::string toLowerCase(const std::string& s) {
    std::string t;
    t.resize(s.size());
    std::transform(
        s.begin(),
        s.end(),
        t.begin(),
        [](unsigned char c) { return std::tolower(c); }
    );
    return t;
}

std::vector<std::string> tokenizeString(const std::string& input, char separator) {
    size_t separatorPos = input.find(separator);
    if (separatorPos == std::string::npos) {
        return { input };
    }

    std::vector<std::string> result;
    size_t prevSeparator = 0;
    while (separatorPos != std::string::npos) {
        result.push_back(input.substr(prevSeparator, separatorPos - prevSeparator));
        prevSeparator = separatorPos + 1;
        separatorPos = input.find(separator, separatorPos + 1);
    }
    result.push_back(input.substr(prevSeparator));
    return result;
}

std::string join(std::vector<std::string> input, const std::string& separator) {
    if (input.empty()) {
        return std::string();
    }

    std::string result;
    for (std::string& s : input) {
        result += std::move(s) + separator;
    }

    return result.substr(0, result.size() - separator.size());
}

void trimWhitespace(std::string& value) {
    // Trim from the left until the first non-whitespace character
    value.erase(
        value.begin(),
        std::find_if(value.begin(), value.end(), [](int ch) { return !std::isspace(ch); })
    );

    // Trim from the right until the first non-whitespace character
    value.erase(
        std::find_if(
            value.rbegin(),
            value.rend(),
            [](int ch) { return !std::isspace(ch); }
        ).base(),
        value.end()
    );
}

void trimWhitespace(std::string_view& value) {
    constexpr std::string_view Ws = " \n\r\t";

    if (value.empty()) {
        return;
    }

    if (size_t it = value.find_first_not_of(Ws);  it != std::string_view::npos) {
        value.remove_prefix(it);
    }

    for (size_t it = value.size() - 1; ; it--) {
        if (value[it] != '\n' && value[it] != '\r' &&
            value[it] != '\t' && value[it] != ' ')
        {
            value.remove_suffix(value.size() - (it + 1));
            break;
        }

        // Can't use this check inside the for loop as the -1 would cause it to underflow
        // to std::string_view::npos and things would break
        if (it == 0) {
            break;
        }
    }
}

void trimSurroundingCharacters(std::string& valueString, const char charToRemove) {
    while (valueString.front() == charToRemove) {
        valueString.erase(0, 1);
    }
    while (valueString.back() == charToRemove) {
        valueString.pop_back();
    }
}

std::string replaceAll(std::string string, const std::string& from,
                       const std::string& to)
{
    ghoul_assert(!from.empty(), "from must not be the empty string");

    size_t pos = string.find(from);
    while (pos != std::string::npos) {
        string.replace(pos, from.length(), to);

        // In case 'to' contains 'from', ex replacing 'x' with 'yx'
        const size_t offset = pos + to.length();
        pos = string.find(from, offset);
    }
    return string;
}

std::string encodeUrl(const std::string& string) {
    std::string result;
    result = replaceAll(string, " ", "%20");
    result = replaceAll(result, "#", "%23");
    result = replaceAll(result, "$", "%24");
    result = replaceAll(result, "&", "%26");
    result = replaceAll(result, "+", "%2B");
    result = replaceAll(result, ",", "%2C");
    result = replaceAll(result, "/", "%2F");
    result = replaceAll(result, ":", "%3A");
    result = replaceAll(result, ";", "%3B");
    result = replaceAll(result, "=", "%3D");
    result = replaceAll(result, "?", "%3F");
    result = replaceAll(result, "@", "%40");
    result = replaceAll(result, "[", "%5B");
    result = replaceAll(result, "]", "%5D");
    return result;
}

std::istream& getline(std::istream& inputStream, std::string& str) {
    std::getline(inputStream, str);
#ifndef WIN32
    if (!str.empty() && (str.back() == '\r')) {
        str.pop_back();
    }
#endif // WIN32
    return inputStream;
}

std::istream& getline(std::istream& inputStream, std::string& str, char delim) {
    std::getline(inputStream, str, delim);
#ifndef WIN32
    if (!str.empty() && (str.back() == '\r')) {
        str.pop_back();
    }
#endif // WIN32
    return inputStream;
}

std::string toAsciiSafePathString(const std::filesystem::path& p, char replacement) {
    std::u8string s8 = p.generic_u8string();
    std::string result;
    result.reserve(s8.size());

    for (size_t i = 0; i < s8.size(); ) {
        unsigned char byte = s8[i];

        size_t charLen = 1;

        // Determine length of UTF-8 character
        if ((byte & 0b10000000) == 0) {
            charLen = 1; // ASCII
        }
        else if ((byte & 0b11100000) == 0b11000000) {
            charLen = 2; // 2-byte UTF-8
        }
        else if ((byte & 0b11110000) == 0b11100000) {
            charLen = 3; // 3-byte UTF-8
        }
        else if ((byte & 0b11111000) == 0b11110000) {
            charLen = 4; // 4-byte UTF-8
        }

        // If ASCII, copy as-is; otherwise, replace with replacement character
        if (charLen == 1) {
            result.push_back(static_cast<char>(byte));
        }
        else {
            result.push_back(replacement);
        }

        i += charLen;
    }

    return result;
}

bool containsNonAscii(const std::filesystem::path& p) {
    const std::u8string s = p.generic_u8string();
    for (auto it = s.begin(); it != s.end(); it++) {
        if (static_cast<unsigned char>(*it) > 0x7F) {
            return true;
        }
    }
    return false;
}

} // namespace ghoul
