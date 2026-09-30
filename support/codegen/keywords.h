/*****************************************************************************************
 *                                                                                       *
 * OpenSpace Codegen                                                                     *
 *                                                                                       *
 * Copyright (c) 2021-2026                                                               *
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

#ifndef __OPENSPACE_CODEGEN___KEYWORDS___H__
#define __OPENSPACE_CODEGEN___KEYWORDS___H__

#include <string_view>
#include <utility>

namespace keywords {

constexpr std::string_view Verbatim = "verbatim";
constexpr std::string_view Dictionary = "Dictionary";
constexpr std::string_view Enum = "enum";
constexpr std::string_view Stringify = "stringify";
constexpr std::string_view Map = "map";
constexpr std::string_view LuaWrap = "luawrap";
constexpr std::string_view Arrayify = "arrayify";

constexpr std::string_view Annotation = "annotation";
constexpr std::string_view Color = "color";
constexpr std::string_view DateTime = "datetime";
constexpr std::string_view Directory = "directory";
constexpr std::string_view Identifier = "identifier";
constexpr std::string_view InList = "inlist";
constexpr std::string_view InRange = "inrange";
constexpr std::string_view Less = "less";
constexpr std::string_view LessEqual = "lessequal";
constexpr std::string_view Greater = "greater";
constexpr std::string_view GreaterEqual = "greaterequal";
constexpr std::string_view MustExist = "mustexist";
constexpr std::string_view NotInList = "notinlist";
constexpr std::string_view NotInRange = "notinrange";
constexpr std::string_view Reference = "reference";
constexpr std::string_view Unequal = "unequal";

constexpr std::string_view Key = "key";

constexpr std::string_view Private = "private";

constexpr std::string_view NoExhaustive = "noexhaustive";
constexpr std::string_view MustBeNotEmpty = "notempty";

} // namespace keywords

/**
 * Looks for the occurance of a keyword in the provided text. If the full keyword,
 * including the prefix `[[codegen::` exists, the beginning and last is returned as
 * indices. If the keyword was not found two `std::string_view::npos` values are returned.
 */
constexpr std::pair<size_t, size_t> findKeyword(std::string_view text,
                                                std::string_view keyword)
{
    constexpr std::string_view Prefix = "[[codegen::";

    const size_t prefixIdx = text.find(Prefix);
    const size_t kwdIdx = text.find(keyword, prefixIdx);

    if (prefixIdx == std::string_view::npos || kwdIdx == std::string_view::npos) {
        // One of the components was not found
        return { std::string_view::npos, std::string_view::npos };
    }

    // We found the keyword if the prefix and the keyword follow each other directly
    const bool found = kwdIdx == (prefixIdx + Prefix.length());
    if (!found) {
        return { std::string_view::npos, std::string_view::npos };
    }

    const size_t begin = prefixIdx;
    const size_t end = kwdIdx + keyword.size();
    return { begin, end };
}

#endif // __OPENSPACE_CODEGEN___KEYWORDS___H__
