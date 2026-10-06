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

#include "util.h"

#include <cassert>
#include <cctype>
#include <string_view>
#include <vector>

bool isEmptyLine(std::string_view sv) {
    if (sv.empty()) {
        return true;
    }
    else {
        const size_t b = sv.find_first_not_of(" \t\n");
        return b == std::string_view::npos;
    }
}

std::string_view strip(std::string_view sv) {
    if (const size_t b = sv.find_first_not_of(" \t\n");  b != std::string_view::npos) {
        sv.remove_prefix(b);
    }

    while (!sv.empty() && isspace(sv[sv.size() - 1]) != 0) {
        sv.remove_suffix(1);
    }

    return sv;
}

bool startsWith(std::string_view base, std::string_view test) {
    assert(!base.empty());
    assert(!test.empty());

    return base.size() >= test.size() && base.substr(0, test.size()) == test;
}

std::vector<std::string_view> extractTemplateTypeList(std::string_view types) {
    assert(!types.empty());

    std::vector<std::string_view> res;

    size_t base = 0;
    size_t cursor = 0;
    size_t nBrackets = 1;
    do {
        cursor++;

        if (types[cursor] == '<') {
            nBrackets += 1;
        }

        if (types[cursor] == '>') {
            nBrackets -= 1;
        }

        const bool isExternalSeparator = (types[cursor] == ',') && (nBrackets == 1);
        const bool isFinalizingBracket = (types[cursor] == '>') && (nBrackets == 0);
        if (isExternalSeparator || isFinalizingBracket) {
            const std::string_view subtype = types.substr(base, cursor - base);
            res.push_back(strip(subtype));
            base = cursor + 1;
        }
    } while (nBrackets > 0);

    return res;
}
