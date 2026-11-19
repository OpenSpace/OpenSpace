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

#ifndef __OPENSPACE_TEST___TESTHELPER___H__
#define __OPENSPACE_TEST___TESTHELPER___H__

#include <openspace/misc/stringhelper.h>
#include <algorithm>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace openspace::test {

/**
 * Returns a unified-diff-style comparison of \p lhs and \p rhs. Lines that only exist in
 * \p lhs are prefixed with `- `, lines that only exist in \p rhs with `+ `, and lines
 * that are present in both with two spaces. This is a lot more readable than printing
 * both texts in full, which is what Catch2 does for a failing string comparison.
 *
 * Runs of unchanged lines that are further than \p nContextLines away from any change are
 * replaced with a single `...` line to keep the output short for large inputs.
 *
 * \param lhs The first of the two texts that are compared
 * \param rhs The second of the two texts that are compared
 * \param nContextLines The number of unchanged lines that are printed around each change
 * \return The line-by-line diff between \p lhs and \p rhs
 */
inline std::string lineDiff(const std::string& lhs, const std::string& rhs,
                            size_t nContextLines = 3)
{
    const std::vector<std::string> l = tokenizeString(lhs, '\n');
    const std::vector<std::string> r = tokenizeString(rhs, '\n');

    // lcs[i][j] is the length of the longest common subsequence of l[i:] and r[j:]. A
    // plain index-by-index comparison is not good enough here as a single inserted line
    // would make every following line show up as different
    std::vector<std::vector<size_t>> lcs = std::vector<std::vector<size_t>>(
        l.size() + 1,
        std::vector<size_t>(r.size() + 1, 0)
    );
    for (size_t i = l.size(); i-- > 0;) {
        for (size_t j = r.size(); j-- > 0;) {
            lcs[i][j] = l[i] == r[j] ?
                lcs[i + 1][j + 1] + 1 :
                std::max(lcs[i + 1][j], lcs[i][j + 1]);
        }
    }

    // Walk the table to collect every line together with the marker it should get
    std::vector<std::pair<std::string_view, std::string_view>> lines;
    size_t i = 0;
    size_t j = 0;
    while (i < l.size() && j < r.size()) {
        if (l[i] == r[j]) {
            lines.emplace_back("  ", l[i]);
            i++;
            j++;
        }
        else if (lcs[i + 1][j] >= lcs[i][j + 1]) {
            lines.emplace_back("- ", l[i]);
            i++;
        }
        else {
            lines.emplace_back("+ ", r[j]);
            j++;
        }
    }
    for (; i < l.size(); i++) {
        lines.emplace_back("- ", l[i]);
    }
    for (; j < r.size(); j++) {
        lines.emplace_back("+ ", r[j]);
    }

    // Only print unchanged lines that are close enough to an actual change
    std::vector<bool> isPrinted = std::vector<bool>(lines.size(), false);
    for (size_t k = 0; k < lines.size(); k++) {
        if (lines[k].first == "  ") {
            continue;
        }
        const size_t begin = k > nContextLines ? k - nContextLines : 0;
        const size_t end = std::min(k + nContextLines + 1, lines.size());
        for (size_t c = begin; c < end; c++) {
            isPrinted[c] = true;
        }
    }

    std::ostringstream out;
    bool wasSkipping = false;
    for (size_t k = 0; k < lines.size(); k++) {
        if (isPrinted[k]) {
            out << lines[k].first << lines[k].second << '\n';
            wasSkipping = false;
        }
        else if (!wasSkipping) {
            out << "  ...\n";
            wasSkipping = true;
        }
    }
    return out.str();
}

/**
 * Returns whether \p diff, as returned by #lineDiff, contains at least one added or
 * removed line.
 *
 * \param diff The diff that is checked for differences
 * \return `true` if \p diff contains any added or removed line, `false` otherwise
 */
inline bool hasDifferences(const std::string& diff) {
    const std::vector<std::string> lines = tokenizeString(diff, '\n');
    return std::any_of(
        lines.begin(),
        lines.end(),
        [](const std::string& line) {
            return line.starts_with("- ") || line.starts_with("+ ");
        }
    );
}

} // namespace openspace::test

#endif // __OPENSPACE_TEST___TESTHELPER___H__
