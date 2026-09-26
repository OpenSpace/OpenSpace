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

#include <ghoul/misc/assert.h>

#include <ghoul/format.h>
#include <ghoul/misc/stacktrace.h>
#include <ghoul/misc/stringhelper.h>
#include <algorithm>
#include <cstdlib>
#include <iostream>
#include <utility>
#include <vector>

#ifdef _MSC_VER
#include <intrin.h>
#endif // _MSC_VER

namespace {
    constexpr bool AlwaysAssert = false;

    std::vector<std::string> PermanentlyIgnoredAsserts;

    std::string hashing(const std::string& file, int line) {
        return file + "||" + std::to_string(line);
    }

    void addPermanentlyIgnoredAssert(const std::string& file, int line) {
        PermanentlyIgnoredAsserts.emplace_back(hashing(file, line));
    }

    bool isPermanentlyIgnored(const std::string& file, int line) {
        const auto it = std::find(
            PermanentlyIgnoredAsserts.cbegin(),
            PermanentlyIgnoredAsserts.cend(),
            hashing(file, line)
        );
        return it != PermanentlyIgnoredAsserts.cend();
    }
} // namespace

namespace ghoul {

AssertionException::AssertionException(std::string exp, std::string msg,
                                       std::string file, std::string func, int line)
    : std::runtime_error(std::format(
        "{}, \"{}\" ({}:{} ({})",
        std::move(exp), std::move(msg), std::move(file), line, std::move(func)
    ))
{}

MissingCaseException::MissingCaseException()
    : std::logic_error("Missing Case")
{}

void internalAssert(std::string expression, std::string message, std::string file,
                    std::string function, int line)
{
    if (isPermanentlyIgnored(file, line)) {
        return;
    }

    constexpr std::string_view padding = "    ";

    std::cerr << std::format(
        "\n"
        "{0}File:       {1}, line {2}\n"
        "{0}Function:   {3}\n"
        "{0}Assertion:  {4}"
        "{0}{5}\n",
        padding, file, line, function, expression, message
    );

    if (AlwaysAssert) {
#ifdef _MSC_VER
        __debugbreak();
#endif // _MSC_VER

        throw AssertionException(
            std::move(expression),
            std::move(message),
            std::move(file),
            std::move(function),
            line
        );
    }

    while (true) {
        std::cerr <<
            "(I)gnore / Ignore (P)ermanently / (A)ssertion / (S)tacktrace / (E)xit: ";
        std::string inputLine;
        ghoul::getline(std::cin, inputLine);
        if (inputLine.empty()) {
            continue;
        }

        inputLine = toLowerCase(inputLine);

        if (inputLine[0] == 'i') {
            break;
        }
        else if (inputLine[0] == 'p') {
            addPermanentlyIgnoredAssert(file, line);
            break;
        }
        else if (inputLine[0] == 'a') {
#ifdef _MSC_VER
            __debugbreak();
#endif // _MSC_VER

            throw AssertionException(
                std::move(expression),
                std::move(message),
                std::move(file),
                std::move(function),
                line
            );
        }
        else if (inputLine[0] == 's') {
            std::vector<std::string> st = stackTrace();

            std::cerr << '\n';
            for (size_t i = 0; i < st.size(); i++) {
                std::cerr << i << ": " << st[i] << '\n';
            }
            std::cerr << '\n';

            continue;
        }
        else if (inputLine[0] == 'e') {
            exit(EXIT_FAILURE);
        }
    }
}

} // namespace ghoul
