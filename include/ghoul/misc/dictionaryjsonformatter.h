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

#ifndef __GHOUL___DICTIONARYJSONFORMATTER___H__
#define __GHOUL___DICTIONARYJSONFORMATTER___H__

#include <ghoul/misc/exception.h>
#include <string>

namespace ghoul {

class Dictionary;

/**
 * This exception is thrown if an unknown type is being converted.
 */
struct JsonFormattingError final : public RuntimeError {
    explicit JsonFormattingError(std::string msg);
};

/**
 * Converts the passed \p dictionary into a JSON string representation.
 *
 * \param dictionary The Dictionary that should be converted
 * \return A JSON string representing the Dictionary
 *
 * \throw JsonFormattingError If the \p key points to a type that cannot be converted
 */
std::string formatJson(const Dictionary& dictionary);

} // namespace ghoul

#endif // __GHOUL___DICTIONARYJSONFORMATTER___H__
