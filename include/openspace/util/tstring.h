/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#ifndef __OPENSPACE_CORE___TSTRING___H__
#define __OPENSPACE_CORE___TSTRING___H__

#include <string>
#include <string_view>

namespace openspace {

/**
 * This string is a temporary string that is generated using the temporary memory storage.
 * This means that under no circumstances must an instance of a tstring be kept across
 * frame boundaries as the temporary storage is reset at between frames. In exchange, the
 * allocation of these objects is extreme fast and with barely any overhead associated
 * with it. The memory accessed through a tstring object shall never be released manually.
 */
using tstring = std::string_view;

/**
 * Allocate and create a temporary string from the passed `std::string`.
 *
 * \param str The string to be copied into a newly allocated tstring
 * \return The copy of the str as a temporary string
 */
tstring temporaryString(const std::string& str);

/**
 * Allocate and create a temporary string from the passed `std::string_view`.
 *
 * \param str The string to be copied into a newly allocated #tstring
 * \return The copy of the str as a temporary string
 */
tstring temporaryString(std::string_view str);

/**
 * Allocate and create a temporary string from the passed char array.
 *
 * \param str The string to be copied into a newly allocated #tstring
 * \return The copy of the str as a temporary string
 */
tstring temporaryString(const char str[]);

} // namespace openspace

#endif // __OPENSPACE_CORE___TSTRING___H__
