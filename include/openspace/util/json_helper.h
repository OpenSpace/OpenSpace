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

#ifndef __OPENSPACE_CORE___JSON_HELPER___H__
#define __OPENSPACE_CORE___JSON_HELPER___H__

#include <openspace/json.h>
#include <ghoul/misc/dictionary.h>
#include <string>

namespace openspace {

/**
 * This function takes a \p text and escapes all necessary characters that JSON does not
 * want in its strings.
 *
 * \param text The text that is to be escaped
 * \return The same text with all required characteres escaped
 */
std::string escapedJson(const std::string& text);

/**
 * This function takes a \p list of text and escapes all necessary characters that JSON
 * does not want in its strings.
 *
 * \param list The list of text that is to be escaped
 * \return The same text with all required characteres escaped
 */
std::string escapedJson(const std::vector<std::string>& list);

/**
 * Convert the input value to a valid JSON formatted string. Nan and Inf values are not
 * valid JSON and will be represented by `null`.
 *
 * \param d The value to format
 * \return The resulting JSON formatted string
 */
std::string formatJsonNumber(double d);

/**
 * Convert the input value to a valid JSON formatted string.
 *
 * \param value The value to be converted
 * \return The resulting JSON formatted string
 */
template <typename T>
std::string formatJson(T value);

/**
 * Sort a json object that is an array of objects with the structure
 * `[ key = {}, key = {} ...]`. Sorts it by the provided key.
 *
 * \param json The JSON object to sort
 * \param key The key the JSON should be sorted by
 */
void sortJson(nlohmann::json& json, const std::string& key);

/**
 * Converts the provided JSON object into its corresponding Dictionary format. Please note
 * that if the JSON contains keys that array of an array type, they are converted into a
 * Dictionary with numerical keys and the numerical keys start with 1.
 */
ghoul::Dictionary jsonToDictionary(const nlohmann::json& json);

} // namespace openspace

#include "json_helper.inl"

#endif // __OPENSPACE_CORE___JSON_HELPER___H__
