/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <openspace/util/json_helper.h>

namespace openspace {

std::string escapedJson(const std::string& text) {
    std::string jsonString;
    jsonString.reserve(text.size());
    for (const char& c : text) {
        switch (c) {
            case '\b':
                jsonString += "\\b";
                break;
            case '\t':
                jsonString += "\\t"; // Replace tab with \t.
                break;
            case '\n':
                jsonString += "\\\\n"; // Replace newline with \n.
                break;
            case '\f':
                jsonString += "\\f";
                break;
            case '\r':
                jsonString += "\\r"; // Replace carriage return with \r.
                break;
            case '"':
                jsonString += "\\\""; // Replace " with \".
                break;
            case '\\':
                jsonString += "\\\\"; // Replace \ with \\.
                break;
            default:
                jsonString += c;
        }
    }
    return jsonString;
}

std::string escapedJson(const std::vector<std::string>& list) {
    std::string jsonString;
    jsonString += "[";
    for (const std::string& text : list) {
        jsonString += "\\\"";
        jsonString += escapedJson(text);
        jsonString += "\\\",";
    }
    if (jsonString.length() > 1) {
        jsonString.pop_back();
    }
    jsonString += "]";

    return jsonString;
}

std::string formatJsonNumber(double d) {
    // to_string will represent infinite values with 'inf' and NaNs with 'nan'.
    // These are not valid in JSON, so use 'null' instead
    if (!std::isfinite(d)) {
        return "null";
    }
    return std::to_string(d);
}

}  // namespace openspace
