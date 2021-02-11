/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2021                                                               *
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

#include <openspace/documentation/documentationgenerator.h>

#include <openspace/openspace.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/invariants.h>
#include <fstream>

namespace openspace {

DocumentationGenerator::DocumentationGenerator(std::string name,
                                               std::string jsonName,
                                        std::vector<HandlebarTemplate> handlebarTemplates)
    : _name(std::move(name))
    , _jsonName(std::move(jsonName))
    , _handlebarTemplates(std::move(handlebarTemplates))
{
    ghoul_precondition(!_name.empty(), "name must not be empty");
    ghoul_precondition(!_jsonName.empty(), "jsonName must not be empty");
    for (const HandlebarTemplate& t : _handlebarTemplates) {
        (void)t; // Unused variable in Release mode
        ghoul_precondition(!t.name.empty(), "name must not be empty");
        ghoul_precondition(!t.filename.empty(), "filename must not be empty");
    }
}

std::vector<DocumentationGenerator::HandlebarTemplate>
DocumentationGenerator::templatesToRegister()
{
    return _handlebarTemplates;
}

std::string DocumentationGenerator::jsonName() {
    return _jsonName;
}

std::string escapedJson(const std::vector<std::string>& list) {
    std::string jsonString;
    jsonString += "[";
    for (const std::string& text : list) {
        jsonString += "\\\"";
        for (const char& c : text) {
            switch (c) {
            case '\t':
                jsonString += "\\t"; // Replace tab with \t.
                break;
            case '"':
                jsonString += "\\\""; // Replace " with \".
                break;
            case '\\':
                jsonString += "\\\\"; // Replace \ with \\.
                break;
            case '\n':
                jsonString += "\\\\n"; // Replace newline with \n.
                break;
            case '\r':
                jsonString += "\\r"; // Replace carriage return with \r.
                break;
            default:
                jsonString += c;
            }
        }
        jsonString += "\\\",";
    }
    if (jsonString.length() > 1) {
        jsonString.pop_back();
    }
    jsonString += "]";

    return jsonString;
}

std::string escapedJson(const std::string& text) {
    std::string jsonString;
    for (const char& c : text) {
        switch (c) {
        case '\t':
            jsonString += "\\t"; // Replace tab with \t.
            break;
        case '"':
            jsonString += "\\\""; // Replace " with \".
            break;
        case '\\':
            jsonString += "\\\\"; // Replace \ with \\.
            break;
        case '\n':
            jsonString += "\\\\n"; // Replace newline with \n.
            break;
        case '\r':
            jsonString += "\\r"; // Replace carriage return with \r.
            break;
        default:
            jsonString += c;
        }
    }
    return jsonString;
}

} // namespace openspace
