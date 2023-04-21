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

#include <openspace/documentation/documentationgenerator.h>

#include <openspace/openspace.h>
#include <openspace/util/time.h>
#include <ghoul/filesystem/filesystem.h>
#include <ghoul/misc/invariants.h>
#include <fstream>

namespace openspace {

const std::string DocumentationGenerator::DataTag = "Data";
const std::string DocumentationGenerator::NameTag = "Name";

DocumentationGenerator::DocumentationGenerator(std::string name,
                                               std::string jsonName)
    : _name(std::move(name))
    , _jsonName(std::move(jsonName))
{
    ghoul_precondition(!_name.empty(), "name must not be empty");
    ghoul_precondition(!_jsonName.empty(), "jsonName must not be empty");
}


std::string DocumentationGenerator::jsonName() {
    return _jsonName;
}

void DocumentationGenerator::sortJson(nlohmann::json& json) const {
    std::sort(
        json.begin(),
        json.end(),
        [](const nlohmann::json& lhs, const nlohmann::json& rhs) {
            std::string lhsString = lhs["Name"];
            std::string rhsString = rhs["Name"];
            std::transform(
                lhsString.begin(),
                lhsString.end(),
                lhsString.begin(),
                [](unsigned char c) { return std::tolower(c); }
            );
            std::transform(
                rhsString.begin(),
                rhsString.end(),
                rhsString.begin(),
                [](unsigned char c) { return std::tolower(c); }
            );

            return rhsString > lhsString;
        });
}

} // namespace openspace
