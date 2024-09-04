/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <ghoul/misc/stringhelper.h>

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

void sortJson(nlohmann::json& json, const std::string& key) {
    std::sort(
        json.begin(),
        json.end(),
        [&key](const nlohmann::json& lhs, const nlohmann::json& rhs) {
            const std::string lhsString = ghoul::toLowerCase(lhs[key]);
            const std::string rhsString = ghoul::toLowerCase(rhs[key]);

            return rhsString > lhsString;
        }
    );
}

ghoul::Dictionary jsonToDictionary(const nlohmann::json& json) {
    if (!json.is_object()) {
        throw ghoul::RuntimeError("Provided JSON is not an object type");
    }

    using Func = std::function<
        void(ghoul::Dictionary& dict, std::string key, const nlohmann::json& j)
    >;
    Func addToDict = [&addToDict](ghoul::Dictionary& dict, std::string key,
                                  const nlohmann::json& j)
    {
        switch (j.type()) {
        case nlohmann::json::value_t::null:
        case nlohmann::json::value_t::discarded:
                break;
            case nlohmann::json::value_t::object: {
                ghoul::Dictionary subDict = jsonToDictionary(j);
                dict.setValue(std::move(key), std::move(subDict));
                break;
            }
            case nlohmann::json::value_t::array: {
                // We can't represent arrays with different types, so we have to use a
                // Dictionary for that instead
                ghoul::Dictionary subDict;
                for (int i = 0; i < j.size(); i++) {
                    const nlohmann::json& value = j[i];
                    // We add 1 to the key to make Lua happy :-/
                    addToDict(subDict, std::format("{}", i + 1), value);
                }
                dict.setValue(std::move(key), std::move(subDict));
                break;
            }
            case nlohmann::json::value_t::string:
                dict.setValue(std::move(key), j.get<std::string>());
                break;
            case nlohmann::json::value_t::boolean:
                dict.setValue(std::move(key), j.get<bool>());
                break;
            case nlohmann::json::value_t::number_integer:
            case nlohmann::json::value_t::number_unsigned:
            case nlohmann::json::value_t::number_float:
                dict.setValue(std::move(key), j.get<double>());
                break;
            case nlohmann::json::value_t::binary:
                throw ghoul::RuntimeError(
                    "Binary format conversion to Dictionary is unsupported. Please "
                    "create an issue with an example of the file that lead to this error"
                );
        }
    };


    ghoul::Dictionary result;
    for (auto& [key, value] : json.get<nlohmann::json::object_t>()) {
        addToDict(result, key, value);
    }
    return result;
}

}  // namespace openspace
