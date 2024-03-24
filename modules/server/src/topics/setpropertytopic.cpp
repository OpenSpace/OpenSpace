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

#include <modules/server/include/topics/setpropertytopic.h>

#include <openspace/json.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <openspace/query/query.h>
#include <openspace/util/timemanager.h>
#include <openspace/util/time.h>
#include <ghoul/fmt.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr std::string_view _loggerCat = "SetPropertyTopic";
    constexpr std::string_view SpecialKeyTime = "__time";

    std::string escapedLuaString(const std::string& str) {
        std::string luaString;

        for (const char& c : str) {
            switch (c) {
            case '\t':
                luaString += "\\t"; // Replace tab with \t.
                break;
            case '"':
                luaString += "\\\""; // Replace " with \".
                break;
            case '\\':
                luaString += "\\\\"; // Replace \ with \\.
                break;
            case '\n':
                luaString += "\\\\n"; // Replace newline with \n.
                break;
            case '\r':
                luaString += "\\r"; // Replace carriage return with \r.
                break;
            default:
                luaString += c;
            }
        }

        return luaString;
    }

    std::string luaLiteralFromJson(nlohmann::json value) {
        if (value.is_string()) {
            return "'" + escapedLuaString(value.get<std::string>()) + "'";
        }
        else if (value.is_boolean()) {
            return value.get<bool>() ? "true" : "false";
        }
        else if (value.is_number()) {
            return std::to_string(value.get<double>());
        }
        else if (value.is_array()) {
            if (value.empty()) {
                return "{}";
            }

            std::string literal = "{";
            for (nlohmann::json::iterator it = value.begin(); it != value.end(); it++) {
                literal += luaLiteralFromJson(it.value()) += ",";
            }
            literal.pop_back(); // remove last comma
            literal += "}";
            return literal;
        }
        else if (value.is_object()) {
            if (value.empty()) {
                return "{}";
            }

            std::string literal = "{";
            for (nlohmann::json::iterator it = value.begin(); it != value.end(); it++) {
                literal += it.key() + "=" + luaLiteralFromJson(it.value()) += ",";
            }
            literal.pop_back(); // remove last comma
            literal += "}";
            return literal;
        }
        else {
            return "nil";
        }
    }
} // namespace

namespace openspace {

void SetPropertyTopic::handleJson(const nlohmann::json& json) {
    try {
        const std::string& propertyKey = json.at("property").get<std::string>();

        if (propertyKey == SpecialKeyTime) {
            Time newTime;
            newTime.setTime(json.at("value").get<std::string>());
            global::timeManager->setTimeNextFrame(newTime);
        }
        else {
            const nlohmann::json value = json.at("value");
            std::string literal = luaLiteralFromJson(value);

            global::scriptEngine->queueScript(
                std::format(
                    "openspace.setPropertyValueSingle(\"{}\", {})", propertyKey, literal
                ),
                scripting::ScriptEngine::ShouldBeSynchronized::Yes,
                scripting::ScriptEngine::ShouldSendToRemote::Yes
             );
        }
    }
    catch (const std::out_of_range& e) {
        LERROR("Could not set property -- key or value is missing in payload");
        LERROR(e.what());
    }
    catch (const ghoul::RuntimeError& e) {
        LERROR("Could not set property -- runtime error:");
        LERROR(e.what());
    }
    catch (...) {
        LERROR("Could not set property -- unknown error");
    }
}

bool SetPropertyTopic::isDone() const {
    return true;
}

} // namespace openspace
