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

#include <modules/server/include/topics/luascripttopic.h>

#include <modules/server/include/jsonconverters.h>
#include <modules/server/include/connection.h>
#include <openspace/json.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/dictionary.h>

namespace {
    constexpr std::string_view KeyScript = "script";
    constexpr std::string_view KeyFunction = "function";
    constexpr std::string_view KeyArguments = "arguments";
    constexpr std::string_view KeyReturn = "return";
    constexpr std::string_view KeyShouldBeSynchronized = "shouldBeSynchronized";
    constexpr std::string_view _loggerCat = "LuaScriptTopic";

    std::string formatLua(const nlohmann::json::const_iterator& it);

    std::string escapeLuaString(const std::string& s) {
        std::string output;
        for (const char& c : s) {
            switch (c) {
                case '\a': output += "\\\a"; break;
                case '\b': output += "\\\b"; break;
                case '\f': output += "\\\f"; break;
                case '\n' : output += "\\\n"; break;
                case '\r' : output += "\\\r"; break;
                case '\t' : output += "\\\t"; break;
                case '\v' : output += "\\\v"; break;
                case '\\' : output += "\\\\"; break;
                case '"' : output += "\\\""; break;
                case '\'' : output += "\\'"; break;
                default: output += c;
            }
        }
        return output;
    }

    std::string formatLuaString(const std::string& s) {
        return "\"" + escapeLuaString(s) + "\"";
    }

    std::string formatKeyValuePair(const nlohmann::json::const_iterator& it) {
        return "[" + formatLuaString(it.key()) + "] = " + formatLua(it);
    }

    std::string formatObjectAsLuaTable(const nlohmann::json& json) {
        std::string output = "{";
        auto it = json.begin();
        for (size_t i = 0; i < json.size(); i++, it++) {
            output += formatKeyValuePair(it);
            if (i < json.size() - 1) {
                output += ",";
            }
        }
        return output + "}";
    }

    std::string formatArrayAsLuaTable(const nlohmann::json& json) {
        std::string output = "{";
        auto it = json.begin();
        for (size_t i = 0; i < json.size(); i++, it++) {
            output += formatLua(it);
            if (i < json.size() - 1) {
                output += ",";
            }
        }
        return output + "}";
    }

    std::string formatLua(const nlohmann::json::const_iterator& it) {
        if (it->is_object()) {
            return formatObjectAsLuaTable(it->get<nlohmann::json>());
        }
        if (it->is_array()) {
            return formatArrayAsLuaTable(it->get<nlohmann::json>());
        }
        if (it->is_number()) {
            return std::format("{}", it->get<double>());
        }
        if (it->is_string()) {
            return formatLuaString(it->get<std::string>());
        }
        if (it->is_boolean()) {
            return it->get<bool>() ? "true" : "false";
        }
        if (it->is_null()) {
            return "nil";
        }
        throw ghoul::lua::LuaFormatException("Format error");
    }

    std::string generateScript(const std::string& function,
                               const std::vector<std::string>& args)
    {
        std::string script = "return " + function + "(";
        auto it = args.begin();
        for (size_t i = 0; i < args.size(); i++, it++) {
            script += *it;
            if (i < args.size() - 1) {
                script += ",";
            }
        }
        return script + ")";
    }


} // namespace

namespace openspace {

void LuaScriptTopic::handleJson(const nlohmann::json& json) {
    try {
        const auto script = json.find(KeyScript);
        const auto function = json.find(KeyFunction);

        if (script != json.end() && script->is_string()) {
            std::string luaScript = script->get<std::string>();
            const auto ret = json.find(KeyReturn);
            const bool shouldReturn =
                (ret != json.end()) && ret->is_boolean() && ret->get<bool>();

            const auto sync = json.find(KeyShouldBeSynchronized);
            bool shouldBeSynchronized = true;
            if (sync != json.end() && sync->is_boolean()) {
                shouldBeSynchronized = sync->get<bool>();
            }

            runScript(std::move(luaScript), shouldReturn, shouldBeSynchronized);
        }
        else if (function != json.end() && function->is_string()) {
            const std::string luaFunction = function->get<std::string>();
            const auto ret = json.find(KeyReturn);
            const bool shouldReturn =
                (ret != json.end()) && ret->is_boolean() && ret->get<bool>();

            const auto sync = json.find(KeyShouldBeSynchronized);
            bool shouldBeSynchronized = true;
            if (sync != json.end() && sync->is_boolean()) {
                shouldBeSynchronized = sync->get<bool>();
            }

            const nlohmann::json::const_iterator args = json.find(KeyArguments);
            if (!args->is_array()) {
                return;
            }

            std::vector<std::string> formattedArgs;
            formattedArgs.reserve(args->size());
            for (auto it = args->begin(); it != args->end(); it++) {
                formattedArgs.push_back(formatLua(it));
            }

            std::string luaScript = generateScript(luaFunction, formattedArgs);
            runScript(std::move(luaScript), shouldReturn, shouldBeSynchronized);
        }
    }
    catch (const std::out_of_range& e) {
        LERROR("Could not run script -- key or value is missing in payload");
        LERROR(e.what());
    }
}

void LuaScriptTopic::runScript(std::string script, bool shouldReturn,
                               bool shouldBeSynchronized)
{
    scripting::ScriptEngine::ScriptCallback callback;
    if (shouldReturn) {
        callback = [this](const ghoul::Dictionary& data) {
            if (_connection) {
                const nlohmann::json payload = wrappedPayload(data);
                _connection->sendJson(payload);
                _waitingForReturnValue = false;
            }
        };
        _waitingForReturnValue = true;
    }
    else {
        _waitingForReturnValue = false;
    }

    global::scriptEngine->queueScript(
        std::move(script),
        scripting::ScriptEngine::ShouldBeSynchronized(shouldBeSynchronized),
        scripting::ScriptEngine::ShouldSendToRemote(shouldBeSynchronized),
        callback
    );
}

bool LuaScriptTopic::isDone() const {
    return !_waitingForReturnValue;
}

} // namespace openspace
