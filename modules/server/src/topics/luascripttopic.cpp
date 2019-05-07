/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2019                                                               *
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
    constexpr const char* KeyScript = "script";
    constexpr const char* KeyFunction = "function";
    constexpr const char* KeyArguments = "arguments";
    constexpr const char* KeyReturn = "return";
    constexpr const char* _loggerCat = "LuaScriptTopic";

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

    std::string formatLuaTable(const nlohmann::json& json) {
        std::string output = "{";
        auto it = json.begin();
        for (size_t i = 0; i < json.size(); ++i, ++it) {
            output += formatKeyValuePair(it);
            if (i < json.size() - 1) {
                output += ",";
            }
        }
        return output + "}";
    }

    std::string formatLua(const nlohmann::json::const_iterator& it) {
        if (it->is_object()) {
            return formatLuaTable(it->get<nlohmann::json>());
        }
        if (it->is_number()) {
            return fmt::format("{:E}", it->get<double>());
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
        throw ghoul::lua::LuaFormatException("Format error.");
    }

    std::string generateScript(const std::string& function,
                               const std::vector<std::string>& args)
    {
        std::string script = "return " + function + "(";
        auto it = args.begin();
        for (size_t i = 0; i < args.size(); ++i, ++it) {
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
        nlohmann::json::const_iterator script = json.find(KeyScript);
        nlohmann::json::const_iterator function = json.find(KeyFunction);

        if (script != json.end() && script->is_string()) {
            std::string luaScript = script->get<std::string>();
            nlohmann::json::const_iterator ret = json.find(KeyReturn);
            bool shouldReturn = (ret != json.end()) &&
                                 ret->is_boolean() &&
                                 ret->get<bool>();

            runScript(luaScript, shouldReturn);
        }
        else if (function != json.end() && function->is_string()) {
            std::string luaFunction = function->get<std::string>();
            nlohmann::json::const_iterator ret = json.find(KeyReturn);
            bool shouldReturn = (ret != json.end()) &&
                                 ret->is_boolean() &&
                                 ret->get<bool>();

            nlohmann::json::const_iterator args = json.find(KeyArguments);
            if (!args->is_array()) {
                return;
            }

            std::vector<std::string> formattedArgs;
            formattedArgs.reserve(args->size());
            for (auto it = args->begin(); it != args->end(); ++it) {
                formattedArgs.push_back(formatLua(it));
            }

            std::string luaScript = generateScript(luaFunction, formattedArgs);
            runScript(luaScript, shouldReturn);
        }
    }
    catch (const std::out_of_range& e) {
        LERROR("Could not run script -- key or value is missing in payload");
        LERROR(e.what());
    }
}

void LuaScriptTopic::runScript(const std::string& script, bool shouldReturn) {
    scripting::ScriptEngine::ScriptCallback callback;
    if (shouldReturn) {
        callback = [this](ghoul::Dictionary data) {
            nlohmann::json j = data;
            _connection->sendJson(wrappedPayload(j));
            _waitingForReturnValue = false;
        };
        _waitingForReturnValue = true;
    }
    else {
        _waitingForReturnValue = false;
    }

    global::scriptEngine.queueScript(
        std::move(script),
        scripting::ScriptEngine::RemoteScripting::No,
        callback
    );
}

bool LuaScriptTopic::isDone() const {
    return !_waitingForReturnValue;
}

} // namespace openspace
