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

#include <openspace/interaction/actionmanager.h>

#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <algorithm>

#include "actionmanager_lua.inl"

namespace {
    constexpr const char* _loggerCat = "ActionManager";
} // namespace

namespace openspace::interaction {

bool ActionManager::hasAction(const std::string& identifier) const {
    ghoul_assert(!identifier.empty(), "Identifier must not be empty");

    const unsigned int hash = ghoul::hashCRC32(identifier);
    const auto it = _actions.find(hash);
    return it != _actions.end();
}

void ActionManager::registerAction(Action action) {
    ghoul_assert(!action.identifier.empty(), "Action must have an identifier");
    ghoul_assert(!hasAction(action.identifier), "Identifier already existed");

    const unsigned int hash = ghoul::hashCRC32(action.identifier);
    _actions[hash] = std::move(action);
}

void ActionManager::removeAction(const std::string& identifier) {
    ghoul_assert(!identifier.empty(), "Identifier must not be empty");
    ghoul_assert(hasAction(identifier), "Action was not found in the list");

    const unsigned int hash = ghoul::hashCRC32(identifier);
    const auto it = _actions.find(hash);
    _actions.erase(it);
}

const Action& ActionManager::action(const std::string& identifier) const {
    ghoul_assert(!identifier.empty(), "Identifier must not be empty");
    ghoul_assert(hasAction(identifier), "Action was not found in the list");

    const unsigned int hash = ghoul::hashCRC32(identifier);
    const auto it = _actions.find(hash);
    return it->second;
}

std::vector<Action> ActionManager::actions() const {
    std::vector<Action> result;
    result.reserve(_actions.size());
    for (const std::pair<unsigned int, Action>& p : _actions) {
        result.push_back(p.second);
    }
    return result;
}

void ActionManager::triggerAction(const std::string& identifier,
                                  const ghoul::Dictionary& arguments) const
{
    ghoul_assert(!identifier.empty(), "Identifier must not be empty");

    if (!hasAction(identifier)) {
        LWARNINGC(
            "ActionManager",
            fmt::format("Action '{}' not found in the list", identifier)
        );
        return;
    }

    const Action& a = action(identifier);
    if (arguments.isEmpty()) {
        global::scriptEngine->queueScript(
            a.command,
            scripting::ScriptEngine::RemoteScripting(a.synchronization)
        );
    }
    else {
        global::scriptEngine->queueScript(
            fmt::format("args = {}\n{}", ghoul::formatLua(arguments), a.command),
            scripting::ScriptEngine::RemoteScripting(a.synchronization)
        );
    }
}

scripting::LuaLibrary ActionManager::luaLibrary() {
    return {
        "action",
        {
            {
                "hasAction",
                &luascriptfunctions::hasAction,
                {},
                "string",
                "Checks if the passed identifier corresponds to an action"
            },
            {
                "removeAction",
                &luascriptfunctions::removeAction,
                {},
                "string",
                "Removes an existing action from the list of possible actions"
            },
            {
                "registerAction",
                &luascriptfunctions::registerAction,
                {},
                "table",
                "Registers a new action. The table must at least contain the keys "
                "'Identifier' and 'Command' represeting the unique identifier and the "
                "Lua script that belong to this new action. Optional keys are 'Name' for "
                "a human-readable name, 'Documentation' for a description of what the "
                "action does, 'GuiPath' for a path used for grouping a user interface. "
                "All of these parameters must be strings. The last parameter is "
                "'IsLocal' and represents whether the action should be executed locally "
                "(= false) or remotely (= true, the default)"
            },
            {
                "action",
                &luascriptfunctions::action,
                {},
                "string",
                "Returns information about the action as a table with the keys "
                "'Identifier', 'Command', 'Name', 'Documentation', 'GuiPath', and "
                "'Synchronization'"
            },
            {
                "actions",
                &luascriptfunctions::actions,
                {},
                "",
                "Returns all registered actions in the system as a table of tables each "
                "containing the keys 'Identifier', 'Command', 'Name', 'Documentation', "
                "'GuiPath', and 'Synchronization'"
            },
            {
                "triggerAction",
                &luascriptfunctions::triggerAction,
                {},
                "string",
                "Triggers the action given by the specified identifier"
            }
        }
    };
}

} // namespace openspace::interaction
