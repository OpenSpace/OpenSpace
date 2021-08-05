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
#include <algorithm>

#include "actionmanager_lua.inl"

namespace openspace::interaction {

ActionManager::ActionManager() {

}

bool ActionManager::hasAction(const std::string& identifier) const {
    ghoul_assert(!identifier.empty(), "Identifier must not be empty");

    const auto it = std::lower_bound(
        _actions.begin(), _actions.end(),
        identifier,
        [](const Action& action, const std::string& identifier) {
            return action.identifier < identifier;
        }
    );

    return it != _actions.end();
}

void ActionManager::registerAction(Action action) {
    ghoul_assert(!action.identifier.empty(), "Action must have an identifier");
    ghoul_assert(!hasAction(action.identifier), "Identifier already existed");

    _actions.push_back(std::move(action));
    std::sort(
        _actions.begin(), _actions.end(),
        [](const Action& lhs, const Action& rhs) {
            return lhs.identifier < rhs.identifier;
        }
    );
}

void ActionManager::removeAction(const std::string& identifier) {
    ghoul_assert(!identifier.empty(), "Identifier must not be empty");
    ghoul_assert(hasAction(identifier), "Action was not found in the list");

    const auto it = std::lower_bound(
        _actions.begin(), _actions.end(),
        identifier,
        [](const Action& action, const std::string& identifier) {
            return action.identifier < identifier;
        }
    );
    _actions.erase(it);
}

const Action& ActionManager::action(const std::string& identifier) const {
    ghoul_assert(!identifier.empty(), "Identifier must not be empty");
    ghoul_assert(hasAction(identifier), "Action was not found in the list");

    const auto it = std::lower_bound(
        _actions.begin(), _actions.end(),
        identifier,
        [](const Action& action, const std::string& identifier) {
            return action.identifier < identifier;
        }
    );

    return *it;
}

const std::vector<Action>& ActionManager::actions() const {
    return _actions;
}

void ActionManager::triggerAction(const std::string& identifier) const {
    ghoul_assert(!identifier.empty(), "Identifier must not be empty");
    ghoul_assert(hasAction(identifier), "Action was not found in the list");

    const Action& a = action(identifier);
    global::scriptEngine->queueScript(
        a.command,
        scripting::ScriptEngine::RemoteScripting(a.synchronization)
    );
}


scripting::LuaLibrary ActionManager::luaLibrary() {
    return {
        "actions",
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
                "string, string [, string, string, string, boolean]",
                "Registers a new action. The first argument is the identifier which "
                "cannot have been used to register a previous action before, the second "
                "argument is the Lua command that is to be executed, and the optional "
                "third argument is the name used in a user-interface to refer to this "
                "action. The fourth is a human readable description of the command for "
                "documentation purposes. The fifth is the GUI path and the last "
                "parameter determines whether the action should be executed locally "
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
