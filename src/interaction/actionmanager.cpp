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

#include <openspace/interaction/actionmanager.h>

#include <openspace/engine/globals.h>
#include <openspace/scripting/lualibrary.h>
#include <openspace/scripting/scriptengine.h>
#include <ghoul/logging/logmanager.h>
#include <ghoul/misc/crc32.h>
#include <ghoul/misc/dictionaryluaformatter.h>
#include <algorithm>

#include "actionmanager_lua.inl"

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
    for (const std::pair<const unsigned int, Action>& p : _actions) {
        result.push_back(p.second);
    }
    return result;
}

void ActionManager::triggerAction(const std::string& identifier,
                                  const ghoul::Dictionary& arguments,
                           ActionManager::ShouldBeSynchronized shouldBeSynchronized) const
{
    ghoul_assert(!identifier.empty(), "Identifier must not be empty");

    if (!hasAction(identifier)) {
        LWARNINGC(
            "ActionManager",
            std::format("Action '{}' not found in the list", identifier)
        );
        return;
    }

    const Action& a = action(identifier);
    std::string script =
        arguments.isEmpty() ?
        a.command :
        std::format("args = {}\n{}", ghoul::formatLua(arguments), a.command);

    if (!shouldBeSynchronized || a.isLocal) {
        global::scriptEngine->queueScript(
            std::move(script),
            scripting::ScriptEngine::ShouldBeSynchronized::No,
            scripting::ScriptEngine::ShouldSendToRemote::No
        );
    }
    else {
        global::scriptEngine->queueScript(
            std::move(script),
            scripting::ScriptEngine::ShouldBeSynchronized::Yes,
            scripting::ScriptEngine::ShouldSendToRemote::Yes
        );
    }
}

scripting::LuaLibrary ActionManager::luaLibrary() {
    return {
        "action",
        {
            codegen::lua::HasAction,
            codegen::lua::RemoveAction,
            codegen::lua::RegisterAction,
            codegen::lua::Action,
            codegen::lua::Actions,
            codegen::lua::TriggerAction
        }
    };
}

} // namespace openspace::interaction
