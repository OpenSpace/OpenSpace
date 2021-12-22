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

#include <openspace/engine/globals.h>

namespace openspace::luascriptfunctions {

/**
 * \ingroup LuaScripts
 * hasAction():
 * Checks if the passed identifier corresponds to an action.
 */
int hasAction(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::hasAction");
    const std::string identifier = ghoul::lua::value<std::string>(L);

    if (identifier.empty()) {
        return ghoul::lua::luaError(L, "Identifier must not be empty");
    }

    const bool res = global::actionManager->hasAction(identifier);
    ghoul::lua::push(L, res);
    return 1;
}

/**
 * \ingroup LuaScripts
 * removeAction():
 * Removes an existing action from the list of possible actions.
*/
int removeAction(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::removeAction");
    const std::string identifier = ghoul::lua::value<std::string>(L);

    if (identifier.empty()) {
        return ghoul::lua::luaError(L, "Identifier must not be empty");
    }
    if (!global::actionManager->hasAction(identifier)) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Identifier '{}' for action not found", identifier)
        );
    }

    global::actionManager->removeAction(identifier);
    return 0;
}

/**
 * \ingroup LuaScripts
 * registerAction():
 * Registers a new action. The first argument is the identifier which cannot have been
 * used to register a previous action before, the second argument is the Lua command that
 * is to be executed, and the optional third argument is the name used in a user-interface
 * to refer to this action. The fourth is a human readable description of the command for
 * documentation purposes. The fifth is the GUI path and the last parameter determines
 * whether the action should be executed locally (= false) or remotely (= true, the
 * default).
 */
int registerAction(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::registerAction");
    const ghoul::Dictionary d = ghoul::lua::value<ghoul::Dictionary>(L);

    if (!d.hasValue<std::string>("Identifier")) {
        return ghoul::lua::luaError(L, "Identifier must to provided to register action");
    }
    std::string identifier = d.value<std::string>("Identifier");
    if (global::actionManager->hasAction(identifier)) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Action for identifier '{}' already existed", identifier)
        );
    }
    if (global::actionManager->hasAction(identifier)) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Identifier '{}' for action already registered", identifier)
        );
    }

    if (!d.hasValue<std::string>("Command")) {
        return ghoul::lua::luaError(
            L,
            fmt::format(
                "Identifier '{}' does not provide a Lua command to execute", identifier
            )
        );
    }

    interaction::Action action;
    action.identifier = std::move(identifier);
    action.command = d.value<std::string>("Command");
    if (d.hasValue<std::string>("Name")) {
        action.name = d.value<std::string>("Name");
    }
    if (d.hasValue<std::string>("Documentation")) {
        action.documentation = d.value<std::string>("Documentation");
    }
    if (d.hasValue<std::string>("GuiPath")) {
        action.guiPath = d.value<std::string>("GuiPath");
    }
    if (d.hasValue<bool>("IsLocal")) {
        bool value = d.value<bool>("IsLocal");
        action.synchronization = interaction::Action::IsSynchronized(value);
    }
    global::actionManager->registerAction(std::move(action));
    return 0;
}

/**
 * \ingroup LuaScripts
 * action():
 * Returns information about the action with the identifier equal to the provided
 * identifier.
 */
int action(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 1, "lua::action");
    const std::string identifier = ghoul::lua::value<std::string>(L);

    if (identifier.empty()) {
        return ghoul::lua::luaError(L, "Identifier must not be empty");
    }
    if (!global::actionManager->hasAction(identifier)) {
        return ghoul::lua::luaError(
            L,
            fmt::format("Identifier '{}' for action not found", identifier)
        );
    }

    const interaction::Action& action = global::actionManager->action(identifier);
    lua_newtable(L);
    ghoul::lua::push(L, "Identifier", action.identifier);
    lua_settable(L, -3);
    ghoul::lua::push(L, "Command", action.command);
    lua_settable(L, -3);
    ghoul::lua::push(L, "Name", action.name);
    lua_settable(L, -3);
    ghoul::lua::push(L, "Documentation", action.documentation);
    lua_settable(L, -3);
    ghoul::lua::push(L, "GuiPath", action.guiPath);
    lua_settable(L, -3);
    ghoul::lua::push(
        L,
        "Synchronization",
        action.synchronization == interaction::Action::IsSynchronized::Yes
    );
    lua_settable(L, -3);
    return 1;
}

/**
 * \ingroup LuaScripts
 * actions():
 * Returns all registered actions in the system.
 */
int actions(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, 0, "lua::actions");

    lua_newtable(L);
    const std::vector<interaction::Action>& actions = global::actionManager->actions();
    for (size_t i = 0; i < actions.size(); ++i) {
        const interaction::Action& action = actions[i];

        ghoul::lua::push(L, i + 1);
        lua_newtable(L);
        ghoul::lua::push(L, "Identifier", action.identifier);
        lua_settable(L, -3);
        ghoul::lua::push(L, "Command", action.command);
        lua_settable(L, -3);
        ghoul::lua::push(L, "Name", action.name);
        lua_settable(L, -3);
        ghoul::lua::push(L, "Documentation", action.documentation);
        lua_settable(L, -3);
        ghoul::lua::push(L, "GuiPath", action.guiPath);
        lua_settable(L, -3);
        ghoul::lua::push(
            L,
            "Synchronization",
            action.synchronization == interaction::Action::IsSynchronized::Yes
        );
        lua_settable(L, -3);

        lua_settable(L, -3);
    }

    return 1;
}

/**
 * \ingroup LuaScripts
 * triggerAction():
 * Triggers the action given by the specified identifier.
 */
int triggerAction(lua_State* L) {
    ghoul::lua::checkArgumentsAndThrow(L, { 1, 2 }, "lua::triggerAction");
    auto [id, arg] = ghoul::lua::values<std::string, std::optional<ghoul::Dictionary>>(L);
    arg = arg.value_or(ghoul::Dictionary());

    if (id.empty()) {
        return ghoul::lua::luaError(L, "Identifier must not be empty");
    }
    if (!global::actionManager->hasAction(id)) {
        return ghoul::lua::luaError(L, fmt::format("Action '{}' not found", id));
    }

    global::actionManager->triggerAction(id, *arg);
    return 0;
}

} // namespace openspace::luascriptfunctions
