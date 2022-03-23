/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2022                                                               *
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

namespace {

// Checks if the passed identifier corresponds to an action.
[[codegen::luawrap]] bool hasAction(std::string identifier) {
    if (identifier.empty()) {
        throw ghoul::lua::LuaError("Identifier must not be empty");
    }
    const bool res = openspace::global::actionManager->hasAction(identifier);
    return res;
}

/**
 * Removes an existing action from the list of possible actions.The action is identifies
 * either by the passed name, or if it is a table, the value behind the 'Identifier' key
 * is extract and used instead.
 */
[[codegen::luawrap]] void removeAction(
                                      std::variant<std::string, ghoul::Dictionary> action)
{
    using namespace openspace;

    std::string identifier;
    if (std::holds_alternative<std::string>(action)) {
        identifier = std::get<std::string>(action);
    }
    else {
        ghoul::Dictionary d = std::get<ghoul::Dictionary>(action);
        if (!d.hasValue<std::string>("Identifier")) {
            throw ghoul::lua::LuaError(
                "Table passed to removeAction does not contain an Identifier"
            );
        }
        identifier = d.value<std::string>("Identifier");
    }

    if (identifier.empty()) {
        throw ghoul::lua::LuaError("Identifier must not be empty");
    }
    if (!global::actionManager->hasAction(identifier)) {
        throw ghoul::lua::LuaError(
            fmt::format("Identifier '{}' for action not found", identifier)
        );
    }

    global::actionManager->removeAction(identifier);
}

/**
 * Registers a new action. The first argument is the identifier which cannot have been
 * used to register a previous action before, the second argument is the Lua command that
 * is to be executed, and the optional third argument is the name used in a user-interface
 * to refer to this action. The fourth is a human readable description of the command for
 * documentation purposes. The fifth is the GUI path and the last parameter determines
 * whether the action should be executed locally (= false) or remotely (= true, the
 * default).
 */
[[codegen::luawrap]] void registerAction(ghoul::Dictionary action) {
    using namespace openspace;

    if (!action.hasValue<std::string>("Identifier")) {
        throw ghoul::lua::LuaError("Identifier must to provided to register action");
    }
    std::string identifier = action.value<std::string>("Identifier");
    if (global::actionManager->hasAction(identifier)) {
        throw ghoul::lua::LuaError(
            fmt::format("Action for identifier '{}' already existed", identifier)
        );
    }
    if (global::actionManager->hasAction(identifier)) {
        throw ghoul::lua::LuaError(
            fmt::format("Identifier '{}' for action already registered", identifier)
        );
    }

    if (!action.hasValue<std::string>("Command")) {
        throw ghoul::lua::LuaError(
            fmt::format(
                "Identifier '{}' does not provide a Lua command to execute", identifier
            )
        );
    }

    interaction::Action a;
    a.identifier = std::move(identifier);
    a.command = action.value<std::string>("Command");
    if (action.hasValue<std::string>("Name")) {
        a.name = action.value<std::string>("Name");
    }
    if (action.hasValue<std::string>("Documentation")) {
        a.documentation = action.value<std::string>("Documentation");
    }
    if (action.hasValue<std::string>("GuiPath")) {
        a.guiPath = action.value<std::string>("GuiPath");
    }
    if (action.hasValue<bool>("IsLocal")) {
        bool value = action.value<bool>("IsLocal");
        a.synchronization = interaction::Action::IsSynchronized(value);
    }
    global::actionManager->registerAction(std::move(a));
}

/**
 * Returns information about the action as a table with the keys 'Identifier', 'Command',
 * 'Name', 'Documentation', 'GuiPath', and 'Synchronization'.
 */
[[codegen::luawrap]] ghoul::Dictionary action(std::string identifier) {
    using namespace openspace;

    if (identifier.empty()) {
        throw ghoul::lua::LuaError("Identifier must not be empty");
    }
    if (!global::actionManager->hasAction(identifier)) {
        throw ghoul::lua::LuaError(
            fmt::format("Identifier '{}' for action not found", identifier)
        );
    }

    const interaction::Action& action = global::actionManager->action(identifier);
    ghoul::Dictionary res;
    res.setValue("Identifier", action.identifier);
    res.setValue("Command", action.command);
    res.setValue("Name", action.name);
    res.setValue("Documentation", action.documentation);
    res.setValue("GuiPath", action.guiPath);
    res.setValue(
        "Synchronization",
        action.synchronization == interaction::Action::IsSynchronized::Yes
    );
    return res;
}

/**
 * Returns all registered actions in the system as a table of tables each containing the
 * keys 'Identifier', 'Command', 'Name', 'Documentation', 'GuiPath', and
 * 'Synchronization'.
 */
[[codegen::luawrap]] std::vector<ghoul::Dictionary> actions() {
    using namespace openspace;

    std::vector<ghoul::Dictionary> res;
    const std::vector<interaction::Action>& actions = global::actionManager->actions();
    for (const interaction::Action& action : actions) {
        ghoul::Dictionary d;
        d.setValue("Identifier", action.identifier);
        d.setValue("Command", action.command);
        d.setValue("Name", action.name);
        d.setValue("Documentation", action.documentation);
        d.setValue("GuiPath", action.guiPath);
        d.setValue(
            "Synchronization",
            action.synchronization == interaction::Action::IsSynchronized::Yes
        );

        res.push_back(d);
    }
    return res;
}

// Triggers the action given by the specified identifier.
[[codegen::luawrap]] void triggerAction(std::string id,
                                        ghoul::Dictionary arg = ghoul::Dictionary())
{
    using namespace openspace;

    if (id.empty()) {
        throw ghoul::lua::LuaError("Identifier must not be empty");
    }
    if (!global::actionManager->hasAction(id)) {
        throw ghoul::lua::LuaError(fmt::format("Action '{}' not found", id));
    }

    global::actionManager->triggerAction(id, arg);
}

#include "actionmanager_lua_codegen.cpp"

} // namespace
