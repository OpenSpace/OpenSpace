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

#include <openspace/documentation/documentation.h>

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
 * Removes an existing action from the list of possible actions. The action is identifies
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
            std::format("Identifier '{}' for action not found", identifier)
        );
    }

    global::actionManager->removeAction(identifier);
}

struct [[codegen::Dictionary(Action)]] Action {
    // The identifier under which the action is registered
    std::string identifier;

    // The Lua script that is to be executed when the action is triggered
    std::string command;

    // The user-facing name of the action
    std::optional<std::string> name;

    // A documentation that explains what the action does
    std::optional<std::string> documentation;

    // The path in the GUI under which the action is shown to the user. If the value is
    // not provided, the default value is /
    std::optional<std::string> guiPath;

    // Determines whether the provided command will be executed locally or will be sent to
    // connected computers in a cluster or parallel connection environment
    std::optional<bool> isLocal;
};
/**
 * Registers a new action. The first argument is the identifier which cannot have been
 * used to register a previous action before, the second argument is the Lua command that
 * is to be executed, and the optional third argument is the name used in a user-interface
 * to refer to this action. The fourth is a human readable description of the command for
 * documentation purposes. The fifth is the GUI path and the last parameter determines
 * whether the action should be executed locally (= false) or remotely (= true, the
 * default).
 */
[[codegen::luawrap]] void registerAction(Action action) {
    using namespace openspace;

    if (global::actionManager->hasAction(action.identifier)) {
        throw ghoul::lua::LuaError(std::format(
            "Identifier '{}' for action already registered", action.identifier
        ));
    }

    interaction::Action a;
    a.identifier = std::move(action.identifier);
    a.command = std::move(action.command);
    a.name = action.name.value_or(a.name);
    a.documentation = action.documentation.value_or(a.documentation);
    a.guiPath = action.guiPath.value_or(a.guiPath);
    if (!a.guiPath.starts_with('/')) {
        throw ghoul::RuntimeError(
            std::format(
                "Tried to register action: '{}'. The field 'GuiPath' is set to '{}' but "
                "should be '/{}' ", a.name, a.guiPath, a.guiPath)
        );
    }
    if (action.isLocal.has_value()) {
        a.isLocal = interaction::Action::IsLocal(*action.isLocal);
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
            std::format("Identifier '{}' for action not found", identifier)
        );
    }

    const interaction::Action& action = global::actionManager->action(identifier);
    ghoul::Dictionary res;
    res.setValue("Identifier", action.identifier);
    res.setValue("Command", action.command);
    res.setValue("Name", action.name);
    res.setValue("Documentation", action.documentation);
    res.setValue("GuiPath", action.guiPath);
    res.setValue("IsLocal", action.isLocal == interaction::Action::IsLocal::Yes);
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
    for (const interaction::Action& a : actions) {
        ghoul::Dictionary d;
        d.setValue("Identifier", a.identifier);
        d.setValue("Command", a.command);
        d.setValue("Name", a.name);
        d.setValue("Documentation", a.documentation);
        d.setValue("GuiPath", a.guiPath);
        d.setValue("IsLocal", a.isLocal == interaction::Action::IsLocal::Yes);
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
        throw ghoul::lua::LuaError(std::format("Action '{}' not found", id));
    }

    // No sync because this is already inside a Lua script, therefor it has
    // already been synced and sent to the connected nodes and peers
    global::actionManager->triggerAction(
        id,
        arg,
        interaction::ActionManager::ShouldBeSynchronized::No
    );
}

#include "actionmanager_lua_codegen.cpp"

} // namespace
