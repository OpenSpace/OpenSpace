/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/server/include/topics/actionkeybindtopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>
#include <openspace/engine/globals.h>
#include <openspace/interaction/actionmanager.h>
#include <openspace/interaction/keybindingmanager.h>
#include <ghoul/logging/logmanager.h>

using nlohmann::json;

namespace openspace {

bool ActionKeybindTopic::isDone() const {
    return true;
}

nlohmann::json jsonKeybind(const KeyWithModifier& k, std::string identifier) {
    const interaction::Action& action = global::actionManager->action(identifier);

    return {
        { "key", ghoul::to_string(k.key) },
        { "modifiers",
            {
                { "shift" , hasKeyModifier(k.modifier, KeyModifier::Shift) },
                { "control" , hasKeyModifier(k.modifier, KeyModifier::Control) },
                { "alt" , hasKeyModifier(k.modifier, KeyModifier::Alt) },
                { "super" , hasKeyModifier(k.modifier, KeyModifier::Super) }
            }
        },
        { "action", action.identifier },
    };
}

nlohmann::json ActionKeybindTopic::allActionsKeybinds() const {
    nlohmann::json json = {};
    std::vector<interaction::Action> actions = global::actionManager->actions();
    std::sort(
        actions.begin(),
        actions.end(),
        [](const interaction::Action& lhs, const interaction::Action& rhs) {
            if (!lhs.name.empty() && !rhs.name.empty()) {
                return lhs.name < rhs.name;
            }
            else {
                return lhs.identifier < rhs.identifier;
            }
        }
    );

    for (const interaction::Action& action : actions) {
        json["actions"].push_back(action);
    }

    const std::multimap<KeyWithModifier, std::string>& keyBindings =
        global::keybindingManager->keyBindings();

    for (const std::pair<const KeyWithModifier, std::string>& keyBinding : keyBindings) {
        if (!global::actionManager->hasAction(keyBinding.second)) {
            // We don't warn here as we don't know if the user didn't expect the action
            // to be there or not. They might have defined a keybind to do multiple things
            // only one of which is actually defined
            continue;
        }
        nlohmann::json keybindJson = jsonKeybind(keyBinding.first, keyBinding.second);
        json["keybinds"].push_back(keybindJson);
    }
    return json;
}

nlohmann::json ActionKeybindTopic::action(const std::string& identifier) const {
    std::vector<interaction::Action> actions = global::actionManager->actions();

    auto found = std::find_if(
        actions.begin(),
        actions.end(),
        [&identifier](const interaction::Action& action) {
            return action.identifier == identifier;
        }
    );

    if (found == actions.end()) {
        return {};
    }
    interaction::Action action = *found;

    return action;
}

void ActionKeybindTopic::sendData(nlohmann::json data) const {
    nlohmann::json payload = wrappedPayload({ data });
    _connection->sendJson(std::move(payload));
}

void ActionKeybindTopic::handleJson(const nlohmann::json& input) {
    const std::string& event = input.at("event").get<std::string>();
    if (event == "get_all") {
        sendData(allActionsKeybinds());
    }
    else if (event == "get_action") {
        const std::string& identifier = input.at("identifier").get<std::string>();
        sendData(action(identifier));
    }
}

} // namespace openspace
