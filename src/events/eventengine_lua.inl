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

#include <ghoul/lua/lua_helper.h>

namespace {

/**
 * Registers an action to be executed whenever an event is encountered. If the optional
 * third parameter is provided, it describes a filter that the event is being checked
 * against and only if it passes the filter, the action is triggered.
 */
[[codegen::luawrap]] void registerEventAction(std::string event, std::string action,
                                    std::optional<ghoul::Dictionary> filter)
{
    using namespace openspace;
    events::Event::Type type = events::fromString(event);
    global::eventEngine->registerEventAction(type, std::move(action), std::move(filter));
}

/**
 * Unregisters a specific combination of event, action, and potentially a filter.
 */
[[codegen::luawrap]] void unregisterEventAction(std::string event, std::string action,
                                                std::optional<ghoul::Dictionary> filter)
{
    using namespace openspace;
    events::Event::Type type = events::fromString(event);
    global::eventEngine->unregisterEventAction(type, action, filter);
}

/**
 * Returns the list of registered events.
 */
[[codegen::luawrap]] std::vector<ghoul::Dictionary> registeredEvents() {
    using namespace openspace;

    std::vector<EventEngine::ActionInfo> actions =
        global::eventEngine->registeredActions();

    std::vector<ghoul::Dictionary> result;
    result.reserve(actions.size());
    for (const EventEngine::ActionInfo& ai : actions) {
        ghoul::Dictionary d;
        d.setValue("Identifier", static_cast<int>(ai.id));
        d.setValue("Type", std::string(events::toString(ai.type)));
        d.setValue("Enabled", ai.isEnabled);
        d.setValue("Action", ai.action);
        if (ai.filter.has_value()) {
            d.setValue("Filter", *ai.filter);
        }
        result.push_back(d);

    }
    return result;
}

/**
 * Enables the event with the provided identifier.
 */
[[codegen::luawrap]] void enableEvent(int identifier) {
    openspace::global::eventEngine->enableEvent(static_cast<uint32_t>(identifier));
}

/**
 * Disables the event with the provided identifier.
 */
[[codegen::luawrap]] void disableEvent(int identifier) {
    openspace::global::eventEngine->disableEvent(static_cast<uint32_t>(identifier));
}

#include "eventengine_lua_codegen.cpp"

} // namespace
