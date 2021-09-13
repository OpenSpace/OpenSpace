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

#include <openspace/events/eventengine.h>

#include <openspace/engine/globals.h>
#include <openspace/interaction/actionmanager.h>

#include "eventengine_lua.inl"

namespace openspace {

#ifdef _DEBUG
uint64_t EventEngine::nEvents = 0;
#endif // _DEBUG

events::Event* EventEngine::firstEvent() const {
    return _firstEvent;
}

void EventEngine::postFrameCleanup() {
    _memory.reset();
    _firstEvent = nullptr;
    _lastEvent = nullptr;
#ifdef _DEBUG
    nEvents = 0;
#endif // _DEBUG
}

void EventEngine::registerEventAction(events::Event::Type type,
                                      std::string identifier,
                                      std::optional<ghoul::Dictionary> filter)
{
    ActionInfo ai;
    ai.action = std::move(identifier);
    ai.filter = std::move(filter);
    const auto it = _eventActions.find(type);
    if (it != _eventActions.end()) {
        it->second.push_back(ai);
    }
    else {
        _eventActions[type] = { std::move(ai) };
    }
}

void EventEngine::unregisterEventAction(events::Event::Type type,
                                        const std::string& identifier,
                                        std::optional<ghoul::Dictionary> filter)
{
    const auto it = _eventActions.find(type);
    if (it != _eventActions.end()) {
        const auto jt = std::find_if(
            it->second.begin(), it->second.end(),
            [identifier, filter](const ActionInfo& ai) {
                const bool a = ai.action == identifier;
                const bool f = !filter.has_value() || *filter == ai.filter;
                return a && f;
            }
        );
        if (jt != it->second.end()) {
            it->second.erase(jt);
        }
    }
}

void EventEngine::triggerActions() const {
    if (_eventActions.empty()) {
        // Nothing to do here
        return;
    }

    const events::Event* e = _firstEvent;
    while (e) {
        const auto it = _eventActions.find(e->type);
        if (it != _eventActions.end()) {
            ghoul::Dictionary params = toParameter(*e);
            for (const ActionInfo& ai : it->second) {
                if (!ai.filter.has_value() || params.isSubset(*ai.filter)) {
                    global::actionManager->triggerAction(ai.action, params);
                }
            }
        }

        e = e->next;
    }
}

scripting::LuaLibrary EventEngine::luaLibrary() {
    scripting::LuaLibrary res;
    res.name = "event";
    res.functions.push_back({
        "registerEventAction",
        &luascriptfunctions::registerEventAction,
        {},
        "string, string [, table]",
        "Registers an action (second parameter) to be executed whenever an event (first "
        "parameter) is encountered. If the optional third parameter is provided, it "
        "describes a filter that the event is being checked against and only if it "
        "passes the filter, the action is triggered"
    });
    res.functions.push_back({
        "unregisterEventAction",
        &luascriptfunctions::unregisterEventAction,
        {},
        "string, string [, table]",
        "Unregisters a specific combination of event (first parameter), action (second "
        "parameter), and potentially a filter (optional third argument)"
    });
    return res;
}

} // namespace openspace
