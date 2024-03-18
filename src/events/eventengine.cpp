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

#include <openspace/events/eventengine.h>

#include <openspace/engine/globals.h>
#include <openspace/interaction/actionmanager.h>

#include "eventengine_lua.inl"

namespace {
    constexpr std::string_view _loggerCat = "EventEngine";
} // namespace

namespace openspace {

uint32_t EventEngine::nextRegisteredEventId = 0;

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
    ai.id = nextRegisteredEventId;
    ai.isEnabled = true;
    ai.type = type;
    ai.action = std::move(identifier);
    ai.filter = std::move(filter);
    const auto it = _eventActions.find(type);
    if (it != _eventActions.end()) {
        it->second.push_back(ai);
    }
    else {
        _eventActions[type] = { std::move(ai) };
    }

    nextRegisteredEventId++;
}

void EventEngine::registerEventTopic(size_t topicId, events::Event::Type type,
                                     ScriptCallback callback)
{
    TopicInfo ti;
    ti.id = topicId;
    ti.callback = std::move(callback);

    _eventTopics[type].push_back(ti);
}

void EventEngine::unregisterEventAction(events::Event::Type type,
                                        const std::string& identifier,
                                        const std::optional<ghoul::Dictionary>& filter)
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

void EventEngine::unregisterEventAction(uint32_t identifier) {
    for (auto it = _eventActions.begin(); it != _eventActions.end(); it++) {
        for (auto jt = it->second.begin(); jt != it->second.end(); jt++) {
            if (jt->id == identifier) {
                it->second.erase(jt);

                // This might have been the last action so we might need to remove the
                // entry alltogether
                if (it->second.empty()) {
                    _eventActions.erase(it);
                }

                // The identifier is unique so we can stop after this
                return;
            }
        }
    }

    // If we get this far, we haven't found the identifier
    throw ghoul::RuntimeError(fmt::format(
        "Could not find event with identifier '{}'", identifier
    ));
}

void EventEngine::unregisterEventTopic(size_t topicId, events::Event::Type type) {
    const auto it = _eventTopics.find(type);
    if (it != _eventTopics.end()) {
        const auto jt = std::find_if(
            it->second.begin(), it->second.end(),
            [topicId](const TopicInfo& ti) {
                return ti.id == topicId;
            }
        );
        if (jt != it->second.end()) {
            it->second.erase(jt);

            // This might have been the last action so we might need to remove the
            // entry alltogether
            if (it->second.empty()) {
                _eventTopics.erase(it);
            }
        }
        else {
            LWARNING(fmt::format("Could not find registered event '{}' with topicId: {}",
                events::toString(type), topicId)
            );
        }
    }
    else {
        LWARNING(fmt::format("Could not find registered event '{}'",
            events::toString(type))
        );
    }
}

std::vector<EventEngine::ActionInfo> EventEngine::registeredActions() const {
    std::vector<EventEngine::ActionInfo> result;
    result.reserve(_eventActions.size());
    using Type = events::Event::Type;
    for (const std::pair<const Type, std::vector<ActionInfo>>& p : _eventActions) {
        result.insert(result.end(), p.second.begin(), p.second.end());
    }
    return result;
}

void EventEngine::enableEvent(uint32_t identifier) {
    using Type = events::Event::Type;
    for (std::pair<const Type, std::vector<ActionInfo>>& p : _eventActions) {
        for (ActionInfo& ai : p.second) {
            if (ai.id == identifier) {
                ai.isEnabled = true;
                break;
            }
        }
    }
}

void EventEngine::disableEvent(uint32_t identifier) {
    using Type = events::Event::Type;
    for (std::pair<const Type, std::vector<ActionInfo>>& p : _eventActions) {
        for (ActionInfo& ai : p.second) {
            if (ai.id == identifier) {
                ai.isEnabled = false;
                break;
            }
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
            const ghoul::Dictionary params = toParameter(*e);
            for (const ActionInfo& ai : it->second) {
                if (ai.isEnabled &&
                    (!ai.filter.has_value() || params.isSubset(*ai.filter)))
                {
                    // No sync because events are always synced and sent to the connected
                    // nodes and peers
                    global::actionManager->triggerAction(
                        ai.action,
                        params,
                        interaction::ActionManager::ShouldBeSynchronized::No
                    );
                }
            }
        }

        e = e->next;
    }
}

void EventEngine::triggerTopics() const {
    if (_eventTopics.empty()) {
        // Nothing to do here
        return;
    }

    const events::Event* e = _firstEvent;
    while (e) {
        const auto it = _eventTopics.find(e->type);

        if (it != _eventTopics.end()) {
            const ghoul::Dictionary params = toParameter(*e);
            for (const TopicInfo& ti : it->second) {
                ti.callback(params);
            }
        }

        e = e->next;
    }
}

scripting::LuaLibrary EventEngine::luaLibrary() {
    return {
        "event",
        {
            codegen::lua::RegisterEventAction,
            codegen::lua::UnregisterEventAction,
            codegen::lua::RegisteredEvents,
            codegen::lua::EnableEvent,
            codegen::lua::DisableEvent
        }
    };
}

} // namespace openspace
