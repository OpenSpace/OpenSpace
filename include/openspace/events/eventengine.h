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

#ifndef __OPENSPACE_CORE___EVENTENGINE___H__
#define __OPENSPACE_CORE___EVENTENGINE___H__

#include <openspace/events/event.h>
#include <openspace/scripting/lualibrary.h>
#include <ghoul/misc/memorypool.h>
#include <unordered_map>

namespace openspace {

namespace events { struct Event; }

class EventEngine {
public:
    /**
     * This function returns the first event stored in the EventEngine, or \c nullptr if
     * no event exists. To navigate the full list of events, you can access the returned
     * Event's next function. If the end of the list is reached, the next pointer will be
     * a nullptr
     *
     * \return The first event stored in the EventEngine or nullptr if no event exists
     */
    events::Event* firstEvent() const;

    /**
     * Publish a new event of type T by providing optional arguments Args to the Event's
     * constructor. An example of usage is
     * <code>engine.publishEvent<MyEvent>("a", 2.0);</code> which would call the
     * constructor of \c MyEvent with a <code>const char*</code> and \c double parameter.
     *
     * \param args The arguments that are passed to the constructor of T
     * \tparam T The subclass of Event that is to be published
     */
    template <typename T, typename... Args>
    void publishEvent(Args&&... args);

    /**
     * This function cleans up the memory for all published events.After this function
     * has been called, no previously published events are valid any longer. This means
     * that pointers retrieved from events before this call must be kept beyond this call.
     */
    void postFrameCleanup();

    /**
     * Registers a new action for a specific event type.
     *
     * \param type The type for which a new action is registered
     * \param actionIdentifier The identifier of the action that will be triggered the
     *        identifier must not exist at this moment, but must exist by the time the
     *        event is encountered next
     * \param filter If the filter is provided, it describes the event parameters that are
     *        checked and only if an event passes the filter, the corresponding action is
     *        triggered
     */
    void registerEventAction(events::Event::Type type, std::string identifier,
        std::optional<ghoul::Dictionary> filter = std::nullopt);

    /**
     * Removing registration for a type/action combination.
     *
     * \param type The type of the action that should be unregistered
     * \param actionIdentifier The identifier of the action that should be unregistered
     * \param filter The optional filter applied to the event-action combination
     */
    void unregisterEventAction(events::Event::Type type,
        const std::string& identifier,
        std::optional<ghoul::Dictionary> filter = std::nullopt);

    /**
     * Triggers all actions that are registered for events that are in the current event
     * queue
     */
    void triggerActions() const;

    static scripting::LuaLibrary luaLibrary();

private:
    /// The storage space in which Events are stored
    ghoul::MemoryPool<4096> _memory;
    /// The first event in the chain of events stored in the memory pool
    events::Event* _firstEvent = nullptr;
    /// The last event in the chain of events stored in the memory pool
    events::Event* _lastEvent = nullptr;

    struct ActionInfo {
        std::string action;
        std::optional<ghoul::Dictionary> filter;
    };
    std::unordered_map<events::Event::Type, std::vector<ActionInfo>> _eventActions;

#ifdef _DEBUG
    /// Stores the total number of events during this frame for debugging purposes
    static uint64_t nEvents;
#endif // _DEBUG
};

} // namespace openspace

#include "eventengine.inl"

#endif // __OPENSPACE_CORE___EVENTENGINE___H__
