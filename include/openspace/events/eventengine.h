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
    using ScriptCallback = std::function<void(ghoul::Dictionary)>;

    struct ActionInfo {
        events::Event::Type type;
        uint32_t id = std::numeric_limits<uint32_t>::max();
        bool isEnabled = true;
        std::string action;
        std::optional<ghoul::Dictionary> filter;
    };

    struct TopicInfo {
        uint32_t id = std::numeric_limits<uint32_t>::max();
        ScriptCallback callback;
    };

    /**
     * This function returns the first event stored in the EventEngine, or `nullptr` if
     * no event exists. To navigate the full list of events, you can access the returned
     * Event's next function. If the end of the list is reached, the next pointer will be
     * a `nullptr`.
     *
     * \return The first event stored in the EventEngine or nullptr if no event exists
     */
    events::Event* firstEvent() const;

    /**
     * Publish a new event of type T by providing optional arguments Args to the Event's
     * constructor. An example of usage is
     * `engine.publishEvent<MyEvent>("a", 2.0);` which would call the constructor of
     * `MyEvent` with a `const char*` and `double` parameter.
     *
     * \tparam T The subclass of Event that is to be published
     * \param args The arguments that are passed to the constructor of T
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
     * \param identifier The identifier of the action that will be triggered the
     *        identifier must not exist at this moment, but must exist by the time the
     *        event is encountered next
     * \param filter If the filter is provided, it describes the event parameters that are
     *        checked and only if an event passes the filter, the corresponding action is
     *        triggered
     */
    void registerEventAction(events::Event::Type type, std::string identifier,
        std::optional<ghoul::Dictionary> filter = std::nullopt);

    /**
     * Registers a new topic for a specific event type.
     *
     * \param topicId The id of the topic that will be triggered
     * \param type The type for which a new topic is registered
     * \param callback The callback function that will be called on triggered event
    */
    void registerEventTopic(size_t topicId, events::Event::Type type,
        ScriptCallback callback);

    /**
     * Removing registration for a type/action combination.
     *
     * \param type The type of the action that should be unregistered
     * \param identifier The identifier of the action that should be unregistered
     * \param filter The optional filter applied to the event-action combination
     */
    void unregisterEventAction(events::Event::Type type,
        const std::string& identifier,
        const std::optional<ghoul::Dictionary>& filter = std::nullopt);

    /**
     * Removing registration for a specific event identified by the \p identifier.
     *
     * \param identifier The unique identifier of the event that should be removed
     */
    void unregisterEventAction(uint32_t identifier);

    /**
     * Removing registration for a topic/type combination, does nothing if topicId or type
     * combination does not exist
     *
     * \param topicId The id of the topic that should be unregistered
     * \param type The type of the topic that should be unregistered
    */
    void unregisterEventTopic(size_t topicId, events::Event::Type type);

    /**
     * Returns the list of all registered actions, sorted by their identifiers.
     *
     * \return The list of all registered actions
     */
    std::vector<ActionInfo> registeredActions() const;

    /**
     * Returns the list of all registered actions, grouped by their event type.
     *
     * \return The unordered map of all registered actions
     */
    const std::unordered_map<events::Event::Type, std::vector<ActionInfo>>&
        eventActions() const;

    /**
     * Enables the event identified by the \p identifier. If the event is already enabled,
     * this function does nothing.
     *
     * \param identifier The identifier of the event that should be enabled
     */
    void enableEvent(uint32_t identifier);

    /**
     * Disables the event identified by the \p identifier. If the event is already
     * disabled, this function does nothing.
     *
     * \param identifier The identifier of the event that should be disabled
     */
    void disableEvent(uint32_t identifier);

    /**
     * Triggers all actions that are registered for events that are in the current event
     * queue.
     */
    void triggerActions() const;

    /**
     * Triggers all topics that are registered for events that are in the current event
     * queue.
    */
    void triggerTopics() const;

    static scripting::LuaLibrary luaLibrary();

private:
    /// The storage space in which Events are stored
    ghoul::MemoryPool<4096> _memory;
    /// The first event in the chain of events stored in the memory pool
    events::Event* _firstEvent = nullptr;
    /// The last event in the chain of events stored in the memory pool
    events::Event* _lastEvent = nullptr;

    /// The type is duplicated in the ActionInfo as well, but we want it in the ActionInfo
    /// to be able to return them to a caller and we want it in this unordered_map to make
    /// the lookup really fast. So having this extra wasted memory is probably worth it
    std::unordered_map<events::Event::Type, std::vector<ActionInfo>> _eventActions;

    std::unordered_map<events::Event::Type, std::vector<TopicInfo>> _eventTopics;

    static uint32_t nextRegisteredEventId;

#ifdef _DEBUG
    /// Stores the total number of events during this frame for debugging purposes
    static uint64_t nEvents;
#endif // _DEBUG
};

} // namespace openspace

#include "eventengine.inl"

#endif // __OPENSPACE_CORE___EVENTENGINE___H__
