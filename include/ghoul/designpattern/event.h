/*****************************************************************************************
 *                                                                                       *
 * GHOUL                                                                                 *
 * General Helpful Open Utility Library                                                  *
 *                                                                                       *
 * Copyright (c) 2012-2026                                                               *
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

#ifndef __GHOUL___EVENT___H__
#define __GHOUL___EVENT___H__

#include <functional>
#include <string>
#include <unordered_map>
#include <vector>

namespace ghoul {

template <typename... T>
class Event {
public:
    using Callback = std::function<void(const T&...)>;

    /**
     * Adds a listener callback to the specified topic. When an event is published with
     * this topic, the callback is called. If this topic does not already exist, it
     * creates a new key for it in the map of topics.
     *
     * \param name The unique name of the subscriber
     * \param topic The event topic to subscribe to
     * \param listener Function that should be called when the event is published
     */
    void subscribe(std::string name, std::string topic, Callback listener);

    /**
     * Given a topic and a message, all subscribers callback functions of this event topic
     * will be called with message as an argument.
     *
     * \param topic The event topic to publish to
     * \param message The message to be used as argument for subscriber callbacks
     */
    void publish(const std::string& topic, T... message);

    /**
     * Unsubscribes the object with given name from a specific topic.
     *
     * \param name The subscriber's unique name
     * \param topic The event to unsubscribe to
     */
    void unsubscribe(const std::string& name, const std::string& topic);

    /**
     * Unsubscribes the object with given name from all topics.
     *
     * \param name The subscriber's unique name
     */
    void unsubscribe(const std::string& name);

private:
    struct Subscriber {
        std::string name;
        Callback callback;
    };

    /// Maps event topics to subscriber callbacks
    std::unordered_map<std::string, std::vector<Subscriber>> _topics;
};

} // namespace ghoul

#include "event.inl"

#endif // __GHOUL___EVENT___H__

