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

#include <utility>

namespace ghoul {

template <class... T>
void Event<T...>::subscribe(std::string name, std::string topic, Callback callback) {
    auto it = _topics.find(topic);
    if (it == _topics.end()) {
        _topics.insert({ std::move(topic), { { std::move(name), std::move(callback) } }});
    }
    else {
        it->second.push_back({ std::move(name), std::move(callback) });
    }
}

template <class... T>
void Event<T...>::publish(const std::string& topic, T... message) {
    // If the topic exists...
    if (_topics.find(topic) != _topics.end()) {
        // ...go through all subscribers and send them the message
        for (Subscriber& subscriber : _topics[topic]) {
            subscriber.callback(message...);
        }
    }
}

template <class... T>
void Event<T...>::unsubscribe(const std::string& name, const std::string& topic) {
    if (_topics.find(topic) != _topics.end()) {
        // Search through the whole array of subscribers to given topic and remove all
        // callbacks that the corresponds to the subscribers name
        _topics[topic].erase(
            std::remove_if(
                _topics[topic].begin(),
                _topics[topic].end(),
                [name](const Subscriber& subscriber) { return (subscriber.name == name); }
            ),
            _topics[topic].end()
        );
    }
}

template <class... T>
void Event<T...>::unsubscribe(const std::string& name) {
    // Search through all topics to erase all callbacks that belong to the object with
    // given name
    for (std::pair<const std::string, std::vector<Subscriber>>& topic : _topics) {
        topic.second.erase(
            std::remove_if(
                topic.second.begin(), topic.second.end(),
                [name](const Subscriber& subscriber) { return (subscriber.name == name); }
            ),
            topic.second.end()
        );
    }
}

} // namespace ghoul
