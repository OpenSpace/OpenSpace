/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#ifndef OPENSPACE_MODULES_SERVER__TOPIC_H
#define OPENSPACE_MODULES_SERVER__TOPIC_H

#include <ext/json/json.hpp>

namespace openspace {

class Connection;

class Topic {
public:
    Topic() {};
    virtual ~Topic() {};
    void initialize(Connection* connection, size_t topicId);
    nlohmann::json wrappedPayload(const nlohmann::json &payload) const;
    nlohmann::json wrappedError(std::string message = "Could not complete request.", int code = 500);
    virtual void handleJson(nlohmann::json json) = 0;
    virtual bool isDone() = 0;

protected:
    size_t _topicId;
    Connection* _connection;
};

class SetPropertyTopic : public Topic {
public:
    SetPropertyTopic() : Topic() {};
    ~SetPropertyTopic() {};
    void handleJson(nlohmann::json json) {};
    bool isDone() { return false; }
};

class BounceTopic : public Topic {
public:
    BounceTopic() : Topic() {};
    ~BounceTopic() {};
    void handleJson(nlohmann::json json);
    bool isDone() { return false; }
};

}

#endif //OPENSPACE_MODULES_SERVER__TOPIC_H
