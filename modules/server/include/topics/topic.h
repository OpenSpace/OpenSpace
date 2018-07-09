/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2018                                                               *
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

#ifndef __OPENSPACE_MODULE_SERVER___TOPIC___H__
#define __OPENSPACE_MODULE_SERVER___TOPIC___H__

#include <openspace/json.h>

namespace openspace {

class Connection;

class Topic {
public:
    Topic() {};
    virtual ~Topic() = default;

    void initialize(Connection* connection, size_t topicId);
    nlohmann::json wrappedPayload(const nlohmann::json& payload) const;
    nlohmann::json wrappedError(std::string message = "Could not complete request.",
        int code = 500);
    virtual void handleJson(const nlohmann::json& json) = 0;
    virtual bool isDone() const = 0;

protected:
    size_t _topicId;
    Connection* _connection;
};

} // namespace openspace

#endif // __OPENSPACE_MODULE_SERVER___TOPIC___H__
