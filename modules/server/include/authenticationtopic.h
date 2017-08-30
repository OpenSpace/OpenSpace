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

#ifndef OPENSPACE_MODULES_SERVER__AUTHENTICATION_TOPIC_H
#define OPENSPACE_MODULES_SERVER__AUTHENTICATION_TOPIC_H

#include <ctime>
#include <ext/json/json.hpp>
#include <ghoul/logging/logmanager.h>
#include <fmt/format.h>

#include "topic.h"
#include "connection.h"

namespace openspace {

class AuthorizationTopic : public Topic {
public:
    AuthorizationTopic();
    ~AuthorizationTopic() {};
    void handleJson(nlohmann::json json);
    bool isDone();

    /* https://httpstatuses.com/ */
    static const struct Statuses {
        enum StatusCode {
            OK            = 200,
            Accepted      = 202,

            BadRequest    = 400,
            Unauthorized  = 401,
            NotAcceptable = 406,

            NotImplemented = 501
        };
    };
private:
    bool _isAuthenticated;

    const std::string getKey() const;
    bool authorize(const std::string key);
    nlohmann::json message(const std::string &message, const int &statusCode = Statuses::NotImplemented);
};

}

#endif //OPENSPACE_MODULES_SERVER__AUTHENTICATION_TOPIC_H
