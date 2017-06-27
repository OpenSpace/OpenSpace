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

#include "include/authorizationtopic.h"

namespace {
std::string _loggerCat = "AuthorizationTopic";
}

namespace openspace {

AuthorizationTopic::AuthorizationTopic()
        : Topic()
        , _key(randomNumber())
        , _isAuthorized(false) {
    LINFO(fmt::format("Ready to authorize socket Client using key: {}", _key));
};

bool AuthorizationTopic::isDone() {
    return _key > 0 && _isAuthorized;
}

void AuthorizationTopic::handleJson(nlohmann::json json) {
    if (isDone()) {
        _connection->sendJson(message("Already authorized.", Statuses::OK));
    } else {
        try {
            int providedKey = json.at("key").get<int>();
            if (authorize(providedKey)) {
                _connection->sendJson(message("Authorization OK.", Statuses::Accepted));
                LINFO("Client successfully authorized.");
            } else {
                _connection->sendJson(message("Invalid key", Statuses::NotAcceptable));
            }
        } catch (const std::out_of_range& e) {
            _connection->sendJson(message("Invalid request, key must be provided.", Statuses::BadRequest));
        } catch (const std::domain_error& e) {
            _connection->sendJson(message("Invalid request, invalid key format.", Statuses::BadRequest));
        }
    }
};

bool AuthorizationTopic::authorize(const int key) {
    _isAuthorized = key == _key;
    return _isAuthorized;
}

int AuthorizationTopic::randomNumber(int min, int max) {
    assert(max > min);
    std::srand(std::time(0));
    return std::rand() % (max - min) + min;
}

nlohmann::json AuthorizationTopic::message(const std::string &message, const int &statusCode) {
    nlohmann::json error = {{"error", message}, {"code", statusCode}};
    return error;
}

}