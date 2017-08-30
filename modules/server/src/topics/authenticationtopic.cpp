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

#include "include/authenticationtopic.h"

namespace {
std::string _loggerCat = "AuthorizationTopic";
}

namespace openspace {

    AuthorizationTopic::AuthorizationTopic()
        : Topic()
        , _isAuthenticated(false) {};

bool AuthorizationTopic::isDone() {
    return _isAuthenticated;
}

void AuthorizationTopic::handleJson(nlohmann::json json) {
    if (isDone()) {
        _connection->sendJson(message("Already authorized.", Statuses::OK));
    } else {
        try {
            auto providedKey = json.at("key").get<std::string>();
            if (authorize(providedKey)) {
                _connection->sendJson(message("Authorization OK.", Statuses::Accepted));
                _connection->setAuthorized(true);
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

bool AuthorizationTopic::authorize(const std::string key) {
    _isAuthenticated = key == getKey();
    return _isAuthenticated;
}

const std::string AuthorizationTopic::getKey() const {
    const bool hasConfigPassword = OsEng.configurationManager().hasKeyAndValue<std::string>(
        ConfigurationManager::KeyServerPasskey);
    if (hasConfigPassword) {
        return OsEng.configurationManager().value<std::string>(
            ConfigurationManager::KeyServerPasskey);
    }

    return "17308";
}

nlohmann::json AuthorizationTopic::message(const std::string &message, const int &statusCode) {
    nlohmann::json error = {{"message", message}, {"code", statusCode}};
    return error;
}

}
