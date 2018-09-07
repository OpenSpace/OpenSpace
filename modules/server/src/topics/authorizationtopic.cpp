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

#include <modules/server/include/topics/authorizationtopic.h>

#include <modules/server/include/connection.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <ghoul/logging/logmanager.h>

namespace {
    constexpr const char* _loggerCat = "AuthorizationTopic";

    /* https://httpstatuses.com/ */
    enum class StatusCode : int {
        OK = 200,
        Accepted = 202,

        BadRequest = 400,
        Unauthorized = 401,
        NotAcceptable = 406,

        NotImplemented = 501
    };



    nlohmann::json message(const std::string& message, StatusCode statusCode) {
        return { { "message", message }, { "code", static_cast<int>(statusCode) } };
    }

} // namespace

namespace openspace {

bool AuthorizationTopic::isDone() const {
    return _isAuthenticated;
}

void AuthorizationTopic::handleJson(const nlohmann::json& json) {
    if (isDone()) {
        _connection->sendJson(message("Already authorized.", StatusCode::OK));
    } else {
        try {
            auto providedKey = json.at("key").get<std::string>();
            if (authorize(providedKey)) {
                _connection->sendJson(message("Authorization OK.", StatusCode::Accepted));
                _connection->setAuthorized(true);
                LINFO("Client successfully authorized.");
            } else {
                _connection->sendJson(message("Invalid key", StatusCode::NotAcceptable));
            }
        } catch (const std::out_of_range&) {
            _connection->sendJson(
                message("Invalid request, key must be provided.", StatusCode::BadRequest)
            );
        } catch (const std::domain_error&) {
            _connection->sendJson(
                message("Invalid request, invalid key format.", StatusCode::BadRequest)
            );
        }
    }
}

bool AuthorizationTopic::authorize(const std::string& key) {
    _isAuthenticated = (key == global::configuration.serverPasskey);
    return _isAuthenticated;
}

} // namespace openspace
