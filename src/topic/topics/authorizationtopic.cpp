/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2026                                                               *
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

#include <openspace/topic/topics/authorizationtopic.h>

#include <openspace/documentation/schema.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/topic/connection.h>
#include <ghoul/logging/logmanager.h>
#include <stdexcept>
#include <string_view>
#include <utility>

namespace {
    constexpr std::string_view _loggerCat = "AuthorizationTopic";

    enum class Status {
        Authorized,
        IncorrecctKey,
        BadRequest
    };

    std::string_view toString(Status response) {
        switch (response) {
            case Status::Authorized: return "authorized";
            case Status::IncorrecctKey: return "incorrectKey";
            case Status::BadRequest: return "badRequest";
            default: throw ghoul::MissingCaseException();
        }
    }

    nlohmann::json response(Status status) {
        nlohmann::json response;
        response["status"] = toString(status);
        return response;
    }
} // namespace

namespace openspace {

AuthorizationTopic::AuthorizationTopic() {}

bool AuthorizationTopic::isDone() const {
    return _isAuthenticated;
}

Schema AuthorizationTopic::Schema() {
    nlohmann::json schema = nlohmann::json::parse(R"(
        {
          "title": "AuthorizationTopic",
          "type": "object",
          "properties": {
            "topicId": { "const": "authorize" },
            "topicPayload": {
              "type": "object",
              "properties": {
                "password": { "type": "string" }
              },
              "additionalProperties": false,
              "required": ["password"]
            },
            "data": {
              "type": "object",
              "properties": {
                "status": {
                  "type": "string",
                  "enum": ["authorized", "incorrectKey", "badRequest"]
                }
              },
              "additionalProperties": false,
              "required": ["status"]
            }
          },
          "additionalProperties": false,
          "required": ["topicId", "topicPayload", "data"]
        }
    )");

    return { "authorizationtopic", schema };
}

void AuthorizationTopic::handleJson(const nlohmann::json& json) {
    if (isDone()) {
        sendData(response(Status::Authorized));
    }
    else {
        try {
            const std::string providedPassword = json.at("password").get<std::string>();
            if (authorize(providedPassword)) {
                _connection->setAuthorized(true);
                sendData(response(Status::Authorized));
                LINFO("Client successfully authorized");
            }
            else {
                sendData(response(Status::IncorrecctKey));
            }
        }
        catch (const std::out_of_range&) {
            sendData(response(Status::BadRequest));
        }
        catch (const std::domain_error&) {
            sendData(response(Status::BadRequest));
        }
    }
}

bool AuthorizationTopic::authorize(const std::string& key) {
    _isAuthenticated = (key == _connection->password());
    return _isAuthenticated;
}

} // namespace openspace
