/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2023                                                               *
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

#include <modules/server/include/topics/documentationtopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>
#include <openspace/engine/globals.h>

#include <openspace/scripting/scriptengine.h>
#include <openspace/util/factorymanager.h>
#include <openspace/interaction/keybindingmanager.h>
#include <openspace/scene/scenelicensewriter.h>
#include <ghoul/logging/logmanager.h>

using nlohmann::json;

namespace openspace {

void DocumentationTopic::handleJson(const nlohmann::json& json) {
    std::string requestedType = json.at("type").get<std::string>();

    nlohmann::json response;

    // @emiax: Proposed future refector.
    // Do not parse generated json. Instead implement ability to get
    // ghoul::Dictionary objects from ScriptEngine, FactoryManager, and KeybindingManager.
    if (requestedType == "lua") {
        response = json::parse(global::scriptEngine->generateJson());
    }
    else if (requestedType == "factories") {
        response = json::parse(FactoryManager::ref().generateJson());
    }
    else if (requestedType == "keyboard") {
        response = json::parse(global::keybindingManager->generateJson());
    }
    else if (requestedType == "asset") {
        response = json::parse(global::keybindingManager->generateJson());
    }
    else if (requestedType == "meta") {
        std::string docs = SceneLicenseWriter().generateJson();
        nlohmann::json parsedDocs = json::parse(docs);
        response = parsedDocs;
    }

    _connection->sendJson(wrappedPayload(response));
}

bool DocumentationTopic::isDone() const {
    return true;
}

} // namespace openspace
