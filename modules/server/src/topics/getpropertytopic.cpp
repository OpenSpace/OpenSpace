/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2024                                                               *
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

#include <modules/server/include/topics/getpropertytopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>
#include <modules/volume/transferfunctionhandler.h>
#include <openspace/engine/globals.h>
#include <openspace/engine/windowdelegate.h>
#include <openspace/navigation/navigationhandler.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/query/query.h>
#include <openspace/rendering/luaconsole.h>
#include <openspace/rendering/renderengine.h>
#include <openspace/rendering/screenspacerenderable.h>
#include <openspace/scene/scene.h>
#include <ghoul/logging/logmanager.h>

using nlohmann::json;

namespace {
    constexpr std::string_view _loggerCat = "GetPropertyTopic";
    constexpr std::string_view AllPropertiesValue = "__allProperties";
    constexpr std::string_view AllNodesValue = "__allNodes";
    constexpr std::string_view AllScreenSpaceRenderablesValue =
        "__screenSpaceRenderables";
    constexpr std::string_view RootPropertyOwner = "__rootOwner";
} // namespace

namespace openspace {

void GetPropertyTopic::handleJson(const nlohmann::json& json) {
    const std::string requestedKey = json.at("property").get<std::string>();
    LDEBUG("Getting property '" + requestedKey + "'...");
    nlohmann::json response;
    if (requestedKey == AllPropertiesValue) {
        response = allProperties();
    }
    else if (requestedKey == AllNodesValue) {
        response = wrappedPayload(sceneGraph()->allSceneGraphNodes());
    }
    else if (requestedKey == AllScreenSpaceRenderablesValue) {
        response = wrappedPayload({
            { "value", global::renderEngine->screenSpaceRenderables() }
        });
    }
    else if (requestedKey == RootPropertyOwner) {
        response = wrappedPayload(global::rootPropertyOwner);
    }
    else {
        response = propertyFromKey(requestedKey);
    }
    _connection->sendJson(response);
}

bool GetPropertyTopic::isDone() const {
    return true;
}

json GetPropertyTopic::allProperties() {
    const json payload {
        {
            "value",
            {
                global::renderEngine,
                global::luaConsole,
                global::parallelPeer,
                global::navigationHandler
            }
        }
    };
    return wrappedPayload(payload);
}

json GetPropertyTopic::propertyFromKey(const std::string& key) {
    properties::Property* prop = property(key);
    if (prop) {
        return wrappedPayload(prop);
    }

    return wrappedError(std::format("Property '{}' not found", key), 404);
}

} // namespace openspace
