/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2025                                                               *
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

#include <modules/server/include/topics/profiletopic.h>

#include <modules/server/include/connection.h>
#include <modules/server/include/jsonconverters.h>
#include <openspace/engine/configuration.h>
#include <openspace/engine/globals.h>
#include <openspace/scene/profile.h>

namespace openspace {

bool ProfileTopic::isDone() const {
    return true;
}

void ProfileTopic::handleJson(const nlohmann::json&) {
    // @TODO (2025-04-30, emmbr) If we expose the json converters from profile.cpp, we
    // could use those here instead and minimize the risk of getting the serialization of
    // the data out of sync
    nlohmann::json data = {
        { "uiPanelVisibility", global::profile->uiPanelVisibility },
        { "markNodes", global::profile->markNodes },
        { "filePath", global::configuration->profile }
    };

    if (global::profile->meta.has_value()) {
        data["name"] = global::profile->meta->name.value_or("");

        data["author"] = global::profile->meta->author.value_or("");
        data["description"] = global::profile->meta->description.value_or("");
        data["license"] = global::profile->meta->license.value_or("");
        data["url"] = global::profile->meta->url.value_or("");
        data["version"] = global::profile->meta->version.value_or("");
    }

    _connection->sendJson(wrappedPayload(data));
}

} // namespace openspace
