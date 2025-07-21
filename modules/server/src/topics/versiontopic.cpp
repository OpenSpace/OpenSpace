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

#include "modules/server/include/topics/versiontopic.h"

#include <modules/server/include/connection.h>
#include <modules/server/servermodule.h>
#include <openspace/openspace.h>
#include <openspace/engine/globals.h>
#include <openspace/util/versionchecker.h>

namespace openspace {

bool VersionTopic::isDone() const {
    return true;
}

void VersionTopic::handleJson(const nlohmann::json&) {
    nlohmann::json versionJson = {
        {
            "openSpaceVersion",
            {
                { "major", OPENSPACE_VERSION_MAJOR },
                { "minor", OPENSPACE_VERSION_MINOR },
                { "patch", OPENSPACE_VERSION_PATCH }
            }
        },
        {
            "socketApiVersion",
            {
                { "major", SOCKET_API_VERSION_MAJOR },
                { "minor", SOCKET_API_VERSION_MINOR },
                { "patch", SOCKET_API_VERSION_PATCH }
            }
        }
    };

    if (global::versionChecker->hasLatestVersionInfo()) {
        VersionChecker::SemanticVersion latestVersion =
            global::versionChecker->latestVersion();

        versionJson["latestOpenSpaceVersion"] = {
            { "major", latestVersion.major },
            { "minor", latestVersion.minor },
            { "patch", latestVersion.patch }
        };
    }

    _connection->sendJson(wrappedPayload(versionJson));
}

} // namespace openspace
