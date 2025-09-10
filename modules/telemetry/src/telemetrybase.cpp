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

#include <modules/telemetry/include/telemetrybase.h>

#include <openspace/engine/globals.h>
#include <openspace/engine/moduleengine.h>
#include <openspace/scene/scenegraphnode.h>
#include <openspace/query/query.h>

namespace {
    constexpr openspace::properties::Property::PropertyInfo EnabledInfo = {
        "Enabled",
        "Enabled",
        "This setting determines whether this telemetry gathering is enabled or not.",
        openspace::properties::Property::Visibility::User
    };
} // namespace

namespace openspace {

TelemetryBase::TelemetryBase(properties::PropertyOwner::PropertyOwnerInfo info,
                             const std::string& ip, int port)
    : properties::PropertyOwner(info)
    , _identifier(info.identifier)
    , _enabled(EnabledInfo, false)
    , _connection(new OpenSoundControlConnection(ip, port))
{
    addProperty(_enabled);
    _enabled.onChange([this]() {
        if (!_enabled) {
            // Disable sending of data
            stop();
        }
    });
}

TelemetryBase::~TelemetryBase() {
    stop();
    delete _connection;
}

void TelemetryBase::update(const Camera* camera) {
    if (!_enabled) {
        return;
    }

    const bool dataWasUpdated = updateData(camera);

    if (dataWasUpdated) {
        sendData();
    }
}

void TelemetryBase::stop() {}

std::string TelemetryBase::identifier() const {
    return _identifier;
}

bool TelemetryBase::enabled() const {
    return _enabled;
}

} // namespace openspace
