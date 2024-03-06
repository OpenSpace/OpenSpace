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

#include <modules/omni/include/scenario.h>

#include <modules/server/include/jsonconverters.h> // TODO: move this outside server module?
#include <string>

namespace {
    constexpr std::string_view ScenarioMessageKeySetIdle = "setIdle";
    constexpr std::string_view ScenarioMessageKeySetScenario = "setScenario";
}

namespace openspace::omni {

Scenario::Scenario(const std::string& identifier, const ghoul::Dictionary& dictionary)
    : _identifier{ identifier }, _assetInformation{ dictionary } {}

void openspace::omni::Scenario::initialize(std::shared_ptr<ghoul::io::TcpSocket> socket)
{
    _socket = std::move(socket);
}

void Scenario::sendMessage(const std::string& message) const {
    _socket->putMessage(message);
}

void Scenario::sendJson(const nlohmann::json& json) const {
    sendMessage(json.dump());
}

nlohmann::json Scenario::wrappedPayload(std::string_view type,
                                                      const nlohmann::json& payload) const
{
    ZoneScoped;

    nlohmann::json j = {
        { "type", type},
        { "payload", payload }
    };
    return j;
}

void Scenario::enableScenario() {
    sendAssetInfo();
    _isActive = true;
    onEnableScenario();
}

void Scenario::disableScenario() {
    sendJson(wrappedPayload(ScenarioMessageKeySetIdle, ""));
    _isActive = false;
    onDisableScenario();
}

bool Scenario::isActive() const {
    return _isActive;
}

std::string_view Scenario::identifier() const {
    return _identifier;
}

void Scenario::sendAssetInfo() const {
    sendJson(wrappedPayload(ScenarioMessageKeySetScenario, _assetInformation));
}

} // namespace openspace::omni
