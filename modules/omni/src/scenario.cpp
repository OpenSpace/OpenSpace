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
#include <openspace/documentation/documentation.h>
#include <openspace/engine/globals.h>
#include <openspace/scripting/scriptengine.h>
#include <string>

namespace {
    constexpr std::string_view ScenarioMessageKeySetIdle = "setIdle";
    constexpr std::string_view ScenarioMessageKeySetScenario = "setScenario";

    // Explanation of Scenario
    struct [[codegen::Dictionary(Scenario)]] Parameters {
        // Unique identifier for this scenario
        std::string identifier;

        // Type of scenario, e.g. poll
        std::string scenarioType;

        // What should happen when we enable this scenario
        std::string onEnable;

        // What should happen we we disable this scenario
        std::string onDisable;
    };
#include "scenario_codegen.cpp"
}

namespace openspace::omni {

documentation::Documentation Scenario::Documentation() {
    return codegen::doc<Parameters>("omni_scenario");
}

Scenario::Scenario(const ghoul::Dictionary& dictionary) {
    const Parameters p = codegen::bake<Parameters>(dictionary);

    _identifier = p.identifier;
    _onEnableScript = p.onEnable;
    _onDisableScript = p.onDisable;
}

void openspace::omni::Scenario::initialize(std::shared_ptr<ghoul::io::TcpSocket> socket) {
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
        { "type", type },
        { "payload", payload }
    };
    return j;
}

void Scenario::sendScenarioData() const {
    sendJson(wrappedPayload(ScenarioMessageKeySetScenario, jsonPayload()));
}

void Scenario::enableScenario() {
    sendScenarioData();
    _isActive = true;

    global::scriptEngine->queueScript(
        _onEnableScript,
        openspace::scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        openspace::scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

void Scenario::disableScenario() {
    sendJson(wrappedPayload(ScenarioMessageKeySetIdle, ""));
    _isActive = false;

    global::scriptEngine->queueScript(
        _onDisableScript,
        openspace::scripting::ScriptEngine::ShouldBeSynchronized::Yes,
        openspace::scripting::ScriptEngine::ShouldSendToRemote::Yes
    );
}

bool Scenario::isActive() const {
    return _isActive;
}

std::string_view Scenario::identifier() const {
    return _identifier;
}

} // namespace openspace::omni
