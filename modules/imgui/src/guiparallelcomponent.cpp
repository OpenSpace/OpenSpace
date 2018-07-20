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

#include <modules/imgui/include/guiparallelcomponent.h>

#include <modules/imgui/include/imgui_include.h>
#include <openspace/engine/globals.h>
#include <openspace/util/timemanager.h>
#include <openspace/interaction/keyframenavigator.h>
#include <openspace/interaction/navigationhandler.h>
#include <openspace/network/parallelpeer.h>
#include <openspace/network/messagestructures.h>

#include <ghoul/fmt.h>

#include <algorithm>
#include <numeric>

namespace openspace::gui {

GuiParallelComponent::GuiParallelComponent()
    : GuiPropertyComponent("Parallel", "Parallel Connection")
{
    setVisibility(properties::Property::Visibility::All);
}

void GuiParallelComponent::renderDisconnected() {
    ImGui::Text("Not connected");

    const bool connect = ImGui::Button("Connect");
    if (connect) {
        global::parallelPeer.connect();
    }
}

void GuiParallelComponent::renderConnecting() {
    ImGui::Text("Connecting...");

    const bool cancel = ImGui::Button("Cancel connection");
    if (cancel) {
        global::parallelPeer.disconnect();
    }
}

void GuiParallelComponent::renderClientWithHost() {
    ParallelPeer& parallel = global::parallelPeer;

    std::string connectionInfo = "Session hosted by \"" + parallel.hostName() + "\"\n";
    const size_t nConnections = parallel.nConnections();
    const size_t nClients = nConnections - 1;

    if (nClients > 2) {
        std::string c = std::to_string(nClients - 1);
        connectionInfo += "You and " + c + " more clients are connected";
    }
    else if (nClients == 2) {
        std::string c = std::to_string(nClients - 1);
        connectionInfo += "You and " + c + " more client are connected";
    }
    else if (nClients == 1) {
        connectionInfo += "You are the only client";
    }

    ImGui::Text("%s", connectionInfo.c_str());
    renderClientCommon();

    const size_t nTimeKeyframes = global::timeManager.nKeyframes();
    const size_t nCameraKeyframes =
        global::navigationHandler.keyframeNavigator().nKeyframes();

    std::string timeKeyframeInfo = "TimeKeyframes : " + std::to_string(nTimeKeyframes);
    std::string cameraKeyframeInfo = "CameraKeyframes : " +
                                     std::to_string(nCameraKeyframes);
    std::string latencyStandardDeviation = "Latency standard deviation: " +
                               std::to_string(parallel.latencyStandardDeviation()) + " s";

    const bool resetTimeOffset = ImGui::Button("Reset time offset");

    if (resetTimeOffset) {
        parallel.resetTimeOffset();
    }

    ImGui::Text("%s", timeKeyframeInfo.c_str());
    ImGui::Text("%s", cameraKeyframeInfo.c_str());
    ImGui::Text("%s", latencyStandardDeviation.c_str());
}

void GuiParallelComponent::renderClientWithoutHost() {
    std::string connectionInfo = "Connected to parallel session with no host\n";
    const size_t nConnections = global::parallelPeer.nConnections();

    if (nConnections > 2) {
        std::string c = std::to_string(nConnections - 1);
        connectionInfo += "You and " + c + " more users are connected";
    }
    else if (nConnections == 2) {
        std::string c = std::to_string(nConnections - 1);
        connectionInfo += "You and " + c + " more users are connected";
    }
    else if (nConnections == 1) {
        connectionInfo += "You are the only one here";
    }

    ImGui::Text("%s", connectionInfo.c_str());

    renderClientCommon();
}

void GuiParallelComponent::renderClientCommon() {
    const bool requestHostship = ImGui::Button("Request hostship");
    if (requestHostship) {
        global::parallelPeer.requestHostship();
    }

    const bool disconnect = ImGui::Button("Disconnect");
    if (disconnect) {
        global::parallelPeer.disconnect();
    }
}

void GuiParallelComponent::renderHost() {
    const size_t nConnections = global::parallelPeer.nConnections();

    std::string connectionInfo;
    const size_t nClients = nConnections - 1;
    if (nClients == 1) {
        connectionInfo = "Hosting session with 1 client";
    } else {
        connectionInfo = "Hosting session with " + std::to_string(nClients) + " clients";
    }

    ImGui::Text("%s", connectionInfo.c_str());

    const bool resignHostship = ImGui::Button("Resign hostship");
    if (resignHostship) {
        global::parallelPeer.resignHostship();
    }
}


void GuiParallelComponent::render() {
    ImGui::SetNextWindowCollapsed(_isCollapsed);
    bool v = _isEnabled;
    ImGui::Begin("Parallel Connection", &v);
    _isEnabled = v;
    _isCollapsed = ImGui::IsWindowCollapsed();

    ParallelConnection::Status status = global::parallelPeer.status();

    switch (status) {
        case ParallelConnection::Status::Disconnected:
            renderDisconnected();
            break;
        case ParallelConnection::Status::Connecting:
            renderConnecting();
            break;
        case ParallelConnection::Status::ClientWithHost:
            renderClientWithHost();
            break;
        case ParallelConnection::Status::ClientWithoutHost:
            renderClientWithoutHost();
            break;
        case ParallelConnection::Status::Host:
            renderHost();
            break;
    }

    GuiPropertyComponent::renderPropertyOwner(&global::parallelPeer);
    ImGui::End();
}

} // namespace openspace::gui
