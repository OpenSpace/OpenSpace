/*****************************************************************************************
 *                                                                                       *
 * OpenSpace                                                                             *
 *                                                                                       *
 * Copyright (c) 2014-2017                                                               *
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

#include <modules/onscreengui/include/guiparallelcomponent.h>

#include <openspace/engine/openspaceengine.h>
#include <openspace/util/timemanager.h>
#include <openspace/interaction/interactionhandler.h>
#include <openspace/network/parallelconnection.h>
#include <openspace/network/messagestructures.h>

#include <imgui.h>
#include <fmt/format.h>

#include <algorithm>
#include <numeric>

namespace {
    const std::string _loggerCat = "GuiParallelComponent";
}

namespace openspace {
namespace gui {

GuiParallelComponent::GuiParallelComponent()
    : GuiPropertyComponent("GuiParallelComponent")
{
    setVisibility(properties::Property::Visibility::All);
}

void GuiParallelComponent::renderDisconnected() {
    ImGui::Text("Not connected");

    const bool connect = ImGui::Button("Connect");

    if (connect) {
        OsEng.parallelConnection().clientConnect();
    }
}

void GuiParallelComponent::renderClientWithHost() {

    ParallelConnection& parallel = OsEng.parallelConnection();

    std::string connectionInfo = "Session hosted by \"" + parallel.hostName() + "\"\n";
    size_t nConnections = parallel.nConnections();
    size_t nClients = nConnections - 1;

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

    ImGui::Text(connectionInfo.c_str());
    renderClientCommon();

    const std::deque<datamessagestructures::TimeKeyframe> timeKeyframes = OsEng.timeManager().keyframes();
    const std::vector<datamessagestructures::CameraKeyframe> cameraKeyframes = OsEng.interactionHandler().keyframes();

    std::string timeKeyframeInfo = "TimeKeyframes : " + std::to_string(timeKeyframes.size());
    std::string cameraKeyframeInfo = "CameraKeyframes : " + std::to_string(cameraKeyframes.size());
    std::string latencyStandardDeviation = "Latency standard deviation: " + std::to_string(parallel.latencyStandardDeviation()) + " s";

    const bool resetTimeOffset = ImGui::Button("Reset time offset");

    if (resetTimeOffset) {
        parallel.resetTimeOffset();
    }

    ImGui::Text(timeKeyframeInfo.c_str());
    ImGui::Text(cameraKeyframeInfo.c_str());
    ImGui::Text(latencyStandardDeviation.c_str());
}

void GuiParallelComponent::renderClientWithoutHost() {
    ParallelConnection& parallel = OsEng.parallelConnection();

    std::string connectionInfo = "Connected to parallel session with no host\n";
    size_t nConnections = parallel.nConnections();

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

    ImGui::Text(connectionInfo.c_str());

    renderClientCommon();

}

void GuiParallelComponent::renderClientCommon() {
    ParallelConnection& parallel = OsEng.parallelConnection();

    bool requestHostship = ImGui::Button("Request hostship");
    const bool disconnect = ImGui::Button("Disconnect");

    if (requestHostship) {
        parallel.requestHostship();
    }

    if (disconnect) {
        parallel.signalDisconnect();
    }
}

void GuiParallelComponent::renderHost() {
    ParallelConnection& parallel = OsEng.parallelConnection();
    size_t nConnections = parallel.nConnections();

    std::string connectionInfo = "";
    size_t nClients = nConnections - 1;
    if (nClients == 1) {
        connectionInfo = "Hosting session with 1 client";
    } else {
        connectionInfo =
            "Hosting session with " + std::to_string(nClients) + " clients";
    }

    ImGui::Text(connectionInfo.c_str());

    const bool resignHostship = ImGui::Button("Resign hostship");
    if (resignHostship) {
        parallel.resignHostship();
    }
}


void GuiParallelComponent::render() {
    bool v = _isEnabled;
    ImGui::Begin("Parallel Connection", &v);
    _isEnabled = v;

    ParallelConnection::Status status = OsEng.parallelConnection().status();

    switch (status) {
    case ParallelConnection::Status::Disconnected:
        renderDisconnected();
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

    GuiPropertyComponent::renderPropertyOwner(&OsEng.parallelConnection());
    ImGui::End();
}

} // namespace gui
} // namespace openspace
